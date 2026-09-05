// SPDX-License-Identifier: MIT
// Copyright (c) 2026 The Cyrus Language

#include <cassert>
#include <cstdlib>
#include <memory>
#include <optional>
#include <string>

#include <llvm/Analysis/TargetLibraryInfo.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Passes/OptimizationLevel.h>
#include <llvm/Passes/PassBuilder.h>
#include <llvm/Passes/StandardInstrumentations.h>
#include <llvm/Support/Debug.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/FormatVariadic.h>
#include <llvm/Support/TimeProfiler.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Transforms/Utils/AddDiscriminators.h>

#define DEBUG_TYPE "cyrus-optimizer"

using namespace llvm;

namespace cyrus {

enum class OptimizeLevel : uint32_t {
  O0 = 0,
  O1 = 1,
  O2 = 2,
  O3 = 3,
  Os = 4,
  Oz = 5,
};

struct OptimizeConfig {
  bool verify_each = false;
  bool debug_logging = false;
  bool allow_loop_unrolling = true;
  bool allow_slp_vectorization = true;
  bool allow_loop_vectorization = true;
  bool allow_loop_interleaving = true;
  bool allow_merge_functions = true;
  bool add_discriminators = true;
  bool verify_input = true;
  bool verify_output = true;
  bool disable_code_hoisting = false;
  bool disable_speculative_execution = false;
};

class TimeTracerRAII {
public:
  TimeTracerRAII(const std::string &program_name) {
    if (const char *trace_file = getenv("CYRUS_LLVM_TIME_TRACE_FILE")) {
      unsigned granularity = 500;
      if (const char *g = getenv("CYRUS_LLVM_TIME_TRACE_GRANULARITY")) {
        granularity = static_cast<unsigned>(std::atoi(g));
      }
      timeTraceProfilerInitialize(granularity, program_name);
      enabled_ = true;
      trace_file_ = trace_file;
    }
  }

  ~TimeTracerRAII() {
    if (enabled_) {
      std::error_code EC;
      raw_fd_ostream OS(trace_file_, EC, sys::fs::OF_None);
      if (!EC) {
        timeTraceProfilerWrite(OS);
      }
      timeTraceProfilerCleanup();
    }
  }

private:
  bool enabled_ = false;
  std::string trace_file_;
};

class LLVMOptimizer {
public:
  LLVMOptimizer(TargetMachine *target_machine = nullptr)
      : target_machine_(target_machine) {
    loop_am_ = std::make_unique<LoopAnalysisManager>();
    function_am_ = std::make_unique<FunctionAnalysisManager>();
    cgscc_am_ = std::make_unique<CGSCCAnalysisManager>();
    module_am_ = std::make_unique<ModuleAnalysisManager>();
  }

  bool optimizeModule(Module &module, OptimizeLevel level,
                      const OptimizeConfig &config = OptimizeConfig{}) {
    config_ = config;
    TimeTracerRAII tracer("cyrus-opt");

    if (config_.verify_input) {
      std::string error;
      raw_string_ostream error_stream(error);
      if (verifyModule(module, &error_stream)) {
        errs() << "LLVM IR verification failed before optimization:\n"
               << error << "\n";
        return false;
      }
    }

    PassInstrumentationCallbacks instrument_callbacks;
    std::unique_ptr<StandardInstrumentations> std_instrumentations;
    if (config_.debug_logging) {
      std_instrumentations = std::make_unique<StandardInstrumentations>(
          module.getContext(), false);
      std_instrumentations->registerCallbacks(instrument_callbacks);
    }

    PipelineTuningOptions pipeline_opts;
    pipeline_opts.LoopUnrolling = config_.allow_loop_unrolling;
    pipeline_opts.SLPVectorization = config_.allow_slp_vectorization;
    pipeline_opts.LoopVectorization = config_.allow_loop_vectorization;
    pipeline_opts.LoopInterleaving = config_.allow_loop_interleaving;
    pipeline_opts.MergeFunctions = config_.allow_merge_functions;

    pass_builder_ = std::make_unique<PassBuilder>(
        target_machine_, pipeline_opts, std::nullopt, &instrument_callbacks);

    setupAnalysisManagers(module);

    pass_builder_->registerPipelineStartEPCallback(
        [this](ModulePassManager &mpm, OptimizationLevel level) {
          if (config_.verify_input) {
            mpm.addPass(VerifierPass());
          }
          if (config_.add_discriminators && level != OptimizationLevel::O0) {
            mpm.addPass(
                createModuleToFunctionPassAdaptor(AddDiscriminatorsPass()));
          }
        });

    pass_builder_->registerOptimizerLastEPCallback(
        [this](ModulePassManager &mpm, OptimizationLevel level,
               ThinOrFullLTOPhase phase) {
          if (config_.verify_output) {
            mpm.addPass(VerifierPass());
          }
        });

    OptimizationLevel llvm_level = toLLVMOptLevel(level);
    bool is_lto = false;

    ModulePassManager mpm;
    if (llvm_level == OptimizationLevel::O0) {
      mpm = pass_builder_->buildO0DefaultPipeline(llvm_level);
    } else if (is_lto) {
      mpm = pass_builder_->buildLTOPreLinkDefaultPipeline(llvm_level);
    } else {
      mpm = pass_builder_->buildPerModuleDefaultPipeline(llvm_level);
    }

    mpm.run(module, *module_am_);

    if (config_.verify_output) {
      std::string error;
      raw_string_ostream error_stream(error);
      if (verifyModule(module, &error_stream)) {
        errs() << "LLVM IR verification failed after optimization:\n"
               << error << "\n";
        return false;
      }
    }

    return true;
  }

  bool optimizeModuleSimple(Module &module, OptimizeLevel level,
                            bool is_debug) {
    OptimizeConfig config;
    config.verify_each = false;
    config.debug_logging = is_debug;
    config.allow_loop_unrolling = !is_debug;
    config.allow_slp_vectorization = !is_debug;
    config.allow_loop_vectorization = !is_debug;
    config.allow_loop_interleaving = !is_debug;
    config.allow_merge_functions = !is_debug;
    config.add_discriminators = !is_debug;
    config.verify_input = true;
    config.verify_output = true;
    config.disable_code_hoisting = false;
    config.disable_speculative_execution = false;

    return optimizeModule(module, level, config);
  }

  void setTargetMachine(TargetMachine *tm) { target_machine_ = tm; }

private:
  OptimizationLevel toLLVMOptLevel(OptimizeLevel level) const {
    switch (level) {
    case OptimizeLevel::O0:
      return OptimizationLevel::O0;
    case OptimizeLevel::O1:
      return OptimizationLevel::O1;
    case OptimizeLevel::O2:
      return OptimizationLevel::O2;
    case OptimizeLevel::O3:
      return OptimizationLevel::O3;
    case OptimizeLevel::Os:
      return OptimizationLevel::Os;
    case OptimizeLevel::Oz:
      return OptimizationLevel::Oz;
    default:
      return OptimizationLevel::O0;
    }
  }

  void setupAnalysisManagers(Module &module) {
    if (target_machine_) {
      const Triple &triple = target_machine_->getTargetTriple();
      auto tlii = std::make_unique<TargetLibraryInfoImpl>(triple);
      function_am_->registerPass(
          [tlii = std::move(tlii)] { return TargetLibraryAnalysis(*tlii); });
    } else {
      Triple triple(module.getTargetTriple());
      auto tlii = std::make_unique<TargetLibraryInfoImpl>(triple);
      function_am_->registerPass(
          [tlii = std::move(tlii)] { return TargetLibraryAnalysis(*tlii); });
    }

    pass_builder_->registerModuleAnalyses(*module_am_);
    pass_builder_->registerCGSCCAnalyses(*cgscc_am_);
    pass_builder_->registerFunctionAnalyses(*function_am_);
    pass_builder_->registerLoopAnalyses(*loop_am_);

    pass_builder_->crossRegisterProxies(*loop_am_, *function_am_, *cgscc_am_,
                                        *module_am_);
  }

  TargetMachine *target_machine_;
  OptimizeConfig config_;

  std::unique_ptr<LoopAnalysisManager> loop_am_;
  std::unique_ptr<FunctionAnalysisManager> function_am_;
  std::unique_ptr<CGSCCAnalysisManager> cgscc_am_;
  std::unique_ptr<ModuleAnalysisManager> module_am_;
  std::unique_ptr<PassBuilder> pass_builder_;
};

} // namespace cyrus

extern "C" {

struct CyrusOptimizerContext {
  std::unique_ptr<cyrus::LLVMOptimizer> optimizer;
};

typedef enum {
  CYRUS_OPT_LEVEL_O0 = 0,
  CYRUS_OPT_LEVEL_O1 = 1,
  CYRUS_OPT_LEVEL_O2 = 2,
  CYRUS_OPT_LEVEL_O3 = 3,
  CYRUS_OPT_LEVEL_Os = 4,
  CYRUS_OPT_LEVEL_Oz = 5,
} CyrusOptLevel;

struct CyrusOptimizerConfig {
  uint32_t verify_each;
  uint32_t debug_logging;
  uint32_t allow_loop_unrolling;
  uint32_t allow_slp_vectorization;
  uint32_t allow_loop_vectorization;
  uint32_t allow_loop_interleaving;
  uint32_t allow_merge_functions;
  uint32_t add_discriminators;
  uint32_t verify_input;
  uint32_t verify_output;
  uint32_t disable_code_hoisting;
  uint32_t disable_speculative_execution;
};

struct CyrusErrorMessage {
  char *data;
  size_t len;
};

CyrusOptimizerContext *cyrus_optimizer_create(void *target_machine) {
  auto *ctx = new CyrusOptimizerContext();
  auto *tm = reinterpret_cast<llvm::TargetMachine *>(target_machine);
  ctx->optimizer = std::make_unique<cyrus::LLVMOptimizer>(tm);
  return ctx;
}

void cyrus_optimizer_destroy(CyrusOptimizerContext *ctx) {
  if (ctx) {
    delete ctx;
  }
}

void cyrus_optimizer_set_target_machine(CyrusOptimizerContext *ctx,
                                        void *target_machine) {
  if (ctx && ctx->optimizer) {
    auto *tm = reinterpret_cast<llvm::TargetMachine *>(target_machine);
    ctx->optimizer->setTargetMachine(tm);
  }
}

CyrusOptimizerConfig cyrus_optimizer_config_default(void) {
  CyrusOptimizerConfig config;
  config.verify_each = 0;
  config.debug_logging = 0;
  config.allow_loop_unrolling = 1;
  config.allow_slp_vectorization = 1;
  config.allow_loop_vectorization = 1;
  config.allow_loop_interleaving = 1;
  config.allow_merge_functions = 1;
  config.add_discriminators = 1;
  config.verify_input = 1;
  config.verify_output = 1;
  config.disable_code_hoisting = 0;
  config.disable_speculative_execution = 0;
  return config;
}

CyrusOptimizerConfig cyrus_optimizer_config_debug(void) {
  CyrusOptimizerConfig config;
  config.verify_each = 1;
  config.debug_logging = 1;
  config.allow_loop_unrolling = 0;
  config.allow_slp_vectorization = 0;
  config.allow_loop_vectorization = 0;
  config.allow_loop_interleaving = 0;
  config.allow_merge_functions = 0;
  config.add_discriminators = 0;
  config.verify_input = 1;
  config.verify_output = 1;
  config.disable_code_hoisting = 0;
  config.disable_speculative_execution = 0;
  return config;
}

int cyrus_optimizer_optimize(CyrusOptimizerContext *ctx, void *module_ptr,
                             CyrusOptLevel level,
                             const CyrusOptimizerConfig *config) {
  if (!ctx || !ctx->optimizer || !module_ptr) {
    return 0;
  }

  auto *module = reinterpret_cast<llvm::Module *>(module_ptr);
  auto opt_level = static_cast<cyrus::OptimizeLevel>(level);

  if (config) {
    cyrus::OptimizeConfig cpp_config;
    cpp_config.verify_each = config->verify_each != 0;
    cpp_config.debug_logging = config->debug_logging != 0;
    cpp_config.allow_loop_unrolling = config->allow_loop_unrolling != 0;
    cpp_config.allow_slp_vectorization = config->allow_slp_vectorization != 0;
    cpp_config.allow_loop_vectorization = config->allow_loop_vectorization != 0;
    cpp_config.allow_loop_interleaving = config->allow_loop_interleaving != 0;
    cpp_config.allow_merge_functions = config->allow_merge_functions != 0;
    cpp_config.add_discriminators = config->add_discriminators != 0;
    cpp_config.verify_input = config->verify_input != 0;
    cpp_config.verify_output = config->verify_output != 0;
    cpp_config.disable_code_hoisting = config->disable_code_hoisting != 0;
    cpp_config.disable_speculative_execution =
        config->disable_speculative_execution != 0;

    return ctx->optimizer->optimizeModule(*module, opt_level, cpp_config) ? 1
                                                                          : 0;
  } else {
    cyrus::OptimizeConfig default_config;
    return ctx->optimizer->optimizeModule(*module, opt_level, default_config)
               ? 1
               : 0;
  }
}

int cyrus_optimizer_optimize_simple(CyrusOptimizerContext *ctx,
                                    void *module_ptr, CyrusOptLevel level,
                                    int is_debug) {
  if (!ctx || !ctx->optimizer || !module_ptr) {
    return 0;
  }

  auto *module = reinterpret_cast<llvm::Module *>(module_ptr);
  auto opt_level = static_cast<cyrus::OptimizeLevel>(level);

  return ctx->optimizer->optimizeModuleSimple(*module, opt_level, is_debug != 0)
             ? 1
             : 0;
}

const char *cyrus_module_get_triple(void *module_ptr) {
  if (!module_ptr)
    return nullptr;
  auto *module = reinterpret_cast<llvm::Module *>(module_ptr);
  static std::string triple_str;
  triple_str = module->getTargetTriple().str();
  return triple_str.c_str();
}

void cyrus_module_set_triple(void *module_ptr, const char *triple) {
  if (!module_ptr || !triple)
    return;
  auto *module = reinterpret_cast<llvm::Module *>(module_ptr);
  module->setTargetTriple(Triple(triple));
}

int cyrus_module_verify(void *module_ptr, CyrusErrorMessage *error) {
  if (!module_ptr)
    return 0;
  auto *module = reinterpret_cast<llvm::Module *>(module_ptr);

  std::string error_str;
  raw_string_ostream error_stream(error_str);

  if (verifyModule(*module, &error_stream)) {
    if (error) {
      error->data = static_cast<char *>(malloc(error_str.size() + 1));
      if (error->data) {
        strcpy(error->data, error_str.c_str());
        error->len = error_str.size();
      }
    }
    return 0;
  }

  return 1;
}

void cyrus_error_message_free(CyrusErrorMessage *error) {
  if (error && error->data) {
    free(error->data);
    error->data = nullptr;
    error->len = 0;
  }
}

} // extern "C"