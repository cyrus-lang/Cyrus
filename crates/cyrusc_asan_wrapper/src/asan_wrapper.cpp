#include "llvm/IR/Module.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Instrumentation/AddressSanitizer.h"
#include "llvm/Transforms/Instrumentation/ThreadSanitizer.h"
#include "llvm/Transforms/Instrumentation/MemorySanitizer.h"
#include "llvm/Transforms/Instrumentation/HWAddressSanitizer.h"
#include "llvm/Transforms/Scalar/EarlyCSE.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/JumpThreading.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Analysis/GlobalsModRef.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm-c/Core.h"
#include "llvm-c/TargetMachine.h"
#include <optional>

extern "C"
{

    typedef struct
    {
        bool address_sanitize;
        bool thread_sanitize;
        bool mem_sanitize;
        bool hwaddress_sanitize;
        bool recover;
        bool asan_use_after_scope;
        bool asan_use_after_return;
    } SanitizerOptions;

    void run_sanitizer_passes(LLVMModuleRef module_ref,
                              LLVMTargetMachineRef tm_ref,
                              int opt_level,
                              SanitizerOptions opts)
    {
        if (!module_ref || !tm_ref)
            return;

        llvm::Module *Mod = llvm::unwrap(module_ref);
        llvm::TargetMachine *Machine = llvm::unwrap(tm_ref);

        llvm::PassInstrumentationCallbacks PIC;
        llvm::PipelineTuningOptions PTO{};
        PTO.LoopUnrolling = true;
        PTO.LoopInterleaving = true;
        PTO.LoopVectorization = true;
        PTO.SLPVectorization = true;
        PTO.MergeFunctions = true;

        llvm::PassBuilder PB(Machine, PTO, std::nullopt, &PIC);

        llvm::LoopAnalysisManager LAM;
        llvm::FunctionAnalysisManager FAM;
        llvm::CGSCCAnalysisManager CGAM;
        llvm::ModuleAnalysisManager MAM;

        PB.registerLoopAnalyses(LAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerModuleAnalyses(MAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        llvm::StandardInstrumentations SI(Mod->getContext(), /*debug=*/false, /*verify=*/true);
        SI.registerCallbacks(PIC, &MAM);

        llvm::OptimizationLevel level;
        switch (opt_level)
        {
        case 0:
            level = llvm::OptimizationLevel::O0;
            break;
        case 1:
            level = llvm::OptimizationLevel::O1;
            break;
        case 2:
            level = llvm::OptimizationLevel::O2;
            break;
        case 3:
            level = llvm::OptimizationLevel::O3;
            break;
        default:
            level = llvm::OptimizationLevel::O2;
            break;
        }

        llvm::ModulePassManager MPM = PB.buildPerModuleDefaultPipeline(level, llvm::ThinOrFullLTOPhase::None);

        if (opts.mem_sanitize)
        {
            llvm::MemorySanitizerOptions MSO(/*TrackOrigins=*/true, opts.recover, false, false);
            MPM.addPass(llvm::MemorySanitizerPass(MSO));
        }

        if (opts.thread_sanitize)
        {
            MPM.addPass(llvm::ModuleThreadSanitizerPass());
            MPM.addPass(createModuleToFunctionPassAdaptor(llvm::ThreadSanitizerPass()));
        }

        if (opts.address_sanitize)
        {
            llvm::AddressSanitizerOptions ASO;
            ASO.CompileKernel = false;
            ASO.Recover = opts.recover;
            ASO.UseAfterScope = opts.asan_use_after_scope;
            ASO.UseAfterReturn = opts.asan_use_after_return
                                     ? llvm::AsanDetectStackUseAfterReturnMode::Always
                                     : llvm::AsanDetectStackUseAfterReturnMode::Never;
            bool is_windows = Machine->getTargetTriple().isOSWindows();
            MPM.addPass(llvm::AddressSanitizerPass(ASO, false, !is_windows, llvm::AsanDtorKind::None));
        }

        if (opts.hwaddress_sanitize)
        {
            MPM.addPass(llvm::HWAddressSanitizerPass({false, opts.recover, opt_level == 0}));
        }

        llvm::FunctionPassManager FPM;
        FPM.addPass(llvm::EarlyCSEPass(true));
        FPM.addPass(llvm::InstCombinePass());
        FPM.addPass(llvm::JumpThreadingPass());
        FPM.addPass(llvm::GVNPass());
        FPM.addPass(llvm::InstCombinePass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));

        MPM.run(*Mod, MAM);
    }

} // extern "C"