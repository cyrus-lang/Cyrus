/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "/home/taha/Code/Cyrus/Cyrus-Lang/src/parser/cyrus.y"

	#include<stdio.h>
	int yylex(void);
	int yyerror(const char *s);
	int success = 1;

#line 78 "/home/taha/Code/Cyrus/Cyrus-Lang/build/src/parser/cyrus.tab.cpp"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "cyrus.tab.hpp"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_int_const = 3,                  /* int_const  */
  YYSYMBOL_char_const = 4,                 /* char_const  */
  YYSYMBOL_float_const = 5,                /* float_const  */
  YYSYMBOL_id = 6,                         /* id  */
  YYSYMBOL_string = 7,                     /* string  */
  YYSYMBOL_enumeration_const = 8,          /* enumeration_const  */
  YYSYMBOL_storage_const = 9,              /* storage_const  */
  YYSYMBOL_type_const = 10,                /* type_const  */
  YYSYMBOL_qual_const = 11,                /* qual_const  */
  YYSYMBOL_struct_const = 12,              /* struct_const  */
  YYSYMBOL_enum_const = 13,                /* enum_const  */
  YYSYMBOL_DEFINE = 14,                    /* DEFINE  */
  YYSYMBOL_IF = 15,                        /* IF  */
  YYSYMBOL_FOR = 16,                       /* FOR  */
  YYSYMBOL_DO = 17,                        /* DO  */
  YYSYMBOL_WHILE = 18,                     /* WHILE  */
  YYSYMBOL_BREAK = 19,                     /* BREAK  */
  YYSYMBOL_SWITCH = 20,                    /* SWITCH  */
  YYSYMBOL_CONTINUE = 21,                  /* CONTINUE  */
  YYSYMBOL_RETURN = 22,                    /* RETURN  */
  YYSYMBOL_CASE = 23,                      /* CASE  */
  YYSYMBOL_DEFAULT = 24,                   /* DEFAULT  */
  YYSYMBOL_GOTO = 25,                      /* GOTO  */
  YYSYMBOL_SIZEOF = 26,                    /* SIZEOF  */
  YYSYMBOL_PUNC = 27,                      /* PUNC  */
  YYSYMBOL_or_const = 28,                  /* or_const  */
  YYSYMBOL_and_const = 29,                 /* and_const  */
  YYSYMBOL_eq_const = 30,                  /* eq_const  */
  YYSYMBOL_shift_const = 31,               /* shift_const  */
  YYSYMBOL_rel_const = 32,                 /* rel_const  */
  YYSYMBOL_inc_const = 33,                 /* inc_const  */
  YYSYMBOL_point_const = 34,               /* point_const  */
  YYSYMBOL_param_const = 35,               /* param_const  */
  YYSYMBOL_ELSE = 36,                      /* ELSE  */
  YYSYMBOL_HEADER = 37,                    /* HEADER  */
  YYSYMBOL_38_ = 38,                       /* '+'  */
  YYSYMBOL_39_ = 39,                       /* '-'  */
  YYSYMBOL_40_ = 40,                       /* '*'  */
  YYSYMBOL_41_ = 41,                       /* '/'  */
  YYSYMBOL_42_then_ = 42,                  /* "then"  */
  YYSYMBOL_43_ = 43,                       /* ';'  */
  YYSYMBOL_44_ = 44,                       /* '{'  */
  YYSYMBOL_45_ = 45,                       /* '}'  */
  YYSYMBOL_46_ = 46,                       /* ','  */
  YYSYMBOL_47_ = 47,                       /* '='  */
  YYSYMBOL_48_ = 48,                       /* ':'  */
  YYSYMBOL_49_ = 49,                       /* '('  */
  YYSYMBOL_50_ = 50,                       /* ')'  */
  YYSYMBOL_51_ = 51,                       /* '['  */
  YYSYMBOL_52_ = 52,                       /* ']'  */
  YYSYMBOL_53_t_ = 53,                     /* 't'  */
  YYSYMBOL_54_ = 54,                       /* '?'  */
  YYSYMBOL_55_ = 55,                       /* '|'  */
  YYSYMBOL_56_ = 56,                       /* '^'  */
  YYSYMBOL_57_ = 57,                       /* '&'  */
  YYSYMBOL_58_ = 58,                       /* '<'  */
  YYSYMBOL_59_ = 59,                       /* '>'  */
  YYSYMBOL_60_ = 60,                       /* '%'  */
  YYSYMBOL_61_ = 61,                       /* '~'  */
  YYSYMBOL_62_ = 62,                       /* '!'  */
  YYSYMBOL_63_ = 63,                       /* '.'  */
  YYSYMBOL_YYACCEPT = 64,                  /* $accept  */
  YYSYMBOL_program_unit = 65,              /* program_unit  */
  YYSYMBOL_translation_unit = 66,          /* translation_unit  */
  YYSYMBOL_external_decl = 67,             /* external_decl  */
  YYSYMBOL_function_definition = 68,       /* function_definition  */
  YYSYMBOL_decl = 69,                      /* decl  */
  YYSYMBOL_decl_list = 70,                 /* decl_list  */
  YYSYMBOL_decl_specs = 71,                /* decl_specs  */
  YYSYMBOL_storage_class_spec = 72,        /* storage_class_spec  */
  YYSYMBOL_type_spec = 73,                 /* type_spec  */
  YYSYMBOL_type_qualifier = 74,            /* type_qualifier  */
  YYSYMBOL_struct_or_union_spec = 75,      /* struct_or_union_spec  */
  YYSYMBOL_struct_or_union = 76,           /* struct_or_union  */
  YYSYMBOL_struct_decl_list = 77,          /* struct_decl_list  */
  YYSYMBOL_init_declarator_list = 78,      /* init_declarator_list  */
  YYSYMBOL_init_declarator = 79,           /* init_declarator  */
  YYSYMBOL_struct_decl = 80,               /* struct_decl  */
  YYSYMBOL_spec_qualifier_list = 81,       /* spec_qualifier_list  */
  YYSYMBOL_struct_declarator_list = 82,    /* struct_declarator_list  */
  YYSYMBOL_struct_declarator = 83,         /* struct_declarator  */
  YYSYMBOL_enum_spec = 84,                 /* enum_spec  */
  YYSYMBOL_enumerator_list = 85,           /* enumerator_list  */
  YYSYMBOL_enumerator = 86,                /* enumerator  */
  YYSYMBOL_declarator = 87,                /* declarator  */
  YYSYMBOL_direct_declarator = 88,         /* direct_declarator  */
  YYSYMBOL_pointer = 89,                   /* pointer  */
  YYSYMBOL_type_qualifier_list = 90,       /* type_qualifier_list  */
  YYSYMBOL_param_type_list = 91,           /* param_type_list  */
  YYSYMBOL_param_list = 92,                /* param_list  */
  YYSYMBOL_param_decl = 93,                /* param_decl  */
  YYSYMBOL_id_list = 94,                   /* id_list  */
  YYSYMBOL_initializer = 95,               /* initializer  */
  YYSYMBOL_initializer_list = 96,          /* initializer_list  */
  YYSYMBOL_type_name = 97,                 /* type_name  */
  YYSYMBOL_abstract_declarator = 98,       /* abstract_declarator  */
  YYSYMBOL_direct_abstract_declarator = 99, /* direct_abstract_declarator  */
  YYSYMBOL_typedef_name = 100,             /* typedef_name  */
  YYSYMBOL_stat = 101,                     /* stat  */
  YYSYMBOL_labeled_stat = 102,             /* labeled_stat  */
  YYSYMBOL_exp_stat = 103,                 /* exp_stat  */
  YYSYMBOL_compound_stat = 104,            /* compound_stat  */
  YYSYMBOL_stat_list = 105,                /* stat_list  */
  YYSYMBOL_selection_stat = 106,           /* selection_stat  */
  YYSYMBOL_iteration_stat = 107,           /* iteration_stat  */
  YYSYMBOL_jump_stat = 108,                /* jump_stat  */
  YYSYMBOL_exp = 109,                      /* exp  */
  YYSYMBOL_assignment_exp = 110,           /* assignment_exp  */
  YYSYMBOL_assignment_operator = 111,      /* assignment_operator  */
  YYSYMBOL_conditional_exp = 112,          /* conditional_exp  */
  YYSYMBOL_const_exp = 113,                /* const_exp  */
  YYSYMBOL_logical_or_exp = 114,           /* logical_or_exp  */
  YYSYMBOL_logical_and_exp = 115,          /* logical_and_exp  */
  YYSYMBOL_inclusive_or_exp = 116,         /* inclusive_or_exp  */
  YYSYMBOL_exclusive_or_exp = 117,         /* exclusive_or_exp  */
  YYSYMBOL_and_exp = 118,                  /* and_exp  */
  YYSYMBOL_equality_exp = 119,             /* equality_exp  */
  YYSYMBOL_relational_exp = 120,           /* relational_exp  */
  YYSYMBOL_shift_expression = 121,         /* shift_expression  */
  YYSYMBOL_additive_exp = 122,             /* additive_exp  */
  YYSYMBOL_mult_exp = 123,                 /* mult_exp  */
  YYSYMBOL_cast_exp = 124,                 /* cast_exp  */
  YYSYMBOL_unary_exp = 125,                /* unary_exp  */
  YYSYMBOL_unary_operator = 126,           /* unary_operator  */
  YYSYMBOL_postfix_exp = 127,              /* postfix_exp  */
  YYSYMBOL_primary_exp = 128,              /* primary_exp  */
  YYSYMBOL_argument_exp_list = 129,        /* argument_exp_list  */
  YYSYMBOL_consts = 130                    /* consts  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  44
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   1216

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  64
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  67
/* YYNRULES -- Number of rules.  */
#define YYNRULES  198
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  351

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   293


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    62,     2,     2,     2,    60,    57,     2,
      49,    50,    40,    38,    46,    39,    63,    41,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    48,    43,
      58,    47,    59,    54,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    51,     2,    52,    56,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,    53,     2,     2,     2,
       2,     2,     2,    44,    55,    45,    61,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    42
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    18,    18,    19,    20,    22,    23,    25,    26,    28,
      29,    30,    31,    33,    34,    36,    37,    39,    40,    41,
      42,    43,    44,    46,    48,    49,    50,    51,    53,    55,
      56,    57,    59,    61,    62,    64,    65,    67,    68,    70,
      72,    73,    74,    75,    77,    78,    80,    81,    82,    84,
      85,    86,    88,    89,    91,    92,    94,    95,    97,    98,
      99,   100,   101,   102,   103,   105,   106,   107,   108,   110,
     111,   113,   114,   116,   117,   119,   120,   121,   123,   124,
     126,   127,   128,   130,   131,   133,   134,   136,   137,   138,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   150,
     152,   153,   154,   155,   156,   157,   159,   160,   161,   163,
     164,   166,   167,   168,   169,   171,   172,   174,   175,   176,
     178,   179,   180,   181,   182,   183,   184,   185,   186,   187,
     189,   190,   191,   192,   193,   195,   196,   198,   199,   201,
     202,   204,   205,   207,   209,   210,   212,   213,   215,   216,
     218,   219,   221,   222,   224,   225,   227,   228,   229,   230,
     232,   233,   235,   236,   237,   239,   240,   241,   242,   244,
     245,   247,   248,   249,   250,   251,   253,   253,   253,   253,
     253,   253,   255,   256,   257,   258,   259,   260,   261,   263,
     264,   265,   266,   268,   269,   271,   272,   273,   274
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "int_const",
  "char_const", "float_const", "id", "string", "enumeration_const",
  "storage_const", "type_const", "qual_const", "struct_const",
  "enum_const", "DEFINE", "IF", "FOR", "DO", "WHILE", "BREAK", "SWITCH",
  "CONTINUE", "RETURN", "CASE", "DEFAULT", "GOTO", "SIZEOF", "PUNC",
  "or_const", "and_const", "eq_const", "shift_const", "rel_const",
  "inc_const", "point_const", "param_const", "ELSE", "HEADER", "'+'",
  "'-'", "'*'", "'/'", "\"then\"", "';'", "'{'", "'}'", "','", "'='",
  "':'", "'('", "')'", "'['", "']'", "'t'", "'?'", "'|'", "'^'", "'&'",
  "'<'", "'>'", "'%'", "'~'", "'!'", "'.'", "$accept", "program_unit",
  "translation_unit", "external_decl", "function_definition", "decl",
  "decl_list", "decl_specs", "storage_class_spec", "type_spec",
  "type_qualifier", "struct_or_union_spec", "struct_or_union",
  "struct_decl_list", "init_declarator_list", "init_declarator",
  "struct_decl", "spec_qualifier_list", "struct_declarator_list",
  "struct_declarator", "enum_spec", "enumerator_list", "enumerator",
  "declarator", "direct_declarator", "pointer", "type_qualifier_list",
  "param_type_list", "param_list", "param_decl", "id_list", "initializer",
  "initializer_list", "type_name", "abstract_declarator",
  "direct_abstract_declarator", "typedef_name", "stat", "labeled_stat",
  "exp_stat", "compound_stat", "stat_list", "selection_stat",
  "iteration_stat", "jump_stat", "exp", "assignment_exp",
  "assignment_operator", "conditional_exp", "const_exp", "logical_or_exp",
  "logical_and_exp", "inclusive_or_exp", "exclusive_or_exp", "and_exp",
  "equality_exp", "relational_exp", "shift_expression", "additive_exp",
  "mult_exp", "cast_exp", "unary_exp", "unary_operator", "postfix_exp",
  "primary_exp", "argument_exp_list", "consts", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-172)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-1)

#define yytable_value_is_error(Yyn) \
  0

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
     258,  -172,  -172,  -172,  -172,  -172,     9,    69,   258,    22,
      48,  -172,     6,   304,  -172,  -172,  -172,   115,   648,   648,
     648,  -172,    11,  -172,   159,   231,    20,  -172,    12,    60,
    -172,  -172,  -172,  -172,  -172,  -172,  1092,   258,  -172,  -172,
    -172,  -172,    22,    52,  -172,  -172,  -172,    21,  -172,   599,
    -172,  -172,  -172,    34,    31,   316,  -172,   159,   115,  -172,
     820,   677,   231,    60,    78,   -15,  -172,  1139,  1154,  -172,
    -172,  -172,   592,  -172,  -172,  -172,   176,  -172,  -172,    -8,
      81,    75,    83,    92,   148,    53,   154,   121,    97,  -172,
       1,  1092,    93,  -172,  -172,  -172,  -172,  -172,  -172,    48,
     692,   159,  -172,    31,    31,    31,   230,  -172,   134,   157,
     196,   232,   532,   239,   287,   309,   324,   739,  1092,   202,
     362,  -172,  -172,   376,  -172,  -172,  -172,  -172,   436,  -172,
    -172,  -172,   105,  -172,  -172,   323,  -172,  -172,   101,   321,
     326,  -172,   201,  -172,  -172,   322,  -172,   263,  1092,  -172,
      60,   592,  -172,  -172,   126,   325,  1092,  -172,  1092,  1092,
    1092,  1092,  1092,  1092,  1092,  1092,  1092,  1092,  1092,  1092,
    1092,  1092,  1092,  1092,  -172,  -172,  1092,  -172,  -172,   370,
     754,  1092,   384,  -172,   692,  -172,  -172,  -172,   695,  -172,
    -172,  -172,  -172,  1092,   168,  -172,   355,   532,  1092,   802,
     392,  1092,  -172,  1092,  -172,  -172,   170,   363,   532,   369,
    -172,   484,  -172,  -172,  -172,   573,   817,  -172,    19,  -172,
     296,  -172,   395,   407,  -172,  -172,  -172,  -172,  -172,   367,
    1157,   299,  -172,  1092,  -172,    81,   318,    75,    83,    92,
     148,    53,   154,   154,   154,   121,    97,    97,  -172,  -172,
    -172,  -172,  -172,  -172,  -172,   207,    76,  -172,  -172,   306,
    -172,  -172,  -172,   134,  1092,  -172,   210,   877,   192,   373,
     227,   228,  -172,   532,  -172,  -172,  -172,  -172,   368,   374,
    -172,   371,   296,   293,   892,  -172,  -172,  -172,  -172,  -172,
    1092,  1092,  -172,  -172,  -172,   630,  -172,  -172,   532,   952,
     216,   967,  1092,   532,   532,  -172,  -172,  -172,  -172,  -172,
     377,  -172,   379,  -172,  -172,  -172,  -172,   390,   532,   241,
    1014,  1029,   233,   243,  -172,  -172,  -172,  -172,   532,  -172,
     532,   532,   244,   532,   250,  1077,   385,  -172,  -172,  -172,
     532,  -172,   532,   532,   251,  -172,  -172,  -172,  -172,   532,
    -172
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,    58,    23,    24,    28,    32,     0,     0,     0,    66,
       0,    99,     0,     4,     5,     7,     8,     0,    18,    20,
      22,    25,     0,    26,     0,    57,     0,    27,    51,     0,
     195,   196,   197,   189,   191,   198,     0,     0,   190,     2,
      69,    68,    65,     0,     1,     6,    14,     0,    35,    37,
      17,    19,    21,    31,     0,     0,    15,     0,     0,    12,
       0,     0,    56,     0,    54,     0,    52,     0,     0,   178,
     179,   177,     0,   176,   180,   181,     0,   135,   137,   141,
     144,   146,   148,   150,   152,   154,   156,   160,   162,   165,
     169,     0,   171,   182,     3,    70,    67,    59,    13,     0,
       0,     0,    11,     0,    41,    43,     0,    33,     0,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   110,   114,     0,   115,   100,   101,   102,     0,   103,
     104,   105,     0,    16,    10,    37,    78,    64,    77,     0,
      71,    73,     0,    61,   143,     0,   169,     0,     0,    50,
       0,     0,   174,   172,    86,     0,     0,   192,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   139,   140,     0,   173,   188,     0,
       0,     0,     0,    36,     0,    38,    80,     9,     0,    40,
      42,    30,    34,     0,     0,    44,    46,     0,     0,     0,
       0,     0,   132,     0,   131,   134,     0,     0,     0,     0,
     113,     0,   112,   116,   109,     0,     0,    75,    87,    76,
      89,    62,     0,     0,    63,    60,    49,    55,    53,     0,
       0,    87,    85,     0,   136,   145,     0,   147,   149,   151,
     153,   155,   159,   157,   158,   161,   163,   164,   166,   167,
     168,   138,   187,   185,   193,     0,     0,   186,    83,     0,
      29,    48,    39,     0,     0,   106,     0,     0,     0,     0,
       0,     0,   133,     0,   108,   130,   111,    98,     0,     0,
      94,     0,    88,     0,     0,    72,    74,    79,   175,   170,
       0,     0,   184,   183,    81,     0,    45,    47,     0,     0,
       0,     0,     0,     0,     0,   107,    96,    90,    92,    97,
       0,    93,     0,   142,   194,    82,    84,   117,     0,     0,
       0,     0,     0,     0,   120,   119,    95,    91,     0,   129,
       0,     0,     0,     0,     0,     0,     0,   118,   128,   127,
       0,   125,     0,     0,     0,   121,   126,   124,   123,     0,
     122
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -172,    42,  -172,   419,  -172,   -10,   132,     3,  -172,   -45,
      29,  -172,  -172,   331,  -172,   336,  -101,   -53,  -172,   173,
    -172,   382,   297,    -9,   -16,    -7,  -172,   -56,  -172,   224,
    -172,  -171,  -172,   298,   -18,   -11,  -172,   -94,  -172,  -172,
       8,   327,  -172,  -172,  -172,   -36,   -76,  -172,   -32,  -111,
    -172,   305,   307,   303,   308,   302,   313,    63,   300,   193,
     -79,    28,  -172,  -172,   459,  -172,  -172
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,    12,    13,    14,    15,    16,    57,    58,    18,    19,
      20,    21,    22,   106,    47,    48,   107,   108,   194,   195,
      23,    65,    66,    24,    25,    26,    42,   278,   140,   141,
     142,   185,   259,   155,   279,   220,    27,   124,   125,   126,
     127,   128,   129,   130,   131,   132,    77,   176,    78,   145,
      79,    80,    81,    82,    83,    84,    85,    86,    87,    88,
      89,    90,    91,    92,    93,   255,    38
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      76,    43,    41,    17,   139,   192,    44,   207,    49,   104,
      62,    17,   177,   258,    56,    28,    17,    53,   200,   154,
     158,    50,    51,    52,   186,     1,     1,   104,   174,   144,
     149,   150,    59,     4,   213,    96,    76,   227,    40,    56,
      17,     3,     4,     5,     6,    56,   159,   133,   175,   135,
      39,   189,   190,    29,     1,    54,    63,   102,   104,   104,
     104,   104,     9,   138,    98,   134,    64,    99,   215,    10,
     216,    95,    30,    31,    32,    33,    34,    35,   103,    94,
     234,   206,   261,   105,    11,   165,   144,   192,     9,   146,
     135,   133,   248,   249,   250,   152,   153,    10,   154,   196,
     251,   105,    97,   265,   254,   281,   104,     1,   186,   187,
     160,   166,   167,   133,   274,    76,   144,   213,    36,   146,
     219,     1,   156,   236,   316,   148,   178,   179,   293,   217,
     161,   218,   105,   105,   105,   105,   232,   171,   172,   162,
       1,     9,   180,   104,   181,   256,   146,   231,   214,   163,
     215,   156,   216,   297,   289,     9,   182,   173,    46,   169,
     170,   144,   266,   268,    10,   270,     9,   271,     2,     3,
       4,     5,     6,   312,     9,   230,   146,   216,   164,   305,
     105,   101,   193,    10,   144,   168,   146,   123,   146,   146,
     146,   146,   146,   146,   146,   146,   146,   146,   146,   146,
     146,   146,    62,    55,   317,   197,    43,   282,   218,   324,
     325,   262,    11,   272,   263,   314,   156,   105,   138,   186,
     282,   146,   156,   231,   329,   138,   157,   310,   242,   243,
     244,   300,   144,   138,   337,   301,   338,   339,   156,   341,
       3,     4,     5,     6,   146,   198,   346,   223,   347,   348,
     208,   224,   144,   291,   196,   350,   156,   292,   313,   320,
     298,   146,   156,   319,     1,   322,   323,     2,     3,     4,
       5,     6,     7,   156,   156,   191,   335,   303,   304,   156,
      60,   199,    61,    11,   332,   334,   138,   156,   201,   156,
     156,   330,   146,   336,   340,     8,   156,   156,     9,   344,
     342,   349,     2,     3,     4,     5,     6,    10,   226,   150,
       1,    11,   146,     2,     3,     4,     5,     6,   146,    30,
      31,    32,   109,    34,    35,     2,     3,     4,     5,     6,
     202,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    67,   309,     9,   283,    11,   284,   230,    68,
     216,   294,   295,    10,    69,    70,    71,    11,   203,   121,
      55,   122,   246,   247,   156,    72,   290,   204,   209,    11,
     100,   221,   222,    73,   225,   233,   252,    74,    75,    30,
      31,    32,   109,    34,    35,     2,     3,     4,     5,     6,
     257,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    67,   264,     2,     3,     4,     5,     6,    68,
     269,   273,   275,   287,    69,    70,    71,   288,   306,   121,
      55,   210,   302,   308,   307,    72,   328,   326,   345,    11,
     285,   327,    45,    73,   188,   183,   296,    74,    75,    30,
      31,    32,   109,    34,    35,   147,   286,   228,    11,   229,
     211,   110,   111,   112,   113,   114,   115,   116,   117,   118,
     119,   120,    67,   235,   238,   240,    37,   237,   245,    68,
     239,     0,     0,     0,    69,    70,    71,   241,     0,   121,
      55,   212,     0,     0,     0,    72,     0,    30,    31,    32,
     109,    34,    35,    73,     0,     0,     0,    74,    75,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
      67,     0,     0,     0,     0,     0,     0,    68,     0,     0,
       0,     0,    69,    70,    71,     0,     0,   121,    55,   276,
       0,     0,     0,    72,     0,    30,    31,    32,   109,    34,
      35,    73,     0,     0,     0,    74,    75,   110,   111,   112,
     113,   114,   115,   116,   117,   118,   119,   120,    67,     0,
       0,     0,     0,     0,     0,    68,     0,     0,     0,     0,
      69,    70,    71,     0,     0,   121,    55,     0,     0,     1,
       0,    72,     2,     3,     4,     5,     6,     0,     0,    73,
       0,     0,     0,    74,    75,    30,    31,    32,    33,    34,
      35,     0,     3,     4,     5,     6,     0,     0,     2,     3,
       4,     5,     6,     9,     0,     0,     0,     0,    67,     0,
       0,     0,   215,   277,   216,    68,    11,     0,     0,     0,
      69,    70,    71,    30,    31,    32,    33,    34,    35,     0,
       0,    72,     0,    55,     0,    11,   100,     0,     0,    73,
       0,     0,    11,    74,    75,     0,    67,     2,     3,     4,
       5,     6,     0,    68,     0,     0,     0,     0,    69,    70,
      71,     0,     0,     0,   184,   315,     0,     0,     0,    72,
      30,    31,    32,    33,    34,    35,     0,    73,     0,     0,
       0,    74,    75,     0,     0,    30,    31,    32,    33,    34,
      35,    11,     0,    67,     0,     3,     4,     5,     6,     0,
      68,     0,     0,     0,     0,    69,    70,    71,    67,     0,
       0,     0,     0,     0,     0,    68,    72,     0,     0,   143,
      69,    70,    71,     0,    73,     0,   184,     0,    74,    75,
     260,    72,    30,    31,    32,    33,    34,    35,    11,    73,
       0,     0,     0,    74,    75,     0,     0,    30,    31,    32,
      33,    34,    35,     0,     0,    67,     0,     0,     0,     0,
       0,     0,    68,     0,     0,     0,     0,    69,    70,    71,
      67,     0,   205,     0,     0,     0,     0,    68,    72,     0,
       0,     0,    69,    70,    71,     0,    73,     0,     0,     0,
      74,    75,     0,    72,   253,    30,    31,    32,    33,    34,
      35,    73,     0,     0,     0,    74,    75,     0,     0,     0,
      30,    31,    32,    33,    34,    35,   136,     0,    67,     2,
       3,     4,     5,     6,     0,    68,     0,     0,     0,     0,
      69,    70,    71,    67,     0,   267,     0,     0,     0,     0,
      68,    72,     0,     0,     0,    69,    70,    71,     0,    73,
       0,     0,     0,    74,    75,     0,    72,     0,     0,   280,
     137,     0,     0,    11,    73,     0,     0,     0,    74,    75,
      30,    31,    32,    33,    34,    35,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,     0,     0,    67,     0,     0,     0,     0,     0,     0,
      68,     0,     0,     0,     0,    69,    70,    71,    67,     0,
     299,     0,     0,     0,     0,    68,    72,     0,     0,     0,
      69,    70,    71,     0,    73,     0,     0,     0,    74,    75,
       0,    72,     0,     0,   311,     0,     0,     0,     0,    73,
       0,     0,     0,    74,    75,    30,    31,    32,    33,    34,
      35,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,    33,    34,    35,     0,     0,    67,     0,
       0,     0,     0,     0,     0,    68,     0,     0,     0,     0,
      69,    70,    71,    67,     0,     0,     0,     0,     0,     0,
      68,    72,   318,     0,     0,    69,    70,    71,     0,    73,
     321,     0,     0,    74,    75,     0,    72,    30,    31,    32,
      33,    34,    35,     0,    73,     0,     0,     0,    74,    75,
       0,     0,    30,    31,    32,    33,    34,    35,     0,     0,
      67,     0,     0,     0,     0,     0,     0,    68,     0,     0,
       0,     0,    69,    70,    71,    67,     0,     0,     0,     0,
       0,     0,    68,    72,   331,     0,     0,    69,    70,    71,
       0,    73,     0,     0,     0,    74,    75,     0,    72,   333,
      30,    31,    32,    33,    34,    35,    73,     0,     0,     0,
      74,    75,     0,     0,     0,    30,    31,    32,    33,    34,
      35,     0,     0,    67,     0,     0,     0,     0,     0,     0,
      68,     0,     0,     0,     0,    69,    70,    71,    67,     0,
       0,     0,     0,     0,     0,    68,    72,   343,     0,     0,
      69,    70,    71,     0,    73,     0,     0,     0,    74,    75,
       0,    72,    30,    31,    32,    33,    34,    35,     0,    73,
       0,     0,     0,    74,    75,     0,     0,    30,    31,    32,
      33,    34,    35,     0,     0,    67,     2,     3,     4,     5,
       6,     0,    68,     0,     0,     0,     0,    69,    70,    71,
      67,     0,     0,     0,     0,     0,     0,    68,   151,     0,
       0,     0,    69,    70,    71,     0,    73,     9,     0,     0,
      74,    75,     0,    36,     0,     0,   230,   277,   216,     0,
      11,    73,     0,     0,     0,    74,    75
};

static const yytype_int16 yycheck[] =
{
      36,    10,     9,     0,    60,   106,     0,   118,    17,    54,
      26,     8,    91,   184,    24,     6,    13,     6,   112,    72,
      28,    18,    19,    20,   100,     6,     6,    72,    27,    61,
      45,    46,    24,    11,   128,    42,    72,   148,     9,    49,
      37,    10,    11,    12,    13,    55,    54,    57,    47,    58,
       8,   104,   105,    44,     6,    44,    44,    49,   103,   104,
     105,   106,    40,    60,    43,    57,     6,    46,    49,    49,
      51,    42,     3,     4,     5,     6,     7,     8,    44,    37,
     156,   117,   193,    54,    53,    32,   118,   188,    40,    61,
      99,   101,   171,   172,   173,    67,    68,    49,   151,   108,
     176,    72,    50,   197,   180,   216,   151,     6,   184,   101,
      29,    58,    59,   123,   208,   151,   148,   211,    49,    91,
     138,     6,    46,   159,   295,    47,    33,    34,    52,   138,
      55,   138,   103,   104,   105,   106,   154,    40,    41,    56,
       6,    40,    49,   188,    51,   181,   118,   154,    43,    57,
      49,    46,    51,   264,   233,    40,    63,    60,    43,    38,
      39,   193,   198,   199,    49,   201,    40,   203,     9,    10,
      11,    12,    13,   284,    40,    49,   148,    51,    30,   273,
     151,    49,    48,    49,   216,    31,   158,    55,   160,   161,
     162,   163,   164,   165,   166,   167,   168,   169,   170,   171,
     172,   173,   218,    44,   298,    48,   215,   218,   215,   303,
     304,    43,    53,    43,    46,   291,    46,   188,   215,   295,
     231,   193,    46,   230,   318,   222,    50,   283,   165,   166,
     167,   267,   264,   230,   328,    43,   330,   331,    46,   333,
      10,    11,    12,    13,   216,    49,   340,    46,   342,   343,
      48,    50,   284,    46,   263,   349,    46,    50,   290,    43,
      50,   233,    46,   299,     6,   301,   302,     9,    10,    11,
      12,    13,    14,    46,    46,    45,    43,    50,    50,    46,
      49,    49,    51,    53,   320,   321,   283,    46,    49,    46,
      46,    50,   264,    50,    50,    37,    46,    46,    40,   335,
      50,    50,     9,    10,    11,    12,    13,    49,    45,    46,
       6,    53,   284,     9,    10,    11,    12,    13,   290,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      43,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    50,    40,    49,    53,    51,    49,    33,
      51,    45,    46,    49,    38,    39,    40,    53,    49,    43,
      44,    45,   169,   170,    46,    49,    48,    43,     6,    53,
      47,    50,    46,    57,    52,    50,     6,    61,    62,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       6,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    48,     9,    10,    11,    12,    13,    33,
      18,    48,    43,     6,    38,    39,    40,    50,    50,    43,
      44,    45,    49,    52,    50,    49,    36,    50,    43,    53,
      35,    52,    13,    57,   103,    99,   263,    61,    62,     3,
       4,     5,     6,     7,     8,    63,   222,   150,    53,   151,
     123,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,   158,   161,   163,     7,   160,   168,    33,
     162,    -1,    -1,    -1,    38,    39,    40,   164,    -1,    43,
      44,    45,    -1,    -1,    -1,    49,    -1,     3,     4,     5,
       6,     7,     8,    57,    -1,    -1,    -1,    61,    62,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    -1,    -1,
      -1,    -1,    38,    39,    40,    -1,    -1,    43,    44,    45,
      -1,    -1,    -1,    49,    -1,     3,     4,     5,     6,     7,
       8,    57,    -1,    -1,    -1,    61,    62,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    -1,    -1,    -1,    33,    -1,    -1,    -1,    -1,
      38,    39,    40,    -1,    -1,    43,    44,    -1,    -1,     6,
      -1,    49,     9,    10,    11,    12,    13,    -1,    -1,    57,
      -1,    -1,    -1,    61,    62,     3,     4,     5,     6,     7,
       8,    -1,    10,    11,    12,    13,    -1,    -1,     9,    10,
      11,    12,    13,    40,    -1,    -1,    -1,    -1,    26,    -1,
      -1,    -1,    49,    50,    51,    33,    53,    -1,    -1,    -1,
      38,    39,    40,     3,     4,     5,     6,     7,     8,    -1,
      -1,    49,    -1,    44,    -1,    53,    47,    -1,    -1,    57,
      -1,    -1,    53,    61,    62,    -1,    26,     9,    10,    11,
      12,    13,    -1,    33,    -1,    -1,    -1,    -1,    38,    39,
      40,    -1,    -1,    -1,    44,    45,    -1,    -1,    -1,    49,
       3,     4,     5,     6,     7,     8,    -1,    57,    -1,    -1,
      -1,    61,    62,    -1,    -1,     3,     4,     5,     6,     7,
       8,    53,    -1,    26,    -1,    10,    11,    12,    13,    -1,
      33,    -1,    -1,    -1,    -1,    38,    39,    40,    26,    -1,
      -1,    -1,    -1,    -1,    -1,    33,    49,    -1,    -1,    52,
      38,    39,    40,    -1,    57,    -1,    44,    -1,    61,    62,
      45,    49,     3,     4,     5,     6,     7,     8,    53,    57,
      -1,    -1,    -1,    61,    62,    -1,    -1,     3,     4,     5,
       6,     7,     8,    -1,    -1,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    -1,    -1,    -1,    -1,    38,    39,    40,
      26,    -1,    43,    -1,    -1,    -1,    -1,    33,    49,    -1,
      -1,    -1,    38,    39,    40,    -1,    57,    -1,    -1,    -1,
      61,    62,    -1,    49,    50,     3,     4,     5,     6,     7,
       8,    57,    -1,    -1,    -1,    61,    62,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     6,    -1,    26,     9,
      10,    11,    12,    13,    -1,    33,    -1,    -1,    -1,    -1,
      38,    39,    40,    26,    -1,    43,    -1,    -1,    -1,    -1,
      33,    49,    -1,    -1,    -1,    38,    39,    40,    -1,    57,
      -1,    -1,    -1,    61,    62,    -1,    49,    -1,    -1,    52,
      50,    -1,    -1,    53,    57,    -1,    -1,    -1,    61,    62,
       3,     4,     5,     6,     7,     8,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,    -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    -1,    -1,    -1,    -1,    38,    39,    40,    26,    -1,
      43,    -1,    -1,    -1,    -1,    33,    49,    -1,    -1,    -1,
      38,    39,    40,    -1,    57,    -1,    -1,    -1,    61,    62,
      -1,    49,    -1,    -1,    52,    -1,    -1,    -1,    -1,    57,
      -1,    -1,    -1,    61,    62,     3,     4,     5,     6,     7,
       8,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,    -1,    -1,    26,    -1,
      -1,    -1,    -1,    -1,    -1,    33,    -1,    -1,    -1,    -1,
      38,    39,    40,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    49,    50,    -1,    -1,    38,    39,    40,    -1,    57,
      43,    -1,    -1,    61,    62,    -1,    49,     3,     4,     5,
       6,     7,     8,    -1,    57,    -1,    -1,    -1,    61,    62,
      -1,    -1,     3,     4,     5,     6,     7,     8,    -1,    -1,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    -1,    -1,
      -1,    -1,    38,    39,    40,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    33,    49,    50,    -1,    -1,    38,    39,    40,
      -1,    57,    -1,    -1,    -1,    61,    62,    -1,    49,    50,
       3,     4,     5,     6,     7,     8,    57,    -1,    -1,    -1,
      61,    62,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,    -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      33,    -1,    -1,    -1,    -1,    38,    39,    40,    26,    -1,
      -1,    -1,    -1,    -1,    -1,    33,    49,    50,    -1,    -1,
      38,    39,    40,    -1,    57,    -1,    -1,    -1,    61,    62,
      -1,    49,     3,     4,     5,     6,     7,     8,    -1,    57,
      -1,    -1,    -1,    61,    62,    -1,    -1,     3,     4,     5,
       6,     7,     8,    -1,    -1,    26,     9,    10,    11,    12,
      13,    -1,    33,    -1,    -1,    -1,    -1,    38,    39,    40,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    33,    49,    -1,
      -1,    -1,    38,    39,    40,    -1,    57,    40,    -1,    -1,
      61,    62,    -1,    49,    -1,    -1,    49,    50,    51,    -1,
      53,    57,    -1,    -1,    -1,    61,    62
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     6,     9,    10,    11,    12,    13,    14,    37,    40,
      49,    53,    65,    66,    67,    68,    69,    71,    72,    73,
      74,    75,    76,    84,    87,    88,    89,   100,     6,    44,
       3,     4,     5,     6,     7,     8,    49,   128,   130,    65,
      74,    89,    90,    87,     0,    67,    43,    78,    79,    87,
      71,    71,    71,     6,    44,    44,    69,    70,    71,   104,
      49,    51,    88,    44,     6,    85,    86,    26,    33,    38,
      39,    40,    49,    57,    61,    62,   109,   110,   112,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,    65,    74,    89,    50,    43,    46,
      47,    70,   104,    44,    73,    74,    77,    80,    81,     6,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    43,    45,    70,   101,   102,   103,   104,   105,   106,
     107,   108,   109,    69,   104,    87,     6,    50,    71,    91,
      92,    93,    94,    52,   112,   113,   125,    85,    47,    45,
      46,    49,   125,   125,    81,    97,    46,    50,    28,    54,
      29,    55,    56,    57,    30,    32,    58,    59,    31,    38,
      39,    40,    41,    60,    27,    47,   111,   124,    33,    34,
      49,    51,    63,    79,    44,    95,   110,   104,    77,    81,
      81,    45,    80,    48,    82,    83,    87,    48,    49,    49,
     101,    49,    43,    49,    43,    43,   109,   113,    48,     6,
      45,   105,    45,   101,    43,    49,    51,    87,    89,    98,
      99,    50,    46,    46,    50,    52,    45,   113,    86,    97,
      49,    89,    98,    50,   110,   115,   109,   116,   117,   118,
     119,   120,   121,   121,   121,   122,   123,   123,   124,   124,
     124,   110,     6,    50,   110,   129,   109,     6,    95,    96,
      45,   113,    43,    46,    48,   101,   109,    43,   109,    18,
     109,   109,    43,    48,   101,    43,    45,    50,    91,    98,
      52,   113,    99,    49,    51,    35,    93,     6,    50,   124,
      48,    46,    50,    52,    45,    46,    83,   113,    50,    43,
     109,    43,    49,    50,    50,   101,    50,    50,    52,    50,
      91,    52,   113,   112,   110,    45,    95,   101,    50,   109,
      43,    43,   109,   109,   101,   101,    50,    52,    36,   101,
      50,    50,   109,    50,   109,    43,    50,   101,   101,   101,
      50,   101,    50,    50,   109,    43,   101,   101,   101,    50,
     101
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_uint8 yyr1[] =
{
       0,    64,    65,    65,    65,    66,    66,    67,    67,    68,
      68,    68,    68,    69,    69,    70,    70,    71,    71,    71,
      71,    71,    71,    72,    73,    73,    73,    73,    74,    75,
      75,    75,    76,    77,    77,    78,    78,    79,    79,    80,
      81,    81,    81,    81,    82,    82,    83,    83,    83,    84,
      84,    84,    85,    85,    86,    86,    87,    87,    88,    88,
      88,    88,    88,    88,    88,    89,    89,    89,    89,    90,
      90,    91,    91,    92,    92,    93,    93,    93,    94,    94,
      95,    95,    95,    96,    96,    97,    97,    98,    98,    98,
      99,    99,    99,    99,    99,    99,    99,    99,    99,   100,
     101,   101,   101,   101,   101,   101,   102,   102,   102,   103,
     103,   104,   104,   104,   104,   105,   105,   106,   106,   106,
     107,   107,   107,   107,   107,   107,   107,   107,   107,   107,
     108,   108,   108,   108,   108,   109,   109,   110,   110,   111,
     111,   112,   112,   113,   114,   114,   115,   115,   116,   116,
     117,   117,   118,   118,   119,   119,   120,   120,   120,   120,
     121,   121,   122,   122,   122,   123,   123,   123,   123,   124,
     124,   125,   125,   125,   125,   125,   126,   126,   126,   126,
     126,   126,   127,   127,   127,   127,   127,   127,   127,   128,
     128,   128,   128,   129,   129,   130,   130,   130,   130
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     2,     3,     1,     1,     2,     1,     1,     4,
       3,     3,     2,     3,     2,     1,     2,     2,     1,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     5,
       4,     2,     1,     1,     2,     1,     3,     1,     3,     3,
       2,     1,     2,     1,     1,     3,     1,     3,     2,     5,
       4,     2,     1,     3,     1,     3,     2,     1,     1,     3,
       4,     3,     4,     4,     3,     2,     1,     3,     2,     1,
       2,     1,     3,     1,     3,     2,     2,     1,     1,     3,
       1,     3,     4,     1,     3,     2,     1,     1,     2,     1,
       3,     4,     3,     3,     2,     4,     3,     3,     2,     1,
       1,     1,     1,     1,     1,     1,     3,     4,     3,     2,
       1,     4,     3,     3,     2,     1,     2,     5,     7,     5,
       5,     7,     9,     8,     8,     7,     8,     7,     7,     6,
       3,     2,     2,     3,     2,     1,     3,     1,     3,     1,
       1,     1,     5,     1,     1,     3,     1,     3,     1,     3,
       1,     3,     1,     3,     1,     3,     1,     3,     3,     3,
       1,     3,     1,     3,     3,     1,     3,     3,     3,     1,
       4,     1,     2,     2,     2,     4,     1,     1,     1,     1,
       1,     1,     1,     4,     4,     3,     3,     3,     2,     1,
       1,     1,     3,     1,     3,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)




# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */

  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {

#line 1894 "/home/taha/Code/Cyrus/Cyrus-Lang/build/src/parser/cyrus.tab.cpp"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 276 "/home/taha/Code/Cyrus/Cyrus-Lang/src/parser/cyrus.y"


int main()
{
    yyparse();
    if(success)
    	printf("Parsing Successful\n");
    return 0;
}

int yyerror(const char *msg)
{
	extern int yylineno;
	printf("Parsing Failed\nLine Number: %d %s\n",yylineno,msg);
	success = 0;
	return 0;
}
