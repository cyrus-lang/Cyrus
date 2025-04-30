/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_HOME_TAHA_CODE_CYRUS_CYRUS_LANG_BUILD_SRC_PARSER_CYRUS_TAB_HPP_INCLUDED
# define YY_YY_HOME_TAHA_CODE_CYRUS_CYRUS_LANG_BUILD_SRC_PARSER_CYRUS_TAB_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    int_const = 258,               /* int_const  */
    char_const = 259,              /* char_const  */
    float_const = 260,             /* float_const  */
    id = 261,                      /* id  */
    string = 262,                  /* string  */
    enumeration_const = 263,       /* enumeration_const  */
    storage_const = 264,           /* storage_const  */
    type_const = 265,              /* type_const  */
    qual_const = 266,              /* qual_const  */
    struct_const = 267,            /* struct_const  */
    enum_const = 268,              /* enum_const  */
    DEFINE = 269,                  /* DEFINE  */
    IF = 270,                      /* IF  */
    FOR = 271,                     /* FOR  */
    DO = 272,                      /* DO  */
    WHILE = 273,                   /* WHILE  */
    BREAK = 274,                   /* BREAK  */
    SWITCH = 275,                  /* SWITCH  */
    CONTINUE = 276,                /* CONTINUE  */
    RETURN = 277,                  /* RETURN  */
    CASE = 278,                    /* CASE  */
    DEFAULT = 279,                 /* DEFAULT  */
    GOTO = 280,                    /* GOTO  */
    SIZEOF = 281,                  /* SIZEOF  */
    PUNC = 282,                    /* PUNC  */
    or_const = 283,                /* or_const  */
    and_const = 284,               /* and_const  */
    eq_const = 285,                /* eq_const  */
    shift_const = 286,             /* shift_const  */
    rel_const = 287,               /* rel_const  */
    inc_const = 288,               /* inc_const  */
    point_const = 289,             /* point_const  */
    param_const = 290,             /* param_const  */
    ELSE = 291,                    /* ELSE  */
    HEADER = 292                   /* HEADER  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_HOME_TAHA_CODE_CYRUS_CYRUS_LANG_BUILD_SRC_PARSER_CYRUS_TAB_HPP_INCLUDED  */
