/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    RPAREN = 258,
    LPAREN = 259,
    DASH = 260,
    rwDEFINE = 261,
    rwACTION = 262,
    rwPARAMS = 263,
    rwEFFECT = 264,
    rwPRECOND = 265,
    rwPREDICATES = 266,
    rwREQUIREMENTS = 267,
    rwTYPES = 268,
    rwOBJECTS = 269,
    rwINIT = 270,
    rwGOAL = 271,
    rwDOMAIN = 272,
    rwTYPING = 273,
    rwAND = 274,
    rwOR = 275,
    rwWHEN = 276,
    rwNOT = 277,
    rwIMPLY = 278,
    rwFORALL = 279,
    rwPROBLEM = 280,
    EQUA = 281,
    rwEXISTS = 282,
    rwLENGTH = 283,
    rwCONSTANTS = 284,
    rwEITHER = 285,
    rwINCREASE = 286,
    rwMETRIC = 287,
    rwMINIMIZE = 288,
    ID = 289,
    VAR = 290,
    INT = 291,
    rwFUNCTIONS = 292
  };
#endif
/* Tokens.  */
#define RPAREN 258
#define LPAREN 259
#define DASH 260
#define rwDEFINE 261
#define rwACTION 262
#define rwPARAMS 263
#define rwEFFECT 264
#define rwPRECOND 265
#define rwPREDICATES 266
#define rwREQUIREMENTS 267
#define rwTYPES 268
#define rwOBJECTS 269
#define rwINIT 270
#define rwGOAL 271
#define rwDOMAIN 272
#define rwTYPING 273
#define rwAND 274
#define rwOR 275
#define rwWHEN 276
#define rwNOT 277
#define rwIMPLY 278
#define rwFORALL 279
#define rwPROBLEM 280
#define EQUA 281
#define rwEXISTS 282
#define rwLENGTH 283
#define rwCONSTANTS 284
#define rwEITHER 285
#define rwINCREASE 286
#define rwMETRIC 287
#define rwMINIMIZE 288
#define ID 289
#define VAR 290
#define INT 291
#define rwFUNCTIONS 292

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 14 "parser.y" /* yacc.c:1909  */

  int i;
  intlist *intlistp;
  atomlist *atomlistp;
  atom *atomp;
  Sfma *Sfmap;
  Sfmalist *Sfmalistp;
  Seff *Seffp;
  Sefflist *Sefflistp;
  typedvarlist *typedvarlistp;

#line 140 "y.tab.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
