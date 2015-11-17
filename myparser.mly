%{
open Syntax
%}

%token <string> Id
%token <int> IntConst
%token <char> CharConst
%token <string> StringConst
%token ASSIGNEQ    		/* = */
%token EQUAL	  		/* == */
%token NOTEQUAL  		/* != */
%token PLUS        		/* + */
%token MINUS       		/* - */
%token ASTERISK			/* * */
%token SLASH			/* / */
%token PERCENT			/* % */
%token ASSIGNPLUS       /* += */
%token ASSIGNMINUS      /* -= */
%token ASSIGNASTERISK	/* *= */
%token ASSIGNSLASH		/* /= */
%token ASSIGNPERCENT	/* %= */
%token LSHIFT			/* << */
%token RSHIFT			/* >> */
%token LBRACKET			/* [ */
%token RBRACKET			/* ] */
%token LPAREN   	    /* ( */
%token RPAREN    	  	/* ) */
%token LBRACE    		/* { */
%token RBRACE     		/* } */
%token GREATER			/* > */
%token LESSER			/* < */
%token GREATEREQ		/* >= */
%token LESSEREQ			/* <= */
%token SEMICOLON  		/* ; */
%token COMMA	  		/* , */
%token DOT		  		/* . */
%token THREEDOT	  		/* ... */
%token COLON	   		/* : */
%token QUESTION	   		/* ? */
%token EXCLAMATION  	/* ! */
%token AND				/* & */
%token LOGICALAND		/* && */
%token LOGICALOR		/* || */
%token ARROW			/* -> */
%token INCREMENT		/* ++ */
%token DECREMENT		/* -- */
%token SIZEOF			/* sizeof */
%token STATIC			/* static */
%token TYPEDEF			/* typedef */
%token CONST			/* const */
%token VOID				/* void */
%token CHAR				/* char */
%token SHORT			/* short */
%token INT				/* int */
%token STRUCT			/* struct */
%token UNION			/* union */
%token ENUM				/* enum */
%token CASE				/* case */
%token DEFAULT			/* default */
%token IF				/* if */
%token ELSE				/* else */
%token SWITCH			/* switch */
%token WHILE			/* while */
%token DO				/* do */
%token FOR				/* for */
%token GOTO				/* goto */
%token CONTINUE			/* continue */
%token BREAK			/* break */
%token RETURN			/* return */
%token EOF

/*優先順位*/
%left LOGICALOR
%left LOGICALAND
%left EQUAL NOTEQUAL
%nonassoc LESSER LESSEREQ GREATER GREATEREQ
%left PLUS MINUS
%left ASTERISK SLASH
%right EXCLAMATION UMINUS

%type <Syntax.dcl list> prog
%start prog

%%


prog:
	EOF
	{ [] }
|	dcl SEMICOLON prog
	{ $1 :: $3 }
|	func prog
	{ $1 :: $2 }

dcl:
	typename var_decl var_decl_additional
	{ GlobalVarDecl(VarDecl($1,$2 :: $3)) }
|	typename Id LPAREN parm_list RPAREN
	{ PrototypeDecl($1,$2,$4) }
|	VOID Id LPAREN parm_list RPAREN
	{ PrototypeDecl(VoidType,$2,$4) }

var_decl:
	Id
	{ VarDeclChild($1,None) }
|	Id  LBRACKET IntConst RBRACKET
	{ VarDeclChild($1,Some($3)) }

var_decl_additional:

	{ [] }
|	COMMA var_decl var_decl_additional
	{ $2 :: $3 }

typename:
	CHAR
	{ CharType }
|	INT
	{ IntType }

parm_list:

	{ [] }
|	typename Id                       parm_list_additional
	{ Parameter($1,$2) :: $3 }
|	typename Id   LBRACKET RBRACKET   parm_list_additional
	{ Parameter(Array($1),$2) :: $5 }

parm_list_additional:

	{ [] }
|	COMMA typename Id parm_list_additional
	{ Parameter($2,$3) :: $4 }
|	COMMA typename Id  LBRACKET RBRACKET parm_list_additional
	{ Parameter(Array($2),$3) :: $6 }

func:
	typename Id LPAREN parm_list RPAREN LBRACE localvar_decl_list stmt_list RBRACE
	{ FuncDef($1,$2,$4,$7,$8) }
|	VOID Id LPAREN parm_list RPAREN LBRACE localvar_decl_list stmt_list RBRACE
	{ FuncDef(VoidType,$2,$4,$7,$8) }

localvar_decl_list:

	{ [] }
|	typename var_decl var_decl_additional SEMICOLON localvar_decl_list
	{ VarDecl($1,$2 :: $3) :: $5 }

expr_commasep_list:

	{ [] }
|	expr expr_additional
	{ $1 :: $2 }

expr_additional:

	{ [] }
|	COMMA expr expr_additional
	{ $2 :: $3 }

stmt_list:

	{ [] }
|	stmt stmt_list
	{ $1 :: $2 }

stmt:
	IF LPAREN expr RPAREN stmt
	{ IfStat($3,$5,PassStat) }
|	IF LPAREN expr RPAREN stmt ELSE stmt
	{ IfStat($3,$5,$7) }
|	WHILE LPAREN expr RPAREN stmt
	{ WhileStat($3,$5) }
|	FOR LPAREN   assg_option   SEMICOLON   expr_option   SEMICOLON   assg_option   RPAREN stmt
	{ ForStat($3,$5,$7,$9) }
|	RETURN   expr_option   SEMICOLON
	{ ReturnStat($2) }
|	assg SEMICOLON
	{ AssignStat($1) }
|	Id LPAREN expr_commasep_list RPAREN SEMICOLON
	{ CallStat($1,$3) }
|	LBRACE stmt_list RBRACE
	{ Block($2) }
|	SEMICOLON
	{ PassStat }

expr_option:

	{ None }
|	expr
	{ Some($1) }

assg_option:

	{ None }
|	assg
	{ Some($1) }

assg:
	Id 							ASSIGNEQ expr
	{ VarAssign($1,$3) }
|	Id  LBRACKET expr RBRACKET  ASSIGNEQ expr
	{ ArrayAssign($1,$3,$6) }

expr:
	MINUS expr %prec UMINUS
	{ Minus($2) }
|	EXCLAMATION expr
	{ Not($2) }
|	expr PLUS expr
	{ Add($1,$3) }
|	expr MINUS expr
	{ Sub($1,$3) }
|	expr ASTERISK expr
	{ Mul($1,$3) }
|	expr SLASH expr
	{ Div($1,$3) }
|	expr PERCENT expr
	{ Mod($1,$3) }
|	expr EQUAL expr
	{ Eq($1,$3) }
|	expr NOTEQUAL expr
	{ NotEq($1,$3) }
|	expr LESSER expr
	{ Lesser($1,$3) }
|	expr GREATER expr
	{ Greater($1,$3) }
|	expr LESSEREQ expr
	{ LesserEq($1,$3) }
|	expr GREATEREQ expr
	{ GreaterEq($1,$3) }
|	expr LOGICALAND expr
	{ LogicalAnd($1,$3) }
|	expr LOGICALOR expr
	{ LogicalOr($1,$3) }
|	Id
	{ VarRef($1) }
|	Id LPAREN  expr_commasep_list   RPAREN
	{ Call($1,$3) }
|	Id LBRACKET expr RBRACKET
	{ ArrayRef($1,$3) }
|	LPAREN expr RPAREN
	{ $2 }
|	IntConst
	{ IntConst($1) }
|	CharConst
	{ CharConst($1) }
|	StringConst
	{ StringConst($1) }

