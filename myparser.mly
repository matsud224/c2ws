%{
open Syntax
%}

%token <string> Id
%token <int> IntConst
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
%right ASSIGN_OP
%left LOGICALOR
%left LOGICALAND
%left EQUAL NOTEQUAL
%left LESSER LESSEREQ GREATER GREATEREQ
%left PLUS MINUS
%left ASTERISK SLASH
%right EXCLAMATION UMINUS PREINC PREDEC ADDRESS INDIRECTION
%left POSTINC POSTDEC


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

var_decl_ptr:
	
	{ 0 }
|	ASTERISK var_decl_ptr
	{ $2+1 }

var_decl:
	var_decl_ptr Id
	{ VarDeclChild($2,None,$1) }
|	var_decl_ptr Id  LBRACKET IntConst RBRACKET
	{ VarDeclChild($2,Some($4),$1) }

var_decl_additional:

	{ [] }
|	COMMA var_decl var_decl_additional
	{ $2 :: $3 }

typename:
	INT
	{ IntType }

parm_list:

	{ [] }
|	typename Id                       parm_list_additional
	{ Parameter($1,$2) :: $3 }
|	typename Id   LBRACKET RBRACKET   parm_list_additional
	{ Parameter(Pointer($1),$2) :: $5 }

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
	IF LPAREN commaexpr RPAREN stmt
	{ IfStat($3,$5,PassStat) }
|	IF LPAREN commaexpr RPAREN stmt ELSE stmt
	{ IfStat($3,$5,$7) }
|	WHILE LPAREN commaexpr RPAREN stmt
	{ WhileStat($3,$5) }
|	DO stmt WHILE LPAREN commaexpr RPAREN SEMICOLON
	{ DoStat($5,$2) }
|	FOR LPAREN   commaexpr_option   SEMICOLON   commaexpr_option   SEMICOLON   commaexpr_option   RPAREN stmt
	{ ForStat($3,$5,$7,$9) }
|	RETURN   commaexpr_option   SEMICOLON
	{ ReturnStat($2) }
|	commaexpr SEMICOLON
	{ ExpStat($1) }
|	LBRACE stmt_list RBRACE
	{ Block($2) }
|	CONTINUE
	{ ContinueStat }
|	BREAK
	{ BreakStat }
|	SEMICOLON
	{ PassStat }

expr_option:

	{ None }
|	expr
	{ Some($1) }

commaexpr_option:

	{ None }
|	commaexpr
	{ Some($1) }

commaexpr:
	expr
	{ $1 }
|	commaexpr COMMA expr
	{ CommaExpr($1,$3) }

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
|	expr ASSIGNEQ expr %prec ASSIGN_OP
	{ Assign($1,$3) }
|	expr ASSIGNPLUS expr %prec ASSIGN_OP
	{ AssignAdd($1,$3) }
|	expr ASSIGNMINUS expr %prec ASSIGN_OP
	{ AssignSub($1,$3) }
|	expr ASSIGNASTERISK expr %prec ASSIGN_OP
	{ AssignMul($1,$3) }
|	expr ASSIGNSLASH expr %prec ASSIGN_OP
	{ AssignDiv($1,$3) }
|	expr ASSIGNPERCENT expr %prec ASSIGN_OP
	{ AssignMod($1,$3) }
|	expr INCREMENT %prec POSTINC
	{ PostIncrement($1) }
|	expr DECREMENT %prec POSTDEC
	{ PostDecrement($1) }
|	INCREMENT expr %prec PREINC
	{ PreIncrement($2) }
|	DECREMENT expr %prec PREDEC
	{ PreDecrement($2) }
|	ASTERISK expr %prec INDIRECTION
	{ Indirection($2) }
|	AND expr %prec ADDRESS
	{ Address($2) }
|	Id
	{ VarRef($1) }
|	Id LPAREN  expr_commasep_list   RPAREN
	{ Call($1,$3) }
|	Id LBRACKET expr RBRACKET
	{ ArrayRef($1,$3) }
|	LPAREN commaexpr RPAREN
	{ $2 }
|	IntConst
	{ IntConst($1) }
|	StringConst
	{ StringConst($1) }

