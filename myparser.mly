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
|	typename var_decl_ptr Id                       parm_list_additional
	{ Parameter($1,$3,$2) :: $4 }
|	typename var_decl_ptr Id   LBRACKET RBRACKET   parm_list_additional
	{ Parameter(Pointer($1),$3,$2) :: $6 }

parm_list_additional:

	{ [] }
|	COMMA typename var_decl_ptr Id parm_list_additional
	{ Parameter($2,$4,$3) :: $5 }
|	COMMA typename var_decl_ptr Id  LBRACKET RBRACKET parm_list_additional
	{ Parameter(Pointer($2),$4,$3) :: $7 }

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
	IF LPAREN comma_expr RPAREN stmt
	{ IfStat($3,$5,PassStat) }
|	IF LPAREN comma_expr RPAREN stmt ELSE stmt
	{ IfStat($3,$5,$7) }
|	WHILE LPAREN comma_expr RPAREN stmt
	{ WhileStat($3,$5) }
|	DO stmt WHILE LPAREN comma_expr RPAREN SEMICOLON
	{ DoStat($5,$2) }
|	FOR LPAREN   comma_expr_option   SEMICOLON   comma_expr_option   SEMICOLON   comma_expr_option   RPAREN stmt
	{ ForStat($3,$5,$7,$9) }
|	RETURN   comma_expr_option   SEMICOLON
	{ ReturnStat($2) }
|	comma_expr SEMICOLON
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

comma_expr_option:

	{ None }
|	comma_expr
	{ Some($1) }

comma_expr:
	expr
	{ $1 }
|	comma_expr COMMA expr
	{ CommaExpr($1,$3) }

expr:
	logical_or_expr
	{ $1 }
|	expr ASSIGNEQ logical_or_expr
	{ Assign($1,$3) }
|	expr ASSIGNPLUS logical_or_expr
	{ AssignAdd($1,$3) }
|	expr ASSIGNMINUS logical_or_expr
	{ AssignSub($1,$3) }
|	expr ASSIGNASTERISK logical_or_expr
	{ AssignMul($1,$3) }
|	expr ASSIGNSLASH logical_or_expr
	{ AssignDiv($1,$3) }
|	expr ASSIGNPERCENT logical_or_expr
	{ AssignMod($1,$3) }

logical_or_expr:
	logical_and_expr
	{ $1 }
|	logical_or_expr LOGICALOR logical_and_expr
	{ LogicalOr($1,$3) }

logical_and_expr:
	equality_expr
	{ $1 }
|	logical_and_expr LOGICALAND equality_expr
	{ LogicalAnd($1,$3) }

equality_expr:
	relational_expr
	{ $1 }
|	equality_expr EQUAL relational_expr
	{ Eq($1,$3) }
|	equality_expr NOTEQUAL relational_expr
	{ NotEq($1,$3) }

relational_expr:
	additive_expr
	{ $1 }
|	relational_expr LESSER additive_expr
	{ Lesser($1,$3) }
|	relational_expr GREATER additive_expr
	{ Greater($1,$3) }
|	relational_expr LESSEREQ additive_expr
	{ LesserEq($1,$3) }
|	relational_expr GREATEREQ additive_expr
	{ GreaterEq($1,$3) }

additive_expr:
	multiplicative_expr
	{ $1 }
|	additive_expr PLUS multiplicative_expr
	{ Add($1,$3) }
|	additive_expr MINUS multiplicative_expr
	{ Sub($1,$3) }

multiplicative_expr:
	unary_expr
	{ $1 }
|	multiplicative_expr ASTERISK unary_expr
	{ Mul($1,$3) }
|	multiplicative_expr SLASH unary_expr
	{ Div($1,$3) }
|	multiplicative_expr PERCENT unary_expr
	{ Mod($1,$3) }

unary_expr:
	postfix_expr
	{ $1 }
|	MINUS unary_expr
	{ Minus($2) }
|	EXCLAMATION unary_expr
	{ Not($2) }
|	SIZEOF unary_expr
	{ Sizeof($2) }
|	INCREMENT unary_expr
	{ PreIncrement($2) }
|	DECREMENT unary_expr
	{ PreDecrement($2) }
|	ASTERISK unary_expr
	{ Indirection($2) }
|	AND unary_expr
	{ Address($2) }

postfix_expr:
	primary_expr
	{ $1 }
|	postfix_expr LBRACKET expr RBRACKET
	{ Indirection(Add($1,$3)) }
|	Id LPAREN  expr_commasep_list RPAREN
	{ Call($1,$3) }
|	postfix_expr INCREMENT
	{ PostIncrement($1) }
|	postfix_expr DECREMENT
	{ PostDecrement($1) }

primary_expr:
	Id
	{ VarRef($1) }
|	IntConst
	{ IntConst($1) }
|	StringConst
	{ StringConst($1) }
|	LPAREN comma_expr RPAREN
	{ $2 }

