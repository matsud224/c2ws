%{
(*header:補助関数の定義*)
open Syntax


let rec ptr_wrap t depth = if depth=0 then t else ptr_wrap (Pointer(t)) (depth-1)

let rec make_vardecllist basetype modifiers children lst = 
	match children with
	| [] -> lst
	| (id,ptrdep,None,init) :: rest -> (make_vardecllist basetype modifiers rest (lst @ [(VarDecl(ptr_wrap basetype ptrdep,modifiers,id,None,init))]))
	| (id,ptrdep,Some(len),init) :: rest -> (make_vardecllist basetype modifiers rest (lst @ [(VarDecl(ptr_wrap (Array(basetype)) ptrdep,modifiers,id,Some(len),init))]))
											
let rec make_fielddecllist basetype children lst = 
	match children with
	| [] -> lst
	| (id,ptrdep,None) :: rest -> make_fielddecllist basetype rest (lst @ [(FieldDecl(ptr_wrap basetype ptrdep,id,None))])
	| (id,ptrdep,Some(len)) :: rest -> make_fielddecllist basetype rest (lst @ [(FieldDecl(ptr_wrap (Array(basetype)) ptrdep,id,Some(len)))])

let rec make_enumdecllist children index lst = 
	match children with
	| [] -> lst
	| (id,None) :: rest -> make_enumdecllist rest (index+1) (lst @ [(EnumDecl(id,index))])
	| (id,Some(start)) :: rest -> (make_enumdecllist rest (start+1) (lst @ [(EnumDecl(id,start))]))
	
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
|	structure prog
	{ $1 :: $2 }
|	union prog
	{ $1 :: $2 }
|	enum prog
	{ $1 :: $2 }
	
dcl:
	var_decl_list
	{ GlobalVarDecl($1) }
|	simple_typename some_asterisk Id LPAREN parm_list RPAREN
	{ PrototypeDecl(ptr_wrap $1 $2,$3,$5) }

some_asterisk:
	
	{ 0 }
|	ASTERISK some_asterisk
	{ $2+1 }

var_decl:
	some_asterisk Id
	{ ($2,$1,None,None) }
|	some_asterisk Id  LBRACKET IntConst RBRACKET
	{ ($2,$1,Some($4),None) }
|	some_asterisk Id ASSIGNEQ initialvalue
	{ ($2,$1,None,Some($4)) }
|	some_asterisk Id  LBRACKET RBRACKET ASSIGNEQ initialvalue
	{ match $6 with InitArray(lst) -> ($2,$1,Some(List.length lst),Some($6)) }
|	some_asterisk Id  LBRACKET IntConst RBRACKET ASSIGNEQ initialvalue
	{ ($2,$1,Some($4),Some($7)) }
	

initialvalue_list:
	initialvalue
	{ [$1] }
|	initialvalue_list COMMA initialvalue
	{ $1 @ [$3] }

initialvalue:
	expr
	{ InitExp($1) }
|	LBRACE initialvalue_list RBRACE
	{ InitArray($2) }
	
var_decl_additional:

	{ [] }
|	COMMA var_decl var_decl_additional
	{ $2 :: $3 }
	
field_decl:
	some_asterisk Id
	{ ($2,$1,None) }
|	some_asterisk Id  LBRACKET IntConst RBRACKET
	{ ($2,$1,Some($4)) }

field_decl_additional:

	{ [] }
|	COMMA field_decl field_decl_additional
	{ $2 :: $3 }
	
enum_decl:
	Id
	{ ($1,None) }
|	Id ASSIGNEQ IntConst
	{ ($1,Some($3)) }

enum_decl_additional:

	{ [] }
|	COMMA enum_decl enum_decl_additional
	{ $2 :: $3 }
	
enum_decl_list:

	{ [] }
|	enum_decl enum_decl_additional
	{ make_enumdecllist ($1 :: $2) 0 [] }
	
structure:
	STRUCT Id LBRACE field_decl_list RBRACE
	{ StructDef($2,$4) }
	
union:
	UNION Id LBRACE field_decl_list RBRACE
	{ UnionDef($2,$4) }

enum:
	ENUM Id LBRACE enum_decl_list RBRACE
	{ EnumDef($2,$4) }

simple_typename:
	INT
	{ IntType }
|	VOID
	{ VoidType }
|	STRUCT Id
	{ StructType($2) }
|	UNION Id
	{ UnionType($2) }
|	ENUM Id
	{ EnumType($2) }

parm_list:

	{ [] }
|	simple_typename some_asterisk Id          parm_list_additional
	{ Parameter(ptr_wrap $1 $2,$3) :: $4 }
|	simple_typename some_asterisk Id   LBRACKET RBRACKET   parm_list_additional
	{ Parameter(Pointer(ptr_wrap $1 $2),$3) :: $6 }

parm_list_additional:

	{ [] }
|	COMMA simple_typename some_asterisk Id parm_list_additional
	{ Parameter(ptr_wrap $2 $3,$4) :: $5 }
|	COMMA simple_typename some_asterisk Id  LBRACKET RBRACKET parm_list_additional
	{ Parameter(Pointer(ptr_wrap $2 $3),$4) :: $7 }

func:
	simple_typename some_asterisk Id LPAREN parm_list RPAREN LBRACE var_decl_list stmt_list RBRACE
	{ FuncDef(ptr_wrap $1 $2,$3,$5,$8,$9) }

var_decl_list:

	{ [] }
|	simple_typename var_decl var_decl_additional SEMICOLON var_decl_list
	{ (make_vardecllist $1 [] ($2 :: $3) []) @ $5 }
|	STATIC simple_typename var_decl var_decl_additional SEMICOLON var_decl_list
	{ (make_vardecllist $2 [StaticMod] ($3 :: $4) []) @ $6 }
	
field_decl_list:

	{ [] }
|	simple_typename field_decl field_decl_additional SEMICOLON field_decl_list
	{ (make_fielddecllist $1 ($2 :: $3) []) @ $5 }

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
	
block:
|	LBRACE var_decl_list stmt_list RBRACE
	{ Block($2,$3) } 

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
|	block
	{ $1 }
|	CONTINUE
	{ ContinueStat }
|	BREAK
	{ BreakStat }
|	SEMICOLON
	{ PassStat }
|	SWITCH LPAREN comma_expr RPAREN stmt
	{ SwitchStat($3,$5) }
|	GOTO Id
	{ GotoStat($2) }
|	DEFAULT COLON stmt_list
	{ DefaultLabel($3) }
|	CASE IntConst COLON stmt_list
	{ CaseLabel($2,$4) }
|	Id COLON
	{ Label($1) }

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
	conditional_expr
	{ $1 }
|	unary_expr ASSIGNEQ conditional_expr
	{ Assign($1,$3) }
|	unary_expr ASSIGNPLUS conditional_expr
	{ Assign($1,Add($1,$3)) }
|	unary_expr ASSIGNMINUS conditional_expr
	{ Assign($1,Sub($1,$3)) }
|	unary_expr ASSIGNASTERISK conditional_expr
	{ Assign($1,Mul($1,$3)) }
|	unary_expr ASSIGNSLASH conditional_expr
	{ Assign($1,Div($1,$3)) }
|	unary_expr ASSIGNPERCENT conditional_expr
	{ Assign($1,Mod($1,$3)) }

conditional_expr:
	logical_or_expr
	{ $1 }
|	logical_or_expr QUESTION comma_expr COLON conditional_expr
	{ ConditionalExpr($1,$3,$5) }

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
	cast_expr
	{ $1 }
|	multiplicative_expr ASTERISK cast_expr
	{ Mul($1,$3) }
|	multiplicative_expr SLASH cast_expr
	{ Div($1,$3) }
|	multiplicative_expr PERCENT cast_expr
	{ Mod($1,$3) }

cast_expr:
	unary_expr
	{ $1 }
|	LPAREN simple_typename some_asterisk RPAREN cast_expr
	{ CastExpr(ptr_wrap $2 $3,$5) }

unary_expr:
	postfix_expr
	{ $1 }
|	MINUS unary_expr
	{ Minus($2) }
|	PLUS unary_expr
	{ Plus($2) }
|	EXCLAMATION unary_expr
	{ Not($2) }
|	SIZEOF unary_expr
	{ ExprSizeof($2) }
|	SIZEOF simple_typename some_asterisk
	{ TypeSizeof(ptr_wrap $2 $3) }
|	INCREMENT unary_expr
	{ Assign($2,Add($2,IntConst(1))) }
|	DECREMENT unary_expr
	{ Assign($2,Sub($2,IntConst(1))) }
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
|	postfix_expr DOT Id
	{ FieldRef($1,$3) }
|	postfix_expr ARROW Id
	{ FieldRef(Indirection($1),$3) }
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

