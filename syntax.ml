type identifier = string
type int_const = int
type string_const = string

type modifier = StaticMod

type typename = IntType | VoidType | Array of typename | Pointer of typename
				| Func of typename * typename list
				| StructType of identifier
				| UnionType of identifier
				| EnumType of identifier

and parm= Parameter of typename * identifier
and init_value = InitExp of exp | InitArray of init_value list
and vardecl = VarDecl of  typename * modifier list *  identifier * int_const  option * init_value option
and fielddecl = FieldDecl of typename * identifier * int_const option
and enumdecl= EnumDecl of identifier * int_const


and dcl= GlobalVarDecl of vardecl list | PrototypeDecl of typename * identifier * parm list
			| FuncDef of typename * identifier * parm list * vardecl list * stat list 
			| StructDef of identifier * fielddecl list
			| UnionDef of identifier * fielddecl list
			| EnumDef of identifier * enumdecl list

and exp=
	Minus of exp
|	Plus of exp
|	Not of exp
|	Add of exp * exp
|	Sub of exp * exp
|	Mul of exp * exp
|	Div of exp * exp
|	Mod of exp * exp
|	Eq	of exp * exp
|	NotEq of exp * exp
|	Lesser of exp * exp
|	Greater of exp * exp
|	LesserEq of exp * exp
|	GreaterEq of exp * exp
|	LogicalAnd of exp * exp
|	LogicalOr of exp * exp
|	Assign of exp * exp
|	PostIncrement of exp
|	PostDecrement of exp
|	Indirection of exp
|	Address of exp
|	CastExpr of typename * exp
|	ConditionalExpr of exp * exp * exp
|	CommaExpr of exp * exp
|	VarRef of identifier
|	Call of identifier * exp list
|	ExprSizeof of exp
|	TypeSizeof of typename
|	FieldRef of exp * identifier
|	IntConst of int_const
|	StringConst of string_const

and stat=
	IfStat of exp * stat * stat
|	WhileStat of exp * stat
|	DoStat of exp * stat
|	ForStat of exp option * exp option * exp option * stat
|	ReturnStat of exp option
|	ExpStat of exp
|	Block of vardecl list * stat list
|	ContinueStat
|	BreakStat
|	PassStat
|	Label of identifier
|	CaseLabel of int_const * stat list
|	DefaultLabel of stat list
|	SwitchStat of exp * stat
|	GotoStat of identifier
