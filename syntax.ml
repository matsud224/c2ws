type identifier = string
type int_const = int
type string_const = string

type typename = IntType | VoidType | Array of typename | Pointer of typename
				| Func of typename * typename list

type parm= Parameter of typename * identifier
type vardecl_child = VarDeclChild of identifier * int_const option * int_const
type vardecl = VarDecl of typename * vardecl_child list

type dcl= GlobalVarDecl of vardecl | PrototypeDecl of typename * identifier * parm list
			| FuncDef of typename * identifier * parm list * vardecl list * stat list 

and exp=
	Minus of exp
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
|	AssignAdd of exp * exp
|	AssignSub of exp * exp
|	AssignMul of exp * exp
|	AssignDiv of exp * exp
|	AssignMod of exp * exp
|	PostIncrement of exp
|	PostDecrement of exp
|	PreIncrement of exp
|	PreDecrement of exp
|	Indirection of exp
|	Address of exp
|	CommaExpr of exp * exp
|	VarRef of identifier
|	Call of identifier * exp list
|	ArrayRef of identifier * exp
|	IntConst of int_const
|	StringConst of string_const

and stat=
	IfStat of exp * stat * stat
|	WhileStat of exp * stat
|	DoStat of exp * stat
|	ForStat of exp option * exp option * exp option * stat
|	ReturnStat of exp option
|	ExpStat of exp
|	Block of stat list
|	ContinueStat
|	BreakStat
|	PassStat

and assg=
	VarAssign of identifier * exp
|	ArrayAssign of identifier * exp * exp







