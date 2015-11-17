type identifier = string
type int_const = int
type char_const = char
type string_const = string

type typename = IntType | CharType | VoidType | Array of typename

type parm= Parameter of typename * identifier
type vardecl_child = VarDeclChild of identifier * int_const option
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
|	VarRef of identifier
|	Call of identifier * exp list
|	ArrayRef of identifier * exp
|	IntConst of int_const
|	CharConst of char_const
|	StringConst of string_const

and stat=
	IfStat of exp * stat * stat
|	WhileStat of exp * stat
|	ForStat of assg option * exp option * assg option * stat
|	ReturnStat of exp option
|	AssignStat of assg
|	CallStat of identifier * exp list
|	Block of stat list
|	PassStat

and assg=
	VarAssign of identifier * exp
|	ArrayAssign of identifier * exp * exp







