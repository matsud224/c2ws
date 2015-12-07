open Syntax
open Printf

type label=int

type asm=
|	PUSH of int
|	DUP
|	COPY of int
|	SWAP
|	DISCARD
|	SLIDE of int
|	ADD
|	SUB
|	MUL
|	DIV
|	MOD
|	STORE
|	RETRIEVE
|	LABEL of label
|	CALL of label
|	JUMP of label
|	JZ of label
|	JN of label
|	RETURN
|	END
|	OUTCHAR
|	OUTINT
|	INCHAR
|	ININT

(*整数をWhitespaceの表現に変換*)
let int_enc n =
	let rec int_enc_sub n l=
		if n<2 then ((if n = 0 then " " else "\t") :: l)
		else int_enc_sub (n/2) ((if n mod 2 = 0 then " " else "\t") :: l)
	in
		String.concat "" ((if n >= 0 then " " else "\t" (*符号ビット*)) :: (int_enc_sub (abs n) ["\n"] ))


let rec assemble oc asm_list =
	let assemble_one asm=
		match asm with
		| PUSH n -> fprintf oc "  %s" (int_enc n)
		| DUP -> fprintf oc " \n "
		| COPY n -> fprintf oc " \t %s" (int_enc n)
		| SWAP -> fprintf oc " \n\t"
		| DISCARD -> fprintf oc " \n\n"
		| SLIDE n -> fprintf oc " \t\n%s" (int_enc n)
		| ADD -> fprintf oc "\t   "
		| SUB -> fprintf oc "\t  \t"
		| MUL -> fprintf oc "\t  \n"
		| DIV -> fprintf oc "\t \t "
		| MOD -> fprintf oc "\t \t\t"
		| STORE -> fprintf oc "\t\t "
		| RETRIEVE -> fprintf oc "\t\t\t"
		| LABEL l -> fprintf oc "\n  %s" (int_enc l)
		| CALL l -> fprintf oc "\n \t%s" (int_enc l)
		| JUMP l -> fprintf oc "\n \n%s" (int_enc l)
		| JZ l -> fprintf oc "\n\t %s" (int_enc l)
		| JN l -> fprintf oc "\n\t\t%s" (int_enc l)
		| RETURN -> fprintf oc "\n\t\n"
		| END -> fprintf oc "\n\n\n"
		| OUTCHAR -> fprintf oc "\t\n  "
		| OUTINT -> fprintf oc "\t\n \t"
		| INCHAR -> fprintf oc "\t\n\t "
		| ININT -> fprintf oc "\t\n\t\t" ;
	in
		match asm_list with
		| [] -> ()
		| x :: xs -> (
			assemble_one x;			
			flush oc; 
			assemble oc xs
		)

let rec print_asm oc asm_list =
	let print_one asm=
		match asm with
		| PUSH n -> fprintf oc "\tPUSH %d\n" n
		| DUP -> fprintf oc "\tDUP\n"
		| COPY n -> fprintf oc "\tCOPY %d\n" n
		| SWAP -> fprintf oc "\tSWAP\n"
		| DISCARD -> fprintf oc "\tDISCARD\n"
		| SLIDE n -> fprintf oc "\tSLIDE %d\n" n
		| ADD -> fprintf oc "\tADD\n"
		| SUB -> fprintf oc "\tSUB\n"
		| MUL -> fprintf oc "\tMUL\n"
		| DIV -> fprintf oc "\tDIV\n"
		| MOD -> fprintf oc "\tMOD\n"
		| STORE -> fprintf oc "\tSTORE\n"
		| RETRIEVE -> fprintf oc "\tRETRIEVE\n"
		| LABEL l -> fprintf oc "<%d>:\n" l
		| CALL l -> fprintf oc "\tCALL <%d>\n" l
		| JUMP l -> fprintf oc "\tJUMP <%d>\n" l
		| JZ l -> fprintf oc "\tJZ <%d>\n" l
		| JN l -> fprintf oc "\tJN <%d>\n" l
		| RETURN -> fprintf oc "\tRETURN\n"
		| END -> fprintf oc "\tEND\n"
		| OUTCHAR -> fprintf oc "\tOUTCHAR\n"
		| OUTINT -> fprintf oc "\tOUTINT\n"
		| INCHAR -> fprintf oc "\tINCHAR\n"
		| ININT -> fprintf oc "\tININT\n" ;
	in
		match asm_list with
		| [] -> ()
		| x :: xs -> (
			print_one x;			
			flush oc; 
			print_asm oc xs
		)
	

let rec print_type t=
	match t with
	| IntType -> "int"
	| VoidType -> "void"
	| Array (st) -> (print_type st) ^ "[]"
	| Pointer (st) -> (print_type st) ^ "*"
	| Func (rt , params) -> (print_type rt) ^ "(" ^ (String.concat ", " (List.map print_type params)) ^ ")"
	| StructType(id) -> "struct " ^ id
	| UnionType(id) -> "union " ^ id
	| EnumType(id) -> "enum " ^ id

type varinfo= StaticVar of typename * int (*アドレス*) | LocalVar of typename * int (*オフセット*) * int(*次の変数の開始位置*)
			| ToplevelFunction of typename * int (*ラベル番号*) | Field of typename * int (*オフセット*) * int(*次の変数の開始位置*) | EnumerationConst of typename * int 
type taginfo= StructTag of int(*サイズ*) * ((identifier * varinfo) list) | UnionTag of int(*サイズ*) * ((identifier * varinfo) list) | EnumTag of ((identifier * varinfo) list)

exception Defined_before
exception Type_error of typename * typename
exception Undefined_variable
exception Undefined_function
exception Undefined_label
exception Undefined_tag
exception Notfound_main
exception ContinueStat_not_within_loop
exception BreakStat_not_within_loop
exception CaseLabel_not_within_switchstat
exception DefaultLabel_not_within_switchstat
exception Invalid_enumindex
exception Cast_error
exception Lvalue_required

let _label=ref 0
let get_label ()=
	_label := !_label + 1;
	!_label

let _stvar=ref 2 (*0はSPに、1はInput時の一時格納先として使われている*)
let get_staticvar size=
	let temp = !_stvar in
		_stvar := !_stvar+size; temp


type 'a symtable={
	env: (identifier * varinfo) list list;
	tags: (identifier * taginfo) list;
	labels: (identifier * int) list;
	constants: 'a; 
	switchlabels: (int option * int) list; (*Noneの場合はdefaultを表す*)
}

let rec sizeof t len symtbl=
	match t with
	| IntType -> 1
	| VoidType -> 0
	| Array(s) -> len * (sizeof s 1 symtbl)
	| Pointer(s) -> 1
	| Func(_,_) -> 0
	| StructType(id) -> (try let StructTag(s,_) = List.assoc id symtbl.tags in s with Not_found -> raise Undefined_tag)
	| UnionType(id) -> (try let UnionTag(s,_) = List.assoc id symtbl.tags in s with Not_found -> raise Undefined_tag)
	| EnumType(id) -> if List.mem_assoc id symtbl.tags then (sizeof IntType 1 symtbl) else raise Undefined_tag


	
let rec make_ptrtype t depth = if depth=0 then t else make_ptrtype (Pointer(t)) (depth-1)

let rec find_flame id fl= match fl with
						| (i,c) :: xs when i=id -> Some(c)
						| _ :: xs -> find_flame id xs
						| [] -> None

let last_localaddr symtbl=
	List.fold_left  (
			fun acc ele -> match ele with
			| (_,LocalVar(_,a,n)) -> max acc n
			| _ -> acc
	) 0 (List.concat (List.tl (List.rev symtbl.env)))

let addstaticvar id t len symtbl init before_asm=
	let x=List.hd symtbl.env in
		match (find_flame id x) with
		| None -> let addr=get_staticvar (sizeof t len symtbl) in
			( {symtbl with env=((id,StaticVar(t,addr))::x) :: (List.tl symtbl.env)} , before_asm @ [] )
		| Some(_)  -> raise Defined_before

let addlocalvar id t len symtbl init before_asm=
	let x=List.hd symtbl.env in
		match (find_flame id x) with
		| None -> let last_addr=last_localaddr symtbl in 
			( { symtbl with env=((id,LocalVar(t,last_addr,last_addr+(sizeof t len symtbl))) :: x) :: symtbl.env } , before_asm @ [] )
		| Some(_)  -> raise Defined_before

let addtoplevelfun id t symtbl=
	match (find_flame id (List.hd symtbl.env)) with
	| None -> let newlabel= get_label () and x=List.hd symtbl.env in
		{ symtbl with env = ((id,ToplevelFunction(t,newlabel))::x) :: (List.tl symtbl.env) }
	| Some(_)  -> raise Defined_before

let find_enumurator name symtbl=
	let rec find_sub tags=
		match tags with
		| [] -> None
		| (_,EnumTag(enums)) :: xs -> (try (Some(List.assoc name enums)) with Not_found -> find_sub xs)
		| _ :: xs -> find_sub xs
	in
		find_sub symtbl.tags

let find_env id symtbl=
	let rec find_env_sub id env=
		match env with
		| [] -> find_enumurator id symtbl
		| x :: xs -> match (find_flame id x) with
						| None -> find_env_sub id xs
						| Some(content) -> Some(content)
	in find_env_sub id symtbl.env


let get_sp=[PUSH(0);RETRIEVE]
let set_sp=[PUSH(0);SWAP;STORE]
let sp_add x= [PUSH(x)] @ get_sp @ [ADD] @ set_sp
let retrieve_sprel offset= get_sp @ [PUSH(offset);ADD;RETRIEVE] (*スタックトップに取ってきた値を置く*)
let store_sprel offset= get_sp @ [PUSH(offset);ADD;SWAP;STORE] (*スタックトップにストアする値があると仮定*)

let rec compile_lvalue x symtbl=
	match x with
	| VarRef(id) -> (match (find_env id symtbl) with
					| Some(StaticVar(Array(t),a)) -> ([PUSH(a)], Pointer(t))
					| Some(StaticVar(t,a)) -> ([PUSH(a)], Pointer(t))
					| Some(LocalVar(Array(t),_,a)) -> ([PUSH(0);RETRIEVE;PUSH(-a+1);ADD], Pointer(t))
					| Some(LocalVar(t,_,a)) -> ([PUSH(0);RETRIEVE;PUSH(-a+1);ADD], Pointer(t))
					| _ -> raise Undefined_variable)
	| FieldRef(exp,fieldid) -> (let (exp_a,Pointer(exp_t))=compile_lvalue exp symtbl in
								match exp_t with
								| StructType(t) -> (match List.assoc t symtbl.tags with StructTag(_,pairs) -> (match List.assoc fieldid pairs with
																											| Field(ty,b,e) -> (exp_a @ [PUSH(b);ADD], Pointer(ty)) ))
								| UnionType(t) -> (match List.assoc t symtbl.tags with UnionTag(_,pairs) -> (match List.assoc fieldid pairs with
																											| Field(ty,b,e) -> (exp_a,Pointer(ty) )))
								)
	| Indirection(exp) -> (let (exp_a,Pointer(t))=compile_exp exp symtbl in (exp_a,Pointer(t)))
	| _ -> raise Lvalue_required

and ssr_seq offset times asm=
	if times=0 then asm else ssr_seq offset (times-1) ((store_sprel (offset+times-1))@asm)
and rsr_seq offset asm=
	if offset<1 then asm else rsr_seq (offset-1) (asm@(retrieve_sprel (offset)))

and compile_exp x symtbl=
	match x with
	| Plus(exp) -> let (a1,IntType)=compile_exp exp symtbl in (a1,IntType)
	| Minus(exp) -> let (a1,IntType)=compile_exp exp symtbl in (a1 @ [PUSH(-1);MUL],IntType)
	| Not(exp) -> let (a1,IntType)=compile_exp exp symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
					(a1 @ [JZ(zlabel);PUSH(0);JUMP(margelabel);LABEL(zlabel);PUSH(1);LABEL(margelabel)], IntType)
	| Add(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl in
							match (t1,t2) with
							| (IntType,IntType) -> (a1 @ a2 @ [ADD],IntType)
							| (Pointer(t),IntType) -> (a1 @ a2 @ [PUSH(sizeof t 1 symtbl);MUL;ADD],Pointer(t))
							| (IntType,Pointer(t)) -> (a1 @ [PUSH(sizeof t 1 symtbl);MUL] @ a2 @ [ADD],Pointer(t))
							| _ -> raise (Type_error (t1,t2)))
	| Sub(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl in
							match (t1,t2) with
							| (IntType,IntType) -> (a1 @ a2 @ [SUB] ,IntType)
							| (Pointer(t),IntType) -> (a1 @ a2 @ [PUSH(sizeof t 1 symtbl);MUL;SUB] ,Pointer(t))
							| (Pointer(pt1),Pointer(pt2)) when pt1=pt2 -> (a1 @ a2 @ [SUB] ,IntType)
							| _ -> raise (Type_error (t1,t2)))
	| Mul(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl  in let asm=a1 @ a2 @ [MUL] in
							match (t1,t2) with
							| (IntType,IntType) -> (asm,IntType)
							| _ -> raise (Type_error (t1,t2)))
	| Div(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl  in let asm=a1 @ a2 @ [DIV] in
							match (t1,t2) with
							| (IntType,IntType) -> (asm,IntType)
							| _ -> raise (Type_error (t1,t2)))
	| Mod(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl  in let asm=a1 @ a2 @ [MOD] in
							match (t1,t2) with
							| (IntType,IntType) -> (asm,IntType)
							| _ -> raise (Type_error (t1,t2)))
	| Eq(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
						match (t1,t2) with
						| (a,b) when a<>b -> raise (Type_error (t1,t2))
						| (StructType(_),StructType(_)) -> raise (Type_error (t1,t2))
						| (UnionType(_),UnionType(_))  -> raise (Type_error (t1,t2))
						| _ -> (a1 @ a2 @ [SUB;JZ(zlabel);PUSH(0);JUMP(margelabel);LABEL(zlabel);PUSH(1);LABEL(margelabel)], IntType)   )
	| NotEq(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
						match (t1,t2) with
						| (a,b) when a<>b -> raise (Type_error (t1,t2))
						| (StructType(_),StructType(_))  -> raise (Type_error (t1,t2))
						| (UnionType(_),UnionType(_))  -> raise (Type_error (t1,t2))
						| _ -> (a1 @ a2 @ [SUB;JZ(zlabel);PUSH(1);JUMP(margelabel);LABEL(zlabel);PUSH(0);LABEL(margelabel)], IntType)   )
	| Lesser(exp1,exp2) -> let (a1,IntType)=compile_exp exp1 symtbl and (a2,IntType)=compile_exp exp2 symtbl and (nlabel,margelabel)=(get_label (),get_label ()) in
							(a1 @ a2 @ [SUB;JN(nlabel);PUSH(0);JUMP(margelabel);LABEL(nlabel);PUSH(1);LABEL(margelabel)], IntType)
	| Greater(exp1,exp2) -> compile_exp (Lesser (exp2,exp1)) symtbl
	| LesserEq(exp1,exp2) -> compile_exp (Not (Lesser (exp2,exp1))) symtbl
	| GreaterEq(exp1,exp2) -> compile_exp (Not (Lesser (exp1,exp2))) symtbl
	| LogicalAnd(exp1,exp2) -> let (a1,IntType)=compile_exp exp1 symtbl and (a2,IntType)=compile_exp exp2 symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
									(a1 @ [JZ(zlabel)] @ a2 @ [JZ(zlabel);PUSH(1);JUMP(margelabel);LABEL(zlabel);PUSH(0);LABEL(margelabel)], IntType)
	| LogicalOr(exp1,exp2) -> let (a1,IntType)=compile_exp exp1 symtbl and (a2,IntType)=compile_exp exp2 symtbl
								and (nextlabel,falselabel,margelabel)=(get_label (),get_label (),get_label ()) in
								(a1 @ [JZ(nextlabel)] @ [PUSH(1);JUMP(margelabel);LABEL(nextlabel)]
								@ a2 @ [JZ(falselabel);PUSH(1);JUMP(margelabel);LABEL(falselabel);PUSH(0);LABEL(margelabel)], IntType)
	| Assign(target,exp) -> (let (target_a,Pointer(target_t))=compile_lvalue target symtbl and (exp_a,exp_t)=compile_exp exp symtbl in
							let rec push_bigdata cnt size=if size>0 then [DUP]@(retrieve_sprel cnt)@[STORE;PUSH(1);ADD]@(push_bigdata (cnt+1) (size-1)) else [] in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t))
							else match target_t with
							| StructType(t) -> (match List.assoc t symtbl.tags with StructTag(ssize,_) -> (exp_a @ target_a @(push_bigdata 1 ssize)@[DISCARD],exp_t) )
							| UnionType(t) -> (match List.assoc t symtbl.tags with UnionTag(ssize,_) ->	(exp_a @ target_a @(push_bigdata 1 ssize)@[DISCARD],exp_t) )
							| _ -> (exp_a @ [DUP] @ target_a @ [SWAP;STORE],exp_t) )
	| PostIncrement(exp) -> let (texp_a,texp_t)=compile_exp exp symtbl and (assg_a,_)=compile_exp (Assign(exp,Add(exp,IntConst(1)))) symtbl in
								(texp_a @ assg_a @ [DISCARD], texp_t)
	| PostDecrement(exp) -> let (texp_a,texp_t)=compile_exp exp symtbl and (assg_a,_)=compile_exp (Assign(exp,Sub(exp,IntConst(1)))) symtbl in
								(texp_a @ assg_a @ [DISCARD], texp_t)
	| VarRef(id) -> let rec push_bigdata_static a size=if size>0 then ([PUSH(a+size-1);RETRIEVE])@(store_sprel size)@(push_bigdata_static a (size-1)) else [] in
					let rec push_bigdata_local a size=if size>0 then (retrieve_sprel (-a+1+size-1))@(store_sprel size)@(push_bigdata_local a (size-1)) else [] in
					(match (find_env id symtbl) with
					| Some(StaticVar(Array(t),a)) -> ([PUSH(a)], Pointer(t))
					| Some(StaticVar(StructType(id) as struct_t,a)) -> (match List.assoc id symtbl.tags with StructTag(ssize,_) -> (push_bigdata_static a ssize, struct_t) )
					| Some(StaticVar(UnionType(id) as union_t,a)) -> (match List.assoc id symtbl.tags with UnionTag(ssize,_) -> (push_bigdata_static a ssize, union_t) )
					| Some(StaticVar(t,a)) -> ([PUSH(a); RETRIEVE], t)
					| Some(LocalVar(Array(t),_,a)) -> ([PUSH(0);RETRIEVE;PUSH(-a+1);ADD], Pointer(t))
					| Some(LocalVar(StructType(id) as struct_t,_,a)) -> (match List.assoc id symtbl.tags with StructTag(ssize,_) -> (push_bigdata_local a ssize,struct_t) )
					| Some(LocalVar(UnionType(id) as union_t,_,a)) -> (match List.assoc id symtbl.tags with UnionTag(ssize,_) -> (push_bigdata_local a ssize,union_t) )
					| Some(LocalVar(t,_,a)) -> (retrieve_sprel (-a+1), t)
					| Some(EnumerationConst(t,c)) -> ([PUSH(c)], t)
					| None -> raise Undefined_variable)
	| Call("geti",[]) -> ([PUSH(1);ININT;PUSH(1);RETRIEVE], IntType) (*Input系命令は、スタックに格納先のヒープのアドレスをおいておかないといけない*)
	| Call("getc",[]) -> ([PUSH(1);INCHAR;PUSH(1);RETRIEVE], IntType)
	| Call("puti",[arg1]) -> let (arg_a,IntType)=compile_exp arg1 symtbl in (arg_a @ [OUTINT],VoidType)
	| Call("putp",[arg1]) -> let (arg_a,Pointer(t))=compile_exp arg1 symtbl in (arg_a @ [OUTINT],VoidType)
	| Call("putc",[arg1]) -> let (arg_a,IntType)=compile_exp arg1 symtbl in (arg_a @ [OUTCHAR],VoidType)
	| Call(id,args) ->  (match find_flame id (List.hd (List.rev symtbl.env)) with
							| Some(ToplevelFunction(Func(rett,params),label)) -> let args=List.map (fun exp -> compile_exp exp symtbl) args in
									let asts=List.map2 (
										fun arg par -> 
											match (arg,par) with
											| ((a,t),ty) -> if t<>ty then raise (Type_error (t,ty)) else (a,t,sizeof t 1 symtbl)
									) args params
									in
									(*一旦オペランドスタックに引数をプッシュして、ヒープに移す*)
									let rec argpush_asmgen args asm = match args with
																	| [] -> asm
																			(*struct/unionは評価するとSPの下に値がのる*)
																	| (a,StructType(id),s) :: rest -> argpush_asmgen rest (asm @ a @ (rsr_seq s []))
																	| (a,UnionType(_),s) :: rest -> argpush_asmgen rest (asm @ a @ (rsr_seq s []))
																	| (a,_,s) :: rest -> argpush_asmgen rest (asm @ a)
									and
										total_argsize=List.fold_left (fun acc ele -> match ele with (_,_,s) -> acc+s) 0 asts
									and
										to_opstack=match rett with
													| StructType(_) | UnionType(_) -> []
													| _ -> (rsr_seq (sizeof rett 1 symtbl) [])
									in
										((argpush_asmgen (asts) []) @ (sp_add (sizeof rett 1 symtbl)) @ (ssr_seq 1 total_argsize [])
											@ [CALL(label)] @ (sp_add (-(sizeof rett 1 symtbl))) @ to_opstack , rett)
							| _ -> raise Undefined_function)
	| Address(exp) -> compile_lvalue exp symtbl
	| Indirection(exp) -> let (exp_a,Pointer(t))=compile_exp exp symtbl in (exp_a @ [RETRIEVE],t)
	| CommaExpr(exp1,exp2) -> let (exp1_a,_)=compile_exp exp1 symtbl and (exp2_a,exp2_t)=compile_exp exp2 symtbl in (exp1_a @ exp2_a, exp2_t)
	| IntConst(const) -> ([PUSH(const)], IntType)
	| StringConst(const) -> let addr=get_staticvar ((String.length const)+1) in (Hashtbl.add symtbl.constants addr const);([PUSH(addr)], Pointer(IntType))
	| ExprSizeof(exp) -> let (exp_a,t)=compile_exp exp symtbl in compile_exp (TypeSizeof(t)) symtbl
	| TypeSizeof(t) -> ([PUSH(sizeof t 1 symtbl)],IntType)
	| FieldRef(exp,fieldid) -> let val_load ty= let rec push_bigdata size=if size>0 then [DUP;RETRIEVE]@(store_sprel size)@[ADD]@(push_bigdata (size-1)) else [] in
												(match ty with (*ロード元アドレスは、スタックトップにある*)
												| Array(t) -> ([], Pointer(t))
												| StructType(id) as struct_t -> 
													(match List.assoc id symtbl.tags with StructTag(ssize,_) -> ((push_bigdata ssize)@[DISCARD], struct_t) )
												| UnionType(id) as union_t -> 
													(match List.assoc id symtbl.tags with UnionTag(ssize,_) -> ((push_bigdata ssize)@[DISCARD], union_t) )
												| t -> ([RETRIEVE], t) ) in
								(try let (exp_a,Pointer(exp_t))=compile_lvalue exp symtbl in
								match exp_t with
								| StructType(t) -> (match List.assoc t symtbl.tags with StructTag(_,pairs) -> (match List.assoc fieldid pairs with
																											| Field(ty,b,e) -> let (loadasm,t)=val_load ty in
																												(exp_a@[PUSH(b);ADD]@loadasm, t) ) )
								| UnionType(t) -> (match List.assoc t symtbl.tags with UnionTag(_,pairs) -> (match List.assoc fieldid pairs with
																											| Field(ty,b,e) -> let (loadasm,t)=val_load ty in
																												(exp_a@loadasm, t) ) )
								with Lvalue_required -> 
									(*lvalueが得られなかったので、rvalueを深さ指定で取得*)
									let (exp_a,exp_t)=compile_exp exp symtbl in
										match exp_t with
										| StructType(t) -> (match List.assoc t symtbl.tags with StructTag(_,pairs) -> (match List.assoc fieldid pairs with
																													| Field(ty,b,e) -> let (loadasm,t)=val_load ty in
																														(exp_a@[PUSH(0);RETRIEVE;PUSH(1);ADD;PUSH(b);ADD]@loadasm, t) ) )
										| UnionType(t) -> (match List.assoc t symtbl.tags with UnionTag(_,pairs) -> (match List.assoc fieldid pairs with
																													| Field(ty,b,e) -> let (loadasm,t)=val_load ty in
																														(exp_a@[PUSH(0);RETRIEVE;PUSH(1);ADD]@loadasm, t) ) )
								)
	| CastExpr(to_type,exp) -> (let (exp_a,exp_t)=compile_exp exp symtbl in
								if exp_t=to_type then (exp_a,exp_t)
								else
									match (exp_t,to_type) with
									| (IntType,Pointer(t)) -> (exp_a,to_type)
									| (Pointer(t),IntType) -> (exp_a,to_type)
									| (Pointer(s),Pointer(t)) -> (exp_a,to_type)
									| (EnumType(t),IntType) -> (exp_a,to_type)
									| (IntType,EnumType(t)) -> (exp_a,to_type)
									| _ -> raise Cast_error )
	| ConditionalExpr(cond,truepart,falsepart) -> let (cond_a,IntType)=compile_exp cond symtbl
								and (tp_a,tp_t)=compile_exp truepart symtbl and (fp_a,fp_t)=compile_exp falsepart symtbl in
								let elselabel=get_label () and endiflabel=get_label () in 
								if tp_t <> fp_t then raise (Type_error (tp_t,fp_t))
								else (cond_a @ [JZ(elselabel)] @ tp_a @ [JUMP(endiflabel)] @ [LABEL(elselabel)] @ fp_a @ [LABEL(endiflabel)], tp_t)


(*スタックトップに条件式を評価したものがおいてあると仮定*)
let make_switchasm cases symtbl = 
	let h=Hashtbl.create 5 in
		(List.iter (function (value,lb) -> if Hashtbl.mem h value then raise Defined_before else Hashtbl.add h value lb) cases);
	let keys=Hashtbl.fold (fun key value acc -> match key with
											| None -> acc
											| Some(k) -> k :: acc
								) h [] in
	let sortedkeys=List.sort compare  keys (*昇順*) in
	let rec binarysearch_if imin imax skeys=
		if imin>imax then [LABEL(Hashtbl.find h None)]
		else
			let imid=imin+(imax-imin)/2 and (glabel,eqlabel,endlabel)=(get_label (),get_label (),get_label ()) in
			[DUP;PUSH(List.nth skeys imid);SUB;JN(glabel);DUP;PUSH(List.nth skeys imid);SUB;JZ(eqlabel)] @ (binarysearch_if imin (imid-1) skeys) @ [JUMP(endlabel)]
			@ [LABEL(glabel)] @ (binarysearch_if (imid+1) imax skeys) @ [JUMP(endlabel)]
			@ [LABEL(eqlabel)] @ [DISCARD;LABEL(Hashtbl.find h (Some(imid)))] @ [LABEL(endlabel)]
	in
		binarysearch_if 0 (List.length keys) sortedkeys


let rec compile_stat x symtbl returntype returnlabel retvaladdr(*SPからの相対位置*) breaklabel continuelabel (*共にoption*) in_switch=
	match x with
	| IfStat(cond,cons,alt) -> let (cond_a,IntType)=compile_exp cond symtbl
								and (cons_a,st1)=(compile_stat cons symtbl returntype returnlabel retvaladdr breaklabel continuelabel in_switch) in
								let (alt_a,st2)=(compile_stat alt st1 returntype returnlabel retvaladdr breaklabel continuelabel in_switch) in
								let elselabel=get_label () and endiflabel=get_label () in 
										(cond_a @ [JZ(elselabel)] @ cons_a @ [JUMP(endiflabel)] @ [LABEL(elselabel)] @ alt_a @ [LABEL(endiflabel)] ,st2)
	| WhileStat(cond, stat) -> let beginlabel=get_label () and endlabel=get_label () in
								let (cond_a,IntType)=compile_exp cond  symtbl
								and (stat_a,st1)=(compile_stat stat  symtbl returntype returnlabel retvaladdr (Some(endlabel)) continuelabel in_switch) in
										([LABEL(beginlabel)] @ cond_a @ [JZ(endlabel)] @ stat_a @ [JUMP(beginlabel); LABEL(endlabel)] ,st1)
	| DoStat(cond, stat) -> let beginlabel=get_label () and endlabel=get_label () in
								let (cond_a,IntType)=compile_exp cond  symtbl
								and (stat_a,st1)=(compile_stat stat  symtbl returntype returnlabel retvaladdr (Some(endlabel)) continuelabel in_switch) in
										([LABEL(beginlabel)] @ stat_a @ cond_a @ [JZ(endlabel); JUMP(beginlabel); LABEL(endlabel)] ,st1)
	| ForStat(init, cond, continue, stat) -> let (nextlabel,contlabel,endlabel)=(get_label (),get_label (),get_label ()) in 
											let initasm=match init with
														| None -> []
														| Some(exp) -> let (a,t)=compile_exp exp symtbl in (a @ (if t <> VoidType then [DISCARD] else []))
											and condasm=match cond with
														| None -> [PUSH(1)]
														| Some(exp) -> let (a,t)=compile_exp exp symtbl in if t <> IntType then raise (Type_error (t,IntType)) else a
											and continueasm=match continue with
															| None -> []
															| Some(exp) -> let (a,t)=compile_exp exp symtbl in (a @ (if t <> VoidType then [DISCARD] else []))
											and (statasm,st1)=(compile_stat stat symtbl returntype returnlabel retvaladdr (Some(endlabel)) (Some(contlabel)) in_switch) in
												(initasm @ [LABEL(nextlabel)] @ condasm @ [JZ(endlabel)]  @ statasm
													 @ [LABEL(contlabel)] @ continueasm @ [JUMP(nextlabel);LABEL(endlabel)] ,st1)
	| ReturnStat(Some(exp)) -> let (a1,t1)=compile_exp exp symtbl in
							if t1<>returntype then raise (Type_error (t1,returntype))
							else (match t1 with
								| StructType(id) -> (match List.assoc id symtbl.tags with StructTag(s,_) ->
														(a1 @ (rsr_seq s []) @ (ssr_seq retvaladdr s []) @ [JUMP(returnlabel)] ,symtbl) )
								| UnionType(id) -> (match List.assoc id symtbl.tags with UnionTag(s,_) ->
														(a1 @ (rsr_seq s []) @ (ssr_seq retvaladdr s []) @ [JUMP(returnlabel)] ,symtbl) )
								| _ -> (a1 @ (store_sprel retvaladdr) @ [JUMP(returnlabel)] ,symtbl)  )
	| ReturnStat(None) -> if returntype <> VoidType then raise (Type_error (returntype,VoidType)) else ([JUMP(returnlabel)],symtbl)
	| ExpStat(exp) -> let (a,t)=compile_exp exp symtbl in
						let cleaning=match t with
									| VoidType -> []
									| StructType(_) -> []
									| UnionType(_) -> []
									| _ -> [DISCARD]
						in ( a @ cleaning ,symtbl) (*Voidじゃない場合、スタックにゴミが残るからポップ*)
 	| Block(decls,stats) -> (List.fold_left (fun acc s -> match (acc,s) with ((a_a,a_s),stat) -> 
				 		match (compile_stat s symtbl returntype returnlabel retvaladdr breaklabel continuelabel in_switch) with
				 		| (asm,st) -> (a_a @ asm, st)
				 		) ([],symtbl) stats)
	| ContinueStat -> (match continuelabel with None -> raise ContinueStat_not_within_loop | Some(lb) ->  ([JUMP(lb)],symtbl))
	| BreakStat -> (match breaklabel with None -> raise BreakStat_not_within_loop | Some(lb) -> ([JUMP(lb)],symtbl) )
	| PassStat -> ([],symtbl)
	| Label(id) -> if (List.mem_assoc id symtbl.labels) then raise Defined_before else let n=get_label () in ([LABEL(n)], {symtbl with labels=(id,n) :: symtbl.labels})
	| GotoStat(id) -> (try ([JUMP(List.assoc id symtbl.labels)],symtbl) with Not_found -> raise Undefined_label)
	| CaseLabel(const,stats) -> let (asm,st0)=(List.fold_left (fun acc ele->match acc with (asm_a,st_a) -> (match compile_stat ele st_a returntype returnlabel retvaladdr breaklabel continuelabel in_switch with (s_a,s_s)->(asm_a@s_a,s_s))) ([],symtbl) stats)
								in let lb=get_label () in
									if in_switch then ([LABEL(lb)]@asm,{st0 with switchlabels=(Some(const),lb)::st0.switchlabels}) else raise CaseLabel_not_within_switchstat
	| DefaultLabel(stats) -> let (asm,st0)=(List.fold_left (fun acc ele->match acc with (asm_a,st_a) -> (match compile_stat ele st_a returntype returnlabel retvaladdr breaklabel continuelabel in_switch with (s_a,s_s)->(asm_a@s_a,s_s))) ([],symtbl) stats)
								in let lb=get_label () in
									if in_switch then ([LABEL(lb)]@asm,{st0 with switchlabels=(None,lb)::st0.switchlabels}) else raise DefaultLabel_not_within_switchstat
	| SwitchStat(exp,stat) -> let (exp_a,IntType)=compile_exp exp symtbl and endofswitchlb=get_label () in
								let (asm,st0)=compile_stat stat {symtbl with switchlabels=[]} returntype returnlabel retvaladdr (Some(endofswitchlb)) continuelabel true in
									(exp_a @ (make_switchasm st0.switchlabels st0)@asm@[LABEL(endofswitchlb)], st0)
	
	
let optlen l=match l with None->1 | Some(v)->v

let rec unique_ex f lst =
	match lst with
	| [] -> true
	| x :: xs -> if List.exists (fun e -> (f e) = (f x)) xs then false else unique_ex f xs

let make_fieldassoc c symtbl=
	let get_lastaddr lst= (if lst=[] then 0 else (match List.hd lst with (_,Field(_,_,a)) -> a)) in
	match c with
	| StructDef(id,fields) -> let idvarlist= (List.fold_left (fun acc ele -> 
								let lastaddr = get_lastaddr acc
								in  
									match ele with
									| FieldDecl(Pointer(StructType(t)) as ty,name,len) when t=id (*自身へのポインタは許可*)
										-> if not (List.mem_assoc name acc) then (name,(Field( ty ,lastaddr,lastaddr+(sizeof ty (optlen len) symtbl)))) :: acc else raise Defined_before
									| FieldDecl(t,name,len)
										-> if not (List.mem_assoc name acc) then (name,(Field(t,lastaddr,lastaddr+(sizeof t (optlen len) symtbl)))) :: acc else raise Defined_before
								) [] fields)
							in 
							let size=get_lastaddr idvarlist in
								(size,idvarlist)
	| UnionDef(id,fields) -> let idvarlist= (List.fold_left (fun acc ele -> 
								let lastaddr = 0
								in 
									match ele with
									| FieldDecl(Pointer(UnionType(t)) as ty,name,len) when t=id (*自身へのポインタは許可*)
										-> if not (List.mem_assoc name acc) then (name,(Field( ty ,lastaddr,lastaddr+(sizeof ty (optlen len)  symtbl)))) :: acc else raise Defined_before
									| FieldDecl(t,name,len)
										-> if not (List.mem_assoc name acc) then  (name,(Field(t,lastaddr,lastaddr+(sizeof t (optlen len)  symtbl)))) :: acc else raise Defined_before
								) [] fields)
							in 
								let size=List.fold_left (fun acc ele -> match ele with (_,Field(_,s,e)) -> max acc (e-s)) 0 idvarlist
							in
								(size,idvarlist)

let enum_is_defined name symtbl=
	List.fold_left (fun acc ele-> if acc then true else match ele with (_,EnumTag(assoc))->List.mem_assoc name assoc | _ -> false) false symtbl.tags

let make_enumassoc tname c symtbl= 
	match c with 
	| EnumDef(id,decls) -> List.fold_left (fun acc ele -> match ele with EnumDecl(name,c)-> if ((List.mem_assoc name acc) || (enum_is_defined name symtbl)) then raise Defined_before else (name,EnumerationConst(EnumType(tname),c))::acc) [] decls

let rec compile_toplevel x symtbl=
	match x with
	| GlobalVarDecl(decllist) -> 
		(List.fold_left (fun acc decl -> match (acc,decl) with
										| ((acc_st,acc_asm), VarDecl(t,modifiers,id,len,init)) -> addstaticvar id t (optlen len) acc_st init acc_asm
						 ) (symtbl,[]) decllist)
	| PrototypeDecl(t,id,params) ->
		((addtoplevelfun id (Func(t, List.map (function Parameter(ty,_) -> ty) params)) symtbl), [])
	| FuncDef(t,id,params,vardecl,body) ->
		let functype=(Func(t, List.map (function Parameter(ty,_) -> ty) params)) in
		let defined_st= match (find_env id symtbl) with
						| Some (ToplevelFunction(ty,lb)) -> if ty <> functype then raise (Type_error (t,functype)) else symtbl
						| _ -> addtoplevelfun id functype symtbl
		in
			let (localdef_st,localdef_asm)=(List.fold_left (fun acc decl -> match (acc,decl) with 
															| ((acc_st,acc_asm), VarDecl(t,modifiers,id,len,init))
																-> if List.mem StaticMod modifiers then addstaticvar id t (optlen len) acc_st init acc_asm
																	else addlocalvar id t (optlen len) acc_st init acc_asm
											) (defined_st,[]) vardecl) in			
			let newfun_st=List.fold_left (fun acc param -> match param with Parameter(ty,id) -> let (e,a)=addlocalvar id ty 1 acc None [] in e) 
																						{localdef_st with env=[] :: localdef_st.env} params in
			let stext_size=(last_localaddr newfun_st)
			and ret_label=get_label () in
			let prologue=(sp_add stext_size) @ localdef_asm and epilogue= [LABEL(ret_label)] @ (sp_add (-stext_size)) @ [RETURN]
			and this_label= match (find_env id defined_st) with
							| Some (ToplevelFunction(ty,lb)) -> lb
			in
				(defined_st, [LABEL(this_label)] @ prologue
				 @ (fst (List.fold_left (fun acc s -> match (acc,s) with ((a_a,a_s),stat) -> 
				 		(match (compile_stat s  a_s t ret_label ((-stext_size)-(sizeof t 1 a_s)+1) None None false) with
				 		| (asm,st) -> (a_a @ asm, st)
				 		)) ([],{newfun_st with labels=[]}) body))
				 @ epilogue)
	| StructDef(id,fields) as definition -> if List.mem_assoc id symtbl.tags then raise Defined_before
								else (let (s,f)=(make_fieldassoc definition symtbl) in ({symtbl with tags=(id,(StructTag(s,f)))::symtbl.tags},[]))
	| UnionDef(id,fields) as definition -> if List.mem_assoc id symtbl.tags then raise Defined_before
								else (let (s,f)=(make_fieldassoc definition symtbl) in ({symtbl with tags=(id,(UnionTag(s,f)))::symtbl.tags},[]))
	| EnumDef(id,enums) as definition -> if List.mem_assoc id symtbl.tags then raise Defined_before
								else ({symtbl with tags=(id,EnumTag(make_enumassoc id definition symtbl))::symtbl.tags},[])

let rec compile ast symtbl asm=
	match ast with
	| [] -> (match (find_flame "main" (List.hd (List.rev symtbl.env))) with
						| None ->  raise Notfound_main
						| Some(ToplevelFunction(t,lb)) -> [PUSH(0);PUSH(get_staticvar 1(*静的変数領域の次からスタック領域*));STORE;CALL(lb);END] @ asm )
	| x :: xs -> match (compile_toplevel x symtbl) with
				| (newenv,newasm) -> compile xs newenv (asm @ newasm)


let ()=
	let ast=Myparser.prog Mylexer.token (Lexing.from_channel stdin) in
		try assemble stdout (compile ast {env=[[]]; tags=[]; labels=[]; constants=(Hashtbl.create 10); switchlabels=[]} [])
		with Type_error(t1,t2) as e -> fprintf stderr "Type_error: expected %s but %s\n" (print_type t2) (print_type t1)

