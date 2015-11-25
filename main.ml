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
		if n<2 then ((if n = 0 then "s" else "t") :: l)
		else int_enc_sub (n/2) ((if n mod 2 = 0 then "s" else "t") :: l)
	in
		String.concat "" ((if n >= 0 then "s" else "t" (*符号ビット*)) :: (int_enc_sub (abs n) ["l"] ))


let rec assemble oc asm_list =
	let assemble_one asm=
		match asm with
		| PUSH n -> fprintf oc "ss%s" (int_enc n)
		| DUP -> fprintf oc "sls"
		| COPY n -> fprintf oc "sts%s" (int_enc n)
		| SWAP -> fprintf oc "slt"
		| DISCARD -> fprintf oc "sll"
		| SLIDE n -> fprintf oc "stl%s" (int_enc n)
		| ADD -> fprintf oc "tsss"
		| SUB -> fprintf oc "tsst"
		| MUL -> fprintf oc "tssl"
		| DIV -> fprintf oc "tsts"
		| MOD -> fprintf oc "tstt"
		| STORE -> fprintf oc "tts"
		| RETRIEVE -> fprintf oc "ttt"
		| LABEL l -> fprintf oc "lss%s" (int_enc l)
		| CALL l -> fprintf oc "lst%s" (int_enc l)
		| JUMP l -> fprintf oc "lsl%s" (int_enc l)
		| JZ l -> fprintf oc "lts%s" (int_enc l)
		| JN l -> fprintf oc "ltt%s" (int_enc l)
		| RETURN -> fprintf oc "ltl"
		| END -> fprintf oc "lll"
		| OUTCHAR -> fprintf oc "tlss"
		| OUTINT -> fprintf oc "tlst"
		| INCHAR -> fprintf oc "tlts"
		| ININT -> fprintf oc "tltt" ;
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
	

type varinfo= StaticVar of typename * int (*アドレス*) | LocalVar of typename * int (*オフセット*) | ToplevelFunction of typename * int (*ラベル番号*)

exception Defined_before
exception Type_error
exception Undefined_variable
exception Undefined_function

let _label=ref 0
let get_label ()=
	_label := !_label + 1;
	!_label

let _stvar=ref 1
let get_staticvar size=
	let temp = !_stvar in
		_stvar := !_stvar+size; temp

let rec sizeof t len=
	match t with
	| IntType -> 1
	| CharType -> 1
	| VoidType -> 0
	| Array s -> len * (sizeof s 1)
	| Func(_,_) -> 0

let rec find_flame id fl= match fl with
						| (i,c) :: xs when i=id -> Some(c)
						| _ :: xs -> find_flame id xs
						| [] -> None

let last_localaddr env=
	List.fold_left  (
			fun acc ele -> match ele with
			| (_,LocalVar(_,a)) -> max acc a
			| _ -> acc
	) 0 (List.concat (List.tl (List.rev env)))

let addstaticvar id t len env=
	let x=List.hd env in
		match (find_flame id x) with
		| None -> let addr=get_staticvar (sizeof t len) in
			((id,StaticVar(t,addr))::x) :: (List.tl env)
		| Some(_)  -> raise Defined_before

let addlocalvar id t len env=
	let x=List.hd env in
		match (find_flame id x) with
		| None -> let last_addr=last_localaddr env in
			((id,LocalVar(t,last_addr+(sizeof t len))) :: x) :: env
		| Some(_)  -> raise Defined_before

let addtoplevelfun id t env=
	match (find_flame id (List.hd env)) with
	| None -> let newlabel= get_label () and x=List.hd env in
		((id,ToplevelFunction(t,newlabel))::x) :: (List.tl env)
	| Some(_)  -> raise Defined_before

let rec find_env id env=
	match env with
	| [] -> None
	| x :: xs -> match (find_flame id x) with
					| None -> find_env id xs
					| Some(content) -> Some(content)


let get_sp=[PUSH(0);RETRIEVE]
let set_sp=[PUSH(0);SWAP;STORE]
let sp_add x= [PUSH(x)] @ get_sp @ [ADD] @ set_sp
let retrieve_sprel offset= get_sp @ [PUSH(offset);ADD;RETRIEVE] (*スタックトップに取ってきた値を置く*)
let store_sprel offset= get_sp @ [PUSH(offset);ADD;SWAP;STORE] (*スタックトップにストアする値があると仮定*)

let rec compile_exp x env=
	match x with
	| Minus(exp) -> let (a1,t1)=compile_exp exp env in
							if t1 <> IntType then raise Type_error
							else (a1 @ [PUSH(-1);MUL],IntType)
	(*| Not(exp) ->*) 
	| Add(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env  in
							if t1 <> IntType || t2 <> IntType then raise Type_error
							else (a1 @ a2 @ [ADD],IntType)
	| Sub(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env  and (a2,t2)=compile_exp exp2 env  in
							if t1 <> IntType || t2 <> IntType then raise Type_error
							else (a1 @ a2 @ [SUB],IntType)
	| Mul(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env  and (a2,t2)=compile_exp exp2 env  in
							if t1 <> IntType || t2 <> IntType then raise Type_error
							else (a1 @ a2 @ [MUL],IntType)
	| Div(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env  and (a2,t2)=compile_exp exp2 env  in
							if t1 <> IntType || t2 <> IntType then raise Type_error
							else (a1 @ a2 @ [DIV],IntType)
	| Mod(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env  and (a2,t2)=compile_exp exp2 env  in
							if t1 <> IntType || t2 <> IntType then raise Type_error
							else (a1 @ a2 @ [MOD] , IntType)
	(*| Eq(exp1,exp2) -> 
	| NotEq(exp1,exp2) -> 
	| Lesser(exp1,exp2) -> 
	| Greater(exp1,exp2) -> 
	| LesserEq(exp1,exp2) -> 
	| GreaterEq(exp1,exp2) -> 
	| LogicalAnd(exp1,exp2) -> 
	| LogicalOr(exp1,exp2) ->*) 
	| VarRef(id) -> (match (find_env id env) with
					| Some(StaticVar(t,a)) -> ([PUSH(a); RETRIEVE], t)
					| Some(LocalVar(t,a)) -> (retrieve_sprel a, t)
					| _ -> raise Undefined_variable)
	| Call(id,args) ->  (match find_flame id (List.hd (List.rev env)) with
							| Some(ToplevelFunction(Func(rett,params),label)) -> let args=List.map (fun exp -> compile_exp exp env) args in
									let asts=List.map2 (
										fun arg par -> 
											match (arg,par) with
											| ((a,t),ty) -> if t<>ty then raise Type_error else (a,sizeof t 1)
									) args params
									in
									let rec argpush_asmgen args offset asm = match args with
																	| [] -> asm
																	| (a,s) :: rest -> argpush_asmgen rest (offset+s) (asm @ a @ (store_sprel offset))
									in
										((sp_add (-(sizeof rett 1))) @ (argpush_asmgen asts 0 []) @ [CALL(label)] @ (sp_add (sizeof rett 1)) , rett)
							| _ -> raise Undefined_function)
	(*| ArrayRef(id,index) -> *)
	| IntConst(const) -> ([PUSH(const)], IntType)
	| CharConst(const) -> ([PUSH(int_of_char const)],CharType)
	(*| StringConst(const) ->*) 

let compile_assignment x env=
	match x with
	| VarAssign(id,exp) -> let (exp_a,exp_t)=compile_exp exp env in
							match (find_env id env) with
							| Some(StaticVar(t,a)) -> if t<>exp_t then raise Type_error else exp_a @ [PUSH(a); STORE]
							| Some(LocalVar(t,a)) -> if t<>exp_t then raise Type_error else exp_a @ (store_sprel a)
							| _ -> raise Undefined_variable
	(*| ArrayAssign(id,index,exp) ->*) 

let rec compile_stat x env returntype=
	match x with
	| IfStat(cond,cons,alt) -> let (cond_a,cond_t)=compile_exp cond env and cons_a=compile_stat cons env returntype and alt_a=compile_stat alt env returntype in
								if cond_t<>IntType then raise Type_error
								else let elselabel=get_label () and endiflabel=get_label () in 
										cond_a @ [JZ(elselabel)] @ cons_a @ [JUMP(endiflabel)] @ [LABEL(elselabel)] @ alt_a @ [LABEL(endiflabel)]
	| WhileStat(cond, stat) -> let (cond_a,cond_t)=compile_exp cond  env and stat_a=compile_stat stat  env returntype in
								if cond_t<>IntType then raise Type_error
								else let beginlabel=get_label () and endlabel=get_label () in 
										[LABEL(beginlabel)] @ cond_a @ [JZ(endlabel)] @ stat_a @ [JUMP(beginlabel)] @ [LABEL(endlabel)]
	(*| ForStat(init, cond, continue, stat) ->*) 
	| ReturnStat(Some(exp)) -> let (a1,t1)=compile_exp exp env in
							if t1!=returntype then raise Type_error
							else a1 @ [RETURN]
	| ReturnStat(None) -> if returntype <> VoidType then raise Type_error else [RETURN]
	| AssignStat(assg) -> compile_assignment assg env
	| CallStat(id, exps) -> let (a,t)=compile_exp (Call(id,exps)) env in a
 	| Block(stats) -> List.concat (List.map (fun s -> compile_stat s env returntype) stats)
	| PassStat -> []


let rec compile_toplevel x env=
	match x with
	| GlobalVarDecl(VarDecl(t,children)) -> 
		(List.fold_left (fun acc child -> match child with
										| VarDeclChild(id,Some(size)) -> addstaticvar id t size acc
										| VarDeclChild(id,None) -> addstaticvar id t 1 acc
						 ) env children , [] )
	| PrototypeDecl(t,id,params) ->
		((addtoplevelfun id (Func(t, List.map (function Parameter(ty,_) -> ty) params)) env), [])
	| FuncDef(t,id,params,vardecl,body) ->
		let defined_env= match (find_env id env) with
						| Some (ToplevelFunction(ty,lb)) -> if ty != t then raise Type_error else env
						| _ -> addtoplevelfun id (Func(t, List.map (function Parameter(ty,_) -> ty) params)) env
		in
			let paramdef_env=List.fold_left (fun acc param -> match param with Parameter(ty,id) -> addlocalvar id ty 1 acc) ([] :: defined_env) params in
			let newfun_env=List.fold_left (fun acc decl -> match decl with VarDecl(t,children) -> 
												(List.fold_left (fun acc child -> match child with
																				| VarDeclChild(id,Some(size)) -> addlocalvar id t size acc
																				| VarDeclChild(id,None) -> addlocalvar id t 1 acc
												) acc children)
											) paramdef_env vardecl in			
			let stext_size=last_localaddr newfun_env in
			let prologue=(sp_add stext_size) and epilogue=(sp_add (-stext_size)) @ [RETURN]
			and this_label= match (find_env id defined_env) with
							| Some (ToplevelFunction(ty,lb)) -> lb
			in
				(defined_env, [LABEL(this_label)] @ prologue @ (List.concat (List.map (fun s -> compile_stat s  newfun_env t) body)) @ epilogue)


let rec compile ast env asm=
	match ast with
	| [] -> asm
	| x :: xs -> match (compile_toplevel x env) with
				| (newenv,newasm) -> compile xs newenv (asm @ newasm)


let ()=
	let ast=Myparser.prog Mylexer.token (Lexing.from_channel stdin) in
		print_asm stdout (compile ast [[]] [])


