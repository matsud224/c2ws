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
	| Pointer (st) -> "*" ^ (print_type st)
	| Func (rt , params) -> (print_type rt) ^ "(" ^ (String.concat ", " (List.map print_type params)) ^ ")"

type varinfo= StaticVar of typename * int (*アドレス*) | LocalVar of typename * int (*オフセット*) * int(*次の変数の開始位置*) | ToplevelFunction of typename * int (*ラベル番号*)

exception Defined_before
exception Type_error of typename * typename
exception Undefined_variable
exception Undefined_function
exception Notfound_main
exception ContinueStat_not_within_loop
exception BreakStat_not_within_loop
exception Invalid_lvalue

let _label=ref 0
let get_label ()=
	_label := !_label + 1;
	!_label

let _stvar=ref 2 (*0はSPに、1はInput時の一時格納先として使われている*)
let get_staticvar size=
	let temp = !_stvar in
		_stvar := !_stvar+size; temp

let rec sizeof t len=
	match t with
	| IntType -> 1
	| VoidType -> 0
	| Array(s) -> len * (sizeof s 1)
	| Pointer(s) -> 1
	| Func(_,_) -> 0

let rec find_flame id fl= match fl with
						| (i,c) :: xs when i=id -> Some(c)
						| _ :: xs -> find_flame id xs
						| [] -> None

let last_localaddr env=
	List.fold_left  (
			fun acc ele -> match ele with
			| (_,LocalVar(_,a,n)) -> max acc n
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
			((id,LocalVar(t,last_addr,last_addr+(sizeof t len))) :: x) :: env
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

let rec compile_lvalue x env=
	match x with
	| VarRef(id) -> (match (find_env id env) with
					| Some(StaticVar(Array(t),a)) -> ([PUSH(a)], t)
					| Some(StaticVar(t,a)) -> ([PUSH(a)], t)
					| Some(LocalVar(Array(t),a,_)) -> ([PUSH(0);RETRIEVE;PUSH(-a);ADD], Pointer(t))
					| Some(LocalVar(t,a,_)) -> ([PUSH(0);RETRIEVE;PUSH(-a);ADD], t)
					| _ -> raise Undefined_variable)
	| ArrayRef(id,index) -> (let (idx_a,IntType)=compile_exp index env in
							match (find_env id env) with
							| Some(StaticVar(Pointer(t),a)) -> ([PUSH(a);RETRIEVE;PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD], t)
							| Some(LocalVar(Pointer(t),a,_)) -> ([PUSH(0);RETRIEVE;PUSH(-a);ADD;RETRIEVE;PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD], t)
							| Some(StaticVar(Array(t),a)) -> ([PUSH(a);PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD], t)
							| Some(LocalVar(Array(t),a,_)) -> ([PUSH(0);PUSH(-a);ADD;RETRIEVE;PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD], t)
							| _ -> raise Undefined_variable)
	| Indirection(exp) -> (let (exp_a,Pointer(t))=compile_exp exp env in (exp_a,t))
	| _ -> raise Invalid_lvalue

and compile_exp x env=
	match x with
	| Minus(exp) -> let (a1,t1)=compile_exp exp env in (a1 @ [PUSH(-1);MUL],IntType)
	| Not(exp) -> let (a1,IntType)=compile_exp exp env and (zlabel,margelabel)=(get_label (),get_label ()) in
					(a1 @ [JZ(zlabel);PUSH(0);JUMP(margelabel);LABEL(zlabel);PUSH(1);LABEL(margelabel)], IntType)
	| Add(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env  in
							if t1<>t2 then raise (Type_error (t1,t2)) else (a1 @ a2 @ [ADD],IntType)
	| Sub(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env  in
							if t1<>t2 then raise (Type_error (t1,t2)) else (a1 @ a2 @ [SUB],IntType)
	| Mul(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env  in
							if t1<>t2 then raise (Type_error (t1,t2)) else (a1 @ a2 @ [MUL],IntType)
	| Div(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env  in
							if t1<>t2 then raise (Type_error (t1,t2)) else (a1 @ a2 @ [DIV],IntType)
	| Mod(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env  in
							if t1<>t2 then raise (Type_error (t1,t2)) else (a1 @ a2 @ [MOD] , IntType)
	| Eq(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env and (zlabel,margelabel)=(get_label (),get_label ()) in
						if t1<>t2 then raise (Type_error (t1,t2))
						else (a1 @ a2 @ [SUB;JZ(zlabel);PUSH(0);JUMP(margelabel);LABEL(zlabel);PUSH(1);LABEL(margelabel)], IntType)
	| NotEq(exp1,exp2) -> let (a1,t1)=compile_exp exp1 env and (a2,t2)=compile_exp exp2 env and (zlabel,margelabel)=(get_label (),get_label ()) in
							if t1<>t2 then raise (Type_error (t1,t2))
							else (a1 @ a2 @ [SUB;JZ(zlabel);PUSH(1);JUMP(margelabel);LABEL(zlabel);PUSH(0);LABEL(margelabel)], IntType)
	| Lesser(exp1,exp2) -> let (a1,IntType)=compile_exp exp1 env and (a2,IntType)=compile_exp exp2 env and (nlabel,margelabel)=(get_label (),get_label ()) in
							(a1 @ a2 @ [SUB;JN(nlabel);PUSH(0);JUMP(margelabel);LABEL(nlabel);PUSH(1);LABEL(margelabel)], IntType)
	| Greater(exp1,exp2) -> compile_exp (Lesser (exp2,exp1)) env
	| LesserEq(exp1,exp2) -> compile_exp (Not (Lesser (exp2,exp1))) env
	| GreaterEq(exp1,exp2) -> compile_exp (Not (Lesser (exp1,exp2))) env
	| LogicalAnd(exp1,exp2) -> let (a1,IntType)=compile_exp exp1 env and (a2,IntType)=compile_exp exp2 env and (zlabel,margelabel)=(get_label (),get_label ()) in
									(a1 @ [JZ(zlabel)] @ a2 @ [JZ(zlabel);PUSH(1);JUMP(margelabel);LABEL(zlabel);PUSH(0);LABEL(margelabel)], IntType)
	| LogicalOr(exp1,exp2) -> let (a1,IntType)=compile_exp exp1 env and (a2,IntType)=compile_exp exp2 env
								and (nextlabel,falselabel,margelabel)=(get_label (),get_label (),get_label ()) in
								(a1 @ [JZ(nextlabel)] @ [PUSH(1);JUMP(margelabel);LABEL(nextlabel)]
								@ a2 @ [JZ(falselabel);PUSH(1);JUMP(margelabel);LABEL(falselabel);PUSH(0);LABEL(margelabel)], IntType)
	| Assign(target,exp) -> let (target_a,target_t)=compile_lvalue target env and (exp_a,exp_t)=compile_exp exp env in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (exp_a @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| AssignAdd(target,exp) -> let (target_a,target_t)=compile_lvalue target env and (texp_a,texp_t)=compile_exp target env and (exp_a,exp_t)=compile_exp exp env in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (texp_a @ exp_a @ [ADD] @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| AssignSub(target,exp) -> let (target_a,target_t)=compile_lvalue target env and (texp_a,texp_t)=compile_exp target env and (exp_a,exp_t)=compile_exp exp env in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (texp_a @ exp_a @ [SUB] @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| AssignMul(target,exp) -> let (target_a,target_t)=compile_lvalue target env and (texp_a,texp_t)=compile_exp target env and (exp_a,exp_t)=compile_exp exp env in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (texp_a @ exp_a @ [MUL] @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| AssignDiv(target,exp) -> let (target_a,target_t)=compile_lvalue target env and (texp_a,texp_t)=compile_exp target env and (exp_a,exp_t)=compile_exp exp env in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (texp_a @ exp_a @ [DIV] @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| AssignMod(target,exp) -> let (target_a,target_t)=compile_lvalue target env and (texp_a,texp_t)=compile_exp target env and (exp_a,exp_t)=compile_exp exp env in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (texp_a @ exp_a @ [MOD] @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| PreIncrement(exp) -> compile_exp (AssignAdd(exp,(IntConst(1)))) env
	| PreDecrement(exp) -> compile_exp (AssignAdd(exp,(IntConst(-1)))) env
	| PostIncrement(exp) -> let (target_a,target_t)=compile_lvalue exp env and (texp_a,texp_t)=compile_exp exp env in
								(texp_a @ [DUP;PUSH(1);ADD] @ target_a @ [SWAP;STORE], target_t)
	| PostDecrement(exp) -> let (target_a,target_t)=compile_lvalue exp env and (texp_a,texp_t)=compile_exp exp env in
								(texp_a @ [DUP;PUSH(-1);ADD] @ target_a @ [SWAP;STORE], target_t)
	| VarRef(id) -> (match (find_env id env) with
					| Some(StaticVar(Array(t),a)) -> ([PUSH(a)], Pointer(t))
					| Some(StaticVar(t,a)) -> ([PUSH(a); RETRIEVE], t)
					| Some(LocalVar(Array(t),a,_)) -> ([PUSH(0);RETRIEVE;PUSH(-a);ADD], Pointer(t))
					| Some(LocalVar(t,a,_)) -> (retrieve_sprel (-a), t)
					| _ -> raise Undefined_variable)
	| Call("geti",[]) -> ([PUSH(1);ININT;PUSH(1);RETRIEVE], IntType) (*Input系命令は、スタックに格納先のヒープのアドレスをおいておかないといけない*)
	| Call("getc",[]) -> ([PUSH(1);INCHAR;PUSH(1);RETRIEVE], IntType)
	| Call("puti",[arg1]) -> let (arg_a,IntType)=compile_exp arg1 env in (arg_a @ [OUTINT],VoidType)
	| Call("putp",[arg1]) -> let (arg_a,Pointer(t))=compile_exp arg1 env in (arg_a @ [OUTINT],VoidType)
	| Call("putc",[arg1]) -> let (arg_a,IntType)=compile_exp arg1 env in (arg_a @ [OUTCHAR],VoidType)
	| Call(id,args) ->  (match find_flame id (List.hd (List.rev env)) with
							| Some(ToplevelFunction(Func(rett,params),label)) -> let args=List.map (fun exp -> compile_exp exp env) args in
									let asts=List.map2 (
										fun arg par -> 
											match (arg,par) with
											| ((a,t),ty) -> if t<>ty then raise (Type_error (t,ty)) else (a,sizeof t 1)
									) args params
									in
									let rec argpush_asmgen args offset asm = match args with
																	| [] -> asm
																	| (a,s) :: rest -> argpush_asmgen rest (offset+s) (asm @ a @ (store_sprel offset))
									in
										((argpush_asmgen (asts) ((sizeof rett 1)+1) []) @ (sp_add (sizeof rett 1))
											@ [CALL(label)] @ (retrieve_sprel 0) @ (sp_add (-(sizeof rett 1))) , rett)
							| _ -> raise Undefined_function)
	| ArrayRef(id,index) -> (let (idx_a,IntType)=compile_exp index env in
							match (find_env id env) with
							| Some(StaticVar(Pointer(t),a)) -> ([PUSH(a);RETRIEVE;PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD; RETRIEVE], t)
							| Some(LocalVar(Pointer(t),a,_)) -> ([PUSH(0);RETRIEVE;PUSH(-a);ADD;RETRIEVE;PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD;RETRIEVE], t)
							| Some(StaticVar(Array(t),a)) -> ([PUSH(a);PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD; RETRIEVE], t)
							| Some(LocalVar(Array(t),a,_)) -> ([PUSH(0);PUSH(-a);ADD;RETRIEVE;PUSH(-(sizeof t 1))] @ idx_a @ [MUL;ADD;RETRIEVE], t)
							| _ -> raise Undefined_variable)
	| Address(exp) -> compile_lvalue exp env
	| Indirection(exp) -> let (exp_a,exp_t)=compile_lvalue exp env in (exp_a @ [RETRIEVE],exp_t)
	| CommaExpr(exp1,exp2) -> let (exp1_a,_)=compile_exp exp1 env and (exp2_a,exp2_t)=compile_exp exp2 env in (exp1_a @ exp2_a, exp2_t)
	| IntConst(const) -> ([PUSH(const)], IntType)
	(*| StringConst(const) ->*) 


let rec compile_stat x env returntype returnlabel retvaladdr(*SPからの相対位置*) breaklabel continuelabel (*break,continuelabel共にoption*)=
	match x with
	| IfStat(cond,cons,alt) -> let (cond_a,IntType)=compile_exp cond env and cons_a=compile_stat cons env returntype returnlabel retvaladdr breaklabel continuelabel
								and alt_a=compile_stat alt env returntype returnlabel retvaladdr breaklabel continuelabel in
								let elselabel=get_label () and endiflabel=get_label () in 
										cond_a @ [JZ(elselabel)] @ cons_a @ [JUMP(endiflabel)] @ [LABEL(elselabel)] @ alt_a @ [LABEL(endiflabel)]
	| WhileStat(cond, stat) -> let beginlabel=get_label () and endlabel=get_label () in
								let (cond_a,IntType)=compile_exp cond  env and stat_a=compile_stat stat  env returntype returnlabel retvaladdr (Some(endlabel)) continuelabel in
										[LABEL(beginlabel)] @ cond_a @ [JZ(endlabel)] @ stat_a @ [JUMP(beginlabel); LABEL(endlabel)]
	| DoStat(cond, stat) -> let beginlabel=get_label () and endlabel=get_label () in
								let (cond_a,IntType)=compile_exp cond  env and stat_a=compile_stat stat  env returntype returnlabel retvaladdr (Some(endlabel)) continuelabel in
										[LABEL(beginlabel)] @ stat_a @ cond_a @ [JZ(endlabel); JUMP(beginlabel); LABEL(endlabel)]
	| ForStat(init, cond, continue, stat) -> let (nextlabel,contlabel,endlabel)=(get_label (),get_label (),get_label ()) in 
											let initasm=match init with
														| None -> []
														| Some(exp) -> let (a,t)=compile_exp exp env in (a @ (if t <> VoidType then [DISCARD] else []))
											and condasm=match cond with
														| None -> [PUSH(1)]
														| Some(exp) -> let (a,t)=compile_exp exp env in if t <> IntType then raise (Type_error (t,IntType)) else a
											and continueasm=match continue with
															| None -> []
															| Some(exp) -> let (a,t)=compile_exp exp env in (a @ (if t <> VoidType then [DISCARD] else []))
											and statasm=compile_stat stat env returntype returnlabel retvaladdr (Some(endlabel)) (Some(contlabel)) in
												initasm @ [LABEL(nextlabel)] @ condasm @ [JZ(endlabel)]  @ statasm @ [LABEL(contlabel)] @ continueasm @ [JUMP(nextlabel);LABEL(endlabel)]
	| ReturnStat(Some(exp)) -> let (a1,t1)=compile_exp exp env in
							if t1!=returntype then raise (Type_error (t1,returntype))
							else a1 @ (store_sprel retvaladdr) @ [JUMP(returnlabel)]
	| ReturnStat(None) -> if returntype <> VoidType then raise (Type_error (returntype,VoidType)) else [JUMP(returnlabel)]
	| ExpStat(exp) -> let (a,t)=compile_exp exp env in (a @ (if t<>VoidType then [DISCARD] else [])) (*Voidじゃない場合、スタックにゴミが残るからポップ*)
 	| Block(stats) -> List.concat (List.map (fun s -> compile_stat s env returntype returnlabel retvaladdr breaklabel continuelabel) stats) 
	| ContinueStat -> (match continuelabel with None -> raise ContinueStat_not_within_loop | Some(lb) ->  [JUMP(lb)])
	| BreakStat -> (match breaklabel with None -> raise BreakStat_not_within_loop | Some(lb) -> [JUMP(lb)])
	| PassStat -> []


let rec make_ptrtype t depth = if depth=0 then t else make_ptrtype (Pointer(t)) (depth-1)

let rec compile_toplevel x env=
	match x with
	| GlobalVarDecl(VarDecl(t,children)) -> 
		(List.fold_left (fun acc child -> match child with
										| VarDeclChild(id,Some(size),ptrdep) -> addstaticvar id (Array(make_ptrtype t ptrdep)) size acc
										| VarDeclChild(id,None,ptrdep) -> addstaticvar id (make_ptrtype t ptrdep) 1 acc
						 ) env children , [] )
	| PrototypeDecl(t,id,params) ->
		((addtoplevelfun id (Func(t, List.map (function Parameter(ty,_) -> ty) params)) env), [])
	| FuncDef(t,id,params,vardecl,body) ->
		let functype=(Func(t, List.map (function Parameter(ty,_) -> ty) params)) in
		let defined_env= match (find_env id env) with
						| Some (ToplevelFunction(ty,lb)) -> if ty <> functype then raise (Type_error (t,functype)) else env
						| _ -> addtoplevelfun id functype env
		in
			let localdef_env=List.fold_left (fun acc decl -> match decl with VarDecl(t,children) -> 
												(List.fold_left (fun acc child -> match child with
																			| VarDeclChild(id,Some(size),ptrdep) -> addlocalvar id (Array(make_ptrtype t ptrdep)) size acc
																			| VarDeclChild(id,None,ptrdep) -> addlocalvar id (make_ptrtype t ptrdep) 1 acc
												) acc children)
											) defined_env vardecl in			
			let newfun_env=List.fold_left (fun acc param -> match param with Parameter(ty,id) -> addlocalvar id ty 1 acc) ([] :: localdef_env) params in
			let stext_size=(last_localaddr newfun_env)
			and ret_label=get_label () in
			let prologue=(sp_add stext_size) and epilogue= [LABEL(ret_label)] @ (sp_add (-stext_size)) @ [RETURN]
			and this_label= match (find_env id defined_env) with
							| Some (ToplevelFunction(ty,lb)) -> lb
			in
				(defined_env, [LABEL(this_label)] @ prologue @ (List.concat (List.map (fun s -> compile_stat s  newfun_env t ret_label ((-stext_size)) None None) body)) @ epilogue)


let rec compile ast env asm=
	match ast with
	| [] -> (match (find_flame "main" (List.hd (List.rev env))) with
						| None ->  raise Notfound_main
						| Some(ToplevelFunction(t,lb)) -> [PUSH(0);PUSH(get_staticvar 1(*静的変数領域の次からスタック領域*));STORE;CALL(lb);END] @ asm )
	| x :: xs -> match (compile_toplevel x env) with
				| (newenv,newasm) -> compile xs newenv (asm @ newasm)


let ()=
	let ast=Myparser.prog Mylexer.token (Lexing.from_channel stdin) in
		try assemble stdout (compile ast [[]] [])
		with Type_error(t1,t2) -> fprintf stderr "Type_error: expected %s but %s\n" (print_type t2) (print_type t1)


