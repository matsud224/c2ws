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

type varinfo= StaticVar of typename * int (*アドレス*) | LocalVar of typename * int (*オフセット*) * int(*次の変数の開始位置*)
			| ToplevelFunction of typename * int (*ラベル番号*) | Field of typename * int (*オフセット*) * int(*次の変数の開始位置*) | EnumerationConst of int 
type taginfo= StructTag of int(*サイズ*) * ((identifier * varinfo) list) | UnionTag of int(*サイズ*) * ((identifier * varinfo) list) | EnumTag of ((identifier * varinfo) list)

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
	
type symtable={
	env: (identifier * varinfo) list list;
	tags: (identifier * taginfo) list;
	labels: (identifier * int) list;
}
	
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
		| None -> let addr=get_staticvar (sizeof t len) in
			( {symtbl with env=((id,StaticVar(t,addr))::x) :: (List.tl symtbl.env)} , before_asm @ [] )
		| Some(_)  -> raise Defined_before

let addlocalvar id t len symtbl init before_asm=
	let x=List.hd symtbl.env in
		match (find_flame id x) with
		| None -> let last_addr=last_localaddr symtbl in
			( { symtbl with env=((id,LocalVar(t,last_addr,last_addr+(sizeof t len))) :: x) :: symtbl.env } , before_asm @ [] )
		| Some(_)  -> raise Defined_before

let addtoplevelfun id t symtbl=
	match (find_flame id (List.hd symtbl.env)) with
	| None -> let newlabel= get_label () and x=List.hd symtbl.env in
		{ symtbl with env = ((id,ToplevelFunction(t,newlabel))::x) :: (List.tl symtbl.env) }
	| Some(_)  -> raise Defined_before

let find_env id symtbl=
	let rec find_env_sub id env=
		match env with
		| [] -> None
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
	| Indirection(exp) -> (let (exp_a,Pointer(t))=compile_exp exp symtbl in (exp_a,Pointer(t)))
	| _ -> raise Invalid_lvalue

and compile_exp x symtbl=
	match x with
	| Minus(exp) -> let (a1,t1)=compile_exp exp symtbl in (a1 @ [PUSH(-1);MUL],IntType)
	| Not(exp) -> let (a1,IntType)=compile_exp exp symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
					(a1 @ [JZ(zlabel);PUSH(0);JUMP(margelabel);LABEL(zlabel);PUSH(1);LABEL(margelabel)], IntType)
	| Add(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl  in let asm=a1 @ a2 @ [ADD] in
							match (t1,t2) with
							| (IntType,IntType) -> (asm,IntType)
							| (Pointer(t),IntType) -> (asm,Pointer(t))
							| (IntType,Pointer(t)) -> (asm,Pointer(t))
							| _ -> raise (Type_error (t1,t2)))
	| Sub(exp1,exp2) -> (let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl  in let asm=a1 @ a2 @ [SUB] in
							match (t1,t2) with
							| (IntType,IntType) -> (asm,IntType)
							| (Pointer(t),IntType) -> (asm,Pointer(t))
							| (Pointer(pt1),Pointer(pt2)) when pt1=pt2 -> (asm,IntType)
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
	| Eq(exp1,exp2) -> let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
						if t1<>t2 then raise (Type_error (t1,t2))
						else (a1 @ a2 @ [SUB;JZ(zlabel);PUSH(0);JUMP(margelabel);LABEL(zlabel);PUSH(1);LABEL(margelabel)], IntType)
	| NotEq(exp1,exp2) -> let (a1,t1)=compile_exp exp1 symtbl and (a2,t2)=compile_exp exp2 symtbl and (zlabel,margelabel)=(get_label (),get_label ()) in
							if t1<>t2 then raise (Type_error (t1,t2))
							else (a1 @ a2 @ [SUB;JZ(zlabel);PUSH(1);JUMP(margelabel);LABEL(zlabel);PUSH(0);LABEL(margelabel)], IntType)
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
	| Assign(target,exp) -> let (target_a,Pointer(target_t))=compile_lvalue target symtbl and (exp_a,exp_t)=compile_exp exp symtbl in
							if target_t<>exp_t then raise (Type_error (target_t,exp_t)) else (exp_a @ [DUP] @ target_a @ [SWAP;STORE],exp_t)
	| AssignAdd(target,exp) -> compile_exp (Assign (target,(Add (target,exp)))) symtbl
	| AssignSub(target,exp) -> compile_exp (Assign (target,(Sub (target,exp)))) symtbl
	| AssignMul(target,exp) -> compile_exp (Assign (target,(Mul (target,exp)))) symtbl
	| AssignDiv(target,exp) -> compile_exp (Assign (target,(Div (target,exp)))) symtbl
	| AssignMod(target,exp) -> compile_exp (Assign (target,(Mod (target,exp)))) symtbl
	| PreIncrement(exp) -> compile_exp (AssignAdd(exp,(IntConst(1)))) symtbl
	| PreDecrement(exp) -> compile_exp (AssignAdd(exp,(IntConst(-1)))) symtbl
	| PostIncrement(exp) -> let (target_a,Pointer(target_t))=compile_lvalue exp symtbl and (texp_a,texp_t)=compile_exp exp symtbl in
								(texp_a @ [DUP;PUSH(1);ADD] @ target_a @ [SWAP;STORE], target_t)
	| PostDecrement(exp) -> let (target_a,Pointer(target_t))=compile_lvalue exp symtbl and (texp_a,texp_t)=compile_exp exp symtbl in
								(texp_a @ [DUP;PUSH(-1);ADD] @ target_a @ [SWAP;STORE], target_t)
	| VarRef(id) -> (match (find_env id symtbl) with
					| Some(StaticVar(Array(t),a)) -> ([PUSH(a)], Pointer(t))
					| Some(StaticVar(t,a)) -> ([PUSH(a); RETRIEVE], t)
					| Some(LocalVar(Array(t),_,a)) -> ([PUSH(0);RETRIEVE;PUSH(-a+1);ADD], Pointer(t))
					| Some(LocalVar(t,_,a)) -> (retrieve_sprel (-a+1), t)
					| _ -> raise Undefined_variable)
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
	| Address(exp) -> compile_lvalue exp symtbl
	| Indirection(exp) -> let (exp_a,Pointer(t))=compile_exp exp symtbl in (exp_a @ [RETRIEVE],t)
	| CommaExpr(exp1,exp2) -> let (exp1_a,_)=compile_exp exp1 symtbl and (exp2_a,exp2_t)=compile_exp exp2 symtbl in (exp1_a @ exp2_a, exp2_t)
	| IntConst(const) -> ([PUSH(const)], IntType)
	(*| StringConst(const) ->
	| ExprSizeof of exp ->
	| TypeSizeof of typename
	| ArrowRef of exp * identifier
	| FieldRef of exp * identifier
	| CastExpr of typename * exp
	| ConditionalExpr of exp * exp * exp*)


let rec compile_stat x symtbl returntype returnlabel retvaladdr(*SPからの相対位置*) breaklabel continuelabel (*break,continuelabel共にoption*)=
	match x with
	| IfStat(cond,cons,alt) -> let (cond_a,IntType)=compile_exp cond symtbl and cons_a=compile_stat cons symtbl returntype returnlabel retvaladdr breaklabel continuelabel
								and alt_a=compile_stat alt symtbl returntype returnlabel retvaladdr breaklabel continuelabel in
								let elselabel=get_label () and endiflabel=get_label () in 
										cond_a @ [JZ(elselabel)] @ cons_a @ [JUMP(endiflabel)] @ [LABEL(elselabel)] @ alt_a @ [LABEL(endiflabel)]
	| WhileStat(cond, stat) -> let beginlabel=get_label () and endlabel=get_label () in
								let (cond_a,IntType)=compile_exp cond  symtbl and stat_a=compile_stat stat  symtbl returntype returnlabel retvaladdr (Some(endlabel)) continuelabel in
										[LABEL(beginlabel)] @ cond_a @ [JZ(endlabel)] @ stat_a @ [JUMP(beginlabel); LABEL(endlabel)]
	| DoStat(cond, stat) -> let beginlabel=get_label () and endlabel=get_label () in
								let (cond_a,IntType)=compile_exp cond  symtbl and stat_a=compile_stat stat  symtbl returntype returnlabel retvaladdr (Some(endlabel)) continuelabel in
										[LABEL(beginlabel)] @ stat_a @ cond_a @ [JZ(endlabel); JUMP(beginlabel); LABEL(endlabel)]
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
											and statasm=compile_stat stat symtbl returntype returnlabel retvaladdr (Some(endlabel)) (Some(contlabel)) in
												initasm @ [LABEL(nextlabel)] @ condasm @ [JZ(endlabel)]  @ statasm @ [LABEL(contlabel)] @ continueasm @ [JUMP(nextlabel);LABEL(endlabel)]
	| ReturnStat(Some(exp)) -> let (a1,t1)=compile_exp exp symtbl in
							if t1!=returntype then raise (Type_error (t1,returntype))
							else a1 @ (store_sprel retvaladdr) @ [JUMP(returnlabel)]
	| ReturnStat(None) -> if returntype <> VoidType then raise (Type_error (returntype,VoidType)) else [JUMP(returnlabel)]
	| ExpStat(exp) -> let (a,t)=compile_exp exp symtbl in (a @ (if t<>VoidType then [DISCARD] else [])) (*Voidじゃない場合、スタックにゴミが残るからポップ*)
 	| Block(stats) -> List.concat (List.map (fun s -> compile_stat s symtbl returntype returnlabel retvaladdr breaklabel continuelabel) stats) 
	| ContinueStat -> (match continuelabel with None -> raise ContinueStat_not_within_loop | Some(lb) ->  [JUMP(lb)])
	| BreakStat -> (match breaklabel with None -> raise BreakStat_not_within_loop | Some(lb) -> [JUMP(lb)])
	| PassStat -> []
	(*|	Label of identifier
	|	CaseLabel of int_const
	|	DefaultLabel
	|	SwitchStat of exp * stat
	|	GotoStat of identifier*)

let optlen l=match l with None->1 | Some(v)->v

let make_field_assoc c symtbl=
	match c with
	| StructDef(id,fields) -> (List.fold_left (fun acc ele -> match ele with
															| FieldDecl(Pointer(StructType(t)),name,len) when t=id ->  (*自身へのポインタは許可*)
															| FieldDecl(t,name,len) -> 
												) 0 fields)
	| UnionDef(id,fields) -> 
	| EnumDef(id,decls) -> 

let rec compile_toplevel x symtbl=
	match x with
	| GlobalVarDecl(decllist) -> 
		(List.fold_left (fun acc decl -> match (acc,decl) with
										| ((acc_st,acc_asm), VarDecl(t,id,len,init)) -> addstaticvar id t (optlen len) acc_st init acc_asm
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
															| ((acc_st,acc_asm), VarDecl(t,id,len,init)) -> addlocalvar id t (optlen len) acc_st init acc_asm
											) (defined_st,[]) vardecl) in			
			let newfun_st=List.fold_left (fun acc param -> match param with Parameter(ty,id) -> let (e,a)=addlocalvar id ty 1 acc None [] in e) 
																						{localdef_st with env=[] :: localdef_st.env} params in
			let stext_size=(last_localaddr newfun_st)
			and ret_label=get_label () in
			let prologue=(sp_add stext_size) @ localdef_asm and epilogue= [LABEL(ret_label)] @ (sp_add (-stext_size)) @ [RETURN]
			and this_label= match (find_env id defined_st) with
							| Some (ToplevelFunction(ty,lb)) -> lb
			in
				(defined_st, [LABEL(this_label)] @ prologue @ (List.concat (List.map (fun s -> compile_stat s  newfun_st t ret_label ((-stext_size)) None None) body)) @ epilogue)
	| StructDef(id,fields) as s -> if List.mem_assoc id symtbl.tags=true then raise Defined_before
									else let ({symtbl with tags: (id,StructTag(calc_comptype_size s,make_fieldassoc fields))::symtbl.tags},[])
	(*| UnionDef of identifier * fielddecl list
	| EnumDef of identifier * enumdecl list*)

let rec compile ast symtbl asm=
	match ast with
	| [] -> (match (find_flame "main" (List.hd (List.rev symtbl.env))) with
						| None ->  raise Notfound_main
						| Some(ToplevelFunction(t,lb)) -> [PUSH(0);PUSH(get_staticvar 1(*静的変数領域の次からスタック領域*));STORE;CALL(lb);END] @ asm )
	| x :: xs -> match (compile_toplevel x symtbl) with
				| (newenv,newasm) -> compile xs newenv (asm @ newasm)


let ()=
	let ast=Myparser.prog Mylexer.token (Lexing.from_channel stdin) in
		try assemble stdout (compile ast {env=[[]]; tags=[]; labels=[]} [])
		with Type_error(t1,t2) -> fprintf stderr "Type_error: expected %s but %s\n" (print_type t2) (print_type t1)

