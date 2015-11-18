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
		if n<2 then ((if n < 0 then "t" else "s" (*符号ビット*)) :: (if n = 0 then "s" else "t") :: l)
		else int_enc_sub (n/2) ((if n mod 2 = 0 then "s" else "t") :: l)
	in
		String.concat "" (int_enc_sub n ["l"])


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

let ()=
	let p=Myparser.prog Mylexer.token (Lexing.from_channel stdin) in
		(assemble stdout [PUSH(3);PUSH(5);ADD;OUTINT;END])
