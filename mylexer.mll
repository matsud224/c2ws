{
open Myparser
open String
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['A'-'Z' 'a'-'z' '_']
let escape = '\\' ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '?' '\'' '\"' '0']


rule token = parse
| "//"
	{ linecomment lexbuf }
| "/*"
	{ blockcomment 1 lexbuf }
| "sizeof"
    { SIZEOF }
| "static"
    { STATIC }
| "typedef"
    { TYPEDEF }
| "const"
    { CONST }
| "void"
    { VOID }
| "char"
    { CHAR }
| "short"
    { SHORT }
| "int"
    { INT }
| "struct"
    { STRUCT }
| "union"
    { UNION }
| "enum"
    { ENUM }
| "case"
    { CASE }
| "default"
    { DEFAULT }
| "if"
    { IF }
| "else"
    { ELSE }
| "switch"
    { SWITCH }
| "while"
    { WHILE }
| "do"
    { DO }
| "for"
    { FOR }
| "goto"
    { GOTO }
| "continue"
    { CONTINUE }
| "break"
    { BREAK }
| "return"
    { RETURN }
| alpha (digit|alpha)*
    (* 以上の予約語にマッチしなければ変数名として処理 *)
    { Id(Lexing.lexeme lexbuf) }
| digit+
    { IntConst(int_of_string (Lexing.lexeme lexbuf)) }
| '\'' ([' ' - '~'] | escape) '\''
    { 
		let len=(length (Lexing.lexeme lexbuf)) - 2 in
		let cut=sub (Lexing.lexeme lexbuf) 1 len in
		let escapedstr=escaped cut in
			IntConst(int_of_char (get escapedstr 0)) 
	}
| '\"' ([' ' - '~'] | escape)* '\"'
    { 
		let len = (length (Lexing.lexeme lexbuf))-2 in
		StringConst(sub (Lexing.lexeme lexbuf) 1 len)
	}
| space+
    (* 空白をスキップして字句解析を続行 *)
    { token lexbuf }
| '='
    { ASSIGNEQ }
| "=="
    { EQUAL }
| "!="
    { NOTEQUAL }
| '+'
    { PLUS }
| '-'
    { MINUS }
| '*'
    { ASTERISK }
| '/'
    { SLASH }
| '%'
    { PERCENT }
| "<<"
    { LSHIFT }
| ">>"
    { RSHIFT }
| "+="
    { ASSIGNPLUS }
| "-="
    { ASSIGNMINUS }
| "*="
    { ASSIGNASTERISK }
| "/="
    { ASSIGNSLASH }
| "%="
    { ASSIGNPERCENT }
| '['
    { LBRACKET }
| ']'
    { RBRACKET }
| '('
    { LPAREN }
| ')'
    { RPAREN }
| '{'
    { LBRACE }
| '}'
    { RBRACE }
| '>'
    { GREATER }
| '<'
    { LESSER }
| ">="
    { GREATEREQ }
| "<="
    { LESSEREQ }
| ';'
    { SEMICOLON }
| ','
    { COMMA }
| '.'
    { DOT }
| "..."
    { THREEDOT }
| ':'
    { COLON }
| '?'
    { QUESTION }
| '!'
    { EXCLAMATION }
| '&'
    { AND }
| "&&"
    { LOGICALAND }
| "||"
    { LOGICALOR }
| "->"
    { ARROW }
| "++"
    { INCREMENT }
| "--"
    { DECREMENT }
| eof
	{ EOF }
| _
    (* 以上にマッチしない場合はエラーとして例外を発生 *)
    { failwith
        (Printf.sprintf
           "unknown token %s near characters %d-%d"
           (Lexing.lexeme lexbuf)
           (Lexing.lexeme_start lexbuf)
           (Lexing.lexeme_end lexbuf)) }

and blockcomment depth = parse
| "/*"
	{ blockcomment (depth+1) lexbuf }
| "*/"
	{ if (depth-1)=0 then token lexbuf (*ふつうのモードへ戻る*) else blockcomment (depth-1) lexbuf }
| _ 
	{ blockcomment depth lexbuf }

and linecomment = parse
| '\n' | eof
	{ token lexbuf }
| _
	{ linecomment lexbuf }
