let ic = open_in "test.c" ;;
open Syntax;;
let s=Myparser.prog Mylexer.token (Lexing.from_channel ic);;
