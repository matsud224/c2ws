open Syntax

let main=
	let p=Myparser.prog Mylexer.token (Lexing.from_channel stdin) in
	unit
