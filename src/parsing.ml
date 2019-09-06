(**
   Copyright 2019 Google LLC

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

type error = [ `LexerError of string | `ParserError of string ]

let parse lexbuf =
  try Ok (Parser.program Lexer.read lexbuf) with
  | Lexer.Error msg ->
      let loc = (lexbuf.lex_curr_p, lexbuf.lex_curr_p) in
      Error Located.{ loc; value = `LexerError msg }
  | Parser.Error ->
      let loc = (lexbuf.lex_curr_p, lexbuf.lex_curr_p) in
      Error Located.{ loc; value = `ParserError "parsing error" }
