(*
                         CS 51 Final Project
                      MiniML -- Lexical Analyzer

 *)

{
  open Printf ;;
  open Miniml_parse ;; (* need access to parser's token definitions *)

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let keyword_table =
    create_hashtable 8 [
                       ("if", IF);
                       ("in", IN);
                       ("then", THEN);
                       ("else", ELSE);
                       ("let", LET);
                       ("raise", RAISE);
                       ("rec", REC);
                       ("true", TRUE);
                       ("false", FALSE);
                       ("lambda", FUNCTION);
                       ("fun", FUNCTION);
                       ("function", FUNCTION);
                       ("try", TRY);
                       ("with", WITH)
                     ]

  let sym_table =
    create_hashtable 8 [
                       ("=", EQUALS);
                       ("<", LESSTHAN);
                       (".", DOT);
                       ("->", DOT);
                       (";;", EOF);
                       ("~-", NEG);
                       ("+", PLUS);
                       ("-", MINUS);
                       ("*", TIMES);
                       ("(", OPEN);
                       (")", CLOSE);
                       ("^", CONCAT);
                       ("#", LENGTH)
                     ]
}

let digit = ['0'-'9']
let id = ['a'-'z'] ['a'-'z' '0'-'9']*
let sym = ['(' ')'] | (['+' '-' '*' '.' '=' '~' ';' '<' '>' '^' '#']+)
(* table for defining a string: quote mark, any string, end quote *)
let str = ['"'] [^ '"']* ['"']

rule token = parse
  | digit+ as inum
        { let num = int_of_string inum in
          INT num
        }
  | id as word
        { try
            let token = Hashtbl.find keyword_table word in
            token
          with Not_found ->
            ID word
        }
  | sym as symbol
        { try
            let token = Hashtbl.find sym_table symbol in
            token
          with Not_found ->
            printf "Ignoring unrecognized token: %s\n" symbol;
            token lexbuf
        }

  (* for defining strings and floats. string removes the quotation marks,
     and float joins to nums with a period and converts to a float *)
  | str as literal
        {
          let open String in
          STRING (sub literal 1 (length literal - 2))
        }
  | digit+ '.' digit* as flt
        {
          FLOAT (float_of_string flt)
        }
  | '{' [^ '\n']* '}'   { token lexbuf }    (* skip one-line comments *)
  | [' ' '\t' '\n']     { token lexbuf }    (* skip whitespace *)
  | _ as c                                  (* warn and skip unrecognized characters *)
        { printf "Ignoring unrecognized character: %c\n" c;
          token lexbuf
        }
  | eof
        { raise End_of_file }
