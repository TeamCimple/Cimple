{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ['0'-'9']+ as lit { INT_LITERAL(int_of_string lit) }
| "auto" { AUTO }
| "register" { REGISTER }
| "static" { STATIC }
| "extern" { EXTERN }
| "typedef" { TYPEDEF }
| "void" { VOID }
| "char" { CHAR }
| "short" { SHORT }
| "int" { INT }
| "long" { LONG }
| "float" { FLOAT }
| "double" { DOUBLE }
| "signed" { SIGNED }
| "unsigned" { UNSIGNED }
| "const" { CONST }
| "volatile" { VOLATILE }
| "struct" { STRUCT }
| "union" { UNION }
| "enum" { ENUM }
| "case" { CASE }
| "default" { DEFAULT }
| "if" { IF }
| "else" { ELSE }
| "switch" { SWITCH }
| "while" { WHILE }
| "do" { DO }
| "for" { FOR }
| "goto" { GOTO }
| "continue" { CONTINUE }
| "break" { BREAK }
| "return" { RETURN }
| '{' { LBRACKET }
| '}' { RBRACKET }
| '[' { LBRACKET_SQUARE }
| ']' { RBRACKET_SQUARE }
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }
| '=' { ASSIGN }
| ';' { SEMICOLON }
| '?' { QUESTION }
| ':' { COLON }
| '*' { ASTERISK }
| "..." { ELLIPSIS }
| eof { EOF }
