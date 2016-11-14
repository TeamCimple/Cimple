(*{ open Parser open Test }*)
{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| ';' { SEMICOLON }
| '+' { PLUS }
| "+=" { PLUS_ASSIGN }
| '-' { MINUS }
| "-=" { MINUS_ASSIGN }
| '*' { TIMES }
| "*=" { TIMES_ASSIGN }
| '/' { DIVIDE }
| "/=" { DIVIDE_ASSIGN }
| "%=" { MOD_ASSIGN }
| "<<=" { LSHIFT_ASSIGN }
| ">>=" { RSHIFT_ASSIGN }
| "&=" { AND_ASSIGN }
| "^=" { XOR_ASSIGN }
| "|=" { OR_ASSIGN }
| ['0'-'9']+ as lit { INT_LITERAL(int_of_string lit) }
| "extends" { EXTENDS }
| "implements" { IMPLEMENTS }
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
| '?' { QUESTION }
| ':' { COLON }
| '*' { ASTERISK }
| '.' { PERIOD }
| "..." { ELLIPSIS }
| ['a'-'z''_']+['a'-'z''A'-'Z''_''0'-'9']* as lit { IDENTIFIER(lit) }
| ['A'-'Z']+['a'-'z''A'-'Z''_''0'-'9']* as structLit {
        STRUCT_IDENTIFIER(structLit) } 
| eof { EOF }
