(*{ open Parser open Test }*)
{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| ';' { SEMICOLON }
| "++" { PLUSPLUS }
| '+' { PLUS }
| "+=" { PLUS_ASSIGN }
| "--" { MINUSMINUS }
| '-' { MINUS }
| "-=" { MINUS_ASSIGN }
| '*' { TIMES }
| "*=" { TIMES_ASSIGN }
| '/' { DIVIDE }
| "/=" { DIVIDE_ASSIGN }
| '%' { MOD }
| "%=" { MOD_ASSIGN }
| "<<" { LSHIFT }
| "<<=" { LSHIFT_ASSIGN }
| ">>" { RSHIFT }
| ">>=" { RSHIFT_ASSIGN }
| "&&" { AND }
| '&' { BITWISE_AND }
| "&=" { AND_ASSIGN }
| '^' { XOR }
| "^=" { XOR_ASSIGN }
| "||" { OR }
| '|' { BITWISE_OR }
| "|=" { OR_ASSIGN }
| '~' { NOT }
| "!=" { NOT_EQUALS }
| "==" { EQUALS }
| '<' { LESS_THAN }
| "<=" { LESS_THAN_EQUALS }
| ">" { GREATER_THAN }
| ">=" { GREATER_THAN_EQUALS }
| ['0'-'9']+ as lit { INT_LITERAL(int_of_string lit) }
| ['0'-'9']+'.'+['0'-'9']* as lit { FLOAT_LITERAL(float_of_string lit) }
| '"' + [' ''0'-'9''a'-'z''A'-'Z''\\']* + '"' as lit { STRING_LITERAL(lit) }
| "extends" { EXTENDS }
| "make" { MAKE }
| "implements" { IMPLEMENTS }
| "interface" { INTERFACE }
| "auto" { AUTO }
| "register" { REGISTER }
| "static" { STATIC }
| "extern" { EXTERN }
| "typedef" { TYPEDEF }
| "void" { VOID }
| "char" { CHAR }
| "short" { SHORT }
| "int" { INT }
| "string" { STRING }
| "long" { LONG }
| "float" { FLOAT }
| "double" { DOUBLE }
| "signed" { SIGNED }
| "unsigned" { UNSIGNED }
| "func" { FUNC }
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
| '.' { PERIOD }
| ',' { COMMA }
| '=' { ASSIGN }
| '?' { QUESTION }
| ':' { COLON }
| '*' { ASTERISK }
| "..." { ELLIPSIS }
| ['a'-'z''_']+['a'-'z''A'-'Z''_''0'-'9']* as lit { IDENTIFIER(lit) }
| ['A'-'Z']+['a'-'z''A'-'Z''_''0'-'9']* as structLit {
        STRUCT_IDENTIFIER(structLit) } 
| eof { EOF }
