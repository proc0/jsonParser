```haskell
{- JSON 

json
    element

value
    object
    array
    string
    number
    "true"
    "false"
    "null"

object
    '{' ws '}'
    '{' members '}'
members
    member
    member ',' members
member
    ws string ws ':' element

array
    '[' ws ']'
    '[' elements ']'
elements
    element
    element ',' elements 

element
    ws value ws 
number
    integer fraction exponent 
nothing
    ""
    indentation '"' '"' newline 
ws
    ""
    '0020' ws
    '000A' ws
    '000D' ws
    '0009' ws 

string
    '"' characters '"' 
characters
    ""
    character characters 
integer 
    digit 
    onenine digits
    '-' digit
    '-' onenine digits 
digits
    digit
    digit digits
digit
    '0'
    onenine
onenine
    '1' . '9'
exponent
    ""
    'E' sign digits
    'e' sign digits
sign
    ""
    '+' 

character
    '0020' . '10FFFF' - '"' - '\'
    '\' escape 
escape
    '"'
    /^[\\\/bfnrt']+/
    'u' hex hex hex hex
hex
    digit
    'A' . 'F'
    'a' . 'f' 
-}
```