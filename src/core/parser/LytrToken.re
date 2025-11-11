type atom =
  | Numlit(int)
  | Identifier(string)
  | Unlexed(string);

let string_of_atom = (a: atom) =>
  switch (a) {
  | Numlit(n) => string_of_int(n)
  | Identifier(s) => s
  | Unlexed(s) => s
  };

type token =
  | BOF // beginning of file
  | EOF // end of file
  | TOP // open parens
  | TCP // close parens
  | TAtom(atom)
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TDoubleDivide
  | TModulo
  | TFun
  | TArrow
  | TLet
  | TEquals
  | TIn
  | TType
  | TCase
  | TPipe
  | TDoubleArrow
  | TEnd;

let string_of_token = (t: token) =>
  switch (t) {
  | BOF => "#"
  | EOF => "#"
  | TOP => "("
  | TCP => ")"
  | TAtom(a) => string_of_atom(a)
  | TPlus => "+"
  | TMinus => "-"
  | TTimes => "*"
  | TDivide => "/"
  | TDoubleDivide => "//"
  | TModulo => "%"
  | TFun => "fun"
  | TArrow => "->"
  | TLet => "let"
  | TEquals => "="
  | TIn => "in"
  | TType => "type"
  | TCase => "case"
  | TPipe => "|"
  | TDoubleArrow => "=>"
  | TEnd => "end"
  };
