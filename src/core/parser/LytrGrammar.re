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
  | TEnd
  | TIf
  | TThen
  | TElse
  | TColon;

type token_entry = token;

type is_valid_start =
  | NotValidStart
  | ValidStart;

type is_valid_end =
  | ValidEnd
  | NotValidEnd;

let is_valid_start_end = (t: token): (is_valid_start, is_valid_end) => {
  switch (t) {
  | BOF => (ValidStart, NotValidEnd)
  | EOF => (NotValidStart, ValidEnd)
  | TOP => (ValidStart, NotValidEnd)
  | TCP => (NotValidStart, ValidEnd)
  | TAtom(_) => (ValidStart, ValidEnd)
  | TPlus => (ValidStart, ValidEnd)
  | TMinus => (ValidStart, ValidEnd)
  | TTimes => (ValidStart, ValidEnd)
  | TDivide => (ValidStart, ValidEnd)
  | TDoubleDivide => (ValidStart, ValidEnd)
  | TModulo => (ValidStart, ValidEnd)
  | TFun => (ValidStart, NotValidEnd)
  | TArrow => (ValidStart, ValidEnd)
  | TLet => (ValidStart, NotValidEnd)
  | TEquals => (NotValidStart, NotValidEnd)
  | TIn => (NotValidStart, ValidEnd)
  | TType => (ValidStart, NotValidEnd)
  | TCase => (ValidStart, NotValidEnd)
  | TPipe => (NotValidStart, NotValidEnd)
  | TDoubleArrow => (NotValidStart, NotValidEnd)
  | TEnd => (NotValidStart, ValidEnd)
  | TIf => (ValidStart, NotValidEnd)
  | TThen => (NotValidStart, NotValidEnd)
  | TElse => (NotValidStart, ValidEnd)
  | TColon => (ValidStart, ValidEnd)
  };
};

let is_valid_start = (t: token): is_valid_start =>
  fst(is_valid_start_end(t));

let is_valid_end = (te: token_entry): is_valid_end =>
  snd(is_valid_start_end(te));

type match_token_result =
  | Match
  | NoMatch;

let match_token = (te1: token_entry, te2: token_entry): match_token_result =>
  switch (te1, te2) {
  | (BOF, EOF) => Match
  | (TOP, TCP) => Match
  | (TFun, TArrow) => Match
  | (TLet, TEquals) => Match
  | (TEquals, TIn) => Match
  | (TType, TEquals) => Match
  | (TCase, TEnd) => Match
  | (TCase, TPipe) => Match
  | (TPipe, TDoubleArrow) => Match
  | (TDoubleArrow, TPipe) => Match
  | (TDoubleArrow, TEnd) => Match
  | (TIf, TThen) => Match
  | (TThen, TElse) => Match
  | (_, _) => NoMatch /* fallthrough */
  };

type prec =
  | Interior // never relevent, always matches over
  | Uninterested // can't have a child
  | Precedence(float);

let prec = (t: token): (prec, prec) =>
  switch (t) {
  | BOF => (Uninterested, Interior)
  | EOF => (Interior, Uninterested)
  | TOP => (Precedence(3.), Interior)
  | TCP => (Interior, Uninterested)
  | TAtom(_) => (Uninterested, Uninterested)
  | TPlus => (Precedence(0.9), Precedence(1.1))
  | TMinus => (Precedence(0.9), Precedence(1.1))
  | TTimes => (Precedence(2.1), Precedence(1.9))
  | TDivide => (Precedence(2.1), Precedence(1.9))
  | TDoubleDivide => (Precedence(2.1), Precedence(1.9))
  | TModulo => (Precedence(2.1), Precedence(1.9))
  | TFun => (Uninterested, Interior)
  | TArrow => (Precedence(0.1), Precedence(0.))
  | TLet => (Uninterested, Interior)
  | TEquals => (Interior, Interior)
  | TIn => (Interior, Precedence(0.))
  | TType => (Uninterested, Interior)
  | TCase => (Uninterested, Interior)
  | TPipe => (Interior, Interior)
  | TDoubleArrow => (Interior, Interior)
  | TEnd => (Interior, Uninterested)
  | TIf => (Uninterested, Interior)
  | TThen => (Interior, Interior)
  | TElse => (Interior, Precedence(0.))
  | TColon => (Precedence(0.5), Precedence(0.5))
  };
