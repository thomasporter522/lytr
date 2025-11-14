type atom =
  | Numlit(int)
  | Identifier(string);

type primary_token =
  | BOF // beginning of file
  | EOF // end of file
  | TOP // open parens
  | TCP // close parens
  | TOSB // open square bracket
  | TCSB // close square bracket
  | TAtom(atom)
  | TPlus
  | TMinus
  | TTimes
  | TDivide
  | TDoubleDivide
  | TModulo
  | TFactorial
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
  | TColon
  | TComma
  | TCommaTuple
  | TCommaList;

type secondary_token =
  | Whitespace(string)
  | Unlexed(string);

type token =
  | Primary(primary_token)
  | Secondary(secondary_token);

type prec =
  | Interior // never relevent, always matches over
  | Uninterested // can't have a child
  | Precedence(float);

let prec = (t: primary_token): (prec, prec) =>
  switch (t) {
  | BOF => (Uninterested, Interior)
  | EOF => (Interior, Uninterested)
  | TOP => (Precedence(3.), Interior)
  | TCP => (Interior, Uninterested)
  | TOSB => (Uninterested, Interior)
  | TCSB => (Interior, Uninterested)
  | TAtom(_) => (Uninterested, Uninterested)
  | TPlus => (Precedence(0.9), Precedence(1.1))
  | TMinus => (Precedence(0.9), Precedence(1.1))
  | TTimes => (Precedence(2.1), Precedence(1.9))
  | TDivide => (Precedence(2.1), Precedence(1.9))
  | TDoubleDivide => (Precedence(2.1), Precedence(1.9))
  | TModulo => (Precedence(2.1), Precedence(1.9))
  | TFactorial => (Precedence(3.), Uninterested)
  | TFun => (Uninterested, Interior)
  | TArrow => (Precedence(0.7), Precedence(0.6))
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
  | TComma => (Interior, Interior)
  | TCommaTuple => (Interior, Interior)
  | TCommaList => (Interior, Interior)
  };

type match_token_result =
  | Match
  | MatchMorph(primary_token)
  | NoMatch;

let match_token = (te1: primary_token, te2: primary_token): match_token_result =>
  switch (te1, te2) {
  | (BOF, EOF) => Match
  | (TOP, TCP) => Match
  | (TOSB, TCSB) => Match
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
  | (TOP, TComma) => MatchMorph(TCommaTuple)
  | (TCommaTuple, TComma) => MatchMorph(TCommaTuple)
  | (TCommaTuple, TCP) => Match
  | (TOSB, TComma) => MatchMorph(TCommaList)
  | (TCommaList, TComma) => MatchMorph(TCommaList)
  | (TCommaList, TCSB) => Match
  | (_, _) => NoMatch /* fallthrough */
  };

let is_valid_start = (t: primary_token): bool => fst(prec(t)) != Interior;

let is_valid_end = (te: primary_token): bool => snd(prec(te)) != Interior;
