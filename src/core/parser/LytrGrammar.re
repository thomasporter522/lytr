open Buffer;

type atom =
  | Numlit(list(character))
  | Identifier(list(character));

type primary_tok =
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

type secondary_tok =
  | Whitespace(list(character))
  | Unlexed(list(character));

type tok =
  | Primary(primary_tok)
  | Secondary(secondary_tok);

type prec =
  | Interior // never relevent, always matches over
  | Uninterested // can't have a child
  | Precedence(float);

let prec = (t: primary_tok): (prec, prec) =>
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

// Match morph: a mechanism to prevent things like [0,1,2) from being parsed.
// The basic format here is to specify which toks match which.
// But [ matches , and , matched ), so how do we prevent the aforementioned nonsense?
// When [ matches , then that comma gets morphed into a "list comma", which looks the
// same but allows it to distinguish itself from the tuple comma and refuse to match ).
// This solution is a little strange but it's extremely lightweight and allows the
// normal case to remain unchanged (you can just use the Match constructor below most
// of the time) so I think it's good.

type match_tok_result =
  | Match
  | MatchMorph(primary_tok)
  | NoMatch;

let match_tok = (te1: primary_tok, te2: primary_tok): match_tok_result =>
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

let is_valid_start = (t: primary_tok): bool => fst(prec(t)) != Interior;

let is_valid_end = (te: primary_tok): bool => snd(prec(te)) != Interior;
