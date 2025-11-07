open LytrToken;

type token_entry = token;

type init_token_result =
  | NoInit
  | Init;

let non_initial_tokens = [EOF, TCP, TEquals, TIn, TPipe, TDoubleArrow, TEnd];

let init_token = (t: token): init_token_result =>
  List.mem(t, non_initial_tokens) ? NoInit : Init;

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
  | (TCase, TEnd) => Match
  | (TCase, TPipe) => Match
  | (TPipe, TDoubleArrow) => Match
  | (TDoubleArrow, TPipe) => Match
  | (TDoubleArrow, TEnd) => Match
  | (_, _) => NoMatch /* fallthrough */
  };

type close_token_result =
  | Closed
  | Unclosed;

let unclosed_tokens = [BOF, TOP, TLet, TEquals, TType, TCase, TPipe, TDoubleArrow];

let close_token = (te: token_entry): close_token_result =>
    List.mem(te, unclosed_tokens) ? Unclosed : Closed;

type compare_tokens_result =
  | Shift // first thing wants the second as a child
  | Reduce // second thing wants the first as a child
  | Roll; // neither can be the other's child

type prec = 
    | Interior // never relevent, always matches over
    | Uninterested // can't have a child
    | Precedence(int); 

let left_prec (t : token) : prec =
  switch (t) {
  | BOF => Interior
  | EOF => Uninterested
  | TOP => Interior
  | TCP => Uninterested
  | TAtom(_) => Uninterested
  | TPlus => Precedence(1)
  | TMinus => Precedence(1)
  | TTimes => Precedence(2)
  | TDivide => Precedence(2)
  | TDoubleDivide => Precedence(2)
  | TModulo => Precedence(2)
  | TFun => Interior
  | TArrow => Interior
  | TLet => Interior
  | TEquals => Interior
  | TIn => Precedence(0)
  | TType => Interior
  | TCase => Interior
  | TPipe => Interior
  | TDoubleArrow => Interior
  | TEnd => Uninterested
  };

/* Precondition: t1 ends a form and t2 starts a form */
let compare_tokens = (t1: token, t2: token): compare_tokens_result =>
  switch (t1, t2) {
  | (TCP, TOP) => Roll
  | (TCP, TTimes) => Reduce
  | (TCP, TMinus) => Reduce
  | (TCP, TAtom(_)) => Roll
  | (TTimes, TOP) => Shift
  | (TTimes, TTimes) => Shift /* right assoc */
  | (TTimes, TMinus) => Reduce /* * binds tighter */
  | (TTimes, TAtom(_)) => Shift
  | (TMinus, TOP) => Shift
  | (TMinus, TTimes) => Shift
  | (TMinus, TMinus) => Reduce /* left assoc */
  | (TMinus, TAtom(_)) => Shift
  | (TAtom(_), TOP) => Roll
  | (TAtom(_), TTimes) => Reduce
  | (TAtom(_), TMinus) => Reduce
  | (TAtom(_), TAtom(_)) => Roll
  | (_, _) => failwith("impossible: precondition violated")
  };

type wants_left_child_result =
  | Yes
  | No;

/* need only consider when t starts a form */
let wants_left_child = (t: token): wants_left_child_result =>
  switch (t) {
  | TOP => No
  | TTimes => Yes
  | TMinus => Yes
  | TAtom(_) => No
  | _ => No /* impossible fallthrough */
  };
