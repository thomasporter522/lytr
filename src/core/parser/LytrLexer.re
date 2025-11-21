open LytrGrammar;
open Buffer;

// hash instead, later
type token_id =
  | Char(int)
  | Merge(token_id, token_id);

type token = {
  text: tok,
  id: token_id,
};

/* Helper function to check if a character is whitespace */
let is_whitespace = (c: character): bool =>
  switch (c.text) {
  | ' '
  | '\t'
  | '\n'
  | '\r' => true
  | _ => false
  };

/* Helper function to check if a character is a digit */
let is_digit = (c: character): bool => c.text >= '0' && c.text <= '9';

/* Helper function to check if a character is a letter */
let is_letter = (c: character): bool =>
  c.text >= 'a' && c.text <= 'z' || c.text >= 'A' && c.text <= 'Z';

/* Helper function to check if a character is alphanumeric */
let is_alphanum = (c: character): bool =>
  is_letter(c) || is_digit(c) || c.text == '_';

/* Helper function to collect a numeric sequence */
let rec collect_number =
        (acc: list(character), id_acc: token_id, chars: list(character))
        : (list(character), token_id, list(character)) =>
  switch (chars) {
  | [] => (acc, id_acc, [])
  | [c, ...rest] when is_digit(c) =>
    collect_number([c, ...acc], Merge(id_acc, Char(c.id)), rest)
  | _ => (acc, id_acc, chars)
  };

/* Helper function to collect a word (alphanumeric sequence) */
let rec collect_word =
        (acc: list(character), id_acc: token_id, chars: list(character))
        : (list(character), token_id, list(character)) =>
  switch (chars) {
  | [] => (acc, id_acc, [])
  | [c, ...rest] when is_alphanum(c) =>
    collect_word([c, ...acc], Merge(id_acc, Char(c.id)), rest)
  | _ => (acc, id_acc, chars)
  };

/* Tokenize a single character or sequence */
let rec lex_chars = (chars: list(character)): list(token) =>
  switch (chars) {
  | [] => []
  | [{text: '(', id}, ...rest] => [
      {
        text: Primary(TOP),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: ')', id}, ...rest] => [
      {
        text: Primary(TCP),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '[', id}, ...rest] => [
      {
        text: Primary(TOSB),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: ']', id}, ...rest] => [
      {
        text: Primary(TCSB),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: ',', id}, ...rest] => [
      {
        text: Primary(TComma),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '+', id}, ...rest] => [
      {
        text: Primary(TPlus),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '-', id}, {text: '>', id: id2}, ...rest] => [
      {
        text: Primary(TArrow),
        id: Merge(Char(id), Char(id2)),
      },
      ...lex_chars(rest),
    ]
  | [{text: '-', id}, ...rest] => [
      {
        text: Primary(TMinus),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '*', id}, ...rest] => [
      {
        text: Primary(TTimes),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '/', id}, {text: '/', id: id2}, ...rest] => [
      {
        text: Primary(TDoubleDivide),
        id: Merge(Char(id), Char(id2)),
      },
      ...lex_chars(rest),
    ]
  | [{text: '/', id}, ...rest] => [
      {
        text: Primary(TDivide),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '%', id}, ...rest] => [
      {
        text: Primary(TModulo),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '!', id}, ...rest] => [
      {
        text: Primary(TFactorial),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '=', id}, {text: '>', id: id2}, ...rest] => [
      {
        text: Primary(TDoubleArrow),
        id: Merge(Char(id), Char(id2)),
      },
      ...lex_chars(rest),
    ]
  | [{text: '=', id}, ...rest] => [
      {
        text: Primary(TEquals),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: '|', id}, ...rest] => [
      {
        text: Primary(TPipe),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [{text: ':', id}, ...rest] => [
      {
        text: Primary(TColon),
        id: Char(id),
      },
      ...lex_chars(rest),
    ]
  | [c, ...rest] when is_whitespace(c) => [
      {
        text: Secondary(Whitespace([c])),
        id: Char(c.id),
      },
      ...lex_chars(rest),
    ]
  | [c, ...rest] when is_digit(c) =>
    /* Collect numeric sequence */
    let (num, id, remaining) = collect_number([c], Char(c.id), rest);
    let num = List.rev(num);
    [
      {
        text: Primary(TAtom(Numlit(num))),
        id,
      },
      ...lex_chars(remaining),
    ];
  | [c, ...rest] when is_letter(c) =>
    /* Collect identifier sequence */
    let (word, id, remaining) = collect_word([c], Char(c.id), rest);
    let word_str =
      String.of_seq(
        List.to_seq(List.map((c: character) => c.text, List.rev(word))),
      );
    /* Check if it's a keyword */
    let token =
      switch (word_str) {
      | "fun" => TFun
      | "let" => TLet
      | "in" => TIn
      | "type" => TType
      | "case" => TCase
      | "end" => TEnd
      | "if" => TIf
      | "then" => TThen
      | "else" => TElse
      | _ => TAtom(Identifier(List.rev(word)))
      };
    [
      {
        text: Primary(token),
        id,
      },
      ...lex_chars(remaining),
    ];
  | [c, ...rest] =>
    /* Unknown character - treat as unlexed atom */
    [
      {
        text: Secondary(Unlexed([c])),
        id: Char(c.id),
      },
      ...lex_chars(rest),
    ]
  };

/* Main lexer function */
let lex = (s: list(character)): list(token) => {
  lex_chars(s);
};
