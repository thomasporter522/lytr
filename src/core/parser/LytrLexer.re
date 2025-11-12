open LytrGrammar;

/* Helper function to check if a character is whitespace */
let is_whitespace = (c: char): bool =>
  switch (c) {
  | ' '
  | '\t'
  | '\n'
  | '\r' => true
  | _ => false
  };

/* Helper function to check if a character is a digit */
let is_digit = (c: char): bool => c >= '0' && c <= '9';

/* Helper function to check if a character is a letter */
let is_letter = (c: char): bool =>
  c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z';

/* Helper function to check if a character is alphanumeric */
let is_alphanum = (c: char): bool =>
  is_letter(c) || is_digit(c) || c == '_';

/* Tokenize a single character or sequence */
let rec lex_chars = (chars: list(char)): list(token) =>
  switch (chars) {
  | [] => []
  | ['(', ...rest] => [Primary(TOP), ...lex_chars(rest)]
  | [')', ...rest] => [Primary(TCP), ...lex_chars(rest)]
  | [',', ...rest] => [Primary(TComma), ...lex_chars(rest)]
  | ['+', ...rest] => [Primary(TPlus), ...lex_chars(rest)]
  | ['-', '>', ...rest] => [Primary(TArrow), ...lex_chars(rest)]
  | ['-', ...rest] => [Primary(TMinus), ...lex_chars(rest)]
  | ['*', ...rest] => [Primary(TTimes), ...lex_chars(rest)]
  | ['/', '/', ...rest] => [Primary(TDoubleDivide), ...lex_chars(rest)]
  | ['/', ...rest] => [Primary(TDivide), ...lex_chars(rest)]
  | ['%', ...rest] => [Primary(TModulo), ...lex_chars(rest)]
  | ['=', '>', ...rest] => [Primary(TDoubleArrow), ...lex_chars(rest)]
  | ['=', ...rest] => [Primary(TEquals), ...lex_chars(rest)]
  | ['|', ...rest] => [Primary(TPipe), ...lex_chars(rest)]
  | [':', ...rest] => [Primary(TColon), ...lex_chars(rest)]
  | [c, ...rest] when is_whitespace(c) => [
      Secondary(Whitespace(String.of_seq(List.to_seq([c])))),
      ...lex_chars(rest),
    ]
  | [c, ...rest] when is_digit(c) =>
    /* Collect numeric sequence */
    let (num, remaining) = collect_number([c], rest);
    let num = int_of_string(String.of_seq(List.to_seq(List.rev(num))));
    [Primary(TAtom(Numlit(num))), ...lex_chars(remaining)];
  | [c, ...rest] when is_letter(c) =>
    /* Collect identifier sequence */
    let (id, remaining) = collect_word([c], rest);
    let id_str = String.of_seq(List.to_seq(List.rev(id)));
    /* Check if it's a keyword */
    let token =
      switch (id_str) {
      | "fun" => TFun
      | "let" => TLet
      | "in" => TIn
      | "type" => TType
      | "case" => TCase
      | "end" => TEnd
      | "if" => TIf
      | "then" => TThen
      | "else" => TElse
      | _ => TAtom(Identifier(id_str))
      };
    [Primary(token), ...lex_chars(remaining)];
  | [c, ...rest] =>
    /* Unknown character - treat as unlexed atom */
    [Secondary(Unlexed(String.make(1, c))), ...lex_chars(rest)]
  }

/* Helper function to collect a numeric sequence */
and collect_number =
    (acc: list(char), chars: list(char)): (list(char), list(char)) =>
  switch (chars) {
  | [] => (acc, [])
  | [c, ...rest] when is_digit(c) => collect_number([c, ...acc], rest)
  | _ => (acc, chars)
  }

/* Helper function to collect a word (alphanumeric sequence) */
and collect_word =
    (acc: list(char), chars: list(char)): (list(char), list(char)) =>
  switch (chars) {
  | [] => (acc, [])
  | [c, ...rest] when is_alphanum(c) => collect_word([c, ...acc], rest)
  | _ => (acc, chars)
  };

/* Main lexer function */
let lex = (s: string): list(token) => {
  let chars = String.to_seq(s) |> List.of_seq;
  lex_chars(chars);
};
