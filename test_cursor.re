open Tylr_core;

let test_text = "a + b * c";
let parsed = LytrParser.parse(LytrLexer.lex(test_text));

let () = {
  Printf.printf("Testing cursor position mapping for: '%s'\n", test_text);
  Printf.printf("Parsed: %s\n\n", LytrParser.string_of_terms(parsed));
  
  for (i in 0 to String.length(test_text)) {
    let debug_str = LytrTerms.debug_cursor_position(i, test_text, parsed);
    Printf.printf("Index %d: %s\n", i, debug_str);
  };
};
