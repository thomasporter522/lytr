open Js_of_ocaml;

let get = (k: string): option(string) =>
  try({
    let local_store =
      Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
    local_store##getItem(Js.string(k))
    |> (
      x => Js.Opt.get(x, () => assert(false)) |> Js.to_string |> Option.some
    );
  }) {
  | _ => None
  };

let set = (k: string, v: string): unit => {
  let local_store =
    Js.Optdef.get(Dom_html.window##.localStorage, () => assert(false));
  local_store##setItem(Js.string(k), Js.string(v));
};
let get_from_clipboard = (): string => {
  /* WIP(andrew):
       This sorta works, somewhat hackily and inconsistently (requires a dom element
       called blorg to be present and ideally hidden). However it prompts the user
       for permissions each time.
     */
  let _ =
    Js.Unsafe.js_expr(
      "window.navigator.clipboard.readText().then(
      function(text)
      {var guy = document.getElementById('blorg'); guy.innerHTML = text; console.log('Clipboard content is: ', text)}).catch
      (function(err)
        {console.error('Failed to read clipboard contents: ', err)})",
    );
  let doc = Dom_html.document;
  let elem =
    Js.Opt.get(doc##getElementById(Js.string("blorg")), () => {
      assert(false)
    });
  let result: Js.t('a) = Js.Unsafe.get(elem, "innerHTML");
  let result = Js.to_string(result);
  print_endline(result);
  result;
};
