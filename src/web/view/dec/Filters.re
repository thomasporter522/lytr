open Virtual_dom.Vdom;

let all =
  Util.Nodes.svg(
    ~attrs=[Attr.id("filters")],
    [
      Silhouette.outer_blur,
      Silhouette.inner_blur,
      ...List.map(Token.drop_shadow, Tylr_core.Sort.all),
    ],
  );
