{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  )
  where


import qualified Data.ByteString.Builder as B

import Literals (b)

import qualified AST.Prim.Module as Module



-- SANDWICH


sandwich :: Module.Name -> Maybe B.Builder -> B.Builder -> B.Builder
sandwich moduleName maybeCss javascript =
  let
    name = Module.toBuilder moduleName

    css =
      case maybeCss of
        Nothing -> mempty
        Just stylesheet -> [b|
  <style>
|] <> stylesheet <> [b|</style>|]
  in
  [b|<!DOCTYPE HTML>
<html>
<head>
  <meta charset="UTF-8">
  <title>|] <> name <> [b|</title>
  <style>body { padding: 0; margin: 0; }</style>|] <> css <> [b|
</head>

<body>

<pre id="elm"></pre>

<script>
try {
|] <> javascript <> [b|

  var app = Elm.|] <> name <> [b|.init({ node: document.getElementById("elm") });
}
catch (e)
{
  // display initialization errors (e.g. bad flags, infinite recursion)
  var header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  var pre = document.getElementById("elm");
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
</script>

</body>
</html>|]
