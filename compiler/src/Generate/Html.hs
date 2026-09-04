{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Generate.Html
  ( sandwich
  , sandwichModule
  )
  where


import qualified Data.ByteString.Builder as B
import qualified Data.Name as Name

import Literals (b)



-- SANDWICH


-- The same page, but loading the program as an ES module from a URL rather
-- than inlining it. A program that spawns workers has to be loaded this way,
-- since a worker is found relative to the bundle and only a module knows its
-- own URL. The reactor uses it with its `Main.elm.mjs` endpoint.
sandwichModule :: Name.Name -> Maybe B.Builder -> B.Builder -> B.Builder
sandwichModule moduleName maybeCss url =
  let
    name = Name.toBuilder moduleName

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

<script type="module">
import { Elm } from "|] <> url <> [b|";
try {
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


sandwich :: Name.Name -> Maybe B.Builder -> B.Builder -> B.Builder
sandwich moduleName maybeCss javascript =
  let
    name = Name.toBuilder moduleName

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
