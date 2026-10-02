{-# LANGUAGE OverloadedStrings #-}
module Tool
  ( Flags(..)
  , run
  , commands
  )
  where


import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Builder as B
import qualified System.IO as IO

import qualified Data.Utf8 as Utf8
import qualified Json.Decode as D
import qualified Json.Encode as E
import Json.Encode ((==>))
import qualified Reporting.Exit.Help as Help
import qualified Reporting
import qualified Tool.Async as Async
import qualified Tool.At as At
import qualified Tool.Cases as Cases
import qualified Tool.Check as Check
import qualified Tool.Decoder as Decoder
import qualified Tool.Docs as Docs
import qualified Tool.Graph as Graph
import qualified Tool.Hole as Hole
import qualified Tool.Move as Move
import qualified Tool.Outline as Outline
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Problem as Problem
import qualified Tool.Refs as Refs
import qualified Tool.Rename as Rename
import qualified Tool.Sizes as Sizes
import qualified Tool.Type as Type
import qualified Tool.Unused as Unused



-- FLAGS


data Flags =
  Flags
    { _asJson :: Bool
    , _everything :: Bool
    , _diff :: Bool
    , _sample :: Bool
    , _dryRun :: Bool
    }



-- COMMANDS


-- (name, usage, what it does)
commands :: [(String, String, String)]
commands =
  [ ("type", "elm tool type Some.Module.value", "Print the inferred type of a definition, ready to paste into the source. Given a module, print the types of its unannotated definitions, or of all of them with --all.")
  , ("docs", "elm tool docs Some.Module", "Print the module comment and exposed API of a module as Markdown. Add --all to include private declarations.")
  , ("outline", "elm tool outline Some.Module", "Print one line per declaration of a module, with the lines it covers.")
  , ("at", "elm tool at src/Some/File.elm:LINE:COLUMN", "Print the type of the expression at a position, and the type of every local name in scope there.")
  , ("hole", "elm tool hole src/Some/File.elm:LINE:COLUMN", "Print the type a place needs, say a `Debug.todo`, and the names in scope that fit there, as they are or with more arguments.")
  , ("async", "elm tool async src/Main.elm", "Print how many bytes each module adds to an --optimize build, whether the program always needs it or only under some branch, and which modules look worth an `import async`.")
  , ("check", "elm tool check [src/Some/File.elm ...]", "Type check the given files, or every module in the source directories, without generating code.")
  , ("graph", "elm tool graph [Some.Module]", "Print every project module with what it imports, or what one module imports and what imports it.")
  , ("why", "elm tool why Some.Module", "Print the shortest chain of imports from each module with a `main` to a module, which may come from a package.")
  , ("unused", "elm tool unused", "Print the values, constructors, types, imports, and modules nothing in the project uses.")
  , ("cases", "elm tool cases Some.Module.Type", "Print every `case` on a custom type, with the constructors a wildcard branch covers without naming them, and every place the type is built.")
  , ("sizes", "elm tool sizes src/Main.elm", "Print the bytes each module and definition adds to an --optimize build. Save the --json output of two builds and compare them with `elm tool sizes --diff before.json after.json`.")
  , ("decoder", "elm tool decoder Some.Module.decoder", "Print the shape of the JSON a Json.Decode decoder accepts, or with --sample a document it accepts.")
  , ("rename", "elm tool rename Some.Module.old new", "Rename a value, type, or constructor everywhere in the project, then check it. Add --dry-run to see the changes without making them.")
  , ("move", "elm tool move Some.Module.name Other.Module", "Move a top level definition to another module and fix the imports, then check the project. Add --dry-run to see the changes without making them.")
  , ("serve", "elm tool serve", "Answer requests until stdin closes: one JSON object per line like {\"id\": 1, \"command\": \"type\", \"args\": [\"Main.view\"]}, one JSON response per line.")
  , ("refs", "elm tool refs Some.Module.name", "Print where a value, type, or constructor is defined and every place in the project that uses it.")
  ]



-- RUN


run :: (String, [String]) -> Flags -> IO ()
run (command, arguments) flags =
  if command == "serve" && null arguments then serve else
  do  style <- if _asJson flags then return Reporting.json else Reporting.terminal
      output <- Reporting.attemptWithStyle style Problem.toReport (dispatch (command, arguments) flags)
      if _asJson flags
        then B.hPutBuilder IO.stdout (E.encodeUgly (_json output) <> B.char7 '\n')
        else IO.putStr (_text output)


dispatch :: (String, [String]) -> Flags -> IO (Either Problem Output)
dispatch (command, arguments) (Flags _ everything compareFiles sample dryRun) =
  case (command, arguments) of
    ("type", [target])    -> Type.run everything target
    ("docs", [target])    -> Docs.run everything target
    ("outline", [target]) -> Outline.run target
    ("refs", [target])    -> Refs.run target
    ("at", [target])      -> At.run target
    ("hole", [target])    -> Hole.run target
    ("async", [target])   -> Async.run target
    ("check", files)      -> Check.run files
    ("graph", targets)    -> Graph.graph targets
    ("why", [target])     -> Graph.why target
    ("unused", [])        -> Unused.run
    ("cases", [target])   -> Cases.run target
    ("decoder", [target]) -> Decoder.run sample target
    ("rename", [target, new]) -> Rename.run dryRun target new
    ("move", [target, to]) -> Move.run dryRun target to
    ("sizes", [a, b]) | compareFiles -> Sizes.diff a b
    ("sizes", [target]) | not compareFiles -> Sizes.run target
    _ ->
      return $ Left $
        case [ usage | (name, usage, _) <- commands, name == command ] of
          usage : _ -> BadArgs command usage
          []        -> UnknownCommand command



-- SERVE
--
-- One JSON request per line on stdin, one JSON response per line on stdout:
--
--   {"id": 1, "command": "type", "args": ["Page.Home.view"]}
--   {"id":1,"ok":true,"result":{"name":"view","signature":"view : Model -> Html Msg","annotated":true}}
--
-- A request can also set "all", "diff", "sample" and "dryRun". Every
-- request reads the project again, through the same caches `elm make`
-- uses, so answers follow the files as they change.


serve :: IO ()
serve =
  do  IO.hSetBuffering IO.stdout IO.LineBuffering
      loop
  where
    loop =
      do  eof <- IO.isEOF
          if eof then return () else
            do  line <- BSC.getLine
                if BS.null line then loop else
                  do  response <- answer line
                      B.hPutBuilder IO.stdout (E.encodeUgly response <> B.char7 '\n')
                      IO.hFlush IO.stdout
                      loop


data Request =
  Request
    { _id :: E.Value
    , _command :: String
    , _args :: [String]
    , _flags :: Flags
    }


answer :: BS.ByteString -> IO E.Value
answer line =
  do  decoded <- D.fromByteString request line
      case decoded of
        Left _ ->
          return $ E.object
            [ "ok" ==> E.bool False
            , "error" ==> E.chars "Expected a JSON object with a \"command\" and a list of \"args\"."
            ]

        Right (Request requestId command args flags)
          | command == "serve" ->
              return (failure requestId (E.chars "Already serving."))

          | otherwise ->
              do  result <- Exception.try (dispatch (command, args) flags)
                  return $
                    case result of
                      Right (Right output) ->
                        E.object [ "id" ==> requestId, "ok" ==> E.bool True, "result" ==> _json output ]

                      Right (Left problem) ->
                        failure requestId (Help.reportToJson (Problem.toReport problem))

                      Left exception ->
                        failure requestId (E.chars (show (exception :: Exception.SomeException)))


failure :: E.Value -> E.Value -> E.Value
failure requestId err =
  E.object [ "id" ==> requestId, "ok" ==> E.bool False, "error" ==> err ]


request :: D.Decoder () Request
request =
  Request
    <$> D.oneOf [ E.int <$> D.field "id" D.int, E.chars . Utf8.toChars <$> D.field "id" D.jsonString, pure E.null ]
    <*> D.field "command" (Utf8.toChars <$> D.jsonString)
    <*> D.oneOf [ D.field "args" (D.list (Utf8.toChars <$> D.jsonString)), pure [] ]
    <*> ( Flags True
            <$> flag "all"
            <*> flag "diff"
            <*> flag "sample"
            <*> flag "dryRun"
        )
  where
    flag name = D.oneOf [ D.field name D.bool, pure False ]
