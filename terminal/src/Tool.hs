module Tool
  ( Flags(..)
  , run
  , commands
  )
  where


import qualified Data.ByteString.Builder as B
import qualified System.IO as IO

import qualified Json.Encode as E
import qualified Reporting
import qualified Tool.Async as Async
import qualified Tool.At as At
import qualified Tool.Cases as Cases
import qualified Tool.Check as Check
import qualified Tool.Decoder as Decoder
import qualified Tool.Docs as Docs
import qualified Tool.Graph as Graph
import qualified Tool.Outline as Outline
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Problem as Problem
import qualified Tool.Refs as Refs
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
    }



-- COMMANDS


-- (name, usage, what it does)
commands :: [(String, String, String)]
commands =
  [ ("type", "elm tool type Some.Module.value", "Print the inferred type of a definition, ready to paste into the source. Given a module, print the types of its unannotated definitions, or of all of them with --all.")
  , ("docs", "elm tool docs Some.Module", "Print the module comment and exposed API of a module as Markdown. Add --all to include private declarations.")
  , ("outline", "elm tool outline Some.Module", "Print one line per declaration of a module, with the lines it covers.")
  , ("at", "elm tool at src/Some/File.elm:LINE:COLUMN", "Print the type of the expression at a position, and the type of every local name in scope there.")
  , ("async", "elm tool async src/Main.elm", "Print how many bytes each module adds to an --optimize build, whether the program always needs it or only under some branch, and which modules look worth an `import async`.")
  , ("check", "elm tool check [src/Some/File.elm ...]", "Type check the given files, or every module in the source directories, without generating code.")
  , ("graph", "elm tool graph [Some.Module]", "Print every project module with what it imports, or what one module imports and what imports it.")
  , ("why", "elm tool why Some.Module", "Print the shortest chain of imports from each module with a `main` to a module, which may come from a package.")
  , ("unused", "elm tool unused", "Print the values, constructors, types, imports, and modules nothing in the project uses.")
  , ("cases", "elm tool cases Some.Module.Type", "Print every `case` on a custom type, with the constructors a wildcard branch covers without naming them, and every place the type is built.")
  , ("sizes", "elm tool sizes src/Main.elm", "Print the bytes each module and definition adds to an --optimize build. Save the --json output of two builds and compare them with `elm tool sizes --diff before.json after.json`.")
  , ("decoder", "elm tool decoder Some.Module.decoder", "Print the shape of the JSON a Json.Decode decoder accepts, or with --sample a document it accepts.")
  , ("refs", "elm tool refs Some.Module.name", "Print where a value, type, or constructor is defined and every place in the project that uses it.")
  ]



-- RUN


run :: (String, [String]) -> Flags -> IO ()
run (command, arguments) (Flags json everything compareFiles sample) =
  do  style <- if json then return Reporting.json else Reporting.terminal
      output <- Reporting.attemptWithStyle style Problem.toReport $
        case (command, arguments) of
          ("type", [target])    -> Type.run everything target
          ("docs", [target])    -> Docs.run everything target
          ("outline", [target]) -> Outline.run target
          ("refs", [target])    -> Refs.run target
          ("at", [target])      -> At.run target
          ("async", [target])   -> Async.run target
          ("check", files)      -> Check.run files
          ("graph", targets)    -> Graph.graph targets
          ("why", [target])     -> Graph.why target
          ("unused", [])        -> Unused.run
          ("cases", [target])   -> Cases.run target
          ("decoder", [target]) -> Decoder.run sample target
          ("sizes", [a, b]) | compareFiles -> Sizes.diff a b
          ("sizes", [target]) | not compareFiles -> Sizes.run target
          _ ->
            return $ Left $
              case [ usage | (name, usage, _) <- commands, name == command ] of
                usage : _ -> BadArgs command usage
                []        -> UnknownCommand command

      if json
        then B.hPutBuilder IO.stdout (E.encodeUgly (_json output) <> B.char7 '\n')
        else IO.putStr (_text output)
