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
import qualified Tool.Docs as Docs
import qualified Tool.Outline as Outline
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Problem as Problem
import qualified Tool.Refs as Refs
import qualified Tool.Type as Type



-- FLAGS


data Flags =
  Flags
    { _asJson :: Bool
    , _everything :: Bool
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
  , ("refs", "elm tool refs Some.Module.name", "Print where a value, type, or constructor is defined and every place in the project that uses it.")
  ]



-- RUN


run :: (String, [String]) -> Flags -> IO ()
run (command, arguments) (Flags json everything) =
  do  style <- if json then return Reporting.json else Reporting.terminal
      output <- Reporting.attemptWithStyle style Problem.toReport $
        case (command, arguments) of
          ("type", [target])    -> Type.run everything target
          ("docs", [target])    -> Docs.run everything target
          ("outline", [target]) -> Outline.run target
          ("refs", [target])    -> Refs.run target
          ("at", [target])      -> At.run target
          ("async", [target])   -> Async.run target
          _ ->
            return $ Left $
              case [ usage | (name, usage, _) <- commands, name == command ] of
                usage : _ -> BadArgs command usage
                []        -> UnknownCommand command

      if json
        then B.hPutBuilder IO.stdout (E.encodeUgly (_json output) <> B.char7 '\n')
        else IO.putStr (_text output)
