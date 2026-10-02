{-# LANGUAGE OverloadedStrings #-}
module Tool.Type
  ( run
  )
  where


import qualified AST.Prim.Module as Module
import qualified AST.Prim.Name as N
import qualified Json.Encode as E
import Json.Encode ((==>))
import Tool.Output (Output(..))
import Tool.Problem (Problem(..))
import qualified Tool.Project as Project
import Tool.Summary (Summary(..), Entry(..))
import qualified Tool.Summary as S



-- RUN
--
--   elm tool type Some.Module.fn   the type of one definition
--   elm tool type Some.Module      every unannotated definition (--all: every one)


run :: Bool -> String -> IO (Either Problem Output)
run everything target =
  case (Project.parseValueName target, Project.parseModuleName target) of
    (Just (home, name), _) ->
      Project.withModule False home $ \summary ->
        let values = filter isValue (_entries summary) in
        return $
          case filter ((== N.toChars name) . _name) values of
            e : _ ->
              Right (Output (_line e ++ "\n") (toJson e))

            [] ->
              Left $ NotFound ("a value in " ++ Module.toChars home) (N.toChars name) $
                S.suggest (N.toChars name) (map _name values)

    (Nothing, Just home) ->
      Project.withModule False home $ \summary ->
        let
          entries =
            [ e | e <- _entries summary, isValue e, everything || not (_annotated e) ]
        in
        return $ Right $
          Output (concatMap (\e -> _line e ++ "\n") entries) (E.list toJson entries)

    (Nothing, Nothing) ->
      return (Left (BadName "a module like `Page.Home` or a value like `Page.Home.view`" target))


isValue :: Entry -> Bool
isValue e =
  case _kind e of
    S.Value -> True
    S.Binop -> True
    _       -> False


toJson :: Entry -> E.Value
toJson e =
  E.object
    [ "name" ==> E.chars (_name e)
    , "signature" ==> E.chars (_line e)
    , "annotated" ==> E.bool (_annotated e)
    ]
