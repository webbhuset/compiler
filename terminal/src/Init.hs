{-# LANGUAGE OverloadedStrings #-}
module Init
  ( run
  )
  where


import Prelude hiding (init)
import qualified Data.Map as Map
import qualified Data.NonEmptyList as NE
import qualified Data.Set as Set
import qualified System.Directory as Dir

import qualified Deps.Registry as Registry
import qualified Deps.Solver as Solver
import qualified Elm.Constraint as Con
import qualified Elm.Outline as Outline
import qualified Elm.Package as Pkg
import qualified Elm.Version as V
import qualified Reporting
import qualified Reporting.Doc as D
import qualified Reporting.Exit as Exit



-- RUN


run :: () -> () -> IO ()
run () () =
  Reporting.attempt Exit.initToReport $
  do  exists <- Dir.doesFileExist "elm.json"
      if exists
        then return (Left Exit.InitAlreadyExists)
        else
          do  approved <- Reporting.ask question
              if approved
                then init
                else
                  do  putStrLn "Okay, I did not make any changes!"
                      return (Right ())


question :: D.Doc
question =
  D.stack
    [ D.fillSep
        ["Hello!"
        ,"Elm","projects","always","start","with","an",D.green "elm.json","file."
        ,"I","can","create","them!"
        ]
    , D.reflow
        "Now you may be wondering, what will be in this file? How do I add Elm files to\
        \ my project? How do I see it in the browser? How will my code grow? Do I need\
        \ more directories? What about tests? Etc."
    , D.fillSep
        ["Check","out",D.cyan (D.fromChars (D.makeLink "init"))
        ,"for","all","the","answers!"
        ]
    , "Knowing all that, would you like me to create an elm.json file now? [Y/n]: "
    ]



-- INIT


init :: IO (Either Exit.Init ())
init =
  do  eitherEnv <- Solver.initEnv
      case eitherEnv of
        Left problem ->
          return (Left (Exit.InitRegistryProblem problem))

        Right (Solver.Env cache _ connection registry gitUrls) ->
          do  let registry' = foldr addForkVersion registry (Map.toList forkDefaults)
              let gitUrls' = Map.union (Map.map snd forkDefaults) gitUrls
              result <- Solver.verify cache connection registry' gitUrls' defaults
              case result of
                Solver.Err exit ->
                  return (Left (Exit.InitSolverProblem exit))

                Solver.NoSolution ->
                  return (Left (Exit.InitNoSolution (Map.keys defaults)))

                Solver.NoOfflineSolution ->
                  return (Left (Exit.InitNoOfflineSolution (Map.keys defaults)))

                Solver.Ok details ->
                  let
                    solution = Map.map (\(Solver.Details vsn _) -> vsn) details
                    directs = Map.restrictKeys solution directDefaults
                    indirects = Map.withoutKeys solution directDefaults
                    usedGitUrls = Map.map snd (Map.intersection forkDefaults solution)
                  in
                  do  Dir.createDirectoryIfMissing True "src"
                      Outline.write "." $ Outline.App $
                        Outline.AppOutline V.compiler (NE.List (Outline.RelativeSrcDir "src") []) directs indirects Map.empty Map.empty usedGitUrls
                      putStrLn "Okay, I created it. Now read that link!"
                      return (Right ())


-- The packages every new project starts with. The patched forks (see
-- CHANGELOG.md) are pinned to their fork versions and fetched as git
-- dependencies; everything else comes from the registry as usual.
--
-- Fork versions are numbered so they can never collide with a version
-- the upstream author publishes: minor = 100 + upstream minor, patch =
-- 100 * upstream patch + fork revision. See docs/git-dependencies.md.
--
-- The URLs are the canonical SSH form. Since the package cache keys on
-- (name, version) and records the URL it cloned from, a project reaching
-- the same fork version through a different URL -- the https form of the
-- same repository included -- is rejected as a cache conflict.


directDefaults :: Set.Set Pkg.Name
directDefaults =
  Set.fromList [ Pkg.core, Pkg.browser, Pkg.html ]


defaults :: Map.Map Pkg.Name Con.Constraint
defaults =
  Map.union
    (Map.map (Con.exactly . fst) forkDefaults)
    (Map.fromList
      [ (Pkg.core, Con.anything)
      , (Pkg.browser, Con.anything)
      , (Pkg.html, Con.anything)
      ])


forkDefaults :: Map.Map Pkg.Name (V.Version, String)
forkDefaults =
  Map.fromList
    [ ( Pkg.core
      , ( V.Version 1 100 503
        , "git@github.com:webbhuset/core.git"
        )
      )
    , ( Pkg.browser
      , ( V.Version 1 100 201
        , "git@github.com:webbhuset/elm-browser.git"
        )
      )
    , ( Pkg.virtualDom
      , ( V.Version 1 100 501
        , "git@github.com:webbhuset/virtual-dom.git"
        )
      )
    ]


addForkVersion :: (Pkg.Name, (V.Version, String)) -> Registry.Registry -> Registry.Registry
addForkVersion (pkg, (vsn, _)) (Registry.Registry count versions) =
  Registry.Registry count (Map.insert pkg (Registry.KnownVersions vsn []) versions)
