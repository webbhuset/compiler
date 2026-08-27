{-# LANGUAGE BangPatterns, ExtendedLiterals, MagicHash, UnboxedTuples #-}
module Parse.Css
  ( css
  )
  where


import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.UTF8 as BS_UTF8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Name as Name
import qualified Data.Set as Set
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Int (Int(..))
import GHC.Prim
import GHC.Word (Word64(..))

import qualified AST.Source as Src
import qualified AST.Utils.Css as Css
import Parse.Primitives (Parser, Cursor)
import qualified Parse.Primitives as P
import qualified Reporting.Annotation as A
import qualified Reporting.Error.Syntax as E



-- CSS
--
-- Parses a [css| ... |] block. The CSS is tokenized and its structure is
-- parsed enough to extract class selectors, custom properties, and
-- @keyframes names, which become the record type of the block. Property
-- values are otherwise passed through as-is.


css :: A.Position -> Parser E.Expr Src.Expr
css start@(A.Position cur) =
  do  block <- parseBlock
      content <- analyzeCss cur block
      end <- P.getPosition
      return (A.at start end (Src.Css content))



-- BLOCK


parseBlock :: Parser E.Expr [Char]
parseBlock =
  P.Parser $ \fpc (P.State pos end indent cur) cok _ cerr eerr ->
    let
      !pos5 = plusAddr# pos 5#
    in
    if P.leAddr pos5 end
      && P.eqIndex pos 0# 0x5B#Word8 {- [ -}
      && P.eqIndex pos 1# 0x63#Word8 {- c -}
      && P.eqIndex pos 2# 0x73#Word8 {- s -}
      && P.eqIndex pos 3# 0x73#Word8 {- s -}
      && P.eqIndex pos 4# 0x7C#Word8 {- | -}
    then
      let
        !(# status, newPos, newCur #) =
          eatCss pos5 end (P.slide cur 5#Word64)
      in
      case status of
        Good ->
          let
            !block = BS_UTF8.toString (BS.BS (ForeignPtr pos5 fpc) (I# (minusAddr# newPos pos5)))
            !newState = P.State (plusAddr# newPos 2#) end indent (P.slide newCur 2#Word64)
          in
          cok block newState

        Unending -> cerr cur E.CssEndless
        NotUtf8  -> cerr cur E.CssNotUtf8

    else
      eerr cur E.Start


data Status
  = Good
  | Unending
  | NotUtf8


eatCss :: Addr# -> Addr# -> Cursor -> (# Status, Addr#, Cursor #)
eatCss pos end cur =
  if P.notLtAddr pos end then
    (# Unending, pos, cur #)

  else
    case indexWord8OffAddr# pos 0# of
      0x7C#Word8 {-|-} | P.ltAddr (plusAddr# pos 1#) end && P.eqIndex pos 1# 0x5D#Word8 {-]-} ->
        (# Good, pos, cur #)

      0x0A#Word8 {- \n -} ->
        eatCss (plusAddr# pos 1#) end (P.newline cur)

      word ->
        let !newPos = P.skipUtf8 pos end word in
        if P.eqAddr pos newPos
          then (# NotUtf8, pos, cur #)
          else eatCss newPos end (P.slide cur 1#Word64)



-- RUN THE ANALYZER


analyzeCss :: Cursor -> [Char] -> Parser E.Expr Css.Content
analyzeCss cur src =
  case analyze src of
    Right content ->
      return content

    Left (Error row col msg) ->
      P.Parser $ \_ _ _ _ cerr _ ->
        cerr (jump (P.slide cur 5#Word64) (fromIntegral row) (fromIntegral col)) (E.CssProblem msg)


jump :: Cursor -> Word64 -> Word64 -> Cursor
jump cur (W64# row) (W64# col) =
  case row of
    0#Word64 -> P.slide cur col
    _        -> P.slide (and64# (plusWord64# cur (uncheckedShiftL64# row 32#)) 0xFFFFFFFF00000000#Word64) col



-- ERRORS
--
-- Positions are 0-based row/col relative to the start of the block.


data Error =
  Error Int Int [Char]



-- TOKENS


data Tok =
  Tok
    { _row :: Int
    , _col :: Int
    , _kind :: TokKind
    , _raw :: [Char]
    }


data TokKind
  = TIdent [Char]
  | TAtKw [Char]
  | TFunction [Char]
  | THash
  | TString [Char]
  | TNumber
  | TWs
  | TDot
  | TColon
  | TSemi
  | TComma
  | TOpenCurly
  | TCloseCurly
  | TOpenParen
  | TCloseParen
  | TOpenSquare
  | TCloseSquare
  | TDelim Char
  deriving (Eq)



-- TOKENIZE


tokenize :: Int -> Int -> [Char] -> Either Error [Tok]
tokenize row col chars =
  let
    emit kind raw rest =
      let (row', col') = advance row col raw in
      fmap (Tok row col kind raw :) (tokenize row' col' rest)
  in
  case chars of
    [] ->
      Right []

    '/':'*':rest ->
      case breakComment rest of
        Nothing ->
          Left (Error row col "I cannot find the `*/` that ends this comment.")

        Just (body, rest') ->
          emit TWs ("/*" ++ body ++ "*/") rest'

    c:_ | Char.isSpace c ->
      let (spaces, rest) = List.span Char.isSpace chars in
      emit TWs spaces rest

    q:rest | q == '"' || q == '\'' ->
      case scanString q rest of
        Left msg ->
          Left (Error row col msg)

        Right (body, rest') ->
          emit (TString body) (q : body ++ [q]) rest'

    '@':rest ->
      case spanIdentAllowingDashes rest of
        ([], _) ->
          emit (TDelim '@') "@" rest

        (name, rest') ->
          emit (TAtKw name) ('@' : name) rest'

    '#':rest ->
      case List.span isIdentChar rest of
        ([], _) ->
          emit (TDelim '#') "#" rest

        (name, rest') ->
          emit THash ('#' : name) rest'

    '.':c:_ | Char.isDigit c ->
      scanNumber row col chars emit

    '.':rest ->
      emit TDot "." rest

    c:_ | Char.isDigit c ->
      scanNumber row col chars emit

    '+':c:_ | Char.isDigit c || c == '.' ->
      scanNumber row col chars emit

    '-':'-':_ ->
      let (name, rest) = List.span isIdentChar (drop 2 chars) in
      emit (TIdent ("--" ++ name)) ("--" ++ name) rest

    '-':c:_ | isIdentStart c ->
      scanIdent row col chars emit

    '-':c:_ | Char.isDigit c || c == '.' ->
      scanNumber row col chars emit

    c:rest
      | isIdentStart c -> scanIdent row col chars emit
      | otherwise -> emit (simpleKind c) [c] rest


simpleKind :: Char -> TokKind
simpleKind c =
  case c of
    '{' -> TOpenCurly
    '}' -> TCloseCurly
    '(' -> TOpenParen
    ')' -> TCloseParen
    '[' -> TOpenSquare
    ']' -> TCloseSquare
    ':' -> TColon
    ';' -> TSemi
    ',' -> TComma
    _   -> TDelim c


scanIdent :: Int -> Int -> [Char] -> (TokKind -> [Char] -> [Char] -> Either Error [Tok]) -> Either Error [Tok]
scanIdent _row _col chars emit =
  let (name, rest) = List.span isIdentChar chars in
  case rest of
    '(':rest' ->
      emit (TFunction name) (name ++ "(") rest'

    _ ->
      emit (TIdent name) name rest


scanNumber :: Int -> Int -> [Char] -> (TokKind -> [Char] -> [Char] -> Either Error [Tok]) -> Either Error [Tok]
scanNumber _row _col chars emit =
  let
    (sign, afterSign) =
      case chars of
        c:cs | c == '+' || c == '-' -> ([c], cs)
        _ -> ([], chars)

    (digits, afterDigits) = List.span (\c -> Char.isDigit c || c == '.') afterSign

    (unit, rest) =
      case afterDigits of
        '%':r -> ("%", r)
        _ -> List.span isIdentChar afterDigits
  in
  emit TNumber (sign ++ digits ++ unit) rest


isIdentStart :: Char -> Bool
isIdentStart c =
  Char.isAlpha c || c == '_' || c >= '\x80'


isIdentChar :: Char -> Bool
isIdentChar c =
  Char.isAlphaNum c || c == '_' || c == '-' || c >= '\x80'


spanIdentAllowingDashes :: [Char] -> ([Char], [Char])
spanIdentAllowingDashes chars =
  case chars of
    c:_ | isIdentStart c || c == '-' -> List.span isIdentChar chars
    _ -> ([], chars)


breakComment :: [Char] -> Maybe ([Char], [Char])
breakComment chars =
  case chars of
    [] -> Nothing
    '*':'/':rest -> Just ([], rest)
    c:rest -> fmap (\(body, r) -> (c:body, r)) (breakComment rest)


scanString :: Char -> [Char] -> Either [Char] ([Char], [Char])
scanString quote chars =
  case chars of
    [] ->
      Left "I cannot find the closing quote of this string."

    '\n':_ ->
      Left "I ran into a newline inside a string. Close the string before the end of the line."

    '\\':c:rest ->
      fmap (\(body, r) -> ('\\':c:body, r)) (scanString quote rest)

    c:rest
      | c == quote -> Right ([], rest)
      | otherwise -> fmap (\(body, r) -> (c:body, r)) (scanString quote rest)


advance :: Int -> Int -> [Char] -> (Int, Int)
advance !row !col chars =
  case chars of
    [] -> (row, col)
    '\n':rest -> advance (row + 1) 0 rest
    _:rest -> advance row (col + 1) rest



-- STRUCTURE
--
-- The token stream is parsed into rules and declarations by looking ahead
-- to the first top-depth `{`, `;`, or `}` of each segment, the way
-- browsers do. Chunks are accumulated in order so the source can be
-- reproduced exactly with names rewritten.


data Ctx
  = CtxTop
  | CtxKeyframes
  | CtxProperty [Char]


data RChunk
  = RText [Char]
  | RClass [Char]
  | RVar [Char]
  | RKf [Char]
  | RAnim Int Int [Char]
  | RAnimLoose [Char]


data Acc =
  Acc
    { _revChunks :: [RChunk]
    , _classes :: Map.Map [Char] (Int, Int)
    , _kfs :: Map.Map [Char] (Int, Int)
    , _assigned :: Set.Set [Char]
    , _used :: Map.Map [Char] (Int, Int)
    , _registered :: Map.Map [Char] ((Int, Int), Maybe [Char])
    }


emptyAcc :: Acc
emptyAcc =
  Acc [] Map.empty Map.empty Set.empty Map.empty Map.empty


pushText :: [Char] -> Acc -> Acc
pushText raw acc =
  pushChunk (RText raw) acc


pushChunk :: RChunk -> Acc -> Acc
pushChunk chunk acc =
  acc { _revChunks = chunk : _revChunks acc }


pushToks :: [Tok] -> Acc -> Acc
pushToks toks acc =
  List.foldl' (\a t -> pushText (_raw t) a) acc toks


analyze :: [Char] -> Either Error Css.Content
analyze src =
  do  toks <- tokenize 0 0 src
      (acc, _) <- items CtxTop Nothing toks emptyAcc
      finalize (blockIndent src) acc


-- The whole block is indented by its position in the Elm file. Emitted
-- CSS drops that common indentation, keeping only the relative nesting.
-- This happens on the finished chunks, not the source, so error positions
-- still point into the block as written.
blockIndent :: [Char] -> Int
blockIndent src =
  let
    indents =
      [ length (takeWhile (== ' ') line)
      | line <- lines src
      , any (\c -> c /= ' ' && c /= '\r' && c /= '\t') line
      ]
  in
  if null indents then 0 else minimum indents



-- ITEMS


items :: Ctx -> Maybe (Int, Int) -> [Tok] -> Acc -> Either Error (Acc, [Tok])
items ctx mopen toks acc =
  case toks of
    [] ->
      case mopen of
        Nothing -> Right (acc, [])
        Just (r, c) -> Left (Error r c "I cannot find the `}` that closes this block.")

    Tok r c TCloseCurly raw : rest ->
      case mopen of
        Nothing -> Left (Error r c "I ran into a stray `}` that does not close anything.")
        Just _ -> Right (pushText raw acc, rest)

    t@(Tok _ _ TWs _) : rest ->
      items ctx mopen rest (pushText (_raw t) acc)

    t@(Tok _ _ TSemi _) : rest ->
      items ctx mopen rest (pushText (_raw t) acc)

    _ ->
      case segment 0 [] toks of
        (seg, Just brace@(Tok br bc TOpenCurly _), rest) ->
          do  (ctx', acc1) <- prelude ctx seg acc
              let acc2 = pushText (_raw brace) acc1
              (acc3, rest') <- items ctx' (Just (br, bc)) rest acc2
              items ctx mopen rest' acc3

        (seg, Just semi@(Tok _ _ TSemi _), rest) ->
          do  acc1 <- declaration ctx seg acc
              items ctx mopen rest (pushText (_raw semi) acc1)

        (seg, _, rest) ->
          -- trailing declaration before `}` or end of input
          do  acc1 <- declaration ctx seg acc
              items ctx mopen rest acc1


-- Collect tokens until the first `{`, `;`, or `}` outside parens/brackets.
-- For `}` the terminator is left in the stream so the caller closes the block.
segment :: Int -> [Tok] -> [Tok] -> ([Tok], Maybe Tok, [Tok])
segment !depth revSeg toks =
  case toks of
    [] ->
      (reverse revSeg, Nothing, [])

    t : rest ->
      case _kind t of
        TFunction _   -> segment (depth + 1) (t : revSeg) rest
        TOpenParen    -> segment (depth + 1) (t : revSeg) rest
        TOpenSquare   -> segment (depth + 1) (t : revSeg) rest
        TCloseParen   -> segment (max 0 (depth - 1)) (t : revSeg) rest
        TCloseSquare  -> segment (max 0 (depth - 1)) (t : revSeg) rest
        TOpenCurly  | depth == 0 -> (reverse revSeg, Just t, rest)
        TSemi       | depth == 0 -> (reverse revSeg, Just t, rest)
        TCloseCurly | depth == 0 -> (reverse revSeg, Just t, toks)
        _             -> segment depth (t : revSeg) rest



-- RULE PRELUDES


prelude :: Ctx -> [Tok] -> Acc -> Either Error (Ctx, Acc)
prelude ctx seg acc =
  case dropWhile isWs seg of
    Tok _ _ (TAtKw name) _ : _ | isKeyframesAtKw name ->
      do  acc' <- keyframesPrelude seg acc
          return (CtxKeyframes, acc')

    Tok _ _ (TAtKw "property") _ : _ ->
      do  (name, acc') <- propertyPrelude seg acc
          return (CtxProperty name, acc')

    _ ->
      do  acc' <- selectorPrelude ctx seg acc
          return (ctx, acc')


isKeyframesAtKw :: [Char] -> Bool
isKeyframesAtKw name =
  name == "keyframes"
    || name == "-webkit-keyframes"
    || name == "-moz-keyframes"
    || name == "-o-keyframes"


isWs :: Tok -> Bool
isWs t =
  _kind t == TWs


keyframesPrelude :: [Tok] -> Acc -> Either Error Acc
keyframesPrelude seg acc =
  let
    (before, afterAtKw) = List.break isAtKw seg
    isAtKw t = case _kind t of { TAtKw _ -> True ; _ -> False }
  in
  case afterAtKw of
    atKw : rest ->
      let
        (ws, afterWs) = List.span isWs rest
      in
      case afterWs of
        Tok r c (TIdent name) _ : trailing ->
          do  checkKeyframesName r c name (_kfs acc)
              let acc1 = pushToks (before ++ [atKw] ++ ws) acc
              let acc2 = pushChunk (RKf name) acc1
              let acc3 = acc2 { _kfs = Map.insert name (r, c) (_kfs acc2) }
              Right (pushToks trailing acc3)

        Tok r c _ _ : _ ->
          Left (Error r c "I was expecting a name right after `@keyframes`, like `@keyframes fadeIn`.")

        [] ->
          let Tok r c _ raw = atKw in
          Left (Error r (c + length raw) "I was expecting a name right after `@keyframes`, like `@keyframes fadeIn`.")

    [] ->
      Right (pushToks seg acc)


propertyPrelude :: [Tok] -> Acc -> Either Error ([Char], Acc)
propertyPrelude seg acc =
  let
    (before, afterAtKw) = List.break isAtKw seg
    isAtKw t = case _kind t of { TAtKw _ -> True ; _ -> False }
  in
  case afterAtKw of
    atKw : rest ->
      let
        (ws, afterWs) = List.span isWs rest
      in
      case afterWs of
        Tok r c (TIdent ('-':'-':name)) _ : trailing ->
          do  checkFieldName r c "The custom property name" name
              let acc1 = pushToks (before ++ [atKw] ++ ws) acc
              let acc2 = pushChunk (RVar name) acc1
              let acc3 = acc2 { _registered = Map.insertWith keepOld name ((r, c), Nothing) (_registered acc2) }
              Right (name, pushToks trailing acc3)

        Tok r c _ _ : _ ->
          Left (Error r c "I was expecting a custom property name right after `@property`, like `@property --progress`.")

        [] ->
          let Tok r c _ raw = atKw in
          Left (Error r (c + length raw) "I was expecting a custom property name right after `@property`, like `@property --progress`.")

    [] ->
      Right ("", pushToks seg acc)


keepOld :: a -> a -> a
keepOld _new old =
  old


selectorPrelude :: Ctx -> [Tok] -> Acc -> Either Error Acc
selectorPrelude ctx seg acc =
  case seg of
    [] ->
      Right acc

    dot@(Tok r c TDot _) : rest ->
      case ctx of
        CtxKeyframes ->
          Left (Error r c "I was not expecting a class selector inside `@keyframes`. Keyframe selectors are `from`, `to`, or percentages.")

        _ ->
          case rest of
            Tok cr cc (TIdent name) _ : trailing ->
              do  checkFieldName cr cc "The class name" name
                  let acc1 = pushText (_raw dot) acc
                  let acc2 = pushChunk (RClass name) acc1
                  let acc3 = acc2 { _classes = Map.insertWith keepOld name (cr, cc) (_classes acc2) }
                  selectorPrelude ctx trailing acc3

            _ ->
              Left (Error r c "I was expecting a class name right after this `.` like `.card`.")

    t : rest ->
      selectorPrelude ctx rest (pushText (_raw t) acc)



-- DECLARATIONS


declaration :: Ctx -> [Tok] -> Acc -> Either Error Acc
declaration ctx seg acc =
  let
    (ws, afterWs) = List.span isWs seg
    acc0 = pushToks ws acc
  in
  case afterWs of
    [] ->
      Right acc0

    Tok r c (TIdent ('-':'-':name)) _ : rest ->
      do  checkFieldName r c "The custom property name" name
          let acc1 = pushChunk (RVar name) acc0
          let acc2 = acc1 { _assigned = Set.insert name (_assigned acc1) }
          value ctx rest acc2

    prop@(Tok _ _ (TIdent p) _) : rest | isAnimationProp p ->
      let
        loose = any isVarFunction rest
      in
      animationValue loose rest (pushText (_raw prop) acc0)

    prop@(Tok _ _ (TIdent "syntax") _) : rest ->
      case ctx of
        CtxProperty varName | not (null varName) ->
          let
            msyntax = List.foldr (\t r -> case _kind t of { TString s -> Just s ; _ -> r }) Nothing rest
            update (pos, old) = (pos, maybe old Just msyntax)
            acc1 = acc0 { _registered = Map.adjust update varName (_registered acc0) }
          in
          value ctx rest (pushText (_raw prop) acc1)

        _ ->
          value ctx rest (pushText (_raw prop) acc0)

    _ ->
      value ctx afterWs acc0


isAnimationProp :: [Char] -> Bool
isAnimationProp p =
  let name = map Char.toLower p in
  name == "animation"
    || name == "animation-name"
    || name == "-webkit-animation"
    || name == "-webkit-animation-name"


isVarFunction :: Tok -> Bool
isVarFunction t =
  case _kind t of
    TFunction name -> map Char.toLower name == "var"
    _ -> False


-- Ordinary declaration values: pass tokens through, resolving var() usages
-- and bare custom property references.
value :: Ctx -> [Tok] -> Acc -> Either Error Acc
value ctx toks acc =
  case toks of
    [] ->
      Right acc

    t : rest | isVarFunction t ->
      do  (acc1, rest') <- varUsage t rest (pushText (_raw t) acc)
          value ctx rest' acc1

    Tok r c (TIdent ('-':'-':name)) _ : rest ->
      do  checkFieldName r c "The custom property name" name
          let acc1 = pushChunk (RVar name) acc
          let acc2 = acc1 { _used = Map.insertWith keepOld name (r, c) (_used acc1) }
          value ctx rest acc2

    t : rest ->
      value ctx rest (pushText (_raw t) acc)


-- After a `var(` token: skip whitespace, then require `--name`.
varUsage :: Tok -> [Tok] -> Acc -> Either Error (Acc, [Tok])
varUsage varTok toks acc =
  let
    (ws, afterWs) = List.span isWs toks
    acc0 = pushToks ws acc
  in
  case afterWs of
    Tok r c (TIdent ('-':'-':name)) _ : rest ->
      do  checkFieldName r c "The custom property name" name
          let acc1 = pushChunk (RVar name) acc0
          let acc2 = acc1 { _used = Map.insertWith keepOld name (r, c) (_used acc1) }
          Right (acc2, rest)

    _ ->
      let Tok r c _ raw = varTok in
      Left (Error r (c + length raw) "I was expecting a custom property name inside `var(...)`, like `var(--gap)`.")


-- Values of `animation` and `animation-name`: identifiers that are not
-- animation keywords must be names of @keyframes declared in this block.
-- If the value contains var(...) it cannot be known statically, so unknown
-- identifiers are passed through instead of being errors.
animationValue :: Bool -> [Tok] -> Acc -> Either Error Acc
animationValue loose toks acc =
  case toks of
    [] ->
      Right acc

    t : rest | isVarFunction t ->
      do  (acc1, rest') <- varUsage t rest (pushText (_raw t) acc)
          animationValue loose rest' acc1

    Tok r c (TIdent ('-':'-':varName)) _ : rest ->
      do  checkFieldName r c "The custom property name" varName
          let acc1 = pushChunk (RVar varName) acc
          let acc2 = acc1 { _used = Map.insertWith keepOld varName (r, c) (_used acc1) }
          animationValue loose rest acc2

    Tok r c (TIdent name) _ : rest ->
      if Set.member (map Char.toLower name) animationKeywords then
        animationValue loose rest (pushText name acc)
      else
        let chunk = if loose then RAnimLoose name else RAnim r c name in
        animationValue loose rest (pushChunk chunk acc)

    t : rest ->
      animationValue loose rest (pushText (_raw t) acc)


animationKeywords :: Set.Set [Char]
animationKeywords =
  Set.fromList
    [ "initial", "inherit", "unset", "revert", "revert-layer", "default"
    , "none", "auto"
    , "normal", "reverse", "alternate", "alternate-reverse"
    , "forwards", "backwards", "both"
    , "running", "paused"
    , "infinite"
    , "linear", "ease", "ease-in", "ease-out", "ease-in-out"
    , "step-start", "step-end"
    ]



-- NAME CHECKS


checkFieldName :: Int -> Int -> [Char] -> [Char] -> Either Error ()
checkFieldName row col what name =
  if isFieldName name then
    Right ()
  else
    Left $ Error row col $
      what ++ " `" ++ name ++ "` must be a valid Elm record field name:\
      \ start with a lowercase letter and use only letters, digits, and underscores."
      ++
      if elem '-' name then
        " Names in CSS blocks become record fields, so instead of kebab-case, try `"
        ++ kebabToCamel name ++ "`."
      else
        ""


isFieldName :: [Char] -> Bool
isFieldName name =
  case name of
    [] ->
      False

    c:cs ->
      Char.isAsciiLower c
        && all (\x -> Char.isAsciiUpper x || Char.isAsciiLower x || Char.isDigit x || x == '_') cs
        && Set.notMember name elmKeywords


elmKeywords :: Set.Set [Char]
elmKeywords =
  Set.fromList
    [ "if", "then", "else", "case", "of", "let", "in", "type"
    , "module", "where", "import", "exposing", "as", "port"
    ]


kebabToCamel :: [Char] -> [Char]
kebabToCamel name =
  case name of
    [] -> []
    '-':c:rest -> Char.toUpper c : kebabToCamel rest
    '-':rest -> kebabToCamel rest
    c:rest -> c : kebabToCamel rest


checkKeyframesName :: Int -> Int -> [Char] -> Map.Map [Char] (Int, Int) -> Either Error ()
checkKeyframesName row col name declared =
  do  checkFieldName row col "The @keyframes name" name
      if Set.member name animationKeywords
        then Left $ Error row col $
          "The @keyframes name `" ++ name ++ "` is a reserved animation keyword,\
          \ so the `animation` shorthand could never refer to it. Pick a different name."
        else Right ()
      if Map.member name declared
        then Left $ Error row col $
          "There is already a `@keyframes " ++ name ++ "` in this block."
        else Right ()



-- FINALIZE


finalize :: Int -> Acc -> Either Error Css.Content
finalize indent acc =
  do  checkCollisions (_classes acc) (_kfs acc)
      chunks <- traverse (resolveChunk (_kfs acc)) (reverse (_revChunks acc))
      let inputs = toInputs acc
      Right $ Css.Content (mergeChunks indent chunks) $
        Css.Types
          { Css._classes = Set.fromList (map Name.fromChars (Map.keys (_classes acc)))
          , Css._keyframes = Set.fromList (map Name.fromChars (Map.keys (_kfs acc)))
          , Css._vars = inputs
          }


checkCollisions :: Map.Map [Char] (Int, Int) -> Map.Map [Char] (Int, Int) -> Either Error ()
checkCollisions classes kfs =
  case Map.toList (Map.intersectionWith (\_ kfPos -> kfPos) classes kfs) of
    [] ->
      Right ()

    (name, (r, c)) : _ ->
      Left $ Error r c $
        "`" ++ name ++ "` is declared as both a class and a @keyframes name.\
        \ They share one record, so rename one of them."


resolveChunk :: Map.Map [Char] (Int, Int) -> RChunk -> Either Error RChunk
resolveChunk kfs chunk =
  case chunk of
    RAnim r c name ->
      if Map.member name kfs
        then Right (RKf name)
        else Left $ Error r c $
          "The animation name `" ++ name ++ "` is not defined by any @keyframes in this block."

    RAnimLoose name ->
      Right (if Map.member name kfs then RKf name else RText name)

    _ ->
      Right chunk


toInputs :: Acc -> Map.Map Name.Name Css.PropType
toInputs acc =
  let
    registeredNames = Map.map (const ()) (_registered acc)
    usedNames = Map.map (const ()) (_used acc)
    candidates = Map.union usedNames registeredNames

    external = Map.withoutKeys candidates (_assigned acc)

    toType name _ =
      case Map.lookup name (_registered acc) of
        Just (_, Just syntax) -> syntaxToType syntax
        _ -> Css.Value
  in
  Map.fromList $
    map (\(name, tipe) -> (Name.fromChars name, tipe)) $
      Map.toList (Map.mapWithKey toType external)


syntaxToType :: [Char] -> Css.PropType
syntaxToType syntax =
  case syntax of
    "<length>"     -> Css.Length
    "<percentage>" -> Css.Percentage
    "<color>"      -> Css.Color
    "<number>"     -> Css.Number
    "<integer>"    -> Css.Integer
    "<time>"       -> Css.Duration
    "<angle>"      -> Css.Angle
    _              -> Css.Value


mergeChunks :: Int -> [RChunk] -> [Css.Chunk]
mergeChunks indent chunks =
  case chunks of
    [] ->
      []

    RText a : RText b : rest ->
      mergeChunks indent (RText (a ++ b) : rest)

    RText a : rest ->
      Css.Text (BS_UTF8.fromString (stripIndent indent a)) : mergeChunks indent rest

    RClass name : rest ->
      Css.ClassRef (Name.fromChars name) : mergeChunks indent rest

    RVar name : rest ->
      Css.VarRef (Name.fromChars name) : mergeChunks indent rest

    RKf name : rest ->
      Css.KeyframesRef (Name.fromChars name) : mergeChunks indent rest

    RAnim _ _ name : rest ->
      Css.KeyframesRef (Name.fromChars name) : mergeChunks indent rest

    RAnimLoose name : rest ->
      Css.Text (BS_UTF8.fromString name) : mergeChunks indent rest


-- Drop up to `indent` spaces after every newline. Newlines only appear in
-- whitespace and comments (strings reject them), and the indentation
-- following a newline always sits in the same text chunk, so a per-chunk
-- pass is safe.
stripIndent :: Int -> [Char] -> [Char]
stripIndent indent chars =
  if indent <= 0 then
    chars
  else
    let
      go cs =
        case cs of
          [] -> []
          '\n' : rest -> '\n' : go (dropSpaces indent rest)
          c : rest -> c : go rest

      dropSpaces n cs =
        case cs of
          ' ' : rest | n > 0 -> dropSpaces (n - 1) rest
          _ -> cs
    in
    go chars
