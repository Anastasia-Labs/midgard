{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Testing.LinkedList
Description : Tests for the Plutarch port of @aiken-design-patterns/linked-list@.

The authentication layer is where the list's structural invariants live, so the
negative cases carry the weight: an element UTxO must hold exactly Ada plus one
NFT of the list policy, at quantity one, with an inline datum, and its asset name
must match the structural role its datum claims.

Note the two rejection modes, which mirror Aiken exactly. Operations that scan
the inputs — @deinit@, the inserts, @append@, @remove@, @fold_from_root@ — run
their checks inside a callback the scanner @expect@s, so a failed check /errors/
and the test asserts 'pfails'. @init@ takes its output directly and simply
returns @False@, so the test asserts @pnot@. Both reject the transaction; the
difference only shows up in a test that distinguishes them.
-}
module Testing.LinkedList (tests) where

import Numeric (showHex)

import Data.ByteString qualified as BS
import PlutusCore.Data qualified as PD
import PlutusLedgerApi.V1.Address (scriptHashAddress)
import PlutusLedgerApi.V1.Value (CurrencySymbol (..), TokenName (..), singleton)
import PlutusLedgerApi.V1.Value (Value (..))
import PlutusLedgerApi.V3 (
  Address,
  Datum (..),
  OutputDatum (..),
  ScriptHash (..),
  TxInInfo (..),
  TxId (..),
  TxOut (..),
  TxOutRef (..),
 )
import PlutusLedgerApi.V3.MintValue (MintValue (UnsafeMintValue))
import PlutusTx.Builtins (BuiltinData, dataToBuiltinData, toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit

import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PMintValue)
import Plutarch.Prelude

import LinkedList (
  pappendUnordered,
  pdeinit,
  pfoldFromRoot,
  pgetElementInfo,
  pgetNodeElementInfo,
  pgetRootElementInfo,
  pinit,
  pinsertAscending,
  premove,
 )
import LinkedList.Internal (POrdering (..), pkeyFitsBetween)
import Testing.Eval (passertEval, pfails)
import Testing.ScriptContextBuilder (currencySymbolFromHex, mkAdaValue)

-- | Collects the tests defined in this module.
tests :: TestTree
tests =
  testGroup
    "Linked List Tests"
    [ testGroup
        "getElementInfo"
        [ testCase "reads a root and reports no key" $
            passertEval $
              elementKeyIsNothing (mkOut rootValue (rootDatum linkToB))
        , testCase "reads a node and strips the key prefix" $
            passertEval $
              elementKeyIs (mkOut (nodeValue "aaa") (nodeDatum linkNone)) "aaa"
        , testCase "rejects a root whose asset name is not the root key" $
            pfails $
              runElementInfo (mkOut (nodeValue "aaa") (rootDatum linkNone))
        , testCase "rejects a node whose asset name lacks the prefix" $
            pfails $
              runElementInfo (mkOut (mkValue listPolicy (TokenName "XX-aaa")) (nodeDatum linkNone))
        , testCase "rejects an element under a foreign policy" $
            pfails $
              runElementInfo (mkOut (mkValue foreignPolicy rootName) (rootDatum linkNone))
        , testCase "rejects an element holding two NFTs" $
            pfails $
              runElementInfo
                ( mkOut
                    (rootValue <> singleton foreignPolicy (TokenName "OTHER") 1)
                    (rootDatum linkNone)
                )
        , testCase "rejects an element whose NFT quantity is two" $
            pfails $
              runElementInfo (mkOut (singleton listPolicy rootName 2 <> mkAdaValue 2_000_000) (rootDatum linkNone))
        , testCase "rejects an element with no Ada entry" $
            pfails $
              runElementInfo (mkOut (singleton listPolicy rootName 1) (rootDatum linkNone))
        , testCase "rejects a datum hash instead of an inline datum" $
            pfails $
              runElementInfo (TxOut listAddress rootValue NoOutputDatum Nothing)
        ]
    , testGroup
        "getRootElementInfo"
        [ testCase "accepts the root" $
            passertEval $
              pgetRootElementInfo
                (pconstant (mkOut rootValue (rootDatum linkNone)))
                (\_addr lovelace _d _link -> lovelace #== 2_000_000)
                (pdata (pconstant listPolicy))
                (pdata (pconstant rootName))
        , testCase "rejects a node" $
            pfails $
              pgetRootElementInfo
                (pconstant (mkOut (nodeValue "aaa") (nodeDatum linkNone)))
                (\_addr _l _d _link -> pconstant @PBool True)
                (pdata (pconstant listPolicy))
                (pdata (pconstant rootName))
        ]
    , testGroup
        "getNodeElementInfo"
        [ testCase "accepts a node and strips the prefix" $
            passertEval $
              pgetNodeElementInfo
                (pconstant (mkOut (nodeValue "aaa") (nodeDatum linkNone)))
                (\_addr _l key _d _link -> key #== pconstant "aaa")
                (pdata (pconstant listPolicy))
                (pconstant nodePrefix)
                (pconstant nodePrefixLen)
        , testCase "rejects the root" $
            pfails $
              pgetNodeElementInfo
                (pconstant (mkOut rootValue (rootDatum linkNone)))
                (\_addr _l _key _d _link -> pconstant @PBool True)
                (pdata (pconstant listPolicy))
                (pconstant nodePrefix)
                (pconstant nodePrefixLen)
        ]
    , testGroup
        "init"
        [ testCase "accepts a fresh empty root" $
            passertEval $ runInit (mkOut rootValue (rootDatum linkNone)) (mintOf rootName 1)
        , testCase "rejects an unproved nonce" $
            pfails $
              pinit
                (pconstant False)
                (pconstant (mkOut rootValue (rootDatum linkNone)))
                (pconstant (mintOf rootName 1))
                (\_a _l _d -> pconstant @PBool True)
                (pdata (pconstant listPolicy))
                (pdata (pconstant rootName))
        , testCase "rejects a root that already links somewhere" $
            passertEval $
              pnot #$ runInit (mkOut rootValue (rootDatum linkToB)) (mintOf rootName 1)
        , testCase "rejects a node datum" $
            pfails $ runInit (mkOut (nodeValue "aaa") (nodeDatum linkNone)) (mintOf (nodeName "aaa") 1)
        , testCase "rejects minting the wrong quantity" $
            passertEval $
              pnot #$ runInit (mkOut rootValue (rootDatum linkNone)) (mintOf rootName 2)
        ]
    , testGroup
        "deinit"
        [ testCase "accepts burning an empty root" $
            passertEval $ runDeinit [rootIn linkNone] (mintOf rootName (-1))
        , testCase "rejects deinitialising a non-empty list" $
            pfails $ runDeinit [rootIn linkToB] (mintOf rootName (-1))
        , testCase "rejects a mint instead of a burn" $
            pfails $ runDeinit [rootIn linkNone] (mintOf rootName 1)
        ]
    , testGroup
        "insertAscending"
        [ testCase "accepts a first node after the root" $
            passertEval $
              runInsert
                [rootIn linkNone]
                (mkOut rootValue (rootDatum (linkTo "bbb")))
                (mkOut (nodeValue "bbb") (nodeDatum linkNone))
                (mintOf (nodeName "bbb") 1)
        , testCase "rejects a key that breaks ascending order" $
            pfails $
              runInsert
                [rootIn (linkTo "bbb")]
                (mkOut rootValue (rootDatum (linkTo "ccc")))
                (mkOut (nodeValue "ccc") (nodeDatum (linkTo "bbb")))
                (mintOf (nodeName "ccc") 1)
        , testCase "rejects a continued anchor not pointing at the new node" $
            pfails $
              runInsert
                [rootIn linkNone]
                (mkOut rootValue (rootDatum linkNone))
                (mkOut (nodeValue "bbb") (nodeDatum linkNone))
                (mintOf (nodeName "bbb") 1)
        , testCase "rejects a burn where a mint is required" $
            pfails $
              runInsert
                [rootIn linkNone]
                (mkOut rootValue (rootDatum (linkTo "bbb")))
                (mkOut (nodeValue "bbb") (nodeDatum linkNone))
                (mintOf (nodeName "bbb") (-1))
        ]
    , testGroup
        "appendUnordered"
        [ testCase "accepts a terminal node after a terminal root" $
            passertEval $
              runAppend
                [rootIn linkNone]
                (mkOut rootValue (rootDatum (linkTo "bbb")))
                (mkOut (nodeValue "bbb") (nodeDatum linkNone))
                (mintOf (nodeName "bbb") 1)
        , testCase "rejects appending to a non-terminal anchor" $
            pfails $
              runAppend
                [rootIn (linkTo "zzz")]
                (mkOut rootValue (rootDatum (linkTo "bbb")))
                (mkOut (nodeValue "bbb") (nodeDatum (linkTo "zzz")))
                (mintOf (nodeName "bbb") 1)
        ]
    , testGroup
        "remove"
        [ testCase "accepts removing the node the root points at" $
            passertEval $
              runRemove
                [rootIn (linkTo "bbb"), nodeIn "bbb" linkNone]
                (mkOut rootValue (rootDatum linkNone))
                (mintOf (nodeName "bbb") (-1))
        , testCase "relinks the anchor past the removed node" $
            passertEval $
              runRemove
                [rootIn (linkTo "bbb"), nodeIn "bbb" (linkTo "ccc")]
                (mkOut rootValue (rootDatum (linkTo "ccc")))
                (mintOf (nodeName "bbb") (-1))
        , testCase "rejects an anchor that does not point at the removed node" $
            pfails $
              runRemove
                [rootIn (linkTo "zzz"), nodeIn "bbb" linkNone]
                (mkOut rootValue (rootDatum linkNone))
                (mintOf (nodeName "bbb") (-1))
        , testCase "rejects a continued anchor that drops the successor" $
            pfails $
              runRemove
                [rootIn (linkTo "bbb"), nodeIn "bbb" (linkTo "ccc")]
                (mkOut rootValue (rootDatum linkNone))
                (mintOf (nodeName "bbb") (-1))
        , testCase "rejects a mint instead of a burn" $
            pfails $
              runRemove
                [rootIn (linkTo "bbb"), nodeIn "bbb" linkNone]
                (mkOut rootValue (rootDatum linkNone))
                (mintOf (nodeName "bbb") 1)
        ]
    , testGroup
        "foldFromRoot"
        [ testCase "folds the first node into the root" $
            passertEval $
              runFold
                [rootIn (linkTo "bbb"), nodeIn "bbb" (linkTo "ccc")]
                (mkOut rootValue (rootDatum (linkTo "ccc")))
                (mintOf (nodeName "bbb") (-1))
        , testCase "rejects a continued element that is not a root" $
            pfails $
              runFold
                [rootIn (linkTo "bbb"), nodeIn "bbb" linkNone]
                (mkOut rootValue (nodeDatum linkNone))
                (mintOf (nodeName "bbb") (-1))
        ]
    , testGroup
        "keyFitsBetween"
        [ testCase "ascending accepts a key between its neighbours" $
            passertEval $
              pkeyFitsBetween # pcon PLess # pjust "aaa" # pconstant "bbb" # pjust "ccc"
        , testCase "ascending rejects a key below its predecessor" $
            passertEval $
              pnot #$ pkeyFitsBetween # pcon PLess # pjust "ccc" # pconstant "bbb" # pnothing
        , testCase "ascending rejects a key above its successor" $
            passertEval $
              pnot #$ pkeyFitsBetween # pcon PLess # pnothing # pconstant "ccc" # pjust "bbb"
        , testCase "descending accepts a decreasing run" $
            passertEval $
              pkeyFitsBetween # pcon PGreater # pjust "ccc" # pconstant "bbb" # pjust "aaa"
        , testCase "absent neighbours are list boundaries" $
            passertEval $
              pkeyFitsBetween # pcon PLess # pnothing # pconstant "bbb" # pnothing
        , testCase "equal keys are rejected in both directions" $
            passertEval $
              pand'
                # (pnot #$ pkeyFitsBetween # pcon PLess # pjust "bbb" # pconstant "bbb" # pnothing)
                # (pnot #$ pkeyFitsBetween # pcon PGreater # pjust "bbb" # pconstant "bbb" # pnothing)
        ]
    ]

--------------------------------------------------------------------------------
-- Fixtures
--------------------------------------------------------------------------------

policyFor :: Int -> CurrencySymbol
policyFor n = currencySymbolFromHex (replicate (56 - length h) '0' <> h)
  where
    h = showHex n ""

listPolicy, foreignPolicy :: CurrencySymbol
listPolicy = policyFor 1
foreignPolicy = policyFor 2

-- | The root NFT's asset name, deliberately outside the node namespace.
rootName :: TokenName
rootName = TokenName "ROOT"

nodePrefix :: BS.ByteString
nodePrefix = "ND"

nodePrefixLen :: Integer
nodePrefixLen = 2

listAddress :: Address
listAddress = scriptHashAddress (ScriptHash (unCurrencySymbol listPolicy))

mkValue :: CurrencySymbol -> TokenName -> Value
mkValue cs tn = mkAdaValue 2_000_000 <> singleton cs tn 1

rootValue :: Value
rootValue = mkValue listPolicy rootName

-- | A node NFT: @node_key_prefix ++ node_key@.
nodeValue :: BS.ByteString -> Value
nodeValue key = mkValue listPolicy (TokenName (toBuiltin (nodePrefix <> key)))

mkOut :: Value -> BuiltinData -> TxOut
mkOut val dat = TxOut listAddress val (OutputDatum (Datum dat)) Nothing

--------------------------------------------------------------------------------
-- Datums
--------------------------------------------------------------------------------

-- | @Element { data, link }@ — a single-constructor record, so @Constr 0@.
mkElement :: PD.Data -> PD.Data -> BuiltinData
mkElement elementData link = dataToBuiltinData (PD.Constr 0 [elementData, link])

-- | @Root { data }@ is constructor 0 of @ElementData@.
rootDatum :: PD.Data -> BuiltinData
rootDatum = mkElement (PD.Constr 0 [PD.I 0])

-- | @Node { data }@ is constructor 1 of @ElementData@.
nodeDatum :: PD.Data -> BuiltinData
nodeDatum = mkElement (PD.Constr 1 [PD.I 0])

-- | @None@ / @Some(key)@ as Plutus @Data@.
linkNone :: PD.Data
linkNone = PD.Constr 1 []

linkToB :: PD.Data
linkToB = PD.Constr 0 [PD.B "bbb"]

--------------------------------------------------------------------------------
-- Applying the terms
--------------------------------------------------------------------------------

runElementInfo :: forall s. TxOut -> Term s PBool
runElementInfo out =
  pgetElementInfo
    (pconstant out)
    (\_addr _l _key _d _link -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

-- | The element's reported key is @None@, i.e. it is the root.
elementKeyIsNothing :: forall s. TxOut -> Term s PBool
elementKeyIsNothing out =
  pgetElementInfo
    (pconstant out)
    (\_addr _l key _d _link -> pmatch key $ \case PDNothing -> pconstant True; _ -> pconstant False)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

-- | The element's reported key is @Some expected@, with the prefix stripped.
elementKeyIs :: forall s. TxOut -> BS.ByteString -> Term s PBool
elementKeyIs out expected =
  pgetElementInfo
    (pconstant out)
    ( \_addr _l key _d _link -> pmatch key $ \case
        PDJust k -> pfromData k #== pconstant expected
        PDNothing -> pconstant False
    )
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

pjust :: forall s. BS.ByteString -> Term s (PMaybeData PByteString)
pjust b = pcon (PDJust (pdata (pconstant b)))

pnothing :: forall s. Term s (PMaybeData PByteString)
pnothing = pcon PDNothing

--------------------------------------------------------------------------------
-- Transaction fixtures for the structural operations
--------------------------------------------------------------------------------

nodeName :: BS.ByteString -> TokenName
nodeName key = TokenName (toBuiltin (nodePrefix <> key))

-- | @Some key@ as a link.
linkTo :: BS.ByteString -> PD.Data
linkTo key = PD.Constr 0 [PD.B key]

-- | A mint field carrying one entry of the list policy.
mintOf :: TokenName -> Integer -> MintValue
mintOf tn qty = UnsafeMintValue (getValue (singleton listPolicy tn qty))

{- | A mint field as a Plutarch term.

Note the @pfromData@: 'pconstant' at 'PMintValue' yields a Data-backed term,
whose inner map is still a Data @Map@ rather than a builtin list of pairs. The
library peels the representation with @pto@, so the term has to be brought
across the data boundary first — exactly as the script context does when a
validator reads @ptxInfo'mint@.
-}
pmint :: forall s. MintValue -> Term s PMintValue
pmint m = pfromData (pconstant @(PAsData PMintValue) m)

outRefN :: Integer -> TxOutRef
outRefN n = TxOutRef (TxId "0101010101010101010101010101010101010101010101010101010101010101") n

-- | The root as a spent input, at out-ref 0.
rootIn :: PD.Data -> TxInInfo
rootIn link = TxInInfo (outRefN 0) (mkOut rootValue (rootDatum link))

-- | A node as a spent input, at out-ref 1.
nodeIn :: BS.ByteString -> PD.Data -> TxInInfo
nodeIn key link = TxInInfo (outRefN 1) (mkOut (nodeValue key) (nodeDatum link))

--------------------------------------------------------------------------------
-- Applying the operations
--------------------------------------------------------------------------------

runInit :: forall s. TxOut -> MintValue -> Term s PBool
runInit out mint =
  pinit
    (pconstant True)
    (pconstant out)
    (pmint mint)
    (\_addr _l _d -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))

runDeinit :: forall s. [TxInInfo] -> MintValue -> Term s PBool
runDeinit ins mint =
  pdeinit
    (pconstant ins)
    (pmint mint)
    (\_in _l _d -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))

runInsert :: forall s. [TxInInfo] -> TxOut -> TxOut -> MintValue -> Term s PBool
runInsert ins contAnchor newNode mint =
  pinsertAscending
    (pconstant contAnchor)
    (pconstant newNode)
    (pconstant ins)
    (pmint mint)
    (\_i _lc _k _d _nl _nk _nd _nlink -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

runAppend :: forall s. [TxInInfo] -> TxOut -> TxOut -> MintValue -> Term s PBool
runAppend ins contAnchor newNode mint =
  pappendUnordered
    (pconstant contAnchor)
    (pconstant newNode)
    (pconstant ins)
    (pmint mint)
    (\_i _lc _k _d _nl _nk _nd -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

runRemove :: forall s. [TxInInfo] -> TxOut -> MintValue -> Term s PBool
runRemove ins contAnchor mint =
  premove
    (pconstant (outRefN 0))
    (pconstant contAnchor)
    (pconstant ins)
    (pmint mint)
    (\_i _lc _k _d _ri _rl _rk _rd _rlink -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)

runFold :: forall s. [TxInInfo] -> TxOut -> MintValue -> Term s PBool
runFold ins contRoot mint =
  pfoldFromRoot
    (pconstant (outRefN 0))
    (pconstant contRoot)
    (pconstant ins)
    (pmint mint)
    (\_i _lc _d _fi _fl _fk _fd _flink _cd -> pconstant @PBool True)
    (pdata (pconstant listPolicy))
    (pdata (pconstant rootName))
    (pconstant nodePrefix)
    (pconstant nodePrefixLen)
