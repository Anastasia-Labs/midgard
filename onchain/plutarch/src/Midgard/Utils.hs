{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Midgard.Utils where

import Data.Text qualified as T
import Plutarch.LedgerApi.Value (padaSymbol, pvalueOf, pnormalize)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
--import Plutarch.LedgerApi.V2 
import Plutarch.LedgerApi.V3 
import Plutarch.Bool (pand')
import Plutarch.TermCont 
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import qualified Plutarch.LedgerApi.Value as Value
import Plutarch.Internal
import Plutarch.Builtin
import Plutarch.Num
import Plutarch.DataRepr.Internal.Field
    ( HRec(..), Labeled(Labeled) )  

type PPosixTimeRange = PInterval PPosixTime

type PScriptInfoHRec (s :: S) =
  HRec
    '[ '("_0", Term s (PAsData PTxOutRef))
     , '("_1", Term s (PAsData (PMaybeData PDatum)))
     ]

pletFieldsSpending :: forall {s :: S} {r :: PType}. Term s PData -> (PScriptInfoHRec s -> Term s r) -> Term s r 
pletFieldsSpending term = runTermCont $ do
  constrPair <- tcont $ plet $ pasConstr # term
  fields <- tcont $ plet $ psndBuiltin # constrPair
  checkedFields <- tcont $ plet $ pif ((pfstBuiltin # constrPair) #== 1) fields perror
  let outRef = punsafeCoerce @_ @_ @(PAsData PTxOutRef) $ phead # checkedFields
      datum = punsafeCoerce @_ @_ @(PAsData (PMaybeData PDatum)) $ phead # (ptail # checkedFields)
  tcont $ \f -> f $ HCons (Labeled @"_0" outRef) (HCons (Labeled @"_1" datum) HNil)

{- | Count the number of spend plutus scripts executed in the transaction via the txInfoRedeemers list.
  Assumes that the txInfoRedeemers list is sorted according to the ledger Ord instance for PlutusPurpose:
  `deriving instance Ord (ConwayPlutusPurpose AsIx era)`
https://github.com/IntersectMBO/cardano-ledger/blob/d79d41e09da6ab93067acddf624d1a540a3e4e8d/eras/conway/impl/src/Cardano/Ledger/Conway/Scripts.hs#L188
-}
pcountSpendRedeemers :: forall {s :: S}. Term s (AssocMap.PMap 'AssocMap.Unsorted PScriptPurpose PRedeemer) -> Term s PInteger
pcountSpendRedeemers rdmrs =
    let go :: Term _ (PInteger :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer)) :--> PInteger)
        go = pfix #$ plam $ \self n -> 
              pelimList 
                (\x xs -> 
                  let constrPair :: Term s (PAsData PScriptPurpose)
                      constrPair = pfstBuiltin # x
                      constrIdx = pfstBuiltin # (pasConstr # (pforgetData constrPair))
                  in pif (constrIdx #== 1) (self # (n + 1) # xs) n 
                )
                n
     in go # 0 # pto rdmrs

ptryFromInlineDatum :: forall (s :: S). Term s (POutputDatum :--> PDatum)
ptryFromInlineDatum = phoistAcyclic $
  plam $
    flip pmatch $ \case
      POutputDatum ((pfield @"outputDatum" #) -> datum) -> datum
      _ -> ptraceInfoError "not an inline datum"

pfromPDatum ::
  forall (a :: S -> Type) (s :: S).
  PTryFrom PData a =>
  Term s (PDatum :--> a)
pfromPDatum = phoistAcyclic $ plam $ flip ptryFrom fst . pto

pnonew :: forall {a :: PType} {b :: PType} {s :: S}.
                ((PInner a :: PType) ~ (PDataNewtype b :: PType), PIsData b) =>
                Term s a -> Term s b
pnonew nt = pmatch (pto nt) $ \(PDataNewtype bs) -> pfromData bs  

punnew :: forall {b :: PType} {s :: S}.
                PIsData b =>
                Term s (PDataNewtype b) -> Term s b
punnew nt = pmatch nt $ \(PDataNewtype bs) -> pfromData bs

data PTriple (a :: PType) (b :: PType) (c :: PType) (s :: S)
  = PTriple (Term s a) (Term s b) (Term s c)
  deriving stock (Generic)
  deriving anyclass (PlutusType, PEq, PShow)

instance DerivePlutusType (PTriple a b c) where type DPTStrat _ = PlutusTypeScott

ppair :: Term s a -> Term s b -> Term s (PPair a b)
ppair a b = pcon (PPair a b)

{- | If the input is True then continue otherwise throw an error message.
   Short trace is a sequence of first letters of long trace words.
-}
passert ::
  forall (s :: S) (a :: PType).
  T.Text -> -- long trace
  Term s PBool ->
  Term s a ->
  Term s a
passert longErrorMsg b inp = pif b inp $ ptraceInfoError (pconstant longErrorMsg)

-- | If the input is True then returns PJust otherwise PNothing
pcheck :: forall (s :: S) (a :: PType). Term s PBool -> Term s a -> Term s (PMaybe a)
pcheck b inp = pif b (pcon $ PJust inp) (pcon PNothing)

{- | Finds the associated Currency symbols that contain token
  names prefixed with the ByteString.
-}
pfindCurrencySymbolsByTokenPrefix ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PByteString
        :--> PBuiltinList (PAsData PCurrencySymbol)
    )
pfindCurrencySymbolsByTokenPrefix = phoistAcyclic $
  plam $ \value prefix ->
    plet (pisPrefixOf # prefix) $ \prefixCheck ->
      let mapVal = pto (pto value)
          isPrefixed = pfilter # plam (\csPair -> 
            pany # plam (\tkPair -> 
              pmatch (pto (pfromData $ pfstBuiltin # tkPair)) $ \(PDataNewtype tkn) ->
                  prefixCheck # pfromData tkn 
              ) # (pto $ pfromData (psndBuiltin # csPair))
            ) # mapVal 
       in pmap # pfstBuiltin # isPrefixed

pcountScriptInputs :: Term s (PBuiltinList PTxInInfo :--> PInteger)
pcountScriptInputs =
  phoistAcyclic $
    let go :: Term s (PInteger :--> PBuiltinList PTxInInfo :--> PInteger)
        go = pfix #$ plam $ \self n -> 
              pelimList 
                (\x xs -> 
                  let cred = pfield @"credential" # (pfield @"address" # (pfield @"resolved" # x))
                   in pmatch cred $ \case 
                        PScriptCredential _ -> self # (n + 1) # xs
                        _ -> self # n # xs
                )
                n
     in go # 0

pfoldl2 ::
  (PListLike listA, PListLike listB, PElemConstraint listA a, PElemConstraint listB b) =>
  Term s ((acc :--> a :--> b :--> acc) :--> acc :--> listA a :--> listB b :--> acc)
pfoldl2 =
  phoistAcyclic $ plam $ \func ->
    pfix #$ plam $ \self acc la lb ->
      pelimList
        ( \a as ->
            pelimList
              (\b bs -> self # (func # acc # a # b) # as # bs)
              perror
              lb
        )
        (pif (pnull # lb) acc perror)
        la

pelemAtWithRest' :: PListLike list => PElemConstraint list a => Term s (PInteger :--> list a :--> PPair a (list a))
pelemAtWithRest' = phoistAcyclic $
  pfix #$ plam $ \self n xs ->
    pif
      (n #== 0)
      (pcon $ PPair (phead # xs) (ptail # xs))
      (self # (n - 1) #$ ptail # xs)

pmapIdxs ::
  (PListLike listB, PElemConstraint listB b) =>
  Term s (PBuiltinList (PAsData PInteger) :--> listB b :--> listB b)
pmapIdxs =
  phoistAcyclic $
    pfix #$ plam $ \self la lb ->
      pelimList
        ( \a as -> P.do
            PPair foundEle xs <- pmatch $ pelemAtWithRest' # pfromData a # lb
            (pcons # foundEle # (self # as # xs))
        )
        pnil
        la

{- | Finds the associated Currency symbols that contain the given token
  name.
-}
pfindCurrencySymbolsByTokenName ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PTokenName
        :--> PBuiltinList (PAsData PCurrencySymbol)
    )
pfindCurrencySymbolsByTokenName = phoistAcyclic $
  plam $ \value tn ->
      let mapVal = pto (pto value)
          hasTn = pfilter # plam (\csPair -> pany # plam (\tk -> tn #== (pfromData (pfstBuiltin # tk))) # (pto $ pfromData (psndBuiltin # csPair))) # mapVal
       in pmap # pfstBuiltin # hasTn

pmapFilter :: 
  (PIsListLike list a, PElemConstraint list b) => Term s ((b :--> PBool) :--> (a :--> b) :-->  list a :--> list b)
pmapFilter =
  phoistAcyclic $
    plam $ \predicate f ->
      precList
        ( \self x' xs -> plet (f # x') $ \x ->
            pif
              (predicate # x)
              (pcons # x # (self # xs))
              (self # xs)
        )
        (const pnil)

-- | Checks if a Currency Symbol is held within a Value
phasDataCS ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    (PAsData PCurrencySymbol :--> PValue anyOrder anyAmount :--> PBool)
phasDataCS = phoistAcyclic $
  plam $ \symbol value ->
    pany # plam (\tkPair -> (pfstBuiltin # tkPair) #== symbol) #$ pto (pto value)

phasCS ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    (PValue anyOrder anyAmount :--> PCurrencySymbol :--> PBool)
phasCS = phoistAcyclic $
  plam $ \value symbol ->
    pany # plam (\tkPair -> pfromData (pfstBuiltin # tkPair) #== symbol) #$ pto (pto value)

-- | Checks that a Value contains all the given CurrencySymbols.
pcontainsCurrencySymbols ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PBuiltinList (PAsData PCurrencySymbol)
        :--> PBool
    )
pcontainsCurrencySymbols = phoistAcyclic $
  plam $ \inValue symbols ->
    let value = pmap # pfstBuiltin #$ pto $ pto inValue
        containsCS = plam $ \cs -> pelem # cs # value
     in pall # containsCS # symbols

-- | Checks if a tokenName is prefixed by a certain ByteString
pisPrefixedWith :: ClosedTerm (PTokenName :--> PByteString :--> PBool)
pisPrefixedWith = plam $ \tn prefix ->
  pmatch (pto tn) $ \(PDataNewtype tnBS) -> pisPrefixOf # prefix # pfromData tnBS

-- | Checks if the first ByteString is a prefix of the second
pisPrefixOf :: ClosedTerm (PByteString :--> PByteString :--> PBool)
pisPrefixOf = plam $ \prefix src ->
  let prefixLength = plengthBS # prefix
      prefix' = psliceBS # 0 # prefixLength # src
   in prefix' #== prefix

tcexpectJust :: forall r (a :: PType) (s :: S). Term s r -> Term s (PMaybe a) -> TermCont @r s (Term s a)
tcexpectJust escape ma = tcont $ \f -> pmatch ma $ \case
  PJust v -> f v
  PNothing -> escape

paysToAddress :: Term s (PAddress :--> PTxOut :--> PBool)
paysToAddress = phoistAcyclic $ plam $ \adr txOut -> adr #== (pfield @"address" # txOut)

paysValueToAddress ::
  Term s (PValue 'Sorted 'Positive :--> PAddress :--> PTxOut :--> PBool)
paysValueToAddress = phoistAcyclic $
  plam $ \val adr txOut ->
    pletFields @'["address", "value"] txOut $ \txoFields ->
      txoFields.address #== adr #&& txoFields.value #== val

paysAtleastValueToAddress ::
  Term s (PValue 'Sorted 'Positive :--> PAddress :--> PTxOut :--> PBool)
paysAtleastValueToAddress = phoistAcyclic $
  plam $ \val adr txOut ->
    pletFields @'["address", "value"] txOut $ \txoFields ->
      txoFields.address #== adr #&& txoFields.value #<= val

paysToCredential :: Term s (PScriptHash :--> PTxOut :--> PBool)
paysToCredential = phoistAcyclic $
  plam $ \valHash txOut ->
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
     in pmatch txOutCred $ \case
          PScriptCredential txOutValHash -> (pfield @"_0" # txOutValHash) #== valHash
          PPubKeyCredential _ -> (pcon PFalse)

pelemAt' :: PIsListLike l a => Term s (PInteger :--> l a :--> a)
pelemAt' = phoistAcyclic $
  pfix #$ plam $ \self n xs ->
    pif
      (n #== 0)
      (phead # xs)
      (self # (n - 1) #$ ptail # xs)

pelemAtFlipped' :: PIsListLike l a => Term s (l a :--> PInteger :--> a)
pelemAtFlipped' = phoistAcyclic $
  pfix #$ plam $ \self xs n ->
    pif
      (n #== 0)
      (phead # xs)
      (self # (ptail # xs) # (n - 1))

pelemAtFast :: (PIsListLike list a) => Term s (list a :--> PInteger :--> a)
pelemAtFast = phoistAcyclic $
  pfix #$ plam $ \self xs n ->
    pif
      (10 #< n)
      ( self
          # ( ptail
                #$ ptail
                #$ ptail
                #$ ptail
                #$ ptail
                #$ ptail
                #$ ptail
                #$ ptail
                #$ ptail
                #$ ptail
                # xs
            )
          # (n - 10)
      )
      ( pif
          (5 #< n)
          (self # (ptail #$ ptail #$ ptail #$ ptail #$ ptail # xs) # (n - 5))
          (pelemAtFlipped' # xs # n)
      )

pmapMaybe ::
  forall (list :: PType -> PType) (a :: PType) (b :: PType).
  PListLike list =>
  PElemConstraint list a =>
  PElemConstraint list b =>
  ClosedTerm ((a :--> PMaybe b) :--> list a :--> list b)
pmapMaybe =
  phoistAcyclic $
    plam $ \func ->
      precList
        ( \self x xs ->
            pmatch (func # x) $ \case
              PJust y -> (pcons # y # (self # xs))
              PNothing -> (self # xs)
        )
        (const pnil)

paysToPubKey :: Term s (PPubKeyHash :--> PTxOut :--> PBool)
paysToPubKey = phoistAcyclic $
  plam $ \pkh txOut ->
    let txOutCred = pfield @"credential" # (pfield @"address" # txOut)
     in pmatch txOutCred $ \case
          PScriptCredential _ -> pconstant False
          PPubKeyCredential pkh' -> (pfield @"_0" # pkh') #== pkh

ptryOutputToAddress :: (PIsListLike list PTxOut) => Term s (list PTxOut :--> PAddress :--> PTxOut)
ptryOutputToAddress = phoistAcyclic $
  plam $ \outs target ->
    ( pfix #$ plam $ \self xs ->
        pelimList
          ( \txo txos ->
              pif (target #== (pfield @"address" # txo)) txo (self # txos)
          )
          perror
          xs
    )
      # outs

ptryOwnOutput :: Term s (PBuiltinList (PAsData PTxOut) :--> PScriptHash :--> PTxOut)
ptryOwnOutput = phoistAcyclic $
  plam $ \outs target ->
    ( pfix #$ plam $ \self xs ->
        pelimList
          ( \txo txos ->
              pmatch (pfield @"credential" # (pfield @"address" # txo)) $ \case
                PPubKeyCredential _ -> (self # txos)
                PScriptCredential ((pfield @"_0" #) -> vh) ->
                  pif (target #== vh) (pfromData txo) (self # txos)
          )
          perror
          xs
    )
      # outs

ptryOwnInput :: Term s (PBuiltinList (PAsData PTxInInfo) :--> PTxOutRef :--> PTxOut)
ptryOwnInput = phoistAcyclic $
  plam $ \inputs ownRef ->
    precList (\self x xs -> pletFields @'["outRef", "resolved"] x $ \txInFields -> pif (ownRef #== txInFields.outRef) txInFields.resolved (self # xs)) (const perror) # inputs

pmustFind :: PIsListLike l a => Term s ((a :--> PBool) :--> l a :--> a)
pmustFind =
  phoistAcyclic $ plam $ \f -> pfix #$ plam $ \self xs -> pelimList (\y ys -> pif (f # y) y (self # ys)) perror xs

-- Get the head of the list if the list contains exactly one element, otherwise error.
pheadSingleton :: (PListLike list, PElemConstraint list a) => Term s (list a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \xs ->
    pelimList
      (pelimList (\_ _ -> ptraceInfoError "List contains more than one element."))
      (ptraceInfoError "List is empty.")
      xs

ptxSignedByPkh ::
  Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
ptxSignedByPkh = pelem

-- | Probably more effective than `plength . pflattenValue`
pcountOfUniqueTokens ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountOfUniqueTokens = phoistAcyclic $
  plam $ \val ->
    let tokensLength = plam (\pair -> pmatch (pfromData $ psndBuiltin # pair) $ \(PMap tokens) -> plength # tokens)
     in pmatch val $ \(PValue val') ->
          pmatch val' $ \(PMap csPairs) -> pfoldl # plam (\acc x -> acc + (tokensLength # x)) # 0 # csPairs

-- | Subtracts one Value from another
(#-) ::
  forall
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue 'Sorted amounts) ->
  Term s (PValue 'Sorted amounts) ->
  Term s (PValue 'Sorted 'NonZero)
a #- b = pnormalize #$ Value.punionResolvingCollisionsWith AssocMap.NonCommutative # plam (-) # a # b

pfindWithRest ::
  forall (list :: PType -> PType) (a :: PType).
  PListLike list =>
  PElemConstraint list a =>
  ClosedTerm
    ( (a :--> PBool)
        :--> list a
        :--> PPair a (list a)
    )
pfindWithRest = phoistAcyclic $
  plam $ \f ys ->
    let mcons self x xs =
          pmatch (f # x) $ \case
            PTrue -> P.do
              acc <- plam
              pcon $ PPair x (pconcat # acc # xs)
            PFalse -> P.do
              acc <- plam
              self # xs #$ pcons # x # acc
        mnil = const (ptraceInfoError "Find")
     in precList mcons mnil # ys # pnil

pcountCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountCS = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      pmatch val' $ \(PMap csPairs) ->
        plength # csPairs

pcountNonAdaCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PInteger)
pcountNonAdaCS =
  phoistAcyclic $
    let go :: Term _ (PInteger :--> PBuiltinList (PBuiltinPair (PAsData PCurrencySymbol) (PAsData (PMap keys PTokenName PInteger))) :--> PInteger)
        go = plet (pdata padaSymbol) $ \padaSymbolD ->
          pfix #$ plam $ \self n ->
            pelimList (\x xs -> pif (pfstBuiltin # x #== padaSymbolD) (self # n # xs) (self # (n + 1) # xs)) n
     in plam $ \val ->
          pmatch val $ \(PValue val') ->
            pmatch val' $ \(PMap csPairs) ->
              go # 0 # csPairs

pfirstTokenName ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PTokenName)
pfirstTokenName = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      pmatch val' $ \(PMap csPairs) ->
        pmatch (pfromData (psndBuiltin # (phead # csPairs))) $ \(PMap tokens) ->
          pfromData $ pfstBuiltin # (phead # tokens)

ptryLookupValue ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PBuiltinList (PBuiltinPair (PAsData PTokenName) (PAsData PInteger))
    )
ptryLookupValue = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  tokens
              )
              (self # xs)
        )
        (const perror)
        # pto val'

{- | Removes a currency symbol from a value 
-}
pfilterCSFromValue ::
  forall
    (anyOrder :: KeyGuarantees)
    (anyAmount :: AmountGuarantees).
  ClosedTerm
    ( PValue anyOrder anyAmount
        :--> PAsData PCurrencySymbol
        :--> PValue anyOrder anyAmount
    )
pfilterCSFromValue = phoistAcyclic $
  plam $ \value policyId ->
      let mapVal = pto (pto value)
          go = pfix #$ plam $ \self ys ->
                pelimList (\x xs -> pif (pfstBuiltin # x #== policyId) xs (pcons # x # (self # xs))) pnil ys
       in pcon (PValue $ pcon $ PMap $ go # mapVal)

psingletonOfCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PPair PTokenName PInteger
    )
psingletonOfCS = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  let tkPair = pheadSingleton # tokens
                   in (pcon (PPair (pfromData (pfstBuiltin # tkPair)) (pfromData (psndBuiltin # tkPair))))
              )
              (self # xs)
        )
        (const perror)
        # pto val'

pvalueOfOne ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PValue keys amounts
        :--> PBool
    )
pvalueOfOne = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  pfromData (psndBuiltin # (pheadSingleton # tokens)) #== 1
              )
              (self # xs)
        )
        (const (pconstant False))
        # pto val'

pvalueOfOneScott ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term
    s
    ( PCurrencySymbol
        :--> PValue keys amounts
        :--> PBool
    )
pvalueOfOneScott = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfromData (pfstBuiltin # x) #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  pfromData (psndBuiltin # (pheadSingleton # tokens)) #== 1
              )
              (self # xs)
        )
        (const (pconstant False))
        # pto val'

pfirstTokenNameWithCS ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PAsData PCurrencySymbol :--> PValue keys amounts :--> PTokenName)
pfirstTokenNameWithCS = phoistAcyclic $
  plam $ \policyId val ->
    pmatch val $ \(PValue val') ->
      precList
        ( \self x xs ->
            pif
              (pfstBuiltin # x #== policyId)
              ( pmatch (pfromData (psndBuiltin # x)) $ \(PMap tokens) ->
                  pfromData $ pfstBuiltin # (phead # tokens)
              )
              (self # xs)
        )
        (const perror)
        # pto val'

{- | @phasUTxO # oref # inputs@
  ensures that in @inputs@ there is an input having @TxOutRef@ @oref@ .
-}
phasUTxO ::
  ClosedTerm
    ( PTxOutRef
        :--> PBuiltinList PTxInInfo
        :--> PBool
    )
phasUTxO = phoistAcyclic $
  plam $ \oref inInputs ->
    pany @PBuiltinList # plam (\input -> oref #== (pfield @"outRef" # input)) # inInputs

pvalueContains ::
  ClosedTerm
    ( PValue 'Sorted 'Positive
        :--> PValue 'Sorted 'Positive
        :--> PBool
    )
pvalueContains = phoistAcyclic $
  plam $ \superset subset ->
    let forEachTN cs = plam $ \tnPair ->
          let tn = pfromData $ pfstBuiltin # tnPair
              amount = pfromData $ psndBuiltin # tnPair
           in amount #<= pvalueOf # superset # cs # tn
        forEachCS = plam $ \csPair ->
          let cs = pfromData $ pfstBuiltin # csPair
              tnMap = pto $ pfromData $ psndBuiltin # csPair
           in pall # forEachTN cs # tnMap
     in pall # forEachCS #$ pto $ pto subset

{- | Extract the token name and the amount of the given currency symbol.
Throws when the token name is not found or more than one token name is involved
Plutarch level function.
-}
ponlyAsset ::
  forall
    (keys :: KeyGuarantees)
    (amounts :: AmountGuarantees)
    (s :: S).
  Term s (PValue keys amounts :--> PTriple PCurrencySymbol PTokenName PInteger)
ponlyAsset = phoistAcyclic $
  plam $ \val ->
    pmatch val $ \(PValue val') ->
      plet (pheadSingleton # pto val') $ \valuePair ->
        pmatch (pfromData (psndBuiltin # valuePair)) $ \(PMap tokens) ->
          plet (pheadSingleton # tokens) $ \tkPair ->
            pcon (PTriple (pfromData (pfstBuiltin # valuePair)) (pfromData (pfstBuiltin # tkPair)) (pfromData (psndBuiltin # tkPair)))

pand'List :: [Term s PBool] -> Term s PBool
pand'List ts' = 
  case ts' of 
    [] -> pconstant True 
    ts -> foldl1 (\res x -> pand' # res # x) ts

pcond ::  [(Term s PBool, Term s a)] -> Term s a -> Term s a
pcond [] def = def
pcond ((cond, x) : conds) def = pif cond x $ pcond conds def

(#>) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #> b = b #< a
infix 4 #>

(#>=) :: (POrd t) => Term s t -> Term s t -> Term s PBool
a #>= b = b #<= a
infix 4 #>=

(#/=) :: (PEq t) => Term s t -> Term s t -> Term s PBool
a #/= b = pnot # (a #== b)
infix 4 #/=

pisFinite :: Term s (PInterval PPosixTime :--> PBool)
pisFinite = plam $ \i -> 
  let isFiniteFrom = pmatch (pfield @"_0" # (pfield @"from" # i)) $ \case 
        PFinite _ -> pconstant True 
        _ -> pconstant False
      isFiniteTo = pmatch (pfield @"_0" # (pfield @"to" # i)) $ \case 
        PFinite _ -> pconstant True 
        _ -> pconstant False
   in pand' # isFiniteFrom # isFiniteTo

pmapAndConvertList :: (PIsListLike listA a, PIsListLike listB b) => Term s ((a :--> b) :--> listA a :--> listB b)
pmapAndConvertList = phoistAcyclic $
  plam $ \f ->
    pfix #$ plam $ \self xs -> pelimList (\y ys -> pcons # (f # y) # (self # ys)) pnil xs 

pintToByteString :: Term s (PInteger :--> PByteString)
pintToByteString = phoistAcyclic $
  pfix #$ plam $ \self n ->
    plet
      (pquot # abs n # 10)
      ( \q ->
          plet (prem # abs n # 10) $ \r ->
            pif
              (q #== 0)
              (pshowDigit # r)
              ( plet (self # q) $ \prefix ->
                  prefix <> pshowDigit # r
              )
      )

pshowDigit :: Term s (PInteger :--> PByteString)
pshowDigit = phoistAcyclic $
  plam $ \digit ->
    pcond 
      [ ((digit #== 0), pconstant "0")
      , ((digit #== 1), pconstant "1")
      , ((digit #== 2), pconstant "2")
      , ((digit #== 3), pconstant "3")
      , ((digit #== 4), pconstant "4")
      , ((digit #== 5), pconstant "5")
      , ((digit #== 6), pconstant "6")
      , ((digit #== 7), pconstant "7")
      , ((digit #== 8), pconstant "8")
      , ((digit #== 9), pconstant "9")
      ]
      perror 

pvalidityRangeStart :: Term s (PPosixTimeRange :--> PAsData PInteger)
pvalidityRangeStart = phoistAcyclic $ plam $ \timeRange -> P.do 
  PInterval ((pfield @"from" #) -> from) <- pmatch timeRange
  PLowerBound lb <- pmatch from
  PFinite ((pfield @"_0" #) -> posixTime) <- pmatch (pfield @"_0" # lb)
  pmatch posixTime $ \(PPosixTime pt) -> pmatch pt $ \(PDataNewtype t) -> t 

pdivCeil :: (PIntegral a, PNum a) => Term s (a :--> a :--> a)
pdivCeil = phoistAcyclic $
  plam $
    \x y -> 1 + pdiv # (x - 1) # y

pisScriptCredential :: Term s (PAsData PCredential) -> Term s PBool 
pisScriptCredential cred = ((pfstBuiltin # (pasConstr # (pforgetData cred))) #== 1)

stakingWrapper :: PIsData a => Term s ((a :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pif
        (validationFunction # pfromDataImpl (punsafeCoerce redData))
        (pconstant ())
        perror

    _ -> pure perror

stakingWrapper2 :: (PIsData a, PIsData b) => Term s ((a :--> b :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper2 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pelimList
        (\(pfromDataImpl . punsafeCoerce -> a) xs -> pelimList
          (\(pfromDataImpl . punsafeCoerce -> b) _ ->
              pif (validationFunction # a # b) (pconstant ()) perror
          )
          perror
          xs
        )
        perror
        (pasList # redData)

    _ -> pure perror


stakingWrapper3 :: (PIsData a, PIsData b, PIsData c) => Term s ((a :--> b :--> c :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper3 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pelimList
        (\(pfromDataImpl . punsafeCoerce -> a) xs -> pelimList
          (\(pfromDataImpl . punsafeCoerce -> b) xs' -> pelimList
              (\(pfromDataImpl . punsafeCoerce -> c) _ ->
                pif (validationFunction # a # b # c) (pconstant ()) perror
              )
              perror
              xs'
          )
          perror
          xs
        )
        perror
        (pasList # redData)

    _ -> pure perror

stakingWrapper4 :: (PIsData a, PIsData b, PIsData c, PIsData d) =>
  Term s ((a :--> b :--> c :--> d :--> PBool) :--> PScriptContext :--> PUnit)
stakingWrapper4 = plam $ \validationFunction ctx -> unTermCont $ do
  sciptInfo <- pmatchC $ pfield @"scriptInfo" # ctx
  case sciptInfo of
    PCertifyingScript cert' -> do
      cert <- pmatchC $ pfield @"_1" # cert'
      pure $ case cert of
        PTxCertRegStaking{} -> pconstant ()
        _ -> perror

    PRewardingScript{}     -> do
      PRedeemer redData <- pmatchC $ pfield @"redeemer" # ctx
      pure $ pelimList
        (\(pfromDataImpl . punsafeCoerce -> a) xs -> pelimList
          (\(pfromDataImpl . punsafeCoerce -> b) xs' -> pelimList
              (\(pfromDataImpl . punsafeCoerce -> c) xs'' -> pelimList
                (\(pfromDataImpl . punsafeCoerce -> d) _ ->
                  pif (validationFunction # a # b # c # d) (pconstant ()) perror
                )
                perror
                xs''
              )
              perror
              xs'
          )
          perror
          xs
        )
        perror
        (pasList # redData)

    _ -> pure perror