{- |
Module      : Midgard.Common.Utils
Description : Plutarch port of the parts of @lib/midgard/common/utils.ak@ that
              the ported validators need so far.

This module is filled in demand-driven: a helper is ported when the first
validator that uses it is ported, so its Plutarch shape is chosen against a real
consumer rather than guessed. Anything still missing is still only in Aiken.
-}
module Midgard.Common.Utils (
  PAssetTriplet (..),
  pconstrOf,
  pzipFoldl,
  pzipFoldr,
  pvalidateMint,
  pgetSingletonAssetWithPolicy,
  pgetSingleAssetFromValueApartFromAda,
  pgetAuthenticInputOf,
  pgetAuthenticInputWithNftAt,
  pgetAuthenticInputAssetNameWithPolicyAt,
  pgetAuthenticInputDatumWithNftAt,
  pgetRedeemerAt,
  pgetSpendingRedeemerDataAt,
  pgetInlineDatumAndSpendingRedeemerDataAt,
  pquantityOfPolicyId,
  pquantityOfMint,
  phasSigned,
  pisEntirelyAfter,
  pisEntirelyBefore,
  pgetInclusiveUpperBoundOfInterval,
  pgetInclusiveLowerBoundOfInterval,
  pgetInclusiveBoundsOfAShortValidityRange,
  pgetAuthenticOutputDatumAtAddressWithNftAt,
  pauthenticateInputOutputAndGetOutputDatumData,
  pgetAuthenticInputWithPolicyAt,
  pgetAuthenticInputDatumWithPolicyAt,
  pgetUniqueWithdrawRedeemer,
  pplutarchPhasRaw,
  pplutarchPhas,
  pplutarchPexcludesRaw,
  pgetSingleAssetFromValue,
) where

import Data.Kind (Type)
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (pasConstr, pasInt)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PRedeemer,
  PScriptPurpose (..),
  PCredential (..),
  PCurrencySymbol (..),
  PMintValue,
  POutputDatum (..),
  PPubKeyHash,
  PScriptHash,
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.LedgerApi.Interval (PExtended (..), PInterval (..), PLowerBound (..), PUpperBound (..))
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (PPosixTime)
import Plutarch.LedgerApi.Value (PLedgerValue, padaSymbol)
import Plutarch.Monadic qualified as P
import Plutarch.Repr.Scott (DeriveAsScottRec (..))
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Env qualified as Env

import DesignPatterns.ValidityRangeNormalization (
  PNormalizedTimeRange (..),
  pnormalizeTimeRange,
 )

{- | The @(PolicyId, AssetName, Int)@ triplet that Aiken's
@get_single_asset_from_value_apart_from_ada@ returns.

Aiken returns a bare tuple. Plutarch has no first-class tuple of three, so this
is a Scott-encoded record: it never crosses a data boundary, so there is no
reason to pay for a data encoding.
-}
data PAssetTriplet (s :: S) = PAssetTriplet
  { passetTriplet'policy :: Term s (PAsData PCurrencySymbol)
  , passetTriplet'name :: Term s (PAsData PTokenName)
  , passetTriplet'amount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic)
  deriving (PlutusType) via (DeriveAsScottRec PAssetTriplet)

{- | The constructor tag of a data-encoded value, and its field list.

Used instead of 'pmatch' where a check only needs to know /which/ constructor is
present, or needs one field common to several constructors.

This is not a micro-optimisation, it is a correctness workaround. Plutarch
mis-compiles a 'pmatch' whose arms have identical bodies: neither arm is
selected and the wildcard is taken instead, so a valid input is silently
rejected. Aiken writes these checks as or-patterns over a shared body, which is
exactly the shape that triggers it. Reading the tag directly has one obvious
meaning and cannot regress into that shape.
-}
pconstrOf ::
  forall (s :: S) (a :: S -> Type).
  Term s (PAsData a) ->
  (Term s PInteger, Term s (PBuiltinList PData))
pconstrOf x =
  let pair = pasConstr # pforgetData x
   in (pfstBuiltin # pair, psndBuiltin # pair)

{- | Aiken @utils.zip_foldl@. Folds two lists from the head and stops as soon
as either list is exhausted.
-}
pzipFoldl ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  (PIsListLike PBuiltinList a, PIsListLike PBuiltinList b) =>
  Term
    s
    ( PBuiltinList a
        :--> PBuiltinList b
        :--> c
        :--> (a :--> b :--> c :--> c)
        :--> c
    )
pzipFoldl = phoistAcyclic $ pfix $ \self ->
  plam $ \as bs acc with ->
    pelimList
      ( \a as' ->
          pelimList
            (\b bs' -> self # as' # bs' # (with # a # b # acc) # with)
            acc
            bs
      )
      acc
      as

{- | Aiken @utils.zip_foldr@. Traverses the same shortest prefix as
'pzipFoldl', but applies the combining function from right to left.
-}
pzipFoldr ::
  forall (a :: S -> Type) (b :: S -> Type) (c :: S -> Type) (s :: S).
  (PIsListLike PBuiltinList a, PIsListLike PBuiltinList b) =>
  Term
    s
    ( PBuiltinList a
        :--> PBuiltinList b
        :--> c
        :--> (a :--> b :--> c :--> c)
        :--> c
    )
pzipFoldr = phoistAcyclic $ pfix $ \self ->
  plam $ \as bs acc with ->
    pelimList
      ( \a as' ->
          pelimList
            (\b bs' -> with # a # b # (self # as' # bs' # acc # with))
            acc
            bs
      )
      acc
      as

{- | Aiken @utils.validate_mint@.

@
let mints_policy = dict.to_pairs(tokens(mints, expected_minting_policy))
mints_policy == [Pair(expected_minting_name, expected_minting_amt)]
@

The Aiken version requires the mint field to contain, under the given policy,
/exactly/ that one token entry — not merely to contain it. That exactness is the
security property, so this compares the policy's whole token map against a
singleton list rather than doing a lookup and an amount check.

A policy absent from the mint field yields 'PNothing' here and an empty dict in
Aiken; both compare unequal to the singleton, so both return false.
-}
pvalidateMint ::
  forall (s :: S).
  Term
    s
    ( PMintValue
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PAsData PInteger
        :--> PBool
    )
pvalidateMint = phoistAcyclic $
  plam $ \mints expectedPolicy expectedName expectedAmt ->
    pmatch (AssocMap.plookup # pfromData expectedPolicy # pto (pto mints)) $ \case
      PNothing -> pconstant False
      PJust tokenMap ->
        pto (pto tokenMap)
          #== (psingleton # (ppairDataBuiltin # expectedName # expectedAmt))

{- | Aiken @utils.get_singleton_asset_with_policy@.

@
expect [p] = value |> assets.tokens(policy_id) |> dict.to_pairs
p
@

Fails unless the policy holds exactly one token name. Unlike 'pvalidateMint'
this reads the entry out rather than comparing it, so the caller decides what
the name and quantity must be.

The @Value@ here is a 'PMintValue': the only call site is the mint field.
-}
pgetSingletonAssetWithPolicy ::
  forall (s :: S).
  Term
    s
    ( PMintValue
        :--> PAsData PCurrencySymbol
        :--> PBuiltinPair (PAsData PTokenName) (PAsData PInteger)
    )
pgetSingletonAssetWithPolicy = phoistAcyclic $
  plam $ \value policyId ->
    pmatch (AssocMap.plookup # pfromData policyId # pto (pto value)) $ \case
      PNothing -> perror
      PJust tokenMap -> pheadSingleton #$ pto (pto tokenMap)

{- | Aiken @utils.get_single_asset_from_value_apart_from_ada@.

Expects exactly two entries in the value — Ada and one other — and returns the
other as a triplet. Fails otherwise.

The Aiken original deletes the Ada policy and then requires a single remaining
policy holding a single token name. The Ada symbol is the empty bytestring and
the map is sorted, so an Ada entry can only be the head; this drops it when
present, which is what @dict.delete@ does.
-}
pgetSingleAssetFromValueApartFromAda ::
  forall (s :: S). Term s (PLedgerValue :--> PAssetTriplet)
pgetSingleAssetFromValueApartFromAda = phoistAcyclic $
  plam $ \value -> P.do
    -- PLedgerValue -> PSortedValue -> PSortedMap -> PAssocMap -> the pair list.
    entries <- plet $ pto (pto (pto (pto value)))
    nonAda <-
      plet $
        pif
          (pnot # (pnull # entries) #&& pfromData (pfstBuiltin # (phead # entries)) #== padaSymbol)
          (ptail # entries)
          entries
    policyEntry <- plet $ pheadSingleton # nonAda
    tokenEntry <- plet $ pheadSingleton #$ pto (pto (pfromData (psndBuiltin # policyEntry)))
    pcon $
      PAssetTriplet
        { passetTriplet'policy = pfstBuiltin # policyEntry
        , passetTriplet'name = pfstBuiltin # tokenEntry
        , passetTriplet'amount = psndBuiltin # tokenEntry
        }

{- | Aiken @utils.get_authentic_input_of@.

For state UTxOs of validators whose NFT policy id equals their own payment
credential. Resolves the input at @input_index@, then requires:

  * the input sits at a script address whose hash is @address_script_hash@, and
  * apart from Ada it holds exactly one asset, and that asset is
    @(address_script_hash, nft_token_name, 1)@.

Indexing rather than searching is deliberate in the Aiken original: the caller
supplies the index as a redeemer hint, keeping the on-chain cost constant.
-}
pgetAuthenticInputOf ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PScriptHash
        :--> PAsData PTokenName
        :--> PInteger
        :--> PAsData PTxInInfo
    )
pgetAuthenticInputOf = phoistAcyclic $
  plam $ \inputs addressScriptHash nftTokenName inputIndex -> P.do
    foundInput <- plet $ pelemAt # inputIndex # inputs
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData foundInput
    PTxOut {ptxOut'address, ptxOut'value} <- pmatch ptxInInfo'resolved
    PAddress {paddress'credential} <- pmatch ptxOut'address
    refsScriptHash <-
      plet $ pmatch paddress'credential $ \case
        PPubKeyCredential _ -> perror
        PScriptCredential h -> h
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
    pif
      ( pand'List
          [ refsScriptHash #== addressScriptHash
          , passetTriplet'policy #== pscriptHashAsCurrencySymbol addressScriptHash
          , passetTriplet'name #== nftTokenName
          , pfromData passetTriplet'amount #== 1
          ]
      )
      foundInput
      perror

{- | Aiken @utils.get_authentic_input_with_nft_at@.

For state UTxOs identified purely by their NFT. Resolves the input at
@input_index@ and requires that, apart from Ada, it holds exactly the asset
@(nft_policy_id, nft_asset_name, 1)@.

Unlike 'pgetAuthenticInputOf' this deliberately does /not/ check the address —
the Aiken original says so explicitly. Authenticity rests on the NFT alone.
-}
pgetAuthenticInputWithNftAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PAsData PTxInInfo
    )
pgetAuthenticInputWithNftAt = phoistAcyclic $
  plam $ \inputs nftPolicyId nftAssetName inputIndex -> P.do
    foundInput <- plet $ pelemAt # inputIndex # inputs
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData foundInput
    PTxOut {ptxOut'value} <- pmatch ptxInInfo'resolved
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
    pif
      ( pand'List
          [ passetTriplet'policy #== nftPolicyId
          , passetTriplet'name #== nftAssetName
          , pfromData passetTriplet'amount #== 1
          ]
      )
      foundInput
      perror

{- | Aiken @utils.get_authentic_input_asset_name_with_policy_at@.

Resolves the input at @input_index@ and returns the asset name of its sole
non-Ada asset, requiring that asset's policy to be @nft_policy_id@ and its
quantity to be exactly one. The asset /name/ is not constrained — it is the
return value, and callers derive meaning from it (a fraud proof's asset name
carries the header hash of the block it convicts).
-}
pgetAuthenticInputAssetNameWithPolicyAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PAsData PTokenName
    )
pgetAuthenticInputAssetNameWithPolicyAt = phoistAcyclic $
  plam $ \inputs nftPolicyId inputIndex -> P.do
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData (pelemAt # inputIndex # inputs)
    PTxOut {ptxOut'value} <- pmatch ptxInInfo'resolved
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
    pif
      ( pand'List
          [ passetTriplet'policy #== nftPolicyId
          , pfromData passetTriplet'amount #== 1
          ]
      )
      passetTriplet'name
      perror

{- | Aiken @utils.get_authentic_input_datum_with_nft_at@.

Like 'pgetAuthenticInputWithNftAt', but returns the input's /inline datum/ rather
than the input itself, and fails if the datum is not inline. Authenticity again
rests on the NFT alone, not the address.
-}
pgetAuthenticInputDatumWithNftAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PData
    )
pgetAuthenticInputDatumWithNftAt = phoistAcyclic $
  plam $ \inputs nftPolicyId nftAssetName inputIndex -> P.do
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData (pelemAt # inputIndex # inputs)
    PTxOut {ptxOut'value, ptxOut'datum} <- pmatch ptxInInfo'resolved
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
    pif
      ( pand'List
          [ passetTriplet'policy #== nftPolicyId
          , passetTriplet'name #== nftAssetName
          , pfromData passetTriplet'amount #== 1
          ]
      )
      ( pmatch ptxOut'datum $ \case
          POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
          _ -> perror
      )
      perror

{- | Aiken @utils.get_redeemer_at@.

@
expect Some(redeemer_pair) = redeemers |> list.at(redeemer_index)
expect redeemer_pair.1st == expected_purpose
redeemer_pair.2nd
@

Fetches another script's redeemer by index and checks that the entry at that
index really belongs to @expected_purpose@. The index is a caller-supplied hint,
so the purpose check is what makes it safe: without it a caller could point at
any redeemer in the transaction.
-}
pgetRedeemerAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))
        :--> PAsData PScriptPurpose
        :--> PInteger
        :--> PAsData PRedeemer
    )
pgetRedeemerAt = phoistAcyclic $
  plam $ \redeemers expectedPurpose redeemerIndex -> P.do
    redeemerPair <- plet $ pelemAt # redeemerIndex # redeemers
    pif
      (pfstBuiltin # redeemerPair #== expectedPurpose)
      (psndBuiltin # redeemerPair)
      perror

{- | Aiken @utils.quantity_of_policy_id@.

@
dict.foldr(tokens(value, policyId), 0, fn(_k, v, result) { v + result })
@

Sums every token quantity under one policy, regardless of name. Returns zero for
an absent policy.
-}
pquantityOfPolicyId ::
  forall (s :: S).
  Term s (PLedgerValue :--> PAsData PCurrencySymbol :--> PInteger)
pquantityOfPolicyId = phoistAcyclic $
  plam $ \value policyId ->
    pmatch (AssocMap.plookup # pfromData policyId # pto (pto value)) $ \case
      PNothing -> 0
      PJust tokenMap ->
        pfoldr
          # plam (\entry acc -> pfromData (psndBuiltin # entry) + acc)
          # 0
          # pto (pto tokenMap)

{- | Aiken @utils.has_signed@.

Whether @key@ appears in the transaction's @extra_signatories@.
-}
phasSigned ::
  forall (s :: S).
  Term s (PAsData PPubKeyHash :--> PBuiltinList (PAsData PPubKeyHash) :--> PBool)
phasSigned = phoistAcyclic $
  plam $ \key signatories -> pelem # key # signatories

{- | Aiken @utils.get_spending_redeemer_data_at@.

Resolves the input at @input_index@, requires its payment credential to match
@script_address@'s, and returns the redeemer registered for spending it.

Only the payment credential is compared, not the whole address — a staking
credential difference must not stop a script recognising its own UTxO.
-}
pgetSpendingRedeemerDataAt ::
  forall (s :: S).
  Term
    s
    ( PAddress
        :--> PInteger
        :--> PInteger
        :--> PBuiltinList (PAsData PTxInInfo)
        :--> PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))
        :--> PAsData PRedeemer
    )
pgetSpendingRedeemerDataAt = phoistAcyclic $
  plam $ \scriptAddress inputIndex redeemerIndex inputs redeemers -> P.do
    PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <-
      pmatch $ pfromData (pelemAt # inputIndex # inputs)
    PTxOut {ptxOut'address} <- pmatch ptxInInfo'resolved
    PAddress {paddress'credential = inputCred} <- pmatch ptxOut'address
    PAddress {paddress'credential = scriptCred} <- pmatch scriptAddress
    pif
      (inputCred #== scriptCred)
      (pgetRedeemerAt # redeemers # pdata (pcon (PSpending ptxInInfo'outRef)) # redeemerIndex)
      perror

{- | Aiken @utils.get_inline_datum_and_spending_redeemer_data_at@.

'pgetSpendingRedeemerDataAt' plus the input's inline datum, returned as a pair.
Callers need both when they have to check that another script's /state/ and its
/intent/ agree — the scheduler's appointed operator and the advancing approach
in its redeemer, for instance.

Returned as a Haskell pair of terms: it is consumed immediately at every call
site, so there is no reason to build an on-chain tuple.
-}
pgetInlineDatumAndSpendingRedeemerDataAt ::
  forall (s :: S).
  Term s PAddress ->
  Term s PInteger ->
  Term s PInteger ->
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  (Term s PData, Term s (PAsData PRedeemer))
pgetInlineDatumAndSpendingRedeemerDataAt
  scriptAddress
  inputIndex
  redeemerIndex
  inputs
  redeemers =
    (datumData, redeemer)
    where
      input = pfromData (pelemAt # inputIndex # inputs)
      resolved = pmatch input $ \(PTxInInfo {ptxInInfo'resolved}) -> ptxInInfo'resolved
      outRef = pmatch input $ \(PTxInInfo {ptxInInfo'outRef}) -> ptxInInfo'outRef
      credentialsMatch =
        pmatch resolved $ \(PTxOut {ptxOut'address}) ->
          pmatch ptxOut'address $ \(PAddress {paddress'credential = inputCred}) ->
            pmatch scriptAddress $ \(PAddress {paddress'credential = scriptCred}) ->
              inputCred #== scriptCred
      datumData =
        pif
          credentialsMatch
          ( pmatch resolved $ \(PTxOut {ptxOut'datum}) ->
              pmatch ptxOut'datum $ \case
                POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
                _ -> perror
          )
          perror
      redeemer =
        pif
          credentialsMatch
          (pgetRedeemerAt # redeemers # pdata (pcon (PSpending outRef)) # redeemerIndex)
          perror

{- | Aiken @utils.get_single_asset_from_value@.

@
expect [Pair(sym, tn_qty_pairs)] = v |> assets.to_dict |> dict.to_pairs
expect [Pair(tn, qty)] = tn_qty_pairs |> dict.to_pairs
(sym, tn, qty)
@

Exactly one policy holding exactly one name. Unlike
'pgetSingleAssetFromValueApartFromAda' this does /not/ drop an Ada entry, so a
value carrying Ada alongside its NFT fails here. That is why the only caller is
the mint field, which has no Ada in it.
-}
pgetSingleAssetFromValue ::
  forall (s :: S). Term s (PMintValue :--> PAssetTriplet)
pgetSingleAssetFromValue = phoistAcyclic $
  plam $ \value -> P.do
    policyEntry <- plet $ pheadSingleton #$ pto (pto (pto (pto value)))
    tokenEntry <- plet $ pheadSingleton #$ pto (pto (pfromData (psndBuiltin # policyEntry)))
    pcon $
      PAssetTriplet
        { passetTriplet'policy = pfstBuiltin # policyEntry
        , passetTriplet'name = pfstBuiltin # tokenEntry
        , passetTriplet'amount = psndBuiltin # tokenEntry
        }

{- | Aiken @utils.plutarch_phas@.

'pplutarchPhasRaw' with the key and value CBOR-serialised first, which is the
form the @phas@ validator's redeemer carries them in. Callers holding raw bytes
rather than structured keys reach for the @raw@ variant instead; the two must
agree about the encoding or the delegated proof is checked against different
arguments than the caller meant.
-}
pplutarchPhas ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PData ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pplutarchPhas merkleRoot key value membershipProof redeemers =
  pplutarchPhasRaw
    merkleRoot
    (pserialiseData # pforgetData (pdata key))
    (pserialiseData # pforgetData (pdata value))
    membershipProof
    redeemers

{- | Aiken @assets.quantity_of@ over the mint field.

The quantity of one @(policy, name)@ pair, zero when absent. Distinct from
'pquantityOfPolicyId', which sums every name under a policy: a policy minting
two names could sum to one while the pair being asked about is not present at
all.
-}
pquantityOfMint ::
  forall (s :: S).
  Term s (PMintValue :--> PAsData PCurrencySymbol :--> PAsData PTokenName :--> PInteger)
pquantityOfMint = phoistAcyclic $
  plam $ \mints policyId tokenName ->
    pmatch (AssocMap.plookup # pfromData policyId # pto (pto mints)) $ \case
      PNothing -> 0
      PJust tokenMap ->
        pmatch (AssocMap.plookup # pfromData tokenName # tokenMap) $ \case
          PNothing -> 0
          PJust quantity -> quantity

{- | Aiken @utils.get_authentic_input_with_policy_at@.

Resolves the input at @input_index@ and requires that, apart from Ada, it holds
exactly one asset, at quantity one, under @nft_policy_id@. The asset /name/ is
unconstrained — unlike 'pgetAuthenticInputWithNftAt', which pins it too.
-}
pgetAuthenticInputWithPolicyAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PAsData PTxInInfo
    )
pgetAuthenticInputWithPolicyAt = phoistAcyclic $
  plam $ \inputs nftPolicyId inputIndex -> P.do
    foundInput <- plet $ pelemAt # inputIndex # inputs
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData foundInput
    PTxOut {ptxOut'value} <- pmatch ptxInInfo'resolved
    PAssetTriplet {passetTriplet'policy, passetTriplet'amount} <-
      pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
    pif
      (passetTriplet'policy #== nftPolicyId #&& pfromData passetTriplet'amount #== 1)
      foundInput
      perror

{- | The inline datum of the input 'pgetAuthenticInputWithPolicyAt' resolves.

Not an Aiken function: it is the body the three user-event @get_datum@ readers
repeat verbatim, factored out here rather than three times.

@
expect Input { output: Output { datum: InlineDatum(d), .. }, .. } =
  utils.get_authentic_input_with_policy_at(reference_inputs, policy, index)
@

A datum hash or no datum at all errors, matching the @expect@.
-}
pgetAuthenticInputDatumWithPolicyAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PData
    )
pgetAuthenticInputDatumWithPolicyAt = phoistAcyclic $
  plam $ \inputs nftPolicyId inputIndex -> P.do
    PTxInInfo {ptxInInfo'resolved} <-
      pmatch $ pfromData (pgetAuthenticInputWithPolicyAt # inputs # nftPolicyId # inputIndex)
    PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
    pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
      _ -> perror

{- | Aiken @utils.get_unique_withdraw_redeemer@.

The redeemer of the /one/ withdrawal by @withdraw_script_hash@ in this
transaction. Uniqueness is the point and is enforced: the merkelized-validator
pattern passes its arguments through a withdrawal redeemer, so two withdrawals
by the same script would make "the arguments" ambiguous. This is why the Aiken
original warns that each such helper may be used only once per transaction.
-}
pgetUniqueWithdrawRedeemer ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))
        :--> PByteString
        :--> PAsData PRedeemer
    )
pgetUniqueWithdrawRedeemer = phoistAcyclic $
  plam $ \redeemers withdrawScriptHash -> P.do
    matching <-
      plet $
        pfilter
          # plam
            ( \entry ->
                pmatch (pfromData (pfstBuiltin # entry)) $ \case
                  PRewarding credential ->
                    pmatch credential $ \case
                      PScriptCredential h -> pto (pfromData h) #== withdrawScriptHash
                      PPubKeyCredential _ -> pconstant False
                  _ -> pconstant False
            )
          # redeemers
    psndBuiltin # (pheadSingleton # matching)

{- | Aiken @utils.plutarch_phas_raw@.

Merkle membership is not verified here. It is delegated to the Plutarch @phas@
staking validator — the merkelized-validator pattern — which runs once in the
same transaction with the root, key, value and proof in its withdrawal redeemer.
All this does is check that those four arguments are the ones being claimed.

So the security argument is split across two scripts: @phas@ establishes that
the proof is valid for /its/ arguments, and this establishes that its arguments
are /ours/. The Aiken original notes the withdraw redeemer index is accepted and
ignored; the port keeps that, since 'pgetUniqueWithdrawRedeemer' finds the entry
by script hash instead.
-}
pplutarchPhasRaw ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PByteString ->
  Term s PData ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pplutarchPhasRaw merkleRoot keyBytes valueBytes membershipProof redeemers = P.do
  fields <-
    plet $
      pto
        ( pfromData
            ( punsafeCoerce @(PAsData (PBuiltinList PData))
                ( pto
                    ( pfromData
                        (pgetUniqueWithdrawRedeemer # redeemers # Env.pplutarchPhasValidatorHash)
                    )
                )
            )
        )
  -- Aiken uses `expect` for each of these four, so a mismatch *errors* rather
  -- than returning False. Preserved deliberately: the caller's `and` block
  -- short-circuits before reaching here when an earlier check already failed,
  -- so this is only evaluated once the claim is otherwise well-formed.
  pif
    ( pand'List
        [ (phead # fields) #== pforgetData (pdata merkleRoot)
        , (phead #$ ptail # fields) #== pforgetData (pdata keyBytes)
        , (phead #$ ptail #$ ptail # fields) #== pforgetData (pdata valueBytes)
        , (phead #$ ptail #$ ptail #$ ptail # fields) #== membershipProof
        ]
    )
    (pconstant True)
    perror

{- | Aiken @utils.plutarch_pexcludes_raw@.

The non-membership twin of 'pplutarchPhasRaw', delegating to the @pexcludes@
staking validator instead. Its redeemer carries three arguments rather than four
— there is no value to open — and the Aiken original destructures with
@[a, b, c, ..]@, so a longer redeemer list is accepted and its tail ignored.
That is reproduced here: only the first three entries are read.
-}
pplutarchPexcludesRaw ::
  forall (s :: S).
  Term s PByteString ->
  Term s PByteString ->
  Term s PData ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PBool
pplutarchPexcludesRaw merkleRoot keyBytes nonMembershipProof redeemers = P.do
  fields <-
    plet $
      pto
        ( pfromData
            ( punsafeCoerce @(PAsData (PBuiltinList PData))
                ( pto
                    ( pfromData
                        (pgetUniqueWithdrawRedeemer # redeemers # Env.pplutarchPexcludesValidatorHash)
                    )
                )
            )
        )
  -- `expect` on each, exactly as in Aiken: a mismatch errors rather than
  -- returning False.
  pif
    ( pand'List
        [ (phead # fields) #== pforgetData (pdata merkleRoot)
        , (phead #$ ptail # fields) #== pforgetData (pdata keyBytes)
        , (phead #$ ptail #$ ptail # fields) #== nonMembershipProof
        ]
    )
    (pconstant True)
    perror

{- | Aiken @interval.is_entirely_after@ (from @aiken-lang/stdlib@).

Whether the whole validity range lies strictly after @point@. An inclusive lower
bound requires @point < low@; an exclusive one allows @point <= low@. An
unbounded-below range is never entirely after anything; an unbounded-above one
(a lower bound of positive infinity) trivially is.
-}
pisEntirelyAfter ::
  forall (s :: S).
  Term s (PInterval PPosixTime :--> PInteger :--> PBool)
pisEntirelyAfter = phoistAcyclic $
  plam $ \self point -> P.do
    PInterval {pinterval'from} <- pmatch self
    PLowerBound bound closed <- pmatch pinterval'from
    pmatch bound $ \case
      PNegInf -> pconstant False
      PPosInf -> pconstant True
      PFinite low ->
        pif
          (pfromData closed)
          (point #< pto (pfromData low))
          (point #<= pto (pfromData low))

{- | Aiken @interval.is_entirely_before@ (from @aiken-lang/stdlib@).

The mirror of 'pisEntirelyAfter', reading the upper bound: an inclusive bound
requires @hi < point@, an exclusive one allows @hi <= point@. A range unbounded
above is never entirely before anything; one unbounded below trivially is.
-}
pisEntirelyBefore ::
  forall (s :: S).
  Term s (PInterval PPosixTime :--> PInteger :--> PBool)
pisEntirelyBefore = phoistAcyclic $
  plam $ \self point -> P.do
    PInterval {pinterval'to} <- pmatch self
    PUpperBound bound closed <- pmatch pinterval'to
    pmatch bound $ \case
      PNegInf -> pconstant True
      PPosInf -> pconstant False
      PFinite hi ->
        pif
          (pfromData closed)
          (pto (pfromData hi) #< point)
          (pto (pfromData hi) #<= point)

{- | Aiken @utils.get_inclusive_upper_bound_of_interval@.

@
when vrn.normalize_time_range(interval) is {
  vrn.ClosedRange { upper, .. } | vrn.FromNegInf { upper } -> upper
  _ -> fail @"Validity range was expected to be bound at upper end"
}
@

Normalising first is what makes the result inclusive regardless of how the
transaction expressed its bound. A range unbounded above has no upper bound to
report, so it fails rather than returning a sentinel — a caller deriving a
deadline from an unbounded range would be computing on nonsense.
-}
pgetInclusiveUpperBoundOfInterval ::
  forall (s :: S). Term s (PInterval PPosixTime :--> PInteger)
pgetInclusiveUpperBoundOfInterval = phoistAcyclic $
  plam $ \interval -> pnormalizedBoundAt (pnormalizeTimeRange # interval) 1 1
    -- @ClosedRange@ (tag 0) carries the upper at index 1; @FromNegInf@ is tag 1.

{- | Aiken @utils.get_inclusive_lower_bound_of_interval@.

The mirror of 'pgetInclusiveUpperBoundOfInterval'. A range unbounded below has
no lower bound to report and fails.
-}
pgetInclusiveLowerBoundOfInterval ::
  forall (s :: S). Term s (PInterval PPosixTime :--> PInteger)
pgetInclusiveLowerBoundOfInterval = phoistAcyclic $
  plam $ \interval -> pnormalizedBoundAt (pnormalizeTimeRange # interval) 0 2
    -- @ClosedRange@ (tag 0) carries the lower at index 0; @ToPosInf@ is tag 2.

{- | One bound of a normalised range, read by constructor tag rather than by
'pmatch'.

__This is a correctness workaround, not an optimisation.__ Both callers have two
arms with /identical bodies/ — "return the bound" — and Plutarch mis-compiles a
@pmatch@ of that shape on a data-encoded sum: neither arm is selected, the
wildcard is taken, and a valid range is silently rejected. See the
branch-selection hazard in the README, and 'Midgard.Validators.ActiveOperators.pconstrOf',
which is the same workaround at the other two sites.

The hazard reached this function the day @NormalizedTimeRange@ moved from
@DeriveAsSOPStruct@ to @DeriveAsDataStruct@ — a move @invalid-range@ forced,
because its step-02 state carries one in a datum. The README had recorded this
function as a candidate that was safe /only/ because of its Scott encoding, and
it failed on exactly the arm the test suite reached first.

@closedFieldIndex@ is where @ClosedRange@ (tag 0) carries the wanted bound — 0
for the lower, 1 for the upper. @singletonTag@ is the one-field constructor that
also carries it, at index 0: @FromNegInf@ (tag 1) for an upper, @ToPosInf@
(tag 2) for a lower. Every other tag has no such bound and aborts.
-}
pnormalizedBoundAt ::
  forall (s :: S).
  Term s PNormalizedTimeRange ->
  Integer ->
  Integer ->
  Term s PInteger
pnormalizedBoundAt normalized closedFieldIndex singletonTag =
  plet (pasConstr # pforgetData (pdata normalized)) $ \pair ->
    plet (pfstBuiltin # pair) $ \tag ->
      plet (psndBuiltin # pair) $ \fields ->
        pif
          (tag #== pconstant closedRangeTag)
          (pasInt #$ pfieldAt fields closedFieldIndex)
          ( pif
              (tag #== pconstant singletonTag)
              (pasInt #$ pfieldAt fields 0)
              perror
          )
  where
    closedRangeTag = 0 :: Integer
    pfieldAt fields n = phead # foldr (\_ acc -> ptail # acc) fields [1 .. n]

{- | Aiken @utils.get_inclusive_bounds_of_a_short_validity_range@.

Both bounds of a range that must be closed /and/ no wider than
@max_validity_range_length@. A block header binds its event interval's end to
the commit transaction's upper bound, so without the width cap an operator could
claim an arbitrarily wide interval for a single block.

Returned as a Haskell pair; every call site destructures it immediately.
-}
pgetInclusiveBoundsOfAShortValidityRange ::
  forall (s :: S).
  Term s (PInterval PPosixTime) ->
  (Term s PInteger, Term s PInteger)
pgetInclusiveBoundsOfAShortValidityRange interval =
  let normalized = pnormalizeTimeRange # interval
      lower = pmatch normalized $ \case
        PClosedRange l u ->
          pif
            (pfromData u - pfromData l #<= Env.pmaxValidityRangeLength)
            (pfromData l)
            perror
        _ -> perror
      upper = pmatch normalized $ \case
        PClosedRange l u ->
          pif
            (pfromData u - pfromData l #<= Env.pmaxValidityRangeLength)
            (pfromData u)
            perror
        _ -> perror
   in (lower, upper)

{- | Aiken @utils.get_authentic_output_datum_at_address_with_nft_at@.

The output at @output_index@ must sit at @expected_address@, carry no reference
script, hold exactly @(nft_policy_id, nft_asset_name, 1)@ apart from Ada, and
have an inline datum — which is returned.
-}
pgetAuthenticOutputDatumAtAddressWithNftAt ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxOut)
        :--> PAddress
        :--> PAsData PCurrencySymbol
        :--> PAsData PTokenName
        :--> PInteger
        :--> PData
    )
pgetAuthenticOutputDatumAtAddressWithNftAt = phoistAcyclic $
  plam $ \outputs expectedAddress nftPolicyId nftAssetName outputIndex -> P.do
    PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
      pmatch $ pfromData (pelemAt # outputIndex # outputs)
    PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} <-
      pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
    pif
      ( pand'List
          [ ptxOut'address #== expectedAddress
          , pmatch ptxOut'referenceScript $ \case
              PDNothing -> pconstant True
              PDJust _ -> pconstant False
          , passetTriplet'policy #== nftPolicyId
          , passetTriplet'name #== nftAssetName
          , pfromData passetTriplet'amount #== 1
          ]
      )
      ( pmatch ptxOut'datum $ \case
          POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
          _ -> perror
      )
      perror

{- | Aiken @utils.authenticate_input_output_and_get_output_datum_data@.

For a spending validator reproducing its own state UTxO: the output must be at
the same address as the input, carry no reference script, and hold the /same/
NFT — the whole triplet is compared, not just the policy — and its inline datum
is returned.

Comparing the full triplet is what keeps a script from being tricked into
reproducing a different one of its own UTxOs; the policy alone would not
distinguish them.
-}
pauthenticateInputOutputAndGetOutputDatumData ::
  forall (s :: S).
  Term s PTxOut ->
  Term s PTxOut ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PData
pauthenticateInputOutputAndGetOutputDatumData input output authenticationPolicyId = P.do
  PTxOut {ptxOut'address = inAddress, ptxOut'value = inValue} <- pmatch input
  PTxOut
    { ptxOut'address = outAddress
    , ptxOut'value = outValue
    , ptxOut'datum = outDatum
    , ptxOut'referenceScript = outRefScript
    } <-
    pmatch output
  PAssetTriplet
    { passetTriplet'policy = inPolicy
    , passetTriplet'name = inName
    , passetTriplet'amount = inAmount
    } <-
    pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData inValue
  PAssetTriplet
    { passetTriplet'policy = outPolicy
    , passetTriplet'name = outName
    , passetTriplet'amount = outAmount
    } <-
    pmatch $ pgetSingleAssetFromValueApartFromAda # pfromData outValue
  pif
    ( pand'List
        [ inAddress #== outAddress
        , pmatch outRefScript $ \case
            PDNothing -> pconstant True
            PDJust _ -> pconstant False
        , inPolicy #== authenticationPolicyId
        , pfromData inAmount #== 1
        , inPolicy #== outPolicy
        , inName #== outName
        , inAmount #== outAmount
        ]
    )
    ( pmatch outDatum $ \case
        POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
        _ -> perror
    )
    perror

{- | A script hash and a currency symbol are the same 28 bytes on the wire, and
Aiken compares them directly because both are @ByteArray@. Plutarch keeps them
as distinct newtypes, so the reinterpretation has to be written out.
-}
pscriptHashAsCurrencySymbol ::
  forall (s :: S). Term s (PAsData PScriptHash) -> Term s (PAsData PCurrencySymbol)
pscriptHashAsCurrencySymbol h = pdata (pcon (PCurrencySymbol (pto (pfromData h))))

-- | The head of a list that must have exactly one element; fails otherwise.
pheadSingleton ::
  forall (a :: S -> Type) (s :: S).
  (PIsListLike PBuiltinList a) =>
  Term s (PBuiltinList a :--> a)
pheadSingleton = phoistAcyclic $
  plam $ \ell ->
    pif
      (pnull # (ptail # ell))
      (phead # ell)
      perror
