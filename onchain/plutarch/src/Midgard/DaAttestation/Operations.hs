{- |
Module      : Midgard.DaAttestation.Operations
Description : Plutarch port of the attestation-lifecycle helpers of
              @validators/da-attestation.ak@.

Gathering signatures onto an attestation, and the two shapes a burn may take.
The validator's own handlers, which dispatch to these, are a separate slice.

'pvalidateAddSignatures' is the one with substance and the one Aiken exposes as
@pub fn@. Everything it pins is about /accumulation being monotonic and
attributable/: the datum's identity fields cannot move, the value cannot move,
the new bitmap must be exactly what verifying the supplied signatures produces
from the old one, the count must be that bitmap's population, and the count must
strictly increase.

That last requirement is what makes a no-op impossible. Without it a caller
could resubmit signatures already recorded and pay a fee to change nothing —
harmless in itself, but it would also mean the count could be advanced without
new signers, which is the thing a threshold is counting.

The committee is checked twice over, and both matter. The datum's frozen
@committee_signers_hash@ must still equal the /current/ governed one, so a
rotation retires an in-progress attestation rather than letting it continue
under keys the protocol no longer trusts. And the signatures verify against
@params.committee@ — the live packed keys — not against anything the attestation
carries.
-}
module Midgard.DaAttestation.Operations (
  pgetAttestationInputDatum,
  pexpectSoleBurn,
  pvalidateRescueRefund,
  pvalidateAddSignatures,
) where

import Data.Kind (Type)
import Plutarch.Core.Internal.Builtins (pcountSetBits')
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  PLedgerValue,
  PMintValue,
  POutputDatum (..),
  PTokenName,
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Value (pvalueWithoutNft)
import Midgard.DaAttestation (
  PDaAttestationDatum (..),
  PDaParamsDatum (..),
  psignatureWitnessByteCount,
 )
import Midgard.DaAttestation.Signatures (
  pattestationAssetName,
  pattestationMessage,
  pverifyIndexedSignatures,
 )

{- | Aiken @get_attestation_input_datum@.

The attestation being spent, together with its value — the value is what the
rescue path refunds, and @has_nft_strict@ is what makes "the attestation token"
unambiguous when it does.
-}
pgetAttestationInputDatum ::
  forall (s :: S) (r :: S -> Type).
  Term s PTxOut ->
  Term s (PAsData PCurrencySymbol) ->
  (Term s PDaAttestationDatum -> Term s (PAsData PLedgerValue) -> Term s r) ->
  Term s r
pgetAttestationInputDatum output ownPolicyId k = P.do
  PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <- pmatch output
  datumData <-
    plet $ pmatch ptxOut'datum $ \case
      POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
      _ -> perror
  datum <- plet $ pfromData (punsafeCoerce @(PAsData PDaAttestationDatum) datumData)
  PDaAttestationDatum {pdaAttestation'headerHash} <- pmatch datum
  pif
    ( pand'List
        [ pmatch ptxOut'address $ \PAddress {paddress'credential} ->
            pmatch paddress'credential $ \case
              PScriptCredential h -> pto (pfromData h) #== pto (pfromData ownPolicyId)
              PPubKeyCredential _ -> pconstant False
        , pmatch ptxOut'referenceScript $ \case
            PDNothing -> pconstant True
            PDJust _ -> pconstant False
        , phasNftStrict
            ptxOut'value
            ownPolicyId
            (pattestationAssetName # pfromData pdaAttestation'headerHash)
        ]
    )
    (k datum ptxOut'value)
    perror

{- | Aiken @expect_sole_burn@.

The policy's whole token map must be exactly one pair, at quantity @-1@, under
the named asset.

Requiring the /whole map/ rather than looking up one name is what the rescue
path's duplicate and cross-attestation rejections rest on: two burns of the same
name collapse into a quantity of @-2@, and two burns of different names produce
a second pair. Either way one authorisation cannot retire two attestations.
-}
pexpectSoleBurn ::
  forall (s :: S).
  Term s (PMintValue :--> PAsData PCurrencySymbol :--> PAsData PTokenName :--> PBool)
pexpectSoleBurn = phoistAcyclic $
  plam $ \mint ownPolicyId asset ->
    pmatch (AssocMap.plookup # pfromData ownPolicyId # pto (pto mint)) $ \case
      PNothing -> perror
      PJust tokenMap ->
        plet (pto (pto tokenMap)) $ \entries ->
          pif
            ( pand'List
                [ pnot # (pnull # entries)
                , pnull # (ptail # entries)
                , pfstBuiltin # (phead # entries) #== asset
                , pfromData (psndBuiltin # (phead # entries)) #== (-1)
                ]
            )
            (pconstant True)
            perror

{- | Aiken @validate_rescue_refund@.

The attestation's entire value, less the burnt token, must land on the one
output the redeemer names — and that output must not be back at this script.

The address check is what makes a rescue actually release the Ada. Every spend
path requires the UTxO to carry its attestation token, and the token is being
burnt here, so an output returned to this script could never be spent again: the
rescue would re-strand exactly what it set out to free.
-}
pvalidateRescueRefund ::
  forall (s :: S).
  Term s PTxOut ->
  Term s (PAsData PLedgerValue) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PBool
pvalidateRescueRefund refundOutput attestationValue ownPolicyId attestationAsset = P.do
  PTxOut {ptxOut'address, ptxOut'value} <- pmatch refundOutput
  -- Aiken writes this as `merge(value, from_asset(policy, asset, -1))`. The
  -- attestation holds exactly one of that asset, so the merge drops the entry —
  -- which is precisely what `pvalueWithoutNft` does, and it is already the
  -- tested way this port removes a held NFT.
  expected <-
    plet $ pvalueWithoutNft # pto (pfromData attestationValue) # ownPolicyId # attestationAsset
  -- An `expect` chain in Aiken, so this fails rather than returning False.
  pif
    ( pand'List
        [ pmatch ptxOut'address $ \PAddress {paddress'credential} ->
            pmatch paddress'credential $ \case
              PScriptCredential h -> pnot # (pto (pfromData h) #== pto (pfromData ownPolicyId))
              PPubKeyCredential _ -> pconstant True
        , pto (pfromData ptxOut'value) #== expected
        ]
    )
    (pconstant True)
    perror

{- | Aiken @validate_add_signatures@.

Adds verified signatures to an in-progress attestation.

The input side is pinned so the caller cannot substitute a different
attestation: the datum handed to the validator must equal the one the resolved
input actually carries, and that input must hold the token its own header hash
derives.

The output side is pinned so accumulation is the /only/ thing that changes. The
address, the whole value and the three identity fields carry over untouched;
only the bitmap and the count may move, and both are determined rather than
supplied — the bitmap is whatever verifying the signatures produces from the
old one, and the count is that bitmap's population.

The signature blob must be a whole number of witnesses and non-empty, so a
caller cannot submit a truncated final signature or an empty batch.
-}
pvalidateAddSignatures ::
  forall (s :: S).
  Term s PDaAttestationDatum ->
  Term s PTxOut ->
  Term s PTxOut ->
  Term s PDaParamsDatum ->
  Term s PByteString ->
  Term s PBool
pvalidateAddSignatures inputDatum ownInput output params signatures = P.do
  PTxOut
    { ptxOut'address = inAddress
    , ptxOut'value = inValue
    , ptxOut'datum = inDatum
    , ptxOut'referenceScript = inRefScript
    } <-
    pmatch ownInput
  PTxOut
    { ptxOut'address = outAddress
    , ptxOut'value = outValue
    , ptxOut'datum = outDatum
    , ptxOut'referenceScript = outRefScript
    } <-
    pmatch output
  PDaAttestationDatum
    { pdaAttestation'headerHash = inHeaderHash
    , pdaAttestation'daThreshold = inThreshold
    , pdaAttestation'committeeSignersHash = inCommitteeHash
    , pdaAttestation'attestedSigners = inAttested
    , pdaAttestation'attestationCount = inCount
    } <-
    pmatch inputDatum
  outputDatum <-
    plet $
      pfromData
        ( punsafeCoerce @(PAsData PDaAttestationDatum) $
            pmatch outDatum $ \case
              POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
              _ -> perror
        )
  PDaAttestationDatum
    { pdaAttestation'headerHash = outHeaderHash
    , pdaAttestation'daThreshold = outThreshold
    , pdaAttestation'committeeSignersHash = outCommitteeHash
    , pdaAttestation'attestedSigners = outAttested
    , pdaAttestation'attestationCount = outCount
    } <-
    pmatch outputDatum
  PDaParamsDatum {pdaParams'committee, pdaParams'committeeSignersHash} <- pmatch params

  ownPolicyId <-
    plet $ pmatch inAddress $ \PAddress {paddress'credential} ->
      pmatch paddress'credential $ \case
        PScriptCredential h -> punsafeCoerce @(PAsData PCurrencySymbol) h
        PPubKeyCredential _ -> perror
  signaturesLen <- plet $ plengthBS # signatures
  attestedSigners <-
    plet $
      pverifyIndexedSignatures
        # signatures
        # (pattestationMessage # pfromData inHeaderHash)
        # pfromData pdaParams'committee
        # pfromData inAttested
        # 0
        # 0
        # signaturesLen
  attestationCount <- plet $ pcountSetBits' # attestedSigners

  -- Aiken writes every one of these as an `expect`, so the function returns True
  -- or fails; it never returns False. Matching that matters for an exported
  -- seam: a caller reading it as a boolean would otherwise get a different
  -- contract from the original's.
  pif
    ( pand'List
        [ -- The datum given to the validator is the one the input carries.
      pmatch inDatum $ \case
        POutputDatum {poutputDatum'outputDatum} ->
          pto poutputDatum'outputDatum #== pforgetData (pdata inputDatum)
        _ -> pconstant False
        , pmatch inRefScript $ \case
        PDNothing -> pconstant True
        PDJust _ -> pconstant False
        , phasNftStrict inValue ownPolicyId (pattestationAssetName # pfromData inHeaderHash)
        , -- Only the bitmap and count may move.
      pdata inAddress #== pdata outAddress
        , inValue #== outValue
        , pmatch outRefScript $ \case
        PDNothing -> pconstant True
        PDJust _ -> pconstant False
        , outHeaderHash #== inHeaderHash
        , outThreshold #== inThreshold
        , outCommitteeHash #== inCommitteeHash
        , -- A rotation retires an in-progress attestation rather than letting it
      -- continue under keys the protocol no longer trusts.
      pdaParams'committeeSignersHash #== inCommitteeHash
        , 0 #< signaturesLen
        , pmod # signaturesLen # psignatureWitnessByteCount #== 0
        , -- The new state is determined, not supplied.
      pfromData outAttested #== attestedSigners
        , pfromData outCount #== attestationCount
        , -- And it must be a real advance.
          pfromData inCount #< attestationCount
        ]
    )
    (pconstant True)
    perror

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

-- | Aiken @assets.has_nft_strict@.
phasNftStrict ::
  forall (s :: S).
  Term s (PAsData PLedgerValue) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PBool
phasNftStrict value policyId tokenName =
  pmatch (AssocMap.plookup # pfromData policyId # pto (pto (pfromData value))) $ \case
    PNothing -> pconstant False
    PJust tokenMap ->
      plet (pto (pto tokenMap)) $ \entries ->
        -- Lazy, and it has to be: the guard is what keeps `ptail` and `phead`
        -- off an empty list. Aiken's original is a total `match` against a
        -- one-entry value and returns False here, so a strict conjunction
        -- would fail the script where the original does not. See the
        -- strictness note in "Midgard.ValidationMerkle".
        pnot
          # (pnull # entries)
          #&& pnull # (ptail # entries)
          #&& pfstBuiltin # (phead # entries) #== tokenName
          #&& pfromData (psndBuiltin # (phead # entries)) #== 1
