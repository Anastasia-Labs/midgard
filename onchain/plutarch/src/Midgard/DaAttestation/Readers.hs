{- |
Module      : Midgard.DaAttestation.Readers
Description : Plutarch port of the authenticated-read layer of
              @validators/da-attestation.ak@.

How the attestation validator learns three things it cannot take on trust: the
current governed parameters, the state queue's policy id, and whether a proposed
attestation output is well-formed.

'pgetDaParams' carries a security property the Aiken source calls out
explicitly, and it is the reason this layer is worth isolating: it re-derives
@committee_signers_hash@ from @committee@ rather than believing the field.
Without that, every downstream consumer compares its own frozen hash against a
number nothing has checked, so a params datum carrying a /rotated/ committee
under the /pre-rotation/ hash would satisfy every such comparison while
signatures verified against the new keys. Re-deriving once, here, makes the two
fields a single fact for every caller.
-}
module Midgard.DaAttestation.Readers (
  pgetDaParams,
  pgetAuthenticatedStateQueuePolicyId,
  pvalidateInitOutput,
) where

import Plutarch.Builtin.Crypto (pblake2b_256)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  PLedgerValue,
  POutputDatum (..),
  PScriptHash,
  PTokenName,
  PTxInInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.DaAttestation (
  PDaAttestationDatum (..),
  PDaParamsDatum (..),
  pdaParamsAssetName,
  pemptyAttestedSignerBitmap,
  pstateQueueMintReferenceScriptAssetName,
 )
import Midgard.DaAttestation.Signatures (pattestationAssetName)

{- | Aiken @get_da_params@.

The authenticated governed parameters, from a reference input that must sit at
the params script's own address, hold exactly one params NFT, and carry an
inline datum with no reference script.

The last check is the one that matters most and is easy to read past:

@
expect blake2b_256(datum.committee) == datum.committee_signers_hash
@

This is the sole re-derivation site in the module, paid once per read. Every
other consumer compares hashes; only this one establishes that the hash means
what it says.
-}
pgetDaParams ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PDaParamsDatum
    )
pgetDaParams = phoistAcyclic $
  plam $ \referenceInputs daParamsPolicyId index -> P.do
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData (pelemAt # index # referenceInputs)
    PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
      pmatch ptxInInfo'resolved
    datumData <-
      plet $ pmatch ptxOut'datum $ \case
        POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
        _ -> perror
    datum <- plet $ pfromData (punsafeCoerce @(PAsData PDaParamsDatum) datumData)
    PDaParamsDatum {pdaParams'committee, pdaParams'committeeSignersHash} <- pmatch datum
    pif
      ( pand'List
          [ pmatch ptxOut'address $ \PAddress {paddress'credential} ->
              pmatch paddress'credential $ \case
                PScriptCredential h -> pscriptHashIs h daParamsPolicyId
                PPubKeyCredential _ -> pconstant False
          , pmatch ptxOut'referenceScript $ \case
              PDNothing -> pconstant True
              PDJust _ -> pconstant False
          , pquantityOf ptxOut'value daParamsPolicyId pdaParamsAssetName #== 1
          , -- The re-derivation. See the module note.
            (pblake2b_256 # pfromData pdaParams'committee)
              #== pfromData pdaParams'committeeSignersHash
          ]
      )
      datum
      perror

{- | Aiken @get_authenticated_state_queue_policy_id@.

The state queue's minting policy id, read out of a reference input's /attached
reference script/ rather than from a redeemer.

Taking it from the reference script is what makes it unforgeable: the ledger
itself computes a reference script's hash, so a caller cannot claim one policy
while supplying another. The authenticating NFT is what says /this/ reference
script is the deployment's own state-queue minter and not some other script
someone attached.
-}
pgetAuthenticatedStateQueuePolicyId ::
  forall (s :: S).
  Term
    s
    ( PBuiltinList (PAsData PTxInInfo)
        :--> PAsData PCurrencySymbol
        :--> PInteger
        :--> PAsData PCurrencySymbol
    )
pgetAuthenticatedStateQueuePolicyId = phoistAcyclic $
  plam $ \referenceInputs refScriptAuthPolicyId index -> P.do
    PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData (pelemAt # index # referenceInputs)
    PTxOut {ptxOut'value, ptxOut'referenceScript} <- pmatch ptxInInfo'resolved
    stateQueuePolicyId <-
      plet $ pmatch ptxOut'referenceScript $ \case
        PDJust h -> punsafeCoerce @(PAsData PCurrencySymbol) h
        PDNothing -> perror
    pif
      ( phasNftStrict
          ptxOut'value
          refScriptAuthPolicyId
          pstateQueueMintReferenceScriptAssetName
      )
      stateQueuePolicyId
      perror

{- | Aiken @validate_init_output@.

Checks a freshly created attestation and returns the asset name it must carry.

The datum is pinned in five ways, and the last two are what make an attestation
start from nothing: the bitmap must be empty and the count zero, so a creator
cannot mint an attestation that already claims signatures. The threshold and
committee hash are copied from the /current/ params, which is what freezes them
for the apply path to reconcile later.

The asset name is derived from the datum's own header hash rather than taken
from the redeemer, so the token cannot name a different block than the datum
does.
-}
pvalidateInitOutput ::
  forall (s :: S).
  Term
    s
    ( PTxOut
        :--> PAsData PCurrencySymbol
        :--> PDaParamsDatum
        :--> PByteString
        :--> PAsData PTokenName
    )
pvalidateInitOutput = phoistAcyclic $
  plam $ \output ownPolicyId params expectedHeaderHash -> P.do
    PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <- pmatch output
    datumData <-
      plet $ pmatch ptxOut'datum $ \case
        POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
        _ -> perror
    PDaAttestationDatum
      { pdaAttestation'headerHash
      , pdaAttestation'daThreshold
      , pdaAttestation'committeeSignersHash
      , pdaAttestation'attestedSigners
      , pdaAttestation'attestationCount
      } <-
      pmatch (pfromData (punsafeCoerce @(PAsData PDaAttestationDatum) datumData))
    PDaParamsDatum {pdaParams'daThreshold, pdaParams'committeeSignersHash} <- pmatch params
    assetName <- plet $ pattestationAssetName # pfromData pdaAttestation'headerHash
    pif
      ( pand'List
          [ pmatch ptxOut'address $ \PAddress {paddress'credential} ->
              pmatch paddress'credential $ \case
                PScriptCredential h -> pscriptHashIs h ownPolicyId
                PPubKeyCredential _ -> pconstant False
          , pmatch ptxOut'referenceScript $ \case
              PDNothing -> pconstant True
              PDJust _ -> pconstant False
          , pfromData pdaAttestation'headerHash #== expectedHeaderHash
          , pdaAttestation'daThreshold #== pdaParams'daThreshold
          , pdaAttestation'committeeSignersHash #== pdaParams'committeeSignersHash
          , -- An attestation starts from nothing.
            pfromData pdaAttestation'attestedSigners #== pemptyAttestedSignerBitmap
          , pfromData pdaAttestation'attestationCount #== 0
          , phasNftStrict ptxOut'value ownPolicyId assetName
          ]
      )
      assetName
      perror

--------------------------------------------------------------------------------
-- Small shared helpers
--------------------------------------------------------------------------------

-- | A script hash and a currency symbol are the same 28 bytes on the wire.
pscriptHashIs ::
  forall (s :: S).
  Term s (PAsData PScriptHash) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s PBool
pscriptHashIs h cs = pto (pfromData h) #== pto (pfromData cs)

{- | Aiken @assets.has_nft_strict@.

Exactly one token of this policy, under this name, at quantity one — and no
other name under the same policy. The strictness is what makes "the attestation
token" unambiguous when the value is later refunded or burnt.
-}
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

-- | Aiken @assets.quantity_of@; zero when absent.
pquantityOf ::
  forall (s :: S).
  Term s (PAsData PLedgerValue) ->
  Term s (PAsData PCurrencySymbol) ->
  Term s (PAsData PTokenName) ->
  Term s PInteger
pquantityOf value policyId tokenName =
  pmatch (AssocMap.plookup # pfromData policyId # pto (pto (pfromData value))) $ \case
    PNothing -> 0
    PJust tokenMap ->
      pmatch (AssocMap.plookup # pfromData tokenName # tokenMap) $ \case
        PNothing -> 0
        PJust quantity -> quantity
