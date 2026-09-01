{- |
Module      : Midgard.Validators.DaAttestation
Description : Plutarch port of @validators/da-attestation.ak@ — the handlers.

The attestation's own mint and spend, dispatching to the three layers ported
separately: "Midgard.DaAttestation.Signatures",
"Midgard.DaAttestation.Readers" and "Midgard.DaAttestation.Operations".

An attestation's life is: @Init@ creates it empty against a committed block;
@AddSignatures@ accumulates committee signatures onto it; and it ends either by
being applied to the state queue, or — if it can no longer be applied — by being
rescued so its Ada is not locked forever.

/Applied and rescuable are exact complements, and that is the design./ An
attestation freezes both governed values at @Init@. Applying requires both to
still match the current parameters; rescuing requires /either/ to have moved.
The Aiken source is emphatic that this must be the full complement rather than
just the committee hash: governance may change @da_threshold@ over an unchanged
committee, and such an attestation could then never apply — the threshold no
longer matches — and never be rescued if the condition only inverted the
committee, while @AddSignatures@ went on accepting signatures that could never
amount to anything. Its Ada would be locked for good.

The complement also means widening the rescue cannot strip an attestation still
in flight: whenever the rescue condition holds, the apply gate is unsatisfiable
however many further signatures arrive.
-}
module Midgard.Validators.DaAttestation (
  daAttestationMintValidator,
  daAttestationSpendValidator,
  daAttestationValidator,
  pvalidateBurnBinding,
) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  PRedeemer,
  PScriptContext (..),
  PScriptInfo (..),
  PScriptPurpose (..),
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
  PTxOutRef,
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetRedeemerAt)
import Midgard.DaAttestation (
  PDaAttestationDatum (..),
  PDaParamsDatum (..),
  PMintRedeemer (..),
  PSpendRedeemer (..),
 )
import Midgard.DaAttestation.Operations (
  pexpectSoleBurn,
  pgetAttestationInputDatum,
  pvalidateAddSignatures,
  pvalidateRescueRefund,
 )
import Midgard.DaAttestation.Readers (
  pgetAuthenticatedStateQueuePolicyId,
  pgetDaParams,
  pvalidateInitOutput,
 )
import Midgard.DaAttestation.Signatures (pattestationAssetName)
import Midgard.StateQueue (
  PStateQueueNode (..),
  pgetStateQueueNode,
  pnoDaAttestation,
  pvalidateDaAttestationAttachment,
 )

punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer r = punsafeCoerce (pto r)

punsafeCoerceRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s (PAsData PRedeemer) -> Term s (PAsData a)
punsafeCoerceRedeemer r = punsafeCoerce (pto (pfromData r))

presolvedAt ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s PInteger ->
  Term s PTxOut
presolvedAt inputs index =
  pmatch (pfromData (pelemAt # index # inputs)) $
    \PTxInInfo {ptxInInfo'resolved} -> ptxInInfo'resolved

{- | Aiken @validate_burn_binding@.

Resolves the mint redeemer a burn-side spend defers to, and requires it to name
/this very input/.

@expectRescue@ selects which mint constructor is acceptable. That selection is
the point: without it a @BurnForStateQueue@ could be satisfied by a rescue
authorisation, and a @BurnForRescue@ by an apply — so an attestation could be
destroyed under conditions neither branch actually checked.
-}
pvalidateBurnBinding ::
  forall (s :: S).
  Term s (PBuiltinList (PAsData PTxInInfo)) ->
  Term s (PBuiltinList (PBuiltinPair (PAsData PScriptPurpose) (PAsData PRedeemer))) ->
  Term s PTxOutRef ->
  Term s PInteger ->
  Term s PBool ->
  Term s PBool
pvalidateBurnBinding inputs redeemers ownRef mintRedeemerIndex expectRescue = P.do
  ownInput <-
    plet $
      pmatch
        ( pfoldr
            # plam
              ( \input found ->
                  pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} ->
                    pif (ptxInInfo'outRef #== ownRef) (pcon (PJust ptxInInfo'resolved)) found
              )
            # pcon PNothing
            # inputs
        )
        $ \case
          PNothing -> perror
          PJust resolved -> resolved
  ownPolicyId <-
    plet $ pmatch ownInput $ \PTxOut {ptxOut'address} ->
      pmatch ptxOut'address $ \PAddress {paddress'credential} ->
        pmatch paddress'credential $ \case
          PScriptCredential h -> punsafeCoerce @(PAsData PCurrencySymbol) h
          PPubKeyCredential _ -> perror
  boundInputIndex <-
    plet $
      pmatch
        ( pfromData
            ( punsafeCoerceRedeemer @PMintRedeemer $
                pgetRedeemerAt
                  # redeemers
                  # pdata (pcon (PMinting ownPolicyId))
                  # mintRedeemerIndex
            )
        )
        $ \case
          PApplyToStateQueue {papply'daAttestationInputIndex} ->
            pif (pnot # expectRescue) (pfromData papply'daAttestationInputIndex) perror
          PRescueStrandedAttestation {prescue'daAttestationInputIndex} ->
            pif expectRescue (pfromData prescue'daAttestationInputIndex) perror
          -- A burn redeemer deferring to Init would be authorising destruction
          -- with a creation.
          PInit {} -> perror
  pif
    ( pmatch (pfromData (pelemAt # boundInputIndex # inputs)) $
        \PTxInInfo {ptxInInfo'outRef} -> ptxInInfo'outRef #== ownRef
    )
    (pconstant True)
    perror

{- | Aiken @validators/da-attestation.ak@ — @mint@.

Two parameters: the DA params policy and the reference-script authenticating
policy.
-}
daAttestationMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- DA params policy id
        :--> PAsData PCurrencySymbol -- reference script auth policy id
        :--> PScriptContext
        :--> PUnit
    )
daAttestationMintValidator = plam $ \daParamsPolicyId refScriptAuthPolicyId ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownPolicyId <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'mint} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  mint <- plet $ pfromData ptxInfo'mint
  redeemer <-
    plet $ pfromData (punsafeCoerceOwnRedeemer @PMintRedeemer pscriptContext'redeemer)

  pif
    ( pmatch redeemer $ \case
        ------------------------------------------------------------------
        PInit
          { pdaInit'outputIndex
          , pdaInit'daParamsRefInputIndex
          , pdaInit'stateQueueRefInputIndex
          , pdaInit'stateQueueMintRefScriptInputIndex
          } -> P.do
            params <-
              plet $
                pgetDaParams
                  # referenceInputs
                  # daParamsPolicyId
                  # pfromData pdaInit'daParamsRefInputIndex
            stateQueuePolicyId <-
              plet $
                pgetAuthenticatedStateQueuePolicyId
                  # referenceInputs
                  # refScriptAuthPolicyId
                  # pfromData pdaInit'stateQueueMintRefScriptInputIndex
            -- The block must exist and must not already carry an attestation,
            -- so one block cannot accumulate two.
            pgetStateQueueNode
              referenceInputs
              stateQueuePolicyId
              (pfromData pdaInit'stateQueueRefInputIndex)
              $ \node headerHash -> P.do
                PStateQueueNode {pstateQueueNode'daAttestation} <- pmatch node
                attestationAsset <-
                  plet $
                    pvalidateInitOutput
                      # pfromData (pelemAt # pfromData pdaInit'outputIndex # outputs)
                      # ownPolicyId
                      # params
                      # headerHash
                pand'List
                  [ pstateQueueNode'daAttestation #== pnoDaAttestation
                  , -- Exactly one token, and it is the one the datum derived.
                    pmintsExactly mint ownPolicyId attestationAsset 1
                  ]
        ------------------------------------------------------------------
        PApplyToStateQueue
          { papply'daAttestationInputIndex
          , papply'daParamsRefInputIndex
          , papply'stateQueueInputIndex
          , papply'stateQueueOutputIndex
          , papply'stateQueueMintRefScriptInputIndex
          } ->
            pgetAttestationInputDatum
              (presolvedAt inputs (pfromData papply'daAttestationInputIndex))
              ownPolicyId
              $ \attestationDatum _value -> P.do
                PDaAttestationDatum
                  { pdaAttestation'headerHash
                  , pdaAttestation'daThreshold
                  , pdaAttestation'committeeSignersHash
                  , pdaAttestation'attestationCount
                  } <-
                  pmatch attestationDatum
                params <-
                  plet $
                    pgetDaParams
                      # referenceInputs
                      # daParamsPolicyId
                      # pfromData papply'daParamsRefInputIndex
                PDaParamsDatum
                  { pdaParams'daThreshold = governedThreshold
                  , pdaParams'committeeSignersHash = governedCommitteeHash
                  } <-
                  pmatch params
                stateQueuePolicyId <-
                  plet $
                    pgetAuthenticatedStateQueuePolicyId
                      # referenceInputs
                      # refScriptAuthPolicyId
                      # pfromData papply'stateQueueMintRefScriptInputIndex
                pand'List
                  [ -- Both frozen values must still be the governed ones. This
                    -- is what makes committee rotation retroactive: a quorum
                    -- gathered under a rotated-out committee no longer applies.
                    pdaAttestation'committeeSignersHash #== governedCommitteeHash
                  , pdaAttestation'daThreshold #== governedThreshold
                  , pfromData pdaAttestation'daThreshold
                      #<= pfromData pdaAttestation'attestationCount
                  , pexpectSoleBurn
                      # mint
                      # ownPolicyId
                      # (pattestationAssetName # pfromData pdaAttestation'headerHash)
                  , pvalidateDaAttestationAttachment
                      inputs
                      outputs
                      stateQueuePolicyId
                      (pfromData papply'stateQueueInputIndex)
                      (pfromData papply'stateQueueOutputIndex)
                      (pfromData pdaAttestation'headerHash)
                      ownPolicyId
                  ]
        ------------------------------------------------------------------
        PRescueStrandedAttestation
          { prescue'daAttestationInputIndex
          , prescue'daParamsRefInputIndex
          , prescue'refundOutputIndex
          } ->
            pgetAttestationInputDatum
              (presolvedAt inputs (pfromData prescue'daAttestationInputIndex))
              ownPolicyId
              $ \attestationDatum attestationValue -> P.do
                PDaAttestationDatum
                  { pdaAttestation'headerHash
                  , pdaAttestation'daThreshold
                  , pdaAttestation'committeeSignersHash
                  } <-
                  pmatch attestationDatum
                params <-
                  plet $
                    pgetDaParams
                      # referenceInputs
                      # daParamsPolicyId
                      # pfromData prescue'daParamsRefInputIndex
                PDaParamsDatum
                  { pdaParams'daThreshold = governedThreshold
                  , pdaParams'committeeSignersHash = governedCommitteeHash
                  } <-
                  pmatch params
                attestationAsset <-
                  plet $ pattestationAssetName # pfromData pdaAttestation'headerHash
                pand'List
                  [ -- The exact complement of the apply gate. Either governed
                    -- value having moved is what proves strandedness — and is
                    -- the whole authorisation: no deadline, no quorum, no new
                    -- parameter to get wrong.
                    pnot
                      # ( (pdaAttestation'committeeSignersHash #== governedCommitteeHash)
                            #&& (pdaAttestation'daThreshold #== governedThreshold)
                        )
                  , pexpectSoleBurn # mint # ownPolicyId # attestationAsset
                  , pvalidateRescueRefund
                      (pfromData (pelemAt # pfromData prescue'refundOutputIndex # outputs))
                      attestationValue
                      ownPolicyId
                      attestationAsset
                  ]
    )
    (pconstant ())
    perror
  where
    -- Aiken: `expect [Pair(n, q)] = mint |> tokens(policy) |> to_pairs`.
    pmintsExactly mint policyId name quantity =
      pmatch (AssocMap.plookup # pfromData policyId # pto (pto mint)) $ \case
        PNothing -> pconstant False
        PJust tokenMap ->
          plet (pto (pto tokenMap)) $ \entries ->
            pand'List
              [ pnot # (pnull # entries)
              , pnull # (ptail # entries)
              , pfstBuiltin # (phead # entries) #== name
              , pfromData (psndBuiltin # (phead # entries)) #== quantity
              ]

{- | Aiken @validators/da-attestation.ak@ — @spend@.

@AddSignatures@ does the work; the two burn variants only bind themselves to the
mint redeemer that actually authorises the destruction, each to its own.
-}
daAttestationSpendValidator ::
  forall (s :: S).
  Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
daAttestationSpendValidator = plam $ \daParamsPolicyId ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  datum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum ->
        pmatch mDatum $ \case
          PDJust d -> pfromData (punsafeCoerce @(PAsData PDaAttestationDatum) (pto (pfromData d)))
          PDNothing -> perror
      _ -> perror
  PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'referenceInputs, ptxInfo'redeemers} <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
  redeemer <-
    plet $ pfromData (punsafeCoerceOwnRedeemer @PSpendRedeemer pscriptContext'redeemer)

  pif
    ( pmatch redeemer $ \case
        PAddSignatures {paddSigs'outputIndex, paddSigs'daParamsRefInputIndex, paddSigs'signatures} ->
          pvalidateAddSignatures
            datum
            (presolvedOwn inputs ownRef)
            (pfromData (pelemAt # pfromData paddSigs'outputIndex # pfromData ptxInfo'outputs))
            ( pgetDaParams
                # pfromData ptxInfo'referenceInputs
                # daParamsPolicyId
                # pfromData paddSigs'daParamsRefInputIndex
            )
            (pfromData paddSigs'signatures)
        PBurnForStateQueue {pburnSq'mintRedeemerIndex} ->
          pvalidateBurnBinding
            inputs
            redeemers
            ownRef
            (pfromData pburnSq'mintRedeemerIndex)
            (pconstant False)
        PBurnForRescue {pburnRescue'mintRedeemerIndex} ->
          pvalidateBurnBinding
            inputs
            redeemers
            ownRef
            (pfromData pburnRescue'mintRedeemerIndex)
            (pconstant True)
    )
    (pconstant ())
    perror
  where
    presolvedOwn inputs ownRef =
      pmatch
        ( pfoldr
            # plam
              ( \input found ->
                  pmatch (pfromData input) $ \PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} ->
                    pif (ptxInInfo'outRef #== ownRef) (pcon (PJust ptxInInfo'resolved)) found
              )
            # pcon PNothing
            # inputs
        )
        $ \case
          PNothing -> perror
          PJust resolved -> resolved

{- | The deployable Aiken validator is one multi-purpose program, so its mint
and spend handlers share a script hash. Keep the reference-script auth policy
parameter on the spend branch even though that handler does not inspect it.
-}
daAttestationValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol
        :--> PAsData PCurrencySymbol
        :--> PScriptContext
        :--> PUnit
    )
daAttestationValidator = plam $ \daParamsPolicyId refScriptAuthPolicyId ctx -> P.do
  PScriptContext {pscriptContext'scriptInfo} <- pmatch ctx
  pmatch pscriptContext'scriptInfo $ \case
    PMintingScript _ ->
      daAttestationMintValidator
        # daParamsPolicyId
        # refScriptAuthPolicyId
        # ctx
    PSpendingScript _ _ ->
      daAttestationSpendValidator
        # daParamsPolicyId
        # ctx
    _ -> perror
