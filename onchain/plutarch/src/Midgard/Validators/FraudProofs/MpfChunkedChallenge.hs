{- |
Module      : Midgard.Validators.FraudProofs.MpfChunkedChallenge
Description : Plutarch port of @validators/fraud-proofs/mpf-chunked-proof/challenge.ak@.

The multi-purpose challenge-thread validator.  Its minting arm creates or
retires the unique thread token; its spending arm verifies the published MPF
proof atomically and returns the challenge value to the recorded proof owner.
-}
module Midgard.Validators.FraudProofs.MpfChunkedChallenge (
    mpfChunkedChallengeValidator,
) where

import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
    PAddress (..),
    PCredential (..),
    PCurrencySymbol,
    PDatum,
    PMintValue,
    POutputDatum (..),
    PScriptContext (..),
    PScriptHash,
    PScriptInfo (..),
    PTxInInfo (..),
    PTxInfo (..),
    PTxOut (..),
    PTxOutRef,
 )
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (pgetSingletonAssetWithPolicy)
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.MpfChunkedProof (
    PChallengeMintRedeemer (..),
    PFinalizeProofRedeemer,
    PProofChallengeDatum (..),
    pchallengeDatumIsWellFormed,
    pchallengeMatchesHeader,
    pverifyPublishedProof,
 )
import Midgard.StateQueue (pgetBlockDatumV1)
import Midgard.UserEvents (poutRefToNonce)

pinputAt ::
    forall s. Term s (PBuiltinList (PAsData PTxInInfo)) -> Term s PInteger -> Term s PTxInInfo
pinputAt inputs index = pfromData $ pelemAt # index # inputs

poutputAt ::
    forall s. Term s (PBuiltinList (PAsData PTxOut)) -> Term s PInteger -> Term s PTxOut
poutputAt outputs index = pfromData $ pelemAt # index # outputs

pscriptPolicy :: forall s. Term s (PAsData PScriptHash) -> Term s PCurrencySymbol
pscriptPolicy scriptHash = punsafeCoerce $ pfromData scriptHash

plovelaceOf :: forall s. Term s (PAsData Value.PLedgerValue) -> Term s PInteger
plovelaceOf value =
    Value.pvalueOf
        # pto (pfromData value)
        # Value.padaSymbol
        # Value.padaToken

pinlineChallengeDatum :: forall s. Term s PTxOut -> Term s PProofChallengeDatum
pinlineChallengeDatum output =
    pmatch output $ \PTxOut{ptxOut'datum} ->
        pmatch ptxOut'datum $ \case
            POutputDatum{poutputDatum'outputDatum} ->
                pfromData $ punsafeCoerce @(PAsData PProofChallengeDatum) $ pto poutputDatum'outputDatum
            _ -> perror

pchallengeFromSpendingDatum ::
    forall s. Term s (PMaybeData PDatum) -> Term s PProofChallengeDatum
pchallengeFromSpendingDatum datum =
    pmatch datum $ \case
        PDNothing -> perror
        PDJust challengeData ->
            pfromData $ punsafeCoerce @(PAsData PProofChallengeDatum) $ pto $ pfromData challengeData

pfindOwnInput ::
    forall s. Term s (PBuiltinList (PAsData PTxInInfo)) -> Term s PTxOutRef -> Term s PTxInInfo
pfindOwnInput inputs ownRef =
    pmatch
        ( pfind
            # plam
                ( \input ->
                    pmatch (pfromData input) $ \PTxInInfo{ptxInInfo'outRef} ->
                        ptxInInfo'outRef #== ownRef
                )
            # inputs
        )
        $ \case
            PNothing -> perror
            PJust input -> pfromData input

pscriptHashOf :: forall s. Term s PAddress -> Term s (PAsData PScriptHash)
pscriptHashOf address =
    pmatch address $ \PAddress{paddress'credential} ->
        pmatch paddress'credential $ \case
            PPubKeyCredential _ -> perror
            PScriptCredential scriptHash -> scriptHash

pnoReferenceScript :: forall s. Term s PTxOut -> Term s PBool
pnoReferenceScript output =
    pmatch output $ \PTxOut{ptxOut'referenceScript} ->
        pmatch ptxOut'referenceScript $ \case
            PDNothing -> pconstant True
            PDJust _ -> pconstant False

prewardIsBoundToProofOwner ::
    forall s.
    Term s (PBuiltinList (PAsData PTxOut)) ->
    Term s PInteger ->
    Term s PByteString ->
    Term s PBool
prewardIsBoundToProofOwner outputs ownLovelace proofOwner =
    pfoldr
        # plam
            ( \outputData paid ->
                pmatch (pfromData outputData) $ \PTxOut{ptxOut'address, ptxOut'value} ->
                    pmatch ptxOut'address $ \PAddress{paddress'credential} ->
                        pmatch paddress'credential $ \case
                            PPubKeyCredential keyHash ->
                                pif
                                    (pto (pfromData keyHash) #== proofOwner)
                                    (plovelaceOf ptxOut'value + paid)
                                    paid
                            PScriptCredential _ -> paid
            )
        # 0
        # outputs
        #>= ownLovelace

pvalidateInit ::
    forall s.
    Term s (PAsData PScriptHash) ->
    Term s (PAsData PCurrencySymbol) ->
    Term s PChallengeMintRedeemer ->
    Term s PTxInfo ->
    Term s PBool
pvalidateInit hubOracleScriptHash ownPolicy redeemer txInfo =
    pmatch redeemer $ \case
        PInitChallenge
            { pinit'nonceInputIndex
            , pinit'challengeOutputIndex
            , pinit'hubOracleRefInputIndex
            , pinit'challengedBlockRefInputIndex
            , pinit'challengedRootDomain
            } -> P.do
                PTxInfo
                    { ptxInfo'inputs
                    , ptxInfo'referenceInputs
                    , ptxInfo'outputs
                    , ptxInfo'mint
                    } <-
                    pmatch txInfo
                inputs <- plet $ pfromData ptxInfo'inputs
                referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
                outputs <- plet $ pfromData ptxInfo'outputs
                PTxInInfo{ptxInInfo'outRef = nonceOutRef} <-
                    pmatch $ pinputAt inputs (pfromData pinit'nonceInputIndex)
                nonce <- plet $ poutRefToNonce # pdata nonceOutRef
                PBuiltinPair mintedAssetName mintedQuantity <-
                    pmatch $
                        pgetSingletonAssetWithPolicy
                            # pfromData ptxInfo'mint
                            # ownPolicy
                output <- plet $ poutputAt outputs (pfromData pinit'challengeOutputIndex)
                PTxOut{ptxOut'address, ptxOut'value} <- pmatch output
                outputScriptHash <- plet $ pscriptHashOf ptxOut'address
                challenge <- plet $ pinlineChallengeDatum output
                PHubOracleDatum{phubOracle'stateQueue} <-
                    pmatch $
                        Hub.pgetDatum
                            # referenceInputs
                            # hubOracleScriptHash
                            # pfromData pinit'hubOracleRefInputIndex
                pgetBlockDatumV1
                    referenceInputs
                    phubOracle'stateQueue
                    (pfromData pinit'challengedBlockRefInputIndex)
                    $ \header headerHash ->
                        pand'List
                            [ pfromData mintedAssetName #== nonce
                            , pfromData mintedQuantity #== 1
                            , pto (pfromData outputScriptHash) #== pto (pfromData ownPolicy)
                            , Value.pvalueOf
                                # pto (pfromData ptxOut'value)
                                # pfromData ownPolicy
                                # nonce
                                #== 1
                            , pnoReferenceScript output
                            , pchallengeDatumIsWellFormed # challenge
                            , pchallengeMatchesHeader
                                # challenge
                                # pfromData header
                                # headerHash
                                # pinit'challengedRootDomain
                            ]
        PBurnChallenge{pburn'challengeInputIndex} -> P.do
            PTxInfo{ptxInfo'inputs, ptxInfo'mint} <- pmatch txInfo
            input <- plet $ pinputAt (pfromData ptxInfo'inputs) (pfromData pburn'challengeInputIndex)
            PTxInInfo{ptxInInfo'resolved} <- pmatch input
            PTxOut{ptxOut'address, ptxOut'value} <- pmatch ptxInInfo'resolved
            inputScriptHash <- plet $ pscriptHashOf ptxOut'address
            PBuiltinPair burntAssetName burntQuantity <-
                pmatch $
                    pgetSingletonAssetWithPolicy
                        # pfromData ptxInfo'mint
                        # ownPolicy
            pand'List
                [ pto (pfromData inputScriptHash) #== pto (pfromData ownPolicy)
                , pfromData burntQuantity #== -1
                , Value.pvalueOf
                    # pto (pfromData ptxOut'value)
                    # pfromData ownPolicy
                    # pfromData burntAssetName
                    #== 1
                ]

pvalidateFinalize ::
    forall s.
    Term s PProofChallengeDatum ->
    Term s PFinalizeProofRedeemer ->
    Term s PTxOutRef ->
    Term s PTxInfo ->
    Term s PBool
pvalidateFinalize challenge redeemer ownRef txInfo = P.do
    PTxInfo
        { ptxInfo'inputs
        , ptxInfo'referenceInputs
        , ptxInfo'outputs
        , ptxInfo'mint
        } <-
        pmatch txInfo
    inputs <- plet $ pfromData ptxInfo'inputs
    ownInput <- plet $ pfindOwnInput inputs ownRef
    PTxInInfo{ptxInInfo'resolved} <- pmatch ownInput
    PTxOut{ptxOut'address, ptxOut'value = ownValue} <- pmatch ptxInInfo'resolved
    ownScriptHash <- plet $ pscriptHashOf ptxOut'address
    ownPolicy <- plet $ pdata $ pscriptPolicy ownScriptHash
    PBuiltinPair threadAssetName threadQuantity <-
        pmatch $
            pgetSingletonAssetWithPolicy
                # (punsafeCoerce @PMintValue $ pfromData ownValue)
                # ownPolicy
    PBuiltinPair burntAssetName burntQuantity <-
        pmatch $
            pgetSingletonAssetWithPolicy
                # pfromData ptxInfo'mint
                # ownPolicy
    PProofChallengeDatum{pchallenge'proofOwner} <- pmatch challenge
    threadInputCount <-
        plet $
            pfoldr
                # plam
                    ( \inputData count ->
                        pmatch (pfromData inputData) $ \PTxInInfo{ptxInInfo'resolved = resolved} ->
                            pmatch resolved $ \PTxOut{ptxOut'value} ->
                                pif
                                    ( Value.pvalueOf
                                        # pto (pfromData ptxOut'value)
                                        # pfromData ownPolicy
                                        # pfromData threadAssetName
                                        #> 0
                                    )
                                    (count + 1)
                                    count
                    )
                # (0 :: Term s PInteger)
                # inputs
    pand'List
        [ pfromData threadQuantity #== 1
        , burntAssetName #== threadAssetName
        , pfromData burntQuantity #== -1
        , threadInputCount #== 1
        , pverifyPublishedProof
            # pfromData ptxInfo'referenceInputs
            # challenge
            # redeemer
        , prewardIsBoundToProofOwner
            (pfromData ptxInfo'outputs)
            (plovelaceOf ownValue)
            (pfromData pchallenge'proofOwner)
        ]

-- | Aiken's single mint/spend validator, unapplied to its hub-oracle policy.
mpfChunkedChallengeValidator ::
    forall s. Term s (PAsData PCurrencySymbol :--> PScriptContext :--> PUnit)
mpfChunkedChallengeValidator = plam $ \hubOraclePolicy ctx -> P.do
    PScriptContext
        { pscriptContext'txInfo
        , pscriptContext'redeemer
        , pscriptContext'scriptInfo
        } <-
        pmatch ctx
    result <-
        plet $
            pmatch pscriptContext'scriptInfo $ \case
                PMintingScript ownPolicy ->
                    pvalidateInit
                        (punsafeCoerce hubOraclePolicy)
                        ownPolicy
                        (pfromData $ punsafeCoerce @(PAsData PChallengeMintRedeemer) $ pto pscriptContext'redeemer)
                        pscriptContext'txInfo
                PSpendingScript ownRef datum ->
                    pvalidateFinalize
                        (pchallengeFromSpendingDatum datum)
                        (pfromData $ punsafeCoerce @(PAsData PFinalizeProofRedeemer) $ pto pscriptContext'redeemer)
                        ownRef
                        pscriptContext'txInfo
                _ -> perror
    pif result (pconstant ()) perror
