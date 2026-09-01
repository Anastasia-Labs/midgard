{- |
Module      : Midgard.Validators.ComputationThread
Description : Plutarch port of @validators/computation-thread.ak@.

A computation thread is a fraud proof in progress. Proving fraud against a
Midgard block is too large a computation for one transaction, so it is split
into steps: each step is a UTxO holding a thread NFT and a working datum, spent
to produce the next. This policy mints the NFT that opens a thread and burns it
when the thread ends, either in success or in cancellation.

The policy is only three branches because almost all the work belongs elsewhere.
@Init@ is the one with substance: it establishes that a thread is being opened
against a real fraud category, over a real block, by someone entitled to the
reward. @Success@ and @BurnForCancellation@ are deliberately thin — the
conditions under which a thread may end are the business of the fraud category's
own last step, not of this policy.

What @Init@ pins is worth stating plainly, because it is what stops a thread
being opened as a lie:

  * the /category/ is a member of the fraud-proof catalogue's Merkle tree,
    proved by delegating to the @phas@ validator;
  * the /destination/ is that category's own script — the output cannot be sent
    somewhere the proof will never be checked;
  * the /token name/ is @fraud_category_id ++ fraudulent_header_hash@, so a
    thread names both what is being proved and which block it is proved against,
    and neither can be swapped later;
  * the /block/ is a real queue entry, read through the state queue's
    authenticated reader; and
  * the /prover/ signed, so the reward cannot be claimed on someone else's
    behalf.
-}
module Midgard.Validators.ComputationThread (
  computationThreadMintValidator,
) where

import Data.Kind (Type)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.V3 (
  PAddress (..),
  PCredential (..),
  PCurrencySymbol,
  POutputDatum (..),
  PRedeemer,
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PTokenName (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import Midgard.Common.Utils (
  PAssetTriplet (..),
  pgetSingleAssetFromValue,
  pgetSingleAssetFromValueApartFromAda,
  phasSigned,
  pplutarchPhas,
  pquantityOfMint,
 )
import Midgard.ComputationThread (PMintRedeemer (..), PStepDatum (..))
import Midgard.FraudProofCatalogue qualified as Catalogue
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.StateQueue (pgetBlockDatumV1)

punsafeCoerceOwnRedeemer ::
  forall (a :: S -> Type) (s :: S). Term s PRedeemer -> Term s (PAsData a)
punsafeCoerceOwnRedeemer r = punsafeCoerce (pto r)

punsafeCoerceData ::
  forall (a :: S -> Type) (s :: S). (PIsData a) => Term s PData -> Term s a
punsafeCoerceData d = pfromData (punsafeCoerce @(PAsData a) d)

pscriptHashOf ::
  forall (s :: S). Term s (PAsData PCurrencySymbol) -> Term s (PAsData PScriptHash)
pscriptHashOf cs = punsafeCoerce cs

{- | Aiken @validators/computation-thread.ak@ — @mint@.

Two parameters: the fraud-proof catalogue's script hash, and the hub oracle's.
-}
computationThreadMintValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof catalogue script hash
        :--> PAsData PCurrencySymbol -- hub oracle script hash
        :--> PScriptContext
        :--> PUnit
    )
computationThreadMintValidator = plam $
  \fraudProofCatalogueScriptHash hubOracleScriptHash ctx -> P.do
    PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
      pmatch ctx
    ownPolicyId <-
      plet $ pmatch pscriptContext'scriptInfo $ \case
        PMintingScript cs -> cs
        _ -> perror
    PTxInfo
      { ptxInfo'outputs
      , ptxInfo'referenceInputs
      , ptxInfo'redeemers
      , ptxInfo'mint
      , ptxInfo'signatories
      } <-
      pmatch pscriptContext'txInfo
    mint <- plet $ pfromData ptxInfo'mint
    redeemer <-
      plet $ pfromData (punsafeCoerceOwnRedeemer @PMintRedeemer pscriptContext'redeemer)

    pif
      ( pmatch redeemer $ \case
          PInit
            { pctInit'firstStepOutputIndex
            , pctInit'fraudCategoryId
            , pctInit'fraudCategory
            , pctInit'fraudCategoryMembershipProof
            , pctInit'fraudProofCatalogueRefInputIndex
            , pctInit'hubOracleRefInputIndex
            , pctInit'fraudulentBlockRefInputIndex
            } -> P.do
              referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
              redeemers <- plet $ pto (pto (pfromData ptxInfo'redeemers))
              fraudCategoryId <- plet $ pfromData pctInit'fraudCategoryId
              fraudCategory <- plet $ pto (pfromData pctInit'fraudCategory)

              -- 1, 2. The category must be in the catalogue's Merkle tree. The
              -- membership proof is not checked here: it is delegated to the
              -- `phas` validator, and what this establishes is that the
              -- arguments `phas` proved are the ones meant here.
              catalogueRoot <-
                plet $
                  Catalogue.pgetDatum
                    # referenceInputs
                    # fraudProofCatalogueScriptHash
                    # pfromData pctInit'fraudProofCatalogueRefInputIndex
              categoryIsCatalogued <-
                plet $
                  pplutarchPhas
                    catalogueRoot
                    fraudCategoryId
                    fraudCategory
                    (pforgetData pctInit'fraudCategoryMembershipProof)
                    redeemers

              -- 3, 4. The block must be a real queue entry, under the policy
              -- the hub oracle names.
              PHubOracleDatum {phubOracle'stateQueue} <-
                pmatch
                  ( Hub.pgetDatum
                      # referenceInputs
                      # pscriptHashOf hubOracleScriptHash
                      # pfromData pctInit'hubOracleRefInputIndex
                  )

              pgetBlockDatumV1
                referenceInputs
                phubOracle'stateQueue
                (pfromData pctInit'fraudulentBlockRefInputIndex)
                $ \_fraudulentHeader fraudulentHeaderHash -> P.do
                  -- 5. The output must sit at a script address, with an inline
                  -- datum and no reference script.
                  PTxOut {ptxOut'address, ptxOut'value, ptxOut'datum, ptxOut'referenceScript} <-
                    pmatch $
                      pfromData
                        (pelemAt # pfromData pctInit'firstStepOutputIndex # pfromData ptxInfo'outputs)
                  PAddress {paddress'credential} <- pmatch ptxOut'address
                  outputScriptHash <-
                    plet $ pmatch paddress'credential $ \case
                      PScriptCredential h -> pto (pfromData h)
                      PPubKeyCredential _ -> perror
                  outputDatumData <-
                    plet $ pmatch ptxOut'datum $ \case
                      POutputDatum {poutputDatum'outputDatum} -> pto poutputDatum'outputDatum
                      _ -> perror
                  _ <-
                    plet $
                      pmatch ptxOut'referenceScript $ \case
                        PDNothing -> pconstant @PUnit ()
                        PDJust _ -> perror

                  -- 7. The thread's token name names both the category and the
                  -- block, so a thread cannot later be re-pointed at either.
                  expectedAssetName <-
                    plet $ pdata (pcon (PTokenName (fraudCategoryId <> fraudulentHeaderHash)))
                  outputAsset <-
                    plet $ pgetSingleAssetFromValueApartFromAda # pfromData ptxOut'value
                  mintedAsset <- plet $ pgetSingleAssetFromValue # mint

                  -- 8. The first step has computed nothing yet, so its datum
                  -- carries the prover and nothing else.
                  PStepDatum {pstep'fraudProver, pstep'data} <-
                    pmatch (punsafeCoerceData @PStepDatum outputDatumData)

                  pand'List
                    [ categoryIsCatalogued
                    , -- 6. The output goes to the proven category's own script,
                      -- so the proof cannot be parked somewhere unchecked.
                      outputScriptHash #== fraudCategory
                    , pisTriplet outputAsset ownPolicyId expectedAssetName 1
                    , pmatch pstep'data $ \case
                        PDNothing -> pconstant True
                        PDJust _ -> pconstant False
                    , -- 9. Only the prover named in the datum can open a thread
                      -- that will pay the prover named in the datum.
                      phasSigned # pstep'fraudProver # pfromData ptxInfo'signatories
                    , -- 10. Nothing else is minted alongside.
                      pisTriplet mintedAsset ownPolicyId expectedAssetName 1
                    ]
          ------------------------------------------------------------------
          PSuccess {pctSuccess'burningTokenAssetName} ->
            -- Deliberately thin: whether the thread earned its success is
            -- decided by the fraud category's last step, which runs in the same
            -- transaction. This only confirms the token is going away.
            pquantityOfMint # mint # ownPolicyId # pctSuccess'burningTokenAssetName
              #== (-1)
          ------------------------------------------------------------------
          PBurnForCancellation {pctBurnForCancellation'burningTokenAssetName} ->
            -- Stricter than @Success@: the whole mint field must be exactly this
            -- one burn. A cancellation earns nothing, so nothing may ride along
            -- with it — in particular no fraud-proof token.
            pmatch (pgetSingleAssetFromValue # mint) $
              \PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} ->
                pand'List
                  [ passetTriplet'policy #== ownPolicyId
                  , passetTriplet'name #== pctBurnForCancellation'burningTokenAssetName
                  , pfromData passetTriplet'amount #== (-1)
                  ]
      )
      (pconstant ())
      perror
  where
    pisTriplet triplet policy name amount =
      pmatch triplet $
        \PAssetTriplet {passetTriplet'policy, passetTriplet'name, passetTriplet'amount} ->
          pand'List
            [ passetTriplet'policy #== policy
            , passetTriplet'name #== name
            , pfromData passetTriplet'amount #== amount
            ]
