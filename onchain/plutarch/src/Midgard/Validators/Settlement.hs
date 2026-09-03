{- |
Module      : Midgard.Validators.Settlement
Description : Partial Plutarch port of @validators/settlement.ak@.

A settlement UTxO is the on-chain record of one merged block's four roots. It is
spawned when the state queue merges a block into the confirmed state, and
removed once an operator's resolution claim over it has matured.

Both sides are ported.
-}
module Midgard.Validators.Settlement (
  settlementMintValidator,
  settlementSpendValidator,
) where

import Plutarch.Builtin.Data (pasByteStr, pasConstr, pasInt)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Utils (PMaybeData (..))
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.LedgerApi.V3 (
  PCurrencySymbol (..),
  POutputDatum (..),
  PRedeemer,
  PScriptContext (..),
  PScriptHash,
  PScriptInfo (..),
  PScriptPurpose (..),
  PTokenName,
  PTxInInfo (..),
  PTxInfo (..),
  PTxOut (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude
import Plutarch.Unsafe (punsafeCoerce)

import DesignPatterns.SingularUtxoIndexer (poneToOne)
import Midgard.Common.Utils (
  pauthenticateInputOutputAndGetOutputDatumData,
  pgetAuthenticInputWithNftAt,
  pgetInclusiveUpperBoundOfInterval,
  pgetSpendingRedeemerDataAt,
  pgetAuthenticOutputDatumAtAddressWithNftAt,
  pgetInclusiveLowerBoundOfInterval,
  pgetRedeemerAt,
  phasSigned,
 )
import Midgard.HubOracle (PHubOracleDatum (..))
import Midgard.HubOracle qualified as Hub
import Midgard.OperatorDirectory qualified as Dir
import Midgard.OperatorDirectory.ActiveOperators qualified as Active
import Midgard.OperatorDirectory.RetiredOperators qualified as Retired
import Midgard.Scheduler qualified as Scheduler
import Midgard.Settlement (
  PEventMembershipProof,
  PEventType,
  PMintRedeemer (..),
  pvalidEventInclusion,
  PResolutionClaim (..),
  PSettlementDatum (..),
  PSpendRedeemer (..),
  pdecodeMintRedeemer,
 )

{- | Aiken @validators/settlement.ak@ — @mint@.

@Spawn@ creates a settlement alongside a state-queue merge. The settlement's
four roots are copied out of the /state queue's own/ merge redeemer rather than
supplied here, so the two cannot disagree; the NFT's asset name must be the
merged block's header hash; and the new datum must carry no resolution claim.

@Remove@ burns a settlement whose claim has matured. It requires the spend to be
using the @Resolve@ redeemer for the same settlement id, the input's datum to
actually carry a resolution claim, that claim's operator to have signed, and the
transaction's validity range to start at or after the claimed resolution time.

Two details of the original are preserved deliberately. The state-queue
redeemer is decoded positionally with an explicit constructor-and-arity check —
it arrives across a script boundary, so its ABI is authenticated before use
rather than trusted. And the presence of the optional settlement-redeemer index
inside it is required: it is what shows at least one of the block's trees is
non-empty, without which this endpoint would accept a merge of an entirely empty
block.
-}
settlementMintValidator ::
  forall (s :: S).
  Term s (PAsData PScriptHash :--> PScriptContext :--> PUnit)
settlementMintValidator = plam $ \hubOracle ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownPolicy <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PMintingScript cs -> cs
      _ -> perror
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'outputs
    , ptxInfo'referenceInputs
    , ptxInfo'mint
    , ptxInfo'redeemers
    , ptxInfo'signatories
    , ptxInfo'validRange
    } <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  mint <- plet $ pfromData ptxInfo'mint
  redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

  pif
    ( pmatch (pdecodeMintRedeemer # pto pscriptContext'redeemer) $ \case
        PSpawn
          { pspawn'settlementId
          , pspawn'outputIndex
          , pspawn'stateQueueMergeRedeemerIndex
          , pspawn'hubRefInputIndex
          } -> P.do
            PHubOracleDatum {phubOracle'stateQueue, phubOracle'settlementAddr} <-
              pmatch $
                Hub.pgetDatum
                  # referenceInputs
                  # hubOracle
                  # pfromData pspawn'hubRefInputIndex
            -- The state queue's merge redeemer, decoded positionally: tag 4 is
            -- MergeToConfirmedStateV1 and it has exactly 18 fields.
            mergeDecoded <-
              plet $
                pasConstr
                  #$ pto
                  $ pfromData
                    ( pgetRedeemerAt
                        # redeemerList
                        # pdata (pcon (PMinting phubOracle'stateQueue))
                        # pfromData pspawn'stateQueueMergeRedeemerIndex
                    )
            mergeFields <- plet $ psndBuiltin # mergeDecoded
            fieldAt <- plet $ plam (\n -> pelemAt @PBuiltinList # n # mergeFields)
            pand'List
              [ pfstBuiltin # mergeDecoded #== 4
              , plength # mergeFields #== 18
              , -- The settlement's id is the merged block's header-hash key.
                pasByteStr # (fieldAt # 0) #== pto (pfromData pspawn'settlementId)
              , -- The optional settlement-redeemer index must be present; see
                -- the module note on why absence would let an empty block pass.
                plet (pasConstr #$ fieldAt # 3) $ \optionPair ->
                  pand'List
                    [ pfstBuiltin # optionPair #== 0
                    , plength # (psndBuiltin # optionPair) #== 1
                    , -- Read for its type check; the value itself is unused,
                      -- exactly as in the Aiken original.
                      plet (pasInt #$ phead # (psndBuiltin # optionPair)) (const (pconstant True))
                    ]
              , -- The produced settlement must carry exactly the merged
                -- block's roots and no resolution claim.
                pgetAuthenticOutputDatumAtAddressWithNftAt
                  # outputs
                  # pfromData phubOracle'settlementAddr
                  # ownPolicy
                  # pspawn'settlementId
                  # pfromData pspawn'outputIndex
                  #== pforgetData
                    ( pdata
                        ( pcon
                            ( PSettlementDatum
                                { psettlement'depositsRoot = pdata (pasByteStr #$ fieldAt # 7)
                                , psettlement'withdrawalsRoot = pdata (pasByteStr #$ fieldAt # 4)
                                , psettlement'forcedTransactionsRoot =
                                    pdata (pasByteStr #$ fieldAt # 5)
                                , psettlement'transactionsRoot = pdata (pasByteStr #$ fieldAt # 6)
                                , psettlement'resolutionClaim = pcon PDNothing
                                }
                            )
                        )
                    )
              , -- Only the new settlement NFT may be minted under this policy.
                pmatch (AssocMap.plookup # pfromData ownPolicy # pto (pto mint)) $ \case
                  PNothing -> pconstant False
                  PJust tokenMap ->
                    pto (pto tokenMap)
                      #== ( psingleton
                              # (ppairDataBuiltin # pspawn'settlementId # pdata 1)
                          )
              ]
        PRemove {premove'settlementId, premove'inputIndex, premove'spendRedeemerIndex} -> P.do
          settlementInput <-
            plet $
              pgetAuthenticInputWithNftAt
                # inputs
                # ownPolicy
                # premove'settlementId
                # pfromData premove'inputIndex
          PTxInInfo {ptxInInfo'outRef, ptxInInfo'resolved} <- pmatch $ pfromData settlementInput
          PTxOut {ptxOut'datum} <- pmatch ptxInInfo'resolved
          PSettlementDatum {psettlement'resolutionClaim} <-
            pmatch $
              pmatch ptxOut'datum $ \case
                POutputDatum {poutputDatum'outputDatum} ->
                  pfromData
                    (punsafeCoerce @(PAsData PSettlementDatum) (pto poutputDatum'outputDatum))
                _ -> perror
          PResolutionClaim {presolutionClaim'resolutionTime, presolutionClaim'operator} <-
            pmatch $
              pmatch psettlement'resolutionClaim $ \case
                PDJust claim -> pfromData claim
                PDNothing -> perror
          spendRedeemer <-
            plet $
              pfromData
                ( punsafeCoerce @(PAsData PSpendRedeemer)
                    ( pto
                        ( pfromData
                            ( pgetRedeemerAt
                                # redeemerList
                                # pdata (pcon (PSpending ptxInInfo'outRef))
                                # pfromData premove'spendRedeemerIndex
                            )
                        )
                    )
                )
          pand'List
            [ pmatch spendRedeemer $ \case
                PResolve {pstlResolve'settlementId} ->
                  pstlResolve'settlementId #== premove'settlementId
                _ -> pconstant False
            , phasSigned # presolutionClaim'operator # pfromData ptxInfo'signatories
            , pfromData presolutionClaim'resolutionTime
                #<= (pgetInclusiveLowerBoundOfInterval # ptxInfo'validRange)
            ]
    )
    (pconstant ())
    perror

{- | Aiken @validators/settlement.ak@ — @spend@.

Three branches, covering a settlement's whole life after it is spawned.

@AttachResolutionClaim@ lets the currently appointed operator claim a settlement
as resolved. It requires the settlement to carry no claim yet, the operator's
signature, the scheduler to actually be pointing at that operator, and — the
load-bearing part — the resolution time to be taken from the /active operators/
set's own @UpdateBondHoldNewSettlement@ redeemer rather than from here. That is
what ties the claim's deadline to the bond hold the operator just accepted.

@DisproveResolutionClaim@ tears a claim down by exhibiting a user event the
settlement's own trees say was never resolved. It requires the disputant to be
inside the claim's resolution time, the event's inclusion to be proved against
the settlement's roots under a named verdict, and the operator to be getting
slashed in the same transaction for /this/ reason — read out of whichever
operator set holds it, not asserted here.

@Resolve@ simply requires the settlement's NFT to be burnt; the burn side does
the rest of the work.
-}
settlementSpendValidator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash
        :--> PAsData PScriptHash
        :--> PScriptContext
        :--> PUnit
    )
settlementSpendValidator = plam $ \hubOracle settlementPolicyId ctx -> P.do
  PScriptContext {pscriptContext'txInfo, pscriptContext'redeemer, pscriptContext'scriptInfo} <-
    pmatch ctx
  ownOutRef <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript outRef _ -> outRef
      _ -> perror
  ownDatum <-
    plet $ pmatch pscriptContext'scriptInfo $ \case
      PSpendingScript _ mDatum -> mDatum
      _ -> perror
  PTxInfo
    { ptxInfo'inputs
    , ptxInfo'outputs
    , ptxInfo'referenceInputs
    , ptxInfo'mint
    , ptxInfo'redeemers
    , ptxInfo'signatories
    , ptxInfo'validRange
    } <-
    pmatch pscriptContext'txInfo
  inputs <- plet $ pfromData ptxInfo'inputs
  outputs <- plet $ pfromData ptxInfo'outputs
  referenceInputs <- plet $ pfromData ptxInfo'referenceInputs
  redeemerList <- plet $ pto (pto (pfromData ptxInfo'redeemers))

  datumData <-
    plet $ pmatch ownDatum $ \case
      PDJust d -> pto (pfromData d)
      PDNothing -> perror
  datum <- plet $ pfromData (punsafeCoerce @(PAsData PSettlementDatum) datumData)
  PSettlementDatum
    { psettlement'depositsRoot
    , psettlement'withdrawalsRoot
    , psettlement'forcedTransactionsRoot
    , psettlement'transactionsRoot
    , psettlement'resolutionClaim
    } <-
    pmatch datum

  -- The settlement reproduced with a chosen resolution claim; both branches
  -- that continue the UTxO compare the output against one of these.
  withClaim <-
    plet $
      plam
        ( \claim ->
            pforgetData
              ( pdata
                  ( pcon
                      ( PSettlementDatum
                          { psettlement'depositsRoot = psettlement'depositsRoot
                          , psettlement'withdrawalsRoot = psettlement'withdrawalsRoot
                          , psettlement'forcedTransactionsRoot = psettlement'forcedTransactionsRoot
                          , psettlement'transactionsRoot = psettlement'transactionsRoot
                          , psettlement'resolutionClaim = claim
                          }
                      )
                  )
              )
        )

  pif
    ( pmatch (pfromData (punsafeCoerce @(PAsData PSpendRedeemer) (pto pscriptContext'redeemer))) $ \case
        PAttachResolutionClaim
          { pstlAttach'settlementInputIndex
          , pstlAttach'settlementOutputIndex
          , pstlAttach'hubRefInputIndex
          , pstlAttach'activeOperatorsNodeInputIndex
          , pstlAttach'activeOperatorsRedeemerIndex
          , pstlAttach'operator
          , pstlAttach'schedulerRefInputIndex
          } -> P.do
            PHubOracleDatum {phubOracle'activeOperatorsAddr, phubOracle'scheduler} <-
              pmatch $
                Hub.pgetDatum
                  # referenceInputs
                  # hubOracle
                  # pfromData pstlAttach'hubRefInputIndex
            -- The resolution time is not this script's to choose: it comes from
            -- the bond hold the active operators set just recorded.
            activeRedeemer <-
              plet $
                pfromData
                  ( punsafeCoerce @(PAsData Active.PSpendRedeemer)
                      ( pto
                          ( pfromData
                              ( pgetSpendingRedeemerDataAt
                                  # pfromData phubOracle'activeOperatorsAddr
                                  # pfromData pstlAttach'activeOperatorsNodeInputIndex
                                  # pfromData pstlAttach'activeOperatorsRedeemerIndex
                                  # inputs
                                  # redeemerList
                              )
                          )
                      )
                  )
            resolutionTime <-
              plet $ pmatch activeRedeemer $ \case
                Active.PUpdateBondHoldNewSettlement {Active.pupdateSettlement'resolutionTime} ->
                  pupdateSettlement'resolutionTime
                _ -> perror
            activeOperator <-
              plet $ pmatch activeRedeemer $ \case
                Active.PUpdateBondHoldNewSettlement {Active.pupdateSettlement'activeOperator} ->
                  pupdateSettlement'activeOperator
                _ -> perror
            pand'List
              [ pmatch psettlement'resolutionClaim $ \case
                  PDNothing -> pconstant True
                  PDJust _ -> pconstant False
              , phasSigned # pstlAttach'operator # pfromData ptxInfo'signatories
              , pstlAttach'operator #== activeOperator
              , -- The scheduler must currently appoint this operator.
                pmatch
                  ( Scheduler.pgetDatum
                      # referenceInputs
                      # phubOracle'scheduler
                      # pfromData pstlAttach'schedulerRefInputIndex
                  )
                  $ \case
                    Scheduler.PActiveOperator {Scheduler.pschedActive'operator} ->
                      pschedActive'operator #== pstlAttach'operator
                    _ -> pconstant False
              , poneToOne
                  (pfromData pstlAttach'settlementInputIndex)
                  (pfromData pstlAttach'settlementOutputIndex)
                  ownOutRef
                  inputs
                  outputs
                  -- Reproducing the input NFT at the output is what prevents
                  -- double satisfaction here.
                  (pconstant True)
                  ( \input output -> P.do
                      PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData input
                      pauthenticateInputOutputAndGetOutputDatumData
                        ptxInInfo'resolved
                        output
                        (pcurrencySymbolOf settlementPolicyId)
                        #== ( withClaim
                                #$ pcon
                                $ PDJust
                                  ( pdata
                                      ( pcon
                                          ( PResolutionClaim
                                              { presolutionClaim'resolutionTime = resolutionTime
                                              , presolutionClaim'operator = pstlAttach'operator
                                              }
                                          )
                                      )
                                  )
                            )
                  )
              ]
        PDisproveResolutionClaim
          { pstlDisprove'settlementInputIndex
          , pstlDisprove'settlementOutputIndex
          , pstlDisprove'hubRefInputIndex
          , pstlDisprove'operatorsRedeemerIndex
          , pstlDisprove'operator
          , pstlDisprove'operatorIsActive
          , pstlDisprove'unresolvedEventRefInputIndex
          , pstlDisprove'unresolvedEventAssetName
          , pstlDisprove'eventType
          , pstlDisprove'membershipProof
          , pstlDisprove'inclusionProofScriptWithdrawRedeemerIndex
          } -> P.do
            PResolutionClaim {presolutionClaim'resolutionTime, presolutionClaim'operator} <-
              pmatch $
                pmatch psettlement'resolutionClaim $ \case
                  PDJust claim -> pfromData claim
                  PDNothing -> perror
            PHubOracleDatum
              { phubOracle'activeOperators
              , phubOracle'retiredOperators
              , phubOracle'deposit
              , phubOracle'withdrawal
              , phubOracle'txOrder
              } <-
              pmatch $
                Hub.pgetDatum
                  # referenceInputs
                  # hubOracle
                  # pfromData pstlDisprove'hubRefInputIndex
            -- Whichever set holds the operator is the one that states the
            -- grounds; this only checks the two agree on who, and that the
            -- grounds are a bad settlement.
            slashingReason <-
              plet $
                pif
                  (pfromData pstlDisprove'operatorIsActive)
                  ( Dir.pcrossValidateSlashingReason
                      pstlDisprove'operator
                      phubOracle'activeOperators
                      (pfromData pstlDisprove'operatorsRedeemerIndex)
                      pactiveSlashArguments
                      redeemerList
                  )
                  ( Dir.pcrossValidateSlashingReason
                      pstlDisprove'operator
                      phubOracle'retiredOperators
                      (pfromData pstlDisprove'operatorsRedeemerIndex)
                      pretiredSlashArguments
                      redeemerList
                  )
            pand'List
              [ pstlDisprove'operator #== presolutionClaim'operator
              , -- The dispute must arrive before the claim's own deadline.
                (pgetInclusiveUpperBoundOfInterval # ptxInfo'validRange)
                  #< pfromData presolutionClaim'resolutionTime
              , pvalidEventInclusion
                  (pfromData pstlDisprove'eventType)
                  phubOracle'deposit
                  phubOracle'withdrawal
                  phubOracle'txOrder
                  (pfromData psettlement'depositsRoot)
                  (pfromData psettlement'withdrawalsRoot)
                  (pfromData psettlement'forcedTransactionsRoot)
                  (pfromData pstlDisprove'membershipProof)
                  pstlDisprove'unresolvedEventAssetName
                  (pfromData pstlDisprove'unresolvedEventRefInputIndex)
                  referenceInputs
                  redeemerList
              , pmatch (pfromData slashingReason) $ \case
                  Dir.PSlashOperatorForBadSettlement {} -> pconstant True
                  _ -> pconstant False
              , poneToOne
                  (pfromData pstlDisprove'settlementInputIndex)
                  (pfromData pstlDisprove'settlementOutputIndex)
                  ownOutRef
                  inputs
                  outputs
                  (pconstant True)
                  ( \input output -> P.do
                      PTxInInfo {ptxInInfo'resolved} <- pmatch $ pfromData input
                      -- The claim is stripped; the settlement itself survives.
                      pauthenticateInputOutputAndGetOutputDatumData
                        ptxInInfo'resolved
                        output
                        (pcurrencySymbolOf settlementPolicyId)
                        #== (withClaim # pcon PDNothing)
                  )
              ]
        PResolve {pstlResolve'settlementId} ->
          Value.pvalueOf
            # pto (pfromData ptxInfo'mint)
            # pfromData (pcurrencySymbolOf settlementPolicyId)
            # pfromData pstlResolve'settlementId
            #== (-1)
    )
    (pconstant ())
    perror

-- | A script hash reinterpreted as its policy id — the same 28 bytes.
pcurrencySymbolOf ::
  forall (s :: S). Term s (PAsData PScriptHash) -> Term s (PAsData PCurrencySymbol)
pcurrencySymbolOf h = pdata (pcon (PCurrencySymbol (pto (pfromData h))))

-- | Reads 'PSlashingArguments' out of the active set's @SlashOperator@.
pactiveSlashArguments ::
  forall (s :: S). Term s (PAsData PRedeemer) -> Term s Dir.PSlashingArguments
pactiveSlashArguments rdmr =
  pmatch (pfromData (punsafeCoerce @(PAsData Active.PMintRedeemer) (pto (pfromData rdmr)))) $ \case
    Active.PSlashOperator {Active.pactiveSlash'slashingArguments} ->
      pfromData pactiveSlash'slashingArguments
    _ -> perror

-- | Reads 'PSlashingArguments' out of the retired set's @SlashOperator@.
pretiredSlashArguments ::
  forall (s :: S). Term s (PAsData PRedeemer) -> Term s Dir.PSlashingArguments
pretiredSlashArguments rdmr =
  pmatch (pfromData (punsafeCoerce @(PAsData Retired.PMintRedeemer) (pto (pfromData rdmr)))) $ \case
    Retired.PSlashOperator {Retired.pretiredSlash'slashingArguments} ->
      pfromData pretiredSlash'slashingArguments
    _ -> perror
