{- |
Module      : Midgard.Validators.FraudProofs.MinFee
Description : Plutarch port of @validators/fraud-proofs/min-fee/step-0{1,2}.ak@.

The min-fee fraud proof (spec §5.1.1): a committed transaction whose fee is below
the protocol minimum.

Two validators. Step-01 binds the transaction and forwards its verified compact
form together with the fee its body declares; step-02 compares that fee against
the minimum.

=== The conviction is unreachable, in both trees

Aiken's @get_min_transaction_fee@ is a stub — @TODO: This will need execution
traces to calculate it@ — returning @0@ for every transaction, so step-02's
conclusion is @fee < 0@. No honest fee satisfies it and the family cannot
finalize.

The port reproduces the stub instead of inventing a fee model. The two trees then
agree on which transactions pass; a Plutarch validator that convicted where
Aiken's does not would be the divergence, not the fix. Compare the receipt
policy's @PublishField@ arm, which is refused outright for the same class of
reason — except that there the branch could not hold under /any/ implementation,
whereas here the missing piece is a calculation nobody has written yet. That is
why this one is reproduced rather than refused: the day the execution traces
land, the only thing that has to change is 'pgetMinTransactionFee'.

Step-01's half is real and is ported unchanged: it authenticates the transaction
against the block's counted @transactions_root@ exactly as every other family's
step-01 does.
-}
module Midgard.Validators.FraudProofs.MinFee (
  minFeeStep01Validator,
  minFeeStep02Validator,
  pnativeTxCompactToData,
) where

import Plutarch.Builtin.Data (pconstrBuiltin)
import Plutarch.LedgerApi.V3 (
  PAddress,
  PCurrencySymbol,
  PScriptContext,
  PScriptHash,
  PTxInfo (..),
 )
import Plutarch.Monadic qualified as P
import Plutarch.Prelude

import Midgard.FraudProofs.Common (pfinalize, ppassNativeTxToNextStep)
import Midgard.FraudProofs.MinFee (PStep02Args (..), PStep02State (..))
import Midgard.FraudProofs.NativeTx.Types (
  PNativeTxBodyCompact (..),
  PNativeTxCompact (..),
  PVerifiedMidgardNativeTxCompact (..),
 )
import Midgard.Validators.FraudProofs.Step (
  pdispatch,
  pexpectDatum,
  pexpectStateAs,
  pexpecting,
  pstep,
 )

{- | The @Data@ encoding of a 'PNativeTxCompact', built by hand.

@Constr 0 [Constr 0 [twelve body fields], B witness_set_hash, I validity_code]@ —
the shape an Aiken @NativeTxCompact@ serialises to, and therefore the shape an SDK
building this family's step-01 output datum must produce.

The port keeps 'PNativeTxCompact' Scott-encoded because it is produced by the
codec and consumed by the accessors inside one script everywhere else, and
Data-encoding a twelve-field record would cost execution units across every native
family to serve this one call site. This function is the whole price of that
choice: one encoder, here, rather than a heavier representation everywhere.
-}
pnativeTxCompactToData :: forall (s :: S). Term s (PNativeTxCompact :--> PData)
pnativeTxCompactToData = phoistAcyclic $
  plam $ \compact -> P.do
    PNativeTxCompact {pcompact'body, pcompact'witnessSetHash, pcompact'validityCode} <-
      pmatch compact
    PNativeTxBodyCompact
      { pbodyCompact'spendInputsHash
      , pbodyCompact'referenceInputsHash
      , pbodyCompact'outputsHash
      , pbodyCompact'fee
      , pbodyCompact'validityIntervalStart
      , pbodyCompact'validityIntervalEnd
      , pbodyCompact'requiredObserversHash
      , pbodyCompact'requiredSignersHash
      , pbodyCompact'mintHash
      , pbodyCompact'scriptIntegrityHash
      , pbodyCompact'auxiliaryDataHash
      , pbodyCompact'networkId
      } <-
      pmatch pcompact'body
    body <-
      plet $
        pconstrBuiltin
          # 0
          #$ pfields
            [ pforgetData (pdata pbodyCompact'spendInputsHash)
            , pforgetData (pdata pbodyCompact'referenceInputsHash)
            , pforgetData (pdata pbodyCompact'outputsHash)
            , pforgetData (pdata pbodyCompact'fee)
            , pforgetData (pdata pbodyCompact'validityIntervalStart)
            , pforgetData (pdata pbodyCompact'validityIntervalEnd)
            , pforgetData (pdata pbodyCompact'requiredObserversHash)
            , pforgetData (pdata pbodyCompact'requiredSignersHash)
            , pforgetData (pdata pbodyCompact'mintHash)
            , pforgetData (pdata pbodyCompact'scriptIntegrityHash)
            , pforgetData (pdata pbodyCompact'auxiliaryDataHash)
            , pforgetData (pdata pbodyCompact'networkId)
            ]
    pforgetData $
      pconstrBuiltin
        # 0
        #$ pfields
          [ pforgetData body
          , pforgetData (pdata pcompact'witnessSetHash)
          , pforgetData (pdata pcompact'validityCode)
          ]
  where
    pfields = foldr (\d acc -> pcons # d # acc) (pcon PNil)

{- | Aiken @min_fee/step_02.get_min_transaction_fee@.

A stub in Aiken — @TODO: This will need execution traces to calculate it@ — and a
stub here, returning @0@ for every transaction and thereby making step-02's
conclusion @fee < 0@. Reproduced rather than invented; see the module header.
-}
pgetMinTransactionFee :: forall (s :: S). Term s PData -> Term s PInteger
pgetMinTransactionFee _transaction = 0

{- | Aiken @validators/fraud-proofs/min-fee/step-01.ak@.

Binds the transaction and forwards its verified compact form and inline body fee.
-}
minFeeStep01Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PScriptHash -- step-02's script hash
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PAsData PScriptHash -- hub oracle
        :--> PScriptContext
        :--> PUnit
    )
minFeeStep01Validator = plam $
  \step02ValidatorScriptHash computationThreadTokenPolicyId hubOracle ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PTxInfo {ptxInfo'inputs, ptxInfo'referenceInputs, ptxInfo'outputs, ptxInfo'redeemers} <-
            pmatch txInfo
          ppassNativeTxToNextStep
            computationThreadTokenPolicyId
            hubOracle
            datum
            args
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'referenceInputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash
               _threadTokenAssetName
               _fraudProver
               _mInputStateData
               outputScriptHash
               outputStateData
               _header
               _badTxId
               badTxView -> P.do
                PVerifiedMidgardNativeTxCompact {pverified'txCompact} <- pmatch badTxView
                PNativeTxCompact {pcompact'body} <- pmatch pverified'txCompact
                PNativeTxBodyCompact {pbodyCompact'fee} <- pmatch pcompact'body
                pexpecting (outputScriptHash #== step02ValidatorScriptHash) $
                  pexpecting
                    ( outputStateData
                        #== pforgetData
                          ( pdata
                              ( pcon
                                  ( PStep02State
                                      { pstep02State'badTx =
                                          pnativeTxCompactToData # pverified'txCompact
                                      , pstep02State'badTxBodyFee = pdata pbodyCompact'fee
                                      }
                                  )
                              )
                          )
                    )
                    (pconstant True)

{- | Aiken @validators/fraud-proofs/min-fee/step-02.ak@.

The conviction that cannot be reached: the declared fee must be below the
protocol minimum, and the minimum is the stub above.
-}
minFeeStep02Validator ::
  forall (s :: S).
  Term
    s
    ( PAsData PCurrencySymbol -- fraud proof token policy
        :--> PAsData PAddress -- fraud proof token address
        :--> PAsData PCurrencySymbol -- computation thread token policy
        :--> PScriptContext
        :--> PUnit
    )
minFeeStep02Validator = plam $
  \fraudProofTokenPolicyId fraudProofTokenAddress computationThreadTokenPolicyId ctx ->
    pstep ctx $ \datum redeemer ownOutRef txInfo ->
      pdispatch @_ @PStep02Args computationThreadTokenPolicyId datum redeemer ownOutRef txInfo $
        \args -> P.do
          PStep02Args
            { pstep02Args'inputIndex
            , pstep02Args'outputIndex
            , pstep02Args'fraudProofMintRedeemerIndex
            } <-
            pmatch args
          PTxInfo {ptxInfo'inputs, ptxInfo'outputs, ptxInfo'redeemers} <- pmatch txInfo
          pfinalize
            computationThreadTokenPolicyId
            fraudProofTokenPolicyId
            fraudProofTokenAddress
            (pexpectDatum datum)
            (pfromData pstep02Args'inputIndex)
            (pfromData pstep02Args'outputIndex)
            (pfromData pstep02Args'fraudProofMintRedeemerIndex)
            ownOutRef
            (pfromData ptxInfo'inputs)
            (pfromData ptxInfo'outputs)
            (pto (pto (pfromData ptxInfo'redeemers)))
            $ \_ownScriptHash _threadTokenAssetName _fraudProver mInputStateData -> P.do
              PStep02State {pstep02State'badTx, pstep02State'badTxBodyFee} <-
                pmatch (pexpectStateAs @PStep02State mInputStateData)
              pexpecting
                ( pfromData pstep02State'badTxBodyFee
                    #< pgetMinTransactionFee pstep02State'badTx
                )
                (pconstant True)
