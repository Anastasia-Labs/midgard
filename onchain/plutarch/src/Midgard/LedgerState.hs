{- |
Module      : Midgard.LedgerState
Description : Partial Plutarch port of @lib/midgard/ledger-state.ak@.

Only the hash aliases are ported so far — they are what the fraud proof modules
need. The rest of the Midgard ledger model (@MidgardLedger@, @MidgardOutput@,
the header record) is still only in Aiken.
-}
module Midgard.LedgerState (
  PHeaderHash,
  pblockMaturityDurationV1,
  PDepositId,
  PDepositInfo (..),
  PDepositEvent (..),
  PWithdrawalId,
  PWithdrawalValidity (..),
  PWithdrawalBody (..),
  PWithdrawalInfo (..),
  PWithdrawalEvent (..),
  PMidgardTxValidity (..),
  PNativeTxProofSourceV1 (..),
  PTxOrderId,
  PTxOrderPayloadV1 (..),
  PTxOrderEventV1 (..),
  PForcedInclusionTxV1 (..),
  PL2TransactionSourceV1 (..),
  PTxFieldPreimageV1 (..),
  PTxFieldReceiptV1 (..),
  PCekProgramMaterialDatumV1 (..),
  PFrontierPeak (..),
  PItemProofV1 (..),
  PChunkProofV1 (..),
  PHeaderV1 (..),
  PHeaderTransitionCommitmentsV1 (..),
  PConfirmedState (..),
  PTransitionPhase (..),
  PEventKey (..),
  PEventToStepValue (..),
  PTransitionStep (..),
  ptransitionStepSchemaVersionV1,
  ptransitionStepV1IsValid,
  pprotocolVersionV1,
  punsafeEventToIdData,
  punsafeEventToKeyValuePair,
  pgenesisProtocolVersion,
  pgenesisHeaderHash,
  pgenesisUtxoRoot,
  pgenesisConfirmedStateV1,
  pconfirmedStateNextHeaderProtocolVersionV1,
  prootMatchesCountV1,
  pheaderTransitionCommitmentsV1,
  pheaderTransitionCommitmentsV1AreValid,
  pheaderValidationContextScalarsV1AreValid,
  pheaderV1IsValid,
) where

import Data.ByteString qualified as BS
import GHC.Generics (Generic)
import Generics.SOP qualified as SOP
import Plutarch.Builtin.Data (pasConstr)
import Plutarch.Core.Utils (pand'List)
import Plutarch.LedgerApi.V3 (PAddress, PCurrencySymbol, POutputDatum, PPubKeyHash, PTxOutRef)
import Plutarch.Prelude

import Midgard.Common.Types (PH28, PMerkleRoot, PPosixTime, PValuePairs)
import Midgard.BoundedItem (PChunkProofV1 (..))
import Midgard.ValidationMerkle (PFrontierPeak)
import Midgard.Env qualified as Env

{- | Aiken @ledger_state.HeaderHash = H28<HeaderV1>@.

A transparent alias for the 28-byte hash, exactly as in Aiken — see
'Midgard.Common.Types.PH28' for why these stay synonyms.
-}
type PHeaderHash = PH28

{- | Aiken testnet @ledger_state.block_maturity_duration_v1@ — @3 * 60 * 1000@.

The Plutarch deployment currently targets the same fast Preprod E2E profile as
the Aiken @testnet@ environment. Never use this value for a production build.
-}
pblockMaturityDurationV1 :: forall (s :: S). Term s PInteger
pblockMaturityDurationV1 = 3 * 60 * 1000

{- | Aiken @ledger_state.DepositId = OutputReference@.

A deposit is identified by the UTxO whose spending created it, which is what
makes deposit ids unique without any registry.
-}
type PDepositId = PTxOutRef

-- | Aiken @ledger_state.DepositInfo@ — where the funds land on L2.
data PDepositInfo (s :: S) = PDepositInfo
  { pdepositInfo'l2Address :: Term s (PAsData PAddress)
  , pdepositInfo'l2NetworkId :: Term s (PAsData PInteger)
  , pdepositInfo'l2Datum :: Term s PData
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDepositInfo)

-- | Aiken @ledger_state.DepositEvent@.
data PDepositEvent (s :: S) = PDepositEvent
  { pdepositEvent'id :: Term s (PAsData PDepositId)
  , pdepositEvent'info :: Term s (PAsData PDepositInfo)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PDepositEvent)

{- | Aiken @ledger_state.unsafe_event_to_id_data@.

Reads the first field of any event record as raw @Data@. Like the Aiken helper,
this deliberately validates neither the constructor tag nor the remaining
fields; malformed data with no first field fails at 'phead'.
-}
punsafeEventToIdData :: forall (s :: S). Term s PData -> Term s PData
punsafeEventToIdData eventDatumData =
  phead #$ psndBuiltin # (pasConstr # eventDatumData)

{- | Aiken @ledger_state.unsafe_event_to_key_value_pair@.

@
expect [id_data, info_data, ..] = builtin.unconstr_fields(event_datum_data)
Pair(id_data, info_data)
@

Reads the first two fields of /any/ event as raw @Data@ — every event type is a
record whose first two fields are its id and its info, so the ledger's
key/value pair can be extracted without knowing which event this is. "Unsafe"
in the original's sense: nothing here checks that the payload really is an
event.

Returned as a Haskell pair; both call sites consume it immediately.
-}
punsafeEventToKeyValuePair ::
  forall (s :: S). Term s PData -> (Term s PData, Term s PData)
punsafeEventToKeyValuePair eventDatumData =
  let fields = psndBuiltin # (pasConstr # eventDatumData)
   in (phead # fields, phead #$ ptail # fields)

{- | Aiken @ledger_state.WithdrawalId = OutputReference@.

As with deposits, a withdrawal is identified by the L1 UTxO whose spending
created it.
-}
type PWithdrawalId = PTxOutRef

{- | Aiken @ledger_state.WithdrawalValidity@.

An operator's verdict on a withdrawal request. Tags: @WithdrawalIsValid@ 0,
@NonExistentWithdrawalUtxo@ 1, @SpentWithdrawalUtxo@ 2,
@IncorrectWithdrawalOwner@ 3, @IncorrectWithdrawalValue@ 4,
@IncorrectWithdrawalSignature@ 5, @TooManyTokensInWithdrawal@ 6,
@UnpayableWithdrawalValue@ 7.

All eight are present so the tags line up; only tag 0 is matched on so far.
-}
data PWithdrawalValidity (s :: S)
  = PWithdrawalIsValid
  | PNonExistentWithdrawalUtxo
  | PSpentWithdrawalUtxo {pspentWithdrawal'l2TxId :: Term s PData}
  | PIncorrectWithdrawalOwner
  | PIncorrectWithdrawalValue
  | PIncorrectWithdrawalSignature
  | PTooManyTokensInWithdrawal
  | PUnpayableWithdrawalValue
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalValidity)

{- | Aiken @ledger_state.WithdrawalBody@.

The L2 side of a withdrawal request: which L2 UTxO is being spent, by whom, for
how much, and where the funds should land on L1.
-}
data PWithdrawalBody (s :: S) = PWithdrawalBody
  { pwithdrawalBody'l2Outref :: Term s PData
  , pwithdrawalBody'l2Owner :: Term s PData
  , pwithdrawalBody'l2Value :: Term s (PAsData PValuePairs)
  , pwithdrawalBody'l1Address :: Term s (PAsData PAddress)
  , pwithdrawalBody'l1Datum :: Term s POutputDatum
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalBody)

{- | Aiken @ledger_state.WithdrawalInfo@.

@signature@ stays 'PData': no ported consumer reads it, and typing it would pull
in the L2 signature model.
-}
data PWithdrawalInfo (s :: S) = PWithdrawalInfo
  { pwithdrawalInfo'body :: Term s (PAsData PWithdrawalBody)
  , pwithdrawalInfo'signature :: Term s PData
  , pwithdrawalInfo'validity :: Term s (PAsData PWithdrawalValidity)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalInfo)

-- | Aiken @ledger_state.WithdrawalEvent@.
data PWithdrawalEvent (s :: S) = PWithdrawalEvent
  { pwithdrawalEvent'id :: Term s (PAsData PWithdrawalId)
  , pwithdrawalEvent'info :: Term s (PAsData PWithdrawalInfo)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PWithdrawalEvent)

{- | Aiken @ledger_state.MidgardTxValidity@.

An operator's verdict on an L2 transaction. Tags: @TxIsValid@ 0,
@NonExistentInputUtxo@ 1, @InvalidSignature@ 2, @FailedScript@ 3, @FeeTooLow@ 4,
@UnbalancedTx@ 5.
-}
data PMidgardTxValidity (s :: S)
  = PTxIsValid
  | PNonExistentInputUtxo
  | PInvalidSignature
  | PFailedScript
  | PFeeTooLow
  | PUnbalancedTx
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PMidgardTxValidity)

{- | Aiken @ledger_state.NativeTxProofSourceV1@.

The compact CBOR an L2 transaction is reconstructed from during a fault proof.
Opaque here — nothing in this slice interprets it, only carries it across.
-}
data PNativeTxProofSourceV1 (s :: S) = PNativeTxProofSourceV1
  { pnativeSource'compactCbor :: Term s (PAsData PByteString)
  , pnativeSource'witnessSetCompactCbor :: Term s (PAsData PByteString)
  , pnativeSource'fieldPreimageLengthsCbor :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PNativeTxProofSourceV1)

-- | Aiken @ledger_state.TxOrderId = OutputReference@.
type PTxOrderId = PTxOutRef

-- | Aiken @ledger_state.TxOrderPayloadV1@.
data PTxOrderPayloadV1 (s :: S) = PTxOrderPayloadV1
  { ptxOrderPayload'txId :: Term s (PAsData PByteString)
  , ptxOrderPayload'transactionCommitment :: Term s (PAsData PByteString)
  , ptxOrderPayload'source :: Term s (PAsData PNativeTxProofSourceV1)
  , ptxOrderPayload'terminalReceiptReference :: Term s PData
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTxOrderPayloadV1)

-- | Aiken @ledger_state.TxOrderEventV1@.
data PTxOrderEventV1 (s :: S) = PTxOrderEventV1
  { ptxOrderEvent'id :: Term s (PAsData PTxOrderId)
  , ptxOrderEvent'tx :: Term s (PAsData PTxOrderPayloadV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTxOrderEventV1)

{- | Aiken @ledger_state.TxFieldPreimageV1@.

One chunk of one field of a forced transaction, published on L1 while the
transaction's material is still going up piecewise. It is filed under the
transaction commitment rather than the transaction id, because the id is not
known until every field is published.

@collection_proof@ is carried but unread by the spend path — see
'Midgard.Validators.TxOrderFields'.
-}
data PTxFieldPreimageV1 (s :: S) = PTxFieldPreimageV1
  { ptxFieldPreimage'fieldReceiptPolicyId :: Term s (PAsData PCurrencySymbol)
  , ptxFieldPreimage'txOrderPolicyId :: Term s (PAsData PCurrencySymbol)
  , ptxFieldPreimage'txOrderId :: Term s (PAsData PTxOrderId)
  , ptxFieldPreimage'transactionCommitment :: Term s (PAsData PByteString)
  , ptxFieldPreimage'collectionProof :: Term s (PAsData PItemProofV1)
  , ptxFieldPreimage'proof :: Term s (PAsData PChunkProofV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTxFieldPreimageV1)

{- | Aiken @ledger_state.TxFieldReceiptV1@.

The receipt acknowledging a published field chunk. Where the preimage carries
the chunk proof itself, the receipt carries only the chunk's /index/ alongside
the collection proof, plus the links that chain receipts into an order:
@field_reference@ names the preimage it acknowledges and
@predecessor_receipt_reference@ the receipt before it.
-}
data PTxFieldReceiptV1 (s :: S) = PTxFieldReceiptV1
  { ptxFieldReceipt'fieldReceiptPolicyId :: Term s (PAsData PCurrencySymbol)
  , ptxFieldReceipt'txOrderPolicyId :: Term s (PAsData PCurrencySymbol)
  , ptxFieldReceipt'txOrderId :: Term s (PAsData PTxOrderId)
  , ptxFieldReceipt'transactionCommitment :: Term s (PAsData PByteString)
  , ptxFieldReceipt'collectionProof :: Term s (PAsData PItemProofV1)
  , ptxFieldReceipt'chunkIndex :: Term s (PAsData PInteger)
  , ptxFieldReceipt'fieldReference :: Term s (PAsData PTxOutRef)
  , ptxFieldReceipt'predecessorReceiptReference :: Term s PData
  , ptxFieldReceipt'fieldEncodedSize :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTxFieldReceiptV1)

{- | Aiken @ledger_state.CekProgramMaterialDatumV1@.

One content-addressed CEK program node, published permissionlessly on L1. The
enclosing validator has no spend path at all, so this datum is write-once: an
operator cannot erase a forced submitter's material before it is classified or
challenged.
-}
data PCekProgramMaterialDatumV1 (s :: S) = PCekProgramMaterialDatumV1
  { pcekProgramMaterial'kind :: Term s (PAsData PInteger)
  , pcekProgramMaterial'root :: Term s (PAsData PByteString)
  , pcekProgramMaterial'preimage :: Term s (PAsData PByteString)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PCekProgramMaterialDatumV1)

{- | Aiken @bounded_collection_v1.ItemProofV1@.

Locates one item within one of a transaction's nine fields. Carried by both the
preimage and the receipt datums; the shape is ported here because those datums
need it, while @bounded_collection_v1@'s verification is not.
-}
data PItemProofV1 (s :: S) = PItemProofV1
  { pitemProof'version :: Term s (PAsData PInteger)
  , pitemProof'fieldIndex :: Term s (PAsData PInteger)
  , pitemProof'itemCount :: Term s (PAsData PInteger)
  , pitemProof'itemIndex :: Term s (PAsData PInteger)
  , pitemProof'itemLength :: Term s (PAsData PInteger)
  , pitemProof'itemCommitment :: Term s (PAsData PByteString)
  , pitemProof'frontier :: Term s (PAsData (PBuiltinList (PAsData PFrontierPeak)))
  , pitemProof'siblings :: Term s (PAsData (PBuiltinList (PAsData PByteString)))
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PItemProofV1)

{- | Aiken @ledger_state.L2TransactionSourceV1@.

What a block's @transactions_root@ stores for one L2 transaction: its id, and the
compact triple it is reconstructed from. It carries no commitment of its own —
§4's flat reversion retired that field, and the commitment is now derived from
this very triple at the one place that reads it. See
@Midgard.ValidationClaim.psourceProofCommitment@.
-}
data PL2TransactionSourceV1 (s :: S) = PL2TransactionSourceV1
  { pl2Source'txId :: Term s (PAsData PByteString)
  , pl2Source'source :: Term s (PAsData PNativeTxProofSourceV1)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PL2TransactionSourceV1)

{- | Aiken @ledger_state.ForcedInclusionTxV1@.

What a block's @forced_transactions_root@ stores for one transaction order. Note
it carries the /operator's/ verdict rather than the order's own payload
commitment — the verdict is supplied by whoever is proving inclusion, which is
why a settlement proof has to substitute it before checking membership.
-}
data PForcedInclusionTxV1 (s :: S) = PForcedInclusionTxV1
  { pforcedTx'txId :: Term s (PAsData PByteString)
  , pforcedTx'source :: Term s (PAsData PNativeTxProofSourceV1)
  , pforcedTx'operatorValidity :: Term s (PAsData PMidgardTxValidity)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PForcedInclusionTxV1)

{- | Aiken @ledger_state.TransitionPhase@.

Which of a block's four event classes a transition step belongs to. Tags are the
declaration order — @Withdrawal@ 0, @ForcedTransaction@ 1, @L2Transaction@ 2,
@Deposit@ 3 — and that order is not decorative: it is the order the steps
themselves are indexed in, so a step's phase is derivable from its index and the
header's four counts. See @transition_trace/proof.phase_for_step_index@.
-}
data PTransitionPhase (s :: S)
  = PWithdrawal
  | PForcedTransaction
  | PL2Transaction
  | PDeposit
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTransitionPhase)

{- | Aiken @ledger_state.EventKey@.

The key an event is committed under in the block's @event_to_step@ tree. Each
arm wraps the identifier of one event class, and the arms are __not__
interchangeable even where the payloads coincide: the first, second and fourth
all wrap an 'PlutusLedgerApi.V3.TxOutRef'-shaped id, so the constructor tag is
the only thing distinguishing a withdrawal's key from a deposit's.
-}
data PEventKey (s :: S)
  = PWithdrawalEventKey {pwithdrawalEventKey'withdrawalId :: Term s (PAsData PWithdrawalId)}
  | PForcedTransactionEventKey {pforcedTxEventKey'txOrderId :: Term s (PAsData PTxOrderId)}
  | PL2TransactionEventKey {pl2TxEventKey'txId :: Term s (PAsData PByteString)}
  | PDepositEventKey {pdepositEventKey'depositId :: Term s (PAsData PDepositId)}
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEventKey)

-- | Aiken @ledger_state.EventToStepValue@ — where in the trace an event landed.
data PEventToStepValue (s :: S) = PEventToStepValue
  { peventToStepValue'stepIndex :: Term s (PAsData PInteger)
  , peventToStepValue'phase :: Term s (PAsData PTransitionPhase)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PEventToStepValue)

{- | Aiken @ledger_state.TransitionStep@ (and its @TransitionStepV1@ alias).

One step of a block's transition trace: which event it applied, at which index,
in which phase, and the ledger root on either side of it. The two roots are what
make a trace /chainable/ — step @n@'s post-root must be step @n+1@'s pre-root —
and the phase is carried alongside the index even though it is derivable from it,
so a proof can be checked against the header's counts rather than assumed
consistent with them.
-}
data PTransitionStep (s :: S) = PTransitionStep
  { ptransitionStep'schemaVersion :: Term s (PAsData PInteger)
  , ptransitionStep'stepIndex :: Term s (PAsData PInteger)
  , ptransitionStep'eventKey :: Term s (PAsData PEventKey)
  , ptransitionStep'phase :: Term s (PAsData PTransitionPhase)
  , ptransitionStep'preUtxosRoot :: Term s (PAsData PMerkleRoot)
  , ptransitionStep'postUtxosRoot :: Term s (PAsData PMerkleRoot)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PTransitionStep)

-- | Aiken @ledger_state.transition_step_schema_version_v1@.
ptransitionStepSchemaVersionV1 :: forall (s :: S). Term s PInteger
ptransitionStepSchemaVersionV1 = 1

{- | Aiken @ledger_state.transition_step_v1_is_valid@.

The version gate, and the whole of it: a step is V1 exactly when it says so.
Everything else about a step is checked by whoever consumes it.
-}
ptransitionStepV1IsValid :: forall (s :: S). Term s (PTransitionStep :--> PBool)
ptransitionStepV1IsValid = phoistAcyclic $
  plam $ \step -> pmatch step $ \PTransitionStep {ptransitionStep'schemaVersion} ->
    pfromData ptransitionStep'schemaVersion #== ptransitionStepSchemaVersionV1

{- | Aiken @ledger_state.HeaderV1@.

One Midgard block's header: nine roots, seven counts, and the metadata binding
it to its predecessor and its operator. Field order is the on-chain encoding and
is read positionally by the state queue's own merge redeemer, so it must not be
reordered.

The @*_count@ fields are what the counted-root scheme commits alongside each
root — see "Midgard.TransitionTrace".
-}
data PHeaderV1 (s :: S) = PHeaderV1
  { pheader'prevUtxosRoot :: Term s (PAsData PMerkleRoot)
  , pheader'utxosRoot :: Term s (PAsData PMerkleRoot)
  , pheader'withdrawalsRoot :: Term s (PAsData PMerkleRoot)
  , pheader'forcedTransactionsRoot :: Term s (PAsData PMerkleRoot)
  , pheader'transactionsRoot :: Term s (PAsData PMerkleRoot)
  , pheader'depositsRoot :: Term s (PAsData PMerkleRoot)
  , pheader'transitionTraceRoot :: Term s (PAsData PMerkleRoot)
  , pheader'eventToStepRoot :: Term s (PAsData PMerkleRoot)
  , pheader'validationTracesRoot :: Term s (PAsData PMerkleRoot)
  , pheader'withdrawalCount :: Term s (PAsData PInteger)
  , pheader'forcedTransactionCount :: Term s (PAsData PInteger)
  , pheader'l2TransactionCount :: Term s (PAsData PInteger)
  , pheader'depositCount :: Term s (PAsData PInteger)
  , pheader'totalEventCount :: Term s (PAsData PInteger)
  , pheader'transitionStepCount :: Term s (PAsData PInteger)
  , pheader'validationTraceCount :: Term s (PAsData PInteger)
  , pheader'startTime :: Term s (PAsData PPosixTime)
  , pheader'endTime :: Term s (PAsData PPosixTime)
  , pheader'blockSlot :: Term s (PAsData PInteger)
  , pheader'expectedNetworkId :: Term s (PAsData PInteger)
  , pheader'minFeeA :: Term s (PAsData PInteger)
  , pheader'minFeeB :: Term s (PAsData PInteger)
  , pheader'prevHeaderHash :: Term s (PAsData PHeaderHash)
  , pheader'operatorVkey :: Term s (PAsData PPubKeyHash)
  , pheader'protocolVersion :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PHeaderV1)

{- | Aiken @ledger_state.HeaderTransitionCommitmentsV1@.

The proof-complete transition subset of a header. Its field order is a wire ABI:
off-chain tooling serialises this exact record independently of the full header.
-}
data PHeaderTransitionCommitmentsV1 (s :: S) = PHeaderTransitionCommitmentsV1
  { pcommitments'forcedTransactionsRoot :: Term s (PAsData PMerkleRoot)
  , pcommitments'transitionTraceRoot :: Term s (PAsData PMerkleRoot)
  , pcommitments'eventToStepRoot :: Term s (PAsData PMerkleRoot)
  , pcommitments'validationTracesRoot :: Term s (PAsData PMerkleRoot)
  , pcommitments'withdrawalCount :: Term s (PAsData PInteger)
  , pcommitments'forcedTransactionCount :: Term s (PAsData PInteger)
  , pcommitments'l2TransactionCount :: Term s (PAsData PInteger)
  , pcommitments'depositCount :: Term s (PAsData PInteger)
  , pcommitments'totalEventCount :: Term s (PAsData PInteger)
  , pcommitments'transitionStepCount :: Term s (PAsData PInteger)
  , pcommitments'validationTraceCount :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PHeaderTransitionCommitmentsV1)

{- | Aiken @ledger_state.ConfirmedState@.

The head of the state queue: the last block merged into confirmed state. It is
the root element of the state queue's linked list, which is why it carries a
condensed summary rather than a whole header.
-}
data PConfirmedState (s :: S) = PConfirmedState
  { pconfirmed'headerHash :: Term s (PAsData PHeaderHash)
  , pconfirmed'prevHeaderHash :: Term s (PAsData PHeaderHash)
  , pconfirmed'utxoRoot :: Term s (PAsData PMerkleRoot)
  , pconfirmed'startTime :: Term s (PAsData PPosixTime)
  , pconfirmed'endTime :: Term s (PAsData PPosixTime)
  , pconfirmed'protocolVersion :: Term s (PAsData PInteger)
  }
  deriving stock (Generic)
  deriving anyclass (SOP.Generic, PIsData, PEq, PShow)
  deriving (PlutusType) via (DeriveAsDataStruct PConfirmedState)

{- | Aiken @ledger_state.protocol_version_v1@.

Version zero is /not/ an ordinary header version: it identifies only the
genesis sentinel. A real block header must carry this.
-}
pprotocolVersionV1 :: forall (s :: S). Term s PInteger
pprotocolVersionV1 = 1

{- | Aiken @ledger_state.genesis_protocol_version@.

Zero, and it names exactly one state — the genesis sentinel. No ordinary header
may carry it, which is what 'pdecodeHeaderView' in "Midgard.StateQueue"
enforces.
-}
pgenesisProtocolVersion :: forall (s :: S). Term s PInteger
pgenesisProtocolVersion = 0

-- | Aiken @ledger_state.genesis_header_hash@ — 28 zero bytes.
pgenesisHeaderHash :: forall (s :: S). Term s PByteString
pgenesisHeaderHash = pconstant (BS.replicate 28 0x00)

-- | Aiken @ledger_state.genesis_utxo_root = env.empty_merkle_tree_root@.
pgenesisUtxoRoot :: forall (s :: S). Term s PByteString
pgenesisUtxoRoot = Env.pemptyMerkleTreeRoot

{- | Aiken @ledger_state.genesis_confirmed_state_v1@.

The one authenticated starting point of the state queue. Both time fields are
the genesis instant, which is what makes the sentinel recognisable: an ordinary
confirmed state covers an interval, this one covers a point.

Returns 'PNothing' for a negative genesis time rather than erroring, mirroring
the @Option@.
-}
pgenesisConfirmedStateV1 ::
  forall (s :: S). Term s (PInteger :--> PMaybe PConfirmedState)
pgenesisConfirmedStateV1 = phoistAcyclic $
  plam $ \genesisTime ->
    pif
      (genesisTime #< 0)
      (pcon PNothing)
      ( pcon . PJust . pcon $
          PConfirmedState
            { pconfirmed'headerHash = pdata pgenesisHeaderHash
            , pconfirmed'prevHeaderHash = pdata pgenesisHeaderHash
            , pconfirmed'utxoRoot = pdata pgenesisUtxoRoot
            , pconfirmed'startTime = pdata genesisTime
            , pconfirmed'endTime = pdata genesisTime
            , pconfirmed'protocolVersion = pdata pgenesisProtocolVersion
            }
      )

{- | Aiken @ledger_state.confirmed_state_next_header_protocol_version_v1@.

Authenticates a confirmed state and says which protocol version the next header
built on it must carry. Both accepted shapes answer @v1@; what differs is what
each has to prove.

The genesis arm is an exact-match test on all six fields — the sentinel is one
specific value, not a shape. The ordinary arm instead demands
@header_hash != genesis_header_hash@, which is what stops a forged state
claiming to be genesis-adjacent, and @start_time <= end_time@.

Returning @v1@ from the genesis arm is the load-bearing part: the sentinel's own
version is zero, and that zero must never reach a committed header. Answering
with the /next/ version rather than the state's own is how that is arranged.
-}
pconfirmedStateNextHeaderProtocolVersionV1 ::
  forall (s :: S). Term s (PConfirmedState :--> PMaybe PInteger)
pconfirmedStateNextHeaderProtocolVersionV1 = phoistAcyclic $
  plam $ \confirmedState -> pmatch confirmedState $
    \PConfirmedState
      { pconfirmed'headerHash
      , pconfirmed'prevHeaderHash
      , pconfirmed'utxoRoot
      , pconfirmed'startTime
      , pconfirmed'endTime
      , pconfirmed'protocolVersion
      } ->
        plet (pfromData pconfirmed'headerHash) $ \headerHash ->
          plet (pfromData pconfirmed'startTime) $ \startTime ->
            plet (pfromData pconfirmed'endTime) $ \endTime ->
              plet (pfromData pconfirmed'protocolVersion) $ \version ->
                pif
                  ( pand'List
                      [ version #== pgenesisProtocolVersion
                      , headerHash #== pgenesisHeaderHash
                      , pfromData pconfirmed'prevHeaderHash #== pgenesisHeaderHash
                      , pfromData pconfirmed'utxoRoot #== pgenesisUtxoRoot
                      , 0 #<= startTime
                      , startTime #== endTime
                      ]
                  )
                  (pcon (PJust pprotocolVersionV1))
                  ( pif
                      ( pand'List
                          [ version #== pprotocolVersionV1
                          , pnot # (headerHash #== pgenesisHeaderHash)
                          , 0 #<= startTime
                          , startTime #<= endTime
                          ]
                      )
                      (pcon (PJust pprotocolVersionV1))
                      (pcon PNothing)
                  )

{- | Aiken @ledger_state.root_matches_count_v1@.

The counted-root invariant in one place: a count of zero forces the empty-tree
root, and any other count forces a 32-byte root that is /not/ the empty one.

This is what stops a block claiming a non-empty commitment over nothing, or
claiming events under a root that commits to none.
-}
prootMatchesCountV1 ::
  forall (s :: S). Term s (PByteString :--> PInteger :--> PBool)
prootMatchesCountV1 = phoistAcyclic $
  plam $ \root count ->
    pif
      (count #== 0)
      (root #== Env.pemptyMerkleTreeRoot)
      ( pand'List
          [ 0 #< count
          , pnot # (root #== Env.pemptyMerkleTreeRoot)
          , plengthBS # root #== 32
          ]
      )

-- | Aiken @ledger_state.header_transition_commitments_v1@.
pheaderTransitionCommitmentsV1 ::
  forall (s :: S).
  Term s (PHeaderV1 :--> PHeaderTransitionCommitmentsV1)
pheaderTransitionCommitmentsV1 = phoistAcyclic $
  plam $ \header -> pmatch header $ \PHeaderV1 {..} ->
    pcon $
      PHeaderTransitionCommitmentsV1
        { pcommitments'forcedTransactionsRoot = pheader'forcedTransactionsRoot
        , pcommitments'transitionTraceRoot = pheader'transitionTraceRoot
        , pcommitments'eventToStepRoot = pheader'eventToStepRoot
        , pcommitments'validationTracesRoot = pheader'validationTracesRoot
        , pcommitments'withdrawalCount = pheader'withdrawalCount
        , pcommitments'forcedTransactionCount = pheader'forcedTransactionCount
        , pcommitments'l2TransactionCount = pheader'l2TransactionCount
        , pcommitments'depositCount = pheader'depositCount
        , pcommitments'totalEventCount = pheader'totalEventCount
        , pcommitments'transitionStepCount = pheader'transitionStepCount
        , pcommitments'validationTraceCount = pheader'validationTraceCount
        }

-- | Aiken @ledger_state.max_withdrawal_count_v1@ and its six siblings.
pmaxWithdrawalCountV1
  , pmaxForcedTransactionCountV1
  , pmaxL2TransactionCountV1
  , pmaxDepositCountV1
  , pmaxTotalEventCountV1
  , pmaxTransitionStepCountV1
  , pmaxValidationTraceCountV1 ::
    forall (s :: S). Term s PInteger
pmaxWithdrawalCountV1 = 10_000
pmaxForcedTransactionCountV1 = 10_000
pmaxL2TransactionCountV1 = 10_000
pmaxDepositCountV1 = 10_000
pmaxTotalEventCountV1 = 40_000
pmaxTransitionStepCountV1 = 40_000
pmaxValidationTraceCountV1 = 20_000

{- | Aiken @ledger_state.header_transition_commitments_v1_are_valid@.

Everything a block claims about how much it did, checked against itself.

Three of these are accounting identities rather than bounds, and they are what
make the counts non-negotiable:

  * @total_event_count@ is the sum of the four per-kind counts, so a block
    cannot inflate its event total beyond what it itemised;
  * @transition_step_count == total_event_count@ — one transition step per
    event, no more and no fewer; and
  * @validation_trace_count@ counts exactly the events that run scripts, the
    forced transactions and the L2 transactions.

The per-kind ceilings then bound the whole from above, and each root is tied to
its count by 'prootMatchesCountV1'.
-}
pheaderTransitionCommitmentsV1AreValid ::
  forall (s :: S). Term s (PHeaderTransitionCommitmentsV1 :--> PBool)
pheaderTransitionCommitmentsV1AreValid = phoistAcyclic $
  plam $ \commitments -> pmatch commitments $
    \PHeaderTransitionCommitmentsV1
      { pcommitments'forcedTransactionsRoot
      , pcommitments'transitionTraceRoot
      , pcommitments'eventToStepRoot
      , pcommitments'validationTracesRoot
      , pcommitments'withdrawalCount
      , pcommitments'forcedTransactionCount
      , pcommitments'l2TransactionCount
      , pcommitments'depositCount
      , pcommitments'totalEventCount
      , pcommitments'transitionStepCount
      , pcommitments'validationTraceCount
      } ->
        plet (pfromData pcommitments'withdrawalCount) $ \withdrawalCount ->
          plet (pfromData pcommitments'forcedTransactionCount) $ \forcedCount ->
            plet (pfromData pcommitments'l2TransactionCount) $ \l2Count ->
              plet (pfromData pcommitments'depositCount) $ \depositCount ->
                plet (pfromData pcommitments'totalEventCount) $ \totalCount ->
                  plet (pfromData pcommitments'validationTraceCount) $ \traceCount ->
                    pand'List
                      [ 0 #<= withdrawalCount
                      , withdrawalCount #<= pmaxWithdrawalCountV1
                      , 0 #<= forcedCount
                      , forcedCount #<= pmaxForcedTransactionCountV1
                      , 0 #<= l2Count
                      , l2Count #<= pmaxL2TransactionCountV1
                      , 0 #<= depositCount
                      , depositCount #<= pmaxDepositCountV1
                      , 0 #<= totalCount
                      , totalCount #<= pmaxTotalEventCountV1
                      , 0 #<= pfromData pcommitments'transitionStepCount
                      , pfromData pcommitments'transitionStepCount #<= pmaxTransitionStepCountV1
                      , 0 #<= traceCount
                      , traceCount #<= pmaxValidationTraceCountV1
                      , totalCount #== withdrawalCount + forcedCount + l2Count + depositCount
                      , pfromData pcommitments'transitionStepCount #== totalCount
                      , traceCount #== forcedCount + l2Count
                      , prootMatchesCountV1 # pfromData pcommitments'forcedTransactionsRoot # forcedCount
                      , prootMatchesCountV1 # pfromData pcommitments'transitionTraceRoot # totalCount
                      , prootMatchesCountV1 # pfromData pcommitments'eventToStepRoot # totalCount
                      , prootMatchesCountV1 # pfromData pcommitments'validationTracesRoot # traceCount
                      ]

{- | Aiken @ledger_state.header_validation_context_scalars_v1_are_valid@.

The scalars an L2 transaction is validated against, sanity-checked. The network
id must be mainnet or testnet, and the fee parameters and slot cannot be
negative — a negative @min_fee_b@ would make fee validation admit free
transactions.
-}
pheaderValidationContextScalarsV1AreValid ::
  forall (s :: S). Term s (PHeaderV1 :--> PBool)
pheaderValidationContextScalarsV1AreValid = phoistAcyclic $
  plam $ \header -> pmatch header $
    \PHeaderV1 {pheader'expectedNetworkId, pheader'minFeeA, pheader'minFeeB, pheader'blockSlot} ->
      plet (pfromData pheader'expectedNetworkId) $ \networkId ->
        pand'List
          [ (networkId #== 0) #|| (networkId #== 1)
          , 0 #<= pfromData pheader'minFeeA
          , 0 #<= pfromData pheader'minFeeB
          , 0 #<= pfromData pheader'blockSlot
          ]

{- | Aiken @ledger_state.header_v1_is_valid@.

Everything a header must satisfy on its own, independent of what it is being
appended to. The state queue checks this on commit and again on merge; a fraud
proof can rely on it having held.

Note what is /not/ here: nothing about @start_time@, @end_time@,
@prev_header_hash@ or @prev_utxos_root@. Those are relational — they only mean
something against the preceding block or the confirmed state — and the state
queue checks them separately.
-}
pheaderV1IsValid :: forall (s :: S). Term s (PHeaderV1 :--> PBool)
pheaderV1IsValid = phoistAcyclic $
  plam $ \header -> pmatch header $
    \PHeaderV1
      { pheader'protocolVersion
      , pheader'withdrawalsRoot
      , pheader'withdrawalCount
      , pheader'transactionsRoot
      , pheader'l2TransactionCount
      , pheader'depositsRoot
      , pheader'depositCount
      } ->
        pand'List
          [ pfromData pheader'protocolVersion #== pprotocolVersionV1
          , pheaderValidationContextScalarsV1AreValid # header
          , prootMatchesCountV1
              # pfromData pheader'withdrawalsRoot
              # pfromData pheader'withdrawalCount
          , prootMatchesCountV1
              # pfromData pheader'transactionsRoot
              # pfromData pheader'l2TransactionCount
          , prootMatchesCountV1
              # pfromData pheader'depositsRoot
              # pfromData pheader'depositCount
          , pheaderTransitionCommitmentsV1AreValid
              # (pheaderTransitionCommitmentsV1 # header)
          ]
