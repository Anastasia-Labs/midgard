/**
 * `fabricated-withdrawal` family (Goal task `Q40`) — off-chain codec and rule
 * twin.
 *
 * Proves a block header commits a withdrawal leaf that is not the authentic L1
 * withdrawal event pair: either no withdrawal event with the committed
 * `WithdrawalId` was ever authenticated (`NonexistentWithdrawalIdentity`), or the
 * authentic event exists and was due for the block but its `(body, signature)`
 * content is not the committed one (`MismatchedWithdrawalContent`).
 *
 * The committed `validity` verdict is deliberately outside that comparison.
 * Decision 0007
 * (`docs/fault-proofs/decisions/0007-operator-owned-event-validity.md`) rules
 * that the operator owns the verdict a block stamps on a committed user event:
 * the L1 order datum's `WithdrawalIsValid` is a placeholder written at order
 * creation (`../user-events/withdrawal.ts`), so a committed verdict that differs
 * from it is the operator's adjudication of L2 state, judged by
 * `withdrawalMistag` against the authenticated ledger, not a fabrication of the
 * L1 order.
 *
 * Violation: `fabricated-withdrawal`.
 * Production catalogue category: `fabricatedWithdrawal` (`0000000c`).
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-withdrawal/step-0{1,2,3,4}.ak`
 * field for field and constructor index for constructor index, and the exact
 * bytes are pinned in `tests/fabricated-withdrawal.test.ts` against values
 * measured out of those Aiken modules.
 *
 * ### Why the commitments normalise their CBOR
 *
 * A `WithdrawalInfo` embeds `WithdrawalBody.l2_value`, a `Value` map — and
 * Lucid's typed encoder writes non-empty Plutus maps in **indefinite** form
 * (`bf … ff`) while Plutus' own `serialiseData`, which is what Aiken's
 * `cbor.serialise` calls on chain, writes them **definite** (`a1 …`). Committed
 * withdrawal leaves are checked on chain by re-serialising the typed leaf
 * (`transition_trace.verify_root_membership_with_bytes` over
 * `cbor.serialise(membership.key/value)`, both in this family's step-01 and in
 * `transition_trace/proof`'s `WithdrawalSourceMembership` arm), so the bytes that
 * bind are the `serialiseData` ones. Every helper here therefore passes its Lucid
 * output through `aikenSerialisedPlutusDataCborPreservingMapOrder`, exactly as
 * the withdrawal MPF producer and reserve-payout override do. Map pair order
 * from Lucid is retained, including mixed-length asset names. Dropping that normalisation would silently produce
 * commitments and leaf bytes no on-chain step can reproduce; the twin test pins
 * both forms so the difference cannot regress unnoticed.
 */

import "@al-ft/midgard-core/lucid-data";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@lucid-evolution/lucid";
import "effect";
import "../common.js";
import "../ledger-state.js";
import "../state-queue.js";
import "../transition-trace.js";
import "../user-events/history.js";
import "../user-events/withdrawal.js";
import "./catalogue.js";
import "./native.js";
import "./fabricated-withdrawal.fabricated-withdrawal-fault-schema.js";
import "./fabricated-withdrawal.fabricated-withdrawal-step02-state.js";
import "./fabricated-withdrawal.types.js";
export {
  type CommittedWithdrawalSourceProof,
  CommittedWithdrawalSourceProofSchema,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID,
  FABRICATED_WITHDRAWAL_VIOLATION_ID,
  FabricatedWithdrawalAuthenticContentOpening,
  FabricatedWithdrawalAuthenticContentOpeningSchema,
  type FabricatedWithdrawalChallengedHeaderHash,
  FabricatedWithdrawalChallengedHeaderHashSchema,
  FabricatedWithdrawalEvidence,
  FabricatedWithdrawalEvidenceSchema,
  FabricatedWithdrawalEvidenceVerdict,
  FabricatedWithdrawalEvidenceVerdictSchema,
  FabricatedWithdrawalFault,
  FabricatedWithdrawalFaultSchema,
  FabricatedWithdrawalStep01Args,
  FabricatedWithdrawalStep01ArgsSchema,
  FabricatedWithdrawalStep01Datum,
  FabricatedWithdrawalStep01DatumSchema,
  FabricatedWithdrawalStep01SpendRedeemer,
  FabricatedWithdrawalStep01SpendRedeemerSchema,
  FabricatedWithdrawalStep02Args,
  FabricatedWithdrawalStep02ArgsSchema,
  FabricatedWithdrawalStep02Datum,
  FabricatedWithdrawalStep02DatumSchema,
  FabricatedWithdrawalStep02SpendRedeemer,
  FabricatedWithdrawalStep02SpendRedeemerSchema,
  FabricatedWithdrawalStep02State,
  FabricatedWithdrawalStep02StateSchema,
  FabricatedWithdrawalStep03Args,
  FabricatedWithdrawalStep03ArgsSchema,
  FabricatedWithdrawalStep03Datum,
  FabricatedWithdrawalStep03DatumSchema,
  FabricatedWithdrawalStep03SpendRedeemer,
  FabricatedWithdrawalStep03SpendRedeemerSchema,
  FabricatedWithdrawalStep03State,
  FabricatedWithdrawalStep03StateSchema,
  fabricatedWithdrawalThreadTokenAssetName,
} from "./fabricated-withdrawal.fabricated-withdrawal-fault-schema.js";
export {
  committedWithdrawalKeyBytes,
  committedWithdrawalValueBytes,
  FABRICATED_WITHDRAWAL_STEP_NAMES,
  fabricatedWithdrawalStep02State,
  fabricatedWithdrawalStep03State,
  FabricatedWithdrawalStep04Args,
  FabricatedWithdrawalStep04ArgsSchema,
  FabricatedWithdrawalStep04Datum,
  FabricatedWithdrawalStep04DatumSchema,
  FabricatedWithdrawalStep04SpendRedeemer,
  FabricatedWithdrawalStep04SpendRedeemerSchema,
  FabricatedWithdrawalStep04State,
  fabricatedWithdrawalStep04State,
  FabricatedWithdrawalStep04StateSchema,
  fabricatedWithdrawalStepDatumSchema,
  type FabricatedWithdrawalStepName,
  isFabricatedWithdrawalFault,
  WithdrawalContent,
  withdrawalContentBytes,
  withdrawalContentBytesCbor,
  withdrawalContentCommitment,
  withdrawalContentCommitmentCbor,
  withdrawalContentOf,
  WithdrawalContentSchema,
  withdrawalEventDatumBytes,
  withdrawalEventDatumCommitment,
  withdrawalEventNonce,
} from "./fabricated-withdrawal.fabricated-withdrawal-step02-state.js";
export { type FabricatedWithdrawalCountedRootInput } from "./fabricated-withdrawal.types.js";
