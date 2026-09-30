/**
 * `fabricated-deposit` family (Goal task `Q39`) — off-chain codec and rule twin.
 *
 * Proves a block header commits a deposit leaf that is not the authentic L1
 * deposit event pair: either no deposit event with the committed `DepositId` was
 * ever authenticated (`NonexistentDepositIdentity`), or the authentic event
 * exists and was due for the block but its `DepositInfo` is not the committed
 * one (`MismatchedDepositContent`).
 *
 * Violation: `fabricated-deposit`.
 * Production catalogue category: `fabricatedDeposit` (`0000000b`).
 *
 * Every schema below mirrors an Aiken type in
 * `onchain/aiken/lib/midgard/fraud-proofs/fabricated-deposit/step-0{1,2,3,4}.ak`
 * field for field and constructor index for constructor index, and the exact
 * bytes are pinned in `tests/fabricated-deposit.test.ts` against values
 * measured out of those Aiken modules.
 */

import "@al-ft/midgard-core/lucid-data";
import "@al-ft/midgard-core/plutus-data-cbor";
import "@lucid-evolution/lucid";
import "effect";
import "../common.js";
import "../ledger-state.js";
import "../state-queue.js";
import "../transition-trace.js";
import "../user-events/deposit.js";
import "../user-events/history.js";
import "./catalogue.js";
import "./native.js";
import "./fabricated-deposit.fabricated-deposit-fault-schema.js";
import "./fabricated-deposit.fabricated-deposit-step02-state.js";
export {
  type ChallengedHeaderHash,
  ChallengedHeaderHashSchema,
  type CommittedDepositSourceProof,
  CommittedDepositSourceProofSchema,
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID,
  FABRICATED_DEPOSIT_VIOLATION_ID,
  FabricatedDepositAuthenticContentOpening,
  FabricatedDepositAuthenticContentOpeningSchema,
  FabricatedDepositEvidence,
  FabricatedDepositEvidenceSchema,
  FabricatedDepositEvidenceVerdict,
  FabricatedDepositEvidenceVerdictSchema,
  FabricatedDepositFault,
  FabricatedDepositFaultSchema,
  FabricatedDepositStep01Args,
  FabricatedDepositStep01ArgsSchema,
  FabricatedDepositStep01Datum,
  FabricatedDepositStep01DatumSchema,
  FabricatedDepositStep01SpendRedeemer,
  FabricatedDepositStep01SpendRedeemerSchema,
  FabricatedDepositStep02Args,
  FabricatedDepositStep02ArgsSchema,
  FabricatedDepositStep02Datum,
  FabricatedDepositStep02DatumSchema,
  FabricatedDepositStep02SpendRedeemer,
  FabricatedDepositStep02SpendRedeemerSchema,
  FabricatedDepositStep02State,
  FabricatedDepositStep02StateSchema,
  FabricatedDepositStep03Args,
  FabricatedDepositStep03ArgsSchema,
  FabricatedDepositStep03Datum,
  FabricatedDepositStep03DatumSchema,
  FabricatedDepositStep03SpendRedeemer,
  FabricatedDepositStep03SpendRedeemerSchema,
  FabricatedDepositStep03State,
  FabricatedDepositStep03StateSchema,
  FabricatedDepositStep04Datum,
  FabricatedDepositStep04DatumSchema,
  FabricatedDepositStep04State,
  FabricatedDepositStep04StateSchema,
  fabricatedDepositThreadTokenAssetName,
} from "./fabricated-deposit.fabricated-deposit-fault-schema.js";
export {
  committedDepositKeyBytes,
  committedDepositValueBytes,
  depositEventDatumCommitment,
  depositEventNonce,
  depositInfoCommitment,
  depositInfoCommitmentCbor,
  FABRICATED_DEPOSIT_STEP_NAMES,
  type FabricatedDepositCountedRootInput,
  fabricatedDepositStep02State,
  fabricatedDepositStep03State,
  FabricatedDepositStep04Args,
  FabricatedDepositStep04ArgsSchema,
  FabricatedDepositStep04SpendRedeemer,
  FabricatedDepositStep04SpendRedeemerSchema,
  fabricatedDepositStep04State,
  fabricatedDepositStepDatumSchema,
  type FabricatedDepositStepName,
  isFabricatedDepositFault,
} from "./fabricated-deposit.fabricated-deposit-step02-state.js";
