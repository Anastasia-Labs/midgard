/**
 * `missing-signature` fault-proof family (Goal task `Q16`) — off-chain wire
 * twins.
 *
 * **Rule.** Every required signer of every committed accepted transaction must
 * be witnessed: `∀t ∈ Ledger, ∀h ∈ required_signers(t): ∃(v, s) ∈
 * addr_tx_wits(t): blake2b_224(v) == h`.
 *
 * **Violation.** A block commits an accepted transaction naming a required
 * signer (body field 4) whose witness is absent from the address-witness
 * collection (witness field 7). The forced direction instead authenticates
 * RequiredSignerUnsigned and proves that exact required signer genuinely
 * signed, or that the reason coordinate names no required signer.
 *
 * The accepted-invalid proof is a four-step computation thread:
 *
 * 1. bind the bad transaction to the block's counted `transactions_root` and
 *    forward the §2.5 anchor — the transaction id plus the `witness_set_hash`
 *    read off the block-committed compact structure (§3's id preimage is the
 *    body alone, so the id says nothing about the witness set);
 * 2. open body field 4 (`required_signers`) through the §8.8 door and select
 *    the accused signer hash by its fixed-28-byte-stride ordinal;
 * 3. lift the accused hash to its verification-key preimage
 *    (`blake2b_224(vkey) == hash`); and
 * 4. open witness field 7 (`address_witnesses`) through the door against the
 *    thread-anchored `witness_set_hash`, then walk the authenticated preimage
 *    in bounded batches, requiring the vkey to appear in no witness. Each
 *    non-terminal batch commits a canonical checkpoint into the thread datum;
 *    the terminal batch burns the thread and mints the permanent proof.
 *
 * Production catalogue category: `missingSignature` (`0000000e`). The
 * asset-name helper accepts the deployed category id so callers remain bound
 * to the manifest they are submitting against.
 *
 * This module is the strict TypeScript twin of
 * `onchain/aiken/lib/midgard/fraud-proofs/missing-signature/step-0{1..4}.ak`.
 * Field order in every `Data.Object` mirrors the aiken record declarations
 * 1:1 — the PlutusData encoding is positional, so re-ordering here would
 * silently produce redeemers the validators reject.
 */

import "@al-ft/midgard-core/lucid-data";
import "@lucid-evolution/lucid";
import "@noble/hashes/blake2.js";
import "../common.js";
import "../ledger-state.js";
import "../transition-trace.js";
import "./field-opening.js";
import "./native.js";
import "./missing-signature.find-missing-required-signer-index.js";
import "./missing-signature.missing-signature-field-walk-checkpoint.js";
import "./missing-signature.missing-signature-forced-signer-datum.js";
export {
  findMissingRequiredSignerIndex,
  MISSING_SIGNATURE_VIOLATION_ID,
  missingSignatureRequiredSignerIsPresent,
  MissingSignatureStep01Args,
  MissingSignatureStep01ArgsSchema,
  MissingSignatureStep01Datum,
  MissingSignatureStep01DatumSchema,
  MissingSignatureStep01SpendRedeemer,
  MissingSignatureStep01SpendRedeemerSchema,
  MissingSignatureStep02Args,
  MissingSignatureStep02ArgsSchema,
  MissingSignatureStep02Datum,
  MissingSignatureStep02DatumSchema,
  MissingSignatureStep02SpendRedeemer,
  MissingSignatureStep02SpendRedeemerSchema,
  MissingSignatureStep02State,
  MissingSignatureStep02StateSchema,
  MissingSignatureStep03Args,
  MissingSignatureStep03ArgsSchema,
  MissingSignatureStep03Datum,
  MissingSignatureStep03DatumSchema,
  MissingSignatureStep03SpendRedeemer,
  MissingSignatureStep03SpendRedeemerSchema,
  MissingSignatureStep03State,
  MissingSignatureStep03StateSchema,
  MissingSignatureStep04Datum,
  MissingSignatureStep04DatumSchema,
  MissingSignatureStep04State,
  MissingSignatureStep04StateSchema,
  MissingSignatureStepCancel,
  MissingSignatureStepCancelSchema,
  missingSignatureThreadTokenAssetName,
  missingSignatureVkeyHash,
  nativeTxHasMissingSignatureViolation,
} from "./missing-signature.find-missing-required-signer-index.js";
export {
  MISSING_SIGNATURE_ADDRESS_WITNESS_STRIDE,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE,
  type MissingSignatureFieldWalkCheckpoint,
  missingSignatureFieldWalkCheckpoint,
  MissingSignatureForcedSignerArgs,
  MissingSignatureForcedSignerArgsSchema,
  MissingSignatureForcedSignerDatumSchema,
  MissingSignatureForcedSignerSpendRedeemerSchema,
  MissingSignatureForcedSignerStateSchema,
  MissingSignatureForcedStepArgs,
  MissingSignatureForcedStepArgsSchema,
  MissingSignatureForcedStepDatum,
  MissingSignatureForcedStepDatumSchema,
  MissingSignatureForcedStepSpendRedeemer,
  MissingSignatureForcedStepSpendRedeemerSchema,
  MissingSignatureForcedWitnessArgsSchema,
  MissingSignatureForcedWitnessStateSchema,
  missingSignatureStep02StateFromVerifiedTx,
  MissingSignatureStep04Args,
  MissingSignatureStep04ArgsSchema,
  MissingSignatureStep04SpendRedeemer,
  MissingSignatureStep04SpendRedeemerSchema,
  resolveMissingSignatureFieldWalkCheckpoint,
} from "./missing-signature.missing-signature-field-walk-checkpoint.js";
export {
  MissingSignatureForcedSignerDatum,
  MissingSignatureForcedSignerSpendRedeemer,
  MissingSignatureForcedSignerState,
  MissingSignatureForcedWitnessArgs,
  MissingSignatureForcedWitnessDatum,
  MissingSignatureForcedWitnessDatumSchema,
  MissingSignatureForcedWitnessSpendRedeemer,
  MissingSignatureForcedWitnessSpendRedeemerSchema,
  MissingSignatureForcedWitnessState,
} from "./missing-signature.missing-signature-forced-signer-datum.js";
