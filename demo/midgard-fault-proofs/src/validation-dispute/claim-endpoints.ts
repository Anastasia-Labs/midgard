import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  computeHash32,
  computeMidgardNativeTxProofCommitment,
  encodeCbor,
} from "@al-ft/midgard-core";
import { computeMidgardForcedTxProofCommitment } from "@al-ft/midgard-core/codec/forced";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import {
  hashMidgardValidationContext,
  hashMidgardValidationRejectionCode,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core/validation-trace";
import {
  EventKey,
  type Header,
  rejectionCodeOf,
  type ValidationClaimWitness,
  type ValidationMachineState,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

const noRejectionHash = "00".repeat(32);
const machineStateIsWellFormed = (state: ValidationMachineState): boolean =>
  state.machine_version === 1n &&
  [
    state.event_key_hash,
    state.transaction_id,
    state.transaction_commitment,
    state.validation_context_hash,
    state.prior_ledger_root,
    state.work_root,
    state.rejection_code_hash,
    state.ledger_delta_root,
  ].every((hash) => /^[0-9a-f]{64}$/u.test(hash)) &&
  state.program_counter >= 0n &&
  state.program_counter <=
    BigInt(MIDGARD_CONSENSUS_LIMITS.maxValidationMachineStepCount) &&
  state.execution_cpu >= 0n &&
  state.execution_memory >= 0n &&
  (state.verdict === "Rejected"
    ? state.rejection_code_hash !== noRejectionHash
    : state.rejection_code_hash === noRejectionHash);

/** Mirrors committed_claim_endpoints_and_source_are_valid for output routing.
 * The source validator independently authenticates the source membership and
 * evaluates this predicate. Completing the transaction with local evaluation
 * remains mandatory: this helper does not authorize an award itself.
 */
export const committedValidationClaimEndpointsAndSourceAreValid = (
  header: Header,
  claim: ValidationClaimWitness,
): boolean => {
  const initial = claim.initial_state;
  const terminal = claim.terminal_state;
  const descriptor = claim.descriptor_membership.value;
  const transition = claim.transition_step_membership.value;
  const forced =
    "ForcedValidationSource" in claim.source_membership
      ? claim.source_membership.ForcedValidationSource.membership.value
      : undefined;
  const entry =
    "NormalValidationSource" in claim.source_membership
      ? claim.source_membership.NormalValidationSource.membership.value
      : claim.source_membership.ForcedValidationSource.membership.value;
  const submitted =
    "submitted_source" in entry ? entry.submitted_source : entry.source;
  const source = {
    compactCbor: Buffer.from(submitted.compact_cbor, "hex"),
    witnessSetCompactCbor: Buffer.from(
      submitted.witness_set_compact_cbor,
      "hex",
    ),
    fieldPreimageLengthsCbor: Buffer.from(
      submitted.field_preimage_lengths_cbor,
      "hex",
    ),
  };
  const context = Buffer.from(claim.validation_context_cbor, "hex");
  const expectedContext = aikenSerialisedPlutusDataCborPreservingMapOrder(
    encodeCbor([
      1n,
      Buffer.from("midgard-consensus-v1", "utf8"),
      header.endTime,
      header.expectedNetworkId,
      header.minFeeA,
      header.minFeeB,
      header.blockSlot,
    ]).toString("hex"),
  );
  const initialWorkRoot = hashMidgardValidationWorkWitness({
    phase: "canonicalDecode",
    programCounter: 0,
    witnessCbor: encodeCbor([
      source.compactCbor,
      source.witnessSetCompactCbor,
      source.fieldPreimageLengthsCbor,
      context,
      0n,
      0n,
      0n,
      -1n,
      0n,
    ]),
  }).toString("hex");
  const sourceVerdictMatches =
    forced === undefined || forced.verdict === "ForcedTxValid"
      ? descriptor.verdict === "Accepted"
      : descriptor.verdict === "Rejected" &&
        descriptor.rejection_code_hash ===
          hashMidgardValidationRejectionCode(
            Buffer.from(
              rejectionCodeOf(forced.verdict.ForcedTxInvalid.reason),
              "hex",
            ).toString("ascii"),
          ).toString("hex");
  return (
    machineStateIsWellFormed(initial) &&
    machineStateIsWellFormed(terminal) &&
    initial.event_key_hash ===
      computeHash32(
        Buffer.from(
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            Data.to(claim.descriptor_membership.key, EventKey),
          ),
          "hex",
        ),
      ).toString("hex") &&
    initial.prior_ledger_root === transition.pre_utxos_root &&
    initial.phase === "CanonicalDecode" &&
    initial.program_counter === 0n &&
    initial.execution_cpu === 0n &&
    initial.execution_memory === 0n &&
    initial.verdict === "Pending" &&
    claim.validation_context_cbor === expectedContext &&
    (header.expectedNetworkId === 0n || header.expectedNetworkId === 1n) &&
    header.minFeeA >= 0n &&
    header.minFeeB >= 0n &&
    header.blockSlot >= 0n &&
    initial.validation_context_hash ===
      hashMidgardValidationContext(context).toString("hex") &&
    initial.work_root === initialWorkRoot &&
    terminal.phase === "Terminal" &&
    terminal.program_counter === descriptor.step_count &&
    terminal.verdict === descriptor.verdict &&
    terminal.rejection_code_hash === descriptor.rejection_code_hash &&
    initial.machine_version === terminal.machine_version &&
    initial.event_key_hash === terminal.event_key_hash &&
    initial.transaction_id === terminal.transaction_id &&
    initial.transaction_commitment === terminal.transaction_commitment &&
    initial.validation_context_hash === terminal.validation_context_hash &&
    initial.source_kind === terminal.source_kind &&
    initial.prior_ledger_root === terminal.prior_ledger_root &&
    initial.ledger_delta_root === terminal.ledger_delta_root &&
    entry.tx_id === initial.transaction_id &&
    (forced === undefined
      ? computeMidgardNativeTxProofCommitment
      : computeMidgardForcedTxProofCommitment)(source).toString("hex") ===
      initial.transaction_commitment &&
    initial.source_kind === (forced === undefined ? "Normal" : "Forced") &&
    sourceVerdictMatches &&
    (descriptor.verdict !== "Rejected" ||
      transition.pre_utxos_root === transition.post_utxos_root)
  );
};
