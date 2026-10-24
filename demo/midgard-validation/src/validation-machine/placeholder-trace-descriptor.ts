/**
 * The P1 validation-trace descriptor: a deterministic two-state placeholder.
 *
 * P1 runs no validation-machine trace. Each transaction event still commits a
 * descriptor of the frozen shape, built from the event identity and the
 * verdict alone, so every party that knows the event and its verdict derives
 * the same bytes without replaying the machine and without retaining any
 * machine state. `machine_version` stays at the compiled version.
 */
import { midgardBlake2b } from "@al-ft/midgard-core";
import {
  computeMidgardForcedTxProofCommitment,
  computeMidgardNativeTxProofCommitment,
  deriveMidgardForcedTxFaultEvidenceMaterial,
  deriveMidgardNativeTxProofSourceFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import {
  buildMidgardValidationTraceTree,
  hashMidgardValidationEventKey,
  hashMidgardValidationRejectionReasonV1,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardRejectionReasonV1,
  type MidgardValidationTraceDescriptor,
  MidgardValidationVerdict,
} from "@al-ft/midgard-core/validation-trace";

const PLACEHOLDER_INITIAL_STATE_DOMAIN = Buffer.from(
  "MidgardValidationPlaceholderInitialV1",
  "ascii",
);

const PLACEHOLDER_TERMINAL_STATE_DOMAIN = Buffer.from(
  "MidgardValidationPlaceholderTerminalV1",
  "ascii",
);

/** The event identity a placeholder descriptor is derived from. */
export type PlaceholderValidationTraceEventV1 = Readonly<{
  /** Canonical CBOR of the event key the descriptor is committed under. */
  eventKeyCbor: Uint8Array;
  /** The transaction's proof-source commitment (see validationTransactionCommitmentV1). */
  transactionCommitment: Uint8Array;
}>;

const hash32 = (domain: Buffer, ...parts: readonly Uint8Array[]): Buffer =>
  Buffer.from(
    midgardBlake2b(
      Buffer.concat([domain, ...parts.map((part) => Buffer.from(part))]),
      {
        dkLen: 32,
      },
    ),
  );

const exact32 = (bytes: Uint8Array, fieldName: string): Buffer => {
  if (bytes.length !== 32) {
    throw new Error(`${fieldName} must be 32 bytes`);
  }
  return Buffer.from(bytes);
};

/**
 * The proof-source commitment of a canonical normal or forced transaction,
 * the same commitment the validation machine binds into every state. A forced
 * transaction's source is read from its envelope alone, so an order whose
 * items do not decode still has its commitment.
 */
export const validationTransactionCommitmentV1 = ({
  canonicalTransactionCbor,
  sourceKind,
}: Readonly<{
  canonicalTransactionCbor: Uint8Array;
  sourceKind: "normal" | "forced";
}>): Buffer =>
  sourceKind === "forced"
    ? computeMidgardForcedTxProofCommitment(
        deriveMidgardForcedTxFaultEvidenceMaterial(canonicalTransactionCbor)
          .proofSource,
      )
    : computeMidgardNativeTxProofCommitment(
        deriveMidgardNativeTxProofSourceFromCanonicalCbor(
          canonicalTransactionCbor,
        ),
      );

/**
 * `init = H(dom_init ‖ event_key_hash ‖ tx_commitment)`,
 * `term = H(dom_term ‖ event_key_hash ‖ verdict ‖ rejection_code_hash)`,
 * `trace_root = branch(leaf(init), leaf(term))`, `step_count = 1`.
 *
 * `verdict` is the one-byte validation verdict code. An accepted verdict
 * carries no rejection reason (its hash is the all-zero hash); a rejected one
 * carries its exact `RejectionReasonV1`, hashed as the machine's terminal
 * state hashes it.
 */
export const placeholderValidationTraceDescriptorV1 = (
  event: PlaceholderValidationTraceEventV1,
  verdict: "accepted" | "rejected",
  rejectionReason: MidgardRejectionReasonV1 | null,
): MidgardValidationTraceDescriptor => {
  if ((verdict === "accepted") !== (rejectionReason === null)) {
    throw new Error(
      "placeholder validation descriptor verdict and rejection reason disagree",
    );
  }
  const eventKeyHash = hashMidgardValidationEventKey(event.eventKeyCbor);
  const rejectionCodeHash =
    rejectionReason === null
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : hashMidgardValidationRejectionReasonV1(rejectionReason);
  const initialStateHash = hash32(
    PLACEHOLDER_INITIAL_STATE_DOMAIN,
    eventKeyHash,
    exact32(event.transactionCommitment, "transaction commitment"),
  );
  const terminalStateHash = hash32(
    PLACEHOLDER_TERMINAL_STATE_DOMAIN,
    eventKeyHash,
    Buffer.of(MidgardValidationVerdict[verdict]),
    rejectionCodeHash,
  );
  return buildMidgardValidationTraceTree(
    [initialStateHash, terminalStateHash],
    verdict,
    rejectionCodeHash,
  ).descriptor;
};
