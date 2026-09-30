import {
  type MidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { type MidgardValidationMachineState } from "@al-ft/midgard-core/validation-trace";
import * as SDK from "@al-ft/midgard-sdk";

import { DaPayloadValidationError } from "./payload.da-payload-validation-error.js";
import {
  PHASE_FROM_DATA,
  retainedSafeNumber,
} from "./payload.validate-trace-coverage.js";

export const stateFromRetainedData = (
  state: SDK.ValidationMachineState,
): MidgardValidationMachineState => {
  const machineVersion = retainedSafeNumber(
    state.machine_version,
    "machine version",
  );
  if (machineVersion !== 1) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      "retained validation witness has an unsupported machine version",
    );
  }
  return {
    machineVersion,
    eventKeyHash: Buffer.from(state.event_key_hash, "hex"),
    transactionId: Buffer.from(state.transaction_id, "hex"),
    transactionCommitment: Buffer.from(state.transaction_commitment, "hex"),
    validationContextHash: Buffer.from(state.validation_context_hash, "hex"),
    sourceKind: state.source_kind === "Normal" ? "normal" : "forced",
    priorLedgerRoot: Buffer.from(state.prior_ledger_root, "hex"),
    phase: PHASE_FROM_DATA[state.phase],
    programCounter: retainedSafeNumber(
      state.program_counter,
      "program counter",
    ),
    workRoot: Buffer.from(state.work_root, "hex"),
    executionCpu: state.execution_cpu,
    executionMemory: state.execution_memory,
    verdict:
      state.verdict === "Pending"
        ? "pending"
        : state.verdict === "Accepted"
          ? "accepted"
          : "rejected",
    rejectionCodeHash: Buffer.from(state.rejection_code_hash, "hex"),
    ledgerDeltaRoot: Buffer.from(state.ledger_delta_root, "hex"),
  };
};

export const retainedFrontier = (
  count: bigint,
  peaks: readonly { readonly height: bigint; readonly hash: string }[],
): MidgardValidationMerkleFrontier => ({
  count: retainedSafeNumber(count, "frontier count"),
  peaks: peaks.map(({ height, hash }) => ({
    height: retainedSafeNumber(height, "frontier peak height"),
    hash: Buffer.from(hash, "hex"),
  })),
});

export const nativeControlRoots = (witnessCbor: Buffer) => {
  const decoded = decodeSingleCbor(witnessCbor);
  if (!Array.isArray(decoded) || decoded.length !== 26) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      "retained NativeScripts control has the wrong shape",
    );
  }
  const integer = (index: number) => BigInt(decoded[index] as bigint | number);
  const peaks = (index: number) =>
    (decoded[index] as readonly (readonly [bigint, Uint8Array])[]).map(
      ([height, hash]) => ({
        height: BigInt(height),
        hash: Buffer.from(hash).toString("hex"),
      }),
    );
  return {
    signerCount: integer(8),
    signerCommitment: Buffer.from(decoded[9] as Uint8Array),
    source: retainedFrontier(integer(10), peaks(11)),
    purpose: retainedFrontier(integer(14), peaks(15)),
    execution: retainedFrontier(integer(21), peaks(22)),
  };
};

/**
 * Classifies a retained witness coordinate for a descriptor with
 * `step_count = n` (docs/fault-proofs/retained-validation-trace.md):
 * non-negative values are NativeScripts execution aliases, `i - n - 1` is
 * chronological state `i` in `[0, n]`, `-n - 2` is the initial endpoint and
 * `-n - 3` the terminal endpoint. Anything else is outside the domain.
 */
type RetainedWitnessSlot =
  | { readonly kind: "nativeAlias" }
  | { readonly kind: "state"; readonly stateIndex: bigint }
  | { readonly kind: "initial" }
  | { readonly kind: "terminal" };

export const retainedWitnessSlot = (
  executionIndex: bigint,
  stepCount: bigint,
): RetainedWitnessSlot | null => {
  if (executionIndex >= 0n) return { kind: "nativeAlias" };
  const stateIndex = executionIndex + stepCount + 1n;
  if (stateIndex >= 0n) return { kind: "state", stateIndex };
  if (executionIndex === -stepCount - 2n) return { kind: "initial" };
  if (executionIndex === -stepCount - 3n) return { kind: "terminal" };
  return null;
};

export const requireRetainedMembership = (
  frontier: MidgardValidationMerkleFrontier,
  leafIndex: bigint,
  leafHash: Buffer,
  siblings: readonly string[],
  label: string,
): void => {
  if (
    !verifyMidgardValidationMerkleMembership({
      frontier,
      leafIndex: retainedSafeNumber(leafIndex, `${label} index`),
      leafHash,
      siblings: siblings.map((sibling) => Buffer.from(sibling, "hex")),
    })
  ) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      `retained validation ${label} membership is invalid`,
    );
  }
};
