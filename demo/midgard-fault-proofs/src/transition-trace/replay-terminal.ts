import { hashMidgardValidationRejectionCode } from "@al-ft/midgard-core";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core";
import { decodeSingleCbor } from "@al-ft/midgard-core/codec/cbor";
import { type EventKey, readRetainedValidationState } from "@al-ft/midgard-sdk";

import { classifyCommittedFieldShapeFields } from "../committed-field-shape/prepare-committed-field-shape.js";
import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { transactionHasNonCanonicalMintItem } from "../mint-item-non-canonical/replay.js";
import { replayPrerequisiteFailure } from "../workflow/replay-prerequisite.js";
import { eventKeyFingerprint } from "./reconstruct.js";
import { buildRetainedValidationClaimWitness } from "./witnesses.js";

type RetainedClaim = Awaited<
  ReturnType<typeof buildRetainedValidationClaimWitness>
>;

/** Endpoint membership authenticates bytes; this separately checks their grammar. */
export const assertRetainedReplayTerminal = (retained: RetainedClaim): void => {
  const terminal = retained.claim.terminal_state;
  const value = decodeSingleCbor(
    Buffer.from(retained.terminalWorkWitnessCbor, "hex"),
  );
  if (
    !Array.isArray(value) ||
    value.length !== 4 ||
    !(value[1] instanceof Uint8Array) ||
    !(value[2] instanceof Uint8Array) ||
    !(value[3] instanceof Uint8Array) ||
    value[2].length !== 32 ||
    terminal.phase !== "Terminal"
  )
    throw new Error("Retained replay terminal witness is malformed");
  const verdict = value[0];
  if (
    (verdict !== 1 && verdict !== 1n && verdict !== 2 && verdict !== 2n) ||
    (terminal.verdict === "Accepted"
      ? BigInt(verdict) !== 1n
      : terminal.verdict === "Rejected"
        ? BigInt(verdict) !== 2n
        : true)
  )
    throw new Error("Retained replay terminal witness verdict is malformed");
  if (terminal.verdict === "Accepted") {
    if (value[1].length !== 0)
      throw new Error("Retained accepted terminal contains a rejection code");
    return;
  }
  const rejectionCode = Buffer.from(value[1]).toString("ascii");
  if (
    !/^E_[A-Z0-9_]+$/u.test(rejectionCode) ||
    !Buffer.from(rejectionCode, "ascii").equals(value[1]) ||
    hashMidgardValidationRejectionCode(rejectionCode).toString("hex") !==
      terminal.rejection_code_hash ||
    Buffer.from(value[2]).toString("hex") !== terminal.prior_ledger_root ||
    Buffer.from(value[3]).toString("hex") !== "80"
  )
    throw new Error("Retained rejected terminal witness is malformed");
};

/** A canonical early rejection has no later-phase witness. Missing witnesses
 * on traces that claim to reach that phase remain ordinary fatal errors. */
export const requireRetainedReplayPhase = async (
  evidence: CanonicalBlockEvidence,
  eventKey: EventKey,
  phase: bigint,
): Promise<void> => {
  const source = evidence.reconstruction.sourceEventsByFingerprint.get(
    eventKeyFingerprint(eventKey),
  );
  if (
    source?.phase === "L2Transaction" &&
    (classifyCommittedFieldShapeFields(
      decodeMidgardNativeTxFullFromCanonicalCbor(
        source.entry.fullTransactionCbor,
      ),
    ).some(({ evidence: field }) => field.isViolation) ||
      transactionHasNonCanonicalMintItem(source.entry.fullTransactionCbor))
  )
    throw replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey,
      "representable_field_shape",
    );
  const retained = await buildRetainedValidationClaimWitness({
    reconstruction: evidence.reconstruction,
    eventKey,
  });
  assertRetainedReplayTerminal(retained);
  if (retained.claim.terminal_state.verdict !== "Rejected") return;
  const descriptor = retained.claim.descriptor_membership.value;
  const previous = readRetainedValidationState({
    entries:
      evidence.reconstruction.payload.block_body.validation_trace_witnesses,
    eventKey,
    descriptor,
    stateIndex: descriptor.step_count - 1n,
  });
  if (previous.phase < phase)
    throw replayPrerequisiteFailure(
      evidence.headerHash,
      eventKey,
      "accepted_terminal",
    );
};
