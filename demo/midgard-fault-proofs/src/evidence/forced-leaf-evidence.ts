import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";

export type ForcedLeafEvidence = {
  readonly eventKey: SDK.EventKey;
  readonly eventKeyFingerprint: string;
  readonly leaf: SDK.ForcedInclusionTxV1;
  readonly fullTransactionCbor: Buffer;
  readonly membership: SDK.RootMembershipProof<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >;
};

const forcedLeafError = (message: string): Error =>
  new Error(`forced-leaf evidence: ${message}`);

const sameReason = (
  left: SDK.RejectionReason,
  right: SDK.RejectionReason,
): boolean =>
  Data.to(left, SDK.RejectionReason) === Data.to(right, SDK.RejectionReason);

/**
 * Extracts the authenticated forced leaf and its exact membership proof from
 * the retained-DA reconstruction. Callers never supply a leaf or verdict:
 * both are selected by the committed event key.
 */
export const extractForcedLeafEvidence = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<ForcedLeafEvidence> => {
  if (!("ForcedTransactionEventKey" in eventKey)) {
    throw forcedLeafError("event key is not a forced-transaction key");
  }
  const fingerprint = eventKeyFingerprint(eventKey);
  const source = reconstruction.sourceEventsByFingerprint.get(fingerprint);
  if (source === undefined || source.phase !== "ForcedTransaction") {
    throw forcedLeafError(
      `committed forced transaction ${fingerprint} is absent from retained DA`,
    );
  }
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction,
    eventKey,
  });
  if (
    Data.to(membership.value, SDK.ForcedInclusionTxV1) !==
    Data.to(source.entry.value, SDK.ForcedInclusionTxV1)
  ) {
    throw forcedLeafError("membership proof selected a different forced leaf");
  }
  return Object.freeze({
    eventKey,
    eventKeyFingerprint: fingerprint,
    leaf: source.entry.value,
    fullTransactionCbor: Buffer.from(source.entry.fullTransactionCbor),
    membership,
  });
};

export const requireForcedLeafAccepted = (
  evidence: ForcedLeafEvidence,
): ForcedLeafEvidence => {
  if (evidence.leaf.verdict !== "ForcedTxValid") {
    throw forcedLeafError("expected an explicit acceptance verdict");
  }
  return evidence;
};

/**
 * Binds direction-B evidence to the complete typed reason, including all
 * subject coordinates. Comparing only the constructor arm is insufficient.
 */
export const requireForcedLeafRejectedFor = (
  evidence: ForcedLeafEvidence,
  expectedReason: SDK.RejectionReason,
): ForcedLeafEvidence => {
  const { verdict } = evidence.leaf;
  if (verdict === "ForcedTxValid") {
    throw forcedLeafError("expected an explicit rejection verdict");
  }
  if (!sameReason(verdict.ForcedTxInvalid.reason, expectedReason)) {
    throw forcedLeafError(
      "typed rejection reason or subject coordinate differs",
    );
  }
  return evidence;
};

export type ForcedLeafVerdictSubject = {
  readonly version: bigint;
  readonly direction: bigint;
  readonly source_kind: bigint;
  readonly transaction_id: string;
  readonly source_key: string;
  readonly rejection_reason: SDK.RejectionReason | null;
};

/**
 * Reduces authenticated leaf evidence to the constant-size shared thread
 * subject. The source key is encoded by the SDK twin of Aiken
 * `cbor.serialise(membership.key)`; raw native out-ref CBOR is never accepted.
 */
export const forcedLeafVerdictSubject = (
  evidence: ForcedLeafEvidence,
): ForcedLeafVerdictSubject => {
  const rejectionReason =
    evidence.leaf.verdict === "ForcedTxValid"
      ? null
      : evidence.leaf.verdict.ForcedTxInvalid.reason;
  return Object.freeze({
    version: 1n,
    direction: rejectionReason === null ? 0n : 1n,
    source_kind: 1n,
    transaction_id: evidence.leaf.tx_id,
    source_key: Data.to(
      evidence.membership.key as never,
      SDK.OutputReferenceSchema as never,
    ),
    rejection_reason: rejectionReason,
  });
};
