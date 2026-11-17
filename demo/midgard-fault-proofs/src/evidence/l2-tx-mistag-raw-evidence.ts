/**
 * The normal leaves of a retained block whose canonical reconstruction
 * stopped at a committed-field or field-length defect, read before any leaf
 * is decoded: where each falls on the block's event order, and the earliest
 * leaf committed with a non-zero validity code.
 */
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  type L2TxMistagCommittedLeafInclusion,
  l2TxMistagInclusionFromCommittedLeaves,
} from "../l2-tx-mistag/committed-leaf-inclusion.js";
import {
  type DaHashPreimageBlockEvidence,
  daHashPreimageBlockEvidenceFromVerifiedPayload,
} from "../prepare-da-hash-preimage.js";
import {
  decodeTypedEntries,
  validateDenseTransitionTrace,
} from "../transition-trace/reconstruct.decode-transactions.js";
import { eventKeyFingerprint } from "../transition-trace/reconstruct.transition-trace-reconstruction.js";
import {
  type EventOrder,
  eventOrdersOver,
} from "../workflow/detection-subject.js";
import { isCommittedInvalidNormalLeaf } from "./committed-normal-leaf-validity.js";

export const L2_TX_MISTAG_RAW_EVIDENCE =
  "midgard-l2-tx-mistag-raw-evidence-v1" as const;

export type L2TxMistagRawBlockEvidence = Readonly<{
  schemaVersion: typeof L2_TX_MISTAG_RAW_EVIDENCE;
  grade: SDK.EvidenceGrade;
  provenance: DaHashPreimageBlockEvidence["provenance"];
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  l1ChainPoint: DaHashPreimageBlockEvidence["l1ChainPoint"];
  selected: L2TxMistagCommittedLeafInclusion;
}>;

/** A block's normal leaves on its event order, read from retained DA. */
export type RawNormalLeaves = Readonly<{
  /** Where the leaf at a committed index falls on the block's event order. */
  orderOf: (transactionIndex: number) => EventOrder;
  /** The earliest leaf committed with a non-zero validity code, if any. */
  mistag:
    | Readonly<{ order: EventOrder; evidence: L2TxMistagRawBlockEvidence }>
    | undefined;
}>;

/**
 * The block's transition steps as reconstruction reads them, or none when
 * they do not decode as a dense trace. Every leaf is then untraced and falls
 * in committed order: the block has a structural trace fault of its own, and
 * this order is still one every watcher computes alike.
 */
const decodedTrace = (body: SDK.DaPayload["block_body"]) => {
  try {
    const trace = decodeTypedEntries<bigint, SDK.TransitionStep>({
      fieldName: "transition_trace",
      entries: body.transition_trace,
      keySchema: Data.Integer() as never,
      valueSchema: SDK.TransitionStepSchema,
    });
    validateDenseTransitionTrace(trace);
    return trace;
  } catch {
    return [];
  }
};

const committedSource = (
  valueCbor: string,
): SDK.L2TransactionSource | undefined => {
  try {
    return Data.from(valueCbor, SDK.L2TransactionSource);
  } catch {
    return undefined;
  }
};

/**
 * Orders the block's committed normal leaves by the classifier's rule (the
 * trace step that first names a leaf's event, untraced leaves after the trace
 * in committed order) and finds the earliest leaf the `l2TxMistag` step
 * proves. The leaves must open the L1-authenticated transactions root before
 * a mistag is reported.
 */
export const rawNormalLeavesFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<RawNormalLeaves> => {
  const raw = await daHashPreimageBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const payload = SDK.decodeDaPayload(
    Buffer.from(
      (
        await unwrapDaPayload(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
        })
      ).innerBytes,
    ),
  );
  const fingerprints = raw.entries.map(([txId]) =>
    eventKeyFingerprint({ L2TransactionEventKey: { tx_id: txId } }),
  );
  const orders = eventOrdersOver(
    decodedTrace(payload.block_body),
    fingerprints,
  );
  const orderOf = (transactionIndex: number): EventOrder => {
    const order = orders.get(fingerprints[transactionIndex] ?? "");
    if (order === undefined)
      throw new Error(
        `normal leaf ${transactionIndex.toString()} is not a leaf of this block`,
      );
    return order;
  };
  let earliest: number | undefined;
  for (const [index, [, valueCbor]] of raw.entries.entries()) {
    const source = committedSource(valueCbor);
    if (source === undefined || !isCommittedInvalidNormalLeaf(source)) continue;
    if (earliest === undefined || orderOf(index) < orderOf(earliest))
      earliest = index;
  }
  if (earliest === undefined)
    return Object.freeze({ orderOf, mistag: undefined });
  const selected = await l2TxMistagInclusionFromCommittedLeaves({
    entries: raw.entries,
    transactionIndex: earliest,
    committedTransactionsRoot: raw.committedTransactionsRoot,
    l2TransactionCount: raw.l2TransactionCount,
  });
  return Object.freeze({
    orderOf,
    mistag: Object.freeze({
      order: orderOf(earliest),
      evidence: Object.freeze({
        schemaVersion: L2_TX_MISTAG_RAW_EVIDENCE,
        grade: raw.grade,
        provenance: raw.provenance,
        headerHash: raw.headerHash,
        payloadEnvelopeSha256: raw.payloadEnvelopeSha256,
        payloadSha256: raw.payloadSha256,
        l1ChainPoint: raw.l1ChainPoint,
        selected,
      }),
    }),
  });
};
