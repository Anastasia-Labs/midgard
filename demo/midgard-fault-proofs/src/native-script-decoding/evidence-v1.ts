/**
 * `native-script-decoding` evidence assembly (offchain plan §4.2).
 *
 * Everything a decoding-fault redeemer carries that is not positional (the
 * submitters resolve input/output/reference indices against the transaction
 * they are actually building, following the family submitter pattern):
 *
 * - the committed-claim openings ride the existing transition-trace witness
 *   builders (`buildEventToStepMembershipProof`, `buildIndexedTraceProof`)
 *   and the forced leaf rides `buildForcedTransactionLeafMembershipProof`;
 * - the subject's field opening rides the §8.8 door builders in
 *   `src/field-opening-v1.ts` — nothing here re-implements the door;
 * - what THIS module owns: the accused outpoint's trie key (twin of
 *   `encode_midgard_tx_input`), the pre-state ledger-trie membership proof
 *   behind an injected trie handle (the plan's §4.2 rule — the evidence
 *   module never owns ledger reconstruction), the bounded-item chunk proofs
 *   in the wire shape the redeemers spell, the planner→wire converters for
 *   scan windows and frame witnesses, and the §7.2 out-of-domain face
 *   classification for the direction-B closing arm.
 *
 * Like the door builder, everything here refuses early what the validator
 * would abort on: a trie whose root is not the thread's `prior_ledger_root`,
 * a window chunk outside the item, a face that is actually in-domain.
 */
import {
  buildMidgardBoundedItemChunkProofV1,
  buildMidgardBoundedItemV1,
  computeHash32,
  encodeMidgardSpendInputItemV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  midgardBoundedItemChunkCountV1,
  type MidgardBoundedItemChunkProofV1,
  type MidgardNativeScriptScanFrameV1,
} from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import {
  MIDGARD_FIELD_INDEX_V1,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE_V1,
  NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_DEPTH_LIMIT_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
  NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_NODE_LIMIT_V1,
  Proof,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import {
  buildEventToStepMembershipProof,
  buildForcedTransactionLeafMembershipProof,
  buildIndexedTraceProof,
} from "../transition-trace/witnesses.js";
import { NATIVE_SCRIPT_DECODING_CATEGORY_LABEL } from "./contracts-v1.js";
import type {
  NativeScriptDecodingPlanWindowV1,
  NativeScriptDecodingScanSegmentPlanV1,
} from "./scan-plan-v1.js";

const evidenceError = (message: string): Error =>
  new Error(`${NATIVE_SCRIPT_DECODING_CATEGORY_LABEL} evidence: ${message}`);

// ## The accused outpoint's ledger-trie key

/**
 * Twin of `encode_midgard_tx_input` — the §5.3 fixed 38-byte spend-input
 * item IS the ledger trie's key for the outpoint.
 */
export const nativeScriptDecodingOutpointKeyV1 = ({
  txIdHex,
  outputIndex,
}: {
  readonly txIdHex: string;
  readonly outputIndex: number;
}): Buffer => {
  if (!/^[0-9a-f]{64}$/u.test(txIdHex)) {
    throw evidenceError(
      "accused outpoint tx id must be 32 bytes of lowercase hex",
    );
  }
  return encodeMidgardSpendInputItemV1({
    txId: Buffer.from(txIdHex, "hex"),
    outputIndex,
  });
};

/** `blake2b_256` of the key bytes — the thread state's `outpoint_key_hash`. */
export const nativeScriptDecodingOutpointKeyHashV1 = (
  outpointKey: Buffer,
): string => computeHash32(outpointKey).toString("hex");

/**
 * The §2.5 field the accused pair names: `outpoint_source_kind` 0 reads the
 * spend inputs, 1 the reference inputs.
 */
export const nativeScriptDecodingSubjectFieldIndexV1 = (
  outpointSourceKind: bigint,
): number => {
  if (outpointSourceKind === NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1) {
    return MIDGARD_FIELD_INDEX_V1.spendInputs;
  }
  if (
    outpointSourceKind === NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE_V1
  ) {
    return MIDGARD_FIELD_INDEX_V1.referenceInputs;
  }
  throw evidenceError(
    `outpoint source kind ${outpointSourceKind.toString()} names no §2.5 field`,
  );
};

// ## Pre-state ledger membership (injected trie handle, §4.2)

/**
 * The one thing the evidence module needs from the pre-state ledger: its
 * root, and a membership proof per key. Reconstruction stays with the caller
 * (the watcher's block replay, or a test's hand-built trie) — the handle is
 * structural precisely so this package depends on neither.
 */
export type NativeScriptDecodingLedgerTrieHandleV1 = {
  /** The trie's current root, 32 bytes of hex. */
  readonly rootHex: string;
  /** MPF membership-proof CBOR for the key; must throw when absent. */
  readonly prove: (key: Buffer) => Promise<Buffer>;
};

/**
 * The `ledger_membership_proof` a `BindOutpoint` redeemer carries: the MPF
 * proof of the accused outpoint's descriptor under the thread's committed
 * `prior_ledger_root`. Refuses a trie whose root is not that commitment —
 * a proof from any other tree would abort on-chain after the thread's
 * unrepeatable bind.
 */
export const buildNativeScriptDecodingLedgerMembershipV1 = async ({
  trie,
  outpointKey,
  priorLedgerRootHex,
}: {
  readonly trie: NativeScriptDecodingLedgerTrieHandleV1;
  readonly outpointKey: Buffer;
  readonly priorLedgerRootHex: string;
}): Promise<SDK.Proof> => {
  const trieRoot = trie.rootHex.toLowerCase();
  const committedRoot = priorLedgerRootHex.toLowerCase();
  if (trieRoot !== committedRoot) {
    throw evidenceError(
      `ledger trie root ${trieRoot} is not the thread's prior_ledger_root ${committedRoot}`,
    );
  }
  const proofCbor = await trie.prove(outpointKey);
  return Data.from(Buffer.from(proofCbor).toString("hex"), Proof);
};

// ## Bounded-item chunk proofs, in wire shape

/** Core chunk proof → the `BoundedItemChunkProofV1` wire value. */
export const nativeScriptDecodingChunkProofDataV1 = (
  proof: MidgardBoundedItemChunkProofV1,
): SDK.BoundedItemChunkProofV1 => ({
  version: BigInt(proof.version),
  field_index: BigInt(proof.fieldIndex),
  item_index: BigInt(proof.itemIndex),
  total_length: BigInt(proof.totalLength),
  chunk_index: BigInt(proof.chunkIndex),
  chunk: Buffer.from(proof.chunk).toString("hex"),
  frontier: proof.frontier.peaks.map((peak) => ({
    height: BigInt(peak.height),
    hash: Buffer.from(peak.hash).toString("hex"),
  })),
  siblings: proof.siblings.map((sibling) =>
    Buffer.from(sibling).toString("hex"),
  ),
});

/**
 * One authenticated chunk of the accused reference-script item, ready for a
 * redeemer's `chunk_proof` slots.
 */
export const buildNativeScriptDecodingChunkProofV1 = ({
  fieldIndex,
  itemIndex,
  itemBytes,
  chunkIndex,
}: {
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemBytes: Uint8Array;
  readonly chunkIndex: number;
}): SDK.BoundedItemChunkProofV1 => {
  const chunkCount = midgardBoundedItemChunkCountV1(itemBytes.length);
  if (chunkIndex < 0 || chunkIndex >= chunkCount) {
    throw evidenceError(
      `chunk ${chunkIndex.toString()} is outside the item's ${chunkCount.toString()} chunks`,
    );
  }
  return nativeScriptDecodingChunkProofDataV1(
    buildMidgardBoundedItemChunkProofV1(
      buildMidgardBoundedItemV1({
        fieldIndex,
        itemIndex,
        bytes: Buffer.from(itemBytes),
      }),
      chunkIndex,
    ),
  );
};

// ## Planner plan → redeemer wire pieces

/**
 * The two chunk-proof slots a `Scan`/`Verdict` redeemer carries for one
 * planned window: the window's chunk, and the mandatory adjacent next chunk
 * whenever the item has one (`needNext`). A windowless plan carries `null`
 * in both slots.
 */
export const nativeScriptDecodingWindowProofsV1 = ({
  window,
  fieldIndex,
  itemIndex,
  itemBytes,
}: {
  readonly window: NativeScriptDecodingPlanWindowV1 | null;
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemBytes: Uint8Array;
}): {
  readonly chunk_proof: SDK.BoundedItemChunkProofV1 | null;
  readonly next_chunk_proof: SDK.BoundedItemChunkProofV1 | null;
} => {
  if (window === null) {
    return { chunk_proof: null, next_chunk_proof: null };
  }
  return {
    chunk_proof: buildNativeScriptDecodingChunkProofV1({
      fieldIndex,
      itemIndex,
      itemBytes,
      chunkIndex: window.chunkIndex,
    }),
    next_chunk_proof: window.needNext
      ? buildNativeScriptDecodingChunkProofV1({
          fieldIndex,
          itemIndex,
          itemBytes,
          chunkIndex: window.chunkIndex + 1,
        })
      : null,
  };
};

/** Engine-twin frame witness → the `NativeScriptFrameV1` wire value. */
export const nativeScriptDecodingFrameDataV1 = (
  frame: MidgardNativeScriptScanFrameV1,
): SDK.NativeScriptFrameV1 => ({
  tail: Buffer.from(frame.tail).toString("hex"),
  kind: BigInt(frame.kind),
  child_count: BigInt(frame.childCount),
  remaining: BigInt(frame.remaining),
  valid_count: BigInt(frame.validCount),
  required: frame.required,
});

/**
 * Everything a `Scan` redeemer carries besides its positional indices, from
 * one planned segment.
 */
export const nativeScriptDecodingScanArgsEvidenceV1 = ({
  segment,
  fieldIndex,
  itemIndex,
  itemBytes,
}: {
  readonly segment: NativeScriptDecodingScanSegmentPlanV1;
  readonly fieldIndex: number;
  readonly itemIndex: number;
  readonly itemBytes: Uint8Array;
}): {
  readonly control_cbor: string;
  readonly chunk_proof: SDK.BoundedItemChunkProofV1 | null;
  readonly next_chunk_proof: SDK.BoundedItemChunkProofV1 | null;
  readonly frames: readonly SDK.NativeScriptFrameV1[];
  readonly step_budget: bigint;
} => ({
  control_cbor: segment.controlBefore.cborHex,
  ...nativeScriptDecodingWindowProofsV1({
    window: segment.window,
    fieldIndex,
    itemIndex,
    itemBytes,
  }),
  frames: segment.frames.map(nativeScriptDecodingFrameDataV1),
  step_budget: BigInt(segment.stepBudget),
});

// ## Committed-claim openings (step-02)

/**
 * The three membership proofs a step-02 redeemer opens the committed claim
 * with. `transition_step_membership` is located through the event→step map
 * rather than by a caller-supplied index, so the two proofs cannot name
 * different steps; `forced_membership` exists exactly for forced threads.
 */
export const buildNativeScriptDecodingStep02EvidenceV1 = async ({
  reconstruction,
  eventKey,
}: {
  readonly reconstruction: TransitionTraceReconstruction;
  readonly eventKey: SDK.EventKey;
}): Promise<{
  readonly header: SDK.HeaderV1;
  readonly eventToStepMembership: SDK.EventToStepMembershipProof;
  readonly transitionStepMembership: SDK.IndexedTraceProof;
  readonly forcedMembership: SDK.RootMembershipProof<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  > | null;
}> => {
  const eventToStepMembership = await buildEventToStepMembershipProof({
    reconstruction,
    eventKey,
  });
  const transitionStepMembership = await buildIndexedTraceProof({
    reconstruction,
    stepIndex: eventToStepMembership.value.step_index,
  });
  const forcedMembership =
    "ForcedTransactionEventKey" in eventKey
      ? await buildForcedTransactionLeafMembershipProof({
          reconstruction,
          eventKey,
        })
      : null;
  return {
    header: reconstruction.header,
    eventToStepMembership,
    transitionStepMembership,
    forcedMembership,
  };
};

/**
 * The accusation a decoding-family rejection makes: its refusal class and the
 * accused `(source_kind, ordinal)` pair.
 */
export type NativeScriptDecodingScanAccusationV1 = {
  readonly scanReasonClass: bigint;
  readonly outpointSourceKind: bigint;
  readonly outpointCursor: bigint;
};

/**
 * Twin of `engine.scan_accusation_of_v1` (2026-08-24 ruling): the three
 * decoding rejection arms map onto refusal classes 0/1/2 with the accused
 * pair copied verbatim; any other rejection is a foreign arm this family
 * cannot dispute in direction B. Deliberately no domain filtering — an
 * out-of-domain pair is step-03's `BindOutOfDomain` close, not a refusal
 * here.
 */
export const nativeScriptDecodingScanAccusationOfV1 = (
  reason: SDK.RejectionReasonV1,
): NativeScriptDecodingScanAccusationV1 => {
  if (
    typeof reason === "object" &&
    "ResolvedReferenceScriptMalformed" in reason
  ) {
    const arm = reason.ResolvedReferenceScriptMalformed;
    return {
      scanReasonClass: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_MALFORMED_V1,
      outpointSourceKind: arm.source_kind,
      outpointCursor: arm.input_index,
    };
  }
  if (
    typeof reason === "object" &&
    "ResolvedReferenceScriptNodeLimit" in reason
  ) {
    const arm = reason.ResolvedReferenceScriptNodeLimit;
    return {
      scanReasonClass: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_NODE_LIMIT_V1,
      outpointSourceKind: arm.source_kind,
      outpointCursor: arm.input_index,
    };
  }
  if (
    typeof reason === "object" &&
    "ResolvedReferenceScriptDepthLimit" in reason
  ) {
    const arm = reason.ResolvedReferenceScriptDepthLimit;
    return {
      scanReasonClass: NATIVE_SCRIPT_DECODING_REFUSAL_CLASS_DEPTH_LIMIT_V1,
      outpointSourceKind: arm.source_kind,
      outpointCursor: arm.input_index,
    };
  }
  throw evidenceError(
    "the disputed rejection is not one of the family's three decoding arms, so a direction-B thread cannot dispute it.",
  );
};

// ## §7.2 out-of-domain faces (direction B closing arm)

export const NativeScriptDecodingOutOfDomainFacesV1 = Object.freeze({
  /** `source_kind` names no field — closes with no opening. */
  UnknownSourceKind: "unknownSourceKind",
  /** A negative ordinal — closes with no opening. */
  NegativeOrdinal: "negativeOrdinal",
  /**
   * Ordinal at or past the named field's item count — the one face that
   * proves against the §8.8 door's authenticated count.
   */
  CountFace: "countFace",
} as const);

export type NativeScriptDecodingOutOfDomainFaceV1 =
  (typeof NativeScriptDecodingOutOfDomainFacesV1)[keyof typeof NativeScriptDecodingOutOfDomainFacesV1];

/**
 * Which §7.2 face — if any — the accused pair presents. `itemCount` is the
 * named field's authenticated item count and may be omitted only when the
 * pair is already out of domain without it; an in-domain pair returns
 * `null`, and the closing-arm submitter must refuse it (the on-chain
 * neutralisation selector `rejects_an_in_domain_ordinal_close` is the same
 * refusal).
 */
export const classifyNativeScriptDecodingOutOfDomainFaceV1 = ({
  outpointSourceKind,
  outpointCursor,
  itemCount,
}: {
  readonly outpointSourceKind: bigint;
  readonly outpointCursor: bigint;
  readonly itemCount: bigint | null;
}): NativeScriptDecodingOutOfDomainFaceV1 | null => {
  if (
    outpointSourceKind !== NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_SPEND_V1 &&
    outpointSourceKind !== NATIVE_SCRIPT_DECODING_OUTPOINT_SOURCE_REFERENCE_V1
  ) {
    return NativeScriptDecodingOutOfDomainFacesV1.UnknownSourceKind;
  }
  if (outpointCursor < 0n) {
    return NativeScriptDecodingOutOfDomainFacesV1.NegativeOrdinal;
  }
  if (itemCount === null) {
    throw evidenceError(
      "the count face needs the named field's authenticated item count",
    );
  }
  return outpointCursor >= itemCount
    ? NativeScriptDecodingOutOfDomainFacesV1.CountFace
    : null;
};

/** Re-exported so submitters name the shared constant, not a literal. */
export { MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1 };
