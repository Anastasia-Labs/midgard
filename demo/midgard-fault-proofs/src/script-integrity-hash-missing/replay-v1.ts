import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardVersionedScript,
  deriveMidgardNativeTxProofSourceV1,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import {
  extractForcedLeafEvidenceV1,
  forcedLeafVerdictSubjectV1,
} from "../evidence/forced-leaf-evidence-v1.js";
import {
  buildTrieView,
  decodeTransactionMaterial,
  requireProof,
  transactionSourceTrieItemV1,
} from "../prepare-double-spend.js";
import {
  parseSubmitStep01TxInclusion,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  eventKeyFingerprint,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  prepareScriptIntegrityHashMissingEvidenceV1,
  type ScriptIntegrityHashMissingEvidenceV1,
  scriptIntegrityHashMissingFaultHoldsV1,
} from "./family-v1.js";

export const SCRIPT_INTEGRITY_HASH_MISSING_VIOLATION_ID_V1 =
  "script-integrity-hash-missing" as const;

export type ScriptIntegrityHashMissingReplayDetectionV1 = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof SCRIPT_INTEGRITY_HASH_MISSING_VIOLATION_ID_V1;
  position: bigint;
  transactionId: string;
  source: "accepted" | "forced";
  direction: "wrongfulAcceptance" | "wrongfulRejection";
  forcedIndex?: number;
  diagnostic: string;
}>;

export type ScriptIntegrityHashMissingAuthenticatedSourceV1 = Readonly<{
  header: SDK.HeaderV1;
  nativeTxCompactCbor: string;
  witnessSetCompactCbor: string;
  acceptedInclusion?: SubmitStep01TxInclusion;
  forcedHeader?: SDK.HeaderV1;
  forcedMembership?: Awaited<
    ReturnType<typeof buildForcedTransactionLeafMembershipProof>
  >;
  forcedDirection?: 0n | 1n;
}>;

type Semantics = Readonly<{
  full: ReturnType<typeof decodeMidgardNativeTxFullV1FromCanonicalCbor>;
  scriptLanguages: readonly (0 | 3 | 128)[];
  redeemerCount: number;
  faultHolds: boolean;
}>;

const decodeSemantics = (canonicalTxCbor: Uint8Array): Semantics | null => {
  const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonicalTxCbor);
  try {
    const scriptLanguages = decodeMidgardNativeByteListPreimage(
      full.witnessSet.scriptTxWitsPreimageCbor,
      "scriptIntegrityHashMissing script witnesses",
    ).map((item): 0 | 3 | 128 => {
      const language = decodeMidgardVersionedScript(item).language;
      return language === "NativeCardano"
        ? 0
        : language === "PlutusV3"
          ? 3
          : 128;
    });
    const redeemerCount = decodeMidgardNativeByteListPreimage(
      full.witnessSet.redeemerTxWitsPreimageCbor,
      "scriptIntegrityHashMissing redeemers",
    ).length;
    const scriptIntegrityHash = full.body.scriptIntegrityHash.toString("hex");
    return {
      full,
      scriptLanguages,
      redeemerCount,
      faultHolds: scriptIntegrityHashMissingFaultHoldsV1({
        scriptIntegrityHash,
        scriptLanguages,
        redeemerCount,
      }),
    };
  } catch {
    // Malformed script/redeemer fields are owned by their earlier, narrower
    // decoding/canonicity families. This detector remains total and does not
    // steal those failures merely because the integrity scalar is zero.
    return null;
  }
};

const eventPosition = (
  reconstruction: TransitionTraceReconstruction,
  eventKey: SDK.EventKey,
): bigint => {
  const mapped = reconstruction.eventToStepByFingerprint.get(
    eventKeyFingerprint(eventKey),
  );
  if (mapped === undefined) {
    throw new Error(
      "scriptIntegrityHashMissing: authenticated event has no transition-step mapping",
    );
  }
  return mapped.value.step_index;
};

/** Complete package-owned scan of every authenticated accepted and forced tx. */
export const detectScriptIntegrityHashMissingFromReconstructionV1 = ({
  headerHash,
  reconstruction,
}: {
  readonly headerHash: string;
  readonly reconstruction: TransitionTraceReconstruction;
}): readonly ScriptIntegrityHashMissingReplayDetectionV1[] => {
  const detections: ScriptIntegrityHashMissingReplayDetectionV1[] = [];
  reconstruction.transactions.forEach((transaction, transactionIndex) => {
    const semantics = decodeSemantics(transaction.fullTransactionCbor);
    if (
      semantics !== null &&
      semantics.full.validity === "TxIsValid" &&
      semantics.faultHolds
    ) {
      detections.push(
        Object.freeze({
          detectionId: `${SCRIPT_INTEGRITY_HASH_MISSING_VIOLATION_ID_V1}:accepted:${transactionIndex.toString()}:${transaction.txId}`,
          headerHash,
          violationId: SCRIPT_INTEGRITY_HASH_MISSING_VIOLATION_ID_V1,
          position: eventPosition(reconstruction, {
            L2TransactionEventKey: { tx_id: transaction.txId },
          }),
          transactionId: transaction.txId,
          source: "accepted",
          direction: "wrongfulAcceptance",
          diagnostic: `accepted transaction ${transaction.txId} requires a script integrity hash but commits zero`,
        }),
      );
    }
  });
  reconstruction.forcedTransactions.forEach((transaction, forcedIndex) => {
    const semantics = decodeSemantics(transaction.fullTransactionCbor);
    if (semantics === null) return;
    const verdict = transaction.value.verdict;
    const rejected = verdict !== "ForcedTxValid";
    const exactReason =
      verdict !== "ForcedTxValid" &&
      verdict.ForcedTxInvalid.reason === "ScriptIntegrityHashMissing";
    const direction =
      !rejected && semantics.faultHolds
        ? "wrongfulAcceptance"
        : exactReason && !semantics.faultHolds
          ? "wrongfulRejection"
          : null;
    if (direction === null) return;
    detections.push(
      Object.freeze({
        detectionId: `${SCRIPT_INTEGRITY_HASH_MISSING_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${direction}`,
        headerHash,
        violationId: SCRIPT_INTEGRITY_HASH_MISSING_VIOLATION_ID_V1,
        position: eventPosition(reconstruction, {
          ForcedTransactionEventKey: { tx_order_id: transaction.key },
        }),
        transactionId: transaction.value.tx_id,
        source: "forced",
        direction,
        forcedIndex,
        diagnostic:
          direction === "wrongfulAcceptance"
            ? `accepted forced transaction ${transaction.value.tx_id} requires a script integrity hash but commits zero`
            : `forced transaction ${transaction.value.tx_id} was rejected for ScriptIntegrityHashMissing although its authenticated semantics contradict that exact reason`,
      }),
    );
  });
  return Object.freeze(detections);
};

export const detectScriptIntegrityHashMissingFromCanonicalEvidenceV1 = (
  evidence: CanonicalBlockEvidenceV1,
): readonly ScriptIntegrityHashMissingReplayDetectionV1[] =>
  detectScriptIntegrityHashMissingFromReconstructionV1({
    headerHash: evidence.headerHash,
    reconstruction: evidence.reconstruction,
  });

/**
 * Selects and reconstructs one detection entirely from authenticated retained
 * DA. The caller supplies only dispatch identity; prepared semantic evidence
 * and verdict callbacks are deliberately absent.
 */
export const reconstructScriptIntegrityHashMissingEvidenceV1 = async ({
  evidence,
  transactionId,
  direction,
}: {
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly transactionId: string;
  readonly direction: "wrongfulAcceptance" | "wrongfulRejection";
}): Promise<ScriptIntegrityHashMissingEvidenceV1> => {
  const candidates = detectScriptIntegrityHashMissingFromCanonicalEvidenceV1(
    evidence,
  ).filter(
    (detection) =>
      detection.transactionId === transactionId &&
      detection.direction === direction,
  );
  if (candidates.length !== 1) {
    throw new Error(
      `scriptIntegrityHashMissing: expected one authenticated replay detection, got ${candidates.length.toString()}`,
    );
  }
  const detection = candidates[0]!;
  if (detection.source === "accepted") {
    const transaction = evidence.reconstruction.transactions.find(
      (entry) => entry.txId === transactionId,
    );
    if (transaction === undefined)
      throw new Error("authenticated accepted transaction disappeared");
    const semantics = decodeSemantics(transaction.fullTransactionCbor);
    if (semantics === null)
      throw new Error("authenticated accepted semantics became undecodable");
    return prepareScriptIntegrityHashMissingEvidenceV1({
      finding: {
        category: "scriptIntegrityHashMissing",
        headerHash: evidence.headerHash,
        transactionId,
        direction,
        source: "accepted",
        rejectionReason: null,
      },
      subject: SDK.acceptedVerdictSubjectV1(transactionId),
      nativeTxCompactCbor: transaction.value.source.compact_cbor,
      witnessSetCompactCbor: transaction.value.source.witness_set_compact_cbor,
      fieldPreimageLengthsCbor:
        transaction.value.source.field_preimage_lengths_cbor,
      scriptWitnessesPreimageCbor:
        semantics.full.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
      redeemersPreimageCbor:
        semantics.full.witnessSet.redeemerTxWitsPreimageCbor.toString("hex"),
      scriptIntegrityHash:
        semantics.full.body.scriptIntegrityHash.toString("hex"),
      scriptLanguages: semantics.scriptLanguages,
      redeemerCount: semantics.redeemerCount,
    });
  }
  const forced =
    evidence.reconstruction.forcedTransactions[detection.forcedIndex!];
  if (forced === undefined || forced.value.tx_id !== transactionId) {
    throw new Error("authenticated forced transaction disappeared");
  }
  const forcedLeaf = await extractForcedLeafEvidenceV1({
    reconstruction: evidence.reconstruction,
    eventKey: {
      ForcedTransactionEventKey: { tx_order_id: forced.key },
    },
  });
  const adjudicated = adjudicateMidgardNativeTxFullV1Validity(
    decodeMidgardNativeTxFullV1FromCanonicalCbor(forced.fullTransactionCbor),
    forced.value.verdict === "ForcedTxValid" ? "TxIsValid" : "TxIsInvalid",
  );
  const semantics = decodeSemantics(forced.fullTransactionCbor);
  if (semantics === null)
    throw new Error("authenticated forced semantics became undecodable");
  // Re-derive once more here so package-owned reconstruction checks the exact
  // retained leaf source rather than copying it without verification.
  const source = deriveMidgardNativeTxProofSourceV1(adjudicated);
  if (
    source.compactCbor.toString("hex") !== forced.value.source.compact_cbor ||
    source.witnessSetCompactCbor.toString("hex") !==
      forced.value.source.witness_set_compact_cbor ||
    source.fieldPreimageLengthsCbor.toString("hex") !==
      forced.value.source.field_preimage_lengths_cbor
  ) {
    throw new Error(
      "authenticated forced proof source changed during reconstruction",
    );
  }
  return prepareScriptIntegrityHashMissingEvidenceV1({
    finding: {
      category: "scriptIntegrityHashMissing",
      headerHash: evidence.headerHash,
      transactionId,
      direction,
      source: "forced",
      rejectionReason:
        forced.value.verdict === "ForcedTxValid"
          ? null
          : "ScriptIntegrityHashMissing",
    },
    subject: forcedLeafVerdictSubjectV1(forcedLeaf),
    nativeTxCompactCbor: source.compactCbor.toString("hex"),
    witnessSetCompactCbor: source.witnessSetCompactCbor.toString("hex"),
    fieldPreimageLengthsCbor: source.fieldPreimageLengthsCbor.toString("hex"),
    scriptWitnessesPreimageCbor:
      semantics.full.witnessSet.scriptTxWitsPreimageCbor.toString("hex"),
    redeemersPreimageCbor:
      semantics.full.witnessSet.redeemerTxWitsPreimageCbor.toString("hex"),
    scriptIntegrityHash:
      semantics.full.body.scriptIntegrityHash.toString("hex"),
    scriptLanguages: semantics.scriptLanguages,
    redeemerCount: semantics.redeemerCount,
    forcedLeaf,
  });
};

/** Rebuilds every submitter source coordinate from authenticated replay. */
export const deriveScriptIntegrityHashMissingAuthenticatedSourceV1 = async ({
  block,
  evidence,
}: {
  readonly block: CanonicalBlockEvidenceV1;
  readonly evidence: ScriptIntegrityHashMissingEvidenceV1;
}): Promise<ScriptIntegrityHashMissingAuthenticatedSourceV1> => {
  if (evidence.finding.source === "accepted") {
    const decoded = await Promise.all(
      block.transactions.map(decodeTransactionMaterial),
    );
    const selected = decoded.find(
      ({ nodeTxId }) => nodeTxId === evidence.finding.transactionId,
    );
    if (selected === undefined) {
      throw new Error(
        "scriptIntegrityHashMissing: accepted subject disappeared from retained DA",
      );
    }
    const trie = await buildTrieView(decoded.map(transactionSourceTrieItemV1));
    if (
      trie.root !== block.reconstruction.rootData.transactions.phasRoot ||
      trie.root !== block.inclusionRootAuthentication.sourceValuePhasRoot
    ) {
      throw new Error(
        "scriptIntegrityHashMissing: accepted source trie differs from authenticated reconstruction",
      );
    }
    return Object.freeze({
      header: block.header,
      nativeTxCompactCbor: evidence.nativeTxCompactCbor,
      witnessSetCompactCbor: evidence.witnessSetCompactCbor,
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: selected.nodeTxId,
        nativeTx: selected.nativeTxCompact,
        nativeTxCompactCbor: selected.nativeCompactCbor,
        l2TransactionSourceCbor: selected.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(selected.nodeTxId, "hex"),
          "scriptIntegrityHashMissing accepted transaction",
        ),
      }),
    });
  }
  const forced = block.reconstruction.forcedTransactions.find(
    ({ key, value }) =>
      value.tx_id === evidence.finding.transactionId &&
      eventKeyFingerprint({
        ForcedTransactionEventKey: { tx_order_id: key },
      }) === evidence.forcedLeaf?.eventKeyFingerprint,
  );
  if (forced === undefined) {
    throw new Error(
      "scriptIntegrityHashMissing: forced subject disappeared from retained DA",
    );
  }
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: forced.key },
  } as const;
  return Object.freeze({
    header: block.header,
    nativeTxCompactCbor: evidence.nativeTxCompactCbor,
    witnessSetCompactCbor: evidence.witnessSetCompactCbor,
    forcedHeader: block.header,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
    forcedDirection:
      evidence.finding.direction === "wrongfulRejection" ? 1n : 0n,
  });
};
