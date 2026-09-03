import { deriveMidgardNativeTxFaultEvidenceMaterial } from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  type EventKey,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification-v1.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
  type DistinctAssetAccumulationEvidence,
  distinctAssetAccumulationEvidenceCloses,
  type DistinctAssetAccumulationFinding,
  prepareDistinctAssetAccumulationEvidence,
} from "./family-v1.js";
import type { DistinctAssetAccumulationActuationArtifact } from "./production-actuator-v1.js";
import {
  buildDistinctAssetAuthenticationFromRetainedDa,
  discoverDistinctAssetRetainedMutationCandidates,
} from "./retained-value-and-mint-v1.js";

const entries = (block: CanonicalBlockEvidence) => ({
  traces: block.reconstruction.payload.block_body.validation_traces.map(
    ([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }),
  ),
  witnesses:
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
});

const eventIsAccepted = (
  eventKey: EventKey,
): eventKey is Extract<EventKey, { L2TransactionEventKey: unknown }> =>
  "L2TransactionEventKey" in eventKey;

const eventIsForced = (
  eventKey: EventKey,
): eventKey is Extract<EventKey, { ForcedTransactionEventKey: unknown }> =>
  "ForcedTransactionEventKey" in eventKey;

const coordinateLabel = (finding: DistinctAssetAccumulationFinding): string => {
  const coordinate = finding.coordinate;
  return coordinate.kind === "input"
    ? `input:${coordinate.inputIndex.toString()}:${coordinate.assetIndex.toString()}`
    : coordinate.kind === "output"
      ? `output:${coordinate.outputIndex.toString()}:${coordinate.assetIndex.toString()}`
      : `mint:${coordinate.mintIndex.toString()}`;
};

const rejectionReasonMatches = (
  finding: DistinctAssetAccumulationFinding,
): boolean => {
  const reason = finding.subject.rejection_reason;
  const coordinate = finding.coordinate;
  if (reason === null || typeof reason === "string") return false;
  if (coordinate.kind === "input")
    return (
      "InputAssetAccumulationLimit" in reason &&
      reason.InputAssetAccumulationLimit.input_index ===
        BigInt(coordinate.inputIndex) &&
      reason.InputAssetAccumulationLimit.asset_index ===
        BigInt(coordinate.assetIndex)
    );
  if (coordinate.kind === "output")
    return (
      "OutputAssetAccumulationLimit" in reason &&
      reason.OutputAssetAccumulationLimit.output_index ===
        BigInt(coordinate.outputIndex) &&
      reason.OutputAssetAccumulationLimit.asset_index ===
        BigInt(coordinate.assetIndex)
    );
  return (
    "MintAssetAccumulationLimit" in reason &&
    reason.MintAssetAccumulationLimit.mint_index ===
      BigInt(coordinate.mintIndex)
  );
};

const evidenceFromCandidate = (
  finding: DistinctAssetAccumulationFinding,
  candidate: ReturnType<
    typeof discoverDistinctAssetRetainedMutationCandidates
  >[number],
): DistinctAssetAccumulationEvidence => {
  const accumulator = candidate.control.value_accumulator;
  const mutation = candidate.action.evidence.mutation;
  const seen = Number(accumulator.seen_asset_count);
  const nonzero = Number(accumulator.nonzero_asset_count);
  if (!Number.isSafeInteger(seen) || !Number.isSafeInteger(nonzero))
    throw new Error("distinctAssetAccumulationLimit accumulator changed");
  const fault = !mutation.delta_was_present && seen >= 16_384;
  const nextSeen = seen + (mutation.delta_was_present ? 0 : 1);
  return prepareDistinctAssetAccumulationEvidence({
    finding,
    traceStateHashHex: candidate.traceStateHashHex,
    workRootHex: candidate.workRootHex,
    pre: {
      assetRootHex: accumulator.asset_root,
      seenAssetCount: seen,
      nonzeroAssetCount: nonzero,
      cursor:
        finding.coordinate.kind === "input"
          ? finding.coordinate.assetIndex
          : finding.coordinate.kind === "output"
            ? finding.coordinate.assetIndex
            : finding.coordinate.mintIndex,
    },
    post: fault
      ? null
      : {
          // The authenticated mutation proof, rather than this diagnostic
          // summary, binds the successor root on-chain.
          assetRootHex: accumulator.asset_root,
          seenAssetCount: nextSeen,
          nonzeroAssetCount: Math.min(nextSeen, Math.max(0, nonzero)),
          cursor:
            (finding.coordinate.kind === "input"
              ? finding.coordinate.assetIndex
              : finding.coordinate.kind === "output"
                ? finding.coordinate.assetIndex
                : finding.coordinate.mintIndex) + 1,
        },
    mutationWasPresent: mutation.delta_was_present,
  });
};

type Candidate = Readonly<{
  detection: CanonicalViolationDetection;
  artifact: DistinctAssetAccumulationActuationArtifact;
}>;

const replayCandidates = async (
  block: CanonicalBlockEvidence,
): Promise<readonly Candidate[]> => {
  const retained = entries(block);
  const discovered = discoverDistinctAssetRetainedMutationCandidates(
    retained.witnesses,
  );
  const acceptedTrie = await buildTrieView(
    block.transactions.map((transaction) => ({
      key: Buffer.from(transaction.nodeTxId, "hex"),
      value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
    })),
  );
  const candidates: Candidate[] = [];
  for (const candidate of discovered) {
    let finding: DistinctAssetAccumulationFinding;
    let accepted:
      | DistinctAssetAccumulationActuationArtifact["accepted"]
      | undefined;
    let forcedSource:
      | DistinctAssetAccumulationActuationArtifact["forcedSource"]
      | undefined;
    let position: number;
    if (eventIsAccepted(candidate.eventKey)) {
      const txId = candidate.eventKey.L2TransactionEventKey.tx_id;
      position = block.transactions.findIndex(
        (transaction) => transaction.nodeTxId === txId,
      );
      const transaction = block.transactions[position];
      if (position < 0 || transaction === undefined) continue;
      finding = {
        subject: acceptedVerdictSubject(txId),
        coordinate: candidate.coordinate,
      };
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(transaction.txCbor, "hex"),
      );
      accepted = {
        txInclusion: parseSubmitStep01TxInclusion({
          nativeTxId: txId,
          nativeTx: nativeTxFromCoreCompact(material.compact),
          nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
          l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
          transactionsPhasRoot: acceptedTrie.root,
          txMembershipProofCbor: requireProof(
            acceptedTrie,
            Buffer.from(txId, "hex"),
            "distinct-asset-accumulation transaction",
          ),
        }),
        validationTracesRoot: block.header.validationTracesRoot,
        validationTraceCount: block.header.validationTraceCount,
      };
    } else if (eventIsForced(candidate.eventKey)) {
      const sourceKey =
        candidate.eventKey.ForcedTransactionEventKey.tx_order_id;
      const forcedPosition = block.reconstruction.forcedTransactions.findIndex(
        ({ key }) =>
          key.transactionId === sourceKey.transactionId &&
          key.outputIndex === sourceKey.outputIndex,
      );
      const transaction =
        block.reconstruction.forcedTransactions[forcedPosition];
      if (forcedPosition < 0 || transaction === undefined) continue;
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") continue;
      finding = {
        subject: forcedVerdictSubject({
          transactionId: transaction.value.tx_id,
          sourceKey: transaction.key,
          rejectionReason: verdict.ForcedTxInvalid.reason,
        }),
        coordinate: candidate.coordinate,
      };
      if (!rejectionReasonMatches(finding)) continue;
      position = block.transactions.length + forcedPosition;
      forcedSource = {
        header: {
          ...block.header,
          validation_traces_root: block.header.validationTracesRoot,
          validation_trace_count: block.header.validationTraceCount,
        },
        membership: await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey: candidate.eventKey,
        }),
        direction: finding.subject.direction,
      };
    } else continue;
    const evidence = evidenceFromCandidate(finding, candidate);
    if (!distinctAssetAccumulationEvidenceCloses(evidence)) continue;
    const authentication = await buildDistinctAssetAuthenticationFromRetainedDa(
      {
        eventKey: candidate.eventKey,
        finding,
        authenticatedValidationTraceEntries: retained.traces,
        retainedValidationWitnessEntries: retained.witnesses,
        expectedValidationTracesRoot: block.header.validationTracesRoot,
      },
    );
    const label = coordinateLabel(finding);
    const detection: CanonicalViolationDetection = {
      detectionId: `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID}:${position.toString()}:${finding.subject.transaction_id}:${label}`,
      headerHash: block.headerHash,
      violationId: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_ID,
      position: BigInt(position),
      diagnostic: `${finding.subject.source_kind === 0n ? "accepted" : "forced"} distinct-asset accumulation contradiction at ${label}`,
    };
    candidates.push({
      detection,
      artifact: Object.freeze({
        headerHash: block.headerHash,
        finding,
        evidence,
        ...(accepted === undefined ? {} : { accepted }),
        ...(forcedSource === undefined ? {} : { forcedSource }),
        authentication: authentication.authentication,
        folds: authentication.folds,
      }),
    });
  }
  return Object.freeze(
    candidates.sort(
      (left, right) =>
        Number(left.detection.position - right.detection.position) ||
        left.detection.detectionId.localeCompare(right.detection.detectionId),
    ),
  );
};

/** Complete canonical accepted/forced category-35 detector. */
export const detectDistinctAssetAccumulationCanonicalViolations = async (
  block: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> =>
  Object.freeze(
    (await replayCandidates(block)).map(({ detection }) => detection),
  );

/** First deterministic production artifact, wholly reconstructed from L1+DA. */
export const prepareDistinctAssetAccumulationArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<DistinctAssetAccumulationActuationArtifact> => {
  const candidate = (await replayCandidates(block))[0];
  if (candidate === undefined)
    throw new Error(
      "distinctAssetAccumulationLimit complete replay found no canonical violation",
    );
  return candidate.artifact;
};
