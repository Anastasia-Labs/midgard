import { EMPTY_MERKLE_TREE_ROOT } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  type CheckedLedger,
  type LedgerAt,
  predecessorLedger,
} from "../ledger-at.js";
import { detectNetworkIdAcceptedForced } from "../network-id/accepted-forced.js";
import { findNetworkIdFaults } from "../network-id/evidence.js";
import { detectNetworkIdWrongfulRejections } from "../network-id/wrongful-rejection.js";
import { decodeTransactionMaterial } from "../prepare-double-spend.js";
import { ledgerKeyBytesHex } from "../step-support.js";
import {
  type CanonicalViolationDetection,
  DOUBLE_SPEND_VIOLATION_ID,
  NETWORK_ID_VIOLATION_ID,
} from "./classification.js";
import {
  COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
  type CompleteCanonicalReplayContext,
  type CompleteCanonicalReplayDecision,
  type CompleteCanonicalReplayPredecessor,
  predecessorLedgerByAuthority,
  requireReplayPredecessorLedger,
} from "./complete-replay.replay-context-identity.js";
import { acceptedTransactionSubject, subjectOf } from "./detection-subject.js";

/**
 * Admits the ledger the challenged block starts from: `ledgerAt` is asked for
 * the ledger after the challenged header's `prev_header_hash`, and the answer
 * is refused unless it opens the header's `prev_utxos_root`. The returned
 * authority is non-revivable: only this module maps it to its ledger.
 */
export const admitCompleteCanonicalReplayPredecessor = async ({
  ledgerAt,
  currentEvidence,
}: {
  readonly ledgerAt: LedgerAt;
  readonly currentEvidence: CanonicalBlockEvidence;
}): Promise<CompleteCanonicalReplayPredecessor> => {
  const ledger = await predecessorLedger({
    prevHeaderHash: currentEvidence.header.prevHeaderHash,
    prevUtxosRoot: currentEvidence.header.prevUtxosRoot,
    ledgerAt,
    label: "complete replay predecessor ledger",
  });
  const authority: CompleteCanonicalReplayPredecessor = Object.freeze({
    schemaVersion: COMPLETE_CANONICAL_REPLAY_PREDECESSOR,
    challengedHeaderHash: currentEvidence.headerHash,
    headerHash: currentEvidence.header.prevHeaderHash,
    utxosRoot: ledger.utxosRoot,
  });
  predecessorLedgerByAuthority.set(authority, ledger);
  return authority;
};

/**
 * The checked ledger the challenged block starts from, or `undefined` when the
 * context admitted no predecessor (then only the empty genesis ledger is
 * available, see `requireLedgerOpening`). Production family preparers use this
 * instead of accepting a caller-supplied ledger.
 */
export const completeCanonicalReplayPredecessorLedger = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CheckedLedger | undefined =>
  requireReplayPredecessorLedger({ evidence, context });

type CanonicalReplayJson =
  | null
  | string
  | readonly CanonicalReplayJson[]
  | { readonly [key: string]: CanonicalReplayJson };

export const canonicalizeReplayJson = (
  value: CanonicalReplayJson,
): CanonicalReplayJson => {
  if (Array.isArray(value)) return value.map(canonicalizeReplayJson);
  if (typeof value !== "object" || value === null) return value;
  return Object.freeze(
    Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([key, child]) => [key, canonicalizeReplayJson(child)]),
    ),
  );
};

export const replayDecisionJson = (
  replay: CompleteCanonicalReplayDecision,
): CanonicalReplayJson => ({
  replayVersion: replay.replayVersion,
  launchScope: replay.launchScope,
  headerHash: replay.headerHash,
  payloadEnvelopeSha256: replay.payloadEnvelopeSha256,
  payloadSha256: replay.payloadSha256,
  context: replay.context,
  detections: replay.detections.map((detection) => ({
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position.toString(),
    diagnostic: detection.diagnostic ?? null,
  })),
});

const outputReferenceKey = (input: {
  readonly transactionId: string;
  readonly outputIndex: bigint;
}): string => `${input.transactionId}#${input.outputIndex.toString()}`;

export const detectDoubleSpends = async (
  evidence: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const firstSpendByInput = new Map<
    string,
    { readonly transactionIndex: number; readonly transactionId: string }
  >();
  const detections: CanonicalViolationDetection[] = [];
  for (const [transactionIndex, transaction] of transactions.entries()) {
    const seenInTransaction = new Set<string>();
    for (const [inputIndex, input] of transaction.inputs.entries()) {
      const inputKey = outputReferenceKey(input);
      if (seenInTransaction.has(inputKey)) continue;
      seenInTransaction.add(inputKey);
      const first = firstSpendByInput.get(inputKey);
      if (first === undefined) {
        firstSpendByInput.set(inputKey, {
          transactionIndex,
          transactionId: transaction.nodeTxId,
        });
        continue;
      }
      if (first.transactionId === transaction.nodeTxId) continue;
      detections.push({
        ...acceptedTransactionSubject(
          first.transactionId,
          transaction.nodeTxId,
        ),
        detectionId: [
          DOUBLE_SPEND_VIOLATION_ID,
          first.transactionIndex.toString(),
          transactionIndex.toString(),
          inputIndex.toString(),
          inputKey,
        ].join(":"),
        headerHash: evidence.headerHash,
        violationId: DOUBLE_SPEND_VIOLATION_ID,
        position: BigInt(transactionIndex),
        diagnostic: `transactions ${first.transactionId} and ${transaction.nodeTxId} spend ${inputKey}`,
      });
    }
  }
  return detections;
};

export const PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID =
  "authenticated-predecessor-context-unavailable" as const;

const predecessorLedgerKeys = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): ReadonlySet<string> | null => {
  const predecessor = requireReplayPredecessorLedger({ evidence, context });
  if (predecessor !== undefined) return new Set(predecessor.outputs.keys());
  return evidence.header.prevUtxosRoot === EMPTY_MERKLE_TREE_ROOT
    ? new Set<string>()
    : null;
};

export const detectLedgerRelativeMissingInputs = async ({
  evidence,
  context,
  kind,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
  readonly kind: "spend" | "reference";
}): Promise<readonly CanonicalViolationDetection[]> => {
  const transactions = await Promise.all(
    evidence.transactions.map(decodeTransactionMaterial),
  );
  const currentTransactionIds = new Set(
    transactions.map((transaction) => transaction.nodeTxId),
  );
  const predecessorLedger = predecessorLedgerKeys({ evidence, context });
  const violationId =
    kind === "spend" ? "non-existent-input" : "no-reference-input";
  const detections: CanonicalViolationDetection[] = [];
  for (const [transactionIndex, transaction] of transactions.entries()) {
    if (transaction.nativeTxCompact.validity_code !== 0n) continue;
    const inputs =
      kind === "spend" ? transaction.inputs : transaction.referenceInputs;
    for (const [inputIndex, input] of inputs.entries()) {
      if (currentTransactionIds.has(input.transactionId)) continue;
      const inputKey = ledgerKeyBytesHex({
        tx_id: input.transactionId,
        output_index: input.outputIndex,
      });
      if (predecessorLedger === null) {
        detections.push({
          ...acceptedTransactionSubject(transaction.nodeTxId),
          detectionId: `${PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID}:${kind}:${transactionIndex.toString()}:${inputIndex.toString()}:${transaction.nodeTxId}:${inputKey}`,
          headerHash: evidence.headerHash,
          violationId: PREDECESSOR_CONTEXT_UNAVAILABLE_VIOLATION_ID,
          position: BigInt(transactionIndex),
          diagnostic: `${kind} input ${inputIndex.toString()} requires the exact authenticated predecessor ledger before classification`,
        });
      } else if (!predecessorLedger.has(inputKey)) {
        detections.push({
          ...acceptedTransactionSubject(transaction.nodeTxId),
          detectionId: `${violationId}:${transactionIndex.toString()}:${inputIndex.toString()}:${transaction.nodeTxId}:${inputKey}`,
          headerHash: evidence.headerHash,
          violationId,
          position: BigInt(transactionIndex),
          diagnostic: `accepted transaction ${transaction.nodeTxId} ${kind} input ${inputIndex.toString()} is absent from both the current transaction set and predecessor ledger`,
        });
      }
    }
  }
  return detections;
};

export const detectNetworkIds = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => [
  ...detectAcceptedNetworkIds(evidence),
  ...(evidence.header.expectedNetworkId === 0n ||
  evidence.header.expectedNetworkId === 1n
    ? detectNetworkIdAcceptedForced({
        block: evidence,
        expectedNetworkId: evidence.header.expectedNetworkId,
      }).map((detection) => ({
        ...subjectOf(detection),
        detectionId: detection.detectionId,
        headerHash: detection.headerHash,
        violationId: detection.violationId,
        position: detection.position,
        diagnostic: `accepted forced transaction ${detection.transactionId} carries a foreign network at its ${detection.faultClaim.kind} claim`,
      }))
    : []),
  ...detectNetworkIdWrongfulRejectionsForHeader(evidence).map((detection) => ({
    ...subjectOf(detection),
    detectionId: detection.detectionId,
    headerHash: detection.headerHash,
    violationId: detection.violationId,
    position: detection.position,
    diagnostic: `forced transaction ${detection.transactionId} was rejected for NetworkIdMismatch despite every authenticated network id agreeing with the committed expectation`,
  })),
];

/**
 * The forced direction argues about a rejection typed `NetworkIdMismatch`, and
 * that argument only exists against a committed expectation the ledger can
 * actually carry. A header expecting anything but mainnet/testnet is a
 * different fault, so it contributes no wrongful-rejection detection here.
 */
const detectNetworkIdWrongfulRejectionsForHeader = (
  evidence: CanonicalBlockEvidence,
) => {
  const expectedNetworkId = evidence.header.expectedNetworkId;
  return expectedNetworkId === 0n || expectedNetworkId === 1n
    ? detectNetworkIdWrongfulRejections({
        block: evidence,
        expectedNetworkId,
      })
    : [];
};

const detectAcceptedNetworkIds = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  evidence.transactions.flatMap((transaction, transactionIndex) =>
    findNetworkIdFaults({
      evidence: {
        source: "retained-da",
        evidenceSourceId: evidence.provenance.da.sourceId,
        nativeTxCanonicalCbor: transaction.txCbor,
      },
      expectedNetworkId: evidence.header.expectedNetworkId,
    }).map((fault) => ({
      ...acceptedTransactionSubject(transaction.nodeTxId),
      detectionId:
        fault.kind === "transaction-network"
          ? `${NETWORK_ID_VIOLATION_ID}:${transactionIndex.toString()}:transaction`
          : `${NETWORK_ID_VIOLATION_ID}:${transactionIndex.toString()}:output:${fault.outputIndex.toString()}`,
      headerHash: evidence.headerHash,
      violationId: NETWORK_ID_VIOLATION_ID,
      position: BigInt(transactionIndex),
      diagnostic:
        fault.kind === "transaction-network"
          ? `transaction ${transaction.nodeTxId} carries the wrong explicit network id`
          : `transaction ${transaction.nodeTxId} output ${fault.outputIndex.toString()} carries the wrong network id`,
    })),
  );
