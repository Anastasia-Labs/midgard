import { Effect, Option } from "effect";

import {
  BlocksDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { Database, MidgardContracts } from "../services/index.js";
import {
  evidence,
  optionRecordEvidence,
  type ReconciliationEvidence,
  type ReconciliationResult,
  type ReconciliationStatus,
  result,
} from "./reconcile.parse-reconciliation-result.js";
import { fetchCanonicalStateQueueHeaderHashes } from "./reconcile.reconcile-phas-registered-program.js";

/**
 * Reports a header's merge completion; it never merges and writes nothing.
 * The running node's scheduled merge fiber merges the oldest queued block once
 * it is due and no halt holds the fiber (merging is permissionless on L1, so
 * any operator's merge completes it).
 */
export const reconcileMergeCompleteProgram = ({
  headerHash,
}: {
  readonly headerHash: Buffer;
}): Effect.Effect<ReconciliationResult, unknown, Database | MidgardContracts> =>
  Effect.gen(function* () {
    const observed = yield* observeMergeCompletion(headerHash);
    const verdict = mergeCompletionVerdict(observed);
    return result({
      milestone: "merge-complete",
      target: { headerHash: headerHash.toString("hex") },
      status: verdict.status,
      evidence: [
        ...mergeCompletionEvidence(observed),
        evidence(
          "state_queue_lease",
          StateQueueMutationLeasesDB.encodeInspectionJson(
            yield* StateQueueMutationLeasesDB.inspect(),
          ),
        ),
      ],
      nextAction: verdict.nextAction,
    });
  });

export type MergeCompletionObservation = {
  readonly canonicalHeaders: readonly string[];
  readonly canonical: boolean;
  readonly txCount: number;
  /** The derived status of the header's journal, if this node built it. */
  readonly status: PendingBlockFinalizationsDB.OwnBlockStatus | undefined;
  readonly journal: Option.Option<PendingBlockFinalizationsDB.Record>;
};

export const observeMergeCompletion = (headerHash: Buffer) =>
  Effect.gen(function* () {
    const canonicalHeaders = yield* fetchCanonicalStateQueueHeaderHashes;
    const txHashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
    return {
      canonicalHeaders,
      canonical: canonicalHeaders.includes(headerHash.toString("hex")),
      txCount: txHashes.length,
      status:
        yield* PendingBlockFinalizationsDB.retrieveStatusByHeaderHash(
          headerHash,
        ),
      journal:
        yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
    } satisfies MergeCompletionObservation;
  });

/**
 * A merge is complete only when the header has left the state queue AND its
 * block is folded into `confirmed_ledger` (its journal derives `folded`).
 * The block's rows prove neither: they stay until its fold is final
 * (`releaseFinalFolds`), well after its merge completed, and a block
 * without L2 transactions never had rows.
 */
export const mergeCompletionVerdict = (
  observed: MergeCompletionObservation,
): {
  readonly status: ReconciliationStatus;
  readonly nextAction: string | null;
} => {
  if (observed.canonical)
    return {
      status: "pending",
      nextAction:
        "HeaderV1 is still queued; the running node's scheduled merge fiber merges the oldest queued block once it is due and no halt holds the fiber. Merging is permissionless on L1, so another operator's merge completes it too.",
    };
  if (observed.status === PendingBlockFinalizationsDB.OwnBlockStatus.Folded)
    return { status: "satisfied", nextAction: null };
  if (
    observed.status === PendingBlockFinalizationsDB.OwnBlockStatus.Applied ||
    observed.status === PendingBlockFinalizationsDB.OwnBlockStatus.Landed
  )
    return {
      status: "blocked",
      nextAction: `The header left the state queue but its block is ${observed.status}, not folded; the running node's merge fiber finalizes a landed merge before every merge while the follower-change driver's view is published, and landed-block processing folds it in queue order. If it stays unfolded, inspect the node's landed-block holds.`,
    };
  return {
    status: "ambiguous",
    nextAction:
      observed.status === undefined
        ? "The header is not queued and this database holds no journal for it: another operator built it, it was merged before this database existed, or it was removed by state-queue correction; inspect node_confirmed_merges and node_landed_blocks."
        : "The header is not queued and its block is unlanded in this database: it was removed by state-queue correction, or the follower has not processed its landing yet; inspect node_landed_blocks and the queue terminals.",
  };
};

export const mergeCompletionEvidence = (
  observed: MergeCompletionObservation,
): readonly ReconciliationEvidence[] => [
  evidence("canonical_state_queue", {
    containsHeader: observed.canonical,
    headers: observed.canonicalHeaders,
  }),
  evidence("local_block_rows", { txCount: observed.txCount }),
  evidence("own_block_status", { status: observed.status ?? null }),
  optionRecordEvidence(observed.journal, observed.status),
];
