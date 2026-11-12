import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { Effect, Option } from "effect";

import {
  BlocksDB,
  PendingBlockFinalizationsDB,
  StateQueueMutationLeasesDB,
} from "../database/index.js";
import { mergeAction, type MergeActionResult } from "../fibers/merge.js";
import {
  Database,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import {
  evidence,
  optionRecordEvidence,
  type ReconciliationEvidence,
  type ReconciliationResult,
  type ReconciliationStatus,
  result,
} from "./reconcile.parse-reconciliation-result.js";
import { fetchCanonicalStateQueueHeaderHashes } from "./reconcile.reconcile-phas-registered-program.js";

export const reconcileMergeCompleteProgram = ({
  headerHash,
  repair,
}: {
  readonly headerHash: Buffer;
  readonly repair: boolean;
}): Effect.Effect<
  ReconciliationResult,
  unknown,
  Database | Lucid | MidgardContracts | Globals | NodeConfig | IntentJournal
> =>
  Effect.gen(function* () {
    const headerHashHex = headerHash.toString("hex");
    const target = { headerHash: headerHashHex };
    const observed = yield* observeMergeCompletion(headerHash);
    const repairActions: string[] = [];
    if (repair && observed.canonical) {
      // The state queue merges strictly oldest-first, so only the oldest queued
      // block can be the target of a merge.
      const oldest = observed.canonicalHeaders[0];
      if (oldest !== headerHashHex) {
        return result({
          milestone: "merge-complete",
          target,
          status: "blocked",
          evidence: mergeCompletionEvidence(observed),
          repairActions,
          nextAction: `Only the oldest queued block can be merged; reconcile merge-complete for ${String(oldest)} first.`,
        });
      }
      const attempt = yield* mergeAction(true, {
        expectedHeaderHash: headerHashHex,
      }).pipe(
        Effect.map((mergeResult) => ({ _tag: "Ran" as const, mergeResult })),
        Effect.catchTag("MergeProducerPermitUnavailable", (unavailable) =>
          Effect.succeed({ _tag: "PermitUnavailable" as const, unavailable }),
        ),
      );
      if (attempt._tag === "PermitUnavailable") {
        // Nothing ran: no L1 work and no local write happened.
        return result({
          milestone: "merge-complete",
          target,
          status: "blocked",
          evidence: [
            evidence("merge_producer_permit", {
              available: false,
              reason: `${attempt.unavailable.message}: ${formatUnknownError(
                attempt.unavailable.cause,
                {
                  includeCause: true,
                },
              )}`,
            }),
            ...mergeCompletionEvidence(observed),
          ],
          repairActions,
          nextAction:
            "A merge finalizes history rows and needs the running node's follower write gate. A standalone process cannot hold it; on the running node it is unavailable until the follower-change driver has published its view. Trigger the merge through the node's admin GET /merge endpoint, which merges the oldest queued block, once the node is ready.",
        });
      }
      const { mergeResult } = attempt;
      repairActions.push("merge_action");
      const after = yield* observeMergeCompletion(headerHash);
      const verdict = mergeCompletionVerdict(after);
      return result({
        milestone: "merge-complete",
        target,
        status:
          verdict.status === "satisfied"
            ? "repaired"
            : after.canonical
              ? "pending"
              : "failed",
        evidence: [
          mergeResultEvidence(mergeResult),
          ...mergeCompletionEvidence(after),
        ],
        repairActions,
        nextAction:
          verdict.status === "satisfied"
            ? null
            : after.canonical
              ? "Merge did not remove the header yet; inspect merge_result and the state-queue lease."
              : verdict.nextAction,
      });
    }

    const verdict = mergeCompletionVerdict(observed);
    return result({
      milestone: "merge-complete",
      target,
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
      repairActions,
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
        "HeaderV1 is still queued; run with --repair only after DA/finality gates are satisfied.",
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

/** The merge outcome as plain JSON: the reconciliation artifact rejects the
 * raw state-queue snapshot and absent optional fields. */
export const mergeResultEvidence = (
  mergeResult: MergeActionResult,
): ReconciliationEvidence =>
  evidence(
    "merge_result",
    mergeResult.status === "merged"
      ? {
          status: mergeResult.status,
          trigger: mergeResult.trigger,
          headerHash: mergeResult.headerHash,
          txHash: mergeResult.txHash,
          // The root and the blocks, as the artifact has always counted them.
          postMergeQueueNodeCount: mergeResult.postMergeSnapshot.blockCount + 1,
        }
      : Object.fromEntries(
          Object.entries(mergeResult).filter(
            ([, value]) => value !== undefined,
          ),
        ),
  );
