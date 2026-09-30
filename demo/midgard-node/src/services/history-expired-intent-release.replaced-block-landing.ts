import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  CANONICAL_COVERAGE_UNAVAILABLE,
  loadCanonicalHistoryCoverage,
} from "../database/eventHistoryCanonicalCoverage.js";
import type { Checkpoint } from "../database/eventHistoryJournal.js";
import * as Pending from "../database/pendingBlockFinalizations.js";
import { DatabaseError } from "../database/utils/common.js";
import { type EventHistorySourceBinding } from "../l1-event-history-source.js";
import { journalAbandonment } from "./canonical-journal-recovery.js";
import {
  type QueueNode,
  type QueueView,
} from "./history-expired-intent-release.signed-commit-node.js";
import { C } from "./history-expired-intent-release.table.js";
import { loadStateQueueCorrectionObserverState } from "./state-queue-correction-rewind.js";

/** Which of `txHashes` the retained canonical history (complete transaction
 * rosters from its anchor to the checkpoint head) includes as valid (inputs
 * spending) transactions. Unavailable coverage is no evidence (none is
 * included); any other failure (a database error, which aborts the owned
 * transaction) fails the attempt, which recovery retries. */
export const includedInCanonicalHistory = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
  txHashes: readonly string[],
) =>
  txHashes.length === 0
    ? Effect.succeed<ReadonlySet<string>>(new Set())
    : loadCanonicalHistoryCoverage(binding, checkpoint).pipe(
        Effect.map((coverage): ReadonlySet<string> => {
          const wanted = new Set(txHashes);
          const included = new Set<string>();
          for (const block of coverage.blocks)
            for (const tx of block.transactions)
              if (tx.spends === "inputs" && wanted.has(tx.txHash))
                included.add(tx.txHash);
          return included;
        }),
        Effect.catchIf(
          (cause) =>
            cause instanceof DatabaseError &&
            cause.message === CANONICAL_COVERAGE_UNAVAILABLE,
          (cause) =>
            Effect.logDebug(
              `Canonical history coverage unavailable as signed-intent evidence: ${formatUnknownError(cause)}`,
            ).pipe(Effect.as<ReadonlySet<string>>(new Set())),
        ),
      );

type ObserverView = Effect.Effect.Success<
  ReturnType<typeof loadStateQueueCorrectionObserverState>
>;

/** Authenticated evidence that this node's replaced block `record` landed:
 * its node on the exact-point queue (returned), the confirmed state equal to
 * its header, its signed commit in the journaled canonical history, or an
 * admitted (final) state-queue transition that merged it or saw it on the
 * queue. A block a recorded correction removed never counts: its members stay
 * reopened, which is what that correction's path does anyway. */
export const replacedBlockLanding = (
  record: Pending.Record,
  queue: QueueView,
  observer: ObserverView,
  canonicalHistory: ReadonlySet<string>,
): { onQueue: QueueNode | undefined; evidence: string } | undefined => {
  const header = record[C.HEADER_HASH].toString("hex");
  const pending = observer.kind === "observed" ? observer.state.pending : [];
  const admitted = observer.kind === "observed" ? observer.state.admitted : [];
  if (
    [...pending, ...admitted].some(
      (transition) =>
        transition.transitionKind !== "merge" &&
        transition.removedHeaderHashes.includes(header),
    )
  )
    return undefined;
  const onQueue = queue.nodes.find(
    (entry) => entry.headerHash === header && entry !== queue.root,
  );
  if (onQueue !== undefined)
    return { onQueue, evidence: "its node is on the queue" };
  const signed = record[C.INTENDED_TX_HASH]?.toString("hex");
  const seen = admitted.find(
    (transition) =>
      (transition.transitionKind === "merge" &&
        transition.removedHeaderHashes.includes(header)) ||
      [...transition.previousQueue, ...transition.nextQueue].some(
        (node) => node.headerHash === header,
      ),
  );
  const evidence =
    queue.root.headerHash === header
      ? "the confirmed state is its header"
      : signed !== undefined && canonicalHistory.has(signed)
        ? "its signed commit is in the journaled canonical history"
        : seen !== undefined
          ? `admitted state-queue transition ${seen.transactionHash} saw it on the queue`
          : undefined;
  return evidence === undefined ? undefined : { onQueue: undefined, evidence };
};

/** This node's journals built on the same base output as `record` (so their
 * commits spend what its commit spends) that were abandoned for replacement.
 * Sorted by header. */
export const replacedSiblings = (record: Pending.Record) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ header_hash: Buffer }>`SELECT header_hash
      FROM pending_block_finalizations
      WHERE base_tail_out_ref = ${record[C.BASE_TAIL_OUT_REF]}
        AND status = ${Pending.Status.Abandoned}
        AND header_hash <> ${record[C.HEADER_HASH]}
      ORDER BY header_hash`;
    const siblings: Pending.Record[] = [];
    for (const row of rows) {
      const found = yield* Pending.retrieveByHeaderHash(row.header_hash);
      if (
        Option.isSome(found) &&
        journalAbandonment(found.value) === "replacement"
      )
        siblings.push(found.value);
    }
    return siblings;
  });

/** L1 order of two recorded state-queue transitions. */
export const chainOrder = (
  left: SDK.StateQueueAuthenticatedTransition,
  right: SDK.StateQueueAuthenticatedTransition,
) => {
  const block = BigInt(left.blockNo) - BigInt(right.blockNo);
  const index =
    block === 0n
      ? BigInt(left.transactionIndex) - BigInt(right.transactionIndex)
      : block;
  return index === 0n ? 0 : index < 0n ? -1 : 1;
};
