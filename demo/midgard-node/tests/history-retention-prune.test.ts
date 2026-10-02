import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  MutationJobsDB,
  StateQueueMutationLeasesDB,
} from "../src/database/index.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import { pruneFinalizedBeyondChallengeability } from "../src/database/pendingBlockFinalizations.retrieve-finalized-missing-da-payloads.js";
import {
  header,
  journalFixture,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { provideDatabaseLayers } from "./utils.js";

const DAY_MS = 24 * 60 * 60_000;
const DEPLOYMENT = Buffer.alloc(32, 7);

/** The journal prune with no authenticated deployment unless one is given:
 * nothing is then held for finality. */
const prune = (
  args: Omit<
    Parameters<typeof pruneFinalizedBeyondChallengeability>[0],
    "deploymentIdentityDigest"
  > & { readonly deploymentIdentityDigest?: Buffer },
) =>
  pruneFinalizedBeyondChallengeability({
    deploymentIdentityDigest: undefined,
    ...args,
  });

/** An authenticated `merged` terminal outcome for `headerHash`, at `blockNo`. */
const recordMergedOutcome = (headerHash: Buffer, blockNo: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bytes = (fill: number, length: number) =>
      Buffer.alloc(length, fill + blockNo);
    yield* sql`INSERT INTO da_payload_terminal_outcomes (
        header_hash, terminal_outcome, transition_kind,
        deployment_identity_digest, state_queue_policy_id,
        transaction_hash, block_hash, slot, block_no,
        transaction_index, chain_point_id, finality_depth,
        transition_digest, transition_record
      ) VALUES (
        ${headerHash}, 'merged', 'merge', ${DEPLOYMENT}, ${bytes(1, 28)},
        ${bytes(2, 32)}, ${bytes(3, 32)}, ${blockNo}, ${blockNo}, 0,
        ${bytes(4, 32)}, 3, ${bytes(5, 32)}, ${"{}"}
      )`;
  });

const run = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const clear = sql`TRUNCATE TABLE pending_block_finalizations,
          state_queue_mutation_leases, event_history_authority,
          local_mutation_jobs, da_payload_terminal_outcomes
          RESTART IDENTITY CASCADE`;
        yield* clear;
        // Never leave a seeded active lease behind for a later file on
        // this shard to find busy.
        return yield* effect.pipe(Effect.ensuring(Effect.orDie(clear)));
      }),
    ) as Effect.Effect<A, unknown, never>,
  );

type JournalSpec = {
  readonly label: string;
  readonly status: "finalized" | "abandoned" | "pending_submission";
  readonly endedAgoMs: number;
  /** The journal's confirmed-merge finalization job; a finalized journal's
   * merge has completed unless said otherwise. */
  readonly mergeJob?: "completed" | "running" | "none";
};

const recordMergeJob = (headerHash: Buffer, status: "completed" | "running") =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const jobId = MutationJobsDB.confirmedMergeFinalizationJobId(
      headerHash.toString("hex"),
    );
    yield* sql`INSERT INTO local_mutation_jobs (job_id, kind, status, completed_at)
      VALUES (${jobId}, 'confirmed_merge_finalization', ${status},
        ${status === "completed" ? sql`NOW()` : null})
      ON CONFLICT (job_id) DO UPDATE SET status = EXCLUDED.status,
        completed_at = EXCLUDED.completed_at`;
  });

/** One journal per spec, each moved to its status before the next is
 * prepared, ending `endedAgoMs` before now. */
const journals = (specs: readonly JournalSpec[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const spec of specs) {
      const headerHash = header(spec.label);
      yield* PendingBlockFinalizationsDB.preparePendingSubmission(
        journalFixture(headerHash),
      );
      yield* sql`UPDATE pending_block_finalizations
        SET status = ${spec.status},
          block_end_time = NOW() - make_interval(secs => ${spec.endedAgoMs / 1000})
        WHERE header_hash = ${headerHash}`;
      const mergeJob =
        spec.mergeJob ?? (spec.status === "finalized" ? "completed" : "none");
      if (mergeJob !== "none") yield* recordMergeJob(headerHash, mergeJob);
    }
  });

const remainingLabels = (labels: readonly string[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      header_hash: Buffer;
    }>`SELECT header_hash FROM pending_block_finalizations`;
    const present = new Set(rows.map((row) => row.header_hash.toString("hex")));
    return labels.filter((label) => present.has(header(label).toString("hex")));
  });

const ALL = [
  "old-plain",
  "old-confirmed-head",
  "old-live-queue",
  "old-abandoned",
  "old-pending",
  "recent-finalized",
];

describe("pruning finalized journals beyond challengeability", () => {
  it("removes only old finalized journals no exemption covers", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* journals([
          { label: "old-plain", status: "finalized", endedAgoMs: 10 * DAY_MS },
          {
            label: "old-confirmed-head",
            status: "finalized",
            endedAgoMs: 10 * DAY_MS,
          },
          {
            label: "old-live-queue",
            status: "finalized",
            endedAgoMs: 9 * DAY_MS,
          },
          {
            label: "old-abandoned",
            status: "abandoned",
            endedAgoMs: 10 * DAY_MS,
            // A completed merge job: only the status keeps it.
            mergeJob: "completed",
          },
          {
            label: "recent-finalized",
            status: "finalized",
            endedAgoMs: 60_000,
          },
          {
            label: "old-pending",
            status: "pending_submission",
            endedAgoMs: 10 * DAY_MS,
            // A completed merge job: only the status keeps it.
            mergeJob: "completed",
          },
        ]);
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view: {
            confirmedHeadHash: header("old-confirmed-head"),
            liveQueueHeaderHashes: [header("old-live-queue")],
          },
        });
        return { removed, remaining: yield* remainingLabels(ALL) };
      }),
    );
    expect(result.removed).toBe(1);
    expect(result.remaining).toEqual(ALL.filter((l) => l !== "old-plain"));
  });

  it("keeps the newest finalized journal even past the horizon, and works in batches", async () => {
    const labels = ["f1", "f2", "f3", "f4"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (20 - index) * DAY_MS,
          })),
        );
        const view = {
          confirmedHeadHash: header("not-journaled"),
          liveQueueHeaderHashes: [],
        };
        const cutoff = new Date(Date.now() - DAY_MS);
        const capped = yield* prune({
          challengeableCutoff: cutoff,
          view,
          batchLimit: 1,
          maxBatches: 2,
        });
        const afterCap = yield* remainingLabels(labels);
        const rest = yield* prune({
          challengeableCutoff: cutoff,
          view,
          batchLimit: 1,
        });
        const again = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        return {
          capped,
          afterCap,
          rest,
          again,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    // Oldest first, two batches of one.
    expect(result.capped).toBe(2);
    expect(result.afterCap).toEqual(["f3", "f4"]);
    expect(result.rest).toBe(1);
    expect(result.again).toBe(0);
    expect(result.remaining).toEqual(["f4"]);
  });

  it("keeps an old finalized journal until its confirmed merge has been folded locally", async () => {
    const labels = ["merge-running", "merge-unstarted", "newest"];
    const view = {
      confirmedHeadHash: header("not-journaled"),
      liveQueueHeaderHashes: [],
    };
    const cutoff = new Date(Date.now() - DAY_MS);
    const result = await run(
      Effect.gen(function* () {
        yield* journals([
          {
            label: "merge-running",
            status: "finalized",
            endedAgoMs: 10 * DAY_MS,
            mergeJob: "running",
          },
          {
            label: "merge-unstarted",
            status: "finalized",
            endedAgoMs: 9 * DAY_MS,
            mergeJob: "none",
          },
          { label: "newest", status: "finalized", endedAgoMs: 8 * DAY_MS },
        ]);
        const whileUnmerged = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        const keptWhileUnmerged = yield* remainingLabels(labels);
        yield* recordMergeJob(header("merge-running"), "completed");
        yield* recordMergeJob(header("merge-unstarted"), "completed");
        const onceMerged = yield* prune({
          challengeableCutoff: cutoff,
          view,
        });
        return {
          whileUnmerged,
          keptWhileUnmerged,
          onceMerged,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.whileUnmerged).toBe(0);
    expect(result.keptWhileUnmerged).toEqual(labels);
    expect(result.onceMerged).toBe(2);
    expect(result.remaining).toEqual(["newest"]);
  });

  it("keeps the journal DA retention holds for finality under the deployment", async () => {
    const labels = ["older-merge", "latest-final-merge", "newest"];
    const view = {
      confirmedHeadHash: header("not-journaled"),
      liveQueueHeaderHashes: [],
    };
    const cutoff = new Date(Date.now() - DAY_MS);
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label, index) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: (10 - index) * DAY_MS,
          })),
        );
        yield* recordMergedOutcome(header("older-merge"), 10);
        yield* recordMergedOutcome(header("latest-final-merge"), 11);
        const held = yield* prune({
          challengeableCutoff: cutoff,
          view,
          deploymentIdentityDigest: DEPLOYMENT,
        });
        const keptUnderDeployment = yield* remainingLabels(labels);
        const unheld = yield* prune({ challengeableCutoff: cutoff, view });
        return {
          held,
          keptUnderDeployment,
          unheld,
          remaining: yield* remainingLabels(labels),
        };
      }),
    );
    expect(result.held).toBe(1);
    expect(result.keptUnderDeployment).toEqual([
      "latest-final-merge",
      "newest",
    ]);
    // Without an authenticated deployment nothing is held for finality.
    expect(result.unheld).toBe(1);
    expect(result.remaining).toEqual(["newest"]);
  });

  it("removes nothing inside the challengeability horizon", async () => {
    const labels = ["in-window-1", "in-window-2"];
    const result = await run(
      Effect.gen(function* () {
        yield* journals(
          labels.map((label) => ({
            label,
            status: "finalized" as const,
            endedAgoMs: DAY_MS / 2,
          })),
        );
        const removed = yield* prune({
          challengeableCutoff: new Date(Date.now() - DAY_MS),
          view: {
            confirmedHeadHash: header("not-journaled"),
            liveQueueHeaderHashes: [],
          },
        });
        return { removed, remaining: yield* remainingLabels(labels) };
      }),
    );
    expect(result.removed).toBe(0);
    expect(result.remaining).toEqual(labels);
  });
});

const insertLease = (lease: {
  readonly token: string;
  readonly status: "active" | "released" | "failed";
  readonly acquiredAgoMs: number;
  readonly releasedAgoMs: number | null;
}) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO state_queue_mutation_leases
      (token, scope, holder, status, acquired_at, expires_at, released_at)
      VALUES (${lease.token}, 'state_queue', 'test', ${lease.status},
        NOW() - make_interval(secs => ${lease.acquiredAgoMs / 1000}),
        NOW() + INTERVAL '1 hour',
        ${
          lease.releasedAgoMs === null
            ? null
            : sql`NOW() - make_interval(secs => ${lease.releasedAgoMs / 1000})`
        })`;
  });

const leaseTokens = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    token: string;
  }>`SELECT token FROM state_queue_mutation_leases ORDER BY token`;
  return rows.map((row) => row.token);
});

describe("pruning ended state-queue mutation leases", () => {
  const seed = Effect.gen(function* () {
    // Three long-ended leases, the oldest by acquisition.
    for (const index of [0, 1, 2])
      yield* insertLease({
        token: `old-${index.toString()}`,
        status: index === 1 ? "failed" : "released",
        acquiredAgoMs: 30 * DAY_MS + index * 1_000,
        releasedAgoMs: 30 * DAY_MS,
      });
    // Ended inside the window: kept though it is not among the newest.
    yield* insertLease({
      token: "recently-ended",
      status: "released",
      acquiredAgoMs: 29 * DAY_MS,
      releasedAgoMs: 60_000,
    });
    // One hundred ended leases, newer by acquisition, all past the window.
    for (let index = 0; index < 100; index++)
      yield* insertLease({
        token: `recent-${index.toString().padStart(3, "0")}`,
        status: "released",
        acquiredAgoMs: 20 * DAY_MS - index * 1_000,
        releasedAgoMs: 15 * DAY_MS,
      });
    yield* insertLease({
      token: "active",
      status: "active",
      acquiredAgoMs: 1_000,
      releasedAgoMs: null,
    });
  });

  it("removes ended leases past the window outside the newest inspectable rows, never the active one", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* seed;
        const removed = yield* StateQueueMutationLeasesDB.pruneSettledLeases({
          olderThanMs: 7 * DAY_MS,
          batchLimit: 2,
        });
        return { removed, tokens: yield* leaseTokens };
      }),
    );
    // The newest 100 are the active lease and recent-001..recent-099, so
    // recent-000 falls out with the three old ones.
    expect(StateQueueMutationLeasesDB.INSPECTABLE_LEASE_ROWS).toBe(100);
    expect(result.removed).toBe(4);
    expect(result.tokens).toHaveLength(101);
    expect(result.tokens).toContain("active");
    expect(result.tokens).toContain("recently-ended");
    expect(result.tokens).not.toContain("recent-000");
    expect(result.tokens).toContain("recent-001");
    for (const old of ["old-0", "old-1", "old-2"])
      expect(result.tokens).not.toContain(old);
  });

  it("removes nothing when every ended lease is inside the window", async () => {
    const result = await run(
      Effect.gen(function* () {
        yield* seed;
        const removed = yield* StateQueueMutationLeasesDB.pruneSettledLeases({
          olderThanMs: 60 * DAY_MS,
        });
        return { removed, tokens: yield* leaseTokens };
      }),
    );
    expect(result.removed).toBe(0);
    expect(result.tokens).toHaveLength(105);
  });
});
