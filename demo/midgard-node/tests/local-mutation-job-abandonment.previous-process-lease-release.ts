import "./local-mutation-job-abandonment.killed-process-startup-recovery.js";

import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Cause, Effect, Exit } from "effect";
import { describe, expect } from "vitest";

import {
  assertStartupMutationJobsRecoverable,
  NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS,
  releaseStateQueueLeasesOfPreviousNodeProcess,
} from "../src/commands/listen-startup.js";
import {
  NODE_PROCESS_MPF_AUDIT_LEASES,
  OFFLINE_MPF_AUDIT_LEASES,
} from "../src/commands/mpf-audit-leases.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as StateQueueLeases from "../src/database/stateQueueMutationLeases.js";
import { TIMEOUT_CORRECTION_LEASE_HOLDER } from "../src/fibers/attestation-timeout-correction.reconcile-state-queue-corrections.js";
import { Database } from "../src/services/database.js";
import {
  type FollowerDriverPermit,
  FollowerDriverWrite,
} from "../src/services/follower-write-gate.js";
import { supersededDriver } from "./helpers/follower-write-gate.js";
import {
  failedLocalJob,
  header,
  isolatedDb,
  localJobId,
  observedJournal,
} from "./local-mutation-job-abandonment.journal-fixture.js";
import { resetApplicationTables } from "./utils.js";

const L = StateQueueLeases.Columns;

/** No lease survives from an earlier test (`isolatedDb` resets first); the
 * table allows one active. Assertions still observe intentionally abandoned
 * leases. No test owner survives this synchronous lease fixture when its
 * effect completes. */
const leaseDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  isolatedDb(
    effect.pipe(Effect.ensuring(Effect.orDie(resetApplicationTables))),
  );

/** A lease a killed process took and never released. */
const leftBehind = (holder: string) =>
  Effect.gen(function* () {
    const acquired = yield* StateQueueLeases.tryAcquire({ holder });
    if (acquired._tag !== "Acquired") throw new Error("lease busy");
    return acquired.token;
  });

const readLease = (token: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<StateQueueLeases.Entry>`SELECT * FROM
      state_queue_mutation_leases WHERE token = ${token}`;
    return rows[0]!;
  });

describe("startup retires state-queue leases of a killed node process", () => {
  it("names every holder the node's fibers take, and not the offline audit's", () => {
    expect([...NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS].sort()).toEqual(
      [
        "attestation_timeout_removal",
        "block_commitment",
        "node-mpf-payload-audit",
        "state_queue_merge",
      ].sort(),
    );
    expect(NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS).toContain(
      TIMEOUT_CORRECTION_LEASE_HOLDER,
    );
    expect(NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS).toContain(
      NODE_PROCESS_MPF_AUDIT_LEASES.stateQueueHolder,
    );
    expect(NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS).not.toContain(
      OFFLINE_MPF_AUDIT_LEASES.stateQueueHolder,
    );
  });

  for (const holder of NODE_PROCESS_STATE_QUEUE_LEASE_HOLDERS)
    it.effect(`retires a live ${holder} lease so commitment is not Busy`, () =>
      leaseDb(
        Effect.gen(function* () {
          const token = yield* leftBehind(holder);
          expect(
            (yield* StateQueueLeases.tryAcquire({ holder: "block_commitment" }))
              ._tag,
          ).toBe("Busy");
          expect(
            yield* releaseStateQueueLeasesOfPreviousNodeProcess,
          ).toHaveLength(1);
          const lease = yield* readLease(token);
          expect(lease[L.STATUS]).toBe(StateQueueLeases.Status.Failed);
          expect(lease[L.RELEASED_AT]).not.toBeNull();
          expect(lease[L.LAST_ERROR]).toContain("retired at startup");
          expect(
            (yield* StateQueueLeases.revalidate(token).pipe(Effect.exit))._tag,
          ).toBe("Failure");
          expect(
            (yield* StateQueueLeases.tryAcquire({ holder: "block_commitment" }))
              ._tag,
          ).toBe("Acquired");
        }),
      ),
    );

  it.effect(
    "keeps an mpf-payload-audit lease, which a live offline audit may hold",
    () =>
      leaseDb(
        Effect.gen(function* () {
          const token = yield* leftBehind(
            OFFLINE_MPF_AUDIT_LEASES.stateQueueHolder,
          );
          expect(yield* releaseStateQueueLeasesOfPreviousNodeProcess).toEqual(
            [],
          );
          expect((yield* readLease(token))[L.STATUS]).toBe(
            StateQueueLeases.Status.Active,
          );
          yield* StateQueueLeases.revalidate(token);
        }),
      ),
  );

  it.effect(
    "retires only under the current follower-change driver, never under a superseded one or a fixture",
    () =>
      leaseDb(
        Effect.gen(function* () {
          const token = yield* leftBehind("block_commitment");
          const { stale, current } = yield* supersededDriver;
          const releaseAs = (driver: FollowerDriverPermit | undefined) =>
            Effect.exit(
              driver === undefined
                ? releaseStateQueueLeasesOfPreviousNodeProcess
                : releaseStateQueueLeasesOfPreviousNodeProcess.pipe(
                    Effect.provideService(FollowerDriverWrite, driver),
                  ),
            );
          expect(Exit.isFailure(yield* releaseAs(undefined))).toBe(true);
          expect(Exit.isFailure(yield* releaseAs(stale))).toBe(true);
          expect((yield* readLease(token))[L.STATUS]).toBe(
            StateQueueLeases.Status.Active,
          );
          const released = yield* releaseAs(current);
          expect(Exit.isSuccess(released)).toBe(true);
          expect((yield* readLease(token))[L.STATUS]).toBe(
            StateQueueLeases.Status.Failed,
          );
        }),
      ),
  );

  it.effect(
    "leaves leases to the startup step: the gate needs no producer permit and retires nothing",
    () =>
      leaseDb(
        Effect.gen(function* () {
          const live = header("gate-without-permit");
          yield* observedJournal(live);
          yield* failedLocalJob(live, "delta invalid");
          const token = yield* leftBehind("block_commitment");
          // Called while a runtime is live, on a bare database layer: no
          // startup preparation, producer permit or fixture gate.
          const exit = yield* Effect.promise(() =>
            Effect.runPromiseExit(
              assertStartupMutationJobsRecoverable.pipe(
                Effect.zipRight(MutationJobsDB.retrieveUnfinished),
                Effect.provide(Database.layer),
              ),
            ),
          );
          if (Exit.isFailure(exit)) throw new Error(Cause.pretty(exit.cause));
          // The same database: the gate saw the failed job and handed it over.
          expect(
            exit.value.map((job) => job[MutationJobsDB.Columns.JOB_ID]),
          ).toEqual([localJobId(live)]);
          expect((yield* readLease(token))[L.STATUS]).toBe(
            StateQueueLeases.Status.Active,
          );
        }),
      ),
  );
});
