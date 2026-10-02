import "./local-mutation-job-abandonment.killed-process-startup-recovery.js";

import { randomUUID } from "node:crypto";

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
import * as Authority from "../src/database/eventHistoryAuthority.js";
import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as StateQueueLeases from "../src/database/stateQueueMutationLeases.js";
import { TIMEOUT_CORRECTION_LEASE_HOLDER } from "../src/fibers/attestation-timeout-correction.reconcile-state-queue-corrections.js";
import { Database } from "../src/services/database.js";
import { HistoryPreparation } from "../src/services/event-history-recovery.js";
import {
  failedLocalJob,
  header,
  isolatedDb,
  localJobId,
  observedJournal,
} from "./local-mutation-job-abandonment.journal-fixture.js";

const L = StateQueueLeases.Columns;

/** No lease survives from an earlier test; the table allows one active. */
const leaseDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  isolatedDb(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`TRUNCATE TABLE state_queue_mutation_leases`;
      return yield* effect;
    }),
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
    "retires only under the current history authority, never under a superseded or absent one",
    () =>
      leaseDb(
        Effect.gen(function* () {
          const token = yield* leftBehind("block_commitment");
          const ownerToken = randomUUID();
          const acquire = Authority.acquire({
            deploymentIdentity: "ab".repeat(32),
            ownerToken,
            leaseDurationMs: 60_000,
          });
          const stale = yield* acquire;
          const current = yield* acquire;
          const releaseAs = (authority: Authority.Token | undefined) =>
            Effect.exit(
              authority === undefined
                ? releaseStateQueueLeasesOfPreviousNodeProcess
                : releaseStateQueueLeasesOfPreviousNodeProcess.pipe(
                    Effect.provideService(HistoryPreparation, {
                      token: authority,
                      assertCurrent: Effect.void,
                    }),
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
          const sql = yield* SqlClient.SqlClient;
          yield* sql`TRUNCATE TABLE event_history_authority`;
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
          // As failed-local-finalization-correction-emulator calls it while a
          // runtime is live: a bare database layer, with no startup
          // preparation, producer permit or fixture gate.
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
