import { mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Exit } from "effect";
import { afterAll, beforeAll, describe, expect } from "vitest";

import * as MutationJobsDB from "../src/database/mutationJobs.js";
import * as PendingBlockFinalizationsDB from "../src/database/pendingBlockFinalizations.js";
import type { WorkerInput } from "../src/workers/utils/commit-block-header.js";
import { successfulLocalFinalizationRecoveryProgram } from "../src/workers/utils/commit-submission.successful-local-finalization-recovery-program.js";
import {
  manifestFixture,
  PRODUCER_PRIVATE_KEY_SOURCE,
} from "./da-payload-libp2p-producer.manifest-fixture.js";
import {
  header,
  isolatedDb,
  J,
  journalFixture,
  localJobId,
  readJob,
  startupGate,
  Status,
  withLogs,
} from "./local-mutation-job-abandonment.journal-fixture.js";

/**
 * A transient between the journal's markFinalized committing and the job's
 * markCompleted (a dropped connection, a lost COMMIT ack) leaves a Finalized
 * journal and a job that is not Completed. The fault is a trigger refusing
 * the job's completion while armed, so markFailed still records the failure,
 * as it does for a real blip that spares it.
 */
const armCompletionFault = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql.unsafe(`CREATE OR REPLACE FUNCTION lf_job_refuse_completion()
    RETURNS trigger LANGUAGE plpgsql AS $$
    BEGIN
      IF NEW.status = 'completed' THEN
        RAISE EXCEPTION 'injected transient: completion record lost';
      END IF;
      RETURN NEW;
    END $$`);
  yield* sql.unsafe(`DROP TRIGGER IF EXISTS lf_job_refuse_completion
    ON local_mutation_jobs`);
  yield* sql.unsafe(`CREATE TRIGGER lf_job_refuse_completion
    BEFORE UPDATE ON local_mutation_jobs
    FOR EACH ROW EXECUTE FUNCTION lf_job_refuse_completion()`);
});

const disarmCompletionFault = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql.unsafe(`DROP TRIGGER IF EXISTS lf_job_refuse_completion
    ON local_mutation_jobs`);
}).pipe(Effect.orDie);

/** Counts each local application of the block: finalizeCommittedBlockLocally
 * runs beforeTransactionsMpfReset and then resets the transactions MPF once
 * per run, after its finalization SQL committed. */
const applicationProbe = () => {
  const counts = { beforeReset: 0, reset: 0 };
  const transactionsMpf = {
    resetToEmpty: () =>
      Effect.sync(() => {
        counts.reset += 1;
      }),
  } as unknown as Parameters<
    typeof successfulLocalFinalizationRecoveryProgram
  >[0];
  const beforeReset = Effect.sync(() => {
    counts.beforeReset += 1;
  });
  return { counts, transactionsMpf, beforeReset };
};

/** An empty-block journal whose header hash is its header's own hash, as the
 * DA payload local finalization persists requires; `label` varies the
 * header. */
const selfHashedJournal = (label: string) =>
  Effect.gen(function* () {
    const fixture = journalFixture(Buffer.alloc(28));
    const blockHeader: SDK.Header = {
      ...(LucidData.from(
        fixture.headerCbor.toString("hex"),
        SDK.Header,
      ) as SDK.Header),
      prevHeaderHash: header(`prev:${label}`).toString("hex"),
    };
    const headerHash = Buffer.from(
      yield* SDK.hashBlockHeader(blockHeader),
      "hex",
    );
    yield* PendingBlockFinalizationsDB.preparePendingSubmission({
      ...fixture,
      headerHash,
      headerCbor: Buffer.from(
        LucidData.to(blockHeader as never, SDK.Header as never),
        "hex",
      ),
    });
    return headerHash;
  });

/** A submitted block awaiting stability, as the confirmation path leaves it
 * for local finalization. */
const observedSelfHashedJournal = (label: string) =>
  Effect.gen(function* () {
    const headerHash = yield* selfHashedJournal(label);
    yield* PendingBlockFinalizationsDB.markSubmitted(
      headerHash,
      Buffer.concat([headerHash, Buffer.alloc(4)]),
    );
    yield* PendingBlockFinalizationsDB.markObservedWaitingStability(
      headerHash,
      1n,
    );
    return headerHash;
  });

const workerInput = {
  data: { mempoolTxsCountSoFar: 0, sizeOfProcessedTxsSoFar: 0 },
} as unknown as WorkerInput;

const recover = (
  headerHash: Buffer,
  probe: ReturnType<typeof applicationProbe>,
) =>
  successfulLocalFinalizationRecoveryProgram(
    probe.transactionsMpf,
    [],
    [],
    headerHash.toString("hex"),
    workerInput,
    0,
    probe.beforeReset,
  );

const journalStatus = (headerHash: Buffer) =>
  PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash).pipe(
    Effect.map((journal) =>
      journal._tag === "Some"
        ? journal.value[PendingBlockFinalizationsDB.Columns.STATUS]
        : undefined,
    ),
  );

const withFreshPayloadTables = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  isolatedDb(effect.pipe(Effect.ensuring(disarmCompletionFault)));

/** The first attempt applies the block and finalizes the journal, then loses
 * its completion record. */
const finalizeLosingCompletion = (
  label: string,
  probe: ReturnType<typeof applicationProbe>,
) =>
  Effect.gen(function* () {
    const headerHash = yield* observedSelfHashedJournal(label);
    yield* armCompletionFault;
    const first = yield* Effect.exit(recover(headerHash, probe));
    yield* disarmCompletionFault;
    expect(Exit.isFailure(first)).toBe(true);
    expect(probe.counts).toEqual({ beforeReset: 1, reset: 1 });
    expect(yield* journalStatus(headerHash)).toBe(Status.LocallyApplied);
    const job = yield* readJob(localJobId(headerHash));
    expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Failed);
    expect(job[J.LAST_ERROR]).toContain("completion record lost");
    return headerHash;
  });

/** Local finalization seeds the DA publication outbox from the deployment
 * manifest, as the node does. */
const DA_ENV = [
  "MIDGARD_DEPLOYMENT_MANIFEST_PATH",
  "DA_LIBP2P_PRIVATE_KEY_SOURCE",
] as const;
let manifestDirectory: string;
const savedEnv: Partial<Record<(typeof DA_ENV)[number], string>> = {};
beforeAll(() => {
  manifestDirectory = mkdtempSync(join(tmpdir(), "lf-job-manifest-"));
  const manifestPath = join(manifestDirectory, "manifest.json");
  writeFileSync(manifestPath, JSON.stringify(manifestFixture()));
  for (const name of DA_ENV) savedEnv[name] = process.env[name];
  process.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH = manifestPath;
  process.env.DA_LIBP2P_PRIVATE_KEY_SOURCE = PRODUCER_PRIVATE_KEY_SOURCE;
});
afterAll(() => {
  for (const name of DA_ENV)
    if (savedEnv[name] === undefined) delete process.env[name];
    else process.env[name] = savedEnv[name];
  rmSync(manifestDirectory, { recursive: true, force: true });
});

describe("local finalization of a journal that is already finalized", () => {
  it.effect(
    "the next tick closes the job and reports success exactly once, without applying the block again",
    () =>
      withFreshPayloadTables(
        Effect.gen(function* () {
          const probe = applicationProbe();
          const done = yield* finalizeLosingCompletion(
            "finalized-completion-lost",
            probe,
          );

          const { result: second, logs } = yield* withLogs(
            recover(done, probe),
          );
          expect(second.type).toBe("SuccessfulLocalFinalizationRecoveryOutput");
          if (second.type === "SuccessfulLocalFinalizationRecoveryOutput") {
            expect(second.finalizedHeaderHash).toBe(done.toString("hex"));
            // The first attempt's ledger deletes already reached the parent
            // as the full reload a failed output triggers.
            expect(second.mempoolLedgerDeletedOutRefHexes).toEqual([]);
          }
          expect(probe.counts).toEqual({ beforeReset: 1, reset: 1 });
          expect(logs.join("\n")).toContain("already finalized");
          const job = yield* readJob(localJobId(done));
          expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Completed);
          expect(job[J.COMPLETED_AT]).not.toBeNull();
          expect(job[J.ATTEMPTS]).toBe(1);
          expect(yield* journalStatus(done)).toBe(Status.LocallyApplied);
          expect(yield* startupGate).toBeUndefined();
        }),
      ),
  );

  it.effect(
    "a restart after the lost completion record closes the failed job instead of refusing every startup",
    () =>
      withFreshPayloadTables(
        Effect.gen(function* () {
          const probe = applicationProbe();
          const done = yield* finalizeLosingCompletion(
            "finalized-completion-lost-restart",
            probe,
          );

          const { result, logs } = yield* withLogs(startupGate);
          expect(result).toBeUndefined();
          expect(logs.join("\n")).toContain(
            `Startup completed local mutation job ${localJobId(done)}`,
          );
          const job = yield* readJob(localJobId(done));
          expect(job[J.STATUS]).toBe(MutationJobsDB.Status.Completed);
          expect(probe.counts).toEqual({ beforeReset: 1, reset: 1 });
        }),
      ),
  );

  it.effect(
    "still refuses to finalize an abandoned or never-submitted journal, and leaves its job open",
    () =>
      withFreshPayloadTables(
        Effect.gen(function* () {
          const removed = yield* observedSelfHashedJournal(
            "abandoned-not-finalized",
          );
          yield* PendingBlockFinalizationsDB.markCorrectedAfterStateQueueRemoval(
            removed,
            "01".repeat(32),
          );
          const unsubmitted = yield* selfHashedJournal(
            "unsubmitted-not-finalized",
          );
          for (const [headerHash, status] of [
            [removed, Status.Abandoned],
            [unsubmitted, Status.PendingSubmission],
          ] as const) {
            const outcome = yield* Effect.exit(
              recover(headerHash, applicationProbe()),
            );
            expect(Exit.isFailure(outcome), status).toBe(true);
            expect(yield* journalStatus(headerHash)).toBe(status);
            expect(
              (yield* readJob(localJobId(headerHash)))[J.STATUS],
              status,
            ).toBe(MutationJobsDB.Status.Failed);
          }
          expect([...((yield* startupGate) ?? [])].sort()).toEqual(
            [localJobId(removed), localJobId(unsubmitted)].sort(),
          );
        }),
      ),
  );
});
