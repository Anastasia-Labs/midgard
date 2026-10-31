import { join } from "node:path";
import { setImmediate as yieldTurn } from "node:timers/promises";
import { Worker } from "node:worker_threads";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it } from "vitest";

import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import { unsafeOpenWatcherSqliteDurableBackendForTest } from "../../src/storage/sqlite-durable-backend.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture as setup,
} from "../support/fault-proof-funding-fixture.js";
import { prepareThroughPermit } from "./superseded-attempt-replacement.js";

// The workflow journal, the funding reservations and the durable state-queue
// observations share watcher.sqlite. Each writer takes the write lock up
// front and waits for it, so writers on the same file queue rather than fail.

afterEach(async () => {
  await cleanupFundingRecoveryFixtures();
});

const hex32 = (value: number): string => value.toString(16).padStart(64, "0");
const observation = (
  value: number,
  previousObservationDigest: string | null,
): WatcherAuthenticatedStateQueueObservation =>
  Object.freeze({
    schemaVersion: "midgard-watcher-production-state-queue-observation-v1",
    deploymentIdentityDigest: "11".repeat(32),
    protocolScriptAuthorityDigest: "12".repeat(32),
    stateQueuePolicyId: "13".repeat(28),
    hubOraclePolicyId: "14".repeat(28),
    nativePoint: Object.freeze({
      blockHash: hex32(value),
      parentBlockHash: value === 1 ? null : hex32(value - 1),
      slot: (1_000 + value).toString(),
      blockNo: (100 + value).toString(),
      chainPointId: hex32(10_000 + value),
      finalityDepth: "30",
    }),
    sourceId: "test-source",
    previousObservationDigest,
    checkpoints: Object.freeze([]),
    finalizedQueue: Object.freeze([]),
    finalizedHeaders: Object.freeze([]),
    finalizedCorrectionLock: null,
    correctionLockWitnesses: Object.freeze([]),
    observationDigest: hex32(20_000 + value),
  });

// Another process on the same file: it holds the write lock for 20 ms at a
// time until told to stop, and reports how many times it held it.
const LOCK_HOLDER = `
const { DatabaseSync } = require("node:sqlite");
const { parentPort, workerData } = require("node:worker_threads");
const database = new DatabaseSync(workerData.path);
database.exec("PRAGMA busy_timeout = 5000");
const stop = new Int32Array(workerData.stop);
const pause = new Int32Array(new SharedArrayBuffer(4));
let held = 0;
while (Atomics.load(stop, 0) === 0) {
  database.exec("BEGIN IMMEDIATE");
  Atomics.wait(pause, 0, 0, 20);
  database.exec("COMMIT");
  held += 1;
  Atomics.wait(pause, 0, 0, 5);
}
database.close();
parentPort.postMessage(held);
`;

it("prepares and confirms one attempt while the durable state-queue backend and another process write the same file", async () => {
  const fixture = await setup(true);
  fixture.useUnspentPendingInputs();
  const path = join(fixture.journalRoot, "watcher.sqlite");
  const backend = await unsafeOpenWatcherSqliteDurableBackendForTest(
    { path },
    () => undefined,
  );
  const stop = new SharedArrayBuffer(4);
  const holder = new Worker(LOCK_HOLDER, {
    eval: true,
    workerData: { path, stop },
  });
  const held = new Promise<number>((resolve, reject) => {
    holder.once("message", resolve);
    holder.once("error", reject);
  });
  let running = true;
  let observations = 0;
  const observing = (async () => {
    let previous: string | null = null;
    while (running) {
      observations += 1;
      const next = observation(observations, previous);
      expect(await backend.stateQueueObservations.append(next)).toBe(
        "appended",
      );
      previous = next.observationDigest;
      await yieldTurn();
    }
  })();
  // A failure surfaces where the loop is awaited.
  observing.catch(() => undefined);
  try {
    const attempt = await prepareThroughPermit(fixture);
    await yieldTurn();
    await fixture.run(await fixture.recover());
    const [record] = await fixture.records();
    expect(record).toMatchObject({ state: "active", pendingTransition: null });
    expect(record!.activeInputs.map(({ outRef }) => outRef)).toContain(
      `${attempt.transactionHash}#0`,
    );
    const events = (await fixture.journal.load(fixture.initial.workflowId)).map(
      ({ event }) => event,
    );
    expect(
      events.filter(({ kind }) => kind === "submission_intent"),
    ).toHaveLength(1);
    running = false;
    await observing;
    // Every observation appended meanwhile is in the chain.
    expect(observations).toBeGreaterThan(1);
    expect(await backend.stateQueueObservations.readAll()).toHaveLength(
      observations,
    );
  } finally {
    running = false;
    await observing.catch(() => undefined);
    Atomics.store(new Int32Array(stop), 0, 1);
    backend.close();
  }
  expect(await held).toBeGreaterThan(0);
}, 60_000);

it("commits the largest prepared body and ordinary entries within bounds", async () => {
  const fixture = await setup();
  const timed = async (run: () => Promise<void>) => {
    const started = performance.now();
    await run();
    return performance.now() - started;
  };
  // A prepared artifact of about 1.4 MB.
  const headerHash = "cd".repeat(28);
  const objective = { category: fixture.objective.category, headerHash };
  const identity = {
    ...fixture.initial.identity,
    target: { kind: "state_queue_header" as const, headerHash },
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const artifact = {
    chunks: Array.from({ length: 1_024 }, (_, index) =>
      index.toString(16).padStart(2, "0").repeat(512),
    ),
  };
  const entry = (
    sequence: number,
    event: FraudProofWorkflowJournalEntry["event"],
  ): FraudProofWorkflowJournalEntry => ({
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence,
    recordedAt: new Date().toISOString(),
    event,
  });
  const large = fixture.workflowJournal.forObjective(objective);
  await large.append(entry(0, { kind: "started" }), 0);
  const body = JSON.stringify(
    entry(1, {
      kind: "prepared",
      artifact,
      artifactDigest: journalJsonDigest(artifact),
    }),
  ).length;
  const prepared = await timed(() =>
    large.append(
      entry(1, {
        kind: "prepared",
        artifact,
        artifactDigest: journalJsonDigest(artifact),
      }),
      1,
    ),
  );
  expect(await large.load(workflowId)).toHaveLength(2);

  // Ordinary entries: one reconciliation each, each its own commit.
  const journal = fixture.workflowJournal.forObjective(fixture.objective);
  const from = (await journal.load(fixture.initial.workflowId)).length;
  const commits: number[] = [];
  for (let sequence = from; sequence < from + 200; sequence += 1)
    commits.push(
      await timed(() =>
        journal.append(
          {
            schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
            workflowId: fixture.initial.workflowId,
            identity: fixture.initial.identity,
            sequence,
            recordedAt: new Date().toISOString(),
            event: {
              kind: "reconciled",
              actionId: "init",
              outcome: "pending",
              txHash: fixture.transactionHash,
            },
          },
          sequence,
        ),
      ),
    );
  const p99 = commits.sort((left, right) => left - right)[
    Math.ceil(commits.length * 0.99) - 1
  ]!;
  process.stdout.write(
    `workflow journal commits: prepared body ${body.toString()} bytes in ${prepared.toFixed(1)} ms; p99 of ${commits.length.toString()} ordinary entries ${p99.toFixed(1)} ms\n`,
  );
  // Loose bounds: a commit of this size stays well below the busy timeout
  // other writers on the file wait for.
  expect(prepared).toBeLessThan(2_500);
  expect(p99).toBeLessThan(500);
}, 60_000);
