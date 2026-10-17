import { createHash } from "node:crypto";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, beforeEach, expect, it, vi } from "vitest";

import { state } from "./journey-mocks.js";
import { runAutonomousWatcherJourney } from "./journey-runner.js";
import {
  captureJourneyWorkflowBaseline,
  openJourneySession,
} from "./journey-session.js";

beforeEach(async () => {
  state.runDirectory = await mkdtemp(join(tmpdir(), "watcher-shared-session-"));
  state.closed = [];
  state.launches = [];
  state.bindingCalls = 0;
  state.failBinding = false;
  state.failNativeClose = false;
  state.sessionSources = [];
  state.replayCheckpointDirectories = [];
  state.watcherState = { state: "running", exitCode: null };
  await mkdir(join(state.runDirectory, "secrets"));
  for (const name of [
    "watcher-rollback.key",
    "watcher-prover.seed",
    "watcher-availability.seed",
  ])
    await writeFile(join(state.runDirectory, "secrets", name), "secret");
  await mkdir(join(state.runDirectory, "work/journeys/runtime"), {
    recursive: true,
  });
  await writeFile(
    join(state.runDirectory, "work/journeys/runtime/actor-funding.txt"),
    "already funded",
  );
  await writeFile(
    join(state.runDirectory, "binding.json"),
    "immutable deployment",
  );
  vi.stubGlobal(
    "fetch",
    vi.fn(async () => ({ ok: true, json: async () => ({}) })),
  );
});
afterEach(async () => {
  vi.unstubAllGlobals();
  await rm(state.runDirectory, { recursive: true, force: true });
});
it("keeps one observer and authenticated-history service lifetime across targets", async () => {
  const session = await openJourneySession(state.runDirectory);
  try {
    const first = await session.ensureWatcher();
    await session.assertHealthy();
    const second = await session.ensureWatcher();
    expect(second).toBe(first);
    expect(second.observe().pid).toBe(first.observe().pid);
    expect(state.launches.map(({ command }) => command)).toEqual(["start"]);
    expect(state.bindingCalls).toBe(1);
    expect(state.closed).toEqual([]);
    expect(session.native.nativeEvidencePath).toBe(
      join(session.directory, "native-chain.ndjson"),
    );
    expect(first.config).not.toHaveProperty("readinessHeaderHash");
    expect(Object.keys(first.config.faultProofInfrastructure).sort()).toEqual([
      "blueprintPath",
      "deploymentInfoPath",
      "manifestPath",
    ]);
    expect(first.config.workflowJournalDirectory).toBe(
      join(state.runDirectory, "work/journeys/runtime/workflows"),
    );
  } finally {
    await session.close();
  }
  await session.close();
  expect(state.closed).toEqual(["start-1", "native", "da"]);
  await expect(session.ensureWatcher()).rejects.toThrow("closed");
});

it("restarts only the failed-closed watcher with the same bindings and live services", async () => {
  const session = await openJourneySession(state.runDirectory);
  try {
    const running = await session.ensureWatcher();
    state.watcherState = { state: "exited", exitCode: 70 };
    await session.assertHealthy();
    expect(running.observe().pid).toBe(2);
    expect(state.bindingCalls).toBe(1);
    expect(state.closed).toEqual([]);
    expect(state.launches[1]).toEqual(state.launches[0]);
  } finally {
    await session.close();
  }
});

it("gates operations on the restarted listener without retrying normal connection failures", async () => {
  const session = await openJourneySession(state.runDirectory);
  try {
    const running = await session.ensureWatcher();
    const refusal = new TypeError("fetch failed", {
      cause: Object.assign(new Error("connection refused"), {
        code: "ECONNREFUSED",
      }),
    });
    const fetch = vi.mocked(globalThis.fetch);
    fetch.mockRejectedValueOnce(refusal);
    await expect(running.operations("/v1/status")).rejects.toBe(refusal);
    state.watcherState = { state: "exited", exitCode: 70 };
    fetch
      .mockRejectedValueOnce(refusal)
      .mockResolvedValueOnce(Response.json({ readiness: "ready" }));
    await expect(running.operations("/v1/status")).resolves.toEqual({
      readiness: "ready",
    });
    expect(running.observe().pid).toBe(2);
    // A later failure can race the pre-read health check. Its observed exit,
    // rather than the network error alone, must authorize the next restart.
    fetch
      .mockImplementationOnce(async () => {
        state.watcherState = { state: "exited", exitCode: 70 };
        throw refusal;
      })
      .mockResolvedValueOnce(Response.json({ readiness: "ready" }));
    await expect(running.operations("/v1/status")).resolves.toEqual({
      readiness: "ready",
    });
    expect(running.observe().pid).toBe(3);
  } finally {
    await session.close();
  }
});

it("launches from a newly retained healthy predecessor after baseline capture and before remaining staging", async () => {
  const session = await openJourneySession(state.runDirectory);
  await expect(
    runAutonomousWatcherJourney(
      state.runDirectory,
      {
        category: "doubleSpend",
        stage: async (input) => {
          expect(state.launches).toEqual([]);
          expect(
            JSON.parse(
              await readFile(
                join(input.directory, "workflow-baseline.json"),
                "utf8",
              ),
            ).prefixes,
          ).toEqual([]);
          // The process has not opened its listener yet. Staging must still
          // continue after binding/launch rather than waiting for catch-up.
          const refusal = new TypeError("fetch failed", {
            cause: Object.assign(new Error("connection refused"), {
              code: "ECONNREFUSED",
            }),
          });
          vi.mocked(globalThis.fetch).mockRejectedValue(refusal);
          await input.onHealthyPredecessor!("12".repeat(28));
          expect(state.launches.map(({ command }) => command)).toEqual([
            "start",
          ]);
          const running = await session.ensureWatcher();
          await expect(running.operations("/v1/status")).rejects.toBe(refusal);
          throw new Error("later staging failed");
        },
      },
      { session },
    ),
  ).rejects.toThrow("later staging failed");
  expect(state.bindingCalls).toBe(1);
  expect(state.closed).toHaveLength(3);
});

it("closes and refuses reuse when a pinned deployment input changes", async () => {
  const session = await openJourneySession(state.runDirectory);
  await session.ensureWatcher();
  await writeFile(
    join(state.runDirectory, "binding.json"),
    "changed deployment",
  );
  await expect(session.assertHealthy()).rejects.toThrow(
    "session input changed",
  );
  expect(state.closed).toHaveLength(3);
  await expect(session.ensureWatcher()).rejects.toThrow("closed");
});

it("cleans up once after binding failure and never reuses partial initialization", async () => {
  const session = await openJourneySession(state.runDirectory);
  state.failBinding = true;
  await expect(session.ensureWatcher()).rejects.toThrow("binding rejected");
  await session.close();
  expect(state.closed).toEqual(["native", "da"]);
  await expect(session.ensureWatcher()).rejects.toThrow("closed");
});

it("starts against the retained head before staging and closes after fixture failure", async () => {
  const headerHash = "12".repeat(28);
  await writeFile(
    join(state.runDirectory, "work/journeys/head.json"),
    JSON.stringify({
      deploymentFingerprint: "ab".repeat(32),
      block: { headerHash },
    }),
  );
  const session = await openJourneySession(state.runDirectory);
  const stage = vi.fn(async () => {
    expect(state.launches.map(({ command }) => command)).toEqual(["start"]);
    throw new Error("fixture rejected");
  });
  await expect(
    runAutonomousWatcherJourney(
      state.runDirectory,
      { category: "doubleSpend", stage },
      { session },
    ),
  ).rejects.toThrow("fixture rejected");
  expect(stage).toHaveBeenCalledOnce();
  expect(state.closed).toHaveLength(3);
  await expect(session.assertHealthy()).rejects.toThrow("closed");
});

it("captures real 56-hex header / 64-hex workflow journal prefixes before staging", async () => {
  const directory = join(state.runDirectory, "workflows");
  const headerHash = "12".repeat(28);
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: "ab".repeat(32),
    category: "doubleSpend" as const,
    target: { kind: "state_queue_header" as const, headerHash },
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const store = new DirectoryFraudProofWorkflowJournalStore(
    join(directory, "fault-proofs/doubleSpend", headerHash),
  );
  const entry: FraudProofWorkflowJournalEntry = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
    workflowId,
    identity,
    sequence: 0,
    recordedAt: "2026-09-14T00:00:00.000Z",
    event: { kind: "started" },
  };
  await store.append(entry, 0);
  const baseline = await captureJourneyWorkflowBaseline(
    directory,
    "doubleSpend",
  );
  expect(workflowId).toHaveLength(64);
  expect(baseline.get(headerHash)).toEqual([entry]);
  await store.append(
    {
      ...entry,
      sequence: 1,
      event: {
        kind: "prepared",
        artifact: {},
        artifactDigest: createHash("sha256").update("{}").digest("hex"),
      },
    },
    1,
  );
  expect(baseline.get(headerHash)).toHaveLength(1);
  expect(
    (await captureJourneyWorkflowBaseline(directory, "doubleSpend")).get(
      headerHash,
    ),
  ).toHaveLength(2);
  expect(baseline.get("34".repeat(28)) ?? []).toEqual([]);
});

it("distinguishes an old completed proof from a target completed during staging", async () => {
  const directory = join(state.runDirectory, "workflows");
  const complete = async (headerHash: string) => {
    const identity = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
      deploymentFingerprint: "ab".repeat(32),
      category: "doubleSpend" as const,
      target: { kind: "state_queue_header" as const, headerHash },
    };
    const workflowId = computeFraudProofWorkflowId(identity);
    const store = new DirectoryFraudProofWorkflowJournalStore(
      join(directory, "fault-proofs/doubleSpend", headerHash),
    );
    const terminal: FraudProofWorkflowTerminal = {
      schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
      category: "doubleSpend",
      headerHash,
      proofToken: {
        unit: "ab".repeat(28),
        outRef: "cd".repeat(32) + "#0",
        createdByTxHash: "cd".repeat(32),
        retainedAtFinalState: true,
      },
      correction: {
        removalTxHash: "ef".repeat(32),
        removedStateQueueOutRef: "12".repeat(32) + "#0",
        fraudulentHeaderAbsent: true,
        referencedProofTokenOutRef: "cd".repeat(32) + "#0",
      },
      economics: {
        operatorCredential: "12".repeat(28),
        proverCredential: "34".repeat(28),
        operatorBondInputOutRef: null,
        operatorBondInputLovelace: "0",
        slashedLovelace: "0",
        proverRewardOutputOutRef: null,
        proverRewardLovelace: "0",
        removalFeeLovelace: "200000",
        duplicateRewardAbsent: true,
      },
      observedAt: {
        slot: "1234",
        blockHash: "56".repeat(32),
        confirmationDepth: 30,
      },
    };
    const events: FraudProofWorkflowJournalEntry["event"][] = [
      { kind: "started" },
      { kind: "prepared", artifact: {}, artifactDigest: journalJsonDigest({}) },
    ];
    for (const [actionId, txHash] of [
      ["prove", terminal.proofToken.createdByTxHash],
      ["remove", terminal.correction.removalTxHash],
    ] as const) {
      events.push(
        {
          kind: "preflight_passed",
          actionId,
          txHash,
          localEvaluator: "Scalus",
          referenceScripts: [],
        },
        {
          kind: "submission_intent",
          actionId,
          txHash,
          actionInput: {},
          attempt: 1,
        },
        { kind: "submitted", actionId, txHash, attempt: 1 },
        { kind: "reconciled", actionId, txHash, outcome: "confirmed" },
        { kind: "confirmed", actionId, txHash },
      );
    }
    events.push({
      kind: "completed",
      terminal,
      terminalDigest: journalJsonDigest(terminal),
    });
    for (const [sequence, event] of events.entries())
      await store.append(
        {
          schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
          workflowId,
          identity,
          sequence,
          recordedAt: "2026-09-14T00:00:00.000Z",
          event,
        },
        sequence,
      );
  };
  const oldHeader = "12".repeat(28),
    newHeader = "34".repeat(28);
  await complete(oldHeader);
  const baseline = await captureJourneyWorkflowBaseline(
    directory,
    "doubleSpend",
  );
  await complete(newHeader);
  expect(
    baseline.get(oldHeader)?.some(({ event }) => event.kind === "completed"),
  ).toBe(true);
  expect(baseline.get(newHeader) ?? []).toEqual([]);
  expect(
    (await captureJourneyWorkflowBaseline(directory, "doubleSpend"))
      .get(newHeader)
      ?.at(-1)?.event.kind,
  ).toBe("completed");
});

it("closes the remaining services even when one service fails to close", async () => {
  const session = await openJourneySession(state.runDirectory);
  await session.ensureWatcher();
  state.failNativeClose = true;
  await expect(session.close()).rejects.toThrow(
    "Could not close watcher journey session",
  );
  expect(state.closed).toEqual(["start-1", "native", "da"]);
  await expect(session.ensureWatcher()).rejects.toThrow("closed");
});

it("rejects a foreign run directory before restarting its failed watcher", async () => {
  const session = await openJourneySession(state.runDirectory);
  await session.ensureWatcher();
  state.watcherState = { state: "exited", exitCode: 70 };
  const stage = vi.fn(async () => {
    throw new Error("must not stage");
  });
  await expect(
    runAutonomousWatcherJourney(
      join(state.runDirectory, "foreign"),
      {
        category: "doubleSpend",
        stage,
      },
      { session },
    ),
  ).rejects.toThrow("different run directory");
  expect(stage).not.toHaveBeenCalled();
  expect(state.launches.map(({ command }) => command)).toEqual(["start"]);
});

it.each([
  "journey-session.ts",
  "journey-session.capture-baseline.ts",
  "journey-runner.run.ts",
])("refuses reuse after pinned module part %s changes", async (changed) => {
  state.sessionSources = [
    "journey-session.ts",
    "journey-session.capture-baseline.ts",
    "journey-runner.run.ts",
  ].map((name) => join(state.runDirectory, name));
  for (const path of state.sessionSources)
    await writeFile(path, "original source");
  const session = await openJourneySession(state.runDirectory);
  await session.ensureWatcher();
  await writeFile(join(state.runDirectory, changed), "changed source");
  await expect(session.assertHealthy()).rejects.toThrow(
    `session input changed: ${join(state.runDirectory, changed)}`,
  );
  expect(state.closed).toHaveLength(3);
  await expect(session.ensureWatcher()).rejects.toThrow("closed");
});

it("keeps concurrent sessions' real checkpoint writes in separately owned directories", async () => {
  const sessions = await Promise.all([
    openJourneySession(state.runDirectory),
    openJourneySession(state.runDirectory),
  ]);
  try {
    expect(new Set(state.replayCheckpointDirectories).size).toBe(2);
    const hashes = ["91".repeat(28), "92".repeat(28)];
    await Promise.all(
      sessions.map((session, i) => session.ledgerAt(hashes[i])),
    );
    const stored = await Promise.all(
      state.replayCheckpointDirectories.map(async (directory) =>
        JSON.parse(
          await readFile(
            join(directory, `replayed-ledger-${"ab".repeat(32)}.json`),
            "utf8",
          ),
        ),
      ),
    );
    expect(stored.map((checkpoint) => checkpoint.headerHash).sort()).toEqual(
      hashes,
    );
  } finally {
    await Promise.all(sessions.map((session) => session.close()));
  }
});
