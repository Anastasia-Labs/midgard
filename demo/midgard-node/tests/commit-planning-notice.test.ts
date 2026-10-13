import { Worker } from "node:worker_threads";

import { Effect, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { takeCommitWorkerOutput } from "../src/fibers/block-commitment.promote-or-recover-native-mpf.js";
import type { GateState } from "../src/services/follower-write-gate.js";
import { Globals } from "../src/services/globals.js";
import {
  applyCommitPlanningNotice,
  commitPlanningReadiness,
  type CommitPlanningSelection,
  makeCommitPlanningNotice,
  parseCommitPlanningNotice,
} from "../src/workers/commit-planning-notice.js";

const selected: CommitPlanningSelection = {
  mode: "catch_up",
  observedAtMs: 5_000,
  authority: {
    view: { generation: 7, slot: 101, height: 90, hash: "aa".repeat(32) },
    epoch: "12345678-1234-4123-8123-123456789abc",
    baseHeaderHash: "bb".repeat(28),
    baseUtxosRoot: "cc".repeat(32),
    deploymentFingerprint: "dd".repeat(32),
  },
  coverageStartTimeMs: 100,
  selectedEndTimeMs: 200,
  blockTimeMs: 1_000,
  canonicalHorizonMs: 900,
  eligibleCutoffMs: 900,
  eligibleEventCountLowerBounds: {
    deposits: 4,
    withdrawals: 0,
    forcedTransactions: 0,
  },
  selectedEventCounts: { deposits: 2, withdrawals: 0, forcedTransactions: 0 },
  includedInstantCount: 1,
  selectedOrdinaryTxCount: 0,
  rescue: null,
};
const gate: GateState = {
  epoch: selected.authority.epoch,
  applied: selected.authority.view,
  pending: undefined,
};
const project = (
  snapshot: Parameters<typeof commitPlanningReadiness>[0]["snapshot"],
  overrides: Partial<Parameters<typeof commitPlanningReadiness>[0]> = {},
) =>
  commitPlanningReadiness({
    snapshot,
    gate,
    localEpoch: gate.epoch,
    recomputing: false,
    deploymentFingerprint: selected.authority.deploymentFingerprint!,
    nowMs: 5_050,
    maxAgeMs: 100,
    ...overrides,
  });

it("compiled worker producer and compiled parent consumer project a selected catch-up plan", async () => {
  const producerUrl = new URL(
    "../dist/commit-planning-notice.js",
    import.meta.url,
  ).href;
  const worker = new Worker(
    new URL("./helpers/commit-planning-compiled-producer.mjs", import.meta.url),
    { workerData: { producerUrl, selection: selected } },
  );
  const notice = await new Promise<unknown>((resolve, reject) => {
    worker.once("message", resolve);
    worker.once("error", reject);
    worker.once("exit", (code) => {
      if (code !== 0) reject(new Error(`producer exited ${code}`));
    });
  });
  await worker.terminate();
  expect(parseCommitPlanningNotice(notice)).toEqual(
    makeCommitPlanningNotice(selected),
  );
  const compiled = (await import(
    producerUrl
  )) as typeof import("../src/workers/commit-planning-notice.js");
  const globals = await Effect.runPromise(
    Globals.pipe(Effect.provide(Globals.Default)),
  );
  await Effect.runPromise(compiled.applyCommitPlanningNotice(globals, notice));
  const snapshot = Effect.runSync(Ref.get(globals.COMMIT_PLANNING));
  expect(snapshot?.selection.mode).toBe("catch_up");
  expect(snapshot?.selection.eligibleCutoffMs).toBe(900);
  expect(
    compiled.commitPlanningReadiness({
      snapshot,
      gate,
      localEpoch: gate.epoch,
      recomputing: false,
      deploymentFingerprint: selected.authority.deploymentFingerprint!,
      nowMs: snapshot!.receivedAtMs,
      maxAgeMs: Number.MAX_SAFE_INTEGER,
    }),
  ).toMatchObject({ status: "current", planned: { mode: "catch_up" } });
  // The actual parent dispatcher consumes the typed notice ahead of output;
  // it cannot accidentally classify it as a successful submission.
  expect(
    takeCommitWorkerOutput(globals, makeCommitPlanningNotice(selected), 10),
  ).toBeUndefined();
  expect(Effect.runSync(Ref.get(globals.COMMIT_PLANNING))?.selection).toEqual(
    selected,
  );
});

describe("planning authority and freshness", () => {
  it("marks a prior plan stale after a rewind, forward view, new epoch, recompute or deployment change", () => {
    const snapshot = { selection: selected, receivedAtMs: 5_010 };
    expect(project(snapshot)).toMatchObject({
      status: "current",
      planned: { mode: "catch_up" },
    });
    for (const overrides of [
      { gate: { ...gate, applied: { ...gate.applied!, generation: 8 } } },
      { gate: { ...gate, applied: { ...gate.applied!, slot: 102 } } },
      { localEpoch: "22345678-1234-4123-8123-123456789abc" },
      { recomputing: true },
      { deploymentFingerprint: "ee".repeat(32) },
      { nowMs: 5_101 },
      { nowMs: 4_999 },
    ])
      expect(project(snapshot, overrides)).toMatchObject({ status: "stale" });
  });
  it("refuses malformed clocks, partial count authority and ordinary work in catch-up, clearing the old mode", async () => {
    const globals = {
      COMMIT_PLANNING: await Effect.runPromise(
        Ref.make({ selection: selected, receivedAtMs: 5_010 } as {
          selection: CommitPlanningSelection;
          receivedAtMs: number;
        } | null),
      ),
    };
    for (const altered of [
      { ...selected, selectedEndTimeMs: 901 },
      { ...selected, blockTimeMs: -1 },
      { ...selected, selectedOrdinaryTxCount: 1 },
      {
        ...selected,
        selectedEventCounts: {
          deposits: 5,
          withdrawals: 0,
          forcedTransactions: 0,
        },
      },
      {
        ...selected,
        authority: {
          ...selected.authority,
          view: { ...selected.authority.view, hash: "bad" },
        },
      },
    ]) {
      const message = structuredClone({
        type: "CommitPlanningNotice",
        status: "selected",
        selection: altered,
      });
      expect(parseCommitPlanningNotice(message)).toBeUndefined();
      await Effect.runPromise(applyCommitPlanningNotice(globals, message));
      expect(
        project(Effect.runSync(Ref.get(globals.COMMIT_PLANNING))),
      ).toMatchObject({ status: "unavailable", planned: null });
    }
  });
  it("clears a selected mode on the next attempt and preserves the approved equal-clock whole-instant rescue", async () => {
    const globals = {
      COMMIT_PLANNING: await Effect.runPromise(
        Ref.make({ selection: selected, receivedAtMs: 5_010 } as {
          selection: CommitPlanningSelection;
          receivedAtMs: number;
        } | null),
      ),
    };
    await Effect.runPromise(
      applyCommitPlanningNotice(globals, {
        type: "CommitPlanningNotice",
        status: "unavailable",
        reason: "planning_started",
      }),
    );
    expect(Effect.runSync(Ref.get(globals.COMMIT_PLANNING))).toBeNull();
    const rescue = makeCommitPlanningNotice({
      ...selected,
      selectedEndTimeMs: 1_000,
      canonicalHorizonMs: 1_000,
      eligibleCutoffMs: 1_000,
      rescue: "whole_instant",
    });
    expect(parseCommitPlanningNotice(structuredClone(rescue))).toEqual(rescue);
  });
});
