import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { fixture, utxo } from "../support/availability-challenge-fixture.js";
import {
  actions,
  ADA,
  answered,
  io,
  liveChallenge,
  observation,
  OPENING,
  openLanded,
  runtime,
  TIMEOUT_COLLATERAL,
  withheld,
} from "./concurrent-challenges.fixture.js";

describe("Open deadline through actual watcher reconciliation", () => {
  it("still closes a live challenge when commitment discovery consumes another header's Open cutoff", async () => {
    vi.useFakeTimers({
      toFake: ["Date", "performance", "setTimeout", "clearTimeout"],
    });
    const now = 1_800_000_000_000;
    vi.setSystemTime(now);
    const window = BigInt(
      SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms,
    );
    const withheld = fixture("59", BigInt(now) - window + 100n);
    const complete = liveChallenge("5a");
    openLanded(complete.challenged.headerHash);
    io.utxos = [utxo(0, TIMEOUT_COLLATERAL, "f1"), utxo(1, 10n * ADA, "f2")];
    io.attestedCommitment.mockImplementation(async () => {
      await vi.advanceTimersByTimeAsync(100);
      return withheld.commitment;
    });
    const watcher = await runtime();
    try {
      await watcher.reconcile(
        observation([withheld.attested, answered(complete.challenged)]),
        true,
      );
      expect(io.attestedCommitment).toHaveBeenCalledTimes(1);
      expect(io.sourceSignals[0]!.reason).toBeInstanceOf(
        SDK.DaAvailabilityReadScopeExpiredError,
      );
      expect(watcher.status()).toMatchObject({
        action: "close",
        missedOpenDeadlines: [withheld.attested.headerHash],
      });
      expect(actions()).toEqual([[complete.challenged.headerHash, "close"]]);
      expect(io.opens).toEqual([]);
    } finally {
      await watcher.close();
      vi.useRealTimers();
    }
  });

  it("aborts all active source scopes on rollback and rejects the late commitment before a fresh Open attempt", async () => {
    const withheldHeader = withheld("5b");
    let began!: () => void;
    const started = new Promise<void>((resolve) => {
      began = resolve;
    });
    let finish!: (commitment: SDK.DaAvailabilityCommitment) => void;
    io.attestedCommitment.mockImplementationOnce(async () => {
      began();
      return await new Promise<SDK.DaAvailabilityCommitment>((resolve) => {
        finish = resolve;
      });
    });
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "f1"),
      utxo(1, OPENING, "f2"),
      utxo(2, 100n * ADA, "f2"),
    ];
    const watcher = await runtime();
    try {
      const state = observation([withheldHeader.attested]);
      const pending = watcher.reconcile(state, true);
      await started;
      const previousSignals = [...io.sourceSignals];
      watcher.invalidateForRollback();
      expect(previousSignals.length).toBeGreaterThan(0);
      expect(previousSignals.every((signal) => signal.aborted)).toBe(true);
      await pending;
      expect(io.run).not.toHaveBeenCalled();
      finish(withheldHeader.commitment);
      await new Promise<void>((resolve) => setImmediate(resolve));
      await watcher.reconcile(state, true);
      expect(actions()).toEqual([[withheldHeader.attested.headerHash, "open"]]);
    } finally {
      finish(withheldHeader.commitment);
      await watcher.close();
    }
  });

  it("passes the authenticated absolute cutoff and the discovery scope to funding preparation", async () => {
    const withheldHeader = withheld("5c");
    io.utxos = [
      utxo(0, TIMEOUT_COLLATERAL, "f1"),
      utxo(1, OPENING + 100n * ADA, "f2"),
    ];
    const watcher = await runtime();
    try {
      await watcher.reconcile(observation([withheldHeader.attested]), true);
      const operation: Pick<
        Parameters<typeof SDK.runDaAvailabilityOperation>[1],
        "action" | "unsignedDeadlineMs" | "preparationScope"
      > = io.run.mock.calls[0]![1];
      const node = Data.castFrom(
        withheldHeader.attested.queue!.datum.data,
        SDK.StateQueueNode,
      );
      const cutoff =
        Number(node.header.endTime) +
        SELECTED_DEPLOYMENT_PROFILE.timing.da_challenge_window_ms;
      expect(operation).toMatchObject({
        action: "prepare",
        unsignedDeadlineMs: expect.any(Number),
        preparationScope: { deadlineEpochMs: expect.any(Number) },
      });
      expect(io.lucidInstances).toHaveLength(2);
      expect(io.lucidInstances[1]).not.toBe(io.lucidInstances[0]);
      const scope = operation.preparationScope;
      if (scope === undefined)
        throw new Error("Watcher omitted shared preparation scope");
      expect(scope.deadlineEpochMs).toBe(operation.unsignedDeadlineMs);
      expect(operation.unsignedDeadlineMs).toBe(cutoff);
      expect(scope.signal.aborted).toBe(true);
    } finally {
      await watcher.close();
    }
  });
});
