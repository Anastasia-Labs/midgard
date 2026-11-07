import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  l1SlotToPosixTimeMs,
  NAMED_NETWORK_L1_SLOT_CONFIGS,
} from "@al-ft/midgard-core/l1-slot-config";
import { RetainedDaPayloadUnavailableError } from "@al-ft/midgard-fault-proofs";
import type { Header as HeaderType } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { watcherDaFetchAlertSubject } from "../../src/runtime/operations-observability.alert-book.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import {
  ATTESTED,
  operations,
  records,
  unavailable,
} from "./fault-decision-bridge.released-fixture.js";

/** Every `headerFixture` ends at 2 ms. */
const END = 2n;
const HORIZON = END + BigInt(MIDGARD_RETENTION_WINDOW.requiredRetentionMs);
const PAST = HORIZON + 1n;

const header = (suffix: string, endTime = END): HeaderType => ({
  ...headerFixture(suffix),
  endTime,
});

const outcomes = (observability: ReturnType<typeof operations>) =>
  records(observability).map(({ headerHash, outcome }) => ({
    headerHash,
    outcome,
  }));

/** A failed fetch for `headerHash`, as the libp2p transport raises it. */
const failedFetch = (
  observability: ReturnType<typeof operations>,
  headerHash: string,
) =>
  observability.sink.setAlert({
    code: "da_fetch_failure",
    subjectDigest: watcherDaFetchAlertSubject(headerHash),
    active: true,
    observedAtMs: "1",
  });

describe("fault decision bridge past the challengeability horizon", () => {
  it("keeps a catch-up missing payload actionable through its commit-time horizon", async () => {
    const clock = NAMED_NETWORK_L1_SLOT_CONFIGS.Preprod;
    const blockSlot = 2_000_000n;
    const blockTime =
      l1SlotToPosixTimeMs(clock, blockSlot) + BigInt(clock.slotLength) - 1n;
    const coverageEnd = blockTime - 6n * 60n * 60n * 1000n;
    const catchUp: HeaderType = {
      ...header("01", coverageEnd),
      startTime: coverageEnd - 1000n,
      blockSlot,
      l2TransactionCount: 0n,
      depositCount: 1n,
    };
    const current = observation([catchUp], "Idle", [ATTESTED], [blockTime]);
    const inWindow = current.finalizedHeaders[0]!;
    const observability = operations();
    const failure = unavailable(inWindow.headerHash);
    const h = harness({
      current,
      categoryByHeader: { [inWindow.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      nowMs: () =>
        blockTime + BigInt(MIDGARD_RETENTION_WINDOW.requiredRetentionMs),
      classifyOverride: () => {
        throw failure;
      },
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(outcomes(observability)).toEqual([
      { headerHash: inWindow.headerHash, outcome: "failed" },
    ]);
    expect(observability.api.metrics().unverifiedHeaders.pastHorizon).toBe("0");
  });

  it("skips a header whose payload is gone past its horizon, warns once and selects the next fault", async () => {
    const current = observation([header("01"), header("02")], "Idle", [
      ATTESTED,
      ATTESTED,
    ]);
    const [gone, faulty] = current.finalizedHeaders;
    const observability = operations();
    failedFetch(observability, gone!.headerHash);
    const warn = vi.fn();
    const h = harness({
      current,
      categoryByHeader: {
        [gone!.headerHash]: "transitionTrace",
        [faulty!.headerHash]: "doubleSpend",
      },
      operationsSink: observability.sink,
      nowMs: () => PAST,
      warn,
      classifyOverride: (fresh) => {
        if (fresh.headerHash === gone!.headerHash)
          throw unavailable(gone!.headerHash);
        return fresh;
      },
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target?.headerHash).toBe(faulty!.headerHash);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(outcomes(observability)).toEqual(
      expect.arrayContaining([
        { headerHash: gone!.headerHash, outcome: "unverified_past_horizon" },
        { headerHash: faulty!.headerHash, outcome: "fault_detected" },
      ]),
    );
    expect(observability.api.metrics()).toMatchObject({
      unverifiedHeaders: { merged: "0", removed: "0", pastHorizon: "1" },
      activeAlertCount: "0",
    });
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
    expect(warn).toHaveBeenCalledExactlyOnceWith({
      event: "unverified_past_horizon",
      headerHash: gone!.headerHash,
      missingPayloadHeaderHash: gone!.headerHash,
      challengeableUntilMs: HORIZON.toString(),
    });

    // Time only moves the header further past: it is never read again.
    await h.bridge.reconcileAndDispatch(current);
    await h.bridge.reconcileAndDispatch(current);
    const goneReads = h.application.classifyHeader.mock.calls.filter(
      ([request]) => request.header.headerHash === gone!.headerHash,
    );
    expect(goneReads).toHaveLength(1);
    expect(warn).toHaveBeenCalledOnce();
    expect(observability.api.metrics().unverifiedHeaders.pastHorizon).toBe("1");
  });

  it.each([
    ["just after the block", END + 1n],
    ["at the horizon itself", HORIZON],
  ])("keeps failing closed on an Attested miss %s", async (_label, nowMs) => {
    const current = observation([header("01")], "Idle", [ATTESTED]);
    const [inWindow] = current.finalizedHeaders;
    const observability = operations();
    const failure = unavailable(inWindow!.headerHash);
    const h = harness({
      current,
      categoryByHeader: { [inWindow!.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      nowMs: () => nowMs,
      classifyOverride: () => {
        throw failure;
      },
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(outcomes(observability)).toEqual([
      { headerHash: inWindow!.headerHash, outcome: "failed" },
    ]);
    expect(observability.api.metrics().unverifiedHeaders.pastHorizon).toBe("0");
  });

  // A deployment's DA history window can outlast the horizon, so a store
  // still holding the payload says nothing about challengeability: the skip
  // turns on the header's own horizon alone, strictly after it.
  it.each([
    ["1 ms inside the horizon", HORIZON - 1n, "failed", "0"],
    ["exactly at the horizon", HORIZON, "failed", "0"],
    ["1 ms past the horizon", PAST, "unverified_past_horizon", "1"],
  ] as const)(
    "decides the skip of a gone payload %s on the clock alone",
    async (_label, nowMs, outcome, pastHorizon) => {
      const current = observation([header("01")], "Idle", [ATTESTED]);
      const [gone] = current.finalizedHeaders;
      const observability = operations();
      failedFetch(observability, gone!.headerHash);
      const failure = unavailable(gone!.headerHash);
      const h = harness({
        current,
        categoryByHeader: { [gone!.headerHash]: "transitionTrace" },
        operationsSink: observability.sink,
        nowMs: () => nowMs,
        classifyOverride: () => {
          throw failure;
        },
      });
      const run = h.bridge.reconcileAndDispatch(current);
      if (outcome === "failed") await expect(run).rejects.toBe(failure);
      else expect((await run).target).toBeNull();
      expect(outcomes(observability)).toEqual([
        { headerHash: gone!.headerHash, outcome },
      ]);
      expect(observability.api.metrics().unverifiedHeaders.pastHorizon).toBe(
        pastHorizon,
      );
    },
  );

  it("skips a past-horizon header even when a source did not answer, and still targets a later in-window fault", async () => {
    // After downtime longer than maturity the oldest payload is pruned: two
    // peers answer not_found, a third is down. Waiting on that peer would
    // hold every later header unclassified past its own window.
    const current = observation([header("01"), header("02", PAST)], "Idle", [
      ATTESTED,
      ATTESTED,
    ]);
    const [silent, faulty] = current.finalizedHeaders;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [silent!.headerHash]: "transitionTrace",
        [faulty!.headerHash]: "doubleSpend",
      },
      operationsSink: observability.sink,
      nowMs: () => PAST,
      classifyOverride: (fresh) => {
        if (fresh.headerHash === silent!.headerHash)
          throw new RetainedDaPayloadUnavailableError(
            silent!.headerHash,
            "a public retained-DA source timed out",
            "unreachable",
          );
        return fresh;
      },
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target?.headerHash).toBe(faulty!.headerHash);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(outcomes(observability)).toEqual(
      expect.arrayContaining([
        { headerHash: silent!.headerHash, outcome: "unverified_past_horizon" },
        { headerHash: faulty!.headerHash, outcome: "fault_detected" },
      ]),
    );
    expect(observability.api.metrics().unverifiedHeaders.pastHorizon).toBe("1");
  });

  it("waits on a source that did not answer while the header is still challengeable", async () => {
    const current = observation([header("01", PAST)], "Idle", [ATTESTED]);
    const [silent] = current.finalizedHeaders;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: { [silent!.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      nowMs: () => PAST,
      classifyOverride: () => {
        throw new RetainedDaPayloadUnavailableError(
          silent!.headerHash,
          "a public retained-DA source timed out",
          "unreachable",
        );
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(outcomes(observability)).toEqual([
      { headerHash: silent!.headerHash, outcome: "pending_da" },
    ]);
    expect(observability.api.metrics().unverifiedHeaders.pastHorizon).toBe("0");
  });

  describe("a predecessor's missing payload", () => {
    const predecessorObservation = observation([header("0a")]);
    const predecessor = predecessorObservation.finalizedHeaders[0]!;

    const run = async (currentEndTime: bigint) => {
      const current = observation([header("01", currentEndTime)], "Idle", [
        ATTESTED,
      ]);
      const [live] = current.finalizedHeaders;
      const observability = operations();
      failedFetch(observability, predecessor.headerHash);
      const failure = unavailable(predecessor.headerHash);
      const h = harness({
        current,
        categoryByHeader: { [live!.headerHash]: "transitionTrace" },
        operationsSink: observability.sink,
        mergedHeaders: async () => new Map(),
        resolvePredecessorOverride: async () => predecessor,
        nowMs: () => PAST,
        classifyOverride: () => {
          throw failure;
        },
      });
      return {
        live: live!,
        observability,
        failure,
        outcome: h.bridge.reconcileAndDispatch(current),
      };
    };

    it("skips the header once both it and its predecessor are past their horizons", async () => {
      const { live, observability, outcome } = await run(END);
      expect((await outcome).target).toBeNull();
      expect(outcomes(observability)).toEqual([
        { headerHash: live.headerHash, outcome: "unverified_past_horizon" },
      ]);
      // The predecessor's failed fetch is accounted for by the skip.
      expect(observability.api.metrics().activeAlertCount).toBe("0");
    });

    it("keeps the unmerged-predecessor refusal while the header itself is challengeable", async () => {
      const { live, observability, failure, outcome } = await run(PAST);
      await expect(outcome).rejects.toBe(failure);
      expect(outcomes(observability)).toEqual([
        { headerHash: live.headerHash, outcome: "failed" },
      ]);
    });
  });
});
