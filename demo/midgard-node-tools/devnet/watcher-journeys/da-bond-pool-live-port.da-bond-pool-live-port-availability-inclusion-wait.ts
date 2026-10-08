import "./da-bond-pool-live-port.da-bond-pool-live-port-parameters-and-funding.js";

import * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import {
  absentBlockStatus,
  awaitAvailabilityInclusion,
  awaitTimeBudgetMs,
  DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
  isSpentInputsRebroadcastRefusal,
  isTransientCanonicalError,
  nextJourneyBlockInterval,
  unsettledReconciliationError,
} from "./da-bond-pool-live-port.js";

describe("DA bond pool live port: chain reads", () => {
  it("tells a merged header from a removed one", () => {
    expect(absentBlockStatus("aa", "aa")).toBe("merged");
    expect(absentBlockStatus("aa", "bb")).toBe("removed");
  });

  it("starts a block at its predecessor's end and never ends it first", () => {
    expect(
      nextJourneyBlockInterval({ predecessorEndTime: 1_000n, nowMs: 5_000 }),
    ).toEqual({ startTime: 1_000n, endTime: 64_999n });
    expect(
      nextJourneyBlockInterval({
        predecessorEndTime: 200_000n,
        nowMs: 5_000,
      }),
    ).toEqual({ startTime: 200_000n, endTime: 201_999n });
  });

  it("ends a block on the last millisecond of a slot even when its predecessor ends within a minute", () => {
    // A slot-aligned predecessor end (x999) less than 60 s ahead of now.
    const { startTime, endTime } = nextJourneyBlockInterval({
      predecessorEndTime: 100_999n,
      nowMs: 30_000,
    });
    expect(endTime).toBeGreaterThan(startTime);
    expect((endTime + 1n) % 1000n).toBe(0n);
  });

  it("bounds awaitTime by the wait plus slack", () => {
    expect(awaitTimeBudgetMs(100_000, 40_000)).toBe(
      60_000 + DA_BOND_POOL_AWAIT_TIME_SLACK_MS,
    );
    expect(awaitTimeBudgetMs(1_000, 40_000, 5)).toBe(5);
  });

  it("retries only canonical-alignment and L1-unavailable errors", () => {
    expect(
      isTransientCanonicalError(
        new Error("L1 provider follower unavailable: behind_node_tip"),
      ),
    ).toBe(true);
    expect(
      isTransientCanonicalError(
        new Error(
          "Availability state changed during canonical discovery; rerun the command",
        ),
      ),
    ).toBe(true);
    expect(
      isTransientCanonicalError(
        new Error(
          "Availability transaction inclusion changed during its canonical read",
        ),
      ),
    ).toBe(true);
    // The foreign-spend read's twin: a block landed during that read.
    const foreignSpendRace = new Error(
      "Availability input spend changed during its canonical read",
    );
    expect(isTransientCanonicalError(foreignSpendRace)).toBe(true);
    expect(unsettledReconciliationError(foreignSpendRace)).toBe(
      "the canonical view is catching up",
    );
    // Not a timing race: the spend sits above the boundary it was read at.
    expect(
      isTransientCanonicalError(
        new Error("Availability input spend lies above the canonical boundary"),
      ),
    ).toBe(false);
    expect(isTransientCanonicalError(new Error("ScriptFailure"))).toBe(false);
  });
});

describe("DA bond pool live port: availability inclusion wait", () => {
  const txId = "ab".repeat(32);
  // The Ogmios refusal the live run hit when reconciliation rebroadcast an
  // Open that was already in the mempool.
  const spentInputs = Object.assign(
    new Error(
      'Ogmios JSON-RPC error 3997: The transaction couldn\'t be added to the mempool. A justification is given as \'data.error\'.: {"error":"All inputs are spent. Transaction has probably already been included"}',
    ),
    {
      code: 3997,
      data: {
        error:
          "All inputs are spent. Transaction has probably already been included",
      },
    },
  );
  const result = (
    status: SDK.DaAvailabilityOperationResult["status"],
  ): SDK.DaAvailabilityOperationResult =>
    ({ txHash: txId, status }) as SDK.DaAvailabilityOperationResult;
  const scripted = (
    steps: readonly (SDK.DaAvailabilityOperationResult["status"] | Error)[],
  ) => {
    let index = 0;
    return async () => {
      const step = steps[Math.min(index, steps.length - 1)]!;
      index += 1;
      if (step instanceof Error) throw step;
      return [result(step)];
    };
  };
  const clock = () => {
    let time = 0;
    return {
      now: () => time,
      wait: async (ms: number) => {
        time += ms;
      },
    };
  };

  it("classifies only a spent-input refusal as a rebroadcast refusal", () => {
    expect(isSpentInputsRebroadcastRefusal(spentInputs)).toBe(true);
    expect(
      isSpentInputsRebroadcastRefusal(
        Object.assign(new Error("submitTransaction failed"), {
          data: { error: { BadInputsUTxO: ["a#0"] } },
        }),
      ),
    ).toBe(true);
    expect(isSpentInputsRebroadcastRefusal(new Error("ScriptFailure"))).toBe(
      false,
    );
    expect(
      isSpentInputsRebroadcastRefusal(
        new Error(
          "Provider returned a different availability transaction hash",
        ),
      ),
    ).toBe(false);
  });

  it("keeps reconciling after a spent-input refusal until the transaction is included", async () => {
    const { now, wait } = clock();
    const lines: string[] = [];
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([spentInputs, spentInputs, "included"]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 60_000,
        pollMs: 2_000,
        wait,
        now,
        log: (line) => lines.push(line),
      }),
    ).resolves.toBeUndefined();
    expect(lines).toHaveLength(1);
    expect(lines[0]).toContain("rebroadcast refused with spent inputs");
  });

  it("fails at once on any other reconciliation error", async () => {
    const { now, wait } = clock();
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([new Error("ScriptFailure"), "included"]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 60_000,
        pollMs: 2_000,
        wait,
        now,
        log: () => undefined,
      }),
    ).rejects.toThrow("ScriptFailure");
  });

  it("fails when the refused transaction's inputs turn out spent by another", async () => {
    const { now, wait } = clock();
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([spentInputs, "expired"]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 60_000,
        pollMs: 2_000,
        wait,
        now,
        log: () => undefined,
      }),
    ).rejects.toThrow(`Availability transaction ${txId} ended expired`);
  });

  it("names the last refusal when the wait times out", async () => {
    const { now, wait } = clock();
    await expect(
      awaitAvailabilityInclusion({
        txId,
        reconcile: scripted([spentInputs]),
        journalRecord: () => ({ state: "pending" }),
        timeoutMs: 10_000,
        pollMs: 2_000,
        wait,
        now,
        log: () => undefined,
      }),
    ).rejects.toThrow(
      /was not included in time \(pending\); \d+ unsettled reconciliation\(s\), last: .*All inputs are spent/u,
    );
  });
});
