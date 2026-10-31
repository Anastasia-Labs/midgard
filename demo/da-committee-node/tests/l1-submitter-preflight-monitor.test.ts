import { describe, expect, it } from "vitest";

import type { L1SubmitterPreflightResult } from "../src/l1/submitter.js";
import { AutoFundPaymentUnsettledError } from "../src/l1/submitter.prune-in-flight-spends.js";
import {
  createL1SubmitterPreflightMonitor,
  type L1SubmitterPreflightMonitorTimers,
} from "../src/l1-submitter-preflight-monitor.js";

const result = (
  status: L1SubmitterPreflightResult["status"],
): L1SubmitterPreflightResult => ({
  status,
  address: "addr_test1submitter",
  totalLiveLovelace: 0n,
  plainAdaLovelace: 0n,
  plainAdaUtxoCount: 0,
  collateralCandidateLovelace: 0n,
  spendableOutRefs: [],
  ignoredOutRefs: [],
  requiredPlainLovelace: 0n,
  requiredCollateralLovelace: 0n,
  requiredSpendableUtxoCount: 0,
  missingPlainLovelace: status === "failed" ? 1n : 0n,
  missingCollateralLovelace: 0n,
  missingSpendableUtxoCount: 0,
  errors: status === "failed" ? ["missing_plain_lovelace"] : [],
});

/** Timers the test fires by hand, recording each requested delay. */
const manualTimers = () => {
  const pending: { run: () => void; ms: number }[] = [];
  const timers: L1SubmitterPreflightMonitorTimers = {
    setTimeout: (run, ms) => {
      const entry = { run, ms };
      pending.push(entry);
      return entry;
    },
    clearTimeout: (handle) => {
      const index = pending.indexOf(handle as (typeof pending)[number]);
      if (index >= 0) pending.splice(index, 1);
    },
  };
  const fire = async (): Promise<number> => {
    const entry = pending.shift();
    if (entry === undefined) throw new Error("no timer is pending");
    entry.run();
    // Let the evaluation the timer started settle.
    for (let turn = 0; turn < 10; turn += 1) await Promise.resolve();
    return entry.ms;
  };
  return { timers, pending, fire };
};

const harness = (outcomes: readonly (Error | L1SubmitterPreflightResult)[]) => {
  const calls: boolean[] = [];
  const lines: string[] = [];
  const clock = manualTimers();
  const monitor = createL1SubmitterPreflightMonitor({
    evaluate: ({ autoFund }) => {
      calls.push(autoFund);
      const outcome = outcomes[calls.length - 1];
      if (outcome === undefined) throw new Error("unexpected evaluation");
      return outcome instanceof Error
        ? Promise.reject(outcome)
        : Promise.resolve(outcome);
    },
    write: (line) => lines.push(line),
    timers: clock.timers,
    retryInitialMs: 5,
    retryMaxMs: 20,
    recheckMs: 100,
  });
  return {
    monitor,
    calls,
    clock,
    events: () =>
      lines.map((line) => (JSON.parse(line) as { event: string }).event),
  };
};

describe("L1 submitter preflight monitor", () => {
  it("reports a preflight that throws as unavailable, retries with backoff, and becomes ready without a restart", async () => {
    const outage = new Error("connect ECONNREFUSED kupo:1442");
    const h = harness([outage, outage, outage, result("ready")]);

    await h.monitor.start();
    expect(h.monitor.snapshot()).toEqual({
      status: "not_run",
      error: "connect ECONNREFUSED kupo:1442",
    });
    expect(h.monitor.reasons()).toEqual([
      "l1_submitter_preflight_unavailable: connect ECONNREFUSED kupo:1442",
    ]);

    expect(await h.clock.fire()).toBe(5);
    expect(await h.clock.fire()).toBe(10);
    expect(h.monitor.snapshot().status).toBe("not_run");
    expect(await h.clock.fire()).toBe(20);

    expect(h.monitor.snapshot().status).toBe("ready");
    expect(h.monitor.reasons()).toEqual([]);
    // Ready is final here: nothing more is scheduled, and it ran exactly once
    // past the outage.
    expect(h.clock.pending).toHaveLength(0);
    expect(h.calls).toHaveLength(4);
    expect(h.events()).toEqual([
      "l1_submitter_preflight_unavailable",
      "l1_submitter_preflight_evaluated",
    ]);
  });

  it("keeps an evaluated shortfall failed, re-reads it read-only, and clears once the wallet is topped up", async () => {
    const h = harness([result("failed"), result("failed"), result("ready")]);

    await h.monitor.start();
    expect(h.monitor.snapshot().status).toBe("failed");
    // A completed evaluation that failed is not a transient: no unavailable
    // reason, and the service's own "preflight failed" reason stands.
    expect(h.monitor.reasons()).toEqual([]);

    expect(await h.clock.fire()).toBe(100);
    expect(h.monitor.snapshot().status).toBe("failed");
    expect(await h.clock.fire()).toBe(100);
    expect(h.monitor.snapshot().status).toBe("ready");
    expect(h.clock.pending).toHaveLength(0);
    // Funding is offered until the first evaluation completes, never after.
    expect(h.calls).toEqual([true, false, false]);
  });

  it("asks for funding on every try until one evaluation completes", async () => {
    const h = harness([new Error("timeout"), result("funded")]);

    await h.monitor.start();
    await h.clock.fire();

    expect(h.monitor.snapshot().status).toBe("funded");
    expect(h.calls).toEqual([true, true]);
  });

  it("never asks for funding again after a funding payment that may have been sent did not settle", async () => {
    const unsettled = new AutoFundPaymentUnsettledError(
      new Error("awaitTxConfirmation timed out"),
    );
    const h = harness([unsettled, new Error("timeout"), result("ready")]);

    await h.monitor.start();
    expect(h.monitor.snapshot().status).toBe("not_run");
    await h.clock.fire();
    await h.clock.fire();

    // The payment landed: the read-only retry finds the wallet ready.
    expect(h.monitor.snapshot().status).toBe("ready");
    expect(h.calls).toEqual([true, false, false]);
  });

  it("schedules nothing after stop", async () => {
    const h = harness([new Error("timeout")]);

    await h.monitor.start();
    h.monitor.stop();

    expect(h.clock.pending).toHaveLength(0);
  });
});
