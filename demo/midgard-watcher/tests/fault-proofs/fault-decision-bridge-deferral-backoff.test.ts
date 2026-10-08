import {
  FraudProofL1UnavailableError,
  RetainedDaPayloadUnavailableError,
} from "@al-ft/midgard-fault-proofs";
import { KupmiosError } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { watcherDeferredRetryDelayMs } from "../../src/fault-proofs/fault-decision-bridge.classification-miss.js";
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

/** Inside every fixture header's challengeability window (they end at 2 ms). */
const IN_WINDOW = () => 3n;

/** The confirmed head every `headerFixture` names as its predecessor. */
const CONFIRMED_HEAD = "08".repeat(28);

const outcomes = (observability: ReturnType<typeof operations>) =>
  records(observability).map(({ outcome }) => outcome);

const nodeLost = () =>
  new FraudProofL1UnavailableError(
    "Ogmios session closed: Connection with the node lost",
  );

describe("fault decision bridge deferral on a transient", () => {
  it("waits out an unavailable L1 source, warns once and targets the fault exactly once", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    const observability = operations();
    const warn = vi.fn();
    let down = 3;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      nowMs: IN_WINDOW,
      warn,
      classifyOverride: (fresh) => {
        if (down > 0) {
          down -= 1;
          throw nodeLost();
        }
        return fresh;
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    await h.bridge.retryDeferredClassification(current);
    await h.bridge.retryDeferredClassification(current);
    expect(h.enqueued).toEqual([]);
    expect(outcomes(observability)).toEqual([
      "pending_l1",
      "pending_l1",
      "pending_l1",
    ]);
    expect(observability.api.metrics().deferredClassifications).toBe("3");
    expect(warn).toHaveBeenCalledExactlyOnceWith({
      event: "classification_deferred",
      headerHash: faulty!.headerHash,
      cause: "l1_source_unavailable",
    });

    await h.bridge.retryDeferredClassification(current);
    expect(h.bridge.status().target?.headerHash).toBe(faulty!.headerHash);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    // Nothing is deferred any more: later wakes neither classify nor enqueue.
    await h.bridge.retryDeferredClassification(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(4);
    expect(h.enqueued).toHaveLength(1);
  });

  it("waits out a retryable Lucid provider error from the classifier and targets the fault once", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    const observability = operations();
    let down = true;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      nowMs: IN_WINDOW,
      classifyOverride: (fresh) => {
        if (down)
          throw new KupmiosError({
            protocol: "kupo",
            operation: "getUtxosByOutRef",
            status: 503,
          });
        return fresh;
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(outcomes(observability)).toEqual(["pending_l1"]);
    down = false;
    await h.bridge.retryDeferredClassification(current);
    await h.bridge.retryDeferredClassification(current);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(2);
  });

  it("keeps a Lucid provider error the provider does not mark retryable hard", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const failure = new KupmiosError({
      protocol: "kupo",
      operation: "getUtxosByOutRef",
      status: 200,
    });
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      nowMs: IN_WINDOW,
      classifyOverride: () => {
        throw failure;
      },
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
  });

  it("keeps an error that only carries the transport name hard", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const forged = Object.assign(new Error("forged transport label"), {
      name: "FraudProofL1UnavailableError",
    });
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      nowMs: IN_WINDOW,
      classifyOverride: () => {
        throw forged;
      },
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(forged);
  });

  it("waits for an Attested payload while a source does not answer, then targets it once", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    const observability = operations();
    let answered = false;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      nowMs: IN_WINDOW,
      classifyOverride: (fresh) => {
        if (!answered)
          throw new RetainedDaPayloadUnavailableError(
            faulty!.headerHash,
            "the public retained-DA peer timed out",
            "unreachable",
          );
        return fresh;
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(outcomes(observability)).toEqual(["pending_da"]);
    answered = true;
    await h.bridge.retryDeferredClassification(current);
    await h.bridge.retryDeferredClassification(current);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(2);
  });

  it("defers a queue head whose confirmed-head payload is gone without holding readiness", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [head] = current.finalizedHeaders;
    const observability = operations();
    const warn = vi.fn();
    const h = harness({
      current,
      categoryByHeader: { [head!.headerHash]: "transitionTrace" },
      operationsSink: observability.sink,
      mergedHeaders: async () => new Map(),
      nowMs: IN_WINDOW,
      warn,
      classifyOverride: () => {
        // The transport marks the fetch failed before the bridge sees it.
        observability.sink.setAlert({
          code: "da_fetch_failure",
          subjectDigest: watcherDaFetchAlertSubject(CONFIRMED_HEAD),
          active: true,
          observedAtMs: "3",
        });
        throw unavailable(CONFIRMED_HEAD);
      },
    });
    await h.bridge.reconcileAndDispatch(current);
    for (let wake = 0; wake < 3; wake++)
      await h.bridge.retryDeferredClassification(current);
    expect(outcomes(observability)).toEqual(Array(4).fill("pending_da"));
    expect(observability.api.metrics()).toMatchObject({
      deferredClassifications: "4",
      activeAlertCount: "0",
    });
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
    expect(warn).toHaveBeenCalledOnce();
  });
});

describe("fault decision bridge deferred retry rate", () => {
  it("spaces production retries from one second, doubling to a one-minute cap", () => {
    expect(
      [1, 2, 3, 4, 5, 6, 7, 8, 100].map(watcherDeferredRetryDelayMs),
    ).toEqual([
      1_000, 2_000, 4_000, 8_000, 16_000, 32_000, 60_000, 60_000, 60_000,
    ]);
  });

  it("retries a deferred header only once its delay has passed, and a fresh observation at once", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    let monotonic = 0;
    let served = false;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      nowMs: IN_WINDOW,
      monotonicNowMs: () => monotonic,
      deferredRetryDelayMs: (consecutive) => 1_000 * consecutive,
      classifyOverride: (fresh) => {
        if (!served) throw nodeLost();
        return fresh;
      },
    });
    const reads = () => h.application.classifyHeader.mock.calls.length;
    await h.bridge.reconcileAndDispatch(current);
    expect(reads()).toBe(1);
    monotonic = 999;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(1);
    monotonic = 1_000;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(2);
    // The second consecutive deferral waits twice as long.
    monotonic = 2_999;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(2);
    // A new observation is not a retry and always classifies.
    await h.bridge.reconcileAndDispatch(current);
    expect(reads()).toBe(3);
    served = true;
    monotonic = 2_999 + 3_000;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(4);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    monotonic += 1_000_000;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(4);
  });
});

describe("fault decision bridge behind a committee that missed the release-final merge", () => {
  it("waits at the capped rate for the confirmed head's payload and targets the fault once it is served", async () => {
    // The committee never held the confirmed head's payload, so no source
    // serves the queue head's predecessor until a committee re-observes it.
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [head] = current.finalizedHeaders;
    const observability = operations();
    const warn = vi.fn();
    let monotonic = 0;
    let served = false;
    const h = harness({
      current,
      categoryByHeader: { [head!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      mergedHeaders: async () => new Map(),
      nowMs: IN_WINDOW,
      monotonicNowMs: () => monotonic,
      deferredRetryDelayMs: (consecutive) => 1_000 * consecutive,
      warn,
      classifyOverride: (fresh) => {
        if (!served) throw unavailable(CONFIRMED_HEAD);
        return fresh;
      },
    });
    const reads = () => h.application.classifyHeader.mock.calls.length;
    await expect(h.bridge.reconcileAndDispatch(current)).resolves.toMatchObject(
      { target: null },
    );
    monotonic = 999;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(1);
    monotonic = 1_000;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(2);
    expect(outcomes(observability)).toEqual(["pending_da", "pending_da"]);
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
    served = true;
    monotonic = 2_999;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(2);
    monotonic = 3_000;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(3);
    monotonic += 1_000_000;
    await h.bridge.retryDeferredClassification(current);
    expect(reads()).toBe(3);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      head!.headerHash,
    ]);
    expect(warn).toHaveBeenCalledOnce();
  });
});
