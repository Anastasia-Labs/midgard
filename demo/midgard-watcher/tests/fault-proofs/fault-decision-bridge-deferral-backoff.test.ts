import {
  FraudProofL1UnavailableError,
  ReplayLocalFault,
  RetainedDaPayloadUnavailableError,
  VALIDATION_TRACE_REPLAY_HELD,
  ValidationTraceReplayHeldError,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import { WATCHER_REPLAY_HOLD_MAXIMUM_AGE_MS } from "../../src/fault-proofs/fault-decision-bridge.classification-miss.js";
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
    await h.bridge.reconcileAndDispatch(current);
    await h.bridge.reconcileAndDispatch(current);
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

    await h.bridge.reconcileAndDispatch(current);
    expect(h.bridge.status().target?.headerHash).toBe(faulty!.headerHash);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    // A later pass reuses the target: no classification, the same generation.
    await h.bridge.reconcileAndDispatch(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(4);
    expect(h.enqueuedGenerations).toHaveLength(2);
    expect(new Set(h.enqueuedGenerations).size).toBe(1);
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

  it("defers a header whose replay failed locally, warns once and targets the fault once it clears", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    const warn = vi.fn();
    let faults = 2;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
      nowMs: IN_WINDOW,
      warn,
      classifyOverride: (fresh) => {
        if (faults > 0) {
          faults -= 1;
          throw new ReplayLocalFault({
            message: "withdrawal classification failed locally",
            cause: undefined,
          });
        }
        return fresh;
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(h.enqueued).toEqual([]);
    expect(warn).toHaveBeenCalledExactlyOnceWith({
      event: "classification_deferred",
      headerHash: faulty!.headerHash,
      cause: "replay_local_fault",
    });

    await h.bridge.reconcileAndDispatch(current);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(3);
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
    await h.bridge.reconcileAndDispatch(current);
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
      await h.bridge.reconcileAndDispatch(current);
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

/** `headerHash`'s classification waits on one held script re-execution. */
const replayHeld = (headerHash: string) =>
  new ValidationTraceReplayHeldError(
    headerHash,
    [
      {
        headerHash,
        position: 0,
        txId: "0c".repeat(32),
        attempts: 1,
        retryAtMs: 1_000,
        detail: "spend: Plutus evaluation stopped without a verdict",
        stuck: false,
      },
    ],
    0,
    [],
  );

describe("fault decision bridge deferral on a held script re-execution", () => {
  it("defers the header under the hold's reason, warns once and targets the fault once it decides", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    const warn = vi.fn();
    let held = 2;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "validationTraceDispute" },
      nowMs: IN_WINDOW,
      warn,
      classifyOverride: (fresh) => {
        if (held > 0) {
          held -= 1;
          throw replayHeld(faulty!.headerHash);
        }
        return fresh;
      },
    });
    expect(h.bridge.readiness()).toEqual([]);
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(h.enqueued).toEqual([]);
    expect(h.bridge.readiness()).toEqual([
      {
        reason: VALIDATION_TRACE_REPLAY_HELD,
        detail: replayHeld(faulty!.headerHash).message,
      },
    ]);
    expect(warn).toHaveBeenCalledExactlyOnceWith({
      event: "classification_deferred",
      headerHash: faulty!.headerHash,
      cause: VALIDATION_TRACE_REPLAY_HELD,
    });

    await h.bridge.reconcileAndDispatch(current);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    expect(h.bridge.readiness()).toEqual([]);
  });

  it("stops naming a hold that is no longer retried once it is too old", async () => {
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [faulty] = current.finalizedHeaders;
    let now = 3n;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "validationTraceDispute" },
      nowMs: () => now,
      classifyOverride: () => {
        throw replayHeld(faulty!.headerHash);
      },
    });
    await h.bridge.reconcileAndDispatch(current);
    now += BigInt(WATCHER_REPLAY_HOLD_MAXIMUM_AGE_MS);
    expect(h.bridge.readiness()).toHaveLength(1);
    now += 1n;
    expect(h.bridge.readiness()).toEqual([]);
  });
});

describe("fault decision bridge behind a committee that missed the release-final merge", () => {
  it("re-reads the confirmed head's payload on every pass and targets the fault once it is served", async () => {
    // The committee never held the confirmed head's payload, so no source
    // serves the queue head's predecessor until a committee re-observes it.
    const current = observation([headerFixture("01")], "Idle", [ATTESTED]);
    const [head] = current.finalizedHeaders;
    const observability = operations();
    const warn = vi.fn();
    let served = false;
    const h = harness({
      current,
      categoryByHeader: { [head!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      mergedHeaders: async () => new Map(),
      nowMs: IN_WINDOW,
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
    await h.bridge.reconcileAndDispatch(current);
    expect(reads()).toBe(2);
    expect(outcomes(observability)).toEqual(["pending_da", "pending_da"]);
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
    expect(h.enqueued).toEqual([]);
    served = true;
    await expect(h.bridge.reconcileAndDispatch(current)).resolves.toMatchObject(
      { target: { headerHash: head!.headerHash } },
    );
    expect(reads()).toBe(3);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      head!.headerHash,
    ]);
    expect(warn).toHaveBeenCalledOnce();
  });
});
