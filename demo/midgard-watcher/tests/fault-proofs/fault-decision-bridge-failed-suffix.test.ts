/**
 * A header whose classification fails postpones itself and every later
 * header, as a deferral does; a fault already decided earlier in the queue
 * is journaled, selected and dispatched in the same pass, and the pass still
 * rejects with the header's failure so the driver names it. A failure at or
 * before a target already in flight fails the pass and leaves the target as
 * it was.
 */
import { describe, expect, it } from "vitest";

import { harness } from "./fault-decision-bridge.harness.js";
import {
  type decision,
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import {
  operations,
  records,
} from "./fault-decision-bridge.released-fixture.js";

/** The fraudulent block, a later block whose replay throws, and its successor. */
const queue = () => {
  const current = observation([
    headerFixture("01"),
    headerFixture("02"),
    headerFixture("03"),
  ]);
  const [fraud, broken, after] = current.finalizedHeaders;
  return { current, fraud: fraud!, broken: broken!, after: after! };
};

const outcomes = (observability: ReturnType<typeof operations>) =>
  records(observability).map(({ headerHash, outcome }) => ({
    headerHash,
    outcome,
  }));

describe("fault decision bridge with a failing header", () => {
  it("dispatches a fault decided before the failing header and postpones the rest", async () => {
    const { current, fraud, broken, after } = queue();
    const failure = new Error("replay of the later block threw");
    let failing = true;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [fraud.headerHash]: "doubleSpend",
        [broken.headerHash]: "invalidRange",
        [after.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      classifyOverride: async (fresh: ReturnType<typeof decision>) => {
        if (fresh.headerHash === fraud.headerHash) {
          // Still in flight when the later header fails.
          await new Promise((resolve) => setTimeout(resolve, 20));
          return fresh;
        }
        if (fresh.headerHash === broken.headerHash && failing) throw failure;
        return { ...fresh, decision: "healthy" as const };
      },
    });

    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(h.appended.map(({ headerHash }) => headerHash)).toEqual([
      fraud.headerHash,
    ]);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      fraud.headerHash,
    ]);
    expect(h.bridge.status()).toEqual({
      observationDigest: current.observationDigest,
      target: expect.objectContaining({ headerHash: fraud.headerHash }),
    });
    // The header after the failing one is not classified this pass.
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([fraud.headerHash, broken.headerHash]);
    expect(outcomes(observability)).toEqual(
      expect.arrayContaining([
        { headerHash: fraud.headerHash, outcome: "fault_detected" },
        { headerHash: broken.headerHash, outcome: "failed" },
      ]),
    );

    // The failure repeats: every pass keeps the fault dispatched under the
    // one permit it was given, and still names the failure.
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      fraud.headerHash,
      fraud.headerHash,
    ]);
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(h.revocations).toEqual([]);

    // Once the header classifies, the postponed suffix is checked again.
    failing = false;
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target?.headerHash).toBe(fraud.headerHash);
    expect(prepared.decisionDigests).toHaveLength(3);
    expect(h.application.classifyHeader).toHaveBeenCalledWith(
      expect.objectContaining({
        header: expect.objectContaining({ headerHash: after.headerHash }),
      }),
    );
  });

  it("journals the decided prefix in a recovery preparation, starts nothing and names the failure", async () => {
    const { current, fraud, broken, after } = queue();
    const failure = new Error("replay of the later block threw");
    const h = harness({
      current,
      categoryByHeader: {
        [fraud.headerHash]: "doubleSpend",
        [broken.headerHash]: "invalidRange",
        [after.headerHash]: "transitionTrace",
      },
      classifyOverride: (fresh: ReturnType<typeof decision>) => {
        if (fresh.headerHash === broken.headerHash) throw failure;
        return fresh;
      },
    });

    await expect(h.bridge.prepareForRecovery(current)).rejects.toBe(failure);
    expect(h.appended.map(({ headerHash }) => headerHash)).toEqual([
      fraud.headerHash,
    ]);
    expect(h.enqueued).toEqual([]);
    // The prepared prefix is what the next dispatch hands the supervisor.
    expect(h.bridge.status()).toEqual({
      observationDigest: current.observationDigest,
      target: expect.objectContaining({ headerHash: fraud.headerHash }),
    });
    await h.bridge.dispatchPrepared();
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      fraud.headerHash,
    ]);
  });

  it("acts on nothing when the failing header is the first one", async () => {
    const { current, fraud: first, broken: second } = queue();
    const failure = new Error("replay of the first block threw");
    const h = harness({
      current,
      categoryByHeader: {
        [first.headerHash]: "invalidRange",
        [second.headerHash]: "doubleSpend",
      },
      classifyOverride: (fresh: ReturnType<typeof decision>) => {
        if (fresh.headerHash === first.headerHash) throw failure;
        return fresh;
      },
    });

    // A fault after the failing header waits for it.
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(h.appended).toEqual([]);
    expect(h.enqueued).toEqual([]);
    expect(h.bridge.status().target).toBeNull();
  });

  it("keeps a target in flight when a header at or before it fails", async () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [earlier, targeted] = current.finalizedHeaders as [
      (typeof current.finalizedHeaders)[number],
      (typeof current.finalizedHeaders)[number],
    ];
    const failure = new Error("replay of the earlier block threw");
    let failing = false;
    const h = harness({
      current,
      categoryByHeader: {
        [earlier.headerHash]: "invalidRange",
        [targeted.headerHash]: "doubleSpend",
      },
      classifyOverride: (fresh: ReturnType<typeof decision>) => {
        if (fresh.headerHash === earlier.headerHash) {
          if (failing) throw failure;
          return { ...fresh, decision: "healthy" as const };
        }
        return fresh;
      },
    });
    const target = (await h.bridge.reconcileAndDispatch(current)).target;
    expect(target?.headerHash).toBe(targeted.headerHash);
    expect(h.controllerGenerations).toEqual(["1"]);

    failing = true;
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    // The target, its permit and its retained replay context stand.
    expect(h.bridge.status().target).toEqual(target);
    expect(h.revocations).toEqual([]);
    expect(h.retainedDecisionAuthorities.at(-1)).toBe(target?.decisionDigest);

    // Once the earlier header classifies, the same permit and rollback
    // generation carry on.
    failing = false;
    expect((await h.bridge.reconcileAndDispatch(current)).target).toEqual(
      target,
    );
    expect(h.controllerGenerations).toEqual(["1"]);
    expect(h.revocations).toEqual([]);
    expect(h.enqueuedGenerations).toEqual(["1", "1"]);
    expect(h.retainedDecisionAuthorities.at(-1)).toBe(target?.decisionDigest);
  });
});
