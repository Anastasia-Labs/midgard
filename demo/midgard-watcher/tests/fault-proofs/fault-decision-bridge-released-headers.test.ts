import { RetainedDaPayloadUnavailableError } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import type { WatcherReleasedHeaderProof } from "../../src/indexers/authenticated-state-queue-observation.js";
import { harness as anyTimeHarness } from "./fault-decision-bridge.harness.js";
import {
  encodedHeader,
  headerFixture,
  observation,
} from "./fault-decision-bridge.observation.js";
import {
  ATTESTED,
  MERGE_TX,
  operations,
  proof,
  records,
  REMOVAL_TX,
  removedProof,
  unavailable,
} from "./fault-decision-bridge.released-fixture.js";

/** The confirmed head every `headerFixture` names as its predecessor. */
const CONFIRMED_HEAD = "08".repeat(28);

const categories = (
  current: ReturnType<typeof observation>,
): Record<string, string> =>
  Object.fromEntries(
    current.finalizedHeaders.map(({ headerHash }) => [
      headerHash,
      "transitionTrace",
    ]),
  );

const twoAttested = () =>
  observation([headerFixture("01"), headerFixture("02")], "Idle", [
    ATTESTED,
    ATTESTED,
  ]);

const outcomes = (observability: ReturnType<typeof operations>) =>
  records(observability).map(({ headerHash, outcome }) => ({
    headerHash,
    outcome,
  }));

/**
 * The fixture headers end at 2 ms. A clock just after that keeps every header
 * inside its challengeability window: these cases are about in-window
 * removals and prunes, never the past-horizon skip.
 */
const harness = (input: Parameters<typeof anyTimeHarness>[0]) =>
  anyTimeHarness({ nowMs: () => 3n, ...input });

describe("fault decision bridge behind an L1 removal", () => {
  it("records a removed header as unverified_removed, warns once and stays ready", async () => {
    const current = twoAttested();
    const [removed, merged] = current.finalizedHeaders;
    const observability = operations();
    const warn = vi.fn();
    const released = new Map<string, WatcherReleasedHeaderProof>([
      [removed!.headerHash, removedProof(removed!.headerHash)],
      [merged!.headerHash, proof(merged!.headerHash)],
    ]);
    const pendingArguments: (ReadonlySet<string> | undefined)[] = [];
    const h = harness({
      current,
      categoryByHeader: categories(current),
      operationsSink: observability.sink,
      warn,
      mergedHeaders: async () => released,
      pendingAvailabilityHeaders: (_observation, skipped) => {
        pendingArguments.push(skipped);
        return new Set();
      },
    });
    for (let wake = 0; wake < 2; wake += 1)
      expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(h.application.classifyHeader).not.toHaveBeenCalled();
    expect(pendingArguments.at(-1)).toEqual(
      new Set([removed!.headerHash, merged!.headerHash]),
    );
    expect(outcomes(observability)).toEqual([
      { headerHash: removed!.headerHash, outcome: "unverified_removed" },
      { headerHash: merged!.headerHash, outcome: "unverified_merged" },
    ]);
    expect(records(observability)[0]).toMatchObject({
      removalTransactionHash: REMOVAL_TX,
      removalKind: "RemoveUnattestedBlockAfterTimeout",
    });
    expect(records(observability)[0]).not.toHaveProperty(
      "mergeTransactionHash",
    );
    expect(warn.mock.calls.map(([warning]) => warning)).toEqual([
      {
        event: "unverified_removed",
        headerHash: removed!.headerHash,
        transactionHash: REMOVAL_TX,
        removalKind: "RemoveUnattestedBlockAfterTimeout",
        lockedCorrectionTarget: false,
      },
      {
        event: "unverified_merged",
        headerHash: merged!.headerHash,
        transactionHash: MERGE_TX,
        lockedCorrectionTarget: false,
      },
    ]);
    const metrics = observability.api.metrics();
    expect(metrics.unverifiedHeaders).toEqual({
      merged: "1",
      removed: "1",
      pastHorizon: "0",
    });
    expect(metrics.verificationLatencyMs.sampleCount).toBe("0");
    expect(metrics.activeAlertCount).toBe("0");
    expect(observability.api.status().readinessReasons).not.toContain(
      "active_alert",
    );
  });

  it("warns and diagnoses a merged Locked correction target, which yields no target", async () => {
    const headers = [headerFixture("01"), headerFixture("02")];
    const target = encodedHeader(headers[0]!).hash;
    const current = observation(
      headers,
      {
        Locked: {
          target_header_hash: target,
          correction_identity: {
            FraudProof: {
              fraud_proof_asset_name: `${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS.doubleSpend}${target}`,
            },
          },
        },
      },
      [ATTESTED, ATTESTED],
    );
    const observability = operations();
    const warn = vi.fn();
    const h = harness({
      current,
      categoryByHeader: categories(current),
      operationsSink: observability.sink,
      warn,
      mergedHeaders: async () => new Map([[target, proof(target)]]),
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(warn).toHaveBeenCalledExactlyOnceWith({
      event: "unverified_merged",
      headerHash: target,
      transactionHash: MERGE_TX,
      lockedCorrectionTarget: true,
    });
    expect(outcomes(observability)[0]).toEqual({
      headerHash: target,
      outcome: "unverified_merged",
    });
  });

  it("returns a header to classification after a rollback un-removes it", async () => {
    const current = twoAttested();
    const [removed] = current.finalizedHeaders;
    const observability = operations();
    let removedOnL1 = true;
    const h = harness({
      current,
      categoryByHeader: categories(current),
      operationsSink: observability.sink,
      mergedHeaders: async () =>
        new Map(
          removedOnL1
            ? [[removed!.headerHash, removedProof(removed!.headerHash)]]
            : [],
        ),
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
    });
    await h.bridge.reconcileAndDispatch(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(1);
    h.bridge.invalidateForRollback();
    removedOnL1 = false;
    await h.bridge.reconcileAndDispatch(current);
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toContain(removed!.headerHash);
  });
});

describe("fault decision bridge behind a predecessor prune", () => {
  /** Classifying `header` reads `predecessor`'s payload, which is pruned. */
  const prunedPredecessor =
    (header: string, predecessor: string) =>
    <Fresh extends { headerHash: string }>(fresh: Fresh) => {
      if (fresh.headerHash === header) throw unavailable(predecessor);
      return { ...fresh, decision: "healthy" as const };
    };

  it("defers a header whose merged predecessor's payload was pruned", async () => {
    const current = twoAttested();
    const [merged, live] = current.finalizedHeaders;
    const observability = operations();
    let served = false;
    const h = harness({
      current,
      categoryByHeader: categories(current),
      operationsSink: observability.sink,
      mergedHeaders: async () =>
        new Map([[merged!.headerHash, proof(merged!.headerHash)]]),
      classifyOverride: (fresh) => {
        if (!served) throw unavailable(merged!.headerHash);
        return { ...fresh, decision: "healthy" };
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(outcomes(observability)).toEqual(
      expect.arrayContaining([
        { headerHash: merged!.headerHash, outcome: "unverified_merged" },
        { headerHash: live!.headerHash, outcome: "pending_da" },
      ]),
    );
    served = true;
    await h.bridge.retryDeferredClassification(current);
    expect(outcomes(observability).at(-1)).toEqual({
      headerHash: live!.headerHash,
      outcome: "verified",
    });
  });

  it("defers the queue head whose confirmed-head predecessor was pruned", async () => {
    const current = twoAttested();
    const [head] = current.finalizedHeaders;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: categories(current),
      operationsSink: observability.sink,
      mergedHeaders: async () => new Map(),
      classifyOverride: prunedPredecessor(head!.headerHash, CONFIRMED_HEAD),
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(outcomes(observability)).toContainEqual({
      headerHash: head!.headerHash,
      outcome: "pending_da",
    });
  });

  it.each([
    {
      label: "the predecessor is not merged",
      released: () => new Map<string, WatcherReleasedHeaderProof>(),
    },
    {
      label: "the predecessor was removed, not merged",
      released: (predecessor: string) =>
        new Map<string, WatcherReleasedHeaderProof>([
          [predecessor, removedProof(predecessor)],
        ]),
    },
  ])("fails closed when $label", async ({ released }) => {
    const current = twoAttested();
    const [predecessor, live] = current.finalizedHeaders;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: categories(current),
      operationsSink: observability.sink,
      mergedHeaders: async () => released(predecessor!.headerHash),
      classifyOverride: prunedPredecessor(
        live!.headerHash,
        predecessor!.headerHash,
      ),
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBeInstanceOf(
      RetainedDaPayloadUnavailableError,
    );
    expect(outcomes(observability)).toContainEqual({
      headerHash: live!.headerHash,
      outcome: "failed",
    });
  });

  it("fails closed when the queue head misses a payload other than its confirmed head's", async () => {
    const current = twoAttested();
    const [head] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: categories(current),
      mergedHeaders: async () => new Map(),
      classifyOverride: prunedPredecessor(head!.headerHash, "ee".repeat(28)),
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBeInstanceOf(
      RetainedDaPayloadUnavailableError,
    );
  });
});
