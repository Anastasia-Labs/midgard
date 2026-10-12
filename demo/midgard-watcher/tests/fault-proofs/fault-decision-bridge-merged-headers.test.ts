import { RetainedDaPayloadUnavailableError } from "@al-ft/midgard-fault-proofs";
import { FRAUD_PROOF_CATALOGUE_CATEGORY_IDS } from "@al-ft/midgard-sdk";
import { describe, expect, it, vi } from "vitest";

import { harness } from "./fault-decision-bridge.harness.js";
import {
  type decision,
  encodedHeader,
  headerFixture,
  observation,
  OBSERVATION_DIGEST,
} from "./fault-decision-bridge.observation.js";
import {
  ATTESTED,
  MERGE_TX,
  operations,
  proof,
  records,
  unavailable,
} from "./fault-decision-bridge.released-fixture.js";

/**
 * A watcher behind a merge: the queue head was merged on L1 and its public DA
 * pruned, so classifying it would fail closed on the missing payload.
 */
const behindMerge = () => {
  const current = observation(
    [headerFixture("01"), headerFixture("02")],
    "Idle",
    [ATTESTED, ATTESTED],
  );
  const [merged, live] = current.finalizedHeaders;
  return { current, merged: merged!, live: live! };
};

const prunedPayload =
  (headerHash: string) => (fresh: ReturnType<typeof decision>) => {
    if (fresh.headerHash === headerHash) throw unavailable(headerHash);
    return { ...fresh, decision: "healthy" as const };
  };

describe("fault decision bridge behind an L1 merge", () => {
  it("records a merged header as unverified_merged before any DA read and stays ready", async () => {
    const { current, merged, live } = behindMerge();
    const observability = operations();
    const mergedHeaders = vi.fn(
      async () => new Map([[merged.headerHash, proof(merged.headerHash)]]),
    );
    const pendingArguments: (ReadonlySet<string> | undefined)[] = [];
    const h = harness({
      current,
      categoryByHeader: {
        [merged.headerHash]: "doubleSpend",
        [live.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      mergedHeaders,
      pendingAvailabilityHeaders: (_observation, skipped) => {
        pendingArguments.push(skipped);
        return new Set();
      },
      classifyOverride: prunedPayload(merged.headerHash),
    });

    // Startup recovery and the runtime loop apply the same rule.
    const recovered = await h.bridge.prepareForRecovery(current);
    const prepared = await h.bridge.reconcileAndDispatch(current);
    for (const result of [recovered, prepared]) {
      expect(result.target).toBeNull();
      expect(result.decisionDigests).toHaveLength(1);
    }
    expect(mergedHeaders).toHaveBeenCalledTimes(2);
    expect(pendingArguments).toEqual([
      new Set([merged.headerHash]),
      new Set([merged.headerHash]),
    ]);
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([live.headerHash, live.headerHash]);
    expect(h.appended).toEqual([]);
    expect(h.enqueued).toEqual([]);
    // The merged header is reported once per merge, not once per wake.
    expect(
      records(observability).map(({ headerHash, outcome }) => ({
        headerHash,
        outcome,
      })),
    ).toEqual([
      { headerHash: merged.headerHash, outcome: "unverified_merged" },
      { headerHash: live.headerHash, outcome: "verified" },
      { headerHash: live.headerHash, outcome: "verified" },
    ]);
    expect(records(observability)[0]).toMatchObject({
      subjectDigest: OBSERVATION_DIGEST,
      mergeTransactionHash: MERGE_TX,
    });
    expect(records(observability)[0]).not.toHaveProperty(
      "payloadEnvelopeSha256",
    );
    // A skip is not a classification latency sample.
    expect(observability.api.metrics().verificationLatencyMs.sampleCount).toBe(
      "2",
    );
    // A later pass reads the live header again, never the merged one.
    await h.bridge.reconcileAndDispatch(current);
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([live.headerHash, live.headerHash, live.headerHash]);
  });

  it("still fails closed on an unmerged Attested header whose payload is unavailable", async () => {
    const { current, merged, live } = behindMerge();
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [merged.headerHash]: "doubleSpend",
        [live.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      // Merged on L1 only above the watcher's release depth: not yet proven.
      mergedHeaders: async () => new Map(),
      classifyOverride: prunedPayload(merged.headerHash),
      // Inside the header's challengeability window (fixtures end at 2 ms).
      nowMs: () => 3n,
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBeInstanceOf(
      RetainedDaPayloadUnavailableError,
    );
    expect(
      records(observability).find(
        ({ headerHash }) => headerHash === merged.headerHash,
      )?.outcome,
    ).toBe("failed");
    expect(
      records(observability).some(
        ({ outcome }) => outcome === "unverified_merged",
      ),
    ).toBe(false);
  });

  it("leaves an unmerged pending-availability header on the DA path", async () => {
    const { current, merged, live } = behindMerge();
    const h = harness({
      current,
      categoryByHeader: {
        [merged.headerHash]: "doubleSpend",
        [live.headerHash]: "transitionTrace",
      },
      mergedHeaders: async () => new Map(),
      pendingAvailabilityHeaders: () => new Set([merged.headerHash]),
      classifyOverride: prunedPayload(merged.headerHash),
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([live.headerHash]);
  });

  it("classifies a header again once a rollback un-merges it", async () => {
    const { current, merged, live } = behindMerge();
    const observability = operations();
    let mergedOnL1 = true;
    const h = harness({
      current,
      categoryByHeader: {
        [merged.headerHash]: "doubleSpend",
        [live.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      mergedHeaders: async () =>
        new Map(
          mergedOnL1 ? [[merged.headerHash, proof(merged.headerHash)]] : [],
        ),
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
    });
    await h.bridge.reconcileAndDispatch(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(1);

    h.bridge.invalidateForRollback();
    mergedOnL1 = false;
    await h.bridge.reconcileAndDispatch(current);
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([live.headerHash, merged.headerHash, live.headerHash]);

    // Merged again after the rollback: reported afresh.
    h.bridge.invalidateForRollback();
    mergedOnL1 = true;
    await h.bridge.reconcileAndDispatch(current);
    expect(
      records(observability)
        .filter(({ outcome }) => outcome === "unverified_merged")
        .map(({ headerHash }) => headerHash),
    ).toEqual([merged.headerHash, merged.headerHash]);
  });

  it("does not demand a runnable classification for a locked target L1 already merged", async () => {
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
    const live = current.finalizedHeaders[1]!.headerHash;
    const categoryByHeader = {
      [target]: "doubleSpend",
      [live]: "transitionTrace",
    };
    const h = harness({
      current,
      categoryByHeader,
      mergedHeaders: async () => new Map([[target, proof(target)]]),
      classifyOverride: prunedPayload(target),
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(h.enqueued).toEqual([]);

    // Unmerged, the same lock still requires its exact runnable decision.
    const unmerged = harness({
      current,
      categoryByHeader,
      mergedHeaders: async () => new Map(),
      classifyOverride: (fresh) => ({ ...fresh, decision: "healthy" }),
    });
    await expect(unmerged.bridge.reconcileAndDispatch(current)).rejects.toThrow(
      "locked fraud-proof target did not reproduce",
    );
  });

  it("refuses a merge proof for a header outside the observation", async () => {
    const { current, merged } = behindMerge();
    const h = harness({
      current,
      categoryByHeader: {},
      mergedHeaders: async () =>
        new Map([
          [merged.headerHash, proof(merged.headerHash)],
          ["ab".repeat(28), proof("ab".repeat(28))],
        ]),
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toThrow(
      "release proof names a header outside the observation",
    );
    expect(h.application.classifyHeader).not.toHaveBeenCalled();
  });
});
