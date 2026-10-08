import {
  RetainedDaPayloadUnavailableError,
  TransitionTraceChallengerError,
} from "@al-ft/midgard-fault-proofs";
import { describe, expect, it } from "vitest";

import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  createWatcherOperationsObservability,
  type WatcherVerificationDiagnostic,
} from "../../src/runtime/operations-observability.js";
import { harness as anyTimeHarness } from "./fault-decision-bridge.harness.js";
import {
  DEPLOYMENT,
  headerFixture,
  observation,
  OBSERVATION_DIGEST,
} from "./fault-decision-bridge.observation.js";

const HEALTHY_ENVELOPE = "5a".repeat(32);

/**
 * The fixture headers end at 2 ms. A wall clock just after that keeps every
 * header inside its challengeability window, so a missing payload is still
 * waited for or refused rather than skipped past the horizon.
 */
const harness = (input: Parameters<typeof anyTimeHarness>[0]) =>
  anyTimeHarness({ nowMs: () => 3n, ...input });

const ATTESTED = Object.freeze({
  Attested: Object.freeze({ commitment_hash: "44".repeat(32) }),
});

const unavailable = (headerHash: string) =>
  new RetainedDaPayloadUnavailableError(
    headerHash,
    `no public retained-DA source served header ${headerHash}`,
  );

const operations = () =>
  createWatcherOperationsObservability({
    deploymentFingerprint: DEPLOYMENT,
    supervisor: {
      status: () => ({
        phase: "accepting",
        recovered: true,
        queuedJobCount: 0,
        activeJob: null,
        blockedJob: null,
        deadlineHealth: "safe",
        earliestDeadlineJob: null,
        remainingSafeStartMs: "1000",
      }),
    } as unknown as WatcherFaultProofSupervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
  });

const verifications = (observability: ReturnType<typeof operations>) =>
  observability.api
    .diagnostics({ kind: "verification" })
    .records.map((record) => ({
      headerHash: (record as { headerHash?: string }).headerHash,
      outcome: (record as { outcome: string }).outcome,
    }));

describe("fault decision bridge public-DA deferral", () => {
  it("defers an Unattested header whose own payload is not served yet and classifies it on the next wake", async () => {
    const current = observation(
      [headerFixture("01"), headerFixture("02")],
      "Idle",
      [ATTESTED, "Unattested"],
    );
    const [healthy, waiting] = current.finalizedHeaders;
    let served = false;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: {
        [healthy!.headerHash]: "doubleSpend",
        [waiting!.headerHash]: "transitionTrace",
      },
      operationsSink: observability.sink,
      classifyOverride: (fresh) => {
        if (fresh.headerHash === healthy!.headerHash)
          return {
            ...fresh,
            decision: "healthy",
            payloadEnvelopeSha256: HEALTHY_ENVELOPE,
          };
        if (!served) throw unavailable(waiting!.headerHash);
        return fresh;
      },
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target).toBeNull();
    expect(prepared.decisionDigests).toHaveLength(1);
    expect(h.enqueued).toEqual([]);
    expect(h.appended).toEqual([]);
    expect(verifications(observability)).toEqual(
      expect.arrayContaining([
        { headerHash: healthy!.headerHash, outcome: "verified" },
        { headerHash: waiting!.headerHash, outcome: "pending_da" },
      ]),
    );
    const records = observability.api.diagnostics({ kind: "verification" })
      .records as WatcherVerificationDiagnostic[];
    const verified = records.find(({ outcome }) => outcome === "verified");
    const pending = records.find(({ outcome }) => outcome === "pending_da");
    // Healthy decisions are not journaled: the verified record binds the
    // payload envelope the header was replayed from, and the deferral has none.
    expect(verified).toMatchObject({
      headerHash: healthy!.headerHash,
      outcome: "verified",
      payloadEnvelopeSha256: HEALTHY_ENVELOPE,
    });
    expect(pending).toMatchObject({ headerHash: waiting!.headerHash });
    expect(pending).not.toHaveProperty("payloadEnvelopeSha256");
    expect(records).toHaveLength(2);
    expect(observability.api.metrics().verificationLatencyMs.sampleCount).toBe(
      "1",
    );

    // Still not served: the retry defers again rather than failing closed.
    await h.bridge.reconcileAndDispatch(current);
    expect(h.enqueued).toEqual([]);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(4);

    served = true;
    await h.bridge.reconcileAndDispatch(current);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      waiting!.headerHash,
    ]);
    expect(h.appended.map(({ headerHash }) => headerHash)).toEqual([
      waiting!.headerHash,
    ]);
    expect(h.bridge.status().target?.headerHash).toBe(waiting!.headerHash);
    expect(verifications(observability).at(-1)).toEqual({
      headerHash: waiting!.headerHash,
      outcome: "fault_detected",
    });
    // A later pass re-classifies the healthy header and reuses the target.
    await h.bridge.reconcileAndDispatch(current);
    expect(h.application.classifyHeader).toHaveBeenCalledTimes(7);
    expect(h.bridge.status().target?.headerHash).toBe(waiting!.headerHash);
  });

  it("targets an Unattested header immediately when its faulty payload is served", async () => {
    const current = observation([headerFixture("01")]);
    const [faulty] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: { [faulty!.headerHash]: "doubleSpend" },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toEqual(
      expect.objectContaining({ headerHash: faulty!.headerHash }),
    );
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
  });

  it("keeps targeting a fault in the classified prefix while a later header waits", async () => {
    const current = observation([
      headerFixture("01"),
      headerFixture("02"),
      headerFixture("03"),
    ]);
    const [faulty, waiting, suffix] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: {
        [faulty!.headerHash]: "doubleSpend",
        [waiting!.headerHash]: "invalidRange",
        [suffix!.headerHash]: "transitionTrace",
      },
      classifyOverride: (fresh) => {
        if (fresh.headerHash === waiting!.headerHash)
          throw unavailable(waiting!.headerHash);
        return fresh;
      },
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target?.headerHash).toBe(faulty!.headerHash);
    expect(prepared.decisionDigests).toHaveLength(1);
    expect(h.enqueued.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
    // The suffix after the waiting header is not journaled until it can be
    // decided over a complete prefix.
    expect(h.appended.map(({ headerHash }) => headerHash)).toEqual([
      faulty!.headerHash,
    ]);
  });

  it("stops classifying the suffix once a header waits and checks it on the retry", async () => {
    const current = observation(
      [headerFixture("01"), headerFixture("02"), headerFixture("03")],
      "Idle",
      ["Unattested", ATTESTED, ATTESTED],
    );
    const [waiting, slow, suffix] = current.finalizedHeaders;
    const failure = unavailable(suffix!.headerHash);
    let served = false;
    const h = harness({
      current,
      categoryByHeader: {
        [waiting!.headerHash]: "doubleSpend",
        [slow!.headerHash]: "invalidRange",
        [suffix!.headerHash]: "transitionTrace",
      },
      classifyOverride: async (fresh) => {
        if (fresh.headerHash === waiting!.headerHash) {
          if (!served) throw unavailable(waiting!.headerHash);
          return { ...fresh, decision: "healthy" };
        }
        if (fresh.headerHash === slow!.headerHash) {
          // Still in flight when the first header defers.
          await new Promise((resolve) => setTimeout(resolve, 20));
          return { ...fresh, decision: "healthy" };
        }
        throw failure;
      },
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target).toBeNull();
    expect(prepared.decisionDigests).toEqual([]);
    expect(
      h.application.classifyHeader.mock.calls.map(
        ([request]) => request.header.headerHash,
      ),
    ).toEqual([waiting!.headerHash, slow!.headerHash]);

    // Once the waiting header is served, the suffix is classified and its
    // Attested unavailable payload still fails closed.
    served = true;
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(h.application.classifyHeader).toHaveBeenCalledWith(
      expect.objectContaining({
        header: expect.objectContaining({ headerHash: suffix!.headerHash }),
      }),
    );
  });

  it.each([
    {
      label: "the header is Attested",
      availability: ATTESTED,
      error: (headerHash: string) => unavailable(headerHash),
    },
    {
      label: "the error names another header",
      availability: "Unattested" as const,
      error: () => unavailable("ee".repeat(28)),
    },
    {
      label: "the failure is a plain fetchFailed",
      availability: "Unattested" as const,
      error: () =>
        new TransitionTraceChallengerError(
          "fetchFailed",
          "retained-DA source answered conflict",
        ),
    },
  ])("fails closed when $label", async ({ availability, error }) => {
    const current = observation([headerFixture("01")], "Idle", [availability]);
    const [header] = current.finalizedHeaders;
    const failure = error(header!.headerHash);
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      classifyOverride: () => {
        throw failure;
      },
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(h.enqueued).toEqual([]);
    expect(verifications(observability)).toEqual([
      { headerHash: header!.headerHash, outcome: "failed" },
    ]);
    // A failure is not a deferral: the next pass fails closed again.
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toBe(failure);
    expect(h.enqueued).toEqual([]);
  });

  it("records the authenticated observation as the deferred subject", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const observability = operations();
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      operationsSink: observability.sink,
      classifyOverride: () => {
        throw unavailable(header!.headerHash);
      },
    });
    expect((await h.bridge.reconcileAndDispatch(current)).target).toBeNull();
    expect(
      observability.api.diagnostics({ kind: "verification" }).records,
    ).toEqual([
      expect.objectContaining({
        subjectDigest: OBSERVATION_DIGEST,
        headerHash: header!.headerHash,
        outcome: "pending_da",
      }),
    ]);
  });
});
