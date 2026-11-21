import { describe, expect, it } from "vitest";

import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { supervisor } from "./operations-observability.supervisor.js";

const observability = () =>
  createWatcherOperationsObservability({
    deploymentFingerprint: "11".repeat(32),
    supervisor: supervisor().runtime,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 1,
      oldestQueuedAtMs: "1000",
    }),
  });

const verification = {
  subjectDigest: "44".repeat(32),
  queuedAtMs: "90000",
  startedAtMs: "92000",
  completedAtMs: "96000",
  elapsedMs: "4000",
  outcome: "pending_da" as const,
};

describe("operations verification subject", () => {
  it("keeps the classified header hash on a verification diagnostic", () => {
    const subject = observability();
    subject.sink.recordVerification({
      ...verification,
      headerHash: "ab".repeat(28),
    });
    subject.sink.recordVerification(verification);
    expect(subject.api.diagnostics({ kind: "verification" }).records).toEqual([
      expect.objectContaining({
        headerHash: "ab".repeat(28),
        outcome: "pending_da",
      }),
      expect.not.objectContaining({ headerHash: expect.anything() }),
    ]);
  });

  it("keeps the payload envelope digest a verified header was replayed from", () => {
    const subject = observability();
    subject.sink.recordVerification({
      ...verification,
      headerHash: "ab".repeat(28),
      payloadEnvelopeSha256: "5a".repeat(32),
      outcome: "verified",
    });
    expect(subject.api.diagnostics({ kind: "verification" }).records).toEqual([
      expect.objectContaining({
        headerHash: "ab".repeat(28),
        payloadEnvelopeSha256: "5a".repeat(32),
        outcome: "verified",
      }),
    ]);
  });

  it("keeps deferred-header retries out of the verification latency samples", () => {
    const subject = observability();
    subject.sink.recordVerification(verification);
    subject.sink.recordVerification({ ...verification, elapsedMs: "9000" });
    expect(subject.api.metrics().verificationLatencyMs).toEqual({
      sampleCount: "0",
      p50: null,
      p95: null,
      maximum: null,
    });
    subject.sink.recordVerification({
      ...verification,
      elapsedMs: "250",
      outcome: "verified",
    });
    expect(subject.api.metrics().verificationLatencyMs).toEqual({
      sampleCount: "1",
      p50: "250",
      p95: "250",
      maximum: "250",
    });
    // The deferral itself stays visible as a diagnostic.
    expect(
      subject.api
        .diagnostics({ kind: "verification" })
        .records.map((record) => (record as { outcome: string }).outcome),
    ).toEqual(["pending_da", "pending_da", "verified"]);
  });

  it("keeps the merge transaction of a header L1 merged before verification", () => {
    const subject = observability();
    subject.sink.recordVerification({
      ...verification,
      headerHash: "ab".repeat(28),
      mergeTransactionHash: "4d".repeat(32),
      outcome: "unverified_merged",
    });
    expect(subject.api.diagnostics({ kind: "verification" }).records).toEqual([
      expect.objectContaining({
        headerHash: "ab".repeat(28),
        mergeTransactionHash: "4d".repeat(32),
        outcome: "unverified_merged",
      }),
    ]);
    // Nothing was classified, so there is no latency sample.
    expect(subject.api.metrics().verificationLatencyMs.sampleCount).toBe("0");
  });

  it.each([
    ["an uppercase header hash", { headerHash: "AB".repeat(28) }],
    ["a 32-byte header hash", { headerHash: "ab".repeat(32) }],
    ["a short subject digest", { subjectDigest: "44".repeat(28) }],
    [
      "an uppercase payload envelope digest",
      { payloadEnvelopeSha256: "5A".repeat(32) },
    ],
    [
      "a 28-byte payload envelope digest",
      { payloadEnvelopeSha256: "5a".repeat(28) },
    ],
    [
      "a 28-byte merge transaction hash",
      { mergeTransactionHash: "4d".repeat(28) },
    ],
  ])("refuses %s", (_label, override) => {
    const subject = observability();
    expect(() =>
      subject.sink.recordVerification({ ...verification, ...override }),
    ).toThrow(
      /verification (header hash|subject digest|payload envelope digest|merge transaction hash) is invalid/u,
    );
    expect(subject.api.diagnostics({ kind: "verification" }).records).toEqual(
      [],
    );
  });
});
