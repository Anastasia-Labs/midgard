import { RETENTION_MS_PER_DAY } from "@al-ft/midgard-core";
import { describe, expect, it } from "vitest";

import { retentionReadinessFromDeadlines } from "../src/committee-service.js";
import type { StateQueueHeaderStatus } from "../src/domain.js";
import {
  pruneExpiredDaPayloads,
  retentionCandidates,
  retentionDeadlineReport,
} from "../src/store/retention.js";
import {
  hashOf,
  HEAD,
  LIVE_A,
  LIVE_B,
  NOW,
  openStore,
  PAST_HORIZON,
  payloadRecord,
  REQUIRED_RETENTION_MS,
  retentionOptions,
  seed,
} from "./store-retention.header-record.js";

describe("retentionCandidatesV1", () => {
  it("retains the L1 confirmed head's payload however old or removed", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: HEAD,
        endTimeMs: NOW - 60 * RETENTION_MS_PER_DAY,
        status: "removed",
      },
    ]);
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate).toMatchObject({
      queueReference: "confirmed_head",
      decision: { decision: "retain", reasonCode: "confirmed_head_payload" },
    });
  });

  it("retains headers live in the L1 queue past the horizon", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: LIVE_A, endTimeMs: PAST_HORIZON, status: "attested" },
      { headerHash: LIVE_B, withoutHeader: true, fetchedAtMs: PAST_HORIZON },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates.map((candidate) => candidate.decision)).toEqual([
      expect.objectContaining({
        decision: "retain",
        reasonCode: "live_queue_header",
      }),
      expect.objectContaining({
        decision: "retain",
        reasonCode: "live_queue_header",
      }),
    ]);
  });

  it("prunes a removed header immediately, inside the horizon", async () => {
    const store = await openStore();
    await seed(store, [{ headerHash: hashOf(1), status: "removed" }]);
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate?.decision).toMatchObject({
      decision: "prune",
      reasonCode: "removed_header",
    });
  });

  it("prunes any status once the horizon has strictly passed, not at it", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(2), endTimeMs: PAST_HORIZON, status: "merged" },
      { headerHash: hashOf(3), endTimeMs: PAST_HORIZON, status: "attested" },
      { headerHash: hashOf(4), endTimeMs: PAST_HORIZON, status: "conflicted" },
      {
        headerHash: hashOf(5),
        endTimeMs: NOW - REQUIRED_RETENTION_MS,
        status: "merged",
      },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(
      candidates.map((candidate) => candidate.decision.reasonCode),
    ).toEqual([
      "past_challengeability_horizon",
      "past_challengeability_horizon",
      "past_challengeability_horizon",
      "still_challengeable",
    ]);
  });

  it("decides a payload with no header row on its receipt time", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(6), withoutHeader: true, fetchedAtMs: NOW },
      { headerHash: hashOf(7), withoutHeader: true, fetchedAtMs: PAST_HORIZON },
    ]);
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates).toMatchObject([
      {
        headerStatus: "unobserved",
        blockEndTimeMs: NOW,
        decision: { decision: "retain", reasonCode: "still_challengeable" },
      },
      {
        headerStatus: "unobserved",
        blockEndTimeMs: PAST_HORIZON,
        decision: {
          decision: "prune",
          reasonCode: "past_challengeability_horizon",
        },
      },
    ]);
  });

  it("prunes an unexempt payload whose receipt time cannot be parsed", async () => {
    const store = await openStore();
    for (const headerHash of [hashOf(8), LIVE_A]) {
      await store.saveDaPayload({
        ...payloadRecord(headerHash),
        fetchedAt: "not-a-timestamp",
      });
    }
    const candidates = await retentionCandidates(store, retentionOptions());
    expect(candidates).toMatchObject([
      {
        headerHash: hashOf(8),
        blockEndTimeMs: 0,
        decision: {
          decision: "prune",
          reasonCode: "past_challengeability_horizon",
        },
      },
      {
        headerHash: LIVE_A,
        decision: { decision: "retain", reasonCode: "live_queue_header" },
      },
    ]);
  });

  it("holds a terminal record from a foreign deployment", async () => {
    const store = await openStore();
    await seed(store, [
      {
        headerHash: hashOf(8),
        endTimeMs: PAST_HORIZON,
        status: "merged",
        deploymentFingerprint: "ee".repeat(32),
      },
    ]);
    const [candidate] = await retentionCandidates(store, retentionOptions());
    expect(candidate).toMatchObject({
      fingerprintMismatch: true,
      terminalHistoryAuthorityMismatch: false,
      decision: { decision: "retain", reasonCode: "terminal_recovery_pending" },
    });
    const report = await retentionDeadlineReport(store, retentionOptions());
    expect(retentionReadinessFromDeadlines(report)).toMatchObject({
      status: "failed",
      error: expect.stringContaining("terminal_recovery_proof_unavailable"),
    });
  });
});

describe("terminal recovery payload retirement", () => {
  it("rechecks strict after-inclusion depth and authority", async () => {
    const store = await openStore();
    const headerHash = hashOf(14);
    await seed(store, [
      { headerHash, endTimeMs: PAST_HORIZON, status: "removed" },
    ]);
    const header = (await store.getStateQueueHeader(headerHash))!;
    const request = { ...retentionOptions(), headerHash };
    for (const depth of [12, 2160]) {
      await store.upsertStateQueueHeader({
        ...header,
        observedChainPoint: { ...header.observedChainPoint, depth },
      });
      expect(
        (await retentionCandidates(store, retentionOptions()))[0]?.decision
          .reasonCode,
      ).toBe("terminal_recovery_pending");
      expect(await store.deleteDaPayloadIfPrunable(request)).toBe(false);
      expect(await store.getDaPayload(headerHash)).toBeDefined();
    }
    await store.upsertStateQueueHeader({
      ...header,
      observedChainPoint: { ...header.observedChainPoint, depth: 2161 },
    });
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...request,
        automaticRecoveryMaxDepth: undefined,
      }),
    ).toBe(false);
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...request,
        deploymentFingerprint: "ee".repeat(32),
      }),
    ).toBe(false);
    expect(await store.deleteDaPayloadIfPrunable(request)).toBe(true);
  });
});

describe("pruneExpiredDaPayloadsV1", () => {
  it("deletes exactly the prunable payloads and keeps the exempt ones", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: HEAD, endTimeMs: PAST_HORIZON, status: "merged" },
      { headerHash: LIVE_A, endTimeMs: PAST_HORIZON, status: "attested" },
      { headerHash: hashOf(10), endTimeMs: PAST_HORIZON, status: "merged" },
      { headerHash: hashOf(11), status: "removed" },
      { headerHash: hashOf(12), status: "attested" },
    ]);
    const result = await pruneExpiredDaPayloads(store, retentionOptions());
    expect(result).toEqual({
      scanned: 5,
      prunedHeaderHashes: [hashOf(10), hashOf(11)],
      retained: 3,
    });
    expect(
      (await store.listDaPayloads()).map(({ headerHash }) => headerHash),
    ).toEqual([hashOf(12), HEAD, LIVE_A].sort());
  });

  it("re-decides inside the store's write boundary against the caller's view", async () => {
    const store = await openStore();
    await seed(store, [
      { headerHash: hashOf(13), endTimeMs: PAST_HORIZON, status: "merged" },
    ]);
    const request = {
      headerHash: hashOf(13),
      finalBlockTimeMs: NOW,
      confirmedHeadHash: HEAD,
      liveQueueHeaderHashes: new Set<string>(),
      automaticRecoveryMaxDepth: 2160,
      deploymentFingerprint: retentionOptions().deploymentFingerprint,
    };
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...request,
        confirmedHeadHash: hashOf(13),
      }),
    ).toBe(false);
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...request,
        liveQueueHeaderHashes: new Set([hashOf(13)]),
      }),
    ).toBe(false);
    expect(
      await store.deleteDaPayloadIfPrunable({
        ...request,
        finalBlockTimeMs: NOW - 2 * RETENTION_MS_PER_DAY,
      }),
    ).toBe(false);
    expect(await store.getDaPayload(hashOf(13))).toBeDefined();
    expect(await store.deleteDaPayloadIfPrunable(request)).toBe(true);
    expect(await store.getDaPayload(hashOf(13))).toBeUndefined();
    expect(await store.deleteDaPayloadIfPrunable(request)).toBe(false);
  });

  it("bounds the retained set by the head, the live queue, and the horizon", async () => {
    const store = await openStore();
    const statuses: readonly StateQueueHeaderStatus[] = [
      "unattested",
      "attesting",
      "attested",
      "merged",
      "removed",
      "conflicted",
    ];
    const entries = Array.from({ length: 60 }, (_, index) => ({
      headerHash: hashOf(20 + index),
      endTimeMs: NOW - index * RETENTION_MS_PER_DAY,
      status: statuses[index % statuses.length]!,
      withoutHeader: index % 7 === 0,
      fetchedAtMs: NOW - index * RETENTION_MS_PER_DAY,
    }));
    await seed(store, entries);
    const view = {
      confirmedHeadHash: hashOf(79),
      liveQueueHeaderHashes: new Set([hashOf(78), hashOf(77)]),
    };
    await pruneExpiredDaPayloads(store, { ...retentionOptions(), ...view });
    const retained = (await store.listDaPayloads()).map(
      ({ headerHash }) => headerHash,
    );
    const insideHorizon = entries.filter(
      (entry) =>
        NOW <= entry.endTimeMs + REQUIRED_RETENTION_MS &&
        (entry.withoutHeader || entry.status !== "removed"),
    );
    expect(retained.length).toBeLessThanOrEqual(1 + 2 + insideHorizon.length);
    for (const headerHash of retained) {
      expect(
        headerHash === view.confirmedHeadHash ||
          view.liveQueueHeaderHashes.has(headerHash) ||
          insideHorizon.some((entry) => entry.headerHash === headerHash),
      ).toBe(true);
    }
    expect(retained).toEqual(
      expect.arrayContaining([hashOf(79), hashOf(78), hashOf(77)]),
    );
  });
});
