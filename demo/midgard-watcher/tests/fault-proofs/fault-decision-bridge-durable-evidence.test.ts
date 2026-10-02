import { mkdtemp, rm } from "node:fs/promises";

import type { HeaderDecision } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it } from "vitest";

import {
  unsafeOpenWatcherFaultDecisionJournalForTest,
  type WatcherPersistedFaultDecisionRecord,
} from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { harness } from "./fault-decision-bridge.harness.js";
import {
  decision,
  DEPLOYMENT,
  headerFixture,
  observation,
  OBSERVATION_DIGEST,
} from "./fault-decision-bridge.observation.js";

const directories: string[] = [];

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => await rm(path, { force: true, recursive: true })),
  );
});

/** A healthy decision whose replay context (and so digest) varies. */
const healthy = (
  headerHash: string,
  replayByte: string,
  authenticatedObservationDigest = OBSERVATION_DIGEST,
): HeaderDecision => {
  const launchScope = [...WATCHER_INSTALLED_WORKFLOW_CATEGORIES];
  const content = {
    schemaVersion: "midgard-production-header-decision-v1" as const,
    classifierVersion: "midgard-production-header-classifier-v1" as const,
    deploymentFingerprint: DEPLOYMENT,
    headerHash,
    authenticatedObservationDigest,
    payloadEnvelopeSha256: "26".repeat(32),
    payloadSha256: "27".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1" as const,
    replayDigest: replayByte.repeat(32),
    launchScope,
    launchScopeDigest: watcherSha256CanonicalJson(launchScope),
    classificationDigest: "2a".repeat(32),
    decision: "healthy" as const,
  };
  return Object.freeze({
    ...content,
    decisionDigest: watcherSha256CanonicalJson(content),
  }) as HeaderDecision;
};

const record = (
  value: HeaderDecision,
  revision: number,
): WatcherPersistedFaultDecisionRecord =>
  Object.freeze({
    schemaVersion: "midgard-watcher-production-fault-decision-record-v1",
    revision: revision.toString(),
    priorRecordSha256: null,
    decision: value,
  });

describe("fault decision bridge durable evidence", () => {
  it("does not journal or compare a healthy decision whose replay context moved", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const durable = healthy(header!.headerHash, "31");
    const fresh = healthy(header!.headerHash, "32");
    expect(fresh.decisionDigest).not.toBe(durable.decisionDigest);
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      records: [record(durable, 0)],
      classifyOverride: () => fresh,
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target).toBeNull();
    expect(prepared.decisionDigests).toEqual([fresh.decisionDigest]);
    expect(h.appended).toEqual([]);
  });

  it("admits a journal holding repeated healthy records for one header and prepares over it", async () => {
    const current = observation([headerFixture("01"), headerFixture("02")]);
    const [first, second] = current.finalizedHeaders;
    const root = await mkdtemp("/var/tmp/midgard-bridge-durable-");
    directories.push(root);
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    // Shaped like a live journal: the same header decided healthy at two
    // observation depths, then once more under an already used identity.
    for (const value of [
      healthy(first!.headerHash, "31", "12".repeat(32)),
      healthy(first!.headerHash, "32"),
      healthy(first!.headerHash, "33"),
    ])
      await journal.unsafeAppendDecisionEnvelopeForTest(value);
    const records = await journal.readAll();
    expect(records).toHaveLength(3);
    const h = harness({
      current,
      categoryByHeader: {
        [first!.headerHash]: "doubleSpend",
        [second!.headerHash]: "transitionTrace",
      },
      records,
      classifyOverride: (fresh) =>
        fresh.headerHash === first!.headerHash
          ? healthy(first!.headerHash, "34")
          : fresh,
    });
    const prepared = await h.bridge.reconcileAndDispatch(current);
    expect(prepared.target?.headerHash).toBe(second!.headerHash);
    expect(h.appended.map(({ headerHash }) => headerHash)).toEqual([
      second!.headerHash,
    ]);
    expect(await journal.audit()).toEqual(records);
  });

  it("admits a fault found later for a header the journal once decided healthy", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      records: [record(healthy(header!.headerHash, "31"), 0)],
    });
    expect(
      (await h.bridge.reconcileAndDispatch(current)).target?.headerHash,
    ).toBe(header!.headerHash);
    expect(h.appended.map(({ decision }) => decision)).toEqual([
      "fault_detected",
    ]);
  });

  it("still refuses a fresh decision that differs from a durable fault", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const fault = decision(header!.headerHash, "doubleSpend");
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      records: [
        record(
          {
            ...fault,
            replayDigest: "31".repeat(32),
            decisionDigest: "ff".repeat(32),
          },
          0,
        ),
      ],
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toThrow(
      "differs from durable decision evidence",
    );
    expect(h.appended).toEqual([]);
  });

  it("still refuses two durable faults under one observation identity", async () => {
    const current = observation([headerFixture("01")]);
    const [header] = current.finalizedHeaders;
    const fault = decision(header!.headerHash, "doubleSpend");
    const h = harness({
      current,
      categoryByHeader: { [header!.headerHash]: "doubleSpend" },
      records: [
        record(fault, 0),
        record({ ...fault, decisionDigest: "ff".repeat(32) }, 1),
      ],
    });
    await expect(h.bridge.reconcileAndDispatch(current)).rejects.toThrow(
      "repeats a header observation identity",
    );
  });
});
