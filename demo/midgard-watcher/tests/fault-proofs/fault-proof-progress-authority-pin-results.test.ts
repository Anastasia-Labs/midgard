import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";

import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it } from "vitest";

import * as DecisionJournal from "../../src/fault-proofs/fault-decision-journal.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.js";
import type {
  WatcherProofPinResult,
  WatcherProofRetention,
} from "../../src/l1-follower/proof-retention.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// Each place the progress authority pins an objective keeps the pin's
// result: a pin pruning beat leaves the objective open and is retried on
// every new observation; a pin that holds is never retried.
afterEach(cleanupFundingRecoveryFixtures);

type PinKind = WatcherProofPinResult["kind"];

/** A retention answering each pin with the next scripted result (the last repeats). */
const scripted = (...results: PinKind[]) => {
  const pins: string[] = [];
  const released: string[] = [];
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    pin: async ({ category, headerHash }) => {
      pins.push(`${category}/${headerHash}`);
      return { kind: results.length > 1 ? results.shift()! : results[0]! };
    },
    release: async ({ category, headerHash }) => {
      released.push(`${category}/${headerHash}`);
    },
  };
  return { retention, pins, released };
};

const observationAt = (
  revision: number,
  header?: Parameters<typeof progressObservation>[0]["header"],
) =>
  progressObservation({
    deploymentFingerprint: deploymentIdentity.manifestId,
    revision,
    ...(header === undefined ? {} : { header }),
  });

/** A watcher restarting over one recorded, unfinished objective. */
const restartWith = async (retention: WatcherProofRetention) => {
  const fixture = await setupFundingRecoveryFixture(false, false, false);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot: fixture.journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    categories: fixture.old.launchScope,
    authenticationKey: TEST_JOURNAL_KEY,
    retention,
  });
  return { fixture, authority };
};
const restart = async (...results: PinKind[]) => {
  const recorded = scripted(...results);
  return { ...(await restartWith(recorded.retention)), ...recorded };
};

const roots: string[] = [];
afterEach(async () => {
  for (const root of roots.splice(0))
    await rm(root, { recursive: true, force: true });
});
/** A watcher whose live writer recorded one fault no objective holds yet. */
const newFaultWith = async (retention: WatcherProofRetention) => {
  const fixture = await setupFundingRecoveryFixture();
  const journalRoot = await mkdtemp(
    join(process.cwd(), ".watcher-progress-pin-results-"),
  );
  roots.push(journalRoot);
  const writer = await DecisionJournal.openWatcherFaultDecisionJournal({
    directory: journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope: fixture.old.launchScope,
    authenticationKey: TEST_JOURNAL_KEY,
  });
  await writer.appendLiveDecision(fixture.old);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    categories: fixture.old.launchScope,
    authenticationKey: TEST_JOURNAL_KEY,
    retention,
  });
  const header = {
    header: fixture.fixture.header,
    headerHash: fixture.old.headerHash,
  };
  const admit = (revision: number) => {
    const observation = observationAt(revision, header);
    return authority.admit({
      observation,
      rollbackGeneration: "1",
      fault: {
        decision: fixture.old,
        deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
        actuationPermit: createWorkflowActuationPermitController({
          decision: fixture.old,
          rollbackGeneration: "1",
        }).permit,
      },
    });
  };
  return { fixture, authority, admit };
};

describe("proof progress keeps every pin result", () => {
  it.each([
    ["pinned", 1],
    ["already_pruned", 2],
  ] as const)(
    "restoring an objective at startup: %s is retried on a new observation %d time(s) in all",
    async (result, pins) => {
      const test = await restart(result);
      await test.authority.admit({
        observation: observationAt(1),
        rollbackGeneration: "2",
      });
      expect(test.pins).toHaveLength(1);
      await test.authority.admit({
        observation: observationAt(2),
        rollbackGeneration: "2",
      });
      expect(test.pins).toHaveLength(pins);
      expect(test.released).toEqual([]);
      expect(test.authority.unfinishedCount()).toBe(1);
    },
  );

  it.each([
    ["pinned", 2],
    ["already_pruned", 3],
  ] as const)(
    "adopting an execution update: %s is retried on a new observation %d time(s) in all",
    async (result, pins) => {
      const test = await restart("pinned", result);
      const old = test.fixture.old;
      await test.authority.admit({
        observation: observationAt(1),
        rollbackGeneration: "2",
      });
      // Completed without a verified marker: unindexed, still held.
      await test.authority.markCompleted(old);
      expect(test.released).toEqual([]);
      await test.authority.updateExecution({
        objective: old,
        execution: {
          workflowId: test.fixture.initial.workflowId,
          entries: test.fixture.originalEntries,
        },
      });
      expect(test.pins).toHaveLength(2);
      await test.authority.admit({
        observation: observationAt(2),
        rollbackGeneration: "2",
      });
      expect(test.pins).toHaveLength(pins);
      expect(test.released).toEqual([]);
      expect(test.authority.unfinishedCount()).toBe(1);
    },
  );

  it.each([
    ["pinned", 1],
    ["already_pruned", 2],
  ] as const)(
    "admitting a new fault: %s is retried on a new observation %d time(s) in all",
    async (result, pins) => {
      const recorded = scripted(result);
      const { fixture, authority, admit } = await newFaultWith(
        recorded.retention,
      );
      await admit(1);
      await admit(1);
      expect(recorded.pins).toHaveLength(1);
      await admit(2);
      expect(recorded.pins).toEqual(
        Array.from(
          { length: pins },
          () => `${fixture.old.category}/${fixture.old.headerHash}`,
        ),
      );
      expect(recorded.released).toEqual([]);
      expect(authority.unfinishedCount()).toBe(1);
    },
  );

  it("stops retrying once a retried pin holds", async () => {
    const test = await restart("already_pruned", "pinned");
    for (const revision of [1, 2, 3, 4])
      await test.authority.admit({
        observation: observationAt(revision),
        rollbackGeneration: "2",
      });
    expect(test.pins).toHaveLength(2);
    expect(test.authority.unfinishedCount()).toBe(1);
  });
});

/**
 * A retention whose pins wait until the test answers them: the invariant
 * the capture path relies on is that no context for an objective is handed
 * out before its pin returned `pinned` or `already_pruned`.
 */
const gated = () => {
  const waiting: ((kind: PinKind) => void)[] = [];
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    pin: () =>
      new Promise<WatcherProofPinResult>((resolve) => {
        waiting.push((kind) => resolve({ kind }));
      }),
  };
  /** Waits for the next pin, checks `pending` has not settled, then answers it. */
  const answer = async (pending: Promise<unknown>, kind: PinKind) => {
    let settled = false;
    void pending.then(
      () => (settled = true),
      () => (settled = true),
    );
    const deadline = Date.now() + 10_000;
    while (waiting.length === 0) {
      if (Date.now() > deadline) throw new Error("no pin was asked for");
      await new Promise((resolve) => setTimeout(resolve, 5));
    }
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(settled).toBe(false);
    waiting.shift()!(kind);
  };
  return { retention, answer };
};

describe("proof progress hands out an objective's context only after its pin answered", () => {
  const KINDS = ["pinned", "already_pruned"] as const;

  it.each(KINDS)(
    "restoring an objective at startup (answered %s)",
    async (kind) => {
      const pins = gated();
      const test = await restartWith(pins.retention);
      const contexts = test.authority.admit({
        observation: observationAt(1),
        rollbackGeneration: "2",
      });
      await pins.answer(contexts, kind);
      expect(await contexts).toMatchObject([
        { decision: { decisionDigest: test.fixture.old.decisionDigest } },
      ]);
    },
  );

  it.each(KINDS)("adopting an execution update (answered %s)", async (kind) => {
    const pins = gated();
    const test = await restartWith(pins.retention);
    const first = test.authority.admit({
      observation: observationAt(1),
      rollbackGeneration: "2",
    });
    await pins.answer(first, "pinned");
    await first;
    await test.authority.markCompleted(test.fixture.old);
    const permit = test.authority.reconcileExecution({
      objective: test.fixture.old,
      execution: {
        workflowId: test.fixture.initial.workflowId,
        entries: test.fixture.originalEntries,
      },
      rollbackGeneration: "2",
    });
    await pins.answer(permit, kind);
    expect(await permit).toBeDefined();
    expect(test.authority.unfinishedCount()).toBe(1);
  });

  it.each(KINDS)("admitting a new fault (answered %s)", async (kind) => {
    const pins = gated();
    const test = await newFaultWith(pins.retention);
    const contexts = test.admit(1);
    await pins.answer(contexts, kind);
    expect(await contexts).toMatchObject([
      { decision: { decisionDigest: test.fixture.old.decisionDigest } },
    ]);
  });
});
