import { existsSync, renameSync } from "node:fs";
import { join } from "node:path";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import {
  createWatcherFaultProofSupervisor,
  watcherFaultProofDeadline,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  closeWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
} from "../../src/fault-proofs/watcher-journal-database.js";
import type { WatcherFundingInputStanding } from "../../src/funding/prover-funding-input-facts.js";
import type { WatcherProofRetentionTarget } from "../../src/l1-follower/proof-retention.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import {
  openWatcherProverFundingRuntime,
  recheckFundingDecisionHolds,
} from "../../src/runtime/watcher-prover-funding-runtime.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";

// The funding fixture classifies a doubleSpend fault; install only that
// category, so the restarted watcher's journals share the fixture's scope.
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-application.js")
  >()),
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
}));

afterEach(cleanupFundingRecoveryFixtures);

/**
 * The README's journal repair: stop the watcher, move
 * `watcher-journals.sqlite` aside (never delete it), restart. The fresh
 * journals lack the decisions the surviving workflow journals and funding
 * reservations name. The watcher must start, hold that work unready under
 * `journal_decision_missing` without ever running it again or exiting, and
 * clear each hold once L1 facts resolve it.
 */

// The rollback key the restarted watcher writes its fresh journals under.
const KEY = Buffer.alloc(32, 0x91);

type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

const moveJournalsAside = (test: Fixture): void => {
  closeWatcherJournalDatabase(test.journalRoot);
  const path = join(test.journalRoot, WATCHER_JOURNAL_DATABASE_FILE);
  for (const suffix of ["", "-wal", "-shm"])
    if (existsSync(`${path}${suffix}`))
      renameSync(`${path}${suffix}`, `${path}.moved-aside${suffix}`);
};

const facts = () => {
  let standing: WatcherFundingInputStanding = {
    spent: [],
    unspent: [],
    undetermined: "the follower has no cursor yet",
  };
  return {
    facts: { standing: async () => standing },
    set: (next: WatcherFundingInputStanding) => {
      standing = next;
    },
  };
};

/** One watcher start: the funding runtime, then the supervisor and readiness. */
const start = async (
  test: Fixture,
  fundingInputFacts: ReturnType<typeof facts>["facts"],
  pins: string[],
) => {
  const funding = await openWatcherProverFundingRuntime({
    deploymentIdentity,
    path: join(test.journalRoot, "watcher.sqlite"),
    authenticationKey: KEY,
    createProtocolParameters: async () => test.protocolParameters,
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    journalRoot: test.journalRoot,
    fundingInputFacts,
  });
  const execute = vi.fn(async () => {
    throw new Error("held work must never run");
  });
  const label = ({ category, headerHash }: WatcherProofRetentionTarget) =>
    `${category}/${headerHash}`;
  const supervisor = createWatcherFaultProofSupervisor({
    journalRoot: test.journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    deadlineAlertHeadroomMs: MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    queueAuthenticationKey: KEY,
    proofRetention: {
      ...storelessProofRetention,
      pin: async (target) => {
        if (!pins.includes(label(target))) pins.push(label(target));
      },
      release: async (target) => {
        pins.splice(pins.indexOf(label(target)) >>> 0, 1);
      },
    },
    reservationDecisionHolds: funding.factory.decisionHolds,
    execution: {
      execute,
      verifyCompleted: async () => {
        throw new Error("held work must never verify");
      },
    },
  });
  let failed: unknown;
  supervisor.done.catch((error: unknown) => {
    failed = error;
  });
  const operations = createWatcherOperationsObservability({
    deploymentFingerprint: deploymentIdentity.manifestId,
    supervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    durableProofQueueStatus: () => supervisor.durableQueueStatus(),
    nowMs: () => 100_000n,
  });
  const readyz = async () => {
    const response = await handleWatcherOperationsHttpRequest(
      new Request("http://127.0.0.1/readyz"),
      operations.api,
    );
    return {
      status: response.status,
      body: (await response.json()) as {
        ready: boolean;
        reasons: readonly string[];
      },
    };
  };
  const stop = async () => {
    await supervisor.close();
    funding.store.close();
  };
  return {
    funding,
    supervisor,
    execute,
    readyz,
    stop,
    failed: () => failed,
  };
};

const settled = async (
  watcher: Awaited<ReturnType<typeof start>>,
): Promise<void> =>
  vi.waitFor(() => {
    const status = watcher.supervisor.status();
    expect(status.activeJob).toBeNull();
    expect(status.queuedJobCount).toBe(0);
  });

describe("journal repair by moving the journal database aside", () => {
  it("holds a pending proof unready across restarts and clears it once its header leaves the finalized queue", async () => {
    // A proof whose signed init attempt was submitted and is pending, with
    // its funding reservation active under that signed transition.
    const test = await setupFundingRecoveryFixture();
    const before = await test.records();
    expect(before).toHaveLength(1);
    expect(before[0]).toMatchObject({ state: "active" });
    expect(before[0]!.pendingTransition).not.toBeNull();
    const headerHash = test.old.headerHash;
    moveJournalsAside(test);
    const pins: string[] = [];
    const inputFacts = facts();

    // The restart reaches the operations server: nothing throws pre-bind.
    let watcher = await start(test, inputFacts.facts, pins);
    expect(watcher.supervisor.status()).toMatchObject({
      phase: "accepting",
      journalDecisionMissing: [],
    });
    // The bridge detects the fault again, under a fresh decision digest.
    const decisions = await openWatcherFaultDecisionJournal({
      directory: test.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      launchScope: test.old.launchScope,
      authenticationKey: KEY,
    });
    await decisions.appendLiveDecision(test.fresh);
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
      header: test.fixture,
    });
    const fault = (generation: string) => ({
      observation,
      rollbackGeneration: generation,
      fault: {
        decision: test.fresh,
        deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
        actuationPermit: createWorkflowActuationPermitController({
          decision: test.fresh,
          rollbackGeneration: generation,
        }).permit,
      },
    });
    const hold = {
      kind: "objective",
      category: "doubleSpend",
      headerHash,
      // The pending workflow names the decision only the moved-aside
      // journal holds.
      decisionDigest: test.old.decisionDigest,
    };
    await watcher.supervisor.requestProgress(fault("1"));
    await settled(watcher);
    expect(watcher.supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 1,
      journalDecisionMissing: [hold],
    });
    expect(await watcher.readyz()).toMatchObject({
      status: 503,
      body: {
        ready: false,
        reasons: expect.arrayContaining(["journal_decision_missing"]),
      },
    });
    expect(pins).toEqual([`doubleSpend/${headerHash}`]);
    // A held objective takes no new work while its header stays queued.
    await watcher.supervisor.requestProgress(fault("2"));
    await settled(watcher);
    expect(watcher.execute).not.toHaveBeenCalled();
    expect(watcher.failed()).toBeUndefined();
    await watcher.stop();

    // A second restart holds it again on its first observation (the queue
    // loads lazily): no throw, no exit, no loop.
    watcher = await start(test, inputFacts.facts, pins);
    await watcher.supervisor.requestProgress(fault("3"));
    await settled(watcher);
    expect(watcher.supervisor.status()).toMatchObject({
      phase: "accepting",
      journalDecisionMissing: [hold],
    });
    expect(await watcher.readyz()).toMatchObject({ status: 503 });
    expect(watcher.execute).not.toHaveBeenCalled();
    // The signed reservation is never released or dropped by the hold.
    expect(await test.records()).toEqual(before);

    // The proof (or another) landed and is final: the header left the queue.
    await watcher.supervisor.requestProgress({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        revision: 2,
      }),
      rollbackGeneration: "3",
    });
    await settled(watcher);
    await vi.waitFor(() =>
      expect(watcher.supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 0,
        journalDecisionMissing: [],
      }),
    );
    expect((await watcher.readyz()).body.reasons).not.toContain(
      "journal_decision_missing",
    );
    expect(pins).toEqual([]);
    expect(watcher.execute).not.toHaveBeenCalled();
    expect(watcher.failed()).toBeUndefined();
    await watcher.stop();

    // The cleared objective stays cleared across a further restart.
    watcher = await start(test, inputFacts.facts, pins);
    expect(watcher.supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      journalDecisionMissing: [],
    });
    expect(watcher.failed()).toBeUndefined();
    await watcher.stop();
  });

  it.each([
    ["returned to the wallet", "unspent"],
    ["dropped", "spent"],
  ] as const)(
    "holds an unused reservation at startup and, once its inputs are final, it is %s",
    async (_outcome, standing) => {
      const test = await setupFundingRecoveryFixture(false, false, true);
      const before = await test.records();
      expect(before).toHaveLength(1);
      const reservation = before[0]!;
      expect(reservation.activeInputs.length).toBeGreaterThan(0);
      expect(reservation.pendingTransition).toBeNull();
      moveJournalsAside(test);
      const inputFacts = facts();
      const pins: string[] = [];

      const watcher = await start(test, inputFacts.facts, pins);
      const hold = {
        kind: "reservation",
        reservationId: reservation.reservationId,
        decisionDigest: reservation.decisionDigest,
      };
      expect(watcher.funding.factory.decisionHolds()).toMatchObject([hold]);
      expect(watcher.supervisor.status()).toMatchObject({
        phase: "accepting",
        journalDecisionMissing: [hold],
      });
      expect(await watcher.readyz()).toMatchObject({
        status: 503,
        body: { reasons: expect.arrayContaining(["journal_decision_missing"]) },
      });
      expect(await test.records()).toEqual(before);

      // Each follower change reads the held inputs again.
      const listeners = new Set<() => void>();
      const unsubscribe = recheckFundingDecisionHolds(
        watcher.funding.factory,
        (listener) => {
          listeners.add(listener);
          return () => listeners.delete(listener);
        },
      );
      const tick = () => {
        for (const listener of listeners) listener();
      };
      const outRefs = reservation.activeInputs.map(({ outRef }) => outRef);
      // Unspent above the final point only: still held.
      inputFacts.set({
        spent: [],
        unspent: [],
        undetermined: `${outRefs[0]!} is spent above the release-final point`,
      });
      tick();
      await vi.waitFor(() =>
        expect(watcher.funding.factory.decisionHolds()).toMatchObject([
          {
            ...hold,
            detail: expect.stringContaining(
              "spent above the release-final point",
            ),
          },
        ]),
      );
      expect(await test.records()).toEqual(before);

      inputFacts.set(
        standing === "spent"
          ? { spent: outRefs, unspent: [], undetermined: null }
          : { spent: [], unspent: outRefs, undetermined: null },
      );
      tick();
      await vi.waitFor(() =>
        expect(watcher.funding.factory.decisionHolds()).toEqual([]),
      );
      expect(watcher.supervisor.status().journalDecisionMissing).toEqual([]);
      expect((await watcher.readyz()).body.reasons).not.toContain(
        "journal_decision_missing",
      );
      const after = await test.records();
      expect(after).toHaveLength(1);
      expect(after[0]).toMatchObject({
        reservationId: reservation.reservationId,
        state: "active",
        activeInputs: [],
      });
      expect(await test.store.readReservedOutRefs({})).toEqual([]);
      unsubscribe();
      expect(watcher.failed()).toBeUndefined();
      await watcher.stop();
    },
  );
});
