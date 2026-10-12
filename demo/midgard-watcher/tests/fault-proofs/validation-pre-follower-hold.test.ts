import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherFaultProofSupervisor,
  watcherFaultProofDeadline,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  type WatcherDecisionHold,
  WatcherProofDecisionMissingError,
} from "../../src/fault-proofs/watcher-decision-hold.js";
import type { WatcherProofRetentionTarget } from "../../src/l1-follower/proof-retention.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// The funding fixture classifies a doubleSpend fault; install only that
// category, so the supervisor's journals share the fixture's scope.
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-application.js")
  >()),
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
}));

afterEach(cleanupFundingRecoveryFixtures);

/**
 * A proof started from a validation transcript archived before user events
 * were read from follower facts: the application's challenge port answers
 * it with a hold named `validation_transcript_pre_follower` instead of a
 * challenge whose digest its journal does not hold. The supervisor and its
 * progress authority hold the objective under that name, keep the process
 * up and never run it again, and clear it once its header leaves the
 * finalized queue. The hold path is the same for every category; the
 * fixture's recorded objective is a doubleSpend one.
 */
const start = async (
  test: Awaited<ReturnType<typeof setupFundingRecoveryFixture>>,
  hold: WatcherDecisionHold,
  pins: string[],
) => {
  const execute = vi.fn(async () => {
    // What `currentChallenge` throws for a held pre-follower decision.
    throw new WatcherProofDecisionMissingError(hold);
  });
  const label = ({ category, headerHash }: WatcherProofRetentionTarget) =>
    `${category}/${headerHash}`;
  const supervisor = createWatcherFaultProofSupervisor({
    journalRoot: test.journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    deadlineAlertHeadroomMs: MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    queueAuthenticationKey: TEST_JOURNAL_KEY,
    reservationDecisionHolds: () => [],
    proofRetention: {
      ...storelessProofRetention,
      pin: async (target) => {
        if (!pins.includes(label(target))) pins.push(label(target));
        return { kind: "pinned" };
      },
      release: async (target) => {
        pins.splice(pins.indexOf(label(target)) >>> 0, 1);
      },
    },
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
  const reasons = async () => {
    const response = await handleWatcherOperationsHttpRequest(
      new Request("http://127.0.0.1/readyz"),
      operations.api,
    );
    return ((await response.json()) as { reasons: readonly string[] }).reasons;
  };
  const settled = async () =>
    vi.waitFor(() => {
      const status = supervisor.status();
      expect(status.activeJob).toBeNull();
      expect(status.queuedJobCount).toBe(0);
    });
  return {
    supervisor,
    execute,
    reasons,
    settled,
    failed: () => failed,
  };
};

describe("a validation proof started from a pre-follower transcript", () => {
  it("is held under validation_transcript_pre_follower with the process up, and clears once its header leaves the finalized queue", async () => {
    const test = await setupFundingRecoveryFixture();
    const decision = test.old;
    const hold = Object.freeze({
      kind: "objective" as const,
      category: "doubleSpend" as const,
      headerHash: decision.headerHash,
      decisionDigest: decision.decisionDigest,
      detail: `held until the header leaves the finalized queue`,
      readiness: "validation_transcript_pre_follower" as const,
    });
    const pins: string[] = [];
    const watcher = await start(test, hold, pins);
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
      header: test.fixture,
    });
    const fault = (generation: string) => ({
      observation,
      rollbackGeneration: generation,
      fault: {
        decision,
        deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
        actuationPermit: createWorkflowActuationPermitController({
          decision,
          rollbackGeneration: generation,
        }).permit,
      },
    });
    await watcher.supervisor.requestProgress(fault("1"));
    await watcher.settled();
    await vi.waitFor(() =>
      expect(watcher.supervisor.status()).toMatchObject({
        phase: "accepting",
        blockedJob: null,
        journalDecisionMissing: [hold],
      }),
    );
    const held = await watcher.reasons();
    expect(held).toContain("validation_transcript_pre_follower");
    expect(held).not.toContain("journal_decision_missing");
    expect(pins).toEqual([`doubleSpend/${decision.headerHash}`]);
    const runs = watcher.execute.mock.calls.length;
    expect(runs).toBeGreaterThan(0);

    // Held work takes no new run while its header stays queued.
    await watcher.supervisor.requestProgress(fault("2"));
    await watcher.settled();
    expect(watcher.execute).toHaveBeenCalledTimes(runs);
    expect(watcher.failed()).toBeUndefined();

    // The header left the finalized queue: the hold clears, the pin goes.
    await watcher.supervisor.requestProgress({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        revision: 2,
      }),
      rollbackGeneration: "2",
    });
    await watcher.settled();
    await vi.waitFor(() =>
      expect(watcher.supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 0,
        journalDecisionMissing: [],
      }),
    );
    expect(await watcher.reasons()).not.toContain(
      "validation_transcript_pre_follower",
    );
    expect(pins).toEqual([]);
    expect(watcher.execute).toHaveBeenCalledTimes(runs);
    expect(watcher.failed()).toBeUndefined();
    await watcher.supervisor.close();
  }, 120_000);
});
