/**
 * Startup reclaims unused funding before the supervisor exists. A
 * reservation that sweep holds, because no workflow drives its signed
 * attempt, raises its failure alert on the operations sink at once, with
 * the time it was held, and clears it when a later sweep releases it.
 */
import { join } from "node:path";

import { supersededAttemptReadSchedule } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import type { WatcherOperationsSink } from "../../src/runtime/operations-observability.js";
import { openWatcherProverFundingRuntime } from "../../src/runtime/watcher-prover-funding-runtime.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

afterEach(async () => {
  vi.useRealTimers();
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type Alert = Parameters<WatcherOperationsSink["setAlert"]>[0];

describe("a funding hold recorded at startup", () => {
  it("raises proof_submission_failure at once with its own time, and clears it on release", async () => {
    const fixture = await setupFundingRecoveryFixture(false, true);
    fixture.workflowJournal.forgetObjective(fixture.objective);
    supersededAttemptReadSchedule.forget(fixture.transactionHash);
    fixture.chain.mockResolvedValue({ kind: "not_found" });
    const alerts: Alert[] = [];
    vi.useFakeTimers({ toFake: ["Date"] });
    vi.setSystemTime(1_000_000);

    const runtime = await openWatcherProverFundingRuntime({
      deploymentIdentity,
      path: join(fixture.journalRoot, "watcher.sqlite"),
      authenticationKey: TEST_JOURNAL_KEY,
      createProtocolParameters: async () => fixture.protocolParameters,
      launchScope: installedLaunchScope(fixture.old.launchScope),
      journalRoot: fixture.journalRoot,
      observeSignedTransaction: fixture.observeSignedTransaction,
      alertSink: { setAlert: (alert) => alerts.push(alert) },
    });
    try {
      expect(runtime.factory.decisionHolds()).toEqual([
        expect.objectContaining({
          kind: "reservation",
          reservationId: fixture.plan.reservationId,
        }),
      ]);
      expect(alerts).toEqual([
        {
          code: "proof_submission_failure",
          subjectDigest: fixture.plan.reservationId,
          active: true,
          observedAtMs: "1000000",
        },
      ]);

      fixture.chain.mockImplementation(async (txHash) =>
        expiredNotFound(txHash!),
      );
      vi.setSystemTime(1_060_000);
      await runtime.factory.releaseUnused();
      expect(runtime.factory.decisionHolds()).toEqual([]);
      expect(alerts.slice(1)).toEqual([
        {
          code: "proof_submission_failure",
          subjectDigest: fixture.plan.reservationId,
          active: false,
          observedAtMs: "1060000",
        },
      ]);
    } finally {
      runtime.store.close();
    }
  });
});
