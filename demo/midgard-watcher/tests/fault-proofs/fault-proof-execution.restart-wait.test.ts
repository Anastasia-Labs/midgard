/** A run held by the restart wait: the objective is pending under
 * `fault_proof_restart_wait`, the reservation minted for the run is released
 * on the way out, no observation admits it again while the wait lasts, and
 * the first one after the wait is over runs it. */
import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { setTimeout as waitForDisk } from "node:timers/promises";

import {
  FAULT_PROOF_RESTART_WAIT,
  WALLET_ONLY_VALIDITY_MS,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
} from "../support/fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

const paths: string[] = [];
afterEach(async () => {
  await cleanupFundingRecoveryFixtures();
  await Promise.all(
    paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
});

const waitFor = async (predicate: () => boolean) => {
  for (let attempt = 0; attempt < 1_000; attempt++) {
    if (predicate()) return;
    await waitForDisk(5);
  }
  throw new Error("supervisor did not reach its expected state");
};

describe("supervisor execution under the restart wait", () => {
  it("holds a gated run pending, releases its reservation, and runs it once the wait is over", async () => {
    const test = await setupFaultProofExecution(true);
    // The run's decision is recorded where its funding is, as in a watcher
    // process: an unused reservation of it is released when the run ends.
    const decisions = await openWatcherFaultDecisionJournal({
      directory: join(test.fixture.journalRoot, "fresh"),
      deploymentFingerprint: deploymentIdentity.manifestId,
      launchScope: installedLaunchScope(test.fixture.fresh.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
    });
    await decisions.appendLiveDecision(test.fixture.fresh);
    const fresh = test.fixture.fresh.decisionDigest;
    // The wallet coins the run's decision holds in its reservations.
    const coinsHeld = async () =>
      (await test.fixture.records())
        .filter((record) => record.decisionDigest === fresh)
        .flatMap(({ activeInputs }) => activeInputs);
    const objective = `doubleSpend/${test.input.job.headerHash}`;
    const reason = `${FAULT_PROOF_RESTART_WAIT}: init spends only wallet coins`;
    // What the runner hands back for a wallet-only action held by the wait.
    const gated = {
      kind: "pending",
      reason,
      resumeOnObservation: true,
      namedState: { name: FAULT_PROOF_RESTART_WAIT, alarm: false },
      entries: [],
    };
    const heldDuringRun: number[] = [];
    let proceeded = 0;
    test.runOrResume.mockImplementation(
      async (invocation: WorkflowAdapterRunnerInput) => {
        heldDuringRun.push((await coinsHeld()).length);
        if (!(await invocation.walletOnlySend.open())) return gated;
        proceeded++;
        return {
          kind: "pending",
          reason: "signed transaction awaiting inclusion",
        };
      },
    );

    const root = await mkdtemp("/var/tmp/midgard-restart-wait-execution-");
    paths.push(root);
    // One-second slots from 0: slot n starts at n seconds.
    const slotConfig = { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 };
    const constructedAtMs = 2_000;
    let now = constructedAtMs;
    let applied: number | null = 2;
    const outcomes: unknown[] = [];
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: "dd".repeat(32),
      unsafeNowMsForTest: () => now,
      unsafeL1ClockForTest: { slotConfig, appliedSlot: () => applied },
      run: async (_job, admission) => {
        const outcome = await test.execution.execute({
          ...test.input,
          job: { ...test.input.job, mode: "run" },
          admission: { ...admission, mode: "run", funding: "create_or_resume" },
        });
        outcomes.push(outcome);
        return outcome;
      },
    });
    let failure: unknown;
    void supervisor.done.catch((error: unknown) => {
      failure = error;
    });
    const job = {
      mode: "run" as const,
      category: "doubleSpend" as const,
      headerHash: test.input.job.headerHash,
      decisionDigest: test.input.job.decisionDigest,
      rollbackGeneration: "2",
      observationRevision: "first",
    };
    // An observation of the objective, as each follower change brings.
    const admitted = async (revision: string, count: number) => {
      await supervisor.unsafeScheduleForTest({
        ...job,
        observationRevision: revision,
      });
      await waitFor(
        () =>
          failure !== undefined ||
          (outcomes.length === count && supervisor.status().activeJob === null),
      );
      if (failure !== undefined) throw failure;
    };
    try {
      await supervisor.recoverExisting(null);
      await admitted("first", 1);
      expect(outcomes[0]).toEqual({
        kind: "pending",
        resume: "await_observation",
        reason,
        namedState: FAULT_PROOF_RESTART_WAIT,
      });
      expect(test.execution.readiness()).toEqual([
        { reason: FAULT_PROOF_RESTART_WAIT, detail: `${objective}: ${reason}` },
      ]);
      // The run held a fresh reservation, and nothing of it is held after the
      // gated return.
      expect(heldDuringRun).toHaveLength(1);
      expect(heldDuringRun[0]).toBeGreaterThan(0);
      expect(await coinsHeld()).toEqual([]);

      // The clock is past the instant, the follower's applied block is not:
      // no observation admits the held objective, so nothing is rebuilt or
      // preflighted again, and its named state stays in readiness.
      const instantSlot = (constructedAtMs + WALLET_ONLY_VALIDITY_MS) / 1_000;
      now = constructedAtMs + WALLET_ONLY_VALIDITY_MS;
      for (const [index, revision] of ["second", "third", "fourth"].entries()) {
        applied = instantSlot - 3 + index;
        await admitted(revision, 1);
        expect(test.runOrResume).toHaveBeenCalledTimes(1);
        expect(test.execution.readiness()).toEqual([
          {
            reason: FAULT_PROOF_RESTART_WAIT,
            detail: `${objective}: ${reason}`,
          },
        ]);
      }
      expect(proceeded).toBe(0);

      // The next block is past it too: the next observation runs the action.
      applied = instantSlot;
      await admitted("fifth", 2);
      expect(proceeded).toBe(1);
      expect(test.execution.readiness()).toEqual([]);
      expect(test.runOrResume).toHaveBeenCalledTimes(2);
    } finally {
      await supervisor.close();
    }
  });
});
