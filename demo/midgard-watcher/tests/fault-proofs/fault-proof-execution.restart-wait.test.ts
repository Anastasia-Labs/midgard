/** A run held by the restart wait: the objective is pending under
 * `fault_proof_restart_wait`, each observation while the wait lasts runs it
 * again and leaves it held the same way, and the first one after the wait is
 * over sends its action. */
import { mkdtemp, rm } from "node:fs/promises";
import { setTimeout as waitForDisk } from "node:timers/promises";

import { WALLET_ONLY_VALIDITY_MS } from "@al-ft/midgard-fault-proofs/wallet-only-validity";
import type { WorkflowAdapterRunnerInput } from "@al-ft/midgard-fault-proofs/workflow/adapters";
import { FAULT_PROOF_RESTART_WAIT } from "@al-ft/midgard-fault-proofs/workflow/wallet-only-send-gate";
import { afterEach, describe, expect, it } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";

const paths: string[] = [];
afterEach(async () => {
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
  it("holds a gated run pending at every observation, and sends its action once the wait is over", async () => {
    const test = await setupFaultProofExecution();
    const objective = `doubleSpend/${test.input.job.headerHash}`;
    const reason = `${FAULT_PROOF_RESTART_WAIT}: init spends only wallet coins`;
    // What the runner hands back for a wallet-only action held by the wait.
    const gated = {
      kind: "pending",
      reason,
      resumeOnObservation: true,
      namedState: { name: FAULT_PROOF_RESTART_WAIT, alarm: false },
    };
    let proceeded = 0;
    test.runOrResume.mockImplementation(
      async (invocation: WorkflowAdapterRunnerInput) => {
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
          admission: { ...admission, mode: "run" },
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

      // The clock is past the instant, the follower's applied block is not:
      // each observation admits the objective, whose run, left with only the
      // held action, ends held again and keeps its named state in readiness.
      const instantSlot = (constructedAtMs + WALLET_ONLY_VALIDITY_MS) / 1_000;
      now = constructedAtMs + WALLET_ONLY_VALIDITY_MS;
      for (const [index, revision] of ["second", "third", "fourth"].entries()) {
        applied = instantSlot - 3 + index;
        await admitted(revision, index + 2);
        expect(test.runOrResume).toHaveBeenCalledTimes(index + 2);
        expect(outcomes.at(-1)).toEqual(outcomes[0]);
        expect(test.execution.readiness()).toEqual([
          {
            reason: FAULT_PROOF_RESTART_WAIT,
            detail: `${objective}: ${reason}`,
          },
        ]);
      }
      expect(proceeded).toBe(0);

      // The next block is past it too: the next observation sends the action.
      applied = instantSlot;
      await admitted("fifth", 5);
      expect(proceeded).toBe(1);
      expect(test.execution.readiness()).toEqual([]);
      expect(test.runOrResume).toHaveBeenCalledTimes(5);
    } finally {
      await supervisor.close();
    }
  });
});
