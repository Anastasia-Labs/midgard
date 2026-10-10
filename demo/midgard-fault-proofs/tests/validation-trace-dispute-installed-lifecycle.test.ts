import { join } from "node:path";

import { WinningValidationResolutionDatum } from "@al-ft/midgard-sdk";
import { Data, toUnit } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/validation-dispute/workflow-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/validation-dispute/workflow-binding.js")
  >()),
  bindValidationTraceDisputeWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});
import { createManifestBoundValidationTraceDisputeWorkflow } from "../src/validation-dispute/workflow-v1.js";
import { type ValidationTraceChallenge } from "../src/workflow/challenge-authority.js";
import { DirectoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import { stageInstalledValidationTraceDisputeJourney as stageJourney } from "./support/installed-validation-trace-dispute-journey.js";
const withLifecycleDeadline = async <T>(
  operation: () => Promise<T>,
  progress: () => string,
  timeoutMs = 30_000,
): Promise<T> => {
  let timeout: ReturnType<typeof setTimeout> | undefined;
  try {
    return await Promise.race([
      Promise.resolve().then(operation),
      new Promise<never>((_resolve, reject) => {
        timeout = setTimeout(
          () =>
            reject(new Error(`Timed out after ${timeoutMs}ms; ${progress()}`)),
          timeoutMs,
        );
      }),
    ]);
  } finally {
    clearTimeout(timeout);
  }
};

describe("installed lifecycle stage deadline", () => {
  it("reports the last completed stage when an operation stops responding", async () => {
    await expect(
      withLifecycleDeadline(
        () => new Promise<never>(() => {}),
        () => "last completed stage: open; waiting for: verify_source",
        5,
      ),
    ).rejects.toThrow(
      "Timed out after 5ms; last completed stage: open; waiting for: verify_source",
    );
  });
});

const stageInstalledValidationTraceDisputeJourney = async (
  terminalCounterMismatch = false,
) => await stageJourney(hooks, terminalCounterMismatch);

type Journey = Awaited<
  ReturnType<typeof stageInstalledValidationTraceDisputeJourney>
>;

const runToCompletion = async (
  journey: Journey,
  onAwaitCounterparty: (deadline: number) => Promise<void>,
) => {
  let restarts = 0;
  let { result } = await journey.runCold();
  if (result.kind === "stalled") throw new Error(result.reason);
  for (let hop = 0; hop < 200 && result.kind !== "completed"; hop++) {
    restarts++;
    await journey.advance(result);
    if (result.kind === "awaiting_counterparty") {
      await onAwaitCounterparty(result.responseDeadline);
      journey.emulator.awaitBlock();
    }
    ({ result } = await journey.runCold());
    if (result.kind === "stalled") throw new Error(result.reason);
  }
  return { result, restarts };
};

const expectRemoved = async (journey: Journey) => {
  const workflow = await createManifestBoundValidationTraceDisputeWorkflow(
    journey.config,
  );
  expect((await workflow.deriveStage(Date.now())).kind).toBe("removed");
  // The fraud-proof token is permanent by design; the removed state-queue
  // node NFT burns while the proof token stays at the proof address.
  expect(
    await journey.config.lucid.utxosAtWithUnit(
      journey.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
      toUnit(
        journey.resolvedContracts.contracts.fraudProof.policyId,
        `00000006${journey.setup.headerHash}`,
      ),
    ),
  ).toHaveLength(1);
};

describe("validation trace dispute installed production workflow", () => {
  it("awards an inconsistent committed terminal counter and removes the block directly after source verification", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney(true);
    const expectedStages = ["init", "open", "verify_source", "award", "remove"];
    let lastCompletedStage = "fixture setup";
    let waitingFor = "init";
    const progress = () =>
      `last completed stage: ${lastCompletedStage}; waiting for: ${waitingFor}`;
    const journal = new DirectoryFraudProofWorkflowJournalStore(
      join(journey.directory, "journal"),
    );
    const runObserved = () =>
      withLifecycleDeadline(async () => {
        const run = await journey.runCold();
        if (run.result.kind === "stalled") throw new Error(run.result.reason);
        const entries = await journal.load(run.result.workflowId);
        const intents = entries.flatMap(({ event }) =>
          event.kind === "submission_intent" ? [event] : [],
        );
        const submitted = entries.flatMap(({ event }) =>
          event.kind === "submitted" ? [event] : [],
        );
        const confirmed = entries.flatMap(({ event }) =>
          event.kind === "confirmed" ? [event] : [],
        );
        expect(intents.map(({ actionInput }) => actionInput.stage)).toEqual(
          expectedStages.slice(0, intents.length),
        );
        expect(submitted.map(({ txHash }) => txHash)).toEqual(
          intents.map(({ txHash }) => txHash),
        );
        for (const event of submitted) {
          // The recorder adds a row only after the real emulator accepts submitTx.
          expect(
            journey.recorder.rows.filter(
              ({ txHash }) => txHash === event.txHash,
            ),
          ).toHaveLength(1);
          expect(journey.recorder.signedCbors.has(event.txHash)).toBe(true);
        }
        expect(
          confirmed.map(({ actionId, txHash }) => ({ actionId, txHash })),
        ).toEqual(
          intents
            .slice(0, confirmed.length)
            .map(({ actionId, txHash }) => ({ actionId, txHash })),
        );
        lastCompletedStage =
          expectedStages[confirmed.length - 1] ?? "fixture setup";
        waitingFor =
          expectedStages[confirmed.length] ?? "removed contract state";
        if (run.result.kind === "completed") {
          expect(confirmed).toHaveLength(expectedStages.length);
        }
        return run;
      }, progress);
    try {
      let observedAward = false;
      let { result } = await runObserved();
      // Included removal remains reversible through the recovery horizon.
      for (let hop = 0; hop < 60 && result.kind !== "completed"; hop++) {
        expect(result.kind).not.toBe("awaiting_counterparty");
        await journey.advance(result);
        const awardUtxos = await journey.config.lucid.utxosAtWithUnit(
          journey.resolvedContracts.contracts.validationTraceDispute.award
            .spendingScriptAddress,
          journey.threadUnit,
        );
        if (awardUtxos.length > 0) {
          expect(awardUtxos).toHaveLength(1);
          const awardDatum = awardUtxos[0]!.datum;
          expect(awardDatum).toBeDefined();
          expect(
            Data.from(awardDatum!, WinningValidationResolutionDatum).data,
          ).toEqual({ version: 1n });
          expect(
            await journey.config.lucid.utxosAtWithUnit(
              journey.resolvedContracts.contracts.validationTraceDispute.source
                .spendingScriptAddress,
              journey.threadUnit,
            ),
          ).toHaveLength(0);
          observedAward = true;
        }
        ({ result } = await runObserved());
      }
      expect(result.kind, progress()).toBe("completed");
      expect(observedAward).toBe(true);
      await withLifecycleDeadline(async () => {
        await expectRemoved(journey);
        expect(
          await journey.config.lucid.utxosAtWithUnit(
            journey.contracts.stateQueue.spendingScriptAddress,
            journey.setup.stateQueueBlockUnit,
          ),
        ).toHaveLength(0);
      }, progress);
      lastCompletedStage = "removed contract state";
    } catch (cause) {
      throw Object.assign(
        new Error(`Direct proof lifecycle failed; ${progress()}`),
        { cause },
      );
    } finally {
      await journey.cleanup();
    }
  }, 180_000);

  it("plays the full honest game to award and removal, refusing forged and caller-authored material at the exact checks", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney();
    try {
      // Refusal polarity 1 (construction): a caller-authored challenge that
      // is byte-identical to the admitted one but not the admitted object is
      // refused by the admission registry before any binding work happens.
      await expect(
        createManifestBoundValidationTraceDisputeWorkflow({
          ...journey.config,
          challenge: {
            ...journey.challenge,
          } as unknown as ValidationTraceChallenge,
        }),
      ).rejects.toThrow(
        "production validation-trace challenge is not admitted",
      );
      let refusalExercised = false;
      const { result, restarts } = await runToCompletion(journey, async () => {
        if (!refusalExercised) {
          refusalExercised = true;
          // Refusal polarity 2 (game move): a forged operator midpoint —
          // the honest committed proof with a tampered state hash — is
          // refused at the exact midpoint-verification conjunct the game
          // validator enforces, before any transaction is built.
          const honest = await journey.honestOperatorMove();
          const tampered = Buffer.from(honest.stateHash);
          tampered[0] = tampered[0]! ^ 0xff;
          await expect(
            journey.operatorResponds({ ...honest, stateHash: tampered }),
          ).rejects.toThrow("Invalid operator midpoint proof");
          // Positive polarity of the same check: the honest reveal at the
          // same position is accepted.
        }
        await journey.operatorResponds();
      });
      expect(result.kind, "reason" in result ? result.reason : undefined).toBe(
        "completed",
      );
      expect(refusalExercised).toBe(true);
      expect(restarts).toBeGreaterThan(2);
      await expectRemoved(journey);
    } finally {
      await journey.cleanup();
    }
  }, 600_000);

  it("claims the timeout and removes the block when the operator stalls past its deadline", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney();
    try {
      let stalls = 0;
      const { result, restarts } = await runToCompletion(
        journey,
        async (responseDeadline) => {
          // The operator never responds: let its response clock lapse so
          // the re-derived cursor arms the timeout claim.
          stalls++;
          journey.emulator.awaitSlot(
            Math.max(
              1,
              Math.ceil((responseDeadline - journey.emulator.now()) / 1000) + 5,
            ),
          );
        },
      );
      expect(result.kind, "reason" in result ? result.reason : undefined).toBe(
        "completed",
      );
      expect(stalls).toBe(1);
      // The stall line mints the proof through the timeout chain, never
      // through the one-step resolution or the award stage.
      const journal = new DirectoryFraudProofWorkflowJournalStore(
        join(journey.directory, "journal"),
      );
      const stages = (await journal.load(result.workflowId))
        .map(({ event }) =>
          event.kind === "submission_intent"
            ? (event.actionInput as { stage?: string } | undefined)?.stage
            : undefined,
        )
        .filter((stage): stage is string => stage !== undefined);
      expect(stages).toContain("enter_timeout");
      expect(stages).toContain("timeout");
      expect(stages).toContain("remove");
      expect(stages).not.toContain("award");
      expect(restarts).toBeGreaterThan(2);
      await expectRemoved(journey);
    } finally {
      await journey.cleanup();
    }
  }, 600_000);

  it("resumes cold from re-derived chain state after an interruption at the counterparty turn", async () => {
    const journey = await stageInstalledValidationTraceDisputeJourney();
    try {
      // Play through the production runner until the dispute is first
      // waiting on the operator, then "kill" the runner: nothing below
      // reuses any workflow object, plan, or in-memory cursor from before
      // this point.
      let { result } = await journey.runCold();
      if (result.kind === "stalled") throw new Error(result.reason);
      for (
        let hop = 0;
        hop < 50 && result.kind !== "awaiting_counterparty";
        hop++
      ) {
        journey.emulator.awaitBlock();
        ({ result } = await journey.runCold());
        if (result.kind === "stalled") throw new Error(result.reason);
      }
      if (result.kind !== "awaiting_counterparty") {
        throw new Error("dispute never reached the counterparty turn");
      }
      const interruptedDeadline = result.responseDeadline;
      // Cold restart while it is still not the watcher's turn: the fresh
      // runner re-derives the same waiting position from chain state only.
      journey.emulator.awaitBlock();
      const resumed = await journey.runCold();
      expect(resumed.result).toMatchObject({
        kind: "awaiting_counterparty",
        responseDeadline: interruptedDeadline,
      });
      // The operator responds; the next cold runner acts on its turn.
      await journey.operatorResponds();
      journey.emulator.awaitBlock();
      const acting = await journey.runCold();
      expect(acting.result.kind).toBe("pending");
      // Land that move in a block before the next cold restart. The runner
      // itself is already idempotent across an unconfirmed submission — it
      // replays the journal's outstanding `submission_intent` and returns
      // `pending` rather than re-planning — but the emulator reports a
      // mempool transaction as confirmed while its outputs are not yet
      // queryable, so an un-awaited restart would re-derive a cursor from a
      // chain that does not yet show the move.
      journey.emulator.awaitBlock();
      // And the resumed runner completes the journey end to end.
      const { result: finalResult } = await runToCompletion(
        journey,
        async () => {
          await journey.operatorResponds();
        },
      );
      expect(
        finalResult.kind,
        "reason" in finalResult ? finalResult.reason : undefined,
      ).toBe("completed");
      await expectRemoved(journey);
    } finally {
      await journey.cleanup();
    }
  }, 600_000);
});
