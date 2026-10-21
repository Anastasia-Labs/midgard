/** Plays defended threads of our case on the Open scene through the runner,
 * up to and through their one-step resolution: each scripted defender
 * reveals from its own trace whenever its thread is on its turn at the game,
 * and the runner runs one block at a time. A thread is found by its token
 * wherever it stands: at the game or timeout validator, or at a one-step
 * holder the deployment names. */
import type { UTxO } from "@lucid-evolution/lucid";

import {
  confirmedValidationTraceStages,
  parseValidationTraceAction,
  validationTraceActionId,
  type ValidationTraceStage,
} from "../../src/validation-trace-dispute/actions.js";
import {
  validationTraceOneStepContracts,
  type ValidationTraceOneStepHolder,
} from "../../src/validation-trace-dispute/one-step-holders.js";
import type { ValidationTraceDisputeDeployment } from "../../src/validation-trace-dispute/transactions.js";
import type { FraudProofWorkflowJournalEntry } from "../../src/workflow/journal.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.js";
import { workflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import type { Defender } from "./validation-trace-dispute-defend.js";
import type { Scene } from "./validation-trace-dispute-open-scene.js";
import type { Runner } from "./validation-trace-dispute-runner.js";
import {
  isTurn,
  liveThread,
  revealAsDefender,
  type ScriptedTrace,
  threadOf,
} from "./validation-trace-dispute-scripted-defender.js";

/** A defended thread: its defender, the trace it reveals from, its token. */
export type Side = Readonly<{
  defender: Defender;
  trace: ScriptedTrace;
  unit: string;
  /** A silent defender never reveals. */
  silent?: boolean;
}>;

export type ThreadPlace = Readonly<{
  holder: "game" | "timeout" | ValidationTraceOneStepHolder;
  utxo: UTxO;
}>;

/** Where the thread holding `unit` stands now, if it is live. */
export const threadPlace = async (
  scene: Scene,
  deployment: ValidationTraceDisputeDeployment,
  unit: string,
): Promise<ThreadPlace | undefined> => {
  const atGame = await liveThread(scene, unit);
  if (atGame !== undefined)
    return {
      holder:
        atGame.address === scene.chain.timeout.spendingScriptAddress
          ? "timeout"
          : "game",
      utxo: atGame,
    };
  for (const { holder, address } of validationTraceOneStepContracts(
    deployment,
  )) {
    const [utxo] = await scene.h.proverLucid.utxosAtWithUnit(address, unit);
    if (utxo !== undefined) return { holder, utxo };
  }
  return undefined;
};

const describeRun = (result: FraudProofWorkflowRunResult) =>
  "reason" in result ? `${result.kind}: ${result.reason}` : result.kind;

/**
 * Runs the runner one block at a time, each defender revealing first
 * whenever its thread is on its turn at the game, until `until` holds after
 * a run; throws with the run history after `limit` runs.
 */
export const playThreads = async ({
  scene,
  runner,
  sides,
  until,
  limit = 400,
}: {
  readonly scene: Scene;
  readonly runner: Runner;
  readonly sides: readonly Side[];
  readonly until: (result: FraudProofWorkflowRunResult) => Promise<boolean>;
  readonly limit?: number;
}): Promise<
  Readonly<{
    result: FraudProofWorkflowRunResult;
    history: readonly string[];
  }>
> => {
  const history: string[] = [];
  for (let run = 0; run < limit; run += 1) {
    for (const side of sides) {
      if (side.silent === true) continue;
      const utxo = await liveThread(scene, side.unit);
      if (
        utxo !== undefined &&
        utxo.address === scene.chain.game.spendingScriptAddress &&
        isTurn(threadOf(utxo), "AwaitingOperator")
      )
        await revealAsDefender(scene, side.defender, utxo, side.trace);
    }
    const result = await runner.runOnce(scene.failing);
    history.push(describeRun(result));
    if (await until(result)) return { result, history };
  }
  throw new Error(`the play did not end:\n${history.join("\n")}`);
};

/** Whether `result` waits for `reasonPrefix` with no transaction in flight. */
export const isPendingFor = (
  result: FraudProofWorkflowRunResult,
  reasonPrefix: string,
): boolean =>
  result.kind === "pending" && result.reason.startsWith(reasonPrefix);

/** One transaction the runner signed, by stage. */
export type PreparedMove = Readonly<{
  stage: ValidationTraceStage;
  consumes: string | null;
  hash: string;
  cbor: string;
}>;

/** Records each transaction the adapter signs into `prepared`. */
export const capturePrepared =
  (prepared: PreparedMove[]) =>
  (
    adapter: FraudProofFamilyWorkflowAdapter,
  ): FraudProofFamilyWorkflowAdapter => ({
    ...adapter,
    preflight: async (context) => {
      const preflight = await adapter.preflight(context);
      const signed = workflowPreflightTransaction(preflight);
      if (signed !== undefined) {
        const { stage, consumes } = parseValidationTraceAction(context.action);
        prepared.push({
          stage,
          consumes,
          hash: signed.toHash(),
          cbor: signed.toCBOR(),
        });
      }
      return preflight;
    },
  });

const ONE_STEP_STAGES: readonly ValidationTraceStage[] = [
  "enter_resolution",
  "resolve",
  "close_awarded",
];

/** The confirmed one-step stages, as [stage, consumed, landed]. */
export const confirmedOneStepMoves = (
  entries: readonly FraudProofWorkflowJournalEntry[],
) =>
  confirmedValidationTraceStages(entries)
    .filter(({ action }) => ONE_STEP_STAGES.includes(action.stage))
    .map(({ action, txHash }) => ({
      stage: action.stage,
      consumes: action.consumes,
      txHash,
    }));

/** The journaled submission intents for the action spending `consumes`. */
export const intentsFor = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  stage: ValidationTraceStage,
  consumes: string,
) =>
  entries.flatMap(({ event }) =>
    event.kind === "submission_intent" &&
    event.actionId === validationTraceActionId({ stage, consumes })
      ? [{ attempt: event.attempt, txHash: event.txHash }]
      : [],
  );
