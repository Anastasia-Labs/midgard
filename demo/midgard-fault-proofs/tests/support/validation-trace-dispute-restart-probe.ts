/** What the validation-trace dispute's runner does next after a restart at
 * some point of a dispute, against what the process that was not stopped
 * does next. A probe is a fresh process, holding nothing the stopped one
 * held in memory; each of its runs observes and stops there, so it takes
 * nothing. What a run asks for is the id of the action its first
 * observation names, or the named state it waits on. */
import { expect } from "vitest";

import type { FraudProofWorkflowObservation } from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "../../src/workflow/orchestrator.fraud-proof-workflow-run-result.js";
import type { FraudProofFamilyWorkflowAdapter } from "../../src/workflow/orchestrator.js";
import type { Runner } from "./validation-trace-dispute-runner.js";

export type AdapterWrap = (
  adapter: FraudProofFamilyWorkflowAdapter,
) => FraudProofFamilyWorkflowAdapter;

type Block = Parameters<Runner["runOnce"]>[0];

/** A fresh process of the runner, its adapter rewritten by `wrap`. */
export type ProcessOver = (wrap: AdapterWrap) => Pick<Runner, "runOnce">;

const PROBED = "restart probe: nothing is taken";

/** What an observation asks for: its action's id, or the named state it
 * waits on. */
export const askedFor = (observed: FraudProofWorkflowObservation): string =>
  observed.kind === "action_required"
    ? observed.action.actionId
    : observed.kind === "completed"
      ? "completed"
      : `${observed.kind}: ${observed.reason.split(":")[0]!}`;

const described = (result: FraudProofWorkflowRunResult) =>
  "reason" in result
    ? `${result.kind}: ${result.reason.split(":")[0]!}`
    : result.kind;

/** Records what each observation of the adapter asks for into `seen`. */
export const recordingAsks =
  (seen: string[]): AdapterWrap =>
  (adapter) => ({
    ...adapter,
    observe: async (context) => {
      const observed = await adapter.observe(context);
      seen.push(askedFor(observed));
      return observed;
    },
  });

/** What a fresh process asks for first, taking nothing. A run that makes
 * no observation (a fresh process prepares its claim first) is run again,
 * up to `limit` runs. */
export const probeRestart = async (
  processOver: ProcessOver,
  block: Block,
  limit = 3,
): Promise<string> => {
  const seen: string[] = [];
  const runner = processOver((adapter) => ({
    ...adapter,
    observe: async (context) => {
      seen.push(askedFor(await adapter.observe(context)));
      return { kind: "pending", reason: PROBED };
    },
  }));
  const runs: string[] = [];
  for (let run = 0; run < limit; run += 1) {
    const result = await runner.runOnce(block);
    if (seen.length > 0) return seen[0]!;
    runs.push(described(result));
  }
  throw new Error(`the restarted process never observed: ${runs.join("\n")}`);
};

/** What the process that was not stopped asks for first on its next run:
 * one run of `runner`, whose adapter records into `seen`. */
export const nextAsk = async (
  runner: Pick<Runner, "runOnce">,
  seen: string[],
  block: Block,
): Promise<string> => {
  const mark = seen.length;
  const result = await runner.runOnce(block);
  return seen[mark] ?? described(result);
};

/** The stages whose action locks our bond or Inits a thread. */
const INIT_OR_BOND = /^validation-trace-(init|open|verify-source):/;

/**
 * Restarts at this point, then runs the process that was not stopped once:
 * the restart asks first for what that process asks for, and, once our
 * case is open, neither Inits a thread or locks a second bond. Returns what
 * they ask for.
 */
export const expectSameNextAfterRestart = async ({
  processOver,
  runner,
  seen,
  block,
  opened,
}: {
  readonly processOver: ProcessOver;
  readonly runner: Pick<Runner, "runOnce">;
  readonly seen: string[];
  readonly block: Block;
  /** Whether our case is open (or another party's verify opened it). */
  readonly opened: boolean;
}): Promise<string> => {
  const restarted = await probeRestart(processOver, block);
  const uninterrupted = await nextAsk(runner, seen, block);
  expect(restarted).toBe(uninterrupted);
  if (opened) expect(uninterrupted).not.toMatch(INIT_OR_BOND);
  return uninterrupted;
};
