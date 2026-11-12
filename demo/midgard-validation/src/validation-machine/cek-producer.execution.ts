/**
 * One execution's CEK core steps (`cek_v1`), from the seeded context state
 * to its halt.
 */
import {
  hashMidgardCekMachineState,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import { type MidgardCekContextControl } from "../cek-context.js";
import {
  type MidgardCekExecutionGraph,
  type MidgardCekExecutionStep,
  type MidgardCekObservedRun,
  type MidgardCekStructuralExecution,
} from "../cek-executor.js";
import {
  type CekContextRun,
  type CekWitnessAt,
  cekWitnessCbor,
} from "./cek-producer.context-step.js";
import { sameSummary } from "./trace-descriptor-summary.js";
import {
  type PhaseOutcome,
  phaseStops,
  type TraceWalk,
  ValidationTraceDisagreement,
} from "./trace-walk.js";

/** Runs the core steps from the seeded `contextControl`, stopping on a
 * failure transition; a halt must agree with local validation. */
export const runCekExecution = (input: {
  readonly context: CekContextRun["parts"]["context"];
  readonly at: CekWitnessAt;
  readonly budget: TraceWalk["budget"];
  readonly contextControl: MidgardCekContextControl;
  readonly graph: MidgardCekExecutionGraph;
  /** The evaluator's captured run on a proof build; on a commit build, a
   * re-execution that streams its steps. */
  readonly exact: MidgardCekStructuralExecution | null;
  readonly observed: MidgardCekObservedRun | null;
  readonly initialState: MidgardCekMachineState;
  readonly exUnits: { readonly steps: bigint; readonly memory: bigint };
  readonly accepted: boolean;
  readonly push: TraceWalk["push"];
}): PhaseOutcome | null => {
  const { at, budget, contextControl, graph, exUnits, push, initialState } =
    input;
  const { context } = input;
  budget.cpu = at.completedCpu + initialState.cpu; // spends startup cost
  budget.memory = at.completedMemory + initialState.memory;
  const { witness } = graph.contextWitness;
  if (
    !Buffer.from(initialState.focusRoot).equals(Buffer.from(graph.root)) ||
    initialState.executionIndex !== BigInt(at.executionCursor) ||
    !sameSummary(witness.payload, context) ||
    witness.memory !== context.memory
  ) {
    throw new Error(
      "CEK execution does not begin at its authenticated context state",
    );
  }

  let stop = null as PhaseOutcome | null;
  const coreStep = (step: MidgardCekExecutionStep): boolean => {
    const { pre, post } = step;
    push(
      "cek",
      cekWitnessCbor(at, contextControl, {
        activeStateHash: hashMidgardCekMachineState(pre),
        exUnits,
      }),
      { kind: "cekCoreStep", step },
    );
    budget.cpu = at.completedCpu + post.cpu;
    budget.memory = at.completedMemory + post.memory;
    if (
      post.cpu > exUnits.steps ||
      post.memory > exUnits.memory ||
      post.mode === "haltError"
    ) {
      stop = phaseStops(
        {
          PlutusExecutionFailed: {
            purpose_kind: BigInt(contextControl.purposeKind),
            purpose_index: BigInt(contextControl.purposeIndex),
          },
        },
        "CEK failure transition",
      );
      return false;
    }
    return true;
  };
  let terminalState: MidgardCekMachineState | null;
  if (input.exact !== null) {
    for (const step of input.exact.steps) {
      if (!coreStep(step)) break;
    }
    terminalState = input.exact.terminalState;
  } else {
    terminalState = input.observed!.run(coreStep)?.terminalState ?? null;
  }
  if (stop !== null) return stop;
  if (terminalState?.mode !== "haltSuccess" || !input.accepted) {
    throw new ValidationTraceDisagreement(
      "CEK successful trace disagrees with local validation",
    );
  }
  return null;
};
