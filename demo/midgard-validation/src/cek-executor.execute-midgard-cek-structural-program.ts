import {
  type MidgardCekMachineState,
  type MidgardCekProgramMaterialEntry,
} from "@al-ft/midgard-core";

import { type MidgardCekConstantValueWitness } from "./cek-builtin.js";
import {
  type Bytes,
  type MidgardCekStructuralExecution,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  type MidgardCekExecutionMode,
  MidgardCekObservedStop,
  type MidgardCekStepObserver,
  MidgardCekStepRecorder,
  type MidgardCekStructuralVerdict,
} from "./cek-executor.step-recorder.js";
import { StructuralExecutor } from "./cek-executor.structural-executor.js";

type ExecuteMidgardCekStructuralProgramInput = {
  readonly root: Bytes;
  readonly material: Iterable<MidgardCekProgramMaterialEntry>;
  readonly constantWitnesses: ReadonlyMap<
    string,
    MidgardCekConstantValueWitness
  >;
  readonly executionIndex?: bigint;
  readonly maxSteps: number;
  readonly executionBudget?: {
    readonly cpu: bigint;
    readonly memory: bigint;
  };
  /** Defaults to `proof`. See `MidgardCekExecutionMode`. */
  readonly mode?: MidgardCekExecutionMode;
};

/**
 * Executes every structural CEK rule. In `proof` mode (the default) each
 * generated transition is proved through the same verifier mirrored on L1 and
 * retained; in `verdict` mode only the terminal outcome is produced. Builtin
 * mode remains fail-closed here until semantic success/failure witnesses are
 * generated.
 */
export function executeMidgardCekStructuralProgram(
  input: ExecuteMidgardCekStructuralProgramInput & { readonly mode: "verdict" },
): MidgardCekStructuralVerdict;
export function executeMidgardCekStructuralProgram(
  input: ExecuteMidgardCekStructuralProgramInput & { readonly mode?: "proof" },
): MidgardCekStructuralExecution;
export function executeMidgardCekStructuralProgram(
  input: ExecuteMidgardCekStructuralProgramInput,
): MidgardCekStructuralExecution | MidgardCekStructuralVerdict;
export function executeMidgardCekStructuralProgram(
  input: ExecuteMidgardCekStructuralProgramInput,
): MidgardCekStructuralExecution | MidgardCekStructuralVerdict {
  return new StructuralExecutor(
    input.root,
    input.material,
    input.executionIndex ?? 0n,
    input.constantWitnesses,
    new MidgardCekStepRecorder(input.mode ?? "proof", input.maxSteps),
  ).run(input.executionBudget);
}

/** A structural CEK run that streams its transitions to an observer. */
export type MidgardCekObservedRun = {
  /** The state the run starts from, before any transition. */
  readonly initialState: MidgardCekMachineState;
  /**
   * Runs the program once, reporting every transition to `observer` in
   * order. Returns the verdict-mode outcome, or `null` when the observer
   * stopped the run.
   */
  readonly run: (
    observer: MidgardCekStepObserver,
  ) => MidgardCekStructuralVerdict | null;
};

/**
 * Prepares a streaming commit-mode run: the machine, its step bound and its
 * budget are those of `verdict` mode, and no transition is verified or
 * retained. The observer sees each transition's pre- and post-state as it
 * is taken, so a caller can fold the run into a trace commitment without
 * holding it.
 */
export const startMidgardCekObservedRun = (
  input: Omit<ExecuteMidgardCekStructuralProgramInput, "mode">,
): MidgardCekObservedRun => {
  let observer: MidgardCekStepObserver | undefined;
  let started = false;
  const executor = new StructuralExecutor(
    input.root,
    input.material,
    input.executionIndex ?? 0n,
    input.constantWitnesses,
    new MidgardCekStepRecorder("verdict", input.maxSteps, (pre, post) =>
      observer!(pre, post),
    ),
  );
  return {
    initialState: executor.machineState,
    run: (observe) => {
      if (started) throw new Error("a CEK observed run runs once");
      started = true;
      observer = observe;
      try {
        return executor.run(
          input.executionBudget,
        ) as MidgardCekStructuralVerdict;
      } catch (cause) {
        if (cause instanceof MidgardCekObservedStop) return null;
        throw cause;
      }
    },
  };
};
