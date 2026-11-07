import { type MidgardCekMachineState } from "@al-ft/midgard-core";

import {
  type MidgardCekExecutionStep,
  type MidgardCekStructuralExecution,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  type MidgardCekCoreStepWitness,
  verifyMidgardCekCoreStep,
} from "./cek-machine.js";

/**
 * How a structural CEK run treats its transitions.
 *
 * - `proof` checks every generated transition with the consensus core-step
 *   verifier and retains it, so the run can be replayed as a validation
 *   trace.
 * - `verdict` does neither. The machine, its step bound and its budget are
 *   unchanged, so the terminal state, stop reason and step count are the
 *   same as in `proof` mode; only the per-step evidence is absent.
 */
export type MidgardCekExecutionMode = "proof" | "verdict";

/**
 * Observes each transition of a run that keeps no per-step evidence (the
 * streaming commit mode): the step a `proof` run would verify and keep, not
 * verified here. Returning `false` stops the run after this transition; the
 * run then ends with the observed post-state.
 */
export type MidgardCekStepObserver = (step: MidgardCekExecutionStep) => boolean;

/** A `verdict`-mode run: the terminal outcome without per-step evidence. */
export type MidgardCekStructuralVerdict = Omit<
  MidgardCekStructuralExecution,
  "steps"
> & {
  readonly stepCount: number;
};

/** Unwinds a run whose observer asked it to stop. */
export class MidgardCekObservedStop extends Error {
  constructor(
    readonly state: MidgardCekMachineState,
    readonly stepCount: number,
  ) {
    super("CEK run stopped by its step observer");
    this.name = "MidgardCekObservedStop";
  }
}

/**
 * Owns the step bound and the per-step evidence policy of one structural
 * CEK run. The executor reports every transition here; this decides whether
 * it is verified and kept.
 */
export class MidgardCekStepRecorder {
  private readonly steps: MidgardCekExecutionStep[] = [];
  private count = 0;

  constructor(
    private readonly mode: MidgardCekExecutionMode,
    private readonly maxSteps: number,
    private readonly observer?: MidgardCekStepObserver,
  ) {
    if (observer !== undefined && mode !== "verdict") {
      throw new Error("a CEK step observer runs only without retained steps");
    }
    if (!Number.isSafeInteger(maxSteps) || maxSteps <= 0) {
      throw new Error("CEK maxSteps must be a positive safe integer");
    }
  }

  /**
   * Fails closed before a transition that would exceed the step bound. The
   * bound is the evaluator's own (the budget stops every honest run long
   * before it), so a caller reports it as no verdict, never a rejection.
   */
  public assertWithinBound(): void {
    if (this.count >= this.maxSteps) {
      throw new Error(
        `CEK execution exceeded the exact ${this.maxSteps.toString(10)}-step bound`,
      );
    }
  }

  public record(
    pre: MidgardCekMachineState,
    post: MidgardCekMachineState,
    witness: MidgardCekCoreStepWitness,
  ): void {
    if (this.mode === "proof") {
      // A generated transition the verifier refuses is the generator's
      // fault, so the run has no verdict.
      if (!verifyMidgardCekCoreStep(pre, post, witness)) {
        throw new Error(
          `generated CEK ${witness.kind} transition does not pass the consensus verifier`,
        );
      }
      this.steps.push(Object.freeze({ pre, post, witness }));
    }
    this.count += 1;
    if (
      this.observer !== undefined &&
      !this.observer(Object.freeze({ pre, post, witness }))
    ) {
      throw new MidgardCekObservedStop(post, this.count);
    }
  }

  public finish(
    initialState: MidgardCekMachineState,
    terminalState: MidgardCekMachineState,
    stopReason: MidgardCekStructuralExecution["stopReason"],
  ): MidgardCekStructuralExecution | MidgardCekStructuralVerdict {
    return this.mode === "proof"
      ? Object.freeze({
          initialState,
          steps: Object.freeze([...this.steps]),
          terminalState,
          stopReason,
        })
      : Object.freeze({
          initialState,
          stepCount: this.count,
          terminalState,
          stopReason,
        });
  }
}
