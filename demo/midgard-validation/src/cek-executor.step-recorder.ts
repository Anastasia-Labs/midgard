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
 *   same as in `proof` mode for resolved builtins. PV 11 additions can execute
 *   in `verdict` mode while `proof` mode refuses their unresolved steps.
 */
export type MidgardCekExecutionMode = "proof" | "verdict";

/** A `verdict`-mode run: the terminal outcome without per-step evidence. */
export type MidgardCekStructuralVerdict = Omit<
  MidgardCekStructuralExecution,
  "steps"
> & {
  readonly stepCount: number;
};

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
  ) {
    if (!Number.isSafeInteger(maxSteps) || maxSteps <= 0) {
      throw new Error("CEK maxSteps must be a positive safe integer");
    }
  }

  /** Fails closed before a transition that would exceed the step bound. */
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
      if (!verifyMidgardCekCoreStep(pre, post, witness)) {
        throw new Error(
          `generated CEK ${witness.kind} transition does not pass the consensus verifier`,
        );
      }
      this.steps.push(Object.freeze({ pre, post, witness }));
    }
    this.count += 1;
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
