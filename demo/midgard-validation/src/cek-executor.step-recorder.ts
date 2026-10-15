import { type MidgardCekMachineState } from "@al-ft/midgard-core";

import {
  type MidgardCekExecutionStep,
  type MidgardCekStructuralExecution,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import { type MidgardCekStepScreen } from "./cek-gated-set.js";
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
 * streaming commit mode). Returning `false` stops the run after this
 * transition; the run then ends with the observed post-state.
 */
export type MidgardCekStepObserver = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
) => boolean;

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
 * Unwinds a run that passed its wall-clock deadline. The deadline is a
 * node-local resource limit, never part of a verdict: a caller reports it as
 * its own refusal, distinct from every script outcome.
 */
export class MidgardCekWallCapExceeded extends Error {
  constructor(readonly stepCount: number) {
    super(
      `CEK execution passed its wall-clock deadline after ${stepCount.toString(10)} steps`,
    );
    this.name = "MidgardCekWallCapExceeded";
  }
}

/** Steps between two reads of the clock against a wall-clock deadline. */
const WALL_CLOCK_CHECK_INTERVAL = 1024;

/**
 * Owns the step bound, the optional wall-clock deadline, the optional
 * gated-set screen and the per-step evidence policy of one structural CEK
 * run. The executor reports every transition here; this decides whether it
 * is verified and kept.
 */
export class MidgardCekStepRecorder {
  private readonly steps: MidgardCekExecutionStep[] = [];
  private count = 0;

  constructor(
    private readonly mode: MidgardCekExecutionMode,
    private readonly maxSteps: number,
    private readonly observer?: MidgardCekStepObserver,
    /** `Date.now()` time past which the run stops; none when undefined. */
    private readonly wallDeadlineMs?: number,
    /** Stops the run at a step outside a gated set; none when undefined. */
    private readonly screen?: MidgardCekStepScreen,
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
    this.screen?.(witness);
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
      this.wallDeadlineMs !== undefined &&
      this.count % WALL_CLOCK_CHECK_INTERVAL === 0 &&
      Date.now() > this.wallDeadlineMs
    ) {
      throw new MidgardCekWallCapExceeded(this.count);
    }
    if (this.observer !== undefined && !this.observer(pre, post)) {
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
