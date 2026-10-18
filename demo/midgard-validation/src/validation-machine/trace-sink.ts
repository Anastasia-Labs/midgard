/**
 * Where the validation-trace builder sends each machine state it emits.
 *
 * The builder replays a transaction phase by phase and pushes one work
 * witness per state. A proof build retains every witness and assembles the
 * full tree at the end; a commit build folds each state into the trace
 * commitment as it is pushed and keeps nothing it does not need. Both read
 * the same pushes, so they commit the same states.
 */
import {
  hashMidgardValidationMachineState,
  hashMidgardValidationWorkWitness,
  MIDGARD_VALIDATION_MACHINE_VERSION,
  MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH,
  type MidgardValidationMachineState,
  type MidgardValidationVerdictName,
} from "@al-ft/midgard-core";

import { type ValidationMachineWorkWitness } from "./types.js";

/** The fields every machine state of one transaction's trace shares. */
export type ValidationTraceStateFrame = {
  readonly eventKeyHash: Buffer;
  readonly transactionId: Buffer;
  readonly transactionCommitment: Buffer;
  readonly validationContextHash: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly priorLedgerRoot: Buffer;
};

/** The executed budget a state records. */
export type ValidationTraceBudget = {
  readonly cpu: bigint;
  readonly memory: bigint;
};

/** The verdict a terminal state records. */
export type ValidationTraceTerminal = {
  readonly verdict: Exclude<MidgardValidationVerdictName, "pending">;
  readonly rejectionCodeHash: Buffer;
};

/**
 * The machine state of one pushed witness. This is the one place the
 * builder lays out a state, for every build mode.
 */
export const validationMachineStateV1 = (
  frame: ValidationTraceStateFrame,
  witness: Pick<
    ValidationMachineWorkWitness,
    "phase" | "programCounter" | "cbor"
  >,
  budget: ValidationTraceBudget,
  terminal: ValidationTraceTerminal | null,
): MidgardValidationMachineState => ({
  machineVersion: MIDGARD_VALIDATION_MACHINE_VERSION,
  eventKeyHash: frame.eventKeyHash,
  transactionId: Buffer.from(frame.transactionId),
  transactionCommitment: frame.transactionCommitment,
  validationContextHash: frame.validationContextHash,
  sourceKind: frame.sourceKind,
  priorLedgerRoot: frame.priorLedgerRoot,
  phase: witness.phase,
  programCounter: witness.programCounter,
  workRoot: hashMidgardValidationWorkWitness({
    phase: witness.phase,
    programCounter: witness.programCounter,
    witnessCbor: witness.cbor,
  }),
  executionCpu: budget.cpu,
  executionMemory: budget.memory,
  verdict: terminal === null ? "pending" : terminal.verdict,
  rejectionCodeHash:
    terminal === null
      ? MIDGARD_VALIDATION_NO_REJECTION_CODE_HASH
      : terminal.rejectionCodeHash,
});

/** Receives the builder's states in program-counter order. */
export interface ValidationTraceSink {
  /** The number of states pushed so far: the next program counter. */
  readonly length: number;
  /**
   * Pushes the next state. `budget` is read at the call; `terminal` is set
   * only on the last push.
   */
  push(
    witness: ValidationMachineWorkWitness,
    budget: ValidationTraceBudget,
    terminal: ValidationTraceTerminal | null,
  ): void;
}

/**
 * How one build records its trace.
 *
 * - `proof` retains every witness with its per-step evidence; Phase B runs
 *   the transaction's scripts in proof mode so each CEK step carries its
 *   verified transition.
 * - `commit` streams: Phase B runs the scripts in verdict mode, and the CEK
 *   phase re-executes each script with a step observer, so no CEK step is
 *   verified or retained.
 */
export type ValidationTraceRecording = {
  readonly mode: "proof" | "commit";
  readonly open: (frame: ValidationTraceStateFrame) => ValidationTraceSink;
};

/** The proof build's sink: every witness and budget, in order. */
export class RetainingValidationTraceSink implements ValidationTraceSink {
  public readonly witnesses: ValidationMachineWorkWitness[] = [];
  public readonly budgets: ValidationTraceBudget[] = [];
  private terminal: ValidationTraceTerminal | null = null;

  constructor(public readonly frame: ValidationTraceStateFrame) {}

  public get length(): number {
    return this.witnesses.length;
  }

  public push(
    witness: ValidationMachineWorkWitness,
    budget: ValidationTraceBudget,
    terminal: ValidationTraceTerminal | null,
  ): void {
    if (this.terminal !== null) {
      throw new Error("validation trace has a state after its terminal");
    }
    this.witnesses.push(witness);
    this.budgets.push({ cpu: budget.cpu, memory: budget.memory });
    this.terminal = terminal;
  }

  /** Every pushed state, the last one terminal. */
  public states(): MidgardValidationMachineState[] {
    const last = this.witnesses.length - 1;
    return this.witnesses.map((witness, index) =>
      validationMachineStateV1(
        this.frame,
        witness,
        this.budgets[index]!,
        index === last ? this.terminal : null,
      ),
    );
  }
}

/** Observes each state a commit build folds, with its hash. */
export type ValidationTraceStateObserver = (
  programCounter: number,
  stateHash: Buffer,
  state: () => MidgardValidationMachineState,
) => void;

/** One state hash per push, handed to `observe` and then dropped. */
export class HashingValidationTraceSink implements ValidationTraceSink {
  private count = 0;
  private terminated = false;

  constructor(
    private readonly frame: ValidationTraceStateFrame,
    private readonly observe: ValidationTraceStateObserver,
  ) {}

  public get length(): number {
    return this.count;
  }

  public get terminal(): boolean {
    return this.terminated;
  }

  public push(
    witness: ValidationMachineWorkWitness,
    budget: ValidationTraceBudget,
    terminal: ValidationTraceTerminal | null,
  ): void {
    if (this.terminated) {
      throw new Error("validation trace has a state after its terminal");
    }
    if (witness.programCounter !== this.count) {
      throw new Error("validation trace state is out of order");
    }
    const state = validationMachineStateV1(
      this.frame,
      witness,
      budget,
      terminal,
    );
    this.observe(
      this.count,
      hashMidgardValidationMachineState(state),
      () => state,
    );
    this.count += 1;
    this.terminated = terminal !== null;
  }
}
