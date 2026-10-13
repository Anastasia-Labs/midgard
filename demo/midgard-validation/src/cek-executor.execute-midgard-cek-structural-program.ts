import { type MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core";

import { type MidgardCekConstantValueWitness } from "./cek-builtin.js";
import {
  type Bytes,
  type MidgardCekStructuralExecution,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  type MidgardCekExecutionMode,
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
