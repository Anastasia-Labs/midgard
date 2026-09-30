import { type MidgardCekProgramMaterialEntry } from "@al-ft/midgard-core";

import { type MidgardCekConstantValueWitness } from "./cek-builtin.js";
import {
  type Bytes,
  type MidgardCekStructuralExecution,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import { StructuralExecutor } from "./cek-executor.structural-executor.js";

/**
 * Executes every structural CEK rule and proves each generated transition
 * through the same verifier mirrored on L1. Builtin mode remains fail-closed
 * here until semantic success/failure witnesses are generated.
 */
export const executeMidgardCekStructuralProgram = (input: {
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
}): MidgardCekStructuralExecution =>
  new StructuralExecutor(
    input.root,
    input.material,
    input.executionIndex ?? 0n,
    input.constantWitnesses,
  ).run(input.maxSteps, input.executionBudget);
