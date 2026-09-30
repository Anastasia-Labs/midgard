import {
  asArray,
  asBigInt,
  decodeSingleCbor,
  encodeCbor,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import {
  hashMidgardCekMachineState,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
} from "@al-ft/midgard-core/cek-proof";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";
import {
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
} from "@al-ft/midgard-validation/cek-builtin";

type Trace = DeterministicValidationMachineTrace;

/**
 * Replace the honest map-conversion start at `lowIndex` with the successor
 * the direct builtin arm would give the same pre-state: the converted value
 * returned at once, charged the direct budget. `states` and `witnesses` are
 * the claim being forged; `honest` is the trace they were copied from.
 */
export const forgeDirectMapConversionSuccessor = (
  honest: Trace,
  lowIndex: number,
  states: Trace["states"][number][],
  witnesses: Trace["witnesses"][number][],
): void => {
  const auxiliary = honest.witnesses[lowIndex]!.auxiliary;
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    auxiliary.step.witness.kind !== "startBuiltinMapConversion"
  )
    throw new Error("disputed step is not a map conversion start");
  const { pre, post: startPost, witness } = auxiliary.step;
  const budget = midgardCekDirectBuiltinBudget(witness.tag, witness.arguments);
  const directPost = {
    ...pre,
    mode: "return" as const,
    focusRoot: Buffer.from(hashMidgardCekDirectValueWitness(witness.result)),
    environmentRoot: Buffer.from(MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT),
    auxiliary: 0n,
    cpu: pre.cpu + budget.cpu,
    memory: pre.memory + budget.memory,
  };
  witnesses[lowIndex] = {
    ...honest.witnesses[lowIndex]!,
    auxiliary: {
      kind: "cekCoreStep",
      step: {
        pre,
        post: directPost,
        witness: {
          kind: "executeBuiltinDirect",
          tag: witness.tag,
          arguments: witness.arguments,
          result: witness.result,
        },
      },
    },
  };
  const successor = honest.witnesses[lowIndex + 1]!;
  const work = asArray(decodeSingleCbor(successor.cbor), "CEK successor");
  if (!encodeCbor(work).equals(successor.cbor))
    throw new Error("CEK successor work witness does not re-encode exactly");
  const honestState = honest.states[lowIndex + 1]!;
  const completedCpu = asBigInt(work[3], "completed cpu");
  const completedMemory = asBigInt(work[4], "completed memory");
  if (
    honestState.executionCpu !== completedCpu + startPost.cpu ||
    honestState.executionMemory !== completedMemory + startPost.memory
  )
    throw new Error("CEK successor budget is not the start post budget");
  work[5] = hashMidgardCekMachineState(directPost);
  const cbor = encodeCbor(work);
  witnesses[lowIndex + 1] = { ...successor, cbor };
  states[lowIndex + 1] = {
    ...honestState,
    workRoot: hashMidgardValidationWorkWitness({
      phase: successor.phase,
      programCounter: successor.programCounter,
      witnessCbor: cbor,
    }),
    executionCpu: completedCpu + directPost.cpu,
    executionMemory: completedMemory + directPost.memory,
  };
};
