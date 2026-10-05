import {
  asArray,
  asBigInt,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
  hashMidgardValidationWorkWitness,
} from "@al-ft/midgard-core";
import { type DeterministicValidationMachineTrace } from "@al-ft/midgard-validation";

import { forgeDirectMapConversionSuccessor } from "./cek-direct-map-conversion-forgery.js";

type Trace = DeterministicValidationMachineTrace;

/** Keep genuine failure material and change only the charged failed successor. */
export const forgeBuiltinFailureBudgetSuccessor = (
  honest: Trace,
  lowIndex: number,
  states: Trace["states"][number][],
  witnesses: Trace["witnesses"][number][],
  field: "cpu" | "memory",
): void => {
  const auxiliary = honest.witnesses[lowIndex]!.auxiliary;
  if (
    auxiliary?.kind !== "cekCoreStep" ||
    auxiliary.step.witness.kind !== "executeBuiltinFailure"
  ) {
    throw new Error("disputed step is not a builtin failure");
  }
  const { post } = auxiliary.step;
  const forgedPost = { ...post, [field]: post[field] + 1n };
  witnesses[lowIndex] = {
    ...honest.witnesses[lowIndex]!,
    auxiliary: { ...auxiliary, step: { ...auxiliary.step, post: forgedPost } },
  };
  const successor = honest.witnesses[lowIndex + 1]!;
  if (successor.phase !== "terminal")
    throw new Error("failed CEK execution must lead to the terminal witness");
  const work = asArray(
    decodeSingleCbor(honest.witnesses[lowIndex]!.cbor),
    "CEK failure pre-state",
  );
  const completedCpu = asBigInt(work[3], "completed cpu");
  const completedMemory = asBigInt(work[4], "completed memory");
  const honestState = honest.states[lowIndex + 1]!;
  if (
    honestState.executionCpu !== completedCpu + post.cpu ||
    honestState.executionMemory !== completedMemory + post.memory
  ) {
    throw new Error(
      "CEK failure successor budget is not the failed post budget",
    );
  }
  states[lowIndex + 1] = {
    ...honestState,
    executionCpu: completedCpu + forgedPost.cpu,
    executionMemory: completedMemory + forgedPost.memory,
  };
};

/** The CEK successor mutations shared by direct, context and failure cases. */
export const forgeCekCoreSuccessor = ({
  challengerTrace,
  disputedLowIndex,
  operatorStates,
  operatorWitnesses,
  dishonestChallenger,
  cekBuiltinFailureBudgetForgery,
  cekDirectMapConversion,
  cekContextStage,
}: {
  readonly challengerTrace: Trace;
  readonly disputedLowIndex: number;
  readonly operatorStates: Trace["states"][number][];
  readonly operatorWitnesses: Trace["witnesses"][number][];
  readonly dishonestChallenger: boolean;
  readonly cekBuiltinFailureBudgetForgery?: "cpu" | "memory";
  readonly cekDirectMapConversion: boolean;
  readonly cekContextStage?: number;
}): void => {
  if (cekBuiltinFailureBudgetForgery !== undefined) {
    if (!dishonestChallenger)
      throw new Error("failure budget forgery requires a dishonest challenger");
    forgeBuiltinFailureBudgetSuccessor(
      challengerTrace,
      disputedLowIndex,
      operatorStates,
      operatorWitnesses,
      cekBuiltinFailureBudgetForgery,
    );
  }
  if (dishonestChallenger && cekDirectMapConversion)
    forgeDirectMapConversionSuccessor(
      challengerTrace,
      disputedLowIndex,
      operatorStates,
      operatorWitnesses,
    );
  if (dishonestChallenger && cekContextStage !== undefined) {
    const successorIndex = disputedLowIndex + 1;
    const adjacent = operatorWitnesses[successorIndex]!;
    const work = asArray(decodeSingleCbor(adjacent.cbor), "context successor");
    const nextContext = asArray(
      decodeSingleCbor(asBytes(work[1], "context control")),
      "context control",
    );
    // Supply a well-encoded dishonest continuation, so refusal reaches the
    // physical context verifier instead of the host's missing-evidence gate.
    nextContext[20] = 1n;
    work[1] = encodeCbor(nextContext);
    const cbor = encodeCbor(work);
    operatorWitnesses[successorIndex] = { ...adjacent, cbor };
    operatorStates[successorIndex] = {
      ...operatorStates[successorIndex]!,
      workRoot: hashMidgardValidationWorkWitness({
        phase: adjacent.phase,
        programCounter: adjacent.programCounter,
        witnessCbor: cbor,
      }),
    };
  }
};
