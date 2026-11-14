import {
  hashMidgardCekEnvironmentNode,
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import {
  errorSuccessor,
  exactState,
  midgardCekBuiltinArgumentCount,
  nonNegativeUint32,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type Bytes,
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";

export const verifyLookup = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  if (witness.kind === "lookupEmptyEnvironment") {
    return (
      sameBytes(pre.focusRoot, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT) &&
      sameBytes(pre.environmentRoot, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT) &&
      sameState(post, errorSuccessor(pre, MidgardCekErrorCodes.UnboundVariable))
    );
  }
  if (witness.kind !== "lookupEnvironment") return false;
  if (
    witness.length <= 0n ||
    !nonNegativeUint32(witness.length) ||
    (witness.length === 1n) !==
      sameBytes(witness.tail, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT)
  ) {
    return false;
  }
  const root = hashMidgardCekEnvironmentNode({
    value: witness.value,
    tail: witness.tail,
    length: witness.length,
  });
  if (
    !sameBytes(pre.focusRoot, root) ||
    !sameBytes(pre.environmentRoot, root)
  ) {
    return false;
  }
  return sameState(
    post,
    pre.auxiliary === 0n
      ? exactState(pre, {
          mode: "return",
          focusRoot: witness.value,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        })
      : exactState(pre, {
          mode: "lookup",
          focusRoot: witness.tail,
          environmentRoot: witness.tail,
          continuationRoot: pre.continuationRoot,
          auxiliary: pre.auxiliary - 1n,
        }),
  );
};

export const applyBuiltinResult = (
  pre: MidgardCekMachineState,
  argument: Bytes,
  input: {
    readonly tag: bigint;
    readonly forcesRemaining: bigint;
    readonly argumentsCount: bigint;
    readonly argumentsRoot: Bytes;
    readonly tail: Bytes;
  },
): MidgardCekMachineState | null => {
  const requiredArguments = midgardCekBuiltinArgumentCount(input.tag);
  if (
    input.forcesRemaining !== 0n ||
    input.argumentsCount < 0n ||
    input.argumentsCount >= requiredArguments
  ) {
    return null;
  }
  const nextCount = input.argumentsCount + 1n;
  const nextRoot = hashMidgardCekSequenceNode({
    head: argument,
    tail: input.argumentsRoot,
    length: nextCount,
  });
  return exactState(pre, {
    mode: nextCount === requiredArguments ? "builtin" : "return",
    focusRoot: hashMidgardCekValueNode({
      kind: "builtin",
      tag: input.tag,
      forcesRemaining: input.forcesRemaining,
      argumentsCount: nextCount,
      argumentsRoot: nextRoot,
    }),
    environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
    continuationRoot: input.tail,
    auxiliary: 0n,
  });
};
