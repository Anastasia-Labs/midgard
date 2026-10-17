import {
  hashMidgardCekContinuationFrame,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import { hashMidgardCekDirectValueWitness } from "./cek-builtin.js";
import {
  errorSuccessor,
  exactComputeSuccessor,
  linkedSequenceRootIsWellFormed,
  linkedSequenceTailIsWellFormed,
  midgardCekBuiltinForceCount,
  sameState,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";

export const verifyCompute = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  // This verifier handles compute witnesses; other phases fail closed below.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (witness.kind) {
    case "computeVariable":
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "variable",
            index: witness.index,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "lookup",
            focusRoot: pre.environmentRoot,
            environmentRoot: pre.environmentRoot,
            continuationRoot: pre.continuationRoot,
            auxiliary: witness.index,
          }),
        )
      );
    case "computeConstant":
      try {
        const direct = {
          kind: "constant" as const,
          witness: witness.value,
        };
        const valueRoot = hashMidgardCekDirectValueWitness(direct);
        return (
          sameBytes(
            pre.focusRoot,
            hashMidgardCekTermNode({
              kind: "constant",
              value: valueRoot,
            }),
          ) &&
          sameState(
            post,
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: valueRoot,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
              continuationRoot: pre.continuationRoot,
              auxiliary: 0n,
            }),
          )
        );
      } catch {
        return false;
      }
    case "computeContextConstant":
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "contextConstant",
            value: witness.valueRoot,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: witness.valueRoot,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    case "computeLambda": {
      const value = hashMidgardCekValueNode({
        kind: "lambda",
        body: witness.body,
        environment: pre.environmentRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "lambda",
            body: witness.body,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeDelay": {
      const value = hashMidgardCekValueNode({
        kind: "delay",
        body: witness.body,
        environment: pre.environmentRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "delay",
            body: witness.body,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeApplication": {
      const continuation = hashMidgardCekContinuationFrame({
        kind: "applyArgument",
        argument: witness.argument,
        environment: pre.environmentRoot,
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "application",
            function: witness.function,
            argument: witness.argument,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: witness.function,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeForce": {
      const continuation = hashMidgardCekContinuationFrame({
        kind: "force",
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "force",
            term: witness.term,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: witness.term,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeError":
      return (
        sameBytes(pre.focusRoot, hashMidgardCekTermNode({ kind: "error" })) &&
        sameState(post, errorSuccessor(pre, MidgardCekErrorCodes.Explicit))
      );
    case "computeBuiltin": {
      const value = hashMidgardCekValueNode({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: midgardCekBuiltinForceCount(witness.tag),
        argumentsCount: 0n,
        argumentsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "builtin",
            tag: witness.tag,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeConstrEmpty": {
      const value = hashMidgardCekValueNode({
        kind: "constr",
        tag: witness.tag,
        valuesCount: 0n,
        valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "constr",
            tag: witness.tag,
            termsCount: 0n,
            termsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeConstrNonempty": {
      if (
        !linkedSequenceTailIsWellFormed(
          witness.remainingTermsRoot,
          witness.termsCount,
        )
      ) {
        return false;
      }
      const termsRoot = hashMidgardCekSequenceNode({
        head: witness.firstTerm,
        tail: witness.remainingTermsRoot,
        length: witness.termsCount,
      });
      const continuation = hashMidgardCekContinuationFrame({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: witness.termsCount - 1n,
        remainingTermsRoot: witness.remainingTermsRoot,
        valuesCount: 0n,
        valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
        environment: pre.environmentRoot,
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "constr",
            tag: witness.tag,
            termsCount: witness.termsCount,
            termsRoot,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: witness.firstTerm,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeCase": {
      if (
        !linkedSequenceRootIsWellFormed(
          witness.branchesRoot,
          witness.branchesCount,
        )
      ) {
        return false;
      }
      const continuation = hashMidgardCekContinuationFrame({
        kind: "case",
        branchesCount: witness.branchesCount,
        branchesRoot: witness.branchesRoot,
        environment: pre.environmentRoot,
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNode({
            kind: "case",
            scrutinee: witness.scrutinee,
            branchesCount: witness.branchesCount,
            branchesRoot: witness.branchesRoot,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "compute",
            focusRoot: witness.scrutinee,
            environmentRoot: pre.environmentRoot,
            continuationRoot: continuation,
            auxiliary: 0n,
          }),
        )
      );
    }
    default:
      return false;
  }
};
