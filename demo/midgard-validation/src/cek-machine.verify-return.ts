import {
  hashMidgardCekContinuationFrame,
  hashMidgardCekEnvironmentNode,
  hashMidgardCekSequenceNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  type MidgardCekMachineState,
} from "@al-ft/midgard-core";

import {
  environmentSummaryLength,
  environmentSummaryMatches,
  errorSuccessor,
  exactState,
  isConstant,
  isDelayOrForceableBuiltin,
  isLambdaOrBuiltin,
  linkedSequenceRootIsWellFormed,
  linkedSequenceTailIsWellFormed,
  midgardCekBuiltinForceCount,
  sameState,
  valueHash,
} from "./cek-machine.midgard-cek-builtin-argument-count.js";
import {
  type MidgardCekCoreStepWitness,
  MidgardCekErrorCodes,
  sameBytes,
} from "./cek-machine.midgard-cek-core-step-witness.js";
import { applyBuiltinResult } from "./cek-machine.verify-lookup.js";

export const verifyReturn = (
  pre: MidgardCekMachineState,
  post: MidgardCekMachineState,
  witness: MidgardCekCoreStepWitness,
): boolean => {
  // This verifier handles return witnesses; other phases fail closed below.
  // eslint-disable-next-line @typescript-eslint/switch-exhaustiveness-check
  switch (witness.kind) {
    case "returnEmptyContinuation":
      return (
        sameBytes(pre.continuationRoot, MIDGARD_CEK_EMPTY_CONTINUATION_ROOT) &&
        sameBytes(pre.focusRoot, valueHash(witness.value)) &&
        sameState(
          post,
          isConstant(witness.value)
            ? exactState(pre, {
                mode: "haltSuccess",
                focusRoot: pre.focusRoot,
                environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
                continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
                auxiliary: 0n,
              })
            : errorSuccessor(pre, MidgardCekErrorCodes.NonconstantHalt),
        )
      );
    case "returnApplyArgument": {
      const continuation = hashMidgardCekContinuationFrame({
        kind: "applyArgument",
        argument: witness.argument,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      return (
        sameBytes(pre.continuationRoot, continuation) &&
        sameState(
          post,
          exactState(pre, {
            mode: "compute",
            focusRoot: witness.argument,
            environmentRoot: witness.capturedEnvironment,
            continuationRoot: hashMidgardCekContinuationFrame({
              kind: "applyFunction",
              functionValue: pre.focusRoot,
              tail: witness.tail,
            }),
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnApplyLambda": {
      if (
        !environmentSummaryMatches(
          witness.closureEnvironment,
          witness.closureSummary,
        )
      ) {
        return false;
      }
      const functionValue = hashMidgardCekValueNode({
        kind: "lambda",
        body: witness.body,
        environment: witness.closureEnvironment,
      });
      const continuation = hashMidgardCekContinuationFrame({
        kind: "applyFunction",
        functionValue,
        tail: witness.tail,
      });
      const nextEnvironment = hashMidgardCekEnvironmentNode({
        value: pre.focusRoot,
        tail: witness.closureEnvironment,
        length: environmentSummaryLength(witness.closureSummary) + 1n,
      });
      return (
        sameBytes(pre.continuationRoot, continuation) &&
        sameState(
          post,
          exactState(pre, {
            mode: "compute",
            focusRoot: witness.body,
            environmentRoot: nextEnvironment,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnApplyBuiltin": {
      const functionValue = hashMidgardCekValueNode({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: witness.forcesRemaining,
        argumentsCount: witness.argumentsCount,
        argumentsRoot: witness.argumentsRoot,
      });
      if (
        !sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "applyFunction",
            functionValue,
            tail: witness.tail,
          }),
        )
      ) {
        return false;
      }
      const expected = applyBuiltinResult(pre, pre.focusRoot, witness);
      return expected !== null && sameState(post, expected);
    }
    case "returnApplyInvalid": {
      const functionValue = valueHash(witness.function);
      return (
        !isLambdaOrBuiltin(witness.function) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "applyFunction",
            functionValue,
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidApplication),
        )
      );
    }
    case "returnApplyValueLambda": {
      if (
        !environmentSummaryMatches(
          witness.closureEnvironment,
          witness.closureSummary,
        )
      ) {
        return false;
      }
      const functionValue = hashMidgardCekValueNode({
        kind: "lambda",
        body: witness.body,
        environment: witness.closureEnvironment,
      });
      const nextEnvironment = hashMidgardCekEnvironmentNode({
        value: witness.argument,
        tail: witness.closureEnvironment,
        length: environmentSummaryLength(witness.closureSummary) + 1n,
      });
      return (
        sameBytes(pre.focusRoot, functionValue) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "applyValue",
            value: witness.argument,
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          exactState(pre, {
            mode: "compute",
            focusRoot: witness.body,
            environmentRoot: nextEnvironment,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnApplyValueBuiltin": {
      const functionValue = hashMidgardCekValueNode({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: witness.forcesRemaining,
        argumentsCount: witness.argumentsCount,
        argumentsRoot: witness.argumentsRoot,
      });
      if (
        !sameBytes(pre.focusRoot, functionValue) ||
        !sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "applyValue",
            value: witness.argument,
            tail: witness.tail,
          }),
        )
      ) {
        return false;
      }
      const expected = applyBuiltinResult(pre, witness.argument, witness);
      return expected !== null && sameState(post, expected);
    }
    case "returnApplyValueInvalid":
      return (
        !isLambdaOrBuiltin(witness.function) &&
        sameBytes(pre.focusRoot, valueHash(witness.function)) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "applyValue",
            value: witness.argument,
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidApplication),
        )
      );
    case "returnForceDelay": {
      const value = hashMidgardCekValueNode({
        kind: "delay",
        body: witness.body,
        environment: witness.closureEnvironment,
      });
      return (
        sameBytes(pre.focusRoot, value) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "force",
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          exactState(pre, {
            mode: "compute",
            focusRoot: witness.body,
            environmentRoot: witness.closureEnvironment,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnForceBuiltin": {
      if (
        witness.forcesRemaining <= 0n ||
        witness.forcesRemaining > midgardCekBuiltinForceCount(witness.tag)
      ) {
        return false;
      }
      const value = hashMidgardCekValueNode({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: witness.forcesRemaining,
        argumentsCount: witness.argumentsCount,
        argumentsRoot: witness.argumentsRoot,
      });
      const nextValue = hashMidgardCekValueNode({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: witness.forcesRemaining - 1n,
        argumentsCount: witness.argumentsCount,
        argumentsRoot: witness.argumentsRoot,
      });
      return (
        sameBytes(pre.focusRoot, value) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "force",
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          exactState(pre, {
            mode: "return",
            focusRoot: nextValue,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnForceInvalid":
      return (
        !isDelayOrForceableBuiltin(witness.value) &&
        sameBytes(pre.focusRoot, valueHash(witness.value)) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "force",
            tail: witness.tail,
          }),
        ) &&
        sameState(post, errorSuccessor(pre, MidgardCekErrorCodes.InvalidForce))
      );
    case "returnConstrNext": {
      if (
        witness.remainingTermsCount <= 0n ||
        !linkedSequenceRootIsWellFormed(
          witness.valuesRoot,
          witness.valuesCount,
        ) ||
        !linkedSequenceTailIsWellFormed(
          witness.remainingTermsTail,
          witness.remainingTermsCount,
        )
      ) {
        return false;
      }
      const remainingRoot = hashMidgardCekSequenceNode({
        head: witness.nextTerm,
        tail: witness.remainingTermsTail,
        length: witness.remainingTermsCount,
      });
      const nextValuesCount = witness.valuesCount + 1n;
      const nextValuesRoot = hashMidgardCekSequenceNode({
        head: pre.focusRoot,
        tail: witness.valuesRoot,
        length: nextValuesCount,
      });
      const currentContinuation = hashMidgardCekContinuationFrame({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: witness.remainingTermsCount,
        remainingTermsRoot: remainingRoot,
        valuesCount: witness.valuesCount,
        valuesRoot: witness.valuesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      const nextContinuation = hashMidgardCekContinuationFrame({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: witness.remainingTermsCount - 1n,
        remainingTermsRoot: witness.remainingTermsTail,
        valuesCount: nextValuesCount,
        valuesRoot: nextValuesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      return (
        sameBytes(pre.continuationRoot, currentContinuation) &&
        sameState(
          post,
          exactState(pre, {
            mode: "compute",
            focusRoot: witness.nextTerm,
            environmentRoot: witness.capturedEnvironment,
            continuationRoot: nextContinuation,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnConstrDone": {
      if (
        !linkedSequenceRootIsWellFormed(witness.valuesRoot, witness.valuesCount)
      ) {
        return false;
      }
      const currentContinuation = hashMidgardCekContinuationFrame({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: 0n,
        remainingTermsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
        valuesCount: witness.valuesCount,
        valuesRoot: witness.valuesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      const nextValuesCount = witness.valuesCount + 1n;
      const nextValuesRoot = hashMidgardCekSequenceNode({
        head: pre.focusRoot,
        tail: witness.valuesRoot,
        length: nextValuesCount,
      });
      return (
        sameBytes(pre.continuationRoot, currentContinuation) &&
        sameState(
          post,
          exactState(pre, {
            mode: "return",
            focusRoot: hashMidgardCekValueNode({
              kind: "constr",
              tag: witness.tag,
              valuesCount: nextValuesCount,
              valuesRoot: nextValuesRoot,
            }),
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "returnCaseConstr": {
      if (
        !linkedSequenceRootIsWellFormed(
          witness.valuesRoot,
          witness.valuesCount,
        ) ||
        !linkedSequenceRootIsWellFormed(
          witness.branchesRoot,
          witness.branchesCount,
        )
      ) {
        return false;
      }
      const value = hashMidgardCekValueNode({
        kind: "constr",
        tag: witness.tag,
        valuesCount: witness.valuesCount,
        valuesRoot: witness.valuesRoot,
      });
      const continuation = hashMidgardCekContinuationFrame({
        kind: "case",
        branchesCount: witness.branchesCount,
        branchesRoot: witness.branchesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      return (
        sameBytes(pre.focusRoot, value) &&
        sameBytes(pre.continuationRoot, continuation) &&
        sameState(
          post,
          witness.tag >= 0n && witness.tag < witness.branchesCount
            ? exactState(pre, {
                mode: "caseSelect",
                focusRoot: witness.branchesRoot,
                environmentRoot: witness.valuesRoot,
                continuationRoot: hashMidgardCekContinuationFrame({
                  kind: "caseSelect",
                  environment: witness.capturedEnvironment,
                  tail: witness.tail,
                  valuesCount: witness.valuesCount,
                }),
                auxiliary: witness.tag,
              })
            : errorSuccessor(pre, MidgardCekErrorCodes.CaseBranchMissing),
        )
      );
    }
    case "returnCaseInvalid":
      return (
        witness.value.kind !== "constr" &&
        linkedSequenceRootIsWellFormed(
          witness.branchesRoot,
          witness.branchesCount,
        ) &&
        sameBytes(pre.focusRoot, valueHash(witness.value)) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrame({
            kind: "case",
            branchesCount: witness.branchesCount,
            branchesRoot: witness.branchesRoot,
            environment: witness.capturedEnvironment,
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidCaseScrutinee),
        )
      );
    default:
      return false;
  }
};
