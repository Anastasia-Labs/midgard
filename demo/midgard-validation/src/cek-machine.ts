import {
  commitMidgardCekBlobV1,
  encodeCbor,
  hashMidgardCekContinuationFrameV1,
  hashMidgardCekDataListNodeV1,
  hashMidgardCekDataNodeV1,
  hashMidgardCekDataPairNodeV1,
  hashMidgardCekEnvironmentNodeV1,
  hashMidgardCekMachineStateV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekTermNodeV1,
  hashMidgardCekValueNodeV1,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
  MIDGARD_CEK_MAX_BUILTIN_TAG,
  midgardCekDataBytesCborLengthV1,
  midgardCekDataConstrCborLengthV1,
  midgardCekDataListCborLengthV1,
  type MidgardCekDataListNodeV1,
  midgardCekDataMapCborLengthV1,
  type MidgardCekDataNodeV1,
  type MidgardCekDataPairNodeV1,
  type MidgardCekMachineStateV1,
  type MidgardCekValueNodeV1,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  evaluateMidgardCekBlsFinalV1,
  evaluateMidgardCekDirectBuiltinV1,
  hashMidgardCekDirectArgumentsV1,
  hashMidgardCekDirectValueWitnessV1,
  type MidgardCekBlsExpressionWitnessV1,
  midgardCekDirectBuiltinBudgetV1,
  type MidgardCekDirectValueWitnessV1,
  type MidgardCekRuntimeValueWitnessV1,
  verifyMidgardCekBlsFinalV1,
  verifyMidgardCekBuiltinTypeFailureV1,
  verifyMidgardCekDirectBuiltinFailureV1,
  verifyMidgardCekDirectBuiltinV1,
} from "./cek-builtin.js";
import {
  decodeMidgardCekConstantTypeCborV1,
  decodeMidgardCekConstantWitnessV1,
  encodeMidgardCekPlutusDataV1,
  midgardCekConstantMemorySizeV1,
  type MidgardCekConstantTypeV1,
  type MidgardCekConstantWitnessV1,
  midgardCekIntegerMemorySizeV1,
} from "./cek-constant.js";
import { commitMidgardCekDataTreeV1 } from "./cek-data-tree.js";

const MACHINE_STEP_CPU = 16_000n;
const MACHINE_STEP_MEMORY = 100n;
const UINT32_MAX = 0xffff_ffffn;
const MAP_CONVERSION_CONTROL_DOMAIN = Buffer.from(
  "MidgardCekMapConversionControlV1",
  "ascii",
);

export const MidgardCekErrorCodes = Object.freeze({
  Explicit: 0n,
  UnboundVariable: 1n,
  InvalidApplication: 2n,
  InvalidForce: 3n,
  NonconstantHalt: 4n,
  InvalidCaseScrutinee: 5n,
  CaseBranchMissing: 6n,
  BuiltinFailure: 7n,
} as const);

type Bytes = Uint8Array;

export type MidgardCekEnvironmentSummaryV1 =
  | { readonly kind: "empty" }
  | {
      readonly kind: "nonempty";
      readonly value: Bytes;
      readonly tail: Bytes;
      readonly length: bigint;
    };

export type MidgardCekSemanticBuiltinWitnessV1 = {
  readonly dataNodes: readonly MidgardCekDataNodeV1[];
  readonly listNodes: readonly MidgardCekDataListNodeV1[];
  readonly pairNodes: readonly MidgardCekDataPairNodeV1[];
  readonly scalarPreimages: readonly Bytes[];
};

export type MidgardCekMapConversionControlV1 = {
  readonly tag: bigint;
  readonly resultRoot: Bytes;
  readonly sourceRoot: Bytes;
  readonly sourceRemaining: bigint;
  readonly sourcePayloadCborLength: bigint;
  readonly sourceMemory: bigint;
  readonly destinationRoot: Bytes;
  readonly destinationRemaining: bigint;
  readonly destinationPayloadCborLength: bigint;
  readonly destinationMemory: bigint;
  readonly budgetCpu: bigint;
  readonly budgetMemory: bigint;
};

export type MidgardCekMapConversionStartWitnessV1 = {
  readonly sourceNode: MidgardCekDataNodeV1;
  readonly sourceList: MidgardCekDataListNodeV1 | null;
  readonly sourcePairs: MidgardCekDataPairNodeV1 | null;
  readonly resultNode: MidgardCekDataNodeV1;
  readonly resultList: MidgardCekDataListNodeV1 | null;
  readonly resultPairs: MidgardCekDataPairNodeV1 | null;
};

export type MidgardCekCoreStepWitnessV1 =
  | { readonly kind: "computeVariable"; readonly index: bigint }
  | {
      readonly kind: "computeConstant";
      readonly value: MidgardCekConstantWitnessV1;
    }
  | { readonly kind: "computeLambda"; readonly body: Bytes }
  | { readonly kind: "computeDelay"; readonly body: Bytes }
  | {
      readonly kind: "computeApplication";
      readonly function: Bytes;
      readonly argument: Bytes;
    }
  | { readonly kind: "computeForce"; readonly term: Bytes }
  | { readonly kind: "computeError" }
  | { readonly kind: "computeBuiltin"; readonly tag: bigint }
  | { readonly kind: "computeConstrEmpty"; readonly tag: bigint }
  | {
      readonly kind: "computeConstrNonempty";
      readonly tag: bigint;
      readonly termsCount: bigint;
      readonly firstTerm: Bytes;
      readonly remainingTermsRoot: Bytes;
    }
  | {
      readonly kind: "computeCase";
      readonly scrutinee: Bytes;
      readonly branchesCount: bigint;
      readonly branchesRoot: Bytes;
    }
  | {
      readonly kind: "lookupEnvironment";
      readonly value: Bytes;
      readonly tail: Bytes;
      readonly length: bigint;
    }
  | { readonly kind: "lookupEmptyEnvironment" }
  | {
      readonly kind: "returnEmptyContinuation";
      readonly value: MidgardCekValueNodeV1;
    }
  | {
      readonly kind: "returnApplyArgument";
      readonly argument: Bytes;
      readonly capturedEnvironment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyLambda";
      readonly body: Bytes;
      readonly closureEnvironment: Bytes;
      readonly closureSummary: MidgardCekEnvironmentSummaryV1;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyBuiltin";
      readonly tag: bigint;
      readonly forcesRemaining: bigint;
      readonly argumentsCount: bigint;
      readonly argumentsRoot: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyInvalid";
      readonly function: MidgardCekValueNodeV1;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyValueLambda";
      readonly argument: Bytes;
      readonly body: Bytes;
      readonly closureEnvironment: Bytes;
      readonly closureSummary: MidgardCekEnvironmentSummaryV1;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyValueBuiltin";
      readonly argument: Bytes;
      readonly tag: bigint;
      readonly forcesRemaining: bigint;
      readonly argumentsCount: bigint;
      readonly argumentsRoot: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyValueInvalid";
      readonly argument: Bytes;
      readonly function: MidgardCekValueNodeV1;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnForceDelay";
      readonly body: Bytes;
      readonly closureEnvironment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnForceBuiltin";
      readonly tag: bigint;
      readonly forcesRemaining: bigint;
      readonly argumentsCount: bigint;
      readonly argumentsRoot: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnForceInvalid";
      readonly value: MidgardCekValueNodeV1;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnConstrNext";
      readonly tag: bigint;
      readonly remainingTermsCount: bigint;
      readonly nextTerm: Bytes;
      readonly remainingTermsTail: Bytes;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
      readonly capturedEnvironment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnConstrDone";
      readonly tag: bigint;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
      readonly capturedEnvironment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnCaseConstr";
      readonly tag: bigint;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
      readonly branchesCount: bigint;
      readonly branchesRoot: Bytes;
      readonly capturedEnvironment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnCaseInvalid";
      readonly value: MidgardCekValueNodeV1;
      readonly branchesCount: bigint;
      readonly branchesRoot: Bytes;
      readonly capturedEnvironment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "selectCaseBranch";
      readonly branch: Bytes;
      readonly remainingBranchesRoot: Bytes;
      readonly length: bigint;
      readonly capturedEnvironment: Bytes;
      readonly tail: Bytes;
      readonly valuesCount: bigint;
    }
  | {
      readonly kind: "applyCaseValue";
      readonly value: Bytes;
      readonly remainingValuesRoot: Bytes;
      readonly length: bigint;
      readonly capturedEnvironment: Bytes;
      readonly builtContinuation: Bytes;
    }
  | {
      readonly kind: "executeBuiltinTypeFailure";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekRuntimeValueWitnessV1[];
    }
  | {
      readonly kind: "executeBuiltinDirect";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitnessV1[];
      readonly result: MidgardCekDirectValueWitnessV1;
    }
  | {
      readonly kind: "executeBuiltinSemantic";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitnessV1[];
      readonly result: MidgardCekDirectValueWitnessV1;
      readonly material: MidgardCekSemanticBuiltinWitnessV1;
    }
  | {
      readonly kind: "startBuiltinMapConversion";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitnessV1[];
      readonly result: MidgardCekDirectValueWitnessV1;
      readonly material: MidgardCekMapConversionStartWitnessV1;
    }
  | {
      readonly kind: "stepBuiltinListToMap";
      readonly control: MidgardCekMapConversionControlV1;
      readonly source: MidgardCekDataListNodeV1;
      readonly pair: MidgardCekDataNodeV1;
      readonly first: MidgardCekDataListNodeV1;
      readonly second: MidgardCekDataListNodeV1;
      readonly key: MidgardCekDataNodeV1;
      readonly value: MidgardCekDataNodeV1;
      readonly destination: MidgardCekDataPairNodeV1;
    }
  | {
      readonly kind: "stepBuiltinMapToList";
      readonly control: MidgardCekMapConversionControlV1;
      readonly source: MidgardCekDataPairNodeV1;
      readonly destination: MidgardCekDataListNodeV1;
      readonly pair: MidgardCekDataNodeV1;
      readonly first: MidgardCekDataListNodeV1;
      readonly second: MidgardCekDataListNodeV1;
      readonly key: MidgardCekDataNodeV1;
      readonly value: MidgardCekDataNodeV1;
    }
  | {
      readonly kind: "finishBuiltinMapConversion";
      readonly control: MidgardCekMapConversionControlV1;
    }
  | {
      readonly kind: "executeBuiltinSemanticFailure";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitnessV1[];
      readonly material: MidgardCekSemanticBuiltinWitnessV1;
    }
  | {
      readonly kind: "executeBuiltinFailure";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitnessV1[];
    }
  | {
      readonly kind: "executeBuiltinBlsFinal";
      readonly leftRoot: Bytes;
      readonly rightRoot: Bytes;
      readonly leftExpression: MidgardCekBlsExpressionWitnessV1;
      readonly rightExpression: MidgardCekBlsExpressionWitnessV1;
      readonly result: MidgardCekDirectValueWitnessV1;
    }
  | { readonly kind: "computeContextConstant"; readonly valueRoot: Bytes };

const sameBytes = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));

const sameState = (
  left: MidgardCekMachineStateV1,
  right: MidgardCekMachineStateV1,
): boolean =>
  left.mode === right.mode &&
  left.executionIndex === right.executionIndex &&
  sameBytes(left.focusRoot, right.focusRoot) &&
  sameBytes(left.environmentRoot, right.environmentRoot) &&
  sameBytes(left.continuationRoot, right.continuationRoot) &&
  left.auxiliary === right.auxiliary &&
  left.cpu === right.cpu &&
  left.memory === right.memory;

export const encodeMidgardCekMapConversionControlV1 = (
  control: MidgardCekMapConversionControlV1,
): Buffer =>
  encodeCbor([
    control.tag,
    Buffer.from(control.resultRoot),
    Buffer.from(control.sourceRoot),
    control.sourceRemaining,
    control.sourcePayloadCborLength,
    control.sourceMemory,
    Buffer.from(control.destinationRoot),
    control.destinationRemaining,
    control.destinationPayloadCborLength,
    control.destinationMemory,
    control.budgetCpu,
    control.budgetMemory,
  ]);

const mapConversionControlIsWellFormed = (
  control: MidgardCekMapConversionControlV1,
): boolean => {
  if (
    (control.tag !== 38n && control.tag !== 43n) ||
    control.resultRoot.length !== 32 ||
    control.sourceRoot.length !== 32 ||
    control.destinationRoot.length !== 32 ||
    control.sourceRemaining < 0n ||
    control.sourceRemaining !== control.destinationRemaining ||
    control.sourcePayloadCborLength < 0n ||
    control.sourceMemory < 0n ||
    control.destinationPayloadCborLength < 0n ||
    control.destinationMemory < 0n ||
    control.budgetCpu < 0n ||
    control.budgetMemory < 0n
  ) {
    return false;
  }
  if (control.sourceRemaining !== 0n) return true;
  return (
    control.sourcePayloadCborLength === 0n &&
    control.sourceMemory === 0n &&
    control.destinationPayloadCborLength === 0n &&
    control.destinationMemory === 0n &&
    (control.tag === 38n
      ? sameBytes(control.sourceRoot, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1) &&
        sameBytes(
          control.destinationRoot,
          MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
        )
      : sameBytes(control.sourceRoot, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1) &&
        sameBytes(
          control.destinationRoot,
          MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
        ))
  );
};

export const hashMidgardCekMapConversionControlV1 = (
  control: MidgardCekMapConversionControlV1,
): Bytes => {
  if (!mapConversionControlIsWellFormed(control)) {
    throw new Error("invalid V1 CEK map-conversion control");
  }
  return Buffer.from(
    blake2b(
      Buffer.concat([
        MAP_CONVERSION_CONTROL_DOMAIN,
        encodeMidgardCekMapConversionControlV1(control),
      ]),
      { dkLen: 32 },
    ),
  );
};

const exactState = (
  pre: MidgardCekMachineStateV1,
  update: {
    readonly mode: MidgardCekMachineStateV1["mode"];
    readonly focusRoot: Bytes;
    readonly environmentRoot: Bytes;
    readonly continuationRoot: Bytes;
    readonly auxiliary: bigint;
    readonly cpuDelta?: bigint;
    readonly memoryDelta?: bigint;
  },
): MidgardCekMachineStateV1 => ({
  mode: update.mode,
  executionIndex: pre.executionIndex,
  focusRoot: update.focusRoot,
  environmentRoot: update.environmentRoot,
  continuationRoot: update.continuationRoot,
  auxiliary: update.auxiliary,
  cpu: pre.cpu + (update.cpuDelta ?? 0n),
  memory: pre.memory + (update.memoryDelta ?? 0n),
});

const exactComputeSuccessor = (
  pre: MidgardCekMachineStateV1,
  update: Omit<
    Parameters<typeof exactState>[1],
    "cpuDelta" | "memoryDelta"
  >,
): MidgardCekMachineStateV1 =>
  exactState(pre, {
    ...update,
    cpuDelta: MACHINE_STEP_CPU,
    memoryDelta: MACHINE_STEP_MEMORY,
  });

const errorSuccessor = (
  pre: MidgardCekMachineStateV1,
  reason: bigint,
): MidgardCekMachineStateV1 =>
  exactState(pre, {
    mode: "haltError",
    focusRoot: hashMidgardCekTermNodeV1({ kind: "error" }),
    environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
    continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    auxiliary: reason,
  });

const nonNegativeUint32 = (value: bigint): boolean =>
  value >= 0n && value <= UINT32_MAX;

const linkedSequenceRootIsWellFormed = (
  root: Bytes,
  count: bigint,
): boolean =>
  nonNegativeUint32(count) &&
  (count === 0n) === sameBytes(root, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1);

const linkedSequenceTailIsWellFormed = (
  tail: Bytes,
  length: bigint,
): boolean =>
  length > 0n &&
  length <= UINT32_MAX &&
  (length === 1n) === sameBytes(tail, MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1);

const valueHash = (value: MidgardCekValueNodeV1): Bytes =>
  hashMidgardCekValueNodeV1(value);

const isConstant = (value: MidgardCekValueNodeV1): boolean =>
  value.kind === "constant";

const isLambdaOrBuiltin = (value: MidgardCekValueNodeV1): boolean =>
  value.kind === "lambda" || value.kind === "builtin";

const isDelayOrForceableBuiltin = (
  value: MidgardCekValueNodeV1,
): boolean =>
  value.kind === "delay" ||
  (value.kind === "builtin" && value.forcesRemaining > 0n);

const environmentSummaryLength = (
  summary: MidgardCekEnvironmentSummaryV1,
): bigint => (summary.kind === "empty" ? 0n : summary.length);

const environmentSummaryMatches = (
  root: Bytes,
  summary: MidgardCekEnvironmentSummaryV1,
): boolean => {
  if (summary.kind === "empty") {
    return sameBytes(root, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1);
  }
  return (
    summary.length > 0n &&
    summary.length <= UINT32_MAX &&
    sameBytes(
      root,
      hashMidgardCekEnvironmentNodeV1({
        value: summary.value,
        tail: summary.tail,
        length: summary.length,
      }),
    ) &&
    (summary.length === 1n) ===
      sameBytes(summary.tail, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1)
  );
};

export const midgardCekBuiltinForceCount = (tag: bigint): bigint => {
  if (tag < 0n || tag > MIDGARD_CEK_MAX_BUILTIN_TAG) {
    throw new RangeError("CEK builtin tag is outside the V1 table");
  }
  if (tag === 29n || tag === 30n || tag === 31n) return 2n;
  if (
    tag === 26n ||
    tag === 27n ||
    tag === 28n ||
    (tag >= 32n && tag <= 36n)
  ) {
    return 1n;
  }
  return 0n;
};

export const midgardCekBuiltinArgumentCount = (tag: bigint): bigint => {
  if (tag < 0n || tag > MIDGARD_CEK_MAX_BUILTIN_TAG) {
    throw new RangeError("CEK builtin tag is outside the V1 table");
  }
  if (tag === 36n) return 6n;
  if (
    [
      12n,
      21n,
      26n,
      31n,
      52n,
      53n,
      73n,
      75n,
      76n,
      77n,
      80n,
    ].includes(tag)
  ) {
    return 3n;
  }
  if (
    tag <= 11n ||
    [
      14n,
      15n,
      16n,
      17n,
      22n,
      23n,
      27n,
      28n,
      32n,
      37n,
      47n,
      48n,
      54n,
      56n,
      57n,
      58n,
      61n,
      63n,
      64n,
      65n,
      68n,
      69n,
      70n,
      74n,
      79n,
      81n,
      82n,
      83n,
    ].includes(tag)
  ) {
    return 2n;
  }
  return 1n;
};

const verifyCompute = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  switch (witness.kind) {
    case "computeVariable":
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
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
        const valueRoot = hashMidgardCekDirectValueWitnessV1(direct);
        return (
          sameBytes(
            pre.focusRoot,
            hashMidgardCekTermNodeV1({
              kind: "constant",
              value: valueRoot,
            }),
          ) &&
          sameState(
            post,
            exactComputeSuccessor(pre, {
              mode: "return",
              focusRoot: valueRoot,
              environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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
          hashMidgardCekTermNodeV1({
            kind: "contextConstant",
            value: witness.valueRoot,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: witness.valueRoot,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    case "computeLambda": {
      const value = hashMidgardCekValueNodeV1({
        kind: "lambda",
        body: witness.body,
        environment: pre.environmentRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
            kind: "lambda",
            body: witness.body,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeDelay": {
      const value = hashMidgardCekValueNodeV1({
        kind: "delay",
        body: witness.body,
        environment: pre.environmentRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
            kind: "delay",
            body: witness.body,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeApplication": {
      const continuation = hashMidgardCekContinuationFrameV1({
        kind: "applyArgument",
        argument: witness.argument,
        environment: pre.environmentRoot,
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
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
      const continuation = hashMidgardCekContinuationFrameV1({
        kind: "force",
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
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
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({ kind: "error" }),
        ) &&
        sameState(
          post,
          errorSuccessor(pre, MidgardCekErrorCodes.Explicit),
        )
      );
    case "computeBuiltin": {
      const value = hashMidgardCekValueNodeV1({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: midgardCekBuiltinForceCount(witness.tag),
        argumentsCount: 0n,
        argumentsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
            kind: "builtin",
            tag: witness.tag,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
            continuationRoot: pre.continuationRoot,
            auxiliary: 0n,
          }),
        )
      );
    }
    case "computeConstrEmpty": {
      const value = hashMidgardCekValueNodeV1({
        kind: "constr",
        tag: witness.tag,
        valuesCount: 0n,
        valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
            kind: "constr",
            tag: witness.tag,
            termsCount: 0n,
            termsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
          }),
        ) &&
        sameState(
          post,
          exactComputeSuccessor(pre, {
            mode: "return",
            focusRoot: value,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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
      const termsRoot = hashMidgardCekSequenceNodeV1({
        head: witness.firstTerm,
        tail: witness.remainingTermsRoot,
        length: witness.termsCount,
      });
      const continuation = hashMidgardCekContinuationFrameV1({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: witness.termsCount - 1n,
        remainingTermsRoot: witness.remainingTermsRoot,
        valuesCount: 0n,
        valuesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
        environment: pre.environmentRoot,
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
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
      const continuation = hashMidgardCekContinuationFrameV1({
        kind: "case",
        branchesCount: witness.branchesCount,
        branchesRoot: witness.branchesRoot,
        environment: pre.environmentRoot,
        tail: pre.continuationRoot,
      });
      return (
        sameBytes(
          pre.focusRoot,
          hashMidgardCekTermNodeV1({
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

const verifyLookup = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  if (witness.kind === "lookupEmptyEnvironment") {
    return (
      sameBytes(pre.focusRoot, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1) &&
      sameBytes(pre.environmentRoot, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1) &&
      sameState(
        post,
        errorSuccessor(pre, MidgardCekErrorCodes.UnboundVariable),
      )
    );
  }
  if (witness.kind !== "lookupEnvironment") return false;
  if (
    witness.length <= 0n ||
    !nonNegativeUint32(witness.length) ||
    (witness.length === 1n) !==
      sameBytes(witness.tail, MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1)
  ) {
    return false;
  }
  const root = hashMidgardCekEnvironmentNodeV1({
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
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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

const applyBuiltinResult = (
  pre: MidgardCekMachineStateV1,
  argument: Bytes,
  input: {
    readonly tag: bigint;
    readonly forcesRemaining: bigint;
    readonly argumentsCount: bigint;
    readonly argumentsRoot: Bytes;
    readonly tail: Bytes;
  },
): MidgardCekMachineStateV1 | null => {
  const requiredArguments = midgardCekBuiltinArgumentCount(input.tag);
  if (
    input.forcesRemaining !== 0n ||
    input.argumentsCount < 0n ||
    input.argumentsCount >= requiredArguments
  ) {
    return null;
  }
  const nextCount = input.argumentsCount + 1n;
  const nextRoot = hashMidgardCekSequenceNodeV1({
    head: argument,
    tail: input.argumentsRoot,
    length: nextCount,
  });
  return exactState(pre, {
    mode: nextCount === requiredArguments ? "builtin" : "return",
    focusRoot: hashMidgardCekValueNodeV1({
      kind: "builtin",
      tag: input.tag,
      forcesRemaining: input.forcesRemaining,
      argumentsCount: nextCount,
      argumentsRoot: nextRoot,
    }),
    environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
    continuationRoot: input.tail,
    auxiliary: 0n,
  });
};

const verifyReturn = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  switch (witness.kind) {
    case "returnEmptyContinuation":
      return (
        sameBytes(
          pre.continuationRoot,
          MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        ) &&
        sameBytes(pre.focusRoot, valueHash(witness.value)) &&
        sameState(
          post,
          isConstant(witness.value)
            ? exactState(pre, {
                mode: "haltSuccess",
                focusRoot: pre.focusRoot,
                environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
                continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
                auxiliary: 0n,
              })
            : errorSuccessor(pre, MidgardCekErrorCodes.NonconstantHalt),
        )
      );
    case "returnApplyArgument": {
      const continuation = hashMidgardCekContinuationFrameV1({
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
            continuationRoot: hashMidgardCekContinuationFrameV1({
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
      const functionValue = hashMidgardCekValueNodeV1({
        kind: "lambda",
        body: witness.body,
        environment: witness.closureEnvironment,
      });
      const continuation = hashMidgardCekContinuationFrameV1({
        kind: "applyFunction",
        functionValue,
        tail: witness.tail,
      });
      const nextEnvironment = hashMidgardCekEnvironmentNodeV1({
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
      const functionValue = hashMidgardCekValueNodeV1({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: witness.forcesRemaining,
        argumentsCount: witness.argumentsCount,
        argumentsRoot: witness.argumentsRoot,
      });
      if (
        !sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrameV1({
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
          hashMidgardCekContinuationFrameV1({
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
      const functionValue = hashMidgardCekValueNodeV1({
        kind: "lambda",
        body: witness.body,
        environment: witness.closureEnvironment,
      });
      const nextEnvironment = hashMidgardCekEnvironmentNodeV1({
        value: witness.argument,
        tail: witness.closureEnvironment,
        length: environmentSummaryLength(witness.closureSummary) + 1n,
      });
      return (
        sameBytes(pre.focusRoot, functionValue) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrameV1({
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
      const functionValue = hashMidgardCekValueNodeV1({
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
          hashMidgardCekContinuationFrameV1({
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
          hashMidgardCekContinuationFrameV1({
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
      const value = hashMidgardCekValueNodeV1({
        kind: "delay",
        body: witness.body,
        environment: witness.closureEnvironment,
      });
      return (
        sameBytes(pre.focusRoot, value) &&
        sameBytes(
          pre.continuationRoot,
          hashMidgardCekContinuationFrameV1({
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
      const value = hashMidgardCekValueNodeV1({
        kind: "builtin",
        tag: witness.tag,
        forcesRemaining: witness.forcesRemaining,
        argumentsCount: witness.argumentsCount,
        argumentsRoot: witness.argumentsRoot,
      });
      const nextValue = hashMidgardCekValueNodeV1({
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
          hashMidgardCekContinuationFrameV1({
            kind: "force",
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          exactState(pre, {
            mode: "return",
            focusRoot: nextValue,
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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
          hashMidgardCekContinuationFrameV1({
            kind: "force",
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          errorSuccessor(pre, MidgardCekErrorCodes.InvalidForce),
        )
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
      const remainingRoot = hashMidgardCekSequenceNodeV1({
        head: witness.nextTerm,
        tail: witness.remainingTermsTail,
        length: witness.remainingTermsCount,
      });
      const nextValuesCount = witness.valuesCount + 1n;
      const nextValuesRoot = hashMidgardCekSequenceNodeV1({
        head: pre.focusRoot,
        tail: witness.valuesRoot,
        length: nextValuesCount,
      });
      const currentContinuation = hashMidgardCekContinuationFrameV1({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: witness.remainingTermsCount,
        remainingTermsRoot: remainingRoot,
        valuesCount: witness.valuesCount,
        valuesRoot: witness.valuesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      const nextContinuation = hashMidgardCekContinuationFrameV1({
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
        !linkedSequenceRootIsWellFormed(
          witness.valuesRoot,
          witness.valuesCount,
        )
      ) {
        return false;
      }
      const currentContinuation = hashMidgardCekContinuationFrameV1({
        kind: "constr",
        tag: witness.tag,
        remainingTermsCount: 0n,
        remainingTermsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
        valuesCount: witness.valuesCount,
        valuesRoot: witness.valuesRoot,
        environment: witness.capturedEnvironment,
        tail: witness.tail,
      });
      const nextValuesCount = witness.valuesCount + 1n;
      const nextValuesRoot = hashMidgardCekSequenceNodeV1({
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
            focusRoot: hashMidgardCekValueNodeV1({
              kind: "constr",
              tag: witness.tag,
              valuesCount: nextValuesCount,
              valuesRoot: nextValuesRoot,
            }),
            environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
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
      const value = hashMidgardCekValueNodeV1({
        kind: "constr",
        tag: witness.tag,
        valuesCount: witness.valuesCount,
        valuesRoot: witness.valuesRoot,
      });
      const continuation = hashMidgardCekContinuationFrameV1({
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
                continuationRoot: hashMidgardCekContinuationFrameV1({
                  kind: "caseSelect",
                  environment: witness.capturedEnvironment,
                  tail: witness.tail,
                  valuesCount: witness.valuesCount,
                }),
                auxiliary: witness.tag,
              })
            : errorSuccessor(
                pre,
                MidgardCekErrorCodes.CaseBranchMissing,
              ),
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
          hashMidgardCekContinuationFrameV1({
            kind: "case",
            branchesCount: witness.branchesCount,
            branchesRoot: witness.branchesRoot,
            environment: witness.capturedEnvironment,
            tail: witness.tail,
          }),
        ) &&
        sameState(
          post,
          errorSuccessor(
            pre,
            MidgardCekErrorCodes.InvalidCaseScrutinee,
          ),
        )
      );
    default:
      return false;
  }
};

const verifyCaseSelect = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  if (witness.kind !== "selectCaseBranch") return false;
  if (
    witness.length <= 0n ||
    pre.auxiliary < 0n ||
    pre.auxiliary >= witness.length ||
    !linkedSequenceTailIsWellFormed(
      witness.remainingBranchesRoot,
      witness.length,
    ) ||
    !linkedSequenceRootIsWellFormed(
      pre.environmentRoot,
      witness.valuesCount,
    )
  ) {
    return false;
  }
  const work = hashMidgardCekContinuationFrameV1({
    kind: "caseSelect",
    environment: witness.capturedEnvironment,
    tail: witness.tail,
    valuesCount: witness.valuesCount,
  });
  if (
    !sameBytes(
      pre.focusRoot,
      hashMidgardCekSequenceNodeV1({
        head: witness.branch,
        tail: witness.remainingBranchesRoot,
        length: witness.length,
      }),
    ) ||
    !sameBytes(pre.continuationRoot, work)
  ) {
    return false;
  }
  const expected =
    pre.auxiliary > 0n
      ? exactState(pre, {
          mode: "caseSelect",
          focusRoot: witness.remainingBranchesRoot,
          environmentRoot: pre.environmentRoot,
          continuationRoot: work,
          auxiliary: pre.auxiliary - 1n,
        })
      : witness.valuesCount === 0n
        ? exactState(pre, {
            mode: "compute",
            focusRoot: witness.branch,
            environmentRoot: witness.capturedEnvironment,
            continuationRoot: witness.tail,
            auxiliary: 0n,
          })
        : exactState(pre, {
            mode: "caseApply",
            focusRoot: pre.environmentRoot,
            environmentRoot: witness.branch,
            continuationRoot: hashMidgardCekContinuationFrameV1({
              kind: "caseApply",
              environment: witness.capturedEnvironment,
              builtContinuation: witness.tail,
            }),
            auxiliary: witness.valuesCount,
          });
  return sameState(post, expected);
};

const verifyCaseApply = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  if (
    witness.kind !== "applyCaseValue" ||
    witness.length <= 0n ||
    pre.auxiliary !== witness.length ||
    !linkedSequenceTailIsWellFormed(
      witness.remainingValuesRoot,
      witness.length,
    )
  ) {
    return false;
  }
  if (
    !sameBytes(
      pre.focusRoot,
      hashMidgardCekSequenceNodeV1({
        head: witness.value,
        tail: witness.remainingValuesRoot,
        length: witness.length,
      }),
    ) ||
    !sameBytes(
      pre.continuationRoot,
      hashMidgardCekContinuationFrameV1({
        kind: "caseApply",
        environment: witness.capturedEnvironment,
        builtContinuation: witness.builtContinuation,
      }),
    )
  ) {
    return false;
  }
  const nextContinuation = hashMidgardCekContinuationFrameV1({
    kind: "applyValue",
    value: witness.value,
    tail: witness.builtContinuation,
  });
  return sameState(
    post,
    witness.length === 1n
      ? exactState(pre, {
          mode: "compute",
          focusRoot: pre.environmentRoot,
          environmentRoot: witness.capturedEnvironment,
          continuationRoot: nextContinuation,
          auxiliary: 0n,
        })
      : exactState(pre, {
          mode: "caseApply",
          focusRoot: witness.remainingValuesRoot,
          environmentRoot: pre.environmentRoot,
          continuationRoot: hashMidgardCekContinuationFrameV1({
            kind: "caseApply",
            environment: witness.capturedEnvironment,
            builtContinuation: nextContinuation,
          }),
          auxiliary: witness.length - 1n,
        }),
  );
};

type DataSummaryV1 = {
  readonly root: Bytes;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type DataSequenceSummaryV1 = {
  readonly root: Bytes;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

const dataNodeSummary = (
  node: MidgardCekDataNodeV1,
): DataSummaryV1 => ({
  root: hashMidgardCekDataNodeV1(node),
  cborLength: node.cborLength,
  memory: node.memory,
});

const listSequenceFromNode = (
  node: MidgardCekDataNodeV1,
): DataSequenceSummaryV1 | null => {
  if (node.kind !== "list") return null;
  return {
    root: node.itemsRoot,
    length: node.itemsCount,
    payloadCborLength:
      node.cborLength - (node.itemsCount === 0n ? 1n : 2n),
    memory: node.memory - 4n,
  };
};

const mapSequenceFromNode = (
  node: MidgardCekDataNodeV1,
): DataSequenceSummaryV1 | null => {
  if (node.kind !== "map") return null;
  const header =
    node.entriesCount < 24n
      ? 1n
      : node.entriesCount <= 0xffn
        ? 2n
        : node.entriesCount <= 0xffffn
          ? 3n
          : 5n;
  return {
    root: node.entriesRoot,
    length: node.entriesCount,
    payloadCborLength: node.cborLength - header,
    memory: node.memory - 4n,
  };
};

const dataListSummaryMatches = (
  sequence: DataSequenceSummaryV1,
  node: MidgardCekDataListNodeV1 | null,
): boolean =>
  sequence.length === 0n
    ? node === null &&
      sameBytes(sequence.root, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1) &&
      sequence.payloadCborLength === 0n &&
      sequence.memory === 0n
    : node !== null &&
      sameBytes(sequence.root, hashMidgardCekDataListNodeV1(node)) &&
      node.length === sequence.length &&
      node.payloadCborLength === sequence.payloadCborLength &&
      node.memory === sequence.memory;

const dataPairSummaryMatches = (
  sequence: DataSequenceSummaryV1,
  node: MidgardCekDataPairNodeV1 | null,
): boolean =>
  sequence.length === 0n
    ? node === null &&
      sameBytes(sequence.root, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1) &&
      sequence.payloadCborLength === 0n &&
      sequence.memory === 0n
    : node !== null &&
      sameBytes(sequence.root, hashMidgardCekDataPairNodeV1(node)) &&
      node.length === sequence.length &&
      node.payloadCborLength === sequence.payloadCborLength &&
      node.memory === sequence.memory;

const semanticPayloadMatches = (
  value: MidgardCekDirectValueWitnessV1,
  node: MidgardCekDataNodeV1,
): boolean =>
  value.kind === "semanticConstant" &&
  sameBytes(value.witness.payload.root, hashMidgardCekDataNodeV1(node)) &&
  value.witness.payload.cborLength === node.cborLength &&
  value.witness.payload.memory === node.memory;

const isDataType = (
  value: MidgardCekDirectValueWitnessV1,
): boolean =>
  value.kind === "semanticConstant" &&
  decodeMidgardCekConstantTypeCborV1(value.witness.typeCbor).kind ===
    "data";

const isListDataPairType = (
  value: MidgardCekDirectValueWitnessV1,
): boolean => {
  if (value.kind !== "semanticConstant") return false;
  const type = decodeMidgardCekConstantTypeCborV1(
    value.witness.typeCbor,
  );
  return (
    type.kind === "list" &&
    type.element.kind === "pair" &&
    type.element.first.kind === "data" &&
    type.element.second.kind === "data"
  );
};

const builtinRootMatches = (
  pre: MidgardCekMachineStateV1,
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
): boolean => {
  const committed = hashMidgardCekDirectArgumentsV1(arguments_);
  return sameBytes(
    pre.focusRoot,
    hashMidgardCekValueNodeV1({
      kind: "builtin",
      tag,
      forcesRemaining: 0n,
      argumentsCount: committed.count,
      argumentsRoot: committed.root,
    }),
  );
};

const verifyMapConversionStart = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: Extract<
    MidgardCekCoreStepWitnessV1,
    { readonly kind: "startBuiltinMapConversion" }
  >,
): boolean => {
  if (
    (witness.tag !== 38n && witness.tag !== 43n) ||
    witness.arguments.length !== 1 ||
    !builtinRootMatches(pre, witness.tag, witness.arguments)
  ) {
    return false;
  }
  const source = witness.arguments[0]!;
  if (
    !semanticPayloadMatches(source, witness.material.sourceNode) ||
    !semanticPayloadMatches(witness.result, witness.material.resultNode)
  ) {
    return false;
  }
  const sourceSequence =
    witness.tag === 38n
      ? listSequenceFromNode(witness.material.sourceNode)
      : mapSequenceFromNode(witness.material.sourceNode);
  const destinationSequence =
    witness.tag === 38n
      ? mapSequenceFromNode(witness.material.resultNode)
      : listSequenceFromNode(witness.material.resultNode);
  if (
    sourceSequence === null ||
    destinationSequence === null ||
    sourceSequence.length !== destinationSequence.length
  ) {
    return false;
  }
  const topologyMatches =
    witness.tag === 38n
      ? dataListSummaryMatches(
          sourceSequence,
          witness.material.sourceList,
        ) &&
        witness.material.sourcePairs === null &&
        dataPairSummaryMatches(
          destinationSequence,
          witness.material.resultPairs,
        ) &&
        witness.material.resultList === null
      : dataPairSummaryMatches(
          sourceSequence,
          witness.material.sourcePairs,
        ) &&
        witness.material.sourceList === null &&
        dataListSummaryMatches(
          destinationSequence,
          witness.material.resultList,
        ) &&
        witness.material.resultPairs === null;
  if (!topologyMatches) return false;
  if (
    witness.tag === 38n
      ? !isListDataPairType(source) ||
        !isDataType(witness.result) ||
        source.kind !== "semanticConstant" ||
        witness.result.kind !== "semanticConstant" ||
        source.witness.memory !==
          sourceSequence.memory - sourceSequence.length * 4n ||
        witness.result.witness.memory !== destinationSequence.memory + 4n
      : !isDataType(source) ||
        !isListDataPairType(witness.result) ||
        source.kind !== "semanticConstant" ||
        witness.result.kind !== "semanticConstant" ||
        source.witness.memory !== sourceSequence.memory + 4n ||
        witness.result.witness.memory !==
          destinationSequence.memory - destinationSequence.length * 4n
  ) {
    return false;
  }
  const budget = midgardCekDirectBuiltinBudgetV1(
    witness.tag,
    witness.arguments,
  );
  const control: MidgardCekMapConversionControlV1 = {
    tag: witness.tag,
    resultRoot: hashMidgardCekDirectValueWitnessV1(witness.result),
    sourceRoot: sourceSequence.root,
    sourceRemaining: sourceSequence.length,
    sourcePayloadCborLength: sourceSequence.payloadCborLength,
    sourceMemory: sourceSequence.memory,
    destinationRoot: destinationSequence.root,
    destinationRemaining: destinationSequence.length,
    destinationPayloadCborLength:
      destinationSequence.payloadCborLength,
    destinationMemory: destinationSequence.memory,
    budgetCpu: budget.cpu,
    budgetMemory: budget.memory,
  };
  return sameState(
    post,
    exactState(pre, {
      mode: "semanticBuiltin",
      focusRoot: hashMidgardCekMapConversionControlV1(control),
      environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      continuationRoot: pre.continuationRoot,
      auxiliary: 0n,
    }),
  );
};

const dataListLinkMatches = (
  link: MidgardCekDataListNodeV1,
  head: MidgardCekDataNodeV1,
  tail: MidgardCekDataListNodeV1 | null,
): boolean => {
  const headSummary = dataNodeSummary(head);
  const tailRoot =
    tail === null
      ? MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1
      : hashMidgardCekDataListNodeV1(tail);
  const tailLength = tail?.length ?? 0n;
  const tailPayload = tail?.payloadCborLength ?? 0n;
  const tailMemory = tail?.memory ?? 0n;
  return (
    sameBytes(link.head, headSummary.root) &&
    link.headCborLength === headSummary.cborLength &&
    link.headMemory === headSummary.memory &&
    sameBytes(link.tail, tailRoot) &&
    link.length === tailLength + 1n &&
    link.payloadCborLength === headSummary.cborLength + tailPayload &&
    link.memory === headSummary.memory + tailMemory
  );
};

const pairWrapperMatches = (
  pair: MidgardCekDataNodeV1,
  first: MidgardCekDataListNodeV1,
  second: MidgardCekDataListNodeV1,
  key: MidgardCekDataNodeV1,
  value: MidgardCekDataNodeV1,
): boolean =>
  pair.kind === "constrSmall" &&
  pair.constructor === 0n &&
  pair.fieldsCount === 2n &&
  sameBytes(pair.fieldsRoot, hashMidgardCekDataListNodeV1(first)) &&
  pair.memory === 4n + first.memory &&
  dataListLinkMatches(first, key, second) &&
  dataListLinkMatches(second, value, null);

const nextMapControl = (
  control: MidgardCekMapConversionControlV1,
  sourcePayload: bigint,
  sourceMemory: bigint,
  sourceTail: Bytes,
  destinationPayload: bigint,
  destinationMemory: bigint,
  destinationTail: Bytes,
): MidgardCekMapConversionControlV1 => ({
  ...control,
  sourceRoot: sourceTail,
  sourceRemaining: control.sourceRemaining - 1n,
  sourcePayloadCborLength:
    control.sourcePayloadCborLength - sourcePayload,
  sourceMemory: control.sourceMemory - sourceMemory,
  destinationRoot: destinationTail,
  destinationRemaining: control.destinationRemaining - 1n,
  destinationPayloadCborLength:
    control.destinationPayloadCborLength - destinationPayload,
  destinationMemory: control.destinationMemory - destinationMemory,
});

const verifySemanticBuiltinControl = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  if (witness.kind === "finishBuiltinMapConversion") {
    return (
      mapConversionControlIsWellFormed(witness.control) &&
      witness.control.sourceRemaining === 0n &&
      sameBytes(
        pre.focusRoot,
        hashMidgardCekMapConversionControlV1(witness.control),
      ) &&
      sameState(
        post,
        exactState(pre, {
          mode: "return",
          focusRoot: witness.control.resultRoot,
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
          cpuDelta: witness.control.budgetCpu,
          memoryDelta: witness.control.budgetMemory,
        }),
      )
    );
  }
  if (witness.kind === "stepBuiltinListToMap") {
    const control = witness.control;
    const pairSummary = dataNodeSummary(witness.pair);
    const keySummary = dataNodeSummary(witness.key);
    const valueSummary = dataNodeSummary(witness.value);
    const next = nextMapControl(
      control,
      witness.source.headCborLength,
      witness.source.headMemory,
      witness.source.tail,
      witness.destination.keyCborLength +
        witness.destination.valueCborLength,
      witness.destination.keyMemory + witness.destination.valueMemory,
      witness.destination.tail,
    );
    return (
      control.tag === 38n &&
      control.sourceRemaining > 0n &&
      sameBytes(
        pre.focusRoot,
        hashMidgardCekMapConversionControlV1(control),
      ) &&
      sameBytes(
        hashMidgardCekDataListNodeV1(witness.source),
        control.sourceRoot,
      ) &&
      witness.source.length === control.sourceRemaining &&
      witness.source.payloadCborLength ===
        control.sourcePayloadCborLength &&
      witness.source.memory === control.sourceMemory &&
      sameBytes(witness.source.head, pairSummary.root) &&
      witness.source.headCborLength === pairSummary.cborLength &&
      witness.source.headMemory === pairSummary.memory &&
      pairWrapperMatches(
        witness.pair,
        witness.first,
        witness.second,
        witness.key,
        witness.value,
      ) &&
      sameBytes(
        hashMidgardCekDataPairNodeV1(witness.destination),
        control.destinationRoot,
      ) &&
      witness.destination.length === control.destinationRemaining &&
      witness.destination.payloadCborLength ===
        control.destinationPayloadCborLength &&
      witness.destination.memory === control.destinationMemory &&
      sameBytes(witness.destination.key, keySummary.root) &&
      witness.destination.keyCborLength === keySummary.cborLength &&
      witness.destination.keyMemory === keySummary.memory &&
      sameBytes(witness.destination.value, valueSummary.root) &&
      witness.destination.valueCborLength === valueSummary.cborLength &&
      witness.destination.valueMemory === valueSummary.memory &&
      sameState(
        post,
        exactState(pre, {
          mode: "semanticBuiltin",
          focusRoot: hashMidgardCekMapConversionControlV1(next),
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        }),
      )
    );
  }
  if (witness.kind === "stepBuiltinMapToList") {
    const control = witness.control;
    const pairSummary = dataNodeSummary(witness.pair);
    const keySummary = dataNodeSummary(witness.key);
    const valueSummary = dataNodeSummary(witness.value);
    const next = nextMapControl(
      control,
      witness.source.keyCborLength + witness.source.valueCborLength,
      witness.source.keyMemory + witness.source.valueMemory,
      witness.source.tail,
      witness.destination.headCborLength,
      witness.destination.headMemory,
      witness.destination.tail,
    );
    return (
      control.tag === 43n &&
      control.sourceRemaining > 0n &&
      sameBytes(
        pre.focusRoot,
        hashMidgardCekMapConversionControlV1(control),
      ) &&
      sameBytes(
        hashMidgardCekDataPairNodeV1(witness.source),
        control.sourceRoot,
      ) &&
      witness.source.length === control.sourceRemaining &&
      witness.source.payloadCborLength ===
        control.sourcePayloadCborLength &&
      witness.source.memory === control.sourceMemory &&
      sameBytes(witness.source.key, keySummary.root) &&
      witness.source.keyCborLength === keySummary.cborLength &&
      witness.source.keyMemory === keySummary.memory &&
      sameBytes(witness.source.value, valueSummary.root) &&
      witness.source.valueCborLength === valueSummary.cborLength &&
      witness.source.valueMemory === valueSummary.memory &&
      pairWrapperMatches(
        witness.pair,
        witness.first,
        witness.second,
        witness.key,
        witness.value,
      ) &&
      sameBytes(
        hashMidgardCekDataListNodeV1(witness.destination),
        control.destinationRoot,
      ) &&
      witness.destination.length === control.destinationRemaining &&
      witness.destination.payloadCborLength ===
        control.destinationPayloadCborLength &&
      witness.destination.memory === control.destinationMemory &&
      sameBytes(witness.destination.head, pairSummary.root) &&
      witness.destination.headCborLength === pairSummary.cborLength &&
      witness.destination.headMemory === pairSummary.memory &&
      sameState(
        post,
        exactState(pre, {
          mode: "semanticBuiltin",
          focusRoot: hashMidgardCekMapConversionControlV1(next),
          environmentRoot: pre.environmentRoot,
          continuationRoot: pre.continuationRoot,
          auxiliary: 0n,
        }),
      )
    );
  }
  return false;
};

type ConstantPartsV1 = {
  readonly type: MidgardCekConstantTypeV1;
  readonly payload: DataSummaryV1;
  readonly memory: bigint;
};

const sameConstantType = (
  left: MidgardCekConstantTypeV1,
  right: MidgardCekConstantTypeV1,
): boolean => {
  if (left.kind !== right.kind) return false;
  if (left.kind === "list" && right.kind === "list") {
    return sameConstantType(left.element, right.element);
  }
  if (left.kind === "pair" && right.kind === "pair") {
    return (
      sameConstantType(left.first, right.first) &&
      sameConstantType(left.second, right.second)
    );
  }
  return true;
};

const constantParts = (
  value: MidgardCekDirectValueWitnessV1,
): ConstantPartsV1 | null => {
  if (value.kind === "constant") {
    const decoded = decodeMidgardCekConstantWitnessV1(value.witness);
    const tree = commitMidgardCekDataTreeV1(decoded.payload);
    return {
      type: decoded.type,
      payload: {
        root: tree.root,
        cborLength: tree.cborLength,
        memory: tree.memory,
      },
      memory: midgardCekConstantMemorySizeV1(
        decoded.type,
        decoded.payload,
      ),
    };
  }
  if (value.kind === "semanticConstant") {
    return {
      type: decodeMidgardCekConstantTypeCborV1(value.witness.typeCbor),
      payload: value.witness.payload,
      memory: value.witness.memory,
    };
  }
  return null;
};

const sameDataSummary = (
  left: DataSummaryV1,
  right: DataSummaryV1,
): boolean =>
  sameBytes(left.root, right.root) &&
  left.cborLength === right.cborLength &&
  left.memory === right.memory;

const resultMatchesParts = (
  result: MidgardCekDirectValueWitnessV1,
  type: MidgardCekConstantTypeV1,
  payload: DataSummaryV1,
  memory: bigint,
): boolean => {
  const actual = constantParts(result);
  return (
    actual !== null &&
    sameConstantType(actual.type, type) &&
    sameDataSummary(actual.payload, payload) &&
    actual.memory === memory
  );
};

const semanticSummary = (value: Data): DataSummaryV1 => {
  const tree = commitMidgardCekDataTreeV1(value);
  return {
    root: tree.root,
    cborLength: tree.cborLength,
    memory: tree.memory,
  };
};

const emptyListSequence = (): DataSequenceSummaryV1 => ({
  root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  length: 0n,
  payloadCborLength: 0n,
  memory: 0n,
});

const prependListSequence = (
  head: DataSummaryV1,
  tail: DataSequenceSummaryV1,
): DataSequenceSummaryV1 => {
  const node: MidgardCekDataListNodeV1 = {
    head: head.root,
    headCborLength: head.cborLength,
    headMemory: head.memory,
    tail: tail.root,
    length: tail.length + 1n,
    payloadCborLength: head.cborLength + tail.payloadCborLength,
    memory: head.memory + tail.memory,
  };
  return {
    root: hashMidgardCekDataListNodeV1(node),
    length: node.length,
    payloadCborLength: node.payloadCborLength,
    memory: node.memory,
  };
};

const listDataSummary = (
  sequence: DataSequenceSummaryV1,
): DataSummaryV1 => {
  const node: MidgardCekDataNodeV1 = {
    kind: "list",
    itemsCount: sequence.length,
    itemsRoot: sequence.root,
    cborLength: midgardCekDataListCborLengthV1(
      sequence.length,
      sequence.payloadCborLength,
    ),
    memory: 4n + sequence.memory,
  };
  return dataNodeSummary(node);
};

const constrDataSummary = (
  constructor: bigint,
  fields: DataSequenceSummaryV1,
): DataSummaryV1 => {
  if (constructor < 0n) {
    throw new Error("CEK Data constructor cannot be negative");
  }
  const cborLength = midgardCekDataConstrCborLengthV1(
    constructor,
    fields.length,
    fields.payloadCborLength,
  );
  const memory = 4n + fields.memory;
  const node: MidgardCekDataNodeV1 =
    constructor <= 127n
      ? {
          kind: "constrSmall",
          constructor,
          fieldsCount: fields.length,
          fieldsRoot: fields.root,
          cborLength,
          memory,
        }
      : {
          kind: "constrLarge",
          constructorCborRoot: commitMidgardCekBlobV1(
            encodeMidgardCekPlutusDataV1(new DataI(constructor)),
          ).root,
          constructorCborLength: BigInt(
            encodeMidgardCekPlutusDataV1(new DataI(constructor)).length,
          ),
          constructorMemory:
            4n + midgardCekIntegerMemorySizeV1(constructor),
          fieldsCount: fields.length,
          fieldsRoot: fields.root,
          cborLength,
          memory,
        };
  return dataNodeSummary(node);
};

const pairDataSummary = (
  first: DataSummaryV1,
  second: DataSummaryV1,
): DataSummaryV1 =>
  constrDataSummary(
    0n,
    prependListSequence(
      first,
      prependListSequence(second, emptyListSequence()),
    ),
  );

const dataNodeTopologyMatches = (
  node: MidgardCekDataNodeV1,
  listNode: MidgardCekDataListNodeV1 | null,
  pairNode: MidgardCekDataPairNodeV1 | null,
): boolean => {
  if (
    node.cborLength < 0n ||
    node.memory < 0n ||
    (listNode !== null && pairNode !== null)
  ) {
    return false;
  }
  if (node.kind === "constrSmall" || node.kind === "constrLarge") {
    const sequence: DataSequenceSummaryV1 = {
      root: node.fieldsRoot,
      length: node.fieldsCount,
      payloadCborLength: listNode?.payloadCborLength ?? 0n,
      memory: listNode?.memory ?? 0n,
    };
    return (
      pairNode === null &&
      dataListSummaryMatches(sequence, listNode) &&
      (node.kind === "constrSmall"
        ? node.constructor >= 0n &&
          node.constructor <= 127n &&
          node.cborLength ===
            midgardCekDataConstrCborLengthV1(
              node.constructor,
              sequence.length,
              sequence.payloadCborLength,
            )
        : node.constructorCborRoot.length === 32 &&
          node.constructorCborLength > 0n &&
          node.constructorMemory >= 5n &&
          node.cborLength ===
            3n +
              node.constructorCborLength +
              (sequence.length === 0n
                ? 1n
                : 2n + sequence.payloadCborLength)) &&
      node.memory === 4n + sequence.memory
    );
  }
  if (node.kind === "list") {
    const sequence = listSequenceFromNode(node);
    return (
      sequence !== null &&
      pairNode === null &&
      dataListSummaryMatches(sequence, listNode) &&
      node.cborLength ===
        midgardCekDataListCborLengthV1(
          sequence.length,
          sequence.payloadCborLength,
        ) &&
      node.memory === 4n + sequence.memory
    );
  }
  if (node.kind === "map") {
    const sequence = mapSequenceFromNode(node);
    return (
      sequence !== null &&
      listNode === null &&
      dataPairSummaryMatches(sequence, pairNode) &&
      node.cborLength ===
        midgardCekDataMapCborLengthV1(
          sequence.length,
          sequence.payloadCborLength,
        ) &&
      node.memory === 4n + sequence.memory
    );
  }
  if (listNode !== null || pairNode !== null) return false;
  if (node.kind === "bytes") {
    return (
      node.bytesRoot.length === 32 &&
      node.bytesLength >= 0n &&
      node.cborLength ===
        midgardCekDataBytesCborLengthV1(node.bytesLength) &&
      node.memory ===
        4n + (node.bytesLength === 0n ? 1n : node.bytesLength)
    );
  }
  return (
    node.kind === "integer" &&
    node.cborRoot.length === 32 &&
    node.cborLength > 0n &&
    node.memory >= 5n
  );
};

const exactTopMaterial = (
  summary: DataSummaryV1,
  node: MidgardCekDataNodeV1,
  lists: readonly MidgardCekDataListNodeV1[],
  pairs: readonly MidgardCekDataPairNodeV1[],
): boolean => {
  const needsList =
    (node.kind === "constrSmall" || node.kind === "constrLarge")
      ? node.fieldsCount > 0n
      : node.kind === "list"
        ? node.itemsCount > 0n
        : false;
  const needsPair = node.kind === "map" && node.entriesCount > 0n;
  if (
    lists.length !== (needsList ? 1 : 0) ||
    pairs.length !== (needsPair ? 1 : 0)
  ) {
    return false;
  }
  return (
    sameDataSummary(summary, dataNodeSummary(node)) &&
    dataNodeTopologyMatches(
      node,
      lists[0] ?? null,
      pairs[0] ?? null,
    )
  );
};

const constantMemoryFromPayloadNode = (
  type: MidgardCekConstantTypeV1,
  node: MidgardCekDataNodeV1,
): bigint | null => {
  if (type.kind === "integer" || type.kind === "bytes") {
    return node.memory - 4n;
  }
  if (type.kind === "list" && node.kind === "list") {
    if (type.element.kind === "data") return node.memory - 4n;
    if (type.element.kind === "integer") {
      return node.memory - 4n - node.itemsCount * 4n;
    }
    if (
      type.element.kind === "pair" &&
      type.element.first.kind === "data" &&
      type.element.second.kind === "data"
    ) {
      return node.memory - 4n - node.itemsCount * 4n;
    }
    return null;
  }
  if (type.kind === "pair") {
    if (
      type.first.kind === "data" &&
      type.second.kind === "data"
    ) {
      return node.memory - 4n;
    }
    if (
      type.first.kind === "integer" &&
      type.second.kind === "list" &&
      type.second.element.kind === "data"
    ) {
      return node.memory - 12n;
    }
    return null;
  }
  return type.kind === "data" ? node.memory : null;
};

const canonicalIntegerLeaf = (
  summary: DataSummaryV1,
  node: MidgardCekDataNodeV1,
  raw: Bytes,
): bigint | null => {
  if (
    node.kind !== "integer" ||
    raw.length === 0 ||
    raw.length > 9_215 ||
    !sameBytes(node.cborRoot, commitMidgardCekBlobV1(raw).root) ||
    node.cborLength !== BigInt(raw.length) ||
    !sameDataSummary(summary, dataNodeSummary(node))
  ) {
    return null;
  }
  const decoded = dataFromCbor(raw);
  return decoded instanceof DataI &&
    sameBytes(encodeMidgardCekPlutusDataV1(decoded), raw)
    ? decoded.int
    : null;
};

const canonicalBytesLeaf = (
  summary: DataSummaryV1,
  node: MidgardCekDataNodeV1,
  raw: Bytes,
): boolean =>
  node.kind === "bytes" &&
  raw.length <= 9_215 &&
  sameBytes(node.bytesRoot, commitMidgardCekBlobV1(raw).root) &&
  node.bytesLength === BigInt(raw.length) &&
  sameDataSummary(summary, dataNodeSummary(node));

const directUnit = (
  value: MidgardCekDirectValueWitnessV1,
): boolean => {
  if (value.kind !== "constant") return false;
  const decoded = decodeMidgardCekConstantWitnessV1(value.witness);
  return (
    decoded.type.kind === "unit" &&
    decoded.payload instanceof DataConstr &&
    decoded.payload.constr === 0n &&
    decoded.payload.fields.length === 0
  );
};

const verifySemanticPair = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  result: MidgardCekDirectValueWitnessV1,
  material: MidgardCekSemanticBuiltinWitnessV1,
): boolean => {
  if (
    arguments_.length !== 1 ||
    material.dataNodes.length !== 3 ||
    material.listNodes.length !== 2 ||
    material.pairNodes.length !== 0 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  const parts = constantParts(arguments_[0]!);
  const [payload, firstNode, secondNode] = material.dataNodes;
  const [firstLink, secondLink] = material.listNodes;
  if (
    parts === null ||
    parts.type.kind !== "pair" ||
    payload?.kind !== "constrSmall" ||
    payload.constructor !== 0n ||
    payload.fieldsCount !== 2n ||
    firstNode === undefined ||
    secondNode === undefined ||
    firstLink === undefined ||
    secondLink === undefined ||
    !exactTopMaterial(parts.payload, payload, [firstLink], []) ||
    !dataListLinkMatches(firstLink, firstNode, secondLink) ||
    !dataListLinkMatches(secondLink, secondNode, null)
  ) {
    return false;
  }
  let firstMemory: bigint | null;
  let secondMemory: bigint | null;
  if (arguments_[0]?.kind === "constant") {
    const decoded = decodeMidgardCekConstantWitnessV1(
      arguments_[0].witness,
    );
    if (
      !(decoded.payload instanceof DataConstr) ||
      decoded.payload.constr !== 0n ||
      decoded.payload.fields.length !== 2 ||
      !sameDataSummary(
        semanticSummary(decoded.payload.fields[0]!),
        dataNodeSummary(firstNode),
      ) ||
      !sameDataSummary(
        semanticSummary(decoded.payload.fields[1]!),
        dataNodeSummary(secondNode),
      )
    ) {
      return false;
    }
    firstMemory = midgardCekConstantMemorySizeV1(
      parts.type.first,
      decoded.payload.fields[0]!,
    );
    secondMemory = midgardCekConstantMemorySizeV1(
      parts.type.second,
      decoded.payload.fields[1]!,
    );
  } else {
    firstMemory = constantMemoryFromPayloadNode(
      parts.type.first,
      firstNode,
    );
    secondMemory = constantMemoryFromPayloadNode(
      parts.type.second,
      secondNode,
    );
  }
  if (
    firstMemory === null ||
    secondMemory === null ||
    parts.memory !== firstMemory + secondMemory
  ) {
    return false;
  }
  return tag === 29n
    ? resultMatchesParts(
        result,
        parts.type.first,
        dataNodeSummary(firstNode),
        firstMemory,
      )
    : tag === 30n &&
        resultMatchesParts(
          result,
          parts.type.second,
          dataNodeSummary(secondNode),
          secondMemory,
        );
};

const semanticListSource = (
  source: MidgardCekDirectValueWitnessV1,
  node: MidgardCekDataNodeV1,
  lists: readonly MidgardCekDataListNodeV1[],
): {
  readonly element: MidgardCekConstantTypeV1;
  readonly sequence: DataSequenceSummaryV1;
  readonly memory: bigint;
} | null => {
  const parts = constantParts(source);
  const sequence = listSequenceFromNode(node);
  if (
    parts === null ||
    parts.type.kind !== "list" ||
    sequence === null ||
    !exactTopMaterial(parts.payload, node, lists, []) ||
    (source.kind !== "constant" &&
      parts.memory !== constantMemoryFromPayloadNode(parts.type, node))
  ) {
    return null;
  }
  return {
    element: parts.type.element,
    sequence,
    memory: parts.memory,
  };
};

const verifySemanticList = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  result: MidgardCekDirectValueWitnessV1,
  material: MidgardCekSemanticBuiltinWitnessV1,
): boolean => {
  if (
    material.pairNodes.length !== 0 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  if (tag === 31n || tag === 35n) {
    if (material.dataNodes.length !== 1) return false;
    const node = material.dataNodes[0]!;
    const expectedListCount =
      node.kind === "list" && node.itemsCount > 0n ? 1 : 0;
    if (material.listNodes.length !== expectedListCount) return false;
    const source = semanticListSource(
      arguments_[0]!,
      node,
      material.listNodes,
    );
    if (source === null) return false;
    if (tag === 31n) {
      if (arguments_.length !== 3) return false;
      const selected =
        source.sequence.length === 0n
          ? arguments_[1]!
          : arguments_[2]!;
      return sameBytes(
        hashMidgardCekDirectValueWitnessV1(result),
        hashMidgardCekDirectValueWitnessV1(selected),
      );
    }
    return (
      arguments_.length === 1 &&
      resultMatchesParts(
        result,
        { kind: "boolean" },
        semanticSummary(
          new DataConstr(source.sequence.length === 0n ? 1n : 0n, []),
        ),
        1n,
      )
    );
  }
  if (tag === 32n) {
    if (arguments_.length !== 2 || material.dataNodes.length !== 1) {
      return false;
    }
    const node = material.dataNodes[0]!;
    const expectedListCount =
      node.kind === "list" && node.itemsCount > 0n ? 1 : 0;
    if (material.listNodes.length !== expectedListCount) return false;
    const source = semanticListSource(
      arguments_[1]!,
      node,
      material.listNodes,
    );
    const item = constantParts(arguments_[0]!);
    if (
      source === null ||
      item === null ||
      !sameConstantType(item.type, source.element)
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: "list", element: source.element },
      listDataSummary(
        prependListSequence(item.payload, source.sequence),
      ),
      item.memory + source.memory,
    );
  }
  if (
    (tag !== 33n && tag !== 34n) ||
    arguments_.length !== 1 ||
    material.dataNodes.length !== 2 ||
    (material.listNodes.length !== 1 &&
      material.listNodes.length !== 2)
  ) {
    return false;
  }
  const [sourceNode, headNode] = material.dataNodes;
  const [firstLink, tailLink] = material.listNodes;
  if (
    sourceNode === undefined ||
    headNode === undefined ||
    firstLink === undefined ||
    firstLink.length <= 0n ||
    (firstLink.length === 1n) !== (tailLink === undefined)
  ) {
    return false;
  }
  const source = semanticListSource(
    arguments_[0]!,
    sourceNode,
    [firstLink],
  );
  let headMemory: bigint | null;
  if (arguments_[0]?.kind === "constant" && source !== null) {
    const decoded = decodeMidgardCekConstantWitnessV1(
      arguments_[0].witness,
    );
    if (
      !(decoded.payload instanceof DataList) ||
      decoded.payload.list.length === 0 ||
      !sameDataSummary(
        semanticSummary(decoded.payload.list[0]!),
        dataNodeSummary(headNode),
      )
    ) {
      return false;
    }
    headMemory = midgardCekConstantMemorySizeV1(
      source.element,
      decoded.payload.list[0]!,
    );
  } else {
    headMemory = constantMemoryFromPayloadNode(
      source?.element ?? { kind: "data" },
      headNode,
    );
  }
  if (
    source === null ||
    headMemory === null ||
    firstLink.length !== source.sequence.length ||
    !dataListLinkMatches(firstLink, headNode, tailLink ?? null)
  ) {
    return false;
  }
  if (tag === 33n) {
    return resultMatchesParts(
      result,
      source.element,
      dataNodeSummary(headNode),
      headMemory,
    );
  }
  const tail: DataSequenceSummaryV1 = {
    root: firstLink.tail,
    length: firstLink.length - 1n,
    payloadCborLength:
      firstLink.payloadCborLength - firstLink.headCborLength,
    memory: firstLink.memory - firstLink.headMemory,
  };
  return resultMatchesParts(
    result,
    { kind: "list", element: source.element },
    listDataSummary(tail),
    source.memory - headMemory,
  );
};

const verifySemanticChooseData = (
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  result: MidgardCekDirectValueWitnessV1,
  material: MidgardCekSemanticBuiltinWitnessV1,
): boolean => {
  if (
    arguments_.length !== 6 ||
    material.dataNodes.length !== 1 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  const source = constantParts(arguments_[0]!);
  const node = material.dataNodes[0]!;
  if (
    source === null ||
    source.type.kind !== "data" ||
    source.memory !== source.payload.memory ||
    !exactTopMaterial(
      source.payload,
      node,
      material.listNodes,
      material.pairNodes,
    )
  ) {
    return false;
  }
  const selected =
    node.kind === "constrSmall" || node.kind === "constrLarge"
      ? arguments_[1]!
      : node.kind === "map"
        ? arguments_[2]!
        : node.kind === "list"
          ? arguments_[3]!
          : node.kind === "integer"
            ? arguments_[4]!
            : arguments_[5]!;
  return sameBytes(
    hashMidgardCekDirectValueWitnessV1(result),
    hashMidgardCekDirectValueWitnessV1(selected),
  );
};

const verifySemanticData = (
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  result: MidgardCekDirectValueWitnessV1,
  material: MidgardCekSemanticBuiltinWitnessV1,
): boolean => {
  if (tag === 47n) {
    if (
      arguments_.length !== 2 ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const left = constantParts(arguments_[0]!);
    const right = constantParts(arguments_[1]!);
    if (
      left?.type.kind !== "data" ||
      right?.type.kind !== "data" ||
      left.memory !== left.payload.memory ||
      right.memory !== right.payload.memory
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: "boolean" },
      semanticSummary(
        new DataConstr(
          sameDataSummary(left.payload, right.payload) ? 1n : 0n,
          [],
        ),
      ),
      1n,
    );
  }
  if (tag === 48n) {
    if (
      arguments_.length !== 2 ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const first = constantParts(arguments_[0]!);
    const second = constantParts(arguments_[1]!);
    if (
      first?.type.kind !== "data" ||
      second?.type.kind !== "data" ||
      first.memory !== first.payload.memory ||
      second.memory !== second.payload.memory
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      {
        kind: "pair",
        first: { kind: "data" },
        second: { kind: "data" },
      },
      pairDataSummary(first.payload, second.payload),
      first.memory + second.memory,
    );
  }
  if (tag === 49n || tag === 50n) {
    if (
      arguments_.length !== 1 ||
      !directUnit(arguments_[0]!) ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const element: MidgardCekConstantTypeV1 =
      tag === 49n
        ? { kind: "data" }
        : {
            kind: "pair",
            first: { kind: "data" },
            second: { kind: "data" },
          };
    return resultMatchesParts(
      result,
      { kind: "list", element },
      listDataSummary(emptyListSequence()),
      0n,
    );
  }
  if (tag === 51n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 0 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    const raw = material.scalarPreimages[0]!;
    if (
      source?.type.kind !== "data" ||
      source.memory !== source.payload.memory ||
      raw.length === 0 ||
      raw.length > 9_215
    ) {
      return false;
    }
    const decoded = dataFromCbor(raw);
    if (
      !sameBytes(encodeMidgardCekPlutusDataV1(decoded), raw)
    ) {
      return false;
    }
    const tree = commitMidgardCekDataTreeV1(decoded);
    if (
      !sameDataSummary(source.payload, {
        root: tree.root,
        cborLength: tree.cborLength,
        memory: tree.memory,
      })
    ) {
      return false;
    }
    const payload = semanticSummary(new DataB(raw));
    return resultMatchesParts(
      result,
      { kind: "bytes" },
      payload,
      BigInt(Math.max(1, raw.length)),
    );
  }
  if (tag === 37n || tag === 39n) {
    const fieldsArgument = arguments_[tag === 37n ? 1 : 0];
    const fieldsNode =
      material.dataNodes[
        tag === 37n &&
        arguments_[0]?.kind === "semanticConstant"
          ? 1
          : 0
      ];
    if (
      fieldsArgument === undefined ||
      fieldsNode === undefined ||
      material.pairNodes.length !== 0
    ) {
      return false;
    }
    const fields = semanticListSource(
      fieldsArgument,
      fieldsNode,
      material.listNodes,
    );
    if (fields === null || fields.element.kind !== "data") return false;
    if (tag === 39n) {
      return (
        arguments_.length === 1 &&
        material.scalarPreimages.length === 0 &&
        resultMatchesParts(
          result,
          { kind: "data" },
          listDataSummary(fields.sequence),
          4n + fields.sequence.memory,
        )
      );
    }
    if (arguments_.length !== 2) return false;
    let constructor: bigint | null = null;
    if (arguments_[0]?.kind === "constant") {
      const index = decodeMidgardCekConstantWitnessV1(
        arguments_[0].witness,
      );
      if (
        index.type.kind === "integer" &&
        index.payload instanceof DataI &&
        material.dataNodes.length === 1 &&
        material.scalarPreimages.length === 0
      ) {
        constructor = index.payload.int;
      }
    } else if (
      arguments_[0]?.kind === "semanticConstant" &&
      material.dataNodes.length === 2 &&
      material.scalarPreimages.length === 1
    ) {
      const index = constantParts(arguments_[0]);
      if (index?.type.kind === "integer") {
        constructor = canonicalIntegerLeaf(
          index.payload,
          material.dataNodes[0]!,
          material.scalarPreimages[0]!,
        );
      }
    }
    if (constructor === null || constructor < 0n) return false;
    const summary = constrDataSummary(constructor, fields.sequence);
    return resultMatchesParts(
      result,
      { kind: "data" },
      summary,
      summary.memory,
    );
  }
  if (tag === 40n || tag === 45n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    if (
      source === null ||
      source.type.kind !== (tag === 40n ? "integer" : "data")
    ) {
      return false;
    }
    const integer = canonicalIntegerLeaf(
      source.payload,
      material.dataNodes[0]!,
      material.scalarPreimages[0]!,
    );
    if (
      integer === null ||
      (tag === 40n
        ? source.memory !== source.payload.memory - 4n
        : source.memory !== source.payload.memory)
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: tag === 40n ? "data" : "integer" },
      source.payload,
      tag === 40n ? source.payload.memory : source.payload.memory - 4n,
    );
  }
  if (tag === 41n || tag === 46n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.listNodes.length !== 0 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    if (
      source === null ||
      source.type.kind !== (tag === 41n ? "bytes" : "data") ||
      !canonicalBytesLeaf(
        source.payload,
        material.dataNodes[0]!,
        material.scalarPreimages[0]!,
      ) ||
      (tag === 41n
        ? source.memory !== source.payload.memory - 4n
        : source.memory !== source.payload.memory)
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: tag === 41n ? "data" : "bytes" },
      source.payload,
      tag === 41n ? source.payload.memory : source.payload.memory - 4n,
    );
  }
  if (tag === 44n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length !== 0
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    const node = material.dataNodes[0]!;
    const sequence = listSequenceFromNode(node);
    if (
      source?.type.kind !== "data" ||
      source.memory !== source.payload.memory ||
      sequence === null ||
      !exactTopMaterial(
        source.payload,
        node,
        material.listNodes,
        [],
      )
    ) {
      return false;
    }
    return resultMatchesParts(
      result,
      { kind: "list", element: { kind: "data" } },
      source.payload,
      sequence.memory,
    );
  }
  if (tag === 42n) {
    if (
      arguments_.length !== 1 ||
      material.dataNodes.length !== 1 ||
      material.pairNodes.length !== 0 ||
      material.scalarPreimages.length > 1
    ) {
      return false;
    }
    const source = constantParts(arguments_[0]!);
    const node = material.dataNodes[0]!;
    if (
      source?.type.kind !== "data" ||
      source.memory !== source.payload.memory ||
      !exactTopMaterial(
        source.payload,
        node,
        material.listNodes,
        [],
      )
    ) {
      return false;
    }
    let constructor: bigint | null = null;
    if (
      node.kind === "constrSmall" &&
      material.scalarPreimages.length === 0
    ) {
      constructor = node.constructor;
    } else if (
      node.kind === "constrLarge" &&
      material.scalarPreimages.length === 1
    ) {
      const raw = material.scalarPreimages[0]!;
      if (
        raw.length <= 9_215 &&
        sameBytes(
          node.constructorCborRoot,
          commitMidgardCekBlobV1(raw).root,
        ) &&
        node.constructorCborLength === BigInt(raw.length)
      ) {
        const decoded = dataFromCbor(raw);
        if (
          decoded instanceof DataI &&
          decoded.int > 127n &&
          sameBytes(encodeMidgardCekPlutusDataV1(decoded), raw)
        ) {
          constructor = decoded.int;
        }
      }
    }
    if (
      constructor === null ||
      (node.kind !== "constrSmall" && node.kind !== "constrLarge")
    ) {
      return false;
    }
    const fields: DataSequenceSummaryV1 = {
      root: node.fieldsRoot,
      length: node.fieldsCount,
      payloadCborLength: material.listNodes[0]?.payloadCborLength ?? 0n,
      memory: material.listNodes[0]?.memory ?? 0n,
    };
    const constructorSummary = semanticSummary(new DataI(constructor));
    const fieldsSummary = listDataSummary(fields);
    return resultMatchesParts(
      result,
      {
        kind: "pair",
        first: { kind: "integer" },
        second: {
          kind: "list",
          element: { kind: "data" },
        },
      },
      pairDataSummary(constructorSummary, fieldsSummary),
      constructorSummary.memory - 4n + fields.memory,
    );
  }
  return false;
};

const verifySemanticBuiltin = (
  tag: bigint,
  pre: MidgardCekMachineStateV1,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  result: MidgardCekDirectValueWitnessV1,
  material: MidgardCekSemanticBuiltinWitnessV1,
): boolean => {
  if (!builtinRootMatches(pre, tag, arguments_)) return false;
  if (tag === 29n || tag === 30n) {
    return verifySemanticPair(tag, arguments_, result, material);
  }
  if (tag >= 31n && tag <= 35n) {
    return verifySemanticList(tag, arguments_, result, material);
  }
  if (tag === 36n) {
    return verifySemanticChooseData(arguments_, result, material);
  }
  return (
    tag >= 37n &&
    tag <= 51n &&
    tag !== 38n &&
    tag !== 43n &&
    verifySemanticData(tag, arguments_, result, material)
  );
};

const verifySemanticBuiltinFailure = (
  tag: bigint,
  pre: MidgardCekMachineStateV1,
  arguments_: readonly MidgardCekDirectValueWitnessV1[],
  material: MidgardCekSemanticBuiltinWitnessV1,
): boolean => {
  if (
    !builtinRootMatches(pre, tag, arguments_) ||
    arguments_.length !== 1 ||
    material.dataNodes.length !== 1 ||
    material.scalarPreimages.length !== 0
  ) {
    return false;
  }
  const source = constantParts(arguments_[0]!);
  const node = material.dataNodes[0]!;
  if (source === null) return false;
  if (tag === 33n || tag === 34n) {
    const list = semanticListSource(
      arguments_[0]!,
      node,
      material.listNodes,
    );
    return material.pairNodes.length === 0 && list?.sequence.length === 0n;
  }
  if (
    tag < 42n ||
    tag > 46n ||
    source.type.kind !== "data" ||
    source.memory !== source.payload.memory ||
    !exactTopMaterial(
      source.payload,
      node,
      material.listNodes,
      material.pairNodes,
    )
  ) {
    return false;
  }
  if (tag === 42n) {
    return node.kind !== "constrSmall" && node.kind !== "constrLarge";
  }
  if (tag === 43n) return node.kind !== "map";
  if (tag === 44n) return node.kind !== "list";
  if (tag === 45n) return node.kind !== "integer";
  return node.kind !== "bytes";
};

const verifyBuiltin = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  if (witness.kind === "startBuiltinMapConversion") {
    return verifyMapConversionStart(pre, post, witness);
  }
  if (witness.kind === "executeBuiltinSemantic") {
    if (
      !verifySemanticBuiltin(
        witness.tag,
        pre,
        witness.arguments,
        witness.result,
        witness.material,
      )
    ) {
      return false;
    }
    const budget = midgardCekDirectBuiltinBudgetV1(
      witness.tag,
      witness.arguments,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitnessV1(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinSemanticFailure") {
    return (
      verifySemanticBuiltinFailure(
        witness.tag,
        pre,
        witness.arguments,
        witness.material,
      ) &&
      sameState(
        post,
        exactState(pre, {
          mode: "haltError",
          focusRoot: hashMidgardCekTermNodeV1({ kind: "error" }),
          environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
          continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
          auxiliary: MidgardCekErrorCodes.BuiltinFailure,
        }),
      )
    );
  }
  if (witness.kind === "executeBuiltinTypeFailure") {
    return (
      verifyMidgardCekBuiltinTypeFailureV1(
        witness.tag,
        pre.focusRoot,
        witness.arguments,
      ) &&
      sameState(
        post,
        errorSuccessor(pre, MidgardCekErrorCodes.BuiltinFailure),
      )
    );
  }
  if (witness.kind === "executeBuiltinBlsFinal") {
    if (
      !verifyMidgardCekBlsFinalV1(
        pre.focusRoot,
        witness.leftRoot,
        witness.rightRoot,
        witness.leftExpression,
        witness.rightExpression,
        witness.result,
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekBlsFinalV1(
      witness.leftRoot,
      witness.rightRoot,
      witness.leftExpression,
      witness.rightExpression,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitnessV1(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: evaluated.budget.cpu,
        memoryDelta: evaluated.budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinDirect") {
    if (
      !verifyMidgardCekDirectBuiltinV1(
        witness.tag,
        pre.focusRoot,
        witness.arguments,
        witness.result,
      )
    ) {
      return false;
    }
    const budget = midgardCekDirectBuiltinBudgetV1(
      witness.tag,
      witness.arguments,
    );
    return sameState(
      post,
      exactState(pre, {
        mode: "return",
        focusRoot: hashMidgardCekDirectValueWitnessV1(witness.result),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: pre.continuationRoot,
        auxiliary: 0n,
        cpuDelta: budget.cpu,
        memoryDelta: budget.memory,
      }),
    );
  }
  if (witness.kind === "executeBuiltinFailure") {
    if (
      !verifyMidgardCekDirectBuiltinFailureV1(
        witness.tag,
        pre.focusRoot,
        witness.arguments,
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekDirectBuiltinV1(
      witness.tag,
      witness.arguments,
    );
    if (evaluated.kind !== "failure") return false;
    return sameState(
      post,
      exactState(pre, {
        mode: "haltError",
        focusRoot: hashMidgardCekTermNodeV1({ kind: "error" }),
        environmentRoot: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
        continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
        auxiliary: MidgardCekErrorCodes.BuiltinFailure,
        cpuDelta: evaluated.budget.cpu,
        memoryDelta: evaluated.budget.memory,
      }),
    );
  }
  return false;
};

/**
 * Mirrors the Aiken structural CEK one-step verifier. The authenticated
 * zero-cost runtime-type failures and direct semantic success/failure rules
 * are active. BLS final verification retains its dedicated expression
 * witness path and remains separate from the direct evaluator.
 */
export const verifyMidgardCekCoreStepV1 = (
  pre: MidgardCekMachineStateV1,
  post: MidgardCekMachineStateV1,
  witness: MidgardCekCoreStepWitnessV1,
): boolean => {
  try {
    // Hashing validates every state field's exact width and numeric range.
    hashMidgardCekMachineStateV1(pre);
    hashMidgardCekMachineStateV1(post);
    if (pre.executionIndex !== post.executionIndex) return false;
    switch (pre.mode) {
      case "compute":
        return verifyCompute(pre, post, witness);
      case "lookup":
        return verifyLookup(pre, post, witness);
      case "return":
        return verifyReturn(pre, post, witness);
      case "caseSelect":
        return verifyCaseSelect(pre, post, witness);
      case "caseApply":
        return verifyCaseApply(pre, post, witness);
      case "builtin":
        return verifyBuiltin(pre, post, witness);
      case "semanticBuiltin":
        return verifySemanticBuiltinControl(pre, post, witness);
      case "haltSuccess":
      case "haltError":
        return false;
    }
  } catch {
    return false;
  }
};
