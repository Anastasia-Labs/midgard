import {
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  type MidgardCekValueNode,
} from "@al-ft/midgard-core";

import {
  type MidgardCekBlsExpressionWitness,
  type MidgardCekDirectValueWitness,
  type MidgardCekRuntimeValueWitness,
} from "./cek-builtin.js";
import { type MidgardCekConstantWitness } from "./cek-constant.js";

export const MACHINE_STEP_CPU = 16_000n;

export const MACHINE_STEP_MEMORY = 100n;

export const UINT32_MAX = 0xffff_ffffn;

export const MAP_CONVERSION_CONTROL_DOMAIN = Buffer.from(
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

export type Bytes = Uint8Array;

export type MidgardCekEnvironmentSummary =
  | { readonly kind: "empty" }
  | {
      readonly kind: "nonempty";
      readonly value: Bytes;
      readonly tail: Bytes;
      readonly length: bigint;
    };

export type MidgardCekSemanticBuiltinWitness = {
  readonly dataNodes: readonly MidgardCekDataNode[];
  readonly listNodes: readonly MidgardCekDataListNode[];
  readonly pairNodes: readonly MidgardCekDataPairNode[];
  readonly scalarPreimages: readonly Bytes[];
};

export type MidgardCekMapConversionControl = {
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

export type MidgardCekMapConversionStartWitness = {
  readonly sourceNode: MidgardCekDataNode;
  readonly sourceList: MidgardCekDataListNode | null;
  readonly sourcePairs: MidgardCekDataPairNode | null;
  readonly resultNode: MidgardCekDataNode;
  readonly resultList: MidgardCekDataListNode | null;
  readonly resultPairs: MidgardCekDataPairNode | null;
};

export type MidgardCekCoreStepWitness =
  | { readonly kind: "computeVariable"; readonly index: bigint }
  | {
      readonly kind: "computeConstant";
      readonly value: MidgardCekConstantWitness;
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
      readonly value: MidgardCekValueNode;
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
      readonly closureSummary: MidgardCekEnvironmentSummary;
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
      readonly function: MidgardCekValueNode;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "returnApplyValueLambda";
      readonly argument: Bytes;
      readonly body: Bytes;
      readonly closureEnvironment: Bytes;
      readonly closureSummary: MidgardCekEnvironmentSummary;
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
      readonly function: MidgardCekValueNode;
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
      readonly value: MidgardCekValueNode;
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
      readonly value: MidgardCekValueNode;
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
      readonly arguments: readonly MidgardCekRuntimeValueWitness[];
    }
  | {
      readonly kind: "executeBuiltinDirect";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitness[];
      readonly result: MidgardCekDirectValueWitness;
    }
  | {
      readonly kind: "executeBuiltinSemantic";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitness[];
      readonly result: MidgardCekDirectValueWitness;
      readonly material: MidgardCekSemanticBuiltinWitness;
    }
  | {
      readonly kind: "startBuiltinMapConversion";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitness[];
      readonly result: MidgardCekDirectValueWitness;
      readonly material: MidgardCekMapConversionStartWitness;
    }
  | {
      readonly kind: "stepBuiltinListToMap";
      readonly control: MidgardCekMapConversionControl;
      readonly source: MidgardCekDataListNode;
      readonly pair: MidgardCekDataNode;
      readonly first: MidgardCekDataListNode;
      readonly second: MidgardCekDataListNode;
      readonly key: MidgardCekDataNode;
      readonly value: MidgardCekDataNode;
      readonly destination: MidgardCekDataPairNode;
    }
  | {
      readonly kind: "stepBuiltinMapToList";
      readonly control: MidgardCekMapConversionControl;
      readonly source: MidgardCekDataPairNode;
      readonly destination: MidgardCekDataListNode;
      readonly pair: MidgardCekDataNode;
      readonly first: MidgardCekDataListNode;
      readonly second: MidgardCekDataListNode;
      readonly key: MidgardCekDataNode;
      readonly value: MidgardCekDataNode;
    }
  | {
      readonly kind: "finishBuiltinMapConversion";
      readonly control: MidgardCekMapConversionControl;
    }
  | {
      readonly kind: "executeBuiltinSemanticFailure";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitness[];
      readonly material: MidgardCekSemanticBuiltinWitness;
    }
  | {
      readonly kind: "executeBuiltinFailure";
      readonly tag: bigint;
      readonly arguments: readonly MidgardCekDirectValueWitness[];
    }
  | {
      readonly kind: "executeBuiltinBlsFinal";
      readonly leftRoot: Bytes;
      readonly rightRoot: Bytes;
      readonly leftExpression: MidgardCekBlsExpressionWitness;
      readonly rightExpression: MidgardCekBlsExpressionWitness;
      readonly result: MidgardCekDirectValueWitness;
    }
  | { readonly kind: "computeContextConstant"; readonly valueRoot: Bytes };

export const sameBytes = (left: Bytes, right: Bytes): boolean =>
  Buffer.from(left).equals(Buffer.from(right));
