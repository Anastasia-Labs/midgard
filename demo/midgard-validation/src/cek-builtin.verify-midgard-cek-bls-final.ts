import {
  hashMidgardCekBlsExpressionNode,
  hashMidgardCekValueNode,
} from "@al-ft/midgard-core";
import { CEKConst, CEKError } from "@harmoniclabs/plutus-machine";

import {
  type Bytes,
  directArgumentsMatchKinds,
  directWitnessPayloadBytes,
  type MidgardCekDirectValueWitness,
  sameBytes,
} from "./cek-builtin.argument-kinds.js";
import {
  directConstantToReferenceValue,
  evaluateMidgardCekDirectBuiltin,
  referenceConstantToDirectWitness,
  runPinnedReferenceBuiltin,
} from "./cek-builtin.evaluate-reference-builtin.js";
import {
  hashMidgardCekDirectArguments,
  hashMidgardCekDirectValueWitness,
  midgardCekDirectBuiltinBudget,
} from "./cek-builtin.selected-control-result.js";
import {
  decodeMidgardCekConstantWitness,
  hashMidgardCekConstantWitness,
  MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES,
  type MidgardCekConstantWitness,
} from "./cek-constant.js";
import { type MidgardCekBuiltinBudget } from "./cek-cost.js";

export const verifyMidgardCekDirectBuiltin = (
  tag: bigint,
  builtinValueRoot: Bytes,
  arguments_: readonly MidgardCekDirectValueWitness[],
  result: MidgardCekDirectValueWitness,
): boolean => {
  // mapData (38) and unMapData (43) succeed only through the map-conversion
  // arm, so each map step has one successor.
  if (tag === 38n || tag === 43n) return false;
  try {
    if (
      directWitnessPayloadBytes([...arguments_, result]) >
      BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES)
    ) {
      return false;
    }
    const committed = hashMidgardCekDirectArguments(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNode({
          kind: "builtin",
          tag,
          forcesRemaining: 0n,
          argumentsCount: committed.count,
          argumentsRoot: committed.root,
        }),
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekDirectBuiltin(tag, arguments_);
    return (
      evaluated.kind === "success" &&
      sameBytes(
        hashMidgardCekDirectValueWitness(evaluated.result),
        hashMidgardCekDirectValueWitness(result),
      )
    );
  } catch {
    return false;
  }
};

export const verifyMidgardCekDirectBuiltinFailure = (
  tag: bigint,
  builtinValueRoot: Bytes,
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  try {
    if (
      directWitnessPayloadBytes(arguments_) >
      BigInt(MIDGARD_CEK_MAX_DIRECT_CONSTANT_PAYLOAD_BYTES)
    ) {
      return false;
    }
    const committed = hashMidgardCekDirectArguments(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNode({
          kind: "builtin",
          tag,
          forcesRemaining: 0n,
          argumentsCount: committed.count,
          argumentsRoot: committed.root,
        }),
      )
    ) {
      return false;
    }
    // A known failure applies only to well-typed arguments; an ill-typed
    // application fails through the type-failure arm instead.
    if (!directArgumentsMatchKinds(Number(tag), arguments_)) return false;
    return evaluateMidgardCekDirectBuiltin(tag, arguments_).kind === "failure";
  } catch {
    return false;
  }
};

export type MidgardCekBlsExpressionWitness =
  | {
      readonly kind: "millerLoop";
      readonly g1: MidgardCekConstantWitness;
      readonly g2: MidgardCekConstantWitness;
    }
  | {
      readonly kind: "multiply";
      readonly left: MidgardCekBlsExpressionWitness;
      readonly right: MidgardCekBlsExpressionWitness;
    };

type EvaluatedBlsExpression = {
  readonly root: Bytes;
  readonly value: CEKConst;
  readonly leaves: number;
  readonly depth: number;
};

const evaluateBlsLeaf = (
  expression: Extract<MidgardCekBlsExpressionWitness, { kind: "millerLoop" }>,
): EvaluatedBlsExpression => {
  const g1Decoded = decodeMidgardCekConstantWitness(expression.g1);
  const g2Decoded = decodeMidgardCekConstantWitness(expression.g2);
  if (g1Decoded.type.kind !== "blsG1" || g2Decoded.type.kind !== "blsG2") {
    throw new Error("BLS expression leaf requires G1 and G2 constants");
  }
  const g1 = directConstantToReferenceValue(expression.g1);
  const g2 = directConstantToReferenceValue(expression.g2);
  const value = runPinnedReferenceBuiltin(68, [g1, g2]);
  if (value instanceof CEKError) {
    throw new Error("reference evaluator rejected a BLS expression leaf");
  }
  return Object.freeze({
    root: hashMidgardCekBlsExpressionNode({
      kind: "millerLoop",
      g1Value: hashMidgardCekConstantWitness(expression.g1),
      g2Value: hashMidgardCekConstantWitness(expression.g2),
    }),
    value,
    leaves: 1,
    depth: 1,
  });
};

const evaluateBlsProduct = (
  left: EvaluatedBlsExpression,
  right: EvaluatedBlsExpression,
): EvaluatedBlsExpression => {
  const value = runPinnedReferenceBuiltin(69, [left.value, right.value]);
  if (value instanceof CEKError) {
    throw new Error("reference evaluator rejected a BLS expression product");
  }
  return Object.freeze({
    root: hashMidgardCekBlsExpressionNode({
      kind: "multiply",
      left: left.root,
      right: right.root,
    }),
    value,
    leaves: left.leaves + right.leaves,
    depth: Math.max(left.depth, right.depth) + 1,
  });
};

/**
 * Evaluates an expression left subtree first, then right, then the product,
 * so the first failure is the one a depth-first reading meets. A subexpression
 * object reached twice is evaluated once, and the walk keeps its own stack, so
 * shared subexpressions cost linear time and any depth is walked.
 */
const evaluateBlsExpression = (
  expression: MidgardCekBlsExpressionWitness,
): EvaluatedBlsExpression => {
  const evaluated = new Map<
    MidgardCekBlsExpressionWitness,
    EvaluatedBlsExpression
  >();
  const active = new Set<MidgardCekBlsExpressionWitness>();
  const work: {
    readonly expression: MidgardCekBlsExpressionWitness;
    readonly expanded: boolean;
  }[] = [{ expression, expanded: false }];
  while (work.length > 0) {
    const next = work.pop()!;
    const node = next.expression;
    if (next.expanded) {
      if (node.kind !== "multiply") {
        throw new Error("BLS expression walk expanded a leaf");
      }
      active.delete(node);
      evaluated.set(
        node,
        evaluateBlsProduct(
          evaluated.get(node.left)!,
          evaluated.get(node.right)!,
        ),
      );
    } else if (!evaluated.has(node)) {
      if (active.has(node)) {
        throw new Error("BLS expression witness is cyclic");
      }
      if (node.kind === "millerLoop") {
        evaluated.set(node, evaluateBlsLeaf(node));
      } else {
        active.add(node);
        work.push(
          { expression: node, expanded: true },
          { expression: node.right, expanded: false },
          { expression: node.left, expanded: false },
        );
      }
    }
  }
  return evaluated.get(expression)!;
};

export type MidgardCekBlsFinalEvaluation = {
  readonly leftRoot: Bytes;
  readonly rightRoot: Bytes;
  readonly result: MidgardCekDirectValueWitness;
  readonly budget: MidgardCekBuiltinBudget;
};

export const evaluateMidgardCekBlsFinal = (
  expectedLeftRoot: Bytes,
  expectedRightRoot: Bytes,
  leftExpression: MidgardCekBlsExpressionWitness,
  rightExpression: MidgardCekBlsExpressionWitness,
): MidgardCekBlsFinalEvaluation => {
  if (expectedLeftRoot.length !== 32 || expectedRightRoot.length !== 32) {
    throw new Error("BLS finalVerify expected roots must be bytes32");
  }
  const left = evaluateBlsExpression(leftExpression);
  const right = evaluateBlsExpression(rightExpression);
  if (
    !sameBytes(left.root, expectedLeftRoot) ||
    !sameBytes(right.root, expectedRightRoot)
  ) {
    throw new Error("BLS finalVerify expression root mismatch");
  }
  if (left.leaves + right.leaves > 10 || left.depth > 10 || right.depth > 10) {
    throw new Error(
      "BLS finalVerify expression exceeds the ten-leaf L1 proof reserve",
    );
  }
  const result = runPinnedReferenceBuiltin(70, [left.value, right.value]);
  if (result instanceof CEKError) {
    throw new Error("reference evaluator rejected BLS finalVerify");
  }
  const arguments_: readonly MidgardCekDirectValueWitness[] = [
    { kind: "blsMillerLoop", expressionRoot: left.root },
    { kind: "blsMillerLoop", expressionRoot: right.root },
  ];
  return Object.freeze({
    leftRoot: left.root,
    rightRoot: right.root,
    result: referenceConstantToDirectWitness(result, false),
    budget: midgardCekDirectBuiltinBudget(70n, arguments_),
  });
};

export const verifyMidgardCekBlsFinal = (
  builtinValueRoot: Bytes,
  expectedLeftRoot: Bytes,
  expectedRightRoot: Bytes,
  leftExpression: MidgardCekBlsExpressionWitness,
  rightExpression: MidgardCekBlsExpressionWitness,
  result: MidgardCekDirectValueWitness,
): boolean => {
  try {
    const arguments_: readonly MidgardCekDirectValueWitness[] = [
      {
        kind: "blsMillerLoop",
        expressionRoot: expectedLeftRoot,
      },
      {
        kind: "blsMillerLoop",
        expressionRoot: expectedRightRoot,
      },
    ];
    const committed = hashMidgardCekDirectArguments(arguments_);
    if (
      !sameBytes(
        builtinValueRoot,
        hashMidgardCekValueNode({
          kind: "builtin",
          tag: 70n,
          forcesRemaining: 0n,
          argumentsCount: committed.count,
          argumentsRoot: committed.root,
        }),
      )
    ) {
      return false;
    }
    const evaluated = evaluateMidgardCekBlsFinal(
      expectedLeftRoot,
      expectedRightRoot,
      leftExpression,
      rightExpression,
    );
    return sameBytes(
      hashMidgardCekDirectValueWitness(evaluated.result),
      hashMidgardCekDirectValueWitness(result),
    );
  } catch {
    return false;
  }
};
