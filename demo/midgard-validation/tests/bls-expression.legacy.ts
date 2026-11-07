/**
 * The recursive BLS expression evaluation `evaluateMidgardCekBlsFinal` used
 * before its explicit-stack rewrite, kept as a differential oracle. Its
 * builtins run in the pinned evaluator (`eval_program_raw`): the expression
 * is written as one program of millerLoop, mulMlResult and finalVerify over
 * the uncompressed leaf points, which traces finalVerify's result.
 */
import {
  hashMidgardCekBlsExpressionNode,
  PLUTUS_V3_CANONICAL_COST_MODEL_VIEW,
} from "@al-ft/midgard-core";
import * as UPLC from "@lucid-evolution/uplc";

import {
  type Bytes,
  type MidgardCekDirectValueWitness,
  sameBytes,
} from "../src/cek-builtin.argument-kinds.js";
import { builtinValueToDirectWitness } from "../src/cek-builtin.evaluate-direct-builtin.js";
import { midgardCekDirectBuiltinBudget } from "../src/cek-builtin.selected-control-result.js";
import type {
  MidgardCekBlsExpressionWitness,
  MidgardCekBlsFinalEvaluation,
} from "../src/cek-builtin.verify-midgard-cek-bls-final.js";
import {
  decodeMidgardCekConstantWitness,
  hashMidgardCekConstantWitness,
  type MidgardCekConstantWitness,
} from "../src/cek-constant.js";
import { encodeMidgardCekCardanoFlatProgram } from "../src/cek-program.cardano-flat.js";
import type { MidgardUplcEvalProgram } from "../src/plutus-script-evaluator.js";
import { UPLCConst } from "../src/uplc-constant.js";
import { DataB } from "../src/uplc-data.js";
import {
  Application,
  Force,
  UPLCProgram,
  type UPLCTerm,
} from "../src/uplc-term.js";
import { builtin } from "./helpers/uplc-builtins.js";

type EvaluatedBlsExpression = {
  readonly root: Bytes;
  readonly term: UPLCTerm;
  readonly leaves: number;
  readonly depth: number;
};

const call = (head: UPLCTerm, ...arguments_: UPLCTerm[]): UPLCTerm =>
  arguments_.reduce<UPLCTerm>(
    (term, argument) => new Application(term, argument),
    head,
  );

/** The point of a BLS constant witness, uncompressed by the evaluator. */
const blsPoint = (witness: MidgardCekConstantWitness): UPLCTerm => {
  const decoded = decodeMidgardCekConstantWitness(witness);
  if (decoded.type.kind !== "blsG1" && decoded.type.kind !== "blsG2") {
    throw new Error("V1 constant is not a BLS point");
  }
  if (!(decoded.payload instanceof DataB)) {
    throw new Error("V1 BLS payload is not bytes");
  }
  return call(
    builtin(
      decoded.type.kind === "blsG1"
        ? "bls12_381_G1_uncompress"
        : "bls12_381_G2_uncompress",
    ),
    UPLCConst.byteString(Uint8Array.from(decoded.payload.bytes)),
  );
};

const evaluateBlsExpression = (
  expression: MidgardCekBlsExpressionWitness,
): EvaluatedBlsExpression => {
  if (expression.kind === "millerLoop") {
    const g1Decoded = decodeMidgardCekConstantWitness(expression.g1);
    const g2Decoded = decodeMidgardCekConstantWitness(expression.g2);
    if (g1Decoded.type.kind !== "blsG1" || g2Decoded.type.kind !== "blsG2") {
      throw new Error("BLS expression leaf requires G1 and G2 constants");
    }
    return Object.freeze({
      root: hashMidgardCekBlsExpressionNode({
        kind: "millerLoop",
        g1Value: hashMidgardCekConstantWitness(expression.g1),
        g2Value: hashMidgardCekConstantWitness(expression.g2),
      }),
      term: call(
        builtin("bls12_381_millerLoop"),
        blsPoint(expression.g1),
        blsPoint(expression.g2),
      ),
      leaves: 1,
      depth: 1,
    });
  }
  const left = evaluateBlsExpression(expression.left);
  const right = evaluateBlsExpression(expression.right);
  return Object.freeze({
    root: hashMidgardCekBlsExpressionNode({
      kind: "multiply",
      left: left.root,
      right: right.root,
    }),
    term: call(builtin("bls12_381_mulMlResult"), left.term, right.term),
    leaves: left.leaves + right.leaves,
    depth: Math.max(left.depth, right.depth) + 1,
  });
};

/** A budget past any transaction's: the result alone is under test. */
const UNBOUNDED = { cpu: 1_000_000_000_000n, memory: 1_000_000_000n };

/** finalVerify of the two expressions in the evaluator. */
const finalVerify = (left: UPLCTerm, right: UPLCTerm): boolean => {
  const verdict = call(
    new Force(builtin("ifThenElse")),
    call(builtin("bls12_381_finalVerify"), left, right),
    UPLCConst.str("True"),
    UPLCConst.str("False"),
  );
  const evaluate = UPLC.eval_program_raw as MidgardUplcEvalProgram;
  const result = evaluate(
    encodeMidgardCekCardanoFlatProgram(
      new UPLCProgram(
        [1, 1, 0],
        call(new Force(builtin("trace")), verdict, UPLCConst.unit),
      ),
    ),
    [],
    3,
    11,
    BigInt64Array.from(PLUTUS_V3_CANONICAL_COST_MODEL_VIEW.map(BigInt)),
    UNBOUNDED.cpu,
    UNBOUNDED.memory,
    false,
  );
  try {
    if (!result.ok || result.logs.length !== 1) {
      throw new Error("reference evaluator rejected BLS finalVerify");
    }
    return result.logs[0] === "True";
  } finally {
    result.free();
  }
};

export const legacyEvaluateMidgardCekBlsFinal = (
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
  const result = finalVerify(left.term, right.term);
  const arguments_: readonly MidgardCekDirectValueWitness[] = [
    { kind: "blsMillerLoop", expressionRoot: left.root },
    { kind: "blsMillerLoop", expressionRoot: right.root },
  ];
  return Object.freeze({
    leftRoot: left.root,
    rightRoot: right.root,
    result: builtinValueToDirectWitness(UPLCConst.bool(result), false),
    budget: midgardCekDirectBuiltinBudget(70n, arguments_),
  });
};
