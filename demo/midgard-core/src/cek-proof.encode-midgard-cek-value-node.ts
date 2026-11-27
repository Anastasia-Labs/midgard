import {
  BLS_EXPRESSION_NODE_DOMAIN,
  boundedBuiltinTag,
  type Bytes,
  CONTINUATION_NODE_DOMAIN,
  ENVIRONMENT_NODE_DOMAIN,
  exactHash,
  hash32,
  MidgardCekValueTags,
  SEQUENCE_NODE_DOMAIN,
  uint32,
  uint64,
  VALUE_NODE_DOMAIN,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import { encodeCbor } from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

export type MidgardCekValueNode =
  | {
      readonly kind: "constant";
      readonly typeRoot: Bytes;
      readonly payloadRoot: Bytes;
      readonly payloadLength: bigint;
      readonly semanticRoot: Bytes;
      readonly memory: bigint;
    }
  | {
      readonly kind: "lambda";
      readonly body: Bytes;
      readonly environment: Bytes;
    }
  | {
      readonly kind: "delay";
      readonly body: Bytes;
      readonly environment: Bytes;
    }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
    }
  | {
      readonly kind: "builtin";
      readonly tag: bigint;
      readonly forcesRemaining: bigint;
      readonly argumentsCount: bigint;
      readonly argumentsRoot: Bytes;
    }
  | {
      readonly kind: "blsMillerLoop";
      readonly expressionRoot: Bytes;
    };

export const encodeMidgardCekValueNode = (
  node: MidgardCekValueNode,
): Buffer => {
  switch (node.kind) {
    case "constant":
      return encodeCbor([
        MidgardCekValueTags.Constant,
        exactHash(node.typeRoot, "cek_value.constant.type_root"),
        exactHash(node.payloadRoot, "cek_value.constant.payload_root"),
        uint64(node.payloadLength, "cek_value.constant.payload_length"),
        exactHash(node.semanticRoot, "cek_value.constant.semantic_root"),
        uint64(node.memory, "cek_value.constant.memory"),
      ]);
    case "lambda":
      return encodeCbor([
        MidgardCekValueTags.Lambda,
        exactHash(node.body, "cek_value.lambda.body"),
        exactHash(node.environment, "cek_value.lambda.environment"),
      ]);
    case "delay":
      return encodeCbor([
        MidgardCekValueTags.Delay,
        exactHash(node.body, "cek_value.delay.body"),
        exactHash(node.environment, "cek_value.delay.environment"),
      ]);
    case "constr":
      return encodeCbor([
        MidgardCekValueTags.Constr,
        uint64(node.tag, "cek_value.constr.tag"),
        uint32(node.valuesCount, "cek_value.constr.values_count"),
        exactHash(node.valuesRoot, "cek_value.constr.values_root"),
      ]);
    case "builtin":
      return encodeCbor([
        MidgardCekValueTags.Builtin,
        boundedBuiltinTag(node.tag),
        uint32(node.forcesRemaining, "cek_value.builtin.forces_remaining"),
        uint32(node.argumentsCount, "cek_value.builtin.arguments_count"),
        exactHash(node.argumentsRoot, "cek_value.builtin.arguments_root"),
      ]);
    case "blsMillerLoop":
      return encodeCbor([
        MidgardCekValueTags.BlsMillerLoop,
        exactHash(
          node.expressionRoot,
          "cek_value.bls_miller_loop.expression_root",
        ),
      ]);
  }
};

export const hashMidgardCekValueNode = (node: MidgardCekValueNode): Hash32 =>
  hash32(VALUE_NODE_DOMAIN, encodeMidgardCekValueNode(node));

export type MidgardCekBlsExpressionNode =
  | {
      readonly kind: "millerLoop";
      readonly g1Value: Bytes;
      readonly g2Value: Bytes;
    }
  | {
      readonly kind: "multiply";
      readonly left: Bytes;
      readonly right: Bytes;
    };

export const encodeMidgardCekBlsExpressionNode = (
  node: MidgardCekBlsExpressionNode,
): Buffer => {
  switch (node.kind) {
    case "millerLoop":
      return encodeCbor([
        0n,
        exactHash(node.g1Value, "cek_bls_expression.g1_value"),
        exactHash(node.g2Value, "cek_bls_expression.g2_value"),
      ]);
    case "multiply":
      return encodeCbor([
        1n,
        exactHash(node.left, "cek_bls_expression.left"),
        exactHash(node.right, "cek_bls_expression.right"),
      ]);
  }
};

export const hashMidgardCekBlsExpressionNode = (
  node: MidgardCekBlsExpressionNode,
): Hash32 =>
  hash32(BLS_EXPRESSION_NODE_DOMAIN, encodeMidgardCekBlsExpressionNode(node));

const EMPTY_SEQUENCE_PREIMAGE = encodeCbor([0n]);

const EMPTY_ENVIRONMENT_PREIMAGE = encodeCbor([0n]);

const EMPTY_CONTINUATION_PREIMAGE = encodeCbor([0n]);

export const MIDGARD_CEK_EMPTY_SEQUENCE_ROOT = hash32(
  SEQUENCE_NODE_DOMAIN,
  EMPTY_SEQUENCE_PREIMAGE,
);

export const MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT = hash32(
  ENVIRONMENT_NODE_DOMAIN,
  EMPTY_ENVIRONMENT_PREIMAGE,
);

export const MIDGARD_CEK_EMPTY_CONTINUATION_ROOT = hash32(
  CONTINUATION_NODE_DOMAIN,
  EMPTY_CONTINUATION_PREIMAGE,
);

export const encodeMidgardCekSequenceNode = (node: {
  readonly head: Bytes;
  readonly tail: Bytes;
  readonly length: bigint;
}): Buffer => {
  const length = uint32(node.length, "cek_sequence.length");
  if (length === 0n) {
    throw new RangeError("non-empty CEK sequence length must be positive");
  }
  return encodeCbor([
    1n,
    exactHash(node.head, "cek_sequence.head"),
    exactHash(node.tail, "cek_sequence.tail"),
    length,
  ]);
};

export const hashMidgardCekSequenceNode = (node: {
  readonly head: Bytes;
  readonly tail: Bytes;
  readonly length: bigint;
}): Hash32 => hash32(SEQUENCE_NODE_DOMAIN, encodeMidgardCekSequenceNode(node));

export const encodeMidgardCekEnvironmentNode = (node: {
  readonly value: Bytes;
  readonly tail: Bytes;
  readonly length: bigint;
}): Buffer => {
  const length = uint32(node.length, "cek_environment.length");
  if (length === 0n) {
    throw new RangeError("non-empty CEK environment length must be positive");
  }
  return encodeCbor([
    1n,
    exactHash(node.value, "cek_environment.value"),
    exactHash(node.tail, "cek_environment.tail"),
    length,
  ]);
};

export const hashMidgardCekEnvironmentNode = (node: {
  readonly value: Bytes;
  readonly tail: Bytes;
  readonly length: bigint;
}): Hash32 =>
  hash32(ENVIRONMENT_NODE_DOMAIN, encodeMidgardCekEnvironmentNode(node));

export type MidgardCekContinuationFrame =
  | { readonly kind: "force"; readonly tail: Bytes }
  | {
      readonly kind: "applyArgument";
      readonly argument: Bytes;
      readonly environment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "applyFunction";
      readonly functionValue: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly remainingTermsCount: bigint;
      readonly remainingTermsRoot: Bytes;
      readonly valuesCount: bigint;
      readonly valuesRoot: Bytes;
      readonly environment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "case";
      readonly branchesCount: bigint;
      readonly branchesRoot: Bytes;
      readonly environment: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "applyValue";
      readonly value: Bytes;
      readonly tail: Bytes;
    }
  | {
      readonly kind: "caseSelect";
      readonly environment: Bytes;
      readonly tail: Bytes;
      readonly valuesCount: bigint;
    }
  | {
      readonly kind: "caseApply";
      readonly environment: Bytes;
      readonly builtContinuation: Bytes;
    };
