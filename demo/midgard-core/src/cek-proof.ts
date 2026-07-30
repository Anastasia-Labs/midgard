import { Data as LucidData } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  decodeMidgardCekDataListNodeV1,
  decodeMidgardCekDataNodeV1,
  decodeMidgardCekDataPairNodeV1,
  hashMidgardCekDataListNodePreimageV1,
  hashMidgardCekDataListNodeV1,
  hashMidgardCekDataNodePreimageV1,
  hashMidgardCekDataNodeV1,
  hashMidgardCekDataPairNodePreimageV1,
  hashMidgardCekDataPairNodeV1,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
  midgardCekDataBytesCborLengthV1,
  midgardCekDataConstrCborLengthV1,
  midgardCekDataListCborLengthV1,
  type MidgardCekDataListNodeV1,
  type MidgardCekDataNodeV1,
  type MidgardCekDataPairNodeV1,
} from "./cek-semantic.js";
import {
  compareBytes,
  encodeCbor,
  readCborArrayHeader,
  readCborBytes,
  readCborUnsigned,
} from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

const TERM_NODE_DOMAIN = Buffer.from("MidgardCekTermNodeV1", "ascii");
const VALUE_NODE_DOMAIN = Buffer.from("MidgardCekValueNodeV1", "ascii");
const SEQUENCE_NODE_DOMAIN = Buffer.from("MidgardCekSequenceNodeV1", "ascii");
const ENVIRONMENT_NODE_DOMAIN = Buffer.from(
  "MidgardCekEnvironmentNodeV1",
  "ascii",
);
const CONTINUATION_NODE_DOMAIN = Buffer.from(
  "MidgardCekContinuationNodeV1",
  "ascii",
);
const BLOB_CHUNK_DOMAIN = Buffer.from("MidgardCekBlobChunkV1", "ascii");
const BLOB_BRANCH_DOMAIN = Buffer.from("MidgardCekBlobBranchV1", "ascii");
const MACHINE_STATE_DOMAIN = Buffer.from("MidgardCekMachineStateV1", "ascii");
const PROGRAM_ENVELOPE_DOMAIN = Buffer.from(
  "MidgardCekProgramEnvelopeV1",
  "ascii",
);
const BLS_EXPRESSION_NODE_DOMAIN = Buffer.from(
  "MidgardCekBlsExpressionV1",
  "ascii",
);

const UINT32_MAX = 0xffff_ffffn;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

/**
 * V1 makes the semantic payload root canonical for every constant, admits
 * semantic builtin/control witnesses, and uses the bounded graph-material
 * interpretation.
 */
export const MIDGARD_CEK_PROGRAM_ENVELOPE_V1_VERSION = 1n;
export const MIDGARD_CEK_MACHINE_STATE_V1_VERSION = 1n;
export const MIDGARD_CEK_BLOB_CHUNK_BYTES = 4_095;
export const MIDGARD_CEK_MAX_BUILTIN_TAG = 86n;
export const MIDGARD_CEK_PROGRAM_UPLC_VERSION_V1 = [1n, 1n, 0n] as const;
/**
 * The canonical V1 DA envelope is the only aggregate program-size budget. The
 * constants below mirror its exact canonical Plutus-Data encoding:
 *
 * - an otherwise-empty, structurally valid V1 payload is 445 bytes;
 * - replacing its one-byte empty material list with the two-byte non-empty
 *   list framing leaves 446 fixed bytes outside material tuples;
 * - the smallest tuple is 42 bytes: tuple framing (2), a bytes32 key (34),
 *   and a five-byte `[v1, kind, one-byte-preimage]` value wrapped as Plutus
 *   bytes (6).
 *
 * The canonical DA encoder has regression tests for all three measurements.
 * The material-byte bound is deliberately the tight structural upper bound
 * after fixed framing. Exact tuple overhead makes the realizable total
 * smaller, and the canonical 64 MiB DA-size check remains authoritative.
 */
export const MIDGARD_MAX_DA_PAYLOAD_BYTES_V1 = 64 * 1024 * 1024;
export const MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1 = 446;
export const MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES_V1 = 42;
export const MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1 = BigInt(
  Math.floor(
    (MIDGARD_MAX_DA_PAYLOAD_BYTES_V1 -
      MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1) /
      MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES_V1,
  ),
);
/**
 * A bundle may contain many transactions that use the same program, but
 * distinct program identities must not multiply verification work beyond one
 * maximum-size V1 program. Identical envelopes are verified once and reuse the
 * same positional result.
 */
export const MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS_V1 =
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1;
export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1 = BigInt(
  MIDGARD_MAX_DA_PAYLOAD_BYTES_V1 -
    MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES_V1,
);
/**
 * Each unique envelope declares the exact bytes reachable from its root. The
 * sum is therefore a conservative upper bound on both byte verification work
 * and retained type/payload result bytes, including when envelopes share
 * material. V1 permits at most one maximum-size program's work per bundle.
 */
export const MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK_V1 =
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1;
// [v1, [1,1,0], h32, uint32(node_count), uint32(material_bytes)].
export const MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1 = 50;
export const MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1 = 9_215;
/**
 * Matches the L1 constant decoder's direct type-CBOR limit. Constant types are
 * flat Plutus-Data tag lists, so this byte cap also gives the iterative parser
 * a deterministic bound independent of JavaScript's call-stack depth.
 */
export const MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES_V1 = 64;

export const MidgardCekTermTags = Object.freeze({
  Variable: 0n,
  Delay: 1n,
  Lambda: 2n,
  Application: 3n,
  Constant: 4n,
  Force: 5n,
  Error: 6n,
  Builtin: 7n,
  Constr: 8n,
  Case: 9n,
  // Runtime-only term used for the validation-machine-authenticated script
  // context. Canonical source-program material must reject this tag.
  ContextConstant: 10n,
} as const);

export const MidgardCekValueTags = Object.freeze({
  Constant: 0n,
  Lambda: 1n,
  Delay: 2n,
  Constr: 3n,
  Builtin: 4n,
  BlsMillerLoop: 5n,
} as const);

export const MidgardCekContinuationTags = Object.freeze({
  Force: 0n,
  ApplyArgument: 1n,
  ApplyFunction: 2n,
  Constr: 3n,
  Case: 4n,
  ApplyValue: 5n,
  CaseSelect: 6n,
  CaseApply: 7n,
} as const);

export const MidgardCekMachineModes = Object.freeze({
  Compute: 0n,
  Return: 1n,
  Lookup: 2n,
  Builtin: 3n,
  HaltSuccess: 4n,
  HaltError: 5n,
  CaseSelect: 6n,
  CaseApply: 7n,
  SemanticBuiltin: 8n,
} as const);

type Bytes = Uint8Array;

const hash32 = (domain: Uint8Array, preimage: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(Buffer.concat([Buffer.from(domain), Buffer.from(preimage)]), {
      dkLen: 32,
    }),
    "cek_proof_hash",
  );

const exactHash = (value: Bytes, fieldName: string): Buffer =>
  Buffer.from(ensureHash32(value, fieldName));

const nonNegative = (value: bigint, fieldName: string): bigint => {
  if (value < 0n) {
    throw new RangeError(`${fieldName} must be non-negative`);
  }
  return value;
};

const uint32 = (value: bigint, fieldName: string): bigint => {
  nonNegative(value, fieldName);
  if (value > UINT32_MAX) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

const uint64 = (value: bigint, fieldName: string): bigint => {
  nonNegative(value, fieldName);
  if (value > UINT64_MAX) {
    throw new RangeError(`${fieldName} must fit uint64`);
  }
  return value;
};

const boundedBuiltinTag = (value: bigint): bigint => {
  if (value < 0n || value > MIDGARD_CEK_MAX_BUILTIN_TAG) {
    throw new RangeError(
      `CEK builtin tag must be between 0 and ${MIDGARD_CEK_MAX_BUILTIN_TAG.toString(10)}`,
    );
  }
  return value;
};

export type MidgardCekTermNodeV1 =
  | { readonly kind: "variable"; readonly index: bigint }
  | { readonly kind: "delay"; readonly body: Bytes }
  | { readonly kind: "lambda"; readonly body: Bytes }
  | {
      readonly kind: "application";
      readonly function: Bytes;
      readonly argument: Bytes;
    }
  | { readonly kind: "constant"; readonly value: Bytes }
  | { readonly kind: "contextConstant"; readonly value: Bytes }
  | { readonly kind: "force"; readonly term: Bytes }
  | { readonly kind: "error" }
  | { readonly kind: "builtin"; readonly tag: bigint }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly termsCount: bigint;
      readonly termsRoot: Bytes;
    }
  | {
      readonly kind: "case";
      readonly scrutinee: Bytes;
      readonly branchesCount: bigint;
      readonly branchesRoot: Bytes;
    };

export const encodeMidgardCekTermNodeV1 = (
  node: MidgardCekTermNodeV1,
): Buffer => {
  switch (node.kind) {
    case "variable":
      return encodeCbor([
        MidgardCekTermTags.Variable,
        uint32(node.index, "cek_term.variable.index"),
      ]);
    case "delay":
      return encodeCbor([
        MidgardCekTermTags.Delay,
        exactHash(node.body, "cek_term.delay.body"),
      ]);
    case "lambda":
      return encodeCbor([
        MidgardCekTermTags.Lambda,
        exactHash(node.body, "cek_term.lambda.body"),
      ]);
    case "application":
      return encodeCbor([
        MidgardCekTermTags.Application,
        exactHash(node.function, "cek_term.application.function"),
        exactHash(node.argument, "cek_term.application.argument"),
      ]);
    case "constant":
      return encodeCbor([
        MidgardCekTermTags.Constant,
        exactHash(node.value, "cek_term.constant.value"),
      ]);
    case "contextConstant":
      return encodeCbor([
        MidgardCekTermTags.ContextConstant,
        exactHash(node.value, "cek_term.context_constant.value"),
      ]);
    case "force":
      return encodeCbor([
        MidgardCekTermTags.Force,
        exactHash(node.term, "cek_term.force.term"),
      ]);
    case "error":
      return encodeCbor([MidgardCekTermTags.Error]);
    case "builtin":
      return encodeCbor([
        MidgardCekTermTags.Builtin,
        boundedBuiltinTag(node.tag),
      ]);
    case "constr":
      return encodeCbor([
        MidgardCekTermTags.Constr,
        uint64(node.tag, "cek_term.constr.tag"),
        uint32(node.termsCount, "cek_term.constr.terms_count"),
        exactHash(node.termsRoot, "cek_term.constr.terms_root"),
      ]);
    case "case":
      return encodeCbor([
        MidgardCekTermTags.Case,
        exactHash(node.scrutinee, "cek_term.case.scrutinee"),
        uint32(node.branchesCount, "cek_term.case.branches_count"),
        exactHash(node.branchesRoot, "cek_term.case.branches_root"),
      ]);
  }
};

export const hashMidgardCekTermNodeV1 = (node: MidgardCekTermNodeV1): Hash32 =>
  hash32(TERM_NODE_DOMAIN, encodeMidgardCekTermNodeV1(node));

export type MidgardCekValueNodeV1 =
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

export const encodeMidgardCekValueNodeV1 = (
  node: MidgardCekValueNodeV1,
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

export const hashMidgardCekValueNodeV1 = (
  node: MidgardCekValueNodeV1,
): Hash32 => hash32(VALUE_NODE_DOMAIN, encodeMidgardCekValueNodeV1(node));

export type MidgardCekBlsExpressionNodeV1 =
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

export const encodeMidgardCekBlsExpressionNodeV1 = (
  node: MidgardCekBlsExpressionNodeV1,
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

export const hashMidgardCekBlsExpressionNodeV1 = (
  node: MidgardCekBlsExpressionNodeV1,
): Hash32 =>
  hash32(BLS_EXPRESSION_NODE_DOMAIN, encodeMidgardCekBlsExpressionNodeV1(node));

const EMPTY_SEQUENCE_PREIMAGE = encodeCbor([0n]);
const EMPTY_ENVIRONMENT_PREIMAGE = encodeCbor([0n]);
const EMPTY_CONTINUATION_PREIMAGE = encodeCbor([0n]);

export const MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1 = hash32(
  SEQUENCE_NODE_DOMAIN,
  EMPTY_SEQUENCE_PREIMAGE,
);
export const MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1 = hash32(
  ENVIRONMENT_NODE_DOMAIN,
  EMPTY_ENVIRONMENT_PREIMAGE,
);
export const MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1 = hash32(
  CONTINUATION_NODE_DOMAIN,
  EMPTY_CONTINUATION_PREIMAGE,
);

export const encodeMidgardCekSequenceNodeV1 = (node: {
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

export const hashMidgardCekSequenceNodeV1 = (node: {
  readonly head: Bytes;
  readonly tail: Bytes;
  readonly length: bigint;
}): Hash32 =>
  hash32(SEQUENCE_NODE_DOMAIN, encodeMidgardCekSequenceNodeV1(node));

export const encodeMidgardCekEnvironmentNodeV1 = (node: {
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

export const hashMidgardCekEnvironmentNodeV1 = (node: {
  readonly value: Bytes;
  readonly tail: Bytes;
  readonly length: bigint;
}): Hash32 =>
  hash32(ENVIRONMENT_NODE_DOMAIN, encodeMidgardCekEnvironmentNodeV1(node));

export type MidgardCekContinuationFrameV1 =
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

export const encodeMidgardCekContinuationFrameV1 = (
  frame: MidgardCekContinuationFrameV1,
): Buffer => {
  switch (frame.kind) {
    case "force":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.Force,
        exactHash(frame.tail, "cek_continuation.force.tail"),
      ]);
    case "applyArgument":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.ApplyArgument,
        exactHash(frame.argument, "cek_continuation.apply_argument.argument"),
        exactHash(
          frame.environment,
          "cek_continuation.apply_argument.environment",
        ),
        exactHash(frame.tail, "cek_continuation.apply_argument.tail"),
      ]);
    case "applyFunction":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.ApplyFunction,
        exactHash(
          frame.functionValue,
          "cek_continuation.apply_function.function_value",
        ),
        exactHash(frame.tail, "cek_continuation.apply_function.tail"),
      ]);
    case "constr":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.Constr,
        uint64(frame.tag, "cek_continuation.constr.tag"),
        uint32(
          frame.remainingTermsCount,
          "cek_continuation.constr.remaining_terms_count",
        ),
        exactHash(
          frame.remainingTermsRoot,
          "cek_continuation.constr.remaining_terms_root",
        ),
        uint32(frame.valuesCount, "cek_continuation.constr.values_count"),
        exactHash(frame.valuesRoot, "cek_continuation.constr.values_root"),
        exactHash(frame.environment, "cek_continuation.constr.environment"),
        exactHash(frame.tail, "cek_continuation.constr.tail"),
      ]);
    case "case":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.Case,
        uint32(frame.branchesCount, "cek_continuation.case.branches_count"),
        exactHash(frame.branchesRoot, "cek_continuation.case.branches_root"),
        exactHash(frame.environment, "cek_continuation.case.environment"),
        exactHash(frame.tail, "cek_continuation.case.tail"),
      ]);
    case "applyValue":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.ApplyValue,
        exactHash(frame.value, "cek_continuation.apply_value.value"),
        exactHash(frame.tail, "cek_continuation.apply_value.tail"),
      ]);
    case "caseSelect":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.CaseSelect,
        exactHash(
          frame.environment,
          "cek_continuation.case_select.environment",
        ),
        exactHash(frame.tail, "cek_continuation.case_select.tail"),
        uint32(frame.valuesCount, "cek_continuation.case_select.values_count"),
      ]);
    case "caseApply":
      return encodeCbor([
        1n,
        MidgardCekContinuationTags.CaseApply,
        exactHash(frame.environment, "cek_continuation.case_apply.environment"),
        exactHash(
          frame.builtContinuation,
          "cek_continuation.case_apply.built_continuation",
        ),
      ]);
  }
};

export const hashMidgardCekContinuationFrameV1 = (
  frame: MidgardCekContinuationFrameV1,
): Hash32 =>
  hash32(CONTINUATION_NODE_DOMAIN, encodeMidgardCekContinuationFrameV1(frame));

export const hashMidgardCekBlobChunkV1 = (chunk: Bytes): Hash32 => {
  return hash32(BLOB_CHUNK_DOMAIN, encodeMidgardCekBlobChunkV1(chunk));
};

export const encodeMidgardCekBlobChunkV1 = (chunk: Bytes): Buffer => {
  if (chunk.length > MIDGARD_CEK_BLOB_CHUNK_BYTES) {
    throw new RangeError(
      `CEK blob chunk must contain at most ${MIDGARD_CEK_BLOB_CHUNK_BYTES.toString(10)} bytes`,
    );
  }
  return encodeCbor(Buffer.from(chunk));
};

export type MidgardCekBlobBranchV1 = {
  readonly left: Bytes;
  readonly right: Bytes;
  readonly byteLength: bigint;
};

export const encodeMidgardCekBlobBranchV1 = (
  input: MidgardCekBlobBranchV1,
): Buffer =>
  encodeCbor([
    exactHash(input.left, "cek_blob_branch.left"),
    exactHash(input.right, "cek_blob_branch.right"),
    uint64(input.byteLength, "cek_blob_branch.byte_length"),
  ]);

export const hashMidgardCekBlobBranchV1 = (
  input: MidgardCekBlobBranchV1,
): Hash32 => hash32(BLOB_BRANCH_DOMAIN, encodeMidgardCekBlobBranchV1(input));

export type MidgardCekBlobCommitmentV1 = {
  readonly root: Hash32;
  readonly byteLength: bigint;
  readonly nodes: ReadonlyMap<
    string,
    {
      readonly kind: "chunk" | "branch";
      readonly preimage: Buffer;
    }
  >;
};

/**
 * Commits a byte string as a canonical left-balanced binary tree of 4,095
 * byte leaves. A one-leaf (including empty) blob is committed directly by its
 * chunk hash. Larger trees split at the greatest power-of-two leaf count below
 * the total, so the same bytes have exactly one root and proof shape.
 */
export const commitMidgardCekBlobV1 = (
  bytes: Bytes,
): MidgardCekBlobCommitmentV1 => {
  const source = Buffer.from(bytes);
  const chunks: Buffer[] = [];
  if (source.length === 0) {
    chunks.push(Buffer.alloc(0));
  } else {
    for (
      let offset = 0;
      offset < source.length;
      offset += MIDGARD_CEK_BLOB_CHUNK_BYTES
    ) {
      chunks.push(
        source.subarray(
          offset,
          Math.min(offset + MIDGARD_CEK_BLOB_CHUNK_BYTES, source.length),
        ),
      );
    }
  }

  const nodes = new Map<
    string,
    {
      readonly kind: "chunk" | "branch";
      readonly preimage: Buffer;
    }
  >();
  const commitRange = (
    start: number,
    end: number,
  ): { readonly root: Hash32; readonly byteLength: bigint } => {
    const count = end - start;
    if (count === 1) {
      const preimage = encodeMidgardCekBlobChunkV1(chunks[start]!);
      const root = hash32(BLOB_CHUNK_DOMAIN, preimage);
      nodes.set(Buffer.from(root).toString("hex"), {
        kind: "chunk",
        preimage,
      });
      return { root, byteLength: BigInt(chunks[start]!.length) };
    }
    let leftCount = 1;
    while (leftCount * 2 < count) {
      leftCount *= 2;
    }
    const left = commitRange(start, start + leftCount);
    const right = commitRange(start + leftCount, end);
    const byteLength = left.byteLength + right.byteLength;
    const preimage = encodeMidgardCekBlobBranchV1({
      left: left.root,
      right: right.root,
      byteLength,
    });
    const root = hash32(BLOB_BRANCH_DOMAIN, preimage);
    nodes.set(Buffer.from(root).toString("hex"), {
      kind: "branch",
      preimage,
    });
    return { root, byteLength };
  };

  const committed = commitRange(0, chunks.length);
  return Object.freeze({
    root: committed.root,
    byteLength: committed.byteLength,
    nodes,
  });
};

export type MidgardCekMachineStateV1 = {
  readonly mode:
    | "compute"
    | "return"
    | "lookup"
    | "builtin"
    | "haltSuccess"
    | "haltError"
    | "caseSelect"
    | "caseApply"
    | "semanticBuiltin";
  readonly executionIndex: bigint;
  readonly focusRoot: Bytes;
  readonly environmentRoot: Bytes;
  readonly continuationRoot: Bytes;
  readonly auxiliary: bigint;
  readonly cpu: bigint;
  readonly memory: bigint;
};

const machineModeTag = (mode: MidgardCekMachineStateV1["mode"]): bigint => {
  switch (mode) {
    case "compute":
      return MidgardCekMachineModes.Compute;
    case "return":
      return MidgardCekMachineModes.Return;
    case "lookup":
      return MidgardCekMachineModes.Lookup;
    case "builtin":
      return MidgardCekMachineModes.Builtin;
    case "haltSuccess":
      return MidgardCekMachineModes.HaltSuccess;
    case "haltError":
      return MidgardCekMachineModes.HaltError;
    case "caseSelect":
      return MidgardCekMachineModes.CaseSelect;
    case "caseApply":
      return MidgardCekMachineModes.CaseApply;
    case "semanticBuiltin":
      return MidgardCekMachineModes.SemanticBuiltin;
  }
};

export const encodeMidgardCekMachineStateV1 = (
  state: MidgardCekMachineStateV1,
): Buffer =>
  encodeCbor([
    MIDGARD_CEK_MACHINE_STATE_V1_VERSION,
    machineModeTag(state.mode),
    uint32(state.executionIndex, "cek_state.execution_index"),
    exactHash(state.focusRoot, "cek_state.focus_root"),
    exactHash(state.environmentRoot, "cek_state.environment_root"),
    exactHash(state.continuationRoot, "cek_state.continuation_root"),
    uint64(state.auxiliary, "cek_state.auxiliary"),
    uint64(state.cpu, "cek_state.cpu"),
    uint64(state.memory, "cek_state.memory"),
  ]);

export const hashMidgardCekMachineStateV1 = (
  state: MidgardCekMachineStateV1,
): Hash32 =>
  hash32(MACHINE_STATE_DOMAIN, encodeMidgardCekMachineStateV1(state));

export type MidgardCekProgramEnvelopeV1 = {
  readonly uplcVersion: readonly [bigint, bigint, bigint];
  readonly termRoot: Bytes;
  readonly nodeCount: bigint;
  readonly materialByteLength: bigint;
};

export const encodeMidgardCekProgramEnvelopeV1 = (
  envelope: MidgardCekProgramEnvelopeV1,
): Buffer =>
  encodeCbor([
    MIDGARD_CEK_PROGRAM_ENVELOPE_V1_VERSION,
    [
      uint32(envelope.uplcVersion[0], "cek_program.version.major"),
      uint32(envelope.uplcVersion[1], "cek_program.version.minor"),
      uint32(envelope.uplcVersion[2], "cek_program.version.patch"),
    ],
    exactHash(envelope.termRoot, "cek_program.term_root"),
    uint32(envelope.nodeCount, "cek_program.node_count"),
    uint64(envelope.materialByteLength, "cek_program.material_byte_length"),
  ]);

/**
 * Decodes the exact V1 consensus payload carried by PlutusV3 and
 * MidgardV1 script witnesses/reference scripts. Raw Flat programs are SDK
 * inputs only and must be canonicalized before transaction construction.
 */
export const decodeMidgardCekProgramEnvelopeV1 = (
  bytes: Uint8Array,
): MidgardCekProgramEnvelopeV1 => {
  const source = Buffer.from(bytes);
  if (source.length > MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1) {
    throw new Error(
      `CEK program envelope exceeds ${MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1.toString()} bytes`,
    );
  }

  const envelopeHeader = readCborArrayHeader(source, 0, "cek_program_envelope");
  if (envelopeHeader.length !== 5) {
    throw new Error("CEK program envelope must contain exactly five fields");
  }
  const envelopeVersion = readCborUnsigned(
    source,
    envelopeHeader.nextOffset,
    "cek_program_envelope.version",
  );
  if (envelopeVersion.value !== MIDGARD_CEK_PROGRAM_ENVELOPE_V1_VERSION) {
    throw new Error(
      `unsupported CEK program envelope version ${envelopeVersion.value.toString()}`,
    );
  }

  const uplcHeader = readCborArrayHeader(
    source,
    envelopeVersion.nextOffset,
    "cek_program_envelope.uplc_version",
  );
  if (uplcHeader.length !== 3) {
    throw new Error("CEK UPLC version must contain exactly three components");
  }
  const major = readCborUnsigned(
    source,
    uplcHeader.nextOffset,
    "cek_program_envelope.uplc_version.major",
  );
  const minor = readCborUnsigned(
    source,
    major.nextOffset,
    "cek_program_envelope.uplc_version.minor",
  );
  const patch = readCborUnsigned(
    source,
    minor.nextOffset,
    "cek_program_envelope.uplc_version.patch",
  );
  if (
    major.value !== MIDGARD_CEK_PROGRAM_UPLC_VERSION_V1[0] ||
    minor.value !== MIDGARD_CEK_PROGRAM_UPLC_VERSION_V1[1] ||
    patch.value !== MIDGARD_CEK_PROGRAM_UPLC_VERSION_V1[2]
  ) {
    throw new Error(
      `V1 supports only UPLC ${MIDGARD_CEK_PROGRAM_UPLC_VERSION_V1.join(".")}`,
    );
  }

  const termRoot = readCborBytes(
    source,
    patch.nextOffset,
    "cek_program_envelope.term_root",
  );
  const exactTermRoot = exactHash(
    termRoot.value,
    "cek_program_envelope.term_root",
  );
  const nodeCount = readCborUnsigned(
    source,
    termRoot.nextOffset,
    "cek_program_envelope.node_count",
  );
  const materialByteLength = readCborUnsigned(
    source,
    nodeCount.nextOffset,
    "cek_program_envelope.material_byte_length",
  );
  if (materialByteLength.nextOffset !== source.length) {
    throw new Error("CEK program envelope has trailing bytes");
  }
  if (
    nodeCount.value === 0n ||
    nodeCount.value > MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1
  ) {
    throw new Error(
      `CEK program node count must be between 1 and ${MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1.toString()}`,
    );
  }
  if (
    materialByteLength.value === 0n ||
    materialByteLength.value > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1
  ) {
    throw new Error(
      `CEK program material length must be between 1 and ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1.toString()}`,
    );
  }

  const decoded: MidgardCekProgramEnvelopeV1 = Object.freeze({
    uplcVersion: MIDGARD_CEK_PROGRAM_UPLC_VERSION_V1,
    termRoot: exactTermRoot,
    nodeCount: nodeCount.value,
    materialByteLength: materialByteLength.value,
  });
  if (!encodeMidgardCekProgramEnvelopeV1(decoded).equals(source)) {
    throw new Error("CEK program envelope CBOR is not canonical");
  }
  return decoded;
};

export const hashMidgardCekProgramEnvelopeV1 = (
  envelope: MidgardCekProgramEnvelopeV1,
): Hash32 =>
  hash32(PROGRAM_ENVELOPE_DOMAIN, encodeMidgardCekProgramEnvelopeV1(envelope));

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1 =
  MIDGARD_CEK_BLOB_CHUNK_BYTES + 3;
export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1 =
  1 + 1 + 34 + 3 + MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1;
export const MIDGARD_CEK_PROGRAM_MATERIAL_V1_VERSION = 1n;
export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1 =
  1 + 1 + 1 + 3 + MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1;

export const MidgardCekProgramMaterialKindTags = Object.freeze({
  Term: 0n,
  Value: 1n,
  Sequence: 2n,
  BlobChunk: 3n,
  BlobBranch: 4n,
  DataNode: 5n,
  DataList: 6n,
  DataPair: 7n,
} as const);

export type MidgardCekProgramMaterialKindV1 =
  | "term"
  | "value"
  | "sequence"
  | "blobChunk"
  | "blobBranch"
  | "dataNode"
  | "dataList"
  | "dataPair";

export type MidgardCekProgramMaterialEntryV1 = {
  readonly kind: MidgardCekProgramMaterialKindV1;
  readonly root: Hash32;
  readonly preimage: Buffer;
};

/**
 * Exact decoded body of the versioned K09 DA/publication value
 * `[1, kind, preimage]`. The version is implicit in the V1 type and is
 * emitted and checked by the encoder/decoder.
 */
export type MidgardCekProgramMaterialValueV1 = Pick<
  MidgardCekProgramMaterialEntryV1,
  "kind" | "preimage"
>;

export const midgardCekProgramMaterialKindTagV1 = (
  kind: MidgardCekProgramMaterialKindV1,
): bigint => {
  switch (kind) {
    case "term":
      return MidgardCekProgramMaterialKindTags.Term;
    case "value":
      return MidgardCekProgramMaterialKindTags.Value;
    case "sequence":
      return MidgardCekProgramMaterialKindTags.Sequence;
    case "blobChunk":
      return MidgardCekProgramMaterialKindTags.BlobChunk;
    case "blobBranch":
      return MidgardCekProgramMaterialKindTags.BlobBranch;
    case "dataNode":
      return MidgardCekProgramMaterialKindTags.DataNode;
    case "dataList":
      return MidgardCekProgramMaterialKindTags.DataList;
    case "dataPair":
      return MidgardCekProgramMaterialKindTags.DataPair;
  }
};

export const midgardCekProgramMaterialKindFromTagV1 = (
  tag: bigint,
): MidgardCekProgramMaterialKindV1 => {
  switch (tag) {
    case MidgardCekProgramMaterialKindTags.Term:
      return "term";
    case MidgardCekProgramMaterialKindTags.Value:
      return "value";
    case MidgardCekProgramMaterialKindTags.Sequence:
      return "sequence";
    case MidgardCekProgramMaterialKindTags.BlobChunk:
      return "blobChunk";
    case MidgardCekProgramMaterialKindTags.BlobBranch:
      return "blobBranch";
    case MidgardCekProgramMaterialKindTags.DataNode:
      return "dataNode";
    case MidgardCekProgramMaterialKindTags.DataList:
      return "dataList";
    case MidgardCekProgramMaterialKindTags.DataPair:
      return "dataPair";
    default:
      throw new Error(
        `unsupported CEK program material kind ${tag.toString()}`,
      );
  }
};

const materialDomain = (kind: MidgardCekProgramMaterialKindV1): Buffer => {
  switch (kind) {
    case "term":
      return TERM_NODE_DOMAIN;
    case "value":
      return VALUE_NODE_DOMAIN;
    case "sequence":
      return SEQUENCE_NODE_DOMAIN;
    case "blobChunk":
      return BLOB_CHUNK_DOMAIN;
    case "blobBranch":
      return BLOB_BRANCH_DOMAIN;
    case "dataNode":
    case "dataList":
    case "dataPair":
      throw new Error("CEK semantic material uses its dedicated domain");
  }
};

const exactMaterialPreimage = (preimage: Uint8Array): Buffer => {
  const exact = Buffer.from(preimage);
  if (exact.length === 0) {
    throw new Error("CEK program material preimage must not be empty");
  }
  if (exact.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1) {
    throw new Error(
      `CEK program material preimage exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1.toString()} bytes`,
    );
  }
  return exact;
};

export const hashMidgardCekProgramMaterialPreimageV1 = (
  kind: MidgardCekProgramMaterialKindV1,
  preimage: Uint8Array,
): Hash32 => {
  const exact = exactMaterialPreimage(preimage);
  switch (kind) {
    case "dataNode":
      return hashMidgardCekDataNodePreimageV1(exact);
    case "dataList":
      return hashMidgardCekDataListNodePreimageV1(exact);
    case "dataPair":
      return hashMidgardCekDataPairNodePreimageV1(exact);
    default:
      return hash32(materialDomain(kind), exact);
  }
};

/**
 * Canonical one-node proof witness. The root is repeated deliberately so an
 * independently revealed entry is self-authenticating before graph traversal.
 */
export const encodeMidgardCekProgramMaterialEntryV1 = (
  entry: MidgardCekProgramMaterialEntryV1,
): Buffer => {
  const encoded = encodeCbor([
    midgardCekProgramMaterialKindTagV1(entry.kind),
    exactHash(entry.root, "cek_program_material.root"),
    exactMaterialPreimage(entry.preimage),
  ]);
  if (encoded.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1) {
    throw new Error(
      `CEK program material entry exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1.toString()} bytes`,
    );
  }
  return encoded;
};

export const decodeMidgardCekProgramMaterialEntryV1 = (
  bytes: Uint8Array,
): MidgardCekProgramMaterialEntryV1 => {
  const source = Buffer.from(bytes);
  if (source.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1) {
    throw new Error(
      `CEK program material entry exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1.toString()} bytes`,
    );
  }
  const header = readCborArrayHeader(source, 0, "cek_program_material_entry");
  if (header.length !== 3) {
    throw new Error(
      "CEK program material entry must contain exactly three fields",
    );
  }
  const tag = readCborUnsigned(
    source,
    header.nextOffset,
    "cek_program_material_entry.kind",
  );
  const kind = midgardCekProgramMaterialKindFromTagV1(tag.value);
  const root = readCborBytes(
    source,
    tag.nextOffset,
    "cek_program_material_entry.root",
  );
  const exactRoot = exactHash(root.value, "cek_program_material_entry.root");
  const preimage = readCborBytes(
    source,
    root.nextOffset,
    "cek_program_material_entry.preimage",
  );
  if (preimage.nextOffset !== source.length) {
    throw new Error("CEK program material entry has trailing bytes");
  }
  const exactPreimage = exactMaterialPreimage(preimage.value);
  const decoded = Object.freeze({
    kind,
    root: exactRoot as Hash32,
    preimage: exactPreimage,
  });
  if (!encodeMidgardCekProgramMaterialEntryV1(decoded).equals(source)) {
    throw new Error("CEK program material entry CBOR is not canonical");
  }
  const computed = hashMidgardCekProgramMaterialPreimageV1(kind, exactPreimage);
  if (!Buffer.from(computed).equals(exactRoot)) {
    throw new Error("CEK program material root does not match its preimage");
  }
  return decoded;
};

/**
 * Compact DA/submission sidecar value. The content root is the containing
 * sorted entry key, while the versioned value carries the domain kind and
 * exact node preimage.
 */
export const encodeMidgardCekProgramMaterialDaValueV1 = (
  entry: MidgardCekProgramMaterialValueV1,
): Buffer => {
  const encoded = encodeCbor([
    MIDGARD_CEK_PROGRAM_MATERIAL_V1_VERSION,
    midgardCekProgramMaterialKindTagV1(entry.kind),
    exactMaterialPreimage(entry.preimage),
  ]);
  if (encoded.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1) {
    throw new Error(
      `CEK program material DA value exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1.toString()} bytes`,
    );
  }
  return encoded;
};

export const decodeMidgardCekProgramMaterialDaEntryV1 = (
  root: Uint8Array,
  value: Uint8Array,
): MidgardCekProgramMaterialEntryV1 => {
  const exactRoot = exactHash(root, "cek_program_material_da.root") as Hash32;
  const source = Buffer.from(value);
  if (source.length > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1) {
    throw new Error(
      `CEK program material DA value exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1.toString()} bytes`,
    );
  }
  const header = readCborArrayHeader(
    source,
    0,
    "cek_program_material_da.value",
  );
  if (header.length !== 3) {
    throw new Error(
      "CEK program material DA value must contain exactly three fields",
    );
  }
  const version = readCborUnsigned(
    source,
    header.nextOffset,
    "cek_program_material_da.version",
  );
  if (version.value !== MIDGARD_CEK_PROGRAM_MATERIAL_V1_VERSION) {
    throw new Error(
      `unsupported CEK program material DA value version ${version.value.toString()}`,
    );
  }
  const tag = readCborUnsigned(
    source,
    version.nextOffset,
    "cek_program_material_da.kind",
  );
  const kind = midgardCekProgramMaterialKindFromTagV1(tag.value);
  const preimage = readCborBytes(
    source,
    tag.nextOffset,
    "cek_program_material_da.preimage",
  );
  if (preimage.nextOffset !== source.length) {
    throw new Error("CEK program material DA value has trailing bytes");
  }
  const decoded = Object.freeze({
    kind,
    root: exactRoot,
    preimage: exactMaterialPreimage(preimage.value),
  });
  if (!encodeMidgardCekProgramMaterialDaValueV1(decoded).equals(source)) {
    throw new Error("CEK program material DA value CBOR is not canonical");
  }
  if (
    !Buffer.from(
      hashMidgardCekProgramMaterialPreimageV1(kind, decoded.preimage),
    ).equals(exactRoot)
  ) {
    throw new Error(
      "CEK program material DA key does not match its typed preimage",
    );
  }
  return decoded;
};

export type MidgardCekDecodedProgramTermV1 =
  | { readonly kind: "variable"; readonly index: bigint }
  | { readonly kind: "error" }
  | { readonly kind: "builtin"; readonly tag: bigint }
  | {
      readonly kind: "unaryTerm";
      readonly termKind: "delay" | "lambda" | "force";
      readonly child: Hash32;
    }
  | {
      readonly kind: "application";
      readonly function: Hash32;
      readonly argument: Hash32;
    }
  | { readonly kind: "constant"; readonly value: Hash32 }
  | { readonly kind: "contextConstant"; readonly value: Hash32 }
  | {
      readonly kind: "constr";
      readonly tag: bigint;
      readonly count: bigint;
      readonly sequence: Hash32;
    }
  | {
      readonly kind: "case";
      readonly scrutinee: Hash32;
      readonly count: bigint;
      readonly sequence: Hash32;
    };

export type MidgardCekDecodedProgramValueV1 = {
  readonly typeRoot: Hash32;
  readonly payloadRoot: Hash32;
  readonly payloadLength: bigint;
  readonly semanticRoot: Hash32;
  readonly memory: bigint;
};

export type MidgardCekDecodedProgramSequenceV1 = {
  readonly head: Hash32;
  readonly tail: Hash32;
  readonly length: bigint;
};

export type MidgardCekDecodedProgramBlobV1 =
  | { readonly kind: "chunk"; readonly bytes: Buffer }
  | {
      readonly kind: "branch";
      readonly left: Hash32;
      readonly right: Hash32;
      readonly byteLength: bigint;
    };

const readExactHashAt = (
  bytes: Buffer,
  offset: number,
  fieldName: string,
): { readonly value: Hash32; readonly nextOffset: number } => {
  const decoded = readCborBytes(bytes, offset, fieldName);
  return {
    value: exactHash(decoded.value, fieldName) as Hash32,
    nextOffset: decoded.nextOffset,
  };
};

const assertPreimageConsumed = (
  bytes: Buffer,
  nextOffset: number,
  fieldName: string,
): void => {
  if (nextOffset !== bytes.length) {
    throw new Error(`${fieldName} has trailing bytes`);
  }
};

export const decodeMidgardCekProgramTermPreimageV1 = (
  preimage: Buffer,
): MidgardCekDecodedProgramTermV1 => {
  const header = readCborArrayHeader(preimage, 0, "cek_program_term");
  const tag = readCborUnsigned(
    preimage,
    header.nextOffset,
    "cek_program_term.tag",
  );
  switch (tag.value) {
    case MidgardCekTermTags.Variable: {
      if (header.length !== 2) {
        throw new Error("CEK variable term must contain two fields");
      }
      const index = readCborUnsigned(
        preimage,
        tag.nextOffset,
        "cek_program_term.variable.index",
      );
      uint32(index.value, "cek_program_term.variable.index");
      assertPreimageConsumed(preimage, index.nextOffset, "CEK variable term");
      return { kind: "variable", index: index.value };
    }
    case MidgardCekTermTags.Delay:
    case MidgardCekTermTags.Lambda:
    case MidgardCekTermTags.Force: {
      if (header.length !== 2) {
        throw new Error("CEK unary term must contain two fields");
      }
      const child = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.child",
      );
      assertPreimageConsumed(preimage, child.nextOffset, "CEK unary term");
      return {
        kind: "unaryTerm",
        termKind:
          tag.value === MidgardCekTermTags.Delay
            ? "delay"
            : tag.value === MidgardCekTermTags.Lambda
              ? "lambda"
              : "force",
        child: child.value,
      };
    }
    case MidgardCekTermTags.Application: {
      if (header.length !== 3) {
        throw new Error("CEK application term must contain three fields");
      }
      const functionRoot = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.application.function",
      );
      const argument = readExactHashAt(
        preimage,
        functionRoot.nextOffset,
        "cek_program_term.application.argument",
      );
      assertPreimageConsumed(
        preimage,
        argument.nextOffset,
        "CEK application term",
      );
      return {
        kind: "application",
        function: functionRoot.value,
        argument: argument.value,
      };
    }
    case MidgardCekTermTags.Constant: {
      if (header.length !== 2) {
        throw new Error("CEK constant term must contain two fields");
      }
      const value = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.constant.value",
      );
      assertPreimageConsumed(preimage, value.nextOffset, "CEK constant term");
      return { kind: "constant", value: value.value };
    }
    case MidgardCekTermTags.ContextConstant: {
      if (header.length !== 2) {
        throw new Error("CEK context-constant term must contain two fields");
      }
      const value = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.context_constant.value",
      );
      assertPreimageConsumed(
        preimage,
        value.nextOffset,
        "CEK context-constant term",
      );
      return { kind: "contextConstant", value: value.value };
    }
    case MidgardCekTermTags.Error: {
      if (header.length !== 1) {
        throw new Error("CEK error term must contain one field");
      }
      assertPreimageConsumed(preimage, tag.nextOffset, "CEK error term");
      return { kind: "error" };
    }
    case MidgardCekTermTags.Builtin: {
      if (header.length !== 2) {
        throw new Error("CEK builtin term must contain two fields");
      }
      const builtin = readCborUnsigned(
        preimage,
        tag.nextOffset,
        "cek_program_term.builtin.tag",
      );
      boundedBuiltinTag(builtin.value);
      assertPreimageConsumed(preimage, builtin.nextOffset, "CEK builtin term");
      return { kind: "builtin", tag: builtin.value };
    }
    case MidgardCekTermTags.Constr: {
      if (header.length !== 4) {
        throw new Error("CEK constr term must contain four fields");
      }
      const constrTag = readCborUnsigned(
        preimage,
        tag.nextOffset,
        "cek_program_term.constr.tag",
      );
      uint64(constrTag.value, "cek_program_term.constr.tag");
      const count = readCborUnsigned(
        preimage,
        constrTag.nextOffset,
        "cek_program_term.constr.count",
      );
      uint32(count.value, "cek_program_term.constr.count");
      const sequence = readExactHashAt(
        preimage,
        count.nextOffset,
        "cek_program_term.constr.sequence",
      );
      assertPreimageConsumed(preimage, sequence.nextOffset, "CEK constr term");
      return {
        kind: "constr",
        tag: constrTag.value,
        count: count.value,
        sequence: sequence.value,
      };
    }
    case MidgardCekTermTags.Case: {
      if (header.length !== 4) {
        throw new Error("CEK case term must contain four fields");
      }
      const scrutinee = readExactHashAt(
        preimage,
        tag.nextOffset,
        "cek_program_term.case.scrutinee",
      );
      const count = readCborUnsigned(
        preimage,
        scrutinee.nextOffset,
        "cek_program_term.case.count",
      );
      uint32(count.value, "cek_program_term.case.count");
      const sequence = readExactHashAt(
        preimage,
        count.nextOffset,
        "cek_program_term.case.sequence",
      );
      assertPreimageConsumed(preimage, sequence.nextOffset, "CEK case term");
      return {
        kind: "case",
        scrutinee: scrutinee.value,
        count: count.value,
        sequence: sequence.value,
      };
    }
    default:
      throw new Error(`unknown CEK program term tag ${tag.value.toString()}`);
  }
};

export const decodeMidgardCekProgramValuePreimageV1 = (
  preimage: Buffer,
): MidgardCekDecodedProgramValueV1 => {
  const header = readCborArrayHeader(preimage, 0, "cek_program_value");
  if (header.length !== 6) {
    throw new Error("CEK source-program value must be a six-field constant");
  }
  const tag = readCborUnsigned(
    preimage,
    header.nextOffset,
    "cek_program_value.tag",
  );
  if (tag.value !== MidgardCekValueTags.Constant) {
    throw new Error("CEK source-program material may contain only constants");
  }
  const typeRoot = readExactHashAt(
    preimage,
    tag.nextOffset,
    "cek_program_value.constant.type_root",
  );
  const payloadRoot = readExactHashAt(
    preimage,
    typeRoot.nextOffset,
    "cek_program_value.constant.payload_root",
  );
  const payloadLength = readCborUnsigned(
    preimage,
    payloadRoot.nextOffset,
    "cek_program_value.constant.payload_length",
  );
  uint64(payloadLength.value, "cek_program_value.constant.payload_length");
  const semanticRoot = readExactHashAt(
    preimage,
    payloadLength.nextOffset,
    "cek_program_value.constant.semantic_root",
  );
  const memory = readCborUnsigned(
    preimage,
    semanticRoot.nextOffset,
    "cek_program_value.constant.memory",
  );
  uint64(memory.value, "cek_program_value.constant.memory");
  assertPreimageConsumed(preimage, memory.nextOffset, "CEK constant value");
  return {
    typeRoot: typeRoot.value,
    payloadRoot: payloadRoot.value,
    payloadLength: payloadLength.value,
    semanticRoot: semanticRoot.value,
    memory: memory.value,
  };
};

export const decodeMidgardCekProgramSequencePreimageV1 = (
  preimage: Buffer,
): MidgardCekDecodedProgramSequenceV1 => {
  const header = readCborArrayHeader(preimage, 0, "cek_program_sequence");
  if (header.length !== 4) {
    throw new Error("CEK program sequence must contain four fields");
  }
  const tag = readCborUnsigned(
    preimage,
    header.nextOffset,
    "cek_program_sequence.tag",
  );
  if (tag.value !== 1n) {
    throw new Error("CEK material cannot encode an explicit empty sequence");
  }
  const head = readExactHashAt(
    preimage,
    tag.nextOffset,
    "cek_program_sequence.head",
  );
  const tail = readExactHashAt(
    preimage,
    head.nextOffset,
    "cek_program_sequence.tail",
  );
  const length = readCborUnsigned(
    preimage,
    tail.nextOffset,
    "cek_program_sequence.length",
  );
  uint32(length.value, "cek_program_sequence.length");
  if (length.value === 0n) {
    throw new Error("CEK non-empty sequence length must be positive");
  }
  assertPreimageConsumed(preimage, length.nextOffset, "CEK program sequence");
  return { head: head.value, tail: tail.value, length: length.value };
};

export const decodeMidgardCekProgramBlobPreimageV1 = (
  kind: "blobChunk" | "blobBranch",
  preimage: Buffer,
): MidgardCekDecodedProgramBlobV1 => {
  if (kind === "blobChunk") {
    const chunk = readCborBytes(preimage, 0, "cek_program_blob.chunk");
    assertPreimageConsumed(preimage, chunk.nextOffset, "CEK blob chunk");
    if (chunk.value.length > MIDGARD_CEK_BLOB_CHUNK_BYTES) {
      throw new Error(
        `CEK blob chunk exceeds ${MIDGARD_CEK_BLOB_CHUNK_BYTES.toString()} bytes`,
      );
    }
    if (!encodeMidgardCekBlobChunkV1(chunk.value).equals(preimage)) {
      throw new Error("CEK blob chunk CBOR is not canonical");
    }
    return { kind: "chunk", bytes: chunk.value };
  }

  const header = readCborArrayHeader(preimage, 0, "cek_program_blob.branch");
  if (header.length !== 3) {
    throw new Error("CEK blob branch must contain three fields");
  }
  const left = readExactHashAt(
    preimage,
    header.nextOffset,
    "cek_program_blob.branch.left",
  );
  const right = readExactHashAt(
    preimage,
    left.nextOffset,
    "cek_program_blob.branch.right",
  );
  const byteLength = readCborUnsigned(
    preimage,
    right.nextOffset,
    "cek_program_blob.branch.byte_length",
  );
  uint64(byteLength.value, "cek_program_blob.branch.byte_length");
  assertPreimageConsumed(preimage, byteLength.nextOffset, "CEK blob branch");
  return {
    kind: "branch",
    left: left.value,
    right: right.value,
    byteLength: byteLength.value,
  };
};

type ProgramMaterialTaskV1 =
  | {
      readonly kind: "term";
      readonly root: Hash32;
    }
  | {
      readonly kind: "value";
      readonly root: Hash32;
    }
  | {
      readonly kind: "sequence";
      readonly root: Hash32;
      readonly length: bigint;
    }
  | {
      readonly kind: "blob";
      readonly root: Hash32;
      readonly byteLength?: bigint;
      readonly maxByteLength?: bigint;
    }
  | {
      readonly kind: "dataNode";
      readonly root: Hash32;
    }
  | {
      readonly kind: "dataList";
      readonly root: Hash32;
      readonly length: bigint;
    }
  | {
      readonly kind: "dataPair";
      readonly root: Hash32;
      readonly length: bigint;
    };

export type MidgardCekProgramConstantMaterialV1 = {
  readonly valueRoot: Hash32;
  readonly typeRoot: Hash32;
  readonly payloadRoot: Hash32;
  readonly semanticRoot: Hash32;
  readonly memory: bigint;
  readonly typeCbor: Buffer;
  readonly payloadCbor: Buffer;
};

export type MidgardCekProgramMaterialVerificationV1 = {
  readonly reachableRoots: ReadonlySet<string>;
  readonly nodeCount: bigint;
  readonly materialByteLength: bigint;
  readonly constants: readonly MidgardCekProgramConstantMaterialV1[];
};

export type MidgardCekProgramMaterialVerificationOptionsV1 = {
  readonly allowUnreachable?: boolean;
  /**
   * Allocation observability for resource-bound tests and metrics. It fires
   * only when a final content root is assembled, never for branch validation
   * or a bundle-cache hit. Callback failures are ignored so observability
   * cannot change verifier acceptance.
   */
  readonly onBlobMaterialized?: (rootHex: string, byteLength: bigint) => void;
  /**
   * Fires on the first validated constant-result materialization for a value
   * content root in a bundle. Callback failures are ignored.
   */
  readonly onConstantMaterialized?: (
    valueRootHex: string,
    payloadByteLength: bigint,
  ) => void;
};

type NormalizedProgramMaterialV1 = ReadonlyMap<
  string,
  MidgardCekProgramMaterialEntryV1
>;

type ProgramMaterialBundleCacheV1 = {
  /**
   * Internal-only buffers keyed by authenticated content root. Callers receive
   * copies, so cached bytes are immutable for the lifetime of verification.
   */
  readonly materializedBlobs: Map<string, Buffer>;
  readonly validatedConstants: Map<
    string,
    {
      readonly typeCbor: Buffer;
      readonly payloadCbor: Buffer;
    }
  >;
};

const normalizeProgramMaterialV1 = (
  entries: Iterable<MidgardCekProgramMaterialEntryV1>,
): NormalizedProgramMaterialV1 => {
  const normalized = new Map<string, MidgardCekProgramMaterialEntryV1>();
  let materialByteLength = 0n;
  for (const entry of entries) {
    const exact = decodeMidgardCekProgramMaterialEntryV1(
      encodeMidgardCekProgramMaterialEntryV1(entry),
    );
    const key = Buffer.from(exact.root).toString("hex");
    if (normalized.has(key)) {
      throw new Error(`duplicate CEK program material root ${key}`);
    }
    normalized.set(key, exact);
    if (BigInt(normalized.size) > MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1) {
      throw new Error(
        `CEK program material contains more than ${MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1.toString()} nodes`,
      );
    }
    materialByteLength += BigInt(exact.preimage.length);
    if (materialByteLength > MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1) {
      throw new Error(
        `CEK program material exceeds ${MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1.toString()} bytes`,
      );
    }
  }
  return normalized;
};

const canonicalProgramEnvelopeV1 = (
  envelope: MidgardCekProgramEnvelopeV1,
): {
  readonly envelope: MidgardCekProgramEnvelopeV1;
  readonly identity: string;
} => {
  const encoded = encodeMidgardCekProgramEnvelopeV1(envelope);
  return {
    envelope: decodeMidgardCekProgramEnvelopeV1(encoded),
    identity: encoded.toString("hex"),
  };
};

const greatestPowerOfTwoBelow = (value: bigint): bigint => {
  let power = 1n;
  while (power * 2n < value) {
    power *= 2n;
  }
  return power;
};

type SemanticConstantTypeV1 =
  | { readonly kind: "integer" }
  | { readonly kind: "bytes" }
  | { readonly kind: "string" }
  | { readonly kind: "unit" }
  | { readonly kind: "boolean" }
  | {
      readonly kind: "list";
      readonly element: SemanticConstantTypeV1;
    }
  | {
      readonly kind: "pair";
      readonly first: SemanticConstantTypeV1;
      readonly second: SemanticConstantTypeV1;
    }
  | { readonly kind: "data" }
  | { readonly kind: "blsG1" }
  | { readonly kind: "blsG2" }
  | { readonly kind: "blsMillerLoop" };

const decodeSemanticConstantTypeV1 = (
  typeCbor: Uint8Array,
): SemanticConstantTypeV1 => {
  if (typeCbor.length > MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES_V1) {
    throw new Error(
      `CEK constant type exceeds the ${MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES_V1.toString()}-byte L1 bound`,
    );
  }
  const decoded = LucidData.from(Buffer.from(typeCbor).toString("hex"));
  if (
    !Array.isArray(decoded) ||
    !decoded.every((tag) => typeof tag === "bigint")
  ) {
    throw new Error("CEK constant type payload is not an integer list");
  }
  const stack: SemanticConstantTypeV1[] = [];
  for (let offset = decoded.length - 1; offset >= 0; offset -= 1) {
    const tag = decoded[offset];
    if (tag === 0n) stack.push({ kind: "integer" });
    else if (tag === 1n) stack.push({ kind: "bytes" });
    else if (tag === 2n) stack.push({ kind: "string" });
    else if (tag === 3n) stack.push({ kind: "unit" });
    else if (tag === 4n) stack.push({ kind: "boolean" });
    else if (tag === 8n) stack.push({ kind: "data" });
    else if (tag === 9n) stack.push({ kind: "blsG1" });
    else if (tag === 10n) stack.push({ kind: "blsG2" });
    else if (tag === 11n) stack.push({ kind: "blsMillerLoop" });
    else if (tag === 5n) {
      const element = stack.pop();
      if (element === undefined) {
        throw new Error("CEK constant list type is missing its element type");
      }
      stack.push({ kind: "list", element });
    } else if (tag === 6n) {
      const first = stack.pop();
      const second = stack.pop();
      if (first === undefined || second === undefined) {
        throw new Error("CEK constant pair type is missing a child type");
      }
      stack.push({ kind: "pair", first, second });
    } else {
      throw new Error("CEK constant has an unknown semantic type tag");
    }
  }
  if (stack.length !== 1) {
    throw new Error("CEK constant type payload has trailing tags");
  }
  return stack[0]!;
};

const semanticIntegerMemoryV1 = (value: bigint): bigint => {
  const doubled = value < 0n ? (-value - 1n) * 2n : value * 2n;
  if (doubled === 0n) return 1n;
  return BigInt(Math.ceil(doubled.toString(2).length / 8));
};

type SemanticConstrV1 = {
  readonly kind: "constr";
  readonly constructor: bigint;
  readonly fields: readonly SemanticDataValueV1[];
};

type SemanticDataValueV1 =
  | bigint
  | string
  | readonly SemanticDataValueV1[]
  | ReadonlyMap<SemanticDataValueV1, SemanticDataValueV1>
  | SemanticConstrV1;

const isSemanticConstrV1 = (
  value: SemanticDataValueV1,
): value is SemanticConstrV1 =>
  typeof value === "object" &&
  value !== null &&
  !Array.isArray(value) &&
  !(value instanceof Map) &&
  "kind" in value &&
  value.kind === "constr";

const semanticCborHeaderV1 = (major: number, value: bigint): Buffer => {
  if (value < 0n) {
    throw new Error("CEK semantic CBOR length must be non-negative");
  }
  const prefix = major << 5;
  if (value < 24n) return Buffer.from([prefix | Number(value)]);
  if (value <= 0xffn) {
    return Buffer.from([prefix | 24, Number(value)]);
  }
  if (value <= 0xffffn) {
    const result = Buffer.alloc(3);
    result[0] = prefix | 25;
    result.writeUInt16BE(Number(value), 1);
    return result;
  }
  if (value <= 0xffff_ffffn) {
    const result = Buffer.alloc(5);
    result[0] = prefix | 26;
    result.writeUInt32BE(Number(value), 1);
    return result;
  }
  if (value <= 0xffff_ffff_ffff_ffffn) {
    const result = Buffer.alloc(9);
    result[0] = prefix | 27;
    result.writeBigUInt64BE(value, 1);
    return result;
  }
  throw new Error("CEK semantic CBOR length exceeds uint64");
};

const encodeSemanticBytesV1 = (value: Buffer): Buffer => {
  if (value.length <= 64) {
    return Buffer.concat([
      semanticCborHeaderV1(2, BigInt(value.length)),
      value,
    ]);
  }
  const chunks: Buffer[] = [Buffer.from([0x5f])];
  for (let offset = 0; offset < value.length; offset += 64) {
    const chunk = value.subarray(offset, offset + 64);
    chunks.push(semanticCborHeaderV1(2, BigInt(chunk.length)), chunk);
  }
  chunks.push(Buffer.from([0xff]));
  return Buffer.concat(chunks);
};

const encodeSemanticListV1 = (
  values: readonly SemanticDataValueV1[],
): Buffer =>
  values.length === 0
    ? Buffer.from([0x80])
    : Buffer.concat([
        Buffer.from([0x9f]),
        ...values.map(encodeSemanticDataV1),
        Buffer.from([0xff]),
      ]);

const encodeSemanticDataV1 = (value: SemanticDataValueV1): Buffer => {
  if (typeof value === "bigint") {
    return Buffer.from(LucidData.to(value), "hex");
  }
  if (typeof value === "string") {
    return encodeSemanticBytesV1(Buffer.from(value, "hex"));
  }
  if (Array.isArray(value)) {
    return encodeSemanticListV1(value);
  }
  if (value instanceof Map) {
    return Buffer.concat([
      semanticCborHeaderV1(5, BigInt(value.size)),
      ...[...value.entries()].flatMap(([key, mapped]) => [
        encodeSemanticDataV1(key),
        encodeSemanticDataV1(mapped),
      ]),
    ]);
  }
  if (isSemanticConstrV1(value)) {
    const fields = encodeSemanticListV1(value.fields);
    if (value.constructor <= 6n) {
      return Buffer.concat([
        semanticCborHeaderV1(6, 121n + value.constructor),
        fields,
      ]);
    }
    if (value.constructor <= 127n) {
      return Buffer.concat([
        semanticCborHeaderV1(6, 1280n + value.constructor - 7n),
        fields,
      ]);
    }
    return Buffer.concat([
      semanticCborHeaderV1(6, 102n),
      Buffer.from([0x82]),
      Buffer.from(LucidData.to(value.constructor), "hex"),
      fields,
    ]);
  }
  throw new Error("CEK constant contains unknown semantic Data");
};

type SemanticDataSummaryV1 = {
  readonly root: Hash32;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type SemanticListSummaryV1 = {
  readonly root: Hash32;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

const commitSemanticDataV1 = (
  value: SemanticDataValueV1,
): SemanticDataSummaryV1 => {
  const canonicalCbor = encodeSemanticDataV1(value);
  const commitList = (
    items: readonly SemanticDataValueV1[],
  ): SemanticListSummaryV1 => {
    let summary: SemanticListSummaryV1 = {
      root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = items.length - 1; index >= 0; index -= 1) {
      const head = commitSemanticDataV1(items[index]!);
      const node: MidgardCekDataListNodeV1 = {
        head: head.root,
        headCborLength: head.cborLength,
        headMemory: head.memory,
        tail: summary.root,
        length: summary.length + 1n,
        payloadCborLength: head.cborLength + summary.payloadCborLength,
        memory: head.memory + summary.memory,
      };
      summary = {
        root: hashMidgardCekDataListNodeV1(node),
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };
  const commitPairs = (
    entries: readonly (readonly [SemanticDataValueV1, SemanticDataValueV1])[],
  ): SemanticListSummaryV1 => {
    let summary: SemanticListSummaryV1 = {
      root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = entries.length - 1; index >= 0; index -= 1) {
      const [keyValue, mappedValue] = entries[index]!;
      const key = commitSemanticDataV1(keyValue);
      const mapped = commitSemanticDataV1(mappedValue);
      const node: MidgardCekDataPairNodeV1 = {
        key: key.root,
        keyCborLength: key.cborLength,
        keyMemory: key.memory,
        value: mapped.root,
        valueCborLength: mapped.cborLength,
        valueMemory: mapped.memory,
        tail: summary.root,
        length: summary.length + 1n,
        payloadCborLength:
          key.cborLength + mapped.cborLength + summary.payloadCborLength,
        memory: key.memory + mapped.memory + summary.memory,
      };
      summary = {
        root: hashMidgardCekDataPairNodeV1(node),
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };

  let node: MidgardCekDataNodeV1;
  if (typeof value === "bigint") {
    node = {
      kind: "integer",
      cborRoot: commitMidgardCekBlobV1(canonicalCbor).root,
      cborLength: BigInt(canonicalCbor.length),
      memory: 4n + semanticIntegerMemoryV1(value),
    };
  } else if (typeof value === "string") {
    const bytes = Buffer.from(value, "hex");
    node = {
      kind: "bytes",
      bytesRoot: commitMidgardCekBlobV1(bytes).root,
      bytesLength: BigInt(bytes.length),
      cborLength: midgardCekDataBytesCborLengthV1(BigInt(bytes.length)),
      memory: 4n + BigInt(Math.max(1, bytes.length)),
    };
  } else if (Array.isArray(value)) {
    const items = commitList(value);
    node = {
      kind: "list",
      itemsCount: items.length,
      itemsRoot: items.root,
      cborLength: midgardCekDataListCborLengthV1(
        items.length,
        items.payloadCborLength,
      ),
      memory: 4n + items.memory,
    };
  } else if (value instanceof Map) {
    const entries = commitPairs([...value.entries()] as readonly (readonly [
      SemanticDataValueV1,
      SemanticDataValueV1,
    ])[]);
    node = {
      kind: "map",
      entriesCount: entries.length,
      entriesRoot: entries.root,
      cborLength: midgardCekDataListCborLengthV1(
        entries.length,
        entries.payloadCborLength,
      ),
      memory: 4n + entries.memory,
    };
  } else if (isSemanticConstrV1(value)) {
    const constructor = value.constructor;
    const fields = commitList(value.fields);
    if (constructor <= 127n) {
      node = {
        kind: "constrSmall",
        constructor,
        fieldsCount: fields.length,
        fieldsRoot: fields.root,
        cborLength: midgardCekDataConstrCborLengthV1(
          constructor,
          fields.length,
          fields.payloadCborLength,
        ),
        memory: 4n + fields.memory,
      };
    } else {
      const constructorCbor = Buffer.from(LucidData.to(constructor), "hex");
      node = {
        kind: "constrLarge",
        constructorCborRoot: commitMidgardCekBlobV1(constructorCbor).root,
        constructorCborLength: BigInt(constructorCbor.length),
        constructorMemory: 4n + semanticIntegerMemoryV1(constructor),
        fieldsCount: fields.length,
        fieldsRoot: fields.root,
        cborLength: midgardCekDataConstrCborLengthV1(
          constructor,
          fields.length,
          fields.payloadCborLength,
        ),
        memory: 4n + fields.memory,
      };
    }
  } else {
    throw new Error("CEK constant contains unknown Plutus Data");
  }
  if (node.cborLength !== BigInt(canonicalCbor.length)) {
    throw new Error("CEK semantic Data CBOR summary is not exact");
  }
  return {
    root: hashMidgardCekDataNodeV1(node),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

const semanticConstantMemoryV1 = (
  type: SemanticConstantTypeV1,
  value: SemanticDataValueV1,
): bigint => {
  if (type.kind === "integer") {
    if (typeof value !== "bigint") {
      throw new Error("CEK integer payload is not an integer");
    }
    return semanticIntegerMemoryV1(value);
  }
  if (type.kind === "bytes" || type.kind === "string") {
    if (typeof value !== "string") {
      throw new Error("CEK bytes payload is not bytes");
    }
    return BigInt(Math.max(1, Buffer.from(value, "hex").length));
  }
  if (type.kind === "unit" || type.kind === "boolean") return 1n;
  if (type.kind === "list") {
    if (!Array.isArray(value)) {
      throw new Error("CEK list payload is not a list");
    }
    return value.reduce(
      (total, item) => total + semanticConstantMemoryV1(type.element, item),
      0n,
    );
  }
  if (type.kind === "pair") {
    if (
      !isSemanticConstrV1(value) ||
      value.constructor !== 0n ||
      value.fields.length !== 2
    ) {
      throw new Error("CEK pair payload is not a pair");
    }
    return (
      semanticConstantMemoryV1(type.first, value.fields[0]!) +
      semanticConstantMemoryV1(type.second, value.fields[1]!)
    );
  }
  if (type.kind === "data") return commitSemanticDataV1(value).memory;
  if (type.kind === "blsG1") return 48n;
  if (type.kind === "blsG2") return 96n;
  return 192n;
};

const verifyOneProgramMaterialV1 = (
  envelope: MidgardCekProgramEnvelopeV1,
  material: NormalizedProgramMaterialV1,
  cache: ProgramMaterialBundleCacheV1,
  options: {
    readonly includeConstants: boolean;
    readonly onBlobMaterialized?: (rootHex: string, byteLength: bigint) => void;
    readonly onConstantMaterialized?: (
      valueRootHex: string,
      payloadByteLength: bigint,
    ) => void;
  },
): MidgardCekProgramMaterialVerificationV1 => {
  const reachable = new Set<string>();
  const dependencies = new Map<string, readonly string[]>();
  const decodedBlobs = new Map<string, MidgardCekDecodedProgramBlobV1>();
  const decodedValues = new Map<string, MidgardCekDecodedProgramValueV1>();
  const decodedDataNodes = new Map<string, MidgardCekDataNodeV1>();
  const decodedDataLists = new Map<string, MidgardCekDataListNodeV1>();
  const decodedDataPairs = new Map<string, MidgardCekDataPairNodeV1>();
  const blobLengthExpectations = new Map<string, Set<bigint>>();
  const blobMaximumLengthExpectations = new Map<string, Set<bigint>>();
  const sequenceLengthExpectations = new Map<string, Set<bigint>>();
  const dataListLengthExpectations = new Map<string, Set<bigint>>();
  const dataPairLengthExpectations = new Map<string, Set<bigint>>();
  const tasks: ProgramMaterialTaskV1[] = [
    {
      kind: "term",
      root: exactHash(envelope.termRoot, "cek_program.root") as Hash32,
    },
  ];

  const rootKey = (root: Uint8Array): string =>
    Buffer.from(root).toString("hex");
  const addDependency = (parent: string, child: Uint8Array): void => {
    const childKey = rootKey(child);
    const prior = dependencies.get(parent) ?? [];
    dependencies.set(parent, [...prior, childKey]);
  };
  const expectEntry = (
    task: ProgramMaterialTaskV1,
  ): {
    readonly key: string;
    readonly entry: MidgardCekProgramMaterialEntryV1;
  } => {
    const key = rootKey(task.root);
    const entry = material.get(key);
    if (entry === undefined) {
      throw new Error(`CEK program material is missing root ${key}`);
    }
    const expectedKinds =
      task.kind === "blob"
        ? (["blobChunk", "blobBranch"] as const)
        : ([task.kind] as const);
    if (
      !(expectedKinds as readonly MidgardCekProgramMaterialKindV1[]).includes(
        entry.kind,
      )
    ) {
      throw new Error(
        `CEK program material root ${key} has kind ${entry.kind}, expected ${expectedKinds.join(" or ")}`,
      );
    }
    return { key, entry };
  };
  const noteExpectation = (
    expectations: Map<string, Set<bigint>>,
    key: string,
    value: bigint | undefined,
  ): void => {
    if (value === undefined) return;
    const values = expectations.get(key) ?? new Set<bigint>();
    values.add(value);
    expectations.set(key, values);
  };

  for (let cursor = 0; cursor < tasks.length; cursor += 1) {
    const task = tasks[cursor]!;
    if (
      task.kind === "sequence" &&
      task.length === 0n &&
      Buffer.from(task.root).equals(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)
    ) {
      continue;
    }
    if (
      task.kind === "dataList" &&
      task.length === 0n &&
      Buffer.from(task.root).equals(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1)
    ) {
      continue;
    }
    if (
      task.kind === "dataPair" &&
      task.length === 0n &&
      Buffer.from(task.root).equals(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1)
    ) {
      continue;
    }
    const { key, entry } = expectEntry(task);
    if (task.kind === "sequence") {
      noteExpectation(sequenceLengthExpectations, key, task.length);
    } else if (task.kind === "blob") {
      noteExpectation(blobLengthExpectations, key, task.byteLength);
      noteExpectation(blobMaximumLengthExpectations, key, task.maxByteLength);
    } else if (task.kind === "dataList") {
      noteExpectation(dataListLengthExpectations, key, task.length);
    } else if (task.kind === "dataPair") {
      noteExpectation(dataPairLengthExpectations, key, task.length);
    }
    if (reachable.has(key)) {
      continue;
    }
    reachable.add(key);
    dependencies.set(key, []);

    if (task.kind === "term") {
      const term = decodeMidgardCekProgramTermPreimageV1(entry.preimage);
      switch (term.kind) {
        case "variable":
        case "error":
        case "builtin":
          break;
        case "unaryTerm":
          addDependency(key, term.child);
          tasks.push({ kind: "term", root: term.child });
          break;
        case "application":
          addDependency(key, term.function);
          addDependency(key, term.argument);
          tasks.push(
            { kind: "term", root: term.function },
            { kind: "term", root: term.argument },
          );
          break;
        case "constant":
          addDependency(key, term.value);
          tasks.push({ kind: "value", root: term.value });
          break;
        case "contextConstant":
          throw new Error(
            "CEK source-program material contains a runtime-only context constant",
          );
        case "constr":
          if (
            term.count === 0n &&
            !Buffer.from(term.sequence).equals(
              MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
            )
          ) {
            throw new Error(
              "empty CEK constr sequence must use the canonical empty root",
            );
          }
          if (term.count > 0n) {
            addDependency(key, term.sequence);
            tasks.push({
              kind: "sequence",
              root: term.sequence,
              length: term.count,
            });
          }
          break;
        case "case":
          addDependency(key, term.scrutinee);
          tasks.push({ kind: "term", root: term.scrutinee });
          if (
            term.count === 0n &&
            !Buffer.from(term.sequence).equals(
              MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
            )
          ) {
            throw new Error(
              "empty CEK case sequence must use the canonical empty root",
            );
          }
          if (term.count > 0n) {
            addDependency(key, term.sequence);
            tasks.push({
              kind: "sequence",
              root: term.sequence,
              length: term.count,
            });
          }
          break;
      }
      continue;
    }

    if (task.kind === "value") {
      const value = decodeMidgardCekProgramValuePreimageV1(entry.preimage);
      decodedValues.set(key, value);
      if (!Buffer.from(value.payloadRoot).equals(value.semanticRoot)) {
        throw new Error(
          "CEK constant payload root must equal its canonical semantic root",
        );
      }
      if (
        value.payloadLength >
        BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1)
      ) {
        throw new Error(
          `CEK source constant payload exceeds the ${MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1.toString()}-byte L1 proof envelope`,
        );
      }
      addDependency(key, value.typeRoot);
      addDependency(key, value.semanticRoot);
      tasks.push(
        {
          kind: "blob",
          root: value.typeRoot,
          maxByteLength: BigInt(MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES_V1),
        },
        { kind: "dataNode", root: value.semanticRoot },
      );
      continue;
    }

    if (task.kind === "sequence") {
      const sequence = decodeMidgardCekProgramSequencePreimageV1(
        entry.preimage,
      );
      if (sequence.length !== task.length) {
        throw new Error(
          `CEK sequence root ${key} declares ${sequence.length.toString()} items, expected ${task.length.toString()}`,
        );
      }
      addDependency(key, sequence.head);
      tasks.push({ kind: "term", root: sequence.head });
      if (sequence.length === 1n) {
        if (
          !Buffer.from(sequence.tail).equals(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)
        ) {
          throw new Error(
            "one-item CEK sequence must end at the canonical empty root",
          );
        }
      } else {
        addDependency(key, sequence.tail);
        tasks.push({
          kind: "sequence",
          root: sequence.tail,
          length: sequence.length - 1n,
        });
      }
      continue;
    }

    if (task.kind === "dataNode") {
      const node = decodeMidgardCekDataNodeV1(entry.preimage);
      decodedDataNodes.set(key, node);
      if (node.kind === "constrSmall" || node.kind === "constrLarge") {
        if (node.fieldsCount === 0n) {
          if (
            !Buffer.from(node.fieldsRoot).equals(
              MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
            )
          ) {
            throw new Error(
              "empty CEK Data constructor must use the canonical fields root",
            );
          }
        } else {
          addDependency(key, node.fieldsRoot);
          tasks.push({
            kind: "dataList",
            root: node.fieldsRoot as Hash32,
            length: node.fieldsCount,
          });
        }
        if (node.kind === "constrLarge") {
          addDependency(key, node.constructorCborRoot);
          tasks.push({
            kind: "blob",
            root: node.constructorCborRoot as Hash32,
            byteLength: node.constructorCborLength,
          });
        }
      } else if (node.kind === "map") {
        if (node.entriesCount === 0n) {
          if (
            !Buffer.from(node.entriesRoot).equals(
              MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
            )
          ) {
            throw new Error(
              "empty CEK Data map must use the canonical entries root",
            );
          }
        } else {
          addDependency(key, node.entriesRoot);
          tasks.push({
            kind: "dataPair",
            root: node.entriesRoot as Hash32,
            length: node.entriesCount,
          });
        }
      } else if (node.kind === "list") {
        if (node.itemsCount === 0n) {
          if (
            !Buffer.from(node.itemsRoot).equals(
              MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
            )
          ) {
            throw new Error(
              "empty CEK Data list must use the canonical items root",
            );
          }
        } else {
          addDependency(key, node.itemsRoot);
          tasks.push({
            kind: "dataList",
            root: node.itemsRoot as Hash32,
            length: node.itemsCount,
          });
        }
      } else if (node.kind === "integer") {
        addDependency(key, node.cborRoot);
        tasks.push({
          kind: "blob",
          root: node.cborRoot as Hash32,
          byteLength: node.cborLength,
        });
      } else {
        addDependency(key, node.bytesRoot);
        tasks.push({
          kind: "blob",
          root: node.bytesRoot as Hash32,
          byteLength: node.bytesLength,
        });
      }
      continue;
    }

    if (task.kind === "dataList") {
      const node = decodeMidgardCekDataListNodeV1(entry.preimage);
      if (node.length !== task.length) {
        throw new Error(
          `CEK Data list root ${key} declares ${node.length.toString()} items, expected ${task.length.toString()}`,
        );
      }
      decodedDataLists.set(key, node);
      addDependency(key, node.head);
      tasks.push({ kind: "dataNode", root: node.head as Hash32 });
      if (node.length === 1n) {
        if (
          !Buffer.from(node.tail).equals(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1)
        ) {
          throw new Error(
            "one-item CEK Data list must end at the canonical empty root",
          );
        }
      } else {
        addDependency(key, node.tail);
        tasks.push({
          kind: "dataList",
          root: node.tail as Hash32,
          length: node.length - 1n,
        });
      }
      continue;
    }

    if (task.kind === "dataPair") {
      const node = decodeMidgardCekDataPairNodeV1(entry.preimage);
      if (node.length !== task.length) {
        throw new Error(
          `CEK Data pair root ${key} declares ${node.length.toString()} items, expected ${task.length.toString()}`,
        );
      }
      decodedDataPairs.set(key, node);
      addDependency(key, node.key);
      addDependency(key, node.value);
      tasks.push(
        { kind: "dataNode", root: node.key as Hash32 },
        { kind: "dataNode", root: node.value as Hash32 },
      );
      if (node.length === 1n) {
        if (
          !Buffer.from(node.tail).equals(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1)
        ) {
          throw new Error(
            "one-item CEK Data pair list must end at the canonical empty root",
          );
        }
      } else {
        addDependency(key, node.tail);
        tasks.push({
          kind: "dataPair",
          root: node.tail as Hash32,
          length: node.length - 1n,
        });
      }
      continue;
    }

    if (entry.kind !== "blobChunk" && entry.kind !== "blobBranch") {
      throw new Error("CEK blob task resolved to non-blob material");
    }
    const blob = decodeMidgardCekProgramBlobPreimageV1(
      entry.kind,
      entry.preimage,
    );
    const declaredBlobByteLength =
      blob.kind === "chunk" ? BigInt(blob.bytes.length) : blob.byteLength;
    if (
      task.maxByteLength !== undefined &&
      declaredBlobByteLength > task.maxByteLength
    ) {
      throw new Error(
        `CEK blob root ${key} declares ${declaredBlobByteLength.toString()} bytes, exceeding ${task.maxByteLength.toString()}`,
      );
    }
    decodedBlobs.set(key, blob);
    if (blob.kind === "branch") {
      addDependency(key, blob.left);
      addDependency(key, blob.right);
      tasks.push(
        { kind: "blob", root: blob.left },
        { kind: "blob", root: blob.right },
      );
    }
  }

  for (const [key, expected] of sequenceLengthExpectations) {
    const entry = material.get(key)!;
    const actual = decodeMidgardCekProgramSequencePreimageV1(
      entry.preimage,
    ).length;
    for (const length of expected) {
      if (actual !== length) {
        throw new Error(
          `CEK sequence root ${key} has inconsistent length expectations`,
        );
      }
    }
  }
  for (const [key, expected] of dataListLengthExpectations) {
    const actual = decodedDataLists.get(key)?.length;
    if (actual === undefined) {
      throw new Error(`CEK Data list root ${key} was not decoded`);
    }
    for (const length of expected) {
      if (actual !== length) {
        throw new Error(
          `CEK Data list root ${key} has inconsistent length expectations`,
        );
      }
    }
  }
  for (const [key, expected] of dataPairLengthExpectations) {
    const actual = decodedDataPairs.get(key)?.length;
    if (actual === undefined) {
      throw new Error(`CEK Data pair root ${key} was not decoded`);
    }
    for (const length of expected) {
      if (actual !== length) {
        throw new Error(
          `CEK Data pair root ${key} has inconsistent length expectations`,
        );
      }
    }
  }

  const colors = new Map<string, 1 | 2>();
  const postorder: string[] = [];
  for (const start of reachable) {
    if (colors.get(start) === 2) continue;
    const stack: Array<{ readonly key: string; readonly exit: boolean }> = [
      { key: start, exit: false },
    ];
    while (stack.length > 0) {
      const current = stack.pop()!;
      const color = colors.get(current.key);
      if (current.exit) {
        colors.set(current.key, 2);
        postorder.push(current.key);
        continue;
      }
      if (color === 2) continue;
      if (color === 1) {
        throw new Error("CEK program material graph contains a cycle");
      }
      colors.set(current.key, 1);
      stack.push({ key: current.key, exit: true });
      const children = dependencies.get(current.key) ?? [];
      for (let index = children.length - 1; index >= 0; index -= 1) {
        const child = children[index]!;
        if (colors.get(child) === 1) {
          throw new Error("CEK program material graph contains a cycle");
        }
        if (colors.get(child) !== 2) {
          stack.push({ key: child, exit: false });
        }
      }
    }
  }

  type BlobShape = {
    readonly byteLength: bigint;
    readonly leafCount: bigint;
    readonly lastLeafLength: number;
  };
  const blobShapes = new Map<string, BlobShape>();
  for (const key of postorder) {
    const blob = decodedBlobs.get(key);
    if (blob === undefined) continue;
    if (blob.kind === "chunk") {
      blobShapes.set(key, {
        byteLength: BigInt(blob.bytes.length),
        leafCount: 1n,
        lastLeafLength: blob.bytes.length,
      });
      continue;
    }
    const left = blobShapes.get(rootKey(blob.left));
    const right = blobShapes.get(rootKey(blob.right));
    if (left === undefined || right === undefined) {
      throw new Error("CEK blob branch child is not a canonical blob node");
    }
    if (
      left.byteLength === 0n ||
      right.byteLength === 0n ||
      left.lastLeafLength !== MIDGARD_CEK_BLOB_CHUNK_BYTES
    ) {
      throw new Error(
        "CEK blob branch must contain full non-final chunks and no empty child",
      );
    }
    const leafCount = left.leafCount + right.leafCount;
    if (left.leafCount !== greatestPowerOfTwoBelow(leafCount)) {
      throw new Error("CEK blob branch is not canonically left-balanced");
    }
    const byteLength = left.byteLength + right.byteLength;
    if (byteLength !== blob.byteLength) {
      throw new Error(
        "CEK blob branch byte length does not match its children",
      );
    }
    blobShapes.set(key, {
      byteLength,
      leafCount,
      lastLeafLength: right.lastLeafLength,
    });
  }

  for (const [key, expectedLengths] of blobLengthExpectations) {
    const shape = blobShapes.get(key);
    if (shape === undefined) {
      throw new Error(`CEK blob root ${key} was not reconstructed`);
    }
    for (const expected of expectedLengths) {
      if (shape.byteLength !== expected) {
        throw new Error(
          `CEK blob root ${key} has ${shape.byteLength.toString()} bytes, expected ${expected.toString()}`,
        );
      }
    }
  }
  for (const [key, maximumLengths] of blobMaximumLengthExpectations) {
    const shape = blobShapes.get(key);
    if (shape === undefined) {
      throw new Error(`CEK blob root ${key} was not reconstructed`);
    }
    for (const maximum of maximumLengths) {
      if (shape.byteLength > maximum) {
        throw new Error(
          `CEK blob root ${key} has ${shape.byteLength.toString()} bytes, exceeding ${maximum.toString()}`,
        );
      }
    }
  }

  const materializeBlob = (
    root: Uint8Array,
    maximumByteLength: bigint,
    fieldName: string,
  ): Buffer => {
    const key = rootKey(root);
    const shape = blobShapes.get(key);
    if (shape === undefined) {
      throw new Error(`${fieldName} blob is missing`);
    }
    if (shape.byteLength > maximumByteLength) {
      throw new Error(
        `${fieldName} blob has ${shape.byteLength.toString()} bytes, exceeding ${maximumByteLength.toString()}`,
      );
    }
    const cached = cache.materializedBlobs.get(key);
    if (cached !== undefined) return cached;

    const leaves: Buffer[] = [];
    const stack = [key];
    while (stack.length > 0) {
      const currentKey = stack.pop()!;
      const blob = decodedBlobs.get(currentKey);
      if (blob === undefined) {
        throw new Error(`${fieldName} blob has an incomplete branch`);
      }
      if (blob.kind === "chunk") {
        leaves.push(blob.bytes);
      } else {
        stack.push(rootKey(blob.right), rootKey(blob.left));
      }
    }
    const materialized = Buffer.concat(leaves, Number(shape.byteLength));
    cache.materializedBlobs.set(key, materialized);
    try {
      options.onBlobMaterialized?.(key, shape.byteLength);
    } catch {
      // Allocation observability must not change verification semantics.
    }
    return materialized;
  };

  for (const key of postorder) {
    const listNode = decodedDataLists.get(key);
    if (listNode !== undefined) {
      const head = decodedDataNodes.get(rootKey(listNode.head));
      if (head === undefined) {
        throw new Error("CEK Data list head is not a Data node");
      }
      const tail =
        listNode.length === 1n
          ? null
          : decodedDataLists.get(rootKey(listNode.tail));
      if (listNode.length > 1n && tail === undefined) {
        throw new Error("CEK Data list tail is not a list node");
      }
      const tailPayload = tail?.payloadCborLength ?? 0n;
      const tailMemory = tail?.memory ?? 0n;
      if (
        listNode.headCborLength !== head.cborLength ||
        listNode.headMemory !== head.memory ||
        listNode.payloadCborLength !== head.cborLength + tailPayload ||
        listNode.memory !== head.memory + tailMemory
      ) {
        throw new Error("CEK Data list cumulative summary is invalid");
      }
      continue;
    }

    const pairNode = decodedDataPairs.get(key);
    if (pairNode !== undefined) {
      const keyNode = decodedDataNodes.get(rootKey(pairNode.key));
      const valueNode = decodedDataNodes.get(rootKey(pairNode.value));
      if (keyNode === undefined || valueNode === undefined) {
        throw new Error("CEK Data map entry child is not a Data node");
      }
      const tail =
        pairNode.length === 1n
          ? null
          : decodedDataPairs.get(rootKey(pairNode.tail));
      if (pairNode.length > 1n && tail === undefined) {
        throw new Error("CEK Data map-entry tail is not a pair node");
      }
      const tailPayload = tail?.payloadCborLength ?? 0n;
      const tailMemory = tail?.memory ?? 0n;
      if (
        pairNode.keyCborLength !== keyNode.cborLength ||
        pairNode.keyMemory !== keyNode.memory ||
        pairNode.valueCborLength !== valueNode.cborLength ||
        pairNode.valueMemory !== valueNode.memory ||
        pairNode.payloadCborLength !==
          keyNode.cborLength + valueNode.cborLength + tailPayload ||
        pairNode.memory !== keyNode.memory + valueNode.memory + tailMemory
      ) {
        throw new Error("CEK Data map-entry cumulative summary is invalid");
      }
      continue;
    }

    const dataNode = decodedDataNodes.get(key);
    if (dataNode === undefined) continue;
    if (dataNode.kind === "constrSmall" || dataNode.kind === "constrLarge") {
      const fields =
        dataNode.fieldsCount === 0n
          ? null
          : decodedDataLists.get(rootKey(dataNode.fieldsRoot));
      if (dataNode.fieldsCount > 0n && fields === undefined) {
        throw new Error("CEK Data constructor fields are incomplete");
      }
      const fieldsPayload = fields?.payloadCborLength ?? 0n;
      const fieldsMemory = fields?.memory ?? 0n;
      const expectedLength =
        dataNode.kind === "constrSmall"
          ? midgardCekDataConstrCborLengthV1(
              dataNode.constructor,
              dataNode.fieldsCount,
              fieldsPayload,
            )
          : 3n +
            dataNode.constructorCborLength +
            midgardCekDataListCborLengthV1(dataNode.fieldsCount, fieldsPayload);
      if (
        dataNode.cborLength !== expectedLength ||
        dataNode.memory !== 4n + fieldsMemory
      ) {
        throw new Error("CEK Data constructor summary is invalid");
      }
      if (
        dataNode.kind === "constrLarge" &&
        (dataNode.constructorCborLength === 0n ||
          dataNode.constructorMemory < 5n)
      ) {
        throw new Error("CEK large Data constructor summary is invalid");
      }
      continue;
    }
    if (dataNode.kind === "map") {
      const entries =
        dataNode.entriesCount === 0n
          ? null
          : decodedDataPairs.get(rootKey(dataNode.entriesRoot));
      if (dataNode.entriesCount > 0n && entries === undefined) {
        throw new Error("CEK Data map entries are incomplete");
      }
      if (
        dataNode.cborLength !==
          midgardCekDataListCborLengthV1(
            dataNode.entriesCount,
            entries?.payloadCborLength ?? 0n,
          ) ||
        dataNode.memory !== 4n + (entries?.memory ?? 0n)
      ) {
        throw new Error("CEK Data map summary is invalid");
      }
      continue;
    }
    if (dataNode.kind === "list") {
      const items =
        dataNode.itemsCount === 0n
          ? null
          : decodedDataLists.get(rootKey(dataNode.itemsRoot));
      if (dataNode.itemsCount > 0n && items === undefined) {
        throw new Error("CEK Data list items are incomplete");
      }
      if (
        dataNode.cborLength !==
          midgardCekDataListCborLengthV1(
            dataNode.itemsCount,
            items?.payloadCborLength ?? 0n,
          ) ||
        dataNode.memory !== 4n + (items?.memory ?? 0n)
      ) {
        throw new Error("CEK Data list summary is invalid");
      }
      continue;
    }
    if (dataNode.kind === "integer") {
      if (dataNode.cborLength === 0n || dataNode.memory < 5n) {
        throw new Error("CEK Data integer summary is invalid");
      }
      continue;
    }
    if (
      dataNode.cborLength !==
        midgardCekDataBytesCborLengthV1(dataNode.bytesLength) ||
      dataNode.memory !==
        4n + (dataNode.bytesLength === 0n ? 1n : dataNode.bytesLength)
    ) {
      throw new Error("CEK Data bytes summary is invalid");
    }
  }

  const reconstructedData = new Map<string, SemanticDataValueV1>();
  const reconstructDataList = (
    root: Uint8Array,
    length: bigint,
  ): readonly SemanticDataValueV1[] => {
    const items: SemanticDataValueV1[] = [];
    let cursor = rootKey(root);
    let remaining = length;
    while (remaining > 0n) {
      const link = decodedDataLists.get(cursor);
      if (link === undefined || link.length !== remaining) {
        throw new Error("CEK semantic Data list cannot be reconstructed");
      }
      items.push(reconstructData(link.head));
      cursor = rootKey(link.tail);
      remaining -= 1n;
    }
    if (cursor !== rootKey(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1)) {
      throw new Error("CEK semantic Data list has a non-empty tail");
    }
    return items;
  };
  const reconstructDataPairs = (
    root: Uint8Array,
    length: bigint,
  ): ReadonlyMap<SemanticDataValueV1, SemanticDataValueV1> => {
    const entries = new Map<SemanticDataValueV1, SemanticDataValueV1>();
    let cursor = rootKey(root);
    let remaining = length;
    while (remaining > 0n) {
      const link = decodedDataPairs.get(cursor);
      if (link === undefined || link.length !== remaining) {
        throw new Error("CEK semantic Data map cannot be reconstructed");
      }
      entries.set(reconstructData(link.key), reconstructData(link.value));
      cursor = rootKey(link.tail);
      remaining -= 1n;
    }
    if (cursor !== rootKey(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1)) {
      throw new Error("CEK semantic Data map has a non-empty tail");
    }
    return entries;
  };
  function reconstructData(root: Uint8Array): SemanticDataValueV1 {
    const key = rootKey(root);
    const cached = reconstructedData.get(key);
    if (cached !== undefined) return cached;
    const node = decodedDataNodes.get(key);
    if (node === undefined) {
      throw new Error("CEK semantic Data node is missing");
    }
    let value: SemanticDataValueV1;
    if (node.kind === "integer") {
      const bytes = materializeBlob(
        node.cborRoot,
        BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1),
        "CEK semantic integer",
      );
      const decoded = LucidData.from(bytes.toString("hex"));
      if (typeof decoded !== "bigint") {
        throw new Error("CEK semantic integer leaf is invalid");
      }
      value = decoded;
    } else if (node.kind === "bytes") {
      const bytes = materializeBlob(
        node.bytesRoot,
        BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1),
        "CEK semantic bytes",
      );
      value = bytes.toString("hex");
    } else if (node.kind === "list") {
      value = reconstructDataList(node.itemsRoot, node.itemsCount);
    } else if (node.kind === "map") {
      value = reconstructDataPairs(node.entriesRoot, node.entriesCount);
    } else {
      let constructor: bigint;
      if (node.kind === "constrLarge") {
        const bytes = materializeBlob(
          node.constructorCborRoot,
          BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1),
          "CEK semantic constructor",
        );
        const decoded = LucidData.from(bytes.toString("hex"));
        if (typeof decoded !== "bigint") {
          throw new Error("CEK semantic constructor index is invalid");
        }
        constructor = decoded;
      } else {
        constructor = node.constructor;
      }
      if (constructor < 0n) {
        throw new Error("CEK semantic constructor index must be non-negative");
      }
      value = {
        kind: "constr",
        constructor,
        fields: [...reconstructDataList(node.fieldsRoot, node.fieldsCount)],
      };
    }
    reconstructedData.set(key, value);
    return value;
  }

  const retainedConstants = new Map<
    string,
    {
      readonly typeCbor: Buffer;
      readonly payloadCbor: Buffer;
    }
  >();
  for (const [valueKey, value] of decodedValues) {
    let validated = cache.validatedConstants.get(valueKey);
    if (validated === undefined) {
      const typeCbor = materializeBlob(
        value.typeRoot,
        BigInt(MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES_V1),
        `CEK constant value ${valueKey} type`,
      );
      const decodedPayload = reconstructData(value.semanticRoot);
      const payloadCbor = encodeSemanticDataV1(decodedPayload);
      const semantic = commitSemanticDataV1(decodedPayload);
      if (!Buffer.from(semantic.root).equals(value.semanticRoot)) {
        throw new Error(
          `CEK constant value ${valueKey} semantic root does not match its canonical payload`,
        );
      }
      if (semantic.cborLength !== value.payloadLength) {
        throw new Error(
          `CEK constant value ${valueKey} payload length does not match its semantic tree`,
        );
      }
      const constantType = decodeSemanticConstantTypeV1(typeCbor);
      const memory = semanticConstantMemoryV1(constantType, decodedPayload);
      if (memory !== value.memory) {
        throw new Error(
          `CEK constant value ${valueKey} memory does not match its semantic payload`,
        );
      }
      validated = {
        typeCbor: Buffer.from(typeCbor),
        payloadCbor: Buffer.from(payloadCbor),
      };
      cache.validatedConstants.set(valueKey, validated);
      try {
        options.onConstantMaterialized?.(valueKey, BigInt(payloadCbor.length));
      } catch {
        // Allocation observability must not change verification semantics.
      }
    }
    if (options.includeConstants) {
      retainedConstants.set(valueKey, {
        typeCbor: Buffer.from(validated.typeCbor),
        payloadCbor: Buffer.from(validated.payloadCbor),
      });
    }
  }

  const materialByteLength = [...reachable].reduce(
    (total, key) => total + BigInt(material.get(key)!.preimage.length),
    0n,
  );
  if (BigInt(reachable.size) !== envelope.nodeCount) {
    throw new Error(
      `CEK program reaches ${reachable.size.toString()} material nodes, envelope declares ${envelope.nodeCount.toString()}`,
    );
  }
  if (materialByteLength !== envelope.materialByteLength) {
    throw new Error(
      `CEK program reaches ${materialByteLength.toString()} material bytes, envelope declares ${envelope.materialByteLength.toString()}`,
    );
  }

  const constants = options.includeConstants
    ? [...decodedValues.entries()].map(
        ([valueKey, value]): MidgardCekProgramConstantMaterialV1 => {
          const retained = retainedConstants.get(valueKey);
          if (retained === undefined) {
            throw new Error(`CEK constant value ${valueKey} was not retained`);
          }
          return Object.freeze({
            valueRoot: Buffer.from(valueKey, "hex") as Hash32,
            typeRoot: value.typeRoot,
            payloadRoot: value.payloadRoot,
            semanticRoot: value.semanticRoot,
            memory: value.memory,
            typeCbor: retained.typeCbor,
            payloadCbor: retained.payloadCbor,
          });
        },
      )
    : [];
  return Object.freeze({
    reachableRoots: reachable,
    nodeCount: BigInt(reachable.size),
    materialByteLength,
    constants: Object.freeze(constants),
  });
};

/**
 * Verifies exact content hashes, node syntax, typed graph edges, sequence
 * lengths, canonical blob shape, acyclicity, and envelope counts. By default
 * a one-program sidecar may not contain unreachable material.
 */
export const verifyMidgardCekProgramMaterialV1 = (
  envelope: MidgardCekProgramEnvelopeV1,
  entries: Iterable<MidgardCekProgramMaterialEntryV1>,
  options: MidgardCekProgramMaterialVerificationOptionsV1 = {},
): MidgardCekProgramMaterialVerificationV1 => {
  const exactEnvelope = canonicalProgramEnvelopeV1(envelope).envelope;
  const material = normalizeProgramMaterialV1(entries);
  const verified = verifyOneProgramMaterialV1(
    exactEnvelope,
    material,
    { materializedBlobs: new Map(), validatedConstants: new Map() },
    {
      includeConstants: true,
      onBlobMaterialized: options.onBlobMaterialized,
      onConstantMaterialized: options.onConstantMaterialized,
    },
  );
  if (
    options.allowUnreachable !== true &&
    verified.reachableRoots.size !== material.size
  ) {
    throw new Error("CEK program material contains unreachable nodes");
  }
  return verified;
};

const verifyProgramMaterialBundleV1 = (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
  entries: Iterable<MidgardCekProgramMaterialEntryV1>,
  options: MidgardCekProgramMaterialVerificationOptionsV1,
  includeResults: boolean,
): readonly MidgardCekProgramMaterialVerificationV1[] => {
  const envelopeIdentities: string[] = [];
  const uniqueEnvelopes = new Map<string, MidgardCekProgramEnvelopeV1>();
  let aggregateNodeVisits = 0n;
  let aggregateByteWork = 0n;
  for (const envelope of envelopes) {
    const canonical = canonicalProgramEnvelopeV1(envelope);
    envelopeIdentities.push(canonical.identity);
    if (uniqueEnvelopes.has(canonical.identity)) continue;
    aggregateNodeVisits += canonical.envelope.nodeCount;
    if (aggregateNodeVisits > MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS_V1) {
      throw new Error(
        `CEK program material bundle declares ${aggregateNodeVisits.toString()} aggregate unique-envelope node visits, exceeding ${MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS_V1.toString()}`,
      );
    }
    aggregateByteWork += canonical.envelope.materialByteLength;
    if (aggregateByteWork > MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK_V1) {
      throw new Error(
        `CEK program material bundle declares ${aggregateByteWork.toString()} aggregate unique-envelope byte work/result, exceeding ${MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK_V1.toString()}`,
      );
    }
    uniqueEnvelopes.set(canonical.identity, canonical.envelope);
  }

  const material = normalizeProgramMaterialV1(entries);
  if (envelopes.length === 0) {
    if (material.size !== 0 && options.allowUnreachable !== true) {
      throw new Error(
        "CEK program material is present without a program envelope",
      );
    }
    return Object.freeze([]);
  }
  const reached = new Set<string>();
  const verifiedByIdentity = new Map<
    string,
    MidgardCekProgramMaterialVerificationV1
  >();
  const cache: ProgramMaterialBundleCacheV1 = {
    materializedBlobs: new Map(),
    validatedConstants: new Map(),
  };
  for (const [identity, envelope] of uniqueEnvelopes) {
    const result = verifyOneProgramMaterialV1(envelope, material, cache, {
      includeConstants: includeResults,
      onBlobMaterialized: options.onBlobMaterialized,
      onConstantMaterialized: options.onConstantMaterialized,
    });
    for (const key of result.reachableRoots) reached.add(key);
    if (includeResults) verifiedByIdentity.set(identity, result);
  }
  if (options.allowUnreachable !== true && reached.size !== material.size) {
    throw new Error(
      "CEK program material bundle contains nodes unreachable from every envelope",
    );
  }
  return includeResults
    ? Object.freeze(
        envelopeIdentities.map((identity) => verifiedByIdentity.get(identity)!),
      )
    : Object.freeze([]);
};

/**
 * Verifies a DA block's deduplicated material against every referenced
 * program. Every supplied node must be reachable from at least one envelope.
 */
export const verifyMidgardCekProgramMaterialBundleV1 = (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
  entries: Iterable<MidgardCekProgramMaterialEntryV1>,
  options: MidgardCekProgramMaterialVerificationOptionsV1 = {},
): readonly MidgardCekProgramMaterialVerificationV1[] =>
  verifyProgramMaterialBundleV1(envelopes, entries, options, true);

/**
 * Strict coverage-only form for DA admission. It performs the same validation
 * but does not retain per-envelope constant buffers after verification.
 */
export const assertMidgardCekProgramMaterialBundleV1 = (
  envelopes: readonly MidgardCekProgramEnvelopeV1[],
  entries: Iterable<MidgardCekProgramMaterialEntryV1>,
  options: MidgardCekProgramMaterialVerificationOptionsV1 = {},
): void => {
  verifyProgramMaterialBundleV1(envelopes, entries, options, false);
};

export const MIDGARD_PROOF_SUBMISSION_ENVELOPE_V1_VERSION = 1n;
export const MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_V1_VERSION = 1n;

export type MidgardCekProgramMaterialSidecarV1 =
  readonly MidgardCekProgramMaterialEntryV1[];

export type MidgardProofSubmissionV1 = {
  readonly transactionCbor: Buffer;
  readonly programMaterial: MidgardCekProgramMaterialSidecarV1;
};

const canonicalizeMidgardCekProgramMaterialEntriesV1 = (
  entries: readonly MidgardCekProgramMaterialEntryV1[],
  label: string,
): readonly MidgardCekProgramMaterialEntryV1[] => {
  const material = [...entries]
    .map((entry) =>
      decodeMidgardCekProgramMaterialEntryV1(
        encodeMidgardCekProgramMaterialEntryV1(entry),
      ),
    )
    .sort((left, right) => compareBytes(left.root, right.root));
  for (let index = 1; index < material.length; index += 1) {
    if (Buffer.from(material[index - 1]!.root).equals(material[index]!.root)) {
      throw new Error(`${label} has duplicate material roots`);
    }
  }
  return Object.freeze(material);
};

const encodeMidgardCekProgramMaterialEntryListV1 = (
  entries: readonly MidgardCekProgramMaterialEntryV1[],
  label: string,
): readonly (readonly [Buffer, Buffer])[] =>
  canonicalizeMidgardCekProgramMaterialEntriesV1(entries, label).map(
    (entry) =>
      Object.freeze([
        Buffer.from(entry.root),
        encodeMidgardCekProgramMaterialDaValueV1(entry),
      ]) as readonly [Buffer, Buffer],
  );

const decodeMidgardCekProgramMaterialEntryListV1 = (
  source: Buffer,
  offset: number,
  label: string,
): {
  readonly entries: readonly MidgardCekProgramMaterialEntryV1[];
  readonly nextOffset: number;
} => {
  const materialHeader = readCborArrayHeader(source, offset, label);
  const programMaterial: MidgardCekProgramMaterialEntryV1[] = [];
  let cursor = materialHeader.nextOffset;
  let previousRoot: Buffer | undefined;
  for (let index = 0; index < materialHeader.length; index += 1) {
    const entryLabel = `${label}[${index.toString()}]`;
    const entryHeader = readCborArrayHeader(source, cursor, entryLabel);
    if (entryHeader.length !== 2) {
      throw new Error(
        "V1 program material entry must contain exactly two fields",
      );
    }
    const root = readCborBytes(
      source,
      entryHeader.nextOffset,
      `${entryLabel}.root`,
    );
    const exactRoot = exactHash(root.value, `${entryLabel}.root`);
    if (
      previousRoot !== undefined &&
      compareBytes(previousRoot, exactRoot) >= 0
    ) {
      throw new Error("V1 program material roots must be strictly sorted");
    }
    const value = readCborBytes(source, root.nextOffset, `${entryLabel}.value`);
    programMaterial.push(
      decodeMidgardCekProgramMaterialDaEntryV1(exactRoot, value.value),
    );
    previousRoot = exactRoot;
    cursor = value.nextOffset;
  }
  return Object.freeze({
    entries: Object.freeze(programMaterial),
    nextOffset: cursor,
  });
};

/**
 * Canonical storage/transport sidecar independent of the HTTP submission
 * wrapper. Keeping the version in the stored bytes makes replay and migration
 * fail closed if a future material encoding changes.
 */
export const encodeMidgardCekProgramMaterialSidecarV1 = (
  entries: MidgardCekProgramMaterialSidecarV1,
): Buffer =>
  encodeCbor([
    MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_V1_VERSION,
    encodeMidgardCekProgramMaterialEntryListV1(
      entries,
      "V1 program material sidecar",
    ),
  ]);

export const decodeMidgardCekProgramMaterialSidecarV1 = (
  bytes: Uint8Array,
): MidgardCekProgramMaterialSidecarV1 => {
  const source = Buffer.from(bytes);
  const header = readCborArrayHeader(source, 0, "program_material_sidecar");
  if (header.length !== 2) {
    throw new Error(
      "V1 program material sidecar must contain exactly two fields",
    );
  }
  const version = readCborUnsigned(
    source,
    header.nextOffset,
    "program_material_sidecar.version",
  );
  if (version.value !== MIDGARD_CEK_PROGRAM_MATERIAL_SIDECAR_V1_VERSION) {
    throw new Error(
      `unsupported V1 program material sidecar version ${version.value.toString()}`,
    );
  }
  const material = decodeMidgardCekProgramMaterialEntryListV1(
    source,
    version.nextOffset,
    "program_material_sidecar.entries",
  );
  if (material.nextOffset !== source.length) {
    throw new Error("V1 program material sidecar has trailing bytes");
  }
  if (
    !encodeMidgardCekProgramMaterialSidecarV1(material.entries).equals(source)
  ) {
    throw new Error("V1 program material sidecar CBOR is not canonical");
  }
  return material.entries;
};

/**
 * Merges transaction-local sidecars into the block-wide content-addressed DA
 * set. Repeated roots are deduplicated only when their exact typed entry bytes
 * agree; any conflicting preimage fails closed.
 */
export const mergeMidgardCekProgramMaterialSidecarsV1 = (
  sidecars: Iterable<Uint8Array>,
): readonly MidgardCekProgramMaterialEntryV1[] => {
  const byRoot = new Map<
    string,
    {
      readonly encoded: Buffer;
      readonly entry: MidgardCekProgramMaterialEntryV1;
    }
  >();
  for (const sidecar of sidecars) {
    for (const entry of decodeMidgardCekProgramMaterialSidecarV1(sidecar)) {
      const rootHex = Buffer.from(entry.root).toString("hex");
      const encoded = encodeMidgardCekProgramMaterialEntryV1(entry);
      const existing = byRoot.get(rootHex);
      if (existing !== undefined && !existing.encoded.equals(encoded)) {
        throw new Error(`conflicting V1 program material for root ${rootHex}`);
      }
      byRoot.set(rootHex, { encoded, entry });
    }
  }
  return Object.freeze(
    [...byRoot.values()]
      .sort((left, right) => compareBytes(left.entry.root, right.entry.root))
      .map(({ entry }) => entry),
  );
};

/**
 * Exact proof-profile submission envelope. Material is sorted by its
 * content-addressed root, and each versioned value is independently usable as
 * the matching DA entry.
 */
export const encodeMidgardProofSubmissionV1 = (
  submission: MidgardProofSubmissionV1,
): Buffer => {
  const transactionCbor = Buffer.from(submission.transactionCbor);
  if (transactionCbor.length === 0) {
    throw new Error("V1 submission transaction must not be empty");
  }
  return encodeCbor([
    MIDGARD_PROOF_SUBMISSION_ENVELOPE_V1_VERSION,
    transactionCbor,
    encodeMidgardCekProgramMaterialEntryListV1(
      submission.programMaterial,
      "V1 submission",
    ),
  ]);
};

export const decodeMidgardProofSubmissionV1 = (
  bytes: Uint8Array,
): MidgardProofSubmissionV1 => {
  const source = Buffer.from(bytes);
  const header = readCborArrayHeader(source, 0, "proof_submission");
  if (header.length !== 3) {
    throw new Error("V1 submission must contain exactly three fields");
  }
  const version = readCborUnsigned(
    source,
    header.nextOffset,
    "proof_submission.version",
  );
  if (version.value !== MIDGARD_PROOF_SUBMISSION_ENVELOPE_V1_VERSION) {
    throw new Error(
      `unsupported V1 submission version ${version.value.toString()}`,
    );
  }
  const transactionCbor = readCborBytes(
    source,
    version.nextOffset,
    "proof_submission.transaction",
  );
  if (transactionCbor.value.length === 0) {
    throw new Error("V1 submission transaction must not be empty");
  }
  const material = decodeMidgardCekProgramMaterialEntryListV1(
    source,
    transactionCbor.nextOffset,
    "proof_submission.program_material",
  );
  if (material.nextOffset !== source.length) {
    throw new Error("V1 submission has trailing bytes");
  }
  const decoded = Object.freeze({
    transactionCbor: transactionCbor.value,
    programMaterial: material.entries,
  });
  if (!encodeMidgardProofSubmissionV1(decoded).equals(source)) {
    throw new Error("V1 submission CBOR is not canonical");
  }
  return decoded;
};
