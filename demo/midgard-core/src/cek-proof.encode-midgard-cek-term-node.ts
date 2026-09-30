import { blake2b } from "@noble/hashes/blake2.js";

import { encodeCbor } from "./codec/cbor.js";
import { ensureHash32, type Hash32 } from "./codec/hash.js";

export const TERM_NODE_DOMAIN = Buffer.from("MidgardCekTermNodeV1", "ascii");

export const VALUE_NODE_DOMAIN = Buffer.from("MidgardCekValueNodeV1", "ascii");

export const SEQUENCE_NODE_DOMAIN = Buffer.from(
  "MidgardCekSequenceNodeV1",
  "ascii",
);

export const ENVIRONMENT_NODE_DOMAIN = Buffer.from(
  "MidgardCekEnvironmentNodeV1",
  "ascii",
);

export const CONTINUATION_NODE_DOMAIN = Buffer.from(
  "MidgardCekContinuationNodeV1",
  "ascii",
);

export const BLOB_CHUNK_DOMAIN = Buffer.from("MidgardCekBlobChunkV1", "ascii");

export const BLOB_BRANCH_DOMAIN = Buffer.from(
  "MidgardCekBlobBranchV1",
  "ascii",
);

export const MACHINE_STATE_DOMAIN = Buffer.from(
  "MidgardCekMachineStateV1",
  "ascii",
);

export const PROGRAM_ENVELOPE_DOMAIN = Buffer.from(
  "MidgardCekProgramEnvelopeV1",
  "ascii",
);

export const BLS_EXPRESSION_NODE_DOMAIN = Buffer.from(
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
export const MIDGARD_CEK_PROGRAM_ENVELOPE_VERSION = 1n;

export const MIDGARD_CEK_MACHINE_STATE_VERSION = 1n;

export const MIDGARD_CEK_BLOB_CHUNK_BYTES = 4_095;

export const MIDGARD_CEK_MAX_BUILTIN_TAG = 86n;

export const MIDGARD_CEK_PROGRAM_UPLC_VERSION = [1n, 1n, 0n] as const;

/**
 * The canonical V1 DA envelope is the only aggregate program-size budget. The
 * constants below mirror its exact canonical Plutus-Data encoding:
 *
 * - an otherwise-empty, structurally valid V1 payload is 446 bytes;
 * - replacing its one-byte empty material list with the two-byte non-empty
 *   list framing leaves 447 fixed bytes outside material tuples;
 * - the smallest tuple is 42 bytes: tuple framing (2), a bytes32 key (34),
 *   and a five-byte `[v1, kind, one-byte-preimage]` value wrapped as Plutus
 *   bytes (6).
 *
 * The canonical DA encoder has regression tests for all three measurements.
 * The material-byte bound is deliberately the tight structural upper bound
 * after fixed framing. Exact tuple overhead makes the realizable total
 * smaller, and the canonical 64 MiB DA-size check remains authoritative.
 */
export const MIDGARD_MAX_DA_PAYLOAD_BYTES = 64 * 1024 * 1024;

export const MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES = 447;

export const MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES = 42;

export const MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT = BigInt(
  Math.floor(
    (MIDGARD_MAX_DA_PAYLOAD_BYTES -
      MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES) /
      MIDGARD_CEK_MIN_PROGRAM_MATERIAL_DA_TUPLE_BYTES,
  ),
);

/**
 * A bundle may contain many transactions that use the same program, but
 * distinct program identities must not multiply verification work beyond one
 * maximum-size V1 program. Identical envelopes are verified once and reuse the
 * same positional result.
 */
export const MIDGARD_CEK_MAX_PROGRAM_BUNDLE_NODE_VISITS =
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT;

export const MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES = BigInt(
  MIDGARD_MAX_DA_PAYLOAD_BYTES - MIDGARD_CEK_PROGRAM_MATERIAL_DA_FIXED_BYTES,
);

/**
 * Each unique envelope declares the exact bytes reachable from its root. The
 * sum is therefore a conservative upper bound on both byte verification work
 * and retained type/payload result bytes, including when envelopes share
 * material. V1 permits at most one maximum-size program's work per bundle.
 */
export const MIDGARD_CEK_MAX_PROGRAM_BUNDLE_BYTE_WORK =
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES;

// [v1, [1,1,0], h32, uint32(node_count), uint32(material_bytes)].
export const MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES = 50;

export const MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES = 9_215;

/**
 * Matches the L1 constant decoder's direct type-CBOR limit. Constant types are
 * flat Plutus-Data tag lists, so this byte cap also gives the iterative parser
 * a deterministic bound independent of JavaScript's call-stack depth.
 */
export const MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES = 64;

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

export type Bytes = Uint8Array;

export const hash32 = (domain: Uint8Array, preimage: Uint8Array): Hash32 =>
  ensureHash32(
    blake2b(Buffer.concat([Buffer.from(domain), Buffer.from(preimage)]), {
      dkLen: 32,
    }),
    "cek_proof_hash",
  );

export const exactHash = (value: Bytes, fieldName: string): Buffer =>
  Buffer.from(ensureHash32(value, fieldName));

const nonNegative = (value: bigint, fieldName: string): bigint => {
  if (value < 0n) {
    throw new RangeError(`${fieldName} must be non-negative`);
  }
  return value;
};

export const uint32 = (value: bigint, fieldName: string): bigint => {
  nonNegative(value, fieldName);
  if (value > UINT32_MAX) {
    throw new RangeError(`${fieldName} must fit uint32`);
  }
  return value;
};

export const uint64 = (value: bigint, fieldName: string): bigint => {
  nonNegative(value, fieldName);
  if (value > UINT64_MAX) {
    throw new RangeError(`${fieldName} must fit uint64`);
  }
  return value;
};

export const boundedBuiltinTag = (value: bigint): bigint => {
  if (value < 0n || value > MIDGARD_CEK_MAX_BUILTIN_TAG) {
    throw new RangeError(
      `CEK builtin tag must be between 0 and ${MIDGARD_CEK_MAX_BUILTIN_TAG.toString(10)}`,
    );
  }
  return value;
};

export type MidgardCekTermNode =
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

export const encodeMidgardCekTermNode = (node: MidgardCekTermNode): Buffer => {
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

export const hashMidgardCekTermNode = (node: MidgardCekTermNode): Hash32 =>
  hash32(TERM_NODE_DOMAIN, encodeMidgardCekTermNode(node));
