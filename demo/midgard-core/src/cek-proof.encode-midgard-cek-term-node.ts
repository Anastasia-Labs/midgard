import { midgardBlake2b } from "./codec/blake2b.js";
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

export const BLS_EXPRESSION_NODE_DOMAIN = Buffer.from(
  "MidgardCekBlsExpressionV1",
  "ascii",
);

const UINT32_MAX = 0xffff_ffffn;

const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

export const MIDGARD_CEK_MACHINE_STATE_VERSION = 1n;

export const MIDGARD_CEK_BLOB_CHUNK_BYTES = 4_095;

/** Cardano's PV 11 builtin indices: 0-100. */
export const MIDGARD_CEK_MAX_BUILTIN_TAG = 100n;

/** Cardano's PV 11 bound on a constr's field count (`mbConstr`). */
export const MIDGARD_CEK_MAX_CONSTR_FIELDS = 1_024n;

/** The admitted program versions: 1.0.0 (no constr or case) and 1.1.0. */
export const MIDGARD_CEK_PROGRAM_UPLC_VERSIONS = Object.freeze([
  Object.freeze([1n, 0n, 0n] as const),
  Object.freeze([1n, 1n, 0n] as const),
]);

export const isMidgardCekProgramUplcVersion = (
  major: bigint,
  minor: bigint,
  patch: bigint,
): boolean => major === 1n && (minor === 0n || minor === 1n) && patch === 0n;

export const MIDGARD_MAX_DA_PAYLOAD_BYTES = 64 * 1024 * 1024;

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
    midgardBlake2b(
      Buffer.concat([Buffer.from(domain), Buffer.from(preimage)]),
      {
        dkLen: 32,
      },
    ),
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
        uint64(node.index, "cek_term.variable.index"),
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

/** True when a term is a constr with more fields than Cardano admits. */
export const midgardCekTermExceedsConstrFields = (
  term: MidgardCekTermNode,
): boolean =>
  term.kind === "constr" && term.termsCount > MIDGARD_CEK_MAX_CONSTR_FIELDS;

/** The program minor version a term needs: constr and case need 1.1.0. */
export const midgardCekTermMinimumVersionMinor = (
  term: MidgardCekTermNode,
): bigint => (term.kind === "constr" || term.kind === "case" ? 1n : 0n);
