import {
  BLOB_BRANCH_DOMAIN,
  BLOB_CHUNK_DOMAIN,
  type Bytes,
  CONTINUATION_NODE_DOMAIN,
  exactHash,
  hash32,
  MACHINE_STATE_DOMAIN,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  MIDGARD_CEK_MACHINE_STATE_VERSION,
  MIDGARD_CEK_PROGRAM_ENVELOPE_VERSION,
  MidgardCekContinuationTags,
  MidgardCekMachineModes,
  uint32,
  uint64,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import { type MidgardCekContinuationFrame } from "./cek-proof.encode-midgard-cek-value-node.js";
import { encodeCbor } from "./codec/cbor.js";
import { type Hash32 } from "./codec/hash.js";

export const encodeMidgardCekContinuationFrame = (
  frame: MidgardCekContinuationFrame,
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

export const hashMidgardCekContinuationFrame = (
  frame: MidgardCekContinuationFrame,
): Hash32 =>
  hash32(CONTINUATION_NODE_DOMAIN, encodeMidgardCekContinuationFrame(frame));

export const hashMidgardCekBlobChunk = (chunk: Bytes): Hash32 => {
  return hash32(BLOB_CHUNK_DOMAIN, encodeMidgardCekBlobChunk(chunk));
};

export const encodeMidgardCekBlobChunk = (chunk: Bytes): Buffer => {
  if (chunk.length > MIDGARD_CEK_BLOB_CHUNK_BYTES) {
    throw new RangeError(
      `CEK blob chunk must contain at most ${MIDGARD_CEK_BLOB_CHUNK_BYTES.toString(10)} bytes`,
    );
  }
  return encodeCbor(Buffer.from(chunk));
};

export type MidgardCekBlobBranch = {
  readonly left: Bytes;
  readonly right: Bytes;
  readonly byteLength: bigint;
};

export const encodeMidgardCekBlobBranch = (
  input: MidgardCekBlobBranch,
): Buffer =>
  encodeCbor([
    exactHash(input.left, "cek_blob_branch.left"),
    exactHash(input.right, "cek_blob_branch.right"),
    uint64(input.byteLength, "cek_blob_branch.byte_length"),
  ]);

export const hashMidgardCekBlobBranch = (input: MidgardCekBlobBranch): Hash32 =>
  hash32(BLOB_BRANCH_DOMAIN, encodeMidgardCekBlobBranch(input));

export type MidgardCekBlobCommitment = {
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
export const commitMidgardCekBlob = (
  bytes: Bytes,
): MidgardCekBlobCommitment => {
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
      const preimage = encodeMidgardCekBlobChunk(chunks[start]!);
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
    const preimage = encodeMidgardCekBlobBranch({
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

export type MidgardCekMachineState = {
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

const machineModeTag = (mode: MidgardCekMachineState["mode"]): bigint => {
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

export const encodeMidgardCekMachineState = (
  state: MidgardCekMachineState,
): Buffer =>
  encodeCbor([
    MIDGARD_CEK_MACHINE_STATE_VERSION,
    machineModeTag(state.mode),
    uint32(state.executionIndex, "cek_state.execution_index"),
    exactHash(state.focusRoot, "cek_state.focus_root"),
    exactHash(state.environmentRoot, "cek_state.environment_root"),
    exactHash(state.continuationRoot, "cek_state.continuation_root"),
    uint64(state.auxiliary, "cek_state.auxiliary"),
    uint64(state.cpu, "cek_state.cpu"),
    uint64(state.memory, "cek_state.memory"),
  ]);

export const hashMidgardCekMachineState = (
  state: MidgardCekMachineState,
): Hash32 => hash32(MACHINE_STATE_DOMAIN, encodeMidgardCekMachineState(state));

export type MidgardCekProgramEnvelope = {
  readonly uplcVersion: readonly [bigint, bigint, bigint];
  readonly termRoot: Bytes;
  readonly nodeCount: bigint;
  readonly materialByteLength: bigint;
};

export const encodeMidgardCekProgramEnvelope = (
  envelope: MidgardCekProgramEnvelope,
): Buffer =>
  encodeCbor([
    MIDGARD_CEK_PROGRAM_ENVELOPE_VERSION,
    [
      uint32(envelope.uplcVersion[0], "cek_program.version.major"),
      uint32(envelope.uplcVersion[1], "cek_program.version.minor"),
      uint32(envelope.uplcVersion[2], "cek_program.version.patch"),
    ],
    exactHash(envelope.termRoot, "cek_program.term_root"),
    uint32(envelope.nodeCount, "cek_program.node_count"),
    uint64(envelope.materialByteLength, "cek_program.material_byte_length"),
  ]);
