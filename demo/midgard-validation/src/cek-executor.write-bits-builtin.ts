/**
 * writeBits (80) through the writeBits loop: over an input of at most 4,096
 * bytes and an index list presented by its summary or longer than the
 * direct route proves on L1. A success's witness carries its compacted
 * arguments and result and the loop charges the builtin's cost over the
 * index count; an index outside the input gives the loop's failure witness,
 * uncharged.
 *
 * The input and the indices are read from the store, which must hold the
 * list's nodes and each index's leaf.
 */
import { DataList } from "@harmoniclabs/plutus-data";
import { CEKConst } from "@harmoniclabs/plutus-machine";

import type {
  Bytes,
  MidgardCekDirectValueWitness,
} from "./cek-builtin.argument-kinds.js";
import { semanticizeMidgardCekDirectConstant } from "./cek-builtin.evaluate-reference-builtin.js";
import {
  decodeMidgardCekConstantWitness,
  type MidgardCekConstantType,
} from "./cek-constant.js";
import { encodeCardanoBytes } from "./cek-constant.semantic-data.js";
import type { MidgardCekBuiltinBudget } from "./cek-cost.js";
import { buildMidgardCekDataBytesNode } from "./cek-data-tree.js";
import type { MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import { compactValue, sameCompacted } from "./cek-staged-builtin.verify.js";
import type { MidgardCekWriteBitsWitness } from "./cek-write-bits-builtin.js";
import {
  MIDGARD_CEK_WRITE_BITS_MAX_INPUT,
  midgardCekWriteBits,
  midgardCekWriteBitsCharge,
} from "./cek-write-bits-loop.js";

const BYTES: MidgardCekConstantType = Object.freeze({ kind: "bytes" });

/** The most indices an inline list has on the direct route: past it, a
 * direct step's list costs more to commit on L1 than its step may. */
export const MIDGARD_CEK_WRITE_BITS_DIRECT_INDICES = 64;

/** Whether writeBits over these presented arguments takes the loop: an
 * index list presented by its summary or past the direct route's length. */
export const midgardCekWriteBitsTakesLoop = (
  arguments_: readonly MidgardCekDirectValueWitness[],
): boolean => {
  const indices = arguments_[1];
  if (indices === undefined) return false;
  if (indices.kind !== "constant") return true;
  const { payload } = decodeMidgardCekConstantWitness(indices.witness);
  return (
    payload instanceof DataList &&
    payload.list.length > MIDGARD_CEK_WRITE_BITS_DIRECT_INDICES
  );
};

const indicesOf = (
  store: MidgardCekSemanticStore,
  root: Bytes,
  argument: MidgardCekDirectValueWitness,
): bigint[] | null => {
  const { type, dataRoot } = store.constant(root, argument);
  if (type.kind !== "list" || type.element.kind !== "integer") return null;
  const node = store.node(dataRoot);
  if (node.kind !== "list") return null;
  const indices: bigint[] = [];
  let at: Bytes = node.itemsRoot;
  for (let index = 0n; index < node.itemsCount; index += 1n) {
    const link = store.link(at);
    indices.push(store.integer(link.head));
    at = link.tail;
  }
  return indices;
};

const inputOf = (
  store: MidgardCekSemanticStore,
  root: Bytes,
  argument: MidgardCekDirectValueWitness,
): Buffer | null => {
  const { type, dataRoot } = store.constant(root, argument);
  if (type.kind !== "bytes") return null;
  const node = store.node(dataRoot);
  return node.kind === "bytes" ? store.blob(node.bytesRoot) : null;
};

/**
 * The loop's step of writeBits over `arguments_` (presented, with value
 * roots `roots`), or null when the input is past 4,096 bytes (the staged
 * failure hop's) or an argument is not of writeBits' type.
 */
export const executeMidgardCekWriteBitsBuiltin = (
  store: MidgardCekSemanticStore,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
):
  | {
      readonly kind: "success";
      readonly witness: MidgardCekWriteBitsWitness;
      readonly result: MidgardCekDirectValueWitness;
      readonly root: Bytes;
      readonly budget: MidgardCekBuiltinBudget;
    }
  | {
      readonly kind: "failure";
      readonly witness: MidgardCekWriteBitsWitness;
      readonly budget: MidgardCekBuiltinBudget;
    }
  | null => {
  const [input, indices, value] = arguments_;
  if (arguments_.length !== 3) return null;
  const bytes = inputOf(store, roots[0]!, input!);
  if (bytes === null || bytes.length > MIDGARD_CEK_WRITE_BITS_MAX_INPUT)
    return null;
  const list = indicesOf(store, roots[1]!, indices!);
  const committed = semanticizeMidgardCekDirectConstant(value!);
  const set = [true, false].find(
    (candidate) =>
      committed.kind === "semanticConstant" &&
      sameCompacted(committed.witness, compactValue(CEKConst.bool(candidate))),
  );
  if (list === null || set === undefined) return null;
  const compacted = arguments_.map(semanticizeMidgardCekDirectConstant);
  const out = midgardCekWriteBits(bytes, list, set);
  if (out === null)
    return {
      kind: "failure",
      witness: {
        kind: "executeBuiltinWriteBitsFailure",
        arguments: compacted,
        input: bytes,
        indices: list,
      },
      budget: { cpu: 0n, memory: 0n },
    };
  const added = store.addConstant(
    BYTES,
    store.addDataNode(buildMidgardCekDataBytesNode(out, store.addBlob)),
    () => encodeCardanoBytes(out),
  );
  const result = semanticizeMidgardCekDirectConstant(added.result);
  return {
    kind: "success",
    witness: {
      kind: "executeBuiltinWriteBits",
      arguments: compacted,
      result,
      input: bytes,
      indices: list,
    },
    result,
    root: added.root,
    budget: midgardCekWriteBitsCharge(out.length, BigInt(list.length)),
  };
};
