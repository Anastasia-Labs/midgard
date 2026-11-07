import {
  type MidgardCekDirectValueWitness,
  semanticizeMidgardCekDirectConstant,
} from "./cek-builtin.js";
import { type MidgardCekConstantType } from "./cek-constant.js";
import { encodeCardanoBytes } from "./cek-constant.semantic-data.js";
import { buildMidgardCekDataBytesNode } from "./cek-data-tree.js";
import { type Bytes } from "./cek-executor.build-midgard-cek-execution-graph.js";
import { type MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";
import { type MidgardCekSerialiseWitness } from "./cek-serialise-builtin.js";

const BYTES: MidgardCekConstantType = Object.freeze({ kind: "bytes" });

/**
 * Executes serialiseData (51) over the authenticated payload root of its Data
 * argument, at any size: the result is the byte leaf of the argument's exact
 * CBOR R, and the step is proved by the serialise loop reading R from its
 * staging outputs. The argument and the result are presented by their
 * summaries, which the loop's last hop checks.
 *
 * R comes from the store, which must hold every node and leaf blob of the
 * argument (`MidgardCekSemanticStore.bytes`).
 */
export const executeMidgardCekSerialiseBuiltin = (
  store: MidgardCekSemanticStore,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
): {
  readonly witness: MidgardCekSerialiseWitness;
  readonly result: MidgardCekDirectValueWitness;
  readonly root: Bytes;
} => {
  const [argument] = arguments_;
  if (arguments_.length !== 1 || argument === undefined) {
    throw new Error("serialiseData requires one argument");
  }
  const source = store.constant(roots[0]!, argument);
  if (source.type.kind !== "data") {
    throw new Error("serialiseData requires Data");
  }
  const serialised = store.bytes(source.dataRoot);
  const added = store.addConstant(
    BYTES,
    store.addDataNode(buildMidgardCekDataBytesNode(serialised, store.addBlob)),
    () => encodeCardanoBytes(serialised),
  );
  const result = semanticizeMidgardCekDirectConstant(added.result);
  return {
    witness: {
      kind: "executeBuiltinSerialise",
      arguments: [semanticizeMidgardCekDirectConstant(argument)],
      result,
      serialised,
    },
    result,
    root: added.root,
  };
};
