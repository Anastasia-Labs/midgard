import { type MidgardCekDirectValueWitness } from "./cek-builtin.js";
import { type MidgardCekConstantType } from "./cek-constant.js";
import { encodeCardanoBytes } from "./cek-constant.semantic-data.js";
import {
  buildMidgardCekDataBytesNode,
  buildMidgardCekDataConstrNode,
  buildMidgardCekDataListNode,
  encodeMidgardCekDataTreeInteger,
} from "./cek-data-tree.js";
import {
  type Bytes,
  sameBytes,
} from "./cek-executor.build-midgard-cek-execution-graph.js";
import {
  spliceMidgardCekListHead,
  spliceMidgardCekListTail,
  spliceMidgardCekPairFields,
} from "./cek-executor.semantic-bytes.js";
import {
  midgardCekSemanticConstrData,
  midgardCekSemanticMkCons,
  midgardCekSemanticMkPairData,
  midgardCekSemanticUnConstrData,
} from "./cek-executor.semantic-constructors.js";
import {
  MIDGARD_CEK_DATA_TYPE,
  MIDGARD_CEK_INTEGER_TYPE,
  MIDGARD_CEK_LIST_DATA_TYPE,
  MIDGARD_CEK_PAIR_DATA_TYPE,
  MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
  type MidgardCekSemanticBuiltinOutcome,
  midgardCekSemanticStep,
  type MidgardCekSemanticStepMaterial,
  midgardCekSemanticTopMaterial,
} from "./cek-executor.semantic-step.js";
import { type MidgardCekSemanticStore } from "./cek-executor.semantic-store.js";

export {
  type MidgardCekSemanticBuiltinOutcome,
  type MidgardCekSemanticStepMaterial,
  midgardCekSemanticTopMaterial,
} from "./cek-executor.semantic-step.js";

const BYTES: MidgardCekConstantType = Object.freeze({ kind: "bytes" });
const BOOLEAN: MidgardCekConstantType = Object.freeze({ kind: "boolean" });
const LIST_PAIR_DATA_TYPE: MidgardCekConstantType = Object.freeze({
  kind: "list",
  element: MIDGARD_CEK_PAIR_DATA_TYPE,
});

const isConstr = (kind: string): boolean =>
  kind === "constrSmall" || kind === "constrLarge";

/**
 * Executes one structural builtin (tags 29-37, 39-42 and 44-51) over the
 * authenticated payload roots of its arguments. Each result is built from
 * its arguments' nodes; only the nodes the result adds are created, and its
 * exact bytes, when it is revealed directly, are spliced from the bytes of
 * the arguments it is built from.
 */
export const executeMidgardCekSemanticBuiltin = (
  store: MidgardCekSemanticStore,
  tag: bigint,
  arguments_: readonly MidgardCekDirectValueWitness[],
  roots: readonly Bytes[],
): MidgardCekSemanticBuiltinOutcome => {
  const step = midgardCekSemanticStep(store, arguments_, roots);
  const { resolve, failure, built, selected, cached } = step;

  if (tag === 33n || tag === 34n) {
    const source = resolve(0);
    if (
      source.type.kind === "list" &&
      source.node.kind === "list" &&
      source.node.itemsCount === 0n
    ) {
      return failure(source);
    }
  }
  if (tag >= 42n && tag <= 46n) {
    const source = resolve(0);
    if (source.type.kind !== "data") {
      throw new Error("semantic Data failure has a non-Data source");
    }
    const kind = source.node.kind;
    const wrongVariant =
      tag === 42n
        ? !isConstr(kind)
        : tag === 43n
          ? kind !== "map"
          : tag === 44n
            ? kind !== "list"
            : tag === 45n
              ? kind !== "integer"
              : kind !== "bytes";
    if (wrongVariant) return failure(source);
  }
  if (tag === 29n || tag === 30n) {
    const source = resolve(0);
    if (
      source.type.kind !== "pair" ||
      source.node.kind !== "constrSmall" ||
      source.node.constructor !== 0n ||
      source.node.fieldsCount !== 2n
    ) {
      throw new Error("fstPair/sndPair requires a canonical pair");
    }
    const firstLink = store.link(source.node.fieldsRoot);
    const secondLink = store.link(firstLink.tail);
    const firstNode = store.node(firstLink.head);
    const secondNode = store.node(secondLink.head);
    const pairType = source.type;
    return built(
      tag === 29n ? pairType.first : pairType.second,
      tag === 29n ? firstLink.head : secondLink.head,
      {
        dataNodes: [source.node, firstNode, secondNode],
        listNodes: [firstLink, secondLink],
        pairNodes: [],
        scalarPreimages: [],
      },
      () => {
        const pair = cached(source.root);
        return pair === undefined
          ? undefined
          : spliceMidgardCekPairFields(pair)[tag === 29n ? 0 : 1];
      },
    );
  }
  if (tag >= 31n && tag <= 35n) {
    const source = resolve(tag === 32n ? 1 : 0);
    if (source.type.kind !== "list" || source.node.kind !== "list") {
      throw new Error("CEK semantic list builtin requires a list");
    }
    const listType = source.type;
    const list = source.node;
    const top = midgardCekSemanticTopMaterial(store, source.root);
    if (tag === 31n) {
      return selected(
        list.itemsCount === 0n ? arguments_[1]! : arguments_[2]!,
        top,
      );
    }
    if (tag === 32n) return midgardCekSemanticMkCons(step, source, top);
    if (tag === 35n) {
      return built(
        BOOLEAN,
        store.addDataNode(
          buildMidgardCekDataConstrNode(
            list.itemsCount === 0n ? 1n : 0n,
            store.items(new Uint8Array(0), 0n),
            store.addBlob,
          ),
        ),
        top,
      );
    }
    if (list.itemsCount === 0n) {
      throw new Error("headList/tailList failed on an empty list");
    }
    const firstLink = store.link(list.itemsRoot);
    const headNode = store.node(firstLink.head);
    const tailLink =
      firstLink.length === 1n ? undefined : store.link(firstLink.tail);
    const material: MidgardCekSemanticStepMaterial = {
      dataNodes: [list, headNode],
      listNodes: tailLink === undefined ? [firstLink] : [firstLink, tailLink],
      pairNodes: [],
      scalarPreimages: [],
    };
    if (tag === 33n) {
      return built(listType.element, firstLink.head, material, () => {
        const bytes = cached(source.root);
        return bytes === undefined
          ? undefined
          : spliceMidgardCekListHead(bytes);
      });
    }
    const remaining = list.itemsCount - 1n;
    return built(
      listType,
      store.addDataNode(
        buildMidgardCekDataListNode(store.items(firstLink.tail, remaining)),
      ),
      material,
      () => {
        const bytes = cached(source.root);
        return bytes === undefined
          ? undefined
          : spliceMidgardCekListTail(bytes, remaining);
      },
    );
  }
  if (tag === 36n) {
    const source = resolve(0);
    if (source.type.kind !== "data") {
      throw new Error("chooseData requires Data");
    }
    const kind = source.node.kind;
    const branch =
      kind === "constrSmall" || kind === "constrLarge"
        ? 1
        : kind === "map"
          ? 2
          : kind === "list"
            ? 3
            : kind === "integer"
              ? 4
              : 5;
    return selected(
      arguments_[branch]!,
      midgardCekSemanticTopMaterial(store, source.root),
    );
  }
  if (tag === 37n) return midgardCekSemanticConstrData(step);
  if (tag === 39n) {
    const items = resolve(0);
    if (
      items.type.kind !== "list" ||
      items.type.element.kind !== "data" ||
      items.node.kind !== "list"
    ) {
      throw new Error("listData requires a Data list");
    }
    return built(
      MIDGARD_CEK_DATA_TYPE,
      items.root,
      midgardCekSemanticTopMaterial(store, items.root),
    );
  }
  if (tag === 40n || tag === 45n) {
    const source = resolve(0);
    if (
      source.node.kind !== "integer" ||
      source.type.kind !== (tag === 40n ? "integer" : "data")
    ) {
      throw new Error("iData/unIData requires an integer payload");
    }
    return built(
      tag === 40n ? MIDGARD_CEK_DATA_TYPE : MIDGARD_CEK_INTEGER_TYPE,
      source.root,
      {
        dataNodes: [source.node],
        listNodes: [],
        pairNodes: [],
        scalarPreimages: [
          encodeMidgardCekDataTreeInteger(store.integer(source.root)),
        ],
      },
    );
  }
  if (tag === 41n || tag === 46n) {
    const source = resolve(0);
    if (
      source.node.kind !== "bytes" ||
      source.type.kind !== (tag === 41n ? "bytes" : "data")
    ) {
      throw new Error("bData/unBData requires a bytes payload");
    }
    return built(tag === 41n ? MIDGARD_CEK_DATA_TYPE : BYTES, source.root, {
      dataNodes: [source.node],
      listNodes: [],
      pairNodes: [],
      scalarPreimages: [store.blob(source.node.bytesRoot)],
    });
  }
  if (tag === 42n) return midgardCekSemanticUnConstrData(step);
  if (tag === 44n) {
    const source = resolve(0);
    if (source.type.kind !== "data" || source.node.kind !== "list") {
      throw new Error("unListData requires list Data");
    }
    return built(
      MIDGARD_CEK_LIST_DATA_TYPE,
      source.root,
      midgardCekSemanticTopMaterial(store, source.root),
    );
  }
  if (tag === 47n) {
    const left = resolve(0);
    const right = resolve(1);
    if (left.type.kind !== "data" || right.type.kind !== "data") {
      throw new Error("equalsData requires Data arguments");
    }
    return built(
      BOOLEAN,
      store.addDataNode(
        buildMidgardCekDataConstrNode(
          sameBytes(left.root, right.root) ? 1n : 0n,
          store.items(new Uint8Array(0), 0n),
          store.addBlob,
        ),
      ),
      MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
    );
  }
  if (tag === 48n) return midgardCekSemanticMkPairData(step);
  if (tag === 49n || tag === 50n) {
    const unit = resolve(0);
    if (unit.type.kind !== "unit") {
      throw new Error("mkNilData requires unit");
    }
    return built(
      tag === 49n ? MIDGARD_CEK_LIST_DATA_TYPE : LIST_PAIR_DATA_TYPE,
      store.addDataNode(
        buildMidgardCekDataListNode(store.items(new Uint8Array(0), 0n)),
      ),
      MIDGARD_CEK_SEMANTIC_EMPTY_MATERIAL,
    );
  }
  if (tag === 51n) {
    const source = resolve(0);
    if (source.type.kind !== "data") {
      throw new Error("serialiseData requires Data");
    }
    const limit = 9_215;
    if (source.node.cborLength > BigInt(limit)) {
      throw new Error(
        "serialiseData source exceeds the exact L1 revealed-preimage envelope",
      );
    }
    const raw = store.bytes(source.root);
    if (raw.length > limit) {
      throw new Error(
        "serialiseData source exceeds the exact L1 revealed-preimage envelope",
      );
    }
    return built(
      BYTES,
      store.addDataNode(buildMidgardCekDataBytesNode(raw, store.addBlob)),
      {
        dataNodes: [],
        listNodes: [],
        pairNodes: [],
        scalarPreimages: [raw],
      },
      () => encodeCardanoBytes(raw),
    );
  }
  throw new Error(`unsupported CEK semantic builtin ${tag.toString()}`);
};
