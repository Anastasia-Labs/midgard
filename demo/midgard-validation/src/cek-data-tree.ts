import {
  commitMidgardCekBlobV1,
  encodeMidgardCekDataListNodeV1,
  encodeMidgardCekDataNodeV1,
  encodeMidgardCekDataPairNodeV1,
  hashMidgardCekDataListNodeV1,
  hashMidgardCekDataNodeV1,
  hashMidgardCekDataPairNodeV1,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
  midgardCekDataBytesCborLengthV1,
  midgardCekDataBytesMemoryV1,
  midgardCekDataConstrCborLengthV1,
  midgardCekDataListCborLengthV1,
  type MidgardCekDataListNodeV1,
  midgardCekDataMapCborLengthV1,
  type MidgardCekDataNodeV1,
  type MidgardCekDataPairNodeV1,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";

import {
  encodeMidgardCekPlutusDataV1,
  midgardCekDataMemorySizeV1,
  midgardCekIntegerMemorySizeV1,
} from "./cek-constant.js";

type HashedMaterial<Node> = {
  readonly node: Node;
  readonly preimage: Buffer;
};

export type MidgardCekDataTreeCommitmentV1 = {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
  readonly dataNodes: ReadonlyMap<
    string,
    HashedMaterial<MidgardCekDataNodeV1>
  >;
  readonly listNodes: ReadonlyMap<
    string,
    HashedMaterial<MidgardCekDataListNodeV1>
  >;
  readonly pairNodes: ReadonlyMap<
    string,
    HashedMaterial<MidgardCekDataPairNodeV1>
  >;
  readonly blobNodes: ReadonlyMap<
    string,
    {
      readonly kind: "chunk" | "branch";
      readonly preimage: Buffer;
    }
  >;
};

type DataSummary = {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
};

type ListSummary = {
  readonly root: Uint8Array;
  readonly length: bigint;
  readonly payloadCborLength: bigint;
  readonly memory: bigint;
};

const rootKey = (root: Uint8Array): string =>
  Buffer.from(root).toString("hex");

const addExact = <Node>(
  entries: Map<string, HashedMaterial<Node>>,
  root: Uint8Array,
  material: HashedMaterial<Node>,
): void => {
  const key = rootKey(root);
  const existing = entries.get(key);
  if (
    existing !== undefined &&
    !existing.preimage.equals(material.preimage)
  ) {
    throw new Error("CEK semantic material hash collision");
  }
  entries.set(key, material);
};

const asByteArray = (value: unknown): Uint8Array => {
  if (
    typeof value !== "object" ||
    value === null ||
    !("toBuffer" in value) ||
    typeof value.toBuffer !== "function"
  ) {
    throw new Error("Plutus Data bytes leaf has an invalid byte string");
  }
  const bytes = value.toBuffer();
  if (!(bytes instanceof Uint8Array)) {
    throw new Error("Plutus Data bytes leaf did not produce bytes");
  }
  return bytes;
};

/**
 * Builds the semantic commitment consumed by the incremental script-context
 * and CEK builtin proofs. No complete-value proof bound is imposed: leaf
 * bytes and large integer encodings use the canonical 4,095-byte CEK blob
 * chunks, while every structural node remains a small fixed preimage.
 */
export const commitMidgardCekDataTreeV1 = (
  value: Data,
): MidgardCekDataTreeCommitmentV1 => {
  const dataNodes = new Map<
    string,
    HashedMaterial<MidgardCekDataNodeV1>
  >();
  const listNodes = new Map<
    string,
    HashedMaterial<MidgardCekDataListNodeV1>
  >();
  const pairNodes = new Map<
    string,
    HashedMaterial<MidgardCekDataPairNodeV1>
  >();
  const blobNodes = new Map<
    string,
    {
      readonly kind: "chunk" | "branch";
      readonly preimage: Buffer;
    }
  >();

  const addBlob = (bytes: Uint8Array): Uint8Array => {
    const committed = commitMidgardCekBlobV1(bytes);
    for (const [key, node] of committed.nodes) {
      const existing = blobNodes.get(key);
      if (
        existing !== undefined &&
        (existing.kind !== node.kind ||
          !existing.preimage.equals(node.preimage))
      ) {
        throw new Error("CEK semantic blob material hash collision");
      }
      blobNodes.set(key, node);
    }
    return committed.root;
  };

  const addList = (
    items: readonly Data[],
    addData: (item: Data) => DataSummary,
  ): ListSummary => {
    let summary: ListSummary = {
      root: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = items.length - 1; index >= 0; index -= 1) {
      const head = addData(items[index]!);
      const node: MidgardCekDataListNodeV1 = {
        head: head.root,
        headCborLength: head.cborLength,
        headMemory: head.memory,
        tail: summary.root,
        length: summary.length + 1n,
        payloadCborLength:
          head.cborLength + summary.payloadCborLength,
        memory: head.memory + summary.memory,
      };
      const preimage = encodeMidgardCekDataListNodeV1(node);
      const root = hashMidgardCekDataListNodeV1(node);
      addExact(listNodes, root, { node, preimage });
      summary = {
        root,
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };

  const addPairs = (
    items: readonly { readonly fst: Data; readonly snd: Data }[],
    addData: (item: Data) => DataSummary,
  ): ListSummary => {
    let summary: ListSummary = {
      root: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    };
    for (let index = items.length - 1; index >= 0; index -= 1) {
      const item = items[index]!;
      const key = addData(item.fst);
      const mapped = addData(item.snd);
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
          key.cborLength +
          mapped.cborLength +
          summary.payloadCborLength,
        memory: key.memory + mapped.memory + summary.memory,
      };
      const preimage = encodeMidgardCekDataPairNodeV1(node);
      const root = hashMidgardCekDataPairNodeV1(node);
      addExact(pairNodes, root, { node, preimage });
      summary = {
        root,
        length: node.length,
        payloadCborLength: node.payloadCborLength,
        memory: node.memory,
      };
    }
    return summary;
  };

  const addData = (data: Data): DataSummary => {
    let node: MidgardCekDataNodeV1;
    if (data instanceof DataConstr) {
      if (data.constr < 0n) {
        throw new Error("Plutus Data constructor must be non-negative");
      }
      const fields = addList(data.fields, addData);
      const cborLength = midgardCekDataConstrCborLengthV1(
        data.constr,
        fields.length,
        fields.payloadCborLength,
      );
      const memory = 4n + fields.memory;
      if (data.constr <= 127n) {
        node = {
          kind: "constrSmall",
          constructor: data.constr,
          fieldsCount: fields.length,
          fieldsRoot: fields.root,
          cborLength,
          memory,
        };
      } else {
        const constructorCbor =
          encodeMidgardCekPlutusDataV1(new DataI(data.constr));
        node = {
          kind: "constrLarge",
          constructorCborRoot: addBlob(constructorCbor),
          constructorCborLength: BigInt(constructorCbor.length),
          constructorMemory:
            4n + midgardCekIntegerMemorySizeV1(data.constr),
          fieldsCount: fields.length,
          fieldsRoot: fields.root,
          cborLength,
          memory,
        };
      }
    } else if (data instanceof DataMap) {
      const entries = addPairs(data.map, addData);
      node = {
        kind: "map",
        entriesCount: entries.length,
        entriesRoot: entries.root,
        cborLength: midgardCekDataMapCborLengthV1(
          entries.length,
          entries.payloadCborLength,
        ),
        memory: 4n + entries.memory,
      };
    } else if (data instanceof DataList) {
      const items = addList(data.list, addData);
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
    } else if (data instanceof DataI) {
      const cbor = encodeMidgardCekPlutusDataV1(data);
      node = {
        kind: "integer",
        cborRoot: addBlob(cbor),
        cborLength: BigInt(cbor.length),
        memory: 4n + midgardCekIntegerMemorySizeV1(data.int),
      };
    } else if (data instanceof DataB) {
      const bytes = asByteArray(data.bytes);
      node = {
        kind: "bytes",
        bytesRoot: addBlob(bytes),
        bytesLength: BigInt(bytes.length),
        cborLength: midgardCekDataBytesCborLengthV1(
          BigInt(bytes.length),
        ),
        memory: midgardCekDataBytesMemoryV1(BigInt(bytes.length)),
      };
    } else {
      throw new Error("V1 semantic tree contains unknown Plutus Data");
    }

    const preimage = encodeMidgardCekDataNodeV1(node);
    const root = hashMidgardCekDataNodeV1(node);
    addExact(dataNodes, root, { node, preimage });

    const canonicalLength = BigInt(
      encodeMidgardCekPlutusDataV1(data).length,
    );
    const exactMemory = midgardCekDataMemorySizeV1(data);
    if (
      node.cborLength !== canonicalLength ||
      node.memory !== exactMemory
    ) {
      throw new Error(
        "V1 semantic tree summary disagrees with canonical Data",
      );
    }
    return {
      root,
      cborLength: node.cborLength,
      memory: node.memory,
    };
  };

  const committed = addData(value);
  return Object.freeze({
    ...committed,
    dataNodes,
    listNodes,
    pairNodes,
    blobNodes,
  });
};
