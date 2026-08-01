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

type ListWork = {
  summary: ListSummary;
};

type DataWork =
  | { readonly kind: "visit"; readonly data: Data }
  | {
      readonly kind: "finishData";
      readonly data: Data;
      readonly children: ListWork;
    }
  | {
      readonly kind: "finishListItem";
      readonly item: Data;
      readonly list: ListWork;
    }
  | {
      readonly kind: "finishPairItem";
      readonly key: Data;
      readonly value: Data;
      readonly list: ListWork;
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

  const summaries = new Map<Data, DataSummary>();
  const operations: DataWork[] = [{ kind: "visit", data: value }];

  const emptyListWork = (root: Uint8Array): ListWork => ({
    summary: {
      root,
      length: 0n,
      payloadCborLength: 0n,
      memory: 0n,
    },
  });

  const finishListItem = (item: Data, list: ListWork): void => {
    const head = summaries.get(item);
    if (head === undefined) {
      throw new Error("V1 semantic tree lost a list child summary");
    }
    const tail = list.summary;
    const node: MidgardCekDataListNodeV1 = {
      head: head.root,
      headCborLength: head.cborLength,
      headMemory: head.memory,
      tail: tail.root,
      length: tail.length + 1n,
      payloadCborLength: head.cborLength + tail.payloadCborLength,
      memory: head.memory + tail.memory,
    };
    const preimage = encodeMidgardCekDataListNodeV1(node);
    const root = hashMidgardCekDataListNodeV1(node);
    addExact(listNodes, root, { node, preimage });
    list.summary = {
      root,
      length: node.length,
      payloadCborLength: node.payloadCborLength,
      memory: node.memory,
    };
  };

  const finishPairItem = (
    keyData: Data,
    valueData: Data,
    list: ListWork,
  ): void => {
    const key = summaries.get(keyData);
    const mapped = summaries.get(valueData);
    if (key === undefined || mapped === undefined) {
      throw new Error("V1 semantic tree lost a pair child summary");
    }
    const tail = list.summary;
    const node: MidgardCekDataPairNodeV1 = {
      key: key.root,
      keyCborLength: key.cborLength,
      keyMemory: key.memory,
      value: mapped.root,
      valueCborLength: mapped.cborLength,
      valueMemory: mapped.memory,
      tail: tail.root,
      length: tail.length + 1n,
      payloadCborLength:
        key.cborLength + mapped.cborLength + tail.payloadCborLength,
      memory: key.memory + mapped.memory + tail.memory,
    };
    const preimage = encodeMidgardCekDataPairNodeV1(node);
    const root = hashMidgardCekDataPairNodeV1(node);
    addExact(pairNodes, root, { node, preimage });
    list.summary = {
      root,
      length: node.length,
      payloadCborLength: node.payloadCborLength,
      memory: node.memory,
    };
  };

  const finishData = (data: Data, children: ListWork): void => {
    const sequence = children.summary;
    let node: MidgardCekDataNodeV1;
    if (data instanceof DataConstr) {
      const cborLength = midgardCekDataConstrCborLengthV1(
        data.constr,
        sequence.length,
        sequence.payloadCborLength,
      );
      const memory = 4n + sequence.memory;
      if (data.constr <= 127n) {
        node = {
          kind: "constrSmall",
          constructor: data.constr,
          fieldsCount: sequence.length,
          fieldsRoot: sequence.root,
          cborLength,
          memory,
        };
      } else {
        const constructorCbor = encodeMidgardCekPlutusDataV1(
          new DataI(data.constr),
        );
        node = {
          kind: "constrLarge",
          constructorCborRoot: addBlob(constructorCbor),
          constructorCborLength: BigInt(constructorCbor.length),
          constructorMemory: 4n + midgardCekIntegerMemorySizeV1(data.constr),
          fieldsCount: sequence.length,
          fieldsRoot: sequence.root,
          cborLength,
          memory,
        };
      }
    } else if (data instanceof DataMap) {
      node = {
        kind: "map",
        entriesCount: sequence.length,
        entriesRoot: sequence.root,
        cborLength: midgardCekDataMapCborLengthV1(
          sequence.length,
          sequence.payloadCborLength,
        ),
        memory: 4n + sequence.memory,
      };
    } else if (data instanceof DataList) {
      node = {
        kind: "list",
        itemsCount: sequence.length,
        itemsRoot: sequence.root,
        cborLength: midgardCekDataListCborLengthV1(
          sequence.length,
          sequence.payloadCborLength,
        ),
        memory: 4n + sequence.memory,
      };
    } else {
      throw new Error("V1 semantic tree contains unknown Plutus Data");
    }

    const preimage = encodeMidgardCekDataNodeV1(node);
    const root = hashMidgardCekDataNodeV1(node);
    addExact(dataNodes, root, { node, preimage });
    summaries.set(data, {
      root,
      cborLength: node.cborLength,
      memory: node.memory,
    });
  };

  while (operations.length > 0) {
    const operation = operations.pop()!;
    if (operation.kind === "visit") {
      if (summaries.has(operation.data)) {
        continue;
      }
      if (operation.data instanceof DataConstr) {
        if (operation.data.constr < 0n) {
          throw new Error("Plutus Data constructor must be non-negative");
        }
        const children = emptyListWork(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1);
        operations.push({
          kind: "finishData",
          data: operation.data,
          children,
        });
        // Push in reverse execution order to preserve the recursive fold:
        // each right-to-left child is finalized before its list node.
        for (let index = 0; index < operation.data.fields.length; index += 1) {
          const item = operation.data.fields[index]!;
          operations.push({
            kind: "finishListItem",
            item,
            list: children,
          });
          operations.push({ kind: "visit", data: item });
        }
        continue;
      }
      if (operation.data instanceof DataMap) {
        const children = emptyListWork(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT_V1);
        operations.push({
          kind: "finishData",
          data: operation.data,
          children,
        });
        // Keys, values, and pair nodes retain the original right-to-left order.
        for (let index = 0; index < operation.data.map.length; index += 1) {
          const item = operation.data.map[index]!;
          operations.push({
            kind: "finishPairItem",
            key: item.fst,
            value: item.snd,
            list: children,
          });
          operations.push({ kind: "visit", data: item.snd });
          operations.push({ kind: "visit", data: item.fst });
        }
        continue;
      }
      if (operation.data instanceof DataList) {
        const children = emptyListWork(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1);
        operations.push({
          kind: "finishData",
          data: operation.data,
          children,
        });
        for (let index = 0; index < operation.data.list.length; index += 1) {
          const item = operation.data.list[index]!;
          operations.push({
            kind: "finishListItem",
            item,
            list: children,
          });
          operations.push({ kind: "visit", data: item });
        }
        continue;
      }

      let node: MidgardCekDataNodeV1;
      if (operation.data instanceof DataI) {
        const cbor = encodeMidgardCekPlutusDataV1(operation.data);
        node = {
          kind: "integer",
          cborRoot: addBlob(cbor),
          cborLength: BigInt(cbor.length),
          memory: 4n + midgardCekIntegerMemorySizeV1(operation.data.int),
        };
      } else if (operation.data instanceof DataB) {
        const bytes = asByteArray(operation.data.bytes);
        node = {
          kind: "bytes",
          bytesRoot: addBlob(bytes),
          bytesLength: BigInt(bytes.length),
          cborLength: midgardCekDataBytesCborLengthV1(BigInt(bytes.length)),
          memory: midgardCekDataBytesMemoryV1(BigInt(bytes.length)),
        };
      } else {
        throw new Error("V1 semantic tree contains unknown Plutus Data");
      }

      const preimage = encodeMidgardCekDataNodeV1(node);
      const root = hashMidgardCekDataNodeV1(node);
      addExact(dataNodes, root, { node, preimage });
      summaries.set(operation.data, {
        root,
        cborLength: node.cborLength,
        memory: node.memory,
      });
      continue;
    }
    if (operation.kind === "finishListItem") {
      finishListItem(operation.item, operation.list);
      continue;
    }
    if (operation.kind === "finishPairItem") {
      finishPairItem(operation.key, operation.value, operation.list);
      continue;
    }
    finishData(operation.data, operation.children);
  }

  const committed = summaries.get(value);
  if (committed === undefined) {
    throw new Error("V1 semantic tree lost its root summary");
  }
  return Object.freeze({
    ...committed,
    dataNodes,
    listNodes,
    pairNodes,
    blobNodes,
  });
};
