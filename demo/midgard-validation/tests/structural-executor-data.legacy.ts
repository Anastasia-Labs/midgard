/**
 * The recursive semantic Data and blob readers the structural executor used
 * before `cek-executor.structural-executor-data.ts`, kept verbatim (methods
 * turned into closures over the same material maps) as a differential oracle.
 */
import {
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";

import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import {
  type Bytes,
  rootHex,
  sameBytes,
} from "../src/cek-executor.build-midgard-cek-execution-graph.js";
import type { MidgardCekSemanticDataMaterial } from "../src/cek-executor.structural-executor-data.js";

export const legacySemanticReaders = (
  material: MidgardCekSemanticDataMaterial,
): {
  readonly blobBytes: (root: Bytes) => Buffer;
  readonly dataValue: (root: Bytes) => Data;
} => {
  const blobBytes = (
    root: Bytes,
    active: ReadonlySet<string> = new Set(),
  ): Buffer => {
    const key = rootHex(root);
    if (active.has(key)) {
      throw new Error("cyclic CEK semantic blob commitment");
    }
    const node = material.blobs.get(key);
    if (node === undefined) {
      throw new Error(`missing authenticated CEK blob ${key}`);
    }
    if (node.kind === "chunk") return Buffer.from(node.bytes);
    const next = new Set(active);
    next.add(key);
    const bytes = Buffer.concat([
      blobBytes(node.left, next),
      blobBytes(node.right, next),
    ]);
    if (BigInt(bytes.length) !== node.byteLength) {
      throw new Error("CEK semantic blob length does not match its root");
    }
    return bytes;
  };

  const dataListValues = (root: Bytes, count: bigint): Data[] => {
    const values: Data[] = [];
    let cursor = Buffer.from(root);
    let remaining = count;
    while (remaining > 0n) {
      const node = material.dataLists.get(rootHex(cursor));
      if (node === undefined || node.length !== remaining) {
        throw new Error(
          `missing authenticated CEK Data-list node ${rootHex(cursor)}`,
        );
      }
      values.push(dataValue(node.head));
      cursor = Buffer.from(node.tail);
      remaining -= 1n;
    }
    if (!sameBytes(cursor, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)) {
      throw new Error("CEK Data-list commitment has a non-empty tail");
    }
    return values;
  };

  const dataPairValues = (
    root: Bytes,
    count: bigint,
  ): DataPair<Data, Data>[] => {
    const values: DataPair<Data, Data>[] = [];
    let cursor = Buffer.from(root);
    let remaining = count;
    while (remaining > 0n) {
      const node = material.dataPairs.get(rootHex(cursor));
      if (node === undefined || node.length !== remaining) {
        throw new Error(
          `missing authenticated CEK Data-pair node ${rootHex(cursor)}`,
        );
      }
      values.push(new DataPair(dataValue(node.key), dataValue(node.value)));
      cursor = Buffer.from(node.tail);
      remaining -= 1n;
    }
    if (!sameBytes(cursor, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)) {
      throw new Error("CEK Data-map commitment has a non-empty tail");
    }
    return values;
  };

  const dataValue = (root: Bytes): Data => {
    const node = material.dataNodes.get(rootHex(root));
    if (node === undefined) {
      throw new Error(`missing authenticated CEK Data node ${rootHex(root)}`);
    }
    let value: Data;
    switch (node.kind) {
      case "constrSmall":
        value = new DataConstr(
          node.constructor,
          dataListValues(node.fieldsRoot, node.fieldsCount),
        );
        break;
      case "constrLarge": {
        const constructor = dataFromCbor(blobBytes(node.constructorCborRoot));
        if (!(constructor instanceof DataI) || constructor.int <= 127n) {
          throw new Error("CEK large Data constructor is not canonical");
        }
        value = new DataConstr(
          constructor.int,
          dataListValues(node.fieldsRoot, node.fieldsCount),
        );
        break;
      }
      case "map":
        value = new DataMap(
          dataPairValues(node.entriesRoot, node.entriesCount),
        );
        break;
      case "list":
        value = new DataList(dataListValues(node.itemsRoot, node.itemsCount));
        break;
      case "integer": {
        const integer = dataFromCbor(blobBytes(node.cborRoot));
        if (!(integer instanceof DataI)) {
          throw new Error("CEK integer Data leaf has a non-integer payload");
        }
        value = integer;
        break;
      }
      case "bytes": {
        const bytes = blobBytes(node.bytesRoot);
        if (BigInt(bytes.length) !== node.bytesLength) {
          throw new Error("CEK bytes Data leaf has the wrong length");
        }
        value = new DataB(bytes);
        break;
      }
    }
    const canonical = commitMidgardCekDataTree(value);
    if (
      !sameBytes(canonical.root, root) ||
      canonical.cborLength !== node.cborLength ||
      canonical.memory !== node.memory
    ) {
      throw new Error("CEK semantic Data material is not self-consistent");
    }
    return value;
  };

  return { blobBytes: (root) => blobBytes(root), dataValue };
};
