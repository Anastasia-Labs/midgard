/**
 * The iterative semantic Data and blob readers against the recursive ones
 * they replace (vendored in `structural-executor-data.legacy.ts`), over
 * committed material and tampered copies of it: the same value when both
 * accept, and the same error message when both reject.
 */
import {
  decodeMidgardCekProgramBlobPreimage,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  type MidgardCekDataListNode,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
  type MidgardCekDecodedProgramBlob,
} from "@al-ft/midgard-core";
import {
  type FuzzRng,
  makeFuzzRng,
  randomDataTree,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import {
  readMidgardCekSemanticBlob,
  readMidgardCekSemanticData,
} from "../src/cek-executor.structural-executor-data.js";
import { midgardDataPair } from "../src/plutus-data-iterative.pair.js";
import { harmonicDataDifference } from "./plutus-data-iterative.recursive-oracles.js";
import { legacySemanticReaders } from "./structural-executor-data.legacy.js";

type Material = {
  dataNodes: Map<string, MidgardCekDataNode>;
  dataLists: Map<string, MidgardCekDataListNode>;
  dataPairs: Map<string, MidgardCekDataPairNode>;
  blobs: Map<string, MidgardCekDecodedProgramBlob>;
};

const materialOf = (value: Data): { root: Buffer; material: Material } => {
  const tree = commitMidgardCekDataTree(value);
  const pick = <N>(
    entries: ReadonlyMap<string, { readonly node: N }>,
  ): Map<string, N> =>
    new Map([...entries].map(([key, entry]) => [key, entry.node]));
  return {
    root: Buffer.from(tree.root),
    material: {
      dataNodes: pick(tree.dataNodes),
      dataLists: pick(tree.listNodes),
      dataPairs: pick(tree.pairNodes),
      blobs: new Map(
        [...tree.blobNodes].map(([key, entry]) => [
          key,
          decodeMidgardCekProgramBlobPreimage(
            entry.kind === "chunk" ? "blobChunk" : "blobBranch",
            entry.preimage,
          ),
        ]),
      ),
    },
  };
};

type Outcome<T> =
  | { readonly ok: true; readonly value: T }
  | { readonly ok: false; readonly message: string };

const attempt = <T>(read: () => T): Outcome<T> => {
  try {
    return { ok: true, value: read() };
  } catch (error) {
    return { ok: false, message: (error as Error).message };
  }
};

/**
 * The readers' own refusals keep their messages. A malformed integer or
 * constructor blob is refused by the CBOR decoder instead, whose messages
 * differ from harmonic's (its accept/reject decisions are pinned by
 * plutus-data-iterative.differential.test.ts); there only the decision is
 * compared.
 */
const READER_MESSAGE = /^(CEK |missing authenticated |cyclic )/u;

/** Compares both readers on one root; returns whether they accepted. */
const compareReaders = (
  material: Material,
  root: Buffer,
  label: string,
): boolean => {
  const legacy = attempt(() => legacySemanticReaders(material).dataValue(root));
  const current = attempt(() => readMidgardCekSemanticData(material, root));
  expect(current.ok, label).toBe(legacy.ok);
  if (legacy.ok && current.ok) {
    expect(harmonicDataDifference(legacy.value, current.value), label).toBe(
      undefined,
    );
  } else if (!legacy.ok && !current.ok && READER_MESSAGE.test(legacy.message)) {
    expect(current.message, label).toBe(legacy.message);
  }
  for (const [key, node] of material.dataNodes) {
    const blobRoot =
      node.kind === "integer"
        ? node.cborRoot
        : node.kind === "bytes"
          ? node.bytesRoot
          : node.kind === "constrLarge"
            ? node.constructorCborRoot
            : undefined;
    if (blobRoot === undefined) continue;
    const oldBlob = attempt(() =>
      legacySemanticReaders(material).blobBytes(blobRoot),
    );
    const newBlob = attempt(() =>
      readMidgardCekSemanticBlob(material.blobs, blobRoot),
    );
    expect(newBlob, `${label} blob of ${key}`).toEqual(oldBlob);
  }
  return legacy.ok;
};

const randomBytes = (rng: FuzzRng, length: number): Buffer =>
  Buffer.from(Array.from({ length }, () => rng.int(256)));

/** Harmonic builders that sometimes emit multi-chunk (branch) blobs. */
const builders = (rng: FuzzRng) => ({
  integer: (value: bigint): Data =>
    new DataI(
      rng.chance(0.03)
        ? (1n << BigInt(33_000 + rng.int(40_000))) - value
        : value,
    ),
  bytes: (bytesHex: string): Data =>
    new DataB(
      rng.chance(0.05)
        ? randomBytes(rng, 4_000 + rng.int(10_000))
        : Buffer.from(bytesHex, "hex"),
    ),
  list: (items: Data[]): Data => new DataList(items),
  map: (entries: [Data, Data][]): Data =>
    new DataMap(entries.map(([key, value]) => midgardDataPair(key, value))),
  constr: (index: bigint, fields: Data[]): Data =>
    new DataConstr(index, fields),
});

const keysOf = <V>(map: Map<string, V>): string[] => [...map.keys()];

/** Applies one seeded tampering to a copy of the material. */
const tamper = (rng: FuzzRng, source: Material): Material => {
  const material: Material = {
    dataNodes: new Map(source.dataNodes),
    dataLists: new Map(source.dataLists),
    dataPairs: new Map(source.dataPairs),
    blobs: new Map(source.blobs),
  };
  const deleteOne = <V>(map: Map<string, V>): void => {
    const keys = keysOf(map);
    if (keys.length > 0) map.delete(rng.pick(keys));
  };
  const swapTwo = <V>(map: Map<string, V>): void => {
    const keys = keysOf(map);
    if (keys.length < 2) return;
    const [a, b] = [rng.pick(keys), rng.pick(keys)];
    const first = map.get(a)!;
    map.set(a, map.get(b)!);
    map.set(b, first);
  };
  const edit = <V>(map: Map<string, V>, change: (node: V) => V): void => {
    const keys = keysOf(map);
    if (keys.length === 0) return;
    const key = rng.pick(keys);
    map.set(key, change(map.get(key)!));
  };
  switch (rng.int(12)) {
    case 0:
      deleteOne(material.dataNodes);
      break;
    case 1:
      deleteOne(material.dataLists);
      break;
    case 2:
      deleteOne(material.dataPairs);
      break;
    case 3:
      deleteOne(material.blobs);
      break;
    case 4:
      swapTwo(material.dataNodes);
      break;
    case 5:
      swapTwo(material.blobs);
      break;
    case 6:
      edit(material.dataLists, (node) => ({
        ...node,
        length: node.length + 1n,
      }));
      break;
    case 7:
      edit(material.dataPairs, (node) => ({
        ...node,
        tail: rng.chance(0.5) ? node.key : MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      }));
      break;
    case 8:
      edit(material.blobs, (node) =>
        node.kind === "chunk"
          ? { ...node, bytes: Buffer.concat([node.bytes, Buffer.from([0])]) }
          : { ...node, byteLength: node.byteLength + 1n },
      );
      break;
    case 9:
      edit(material.dataNodes, (node) => ({
        ...node,
        memory: node.memory + (rng.chance(0.5) ? 1n : 0n),
        cborLength: node.cborLength + (rng.chance(0.5) ? 1n : 0n),
      }));
      break;
    case 10:
      edit(material.dataLists, (node) => {
        const other = rng.pick([...material.dataLists.values()]);
        return { ...node, tail: other.tail };
      });
      break;
    default:
      swapTwo(material.dataLists);
  }
  return material;
};

const DIFFERENTIAL_TIMEOUT_MS = 180_000;

describe("readMidgardCekSemanticData vs the recursive reader", () => {
  it(
    "reads 3,000 seeded committed values identically",
    () => {
      for (let seed = 1; seed <= 3_000; seed += 1) {
        const rng = makeFuzzRng(seed);
        const value = randomDataTree(rng, builders(rng), {
          maxDepth: 1 + rng.int(6),
          maxWidth: 1 + rng.int(4),
        });
        const { root, material } = materialOf(value);
        const label = `seed ${seed.toString()}`;
        expect(compareReaders(material, root, label), label).toBe(true);
        const read = readMidgardCekSemanticData(material, root);
        expect(harmonicDataDifference(value, read), label).toBe(undefined);
      }
    },
    DIFFERENTIAL_TIMEOUT_MS,
  );

  it(
    "refuses 6,000 seeded tamperings with the same error",
    () => {
      let refused = 0;
      for (let seed = 1; seed <= 6_000; seed += 1) {
        const rng = makeFuzzRng(seed ^ 0x7a3e);
        const value = randomDataTree(rng, builders(rng), {
          maxDepth: 1 + rng.int(5),
          maxWidth: 1 + rng.int(4),
        });
        const { root, material } = materialOf(value);
        const tampered = tamper(rng, material);
        if (!compareReaders(tampered, root, `tamper seed ${seed.toString()}`)) {
          refused += 1;
        }
      }
      expect(refused).toBeGreaterThan(2_000);
    },
    DIFFERENTIAL_TIMEOUT_MS,
  );

  it("refuses a cyclic blob commitment with the same error", () => {
    const { root, material } = materialOf(
      new DataB(Buffer.alloc(10_000, 0x5a)),
    );
    const bytesNode = material.dataNodes.get(root.toString("hex"))!;
    if (bytesNode.kind !== "bytes") throw new Error("expected a bytes node");
    const branchKey = Buffer.from(bytesNode.bytesRoot).toString("hex");
    const branch = material.blobs.get(branchKey)!;
    if (branch.kind !== "branch") throw new Error("expected a branch blob");
    material.blobs.set(branchKey, {
      ...branch,
      right: Buffer.from(bytesNode.bytesRoot),
    });
    compareReaders(material, root, "blob cycle");
    expect(() => readMidgardCekSemanticData(material, root)).toThrow(
      "cyclic CEK semantic blob commitment",
    );
  });

  it("refuses a cyclic Data commitment, which the recursive reader overflowed on", () => {
    const { root, material } = materialOf(new DataList([new DataI(1n)]));
    const listNode = material.dataNodes.get(root.toString("hex"))!;
    if (listNode.kind !== "list") throw new Error("expected a list node");
    const itemsKey = Buffer.from(listNode.itemsRoot).toString("hex");
    const items = material.dataLists.get(itemsKey)!;
    material.dataLists.set(itemsKey, { ...items, head: root });
    expect(() => legacySemanticReaders(material).dataValue(root)).toThrow(
      RangeError,
    );
    expect(() => readMidgardCekSemanticData(material, root)).toThrow(
      "cyclic CEK semantic Data commitment",
    );
  });
});
