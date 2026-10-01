/**
 * Depth cases at the protocol maximum for the validation package's Plutus
 * Data reader, writer, memory size and semantic Data reader. Plutus Data has
 * no depth bound, only byte caps, so each must handle the deepest value the
 * redeemer byte cap admits without recursion. The same cases run on a process
 * main thread (about 1 MB of stack) and on a worker thread (4 MB).
 *
 * The BLS finalVerify expression walk is in the same set: its witness
 * arrives with any depth, and it must reach the ten-leaf refusal without
 * recursion.
 */
import { isMainThread } from "node:worker_threads";

import { decodeMidgardCekProgramBlobPreimage } from "@al-ft/midgard-core";
import {
  type DeepDataShape,
  deepPlutusDataCbor,
  PLUTUS_DATA_PROTOCOL_MAX_DEPTHS,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import {
  type Data,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  type KV,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  evaluateMidgardCekBlsFinal,
  type MidgardCekBlsExpressionWitness,
} from "../src/cek-builtin.js";
import { commitMidgardCekDataTree } from "../src/cek-data-tree.js";
import { readMidgardCekSemanticData } from "../src/cek-executor.structural-executor-data.js";
import { plutusDataFromCborIterative } from "../src/plutus-data-iterative.decode.js";
import { encodeMidgardCekPlutusData } from "../src/plutus-data-iterative.encode.js";
import { midgardCekDataMemorySize } from "../src/plutus-data-iterative.memory.js";
import {
  blsLeftChain,
  expressionRoot,
  G1,
  G2,
  leafRoot,
} from "./bls-expression.fixtures.js";

export type DepthThread = "main" | "worker";

const REDEEMER = PLUTUS_DATA_PROTOCOL_MAX_DEPTHS.redeemer;

/** Generous: the iterative sites finish each case in well under a second. */
const CASE_TIMEOUT_MS = 30_000;

const SHAPES: readonly DeepDataShape[] = [
  "definite-list",
  "indefinite-list",
  "constr",
  "map",
];

/** Follows the single path down to the innermost value; returns the depth. */
const pathDepth = (value: Data): number => {
  let depth = 0;
  let current = value;
  for (;;) {
    if (current instanceof DataList) {
      current = current.list[0]!;
    } else if (current instanceof DataConstr) {
      current = current.fields[0]!;
    } else if (current instanceof DataMap) {
      const entry = (current.map as readonly KV<Data, Data>[])[0]!;
      expect(entry.fst).toEqual(new DataI(0n));
      current = entry.snd;
    } else {
      break;
    }
    depth += 1;
  }
  expect(current).toBeInstanceOf(DataI);
  return depth;
};

/**
 * Every node is four memory words, and a map level's integer key 0 adds five
 * more; the innermost integer 0 adds five.
 */
const expectedMemory = (shape: DeepDataShape, depth: number): bigint =>
  BigInt(depth) * (shape === "map" ? 9n : 4n) + 5n;

/** Bytes per level in the canonical encoding, which writes maps definite. */
const encodedLevelBytes = (shape: DeepDataShape): number =>
  shape === "constr" ? 4 : 2;

/** The one-path value `deepPlutusDataCbor` encodes, built without decoding. */
const deepHarmonicValue = (shape: DeepDataShape, depth: number): Data => {
  let value: Data = new DataI(0n);
  for (let level = 0; level < depth; level += 1) {
    value =
      shape === "constr"
        ? new DataConstr(0n, [value])
        : shape === "map"
          ? new DataMap([{ fst: new DataI(0n), snd: value }])
          : new DataList([value]);
  }
  return value;
};

export const registerNoRecursionDepthCases = (thread: DepthThread): void => {
  it("runs on the intended thread", () => {
    expect(isMainThread).toBe(thread === "main");
  });

  it(
    "walks a 20,000-level BLS expression chain and refuses it by leaf count",
    () => {
      const leaf: MidgardCekBlsExpressionWitness = {
        kind: "millerLoop",
        g1: G1,
        g2: G2,
      };
      const chain = blsLeftChain(leaf, 20_000);
      expect(() =>
        evaluateMidgardCekBlsFinal(
          expressionRoot(chain),
          leafRoot(leaf),
          chain,
          leaf,
        ),
      ).toThrow(/ten-leaf/);
    },
    CASE_TIMEOUT_MS,
  );

  for (const shape of SHAPES) {
    const depth = REDEEMER[shape];
    const cbor = deepPlutusDataCbor(shape, depth);
    describe(`${shape} at the redeemer maximum depth ${depth.toString()}`, () => {
      it(
        "decodes it",
        () => {
          const value = plutusDataFromCborIterative(Buffer.from(cbor));
          expect(pathDepth(value)).toBe(depth);
        },
        CASE_TIMEOUT_MS,
      );

      it(
        "encodes and sizes it",
        () => {
          const value = deepHarmonicValue(shape, depth);
          const encoded = encodeMidgardCekPlutusData(value);
          expect(encoded.length).toBe(depth * encodedLevelBytes(shape) + 1);
          expect(midgardCekDataMemorySize(value)).toBe(
            expectedMemory(shape, depth),
          );
        },
        CASE_TIMEOUT_MS,
      );

      it(
        "reads it back from committed semantic material",
        () => {
          const tree = commitMidgardCekDataTree(
            deepHarmonicValue(shape, depth),
          );
          const pick = <N>(
            entries: ReadonlyMap<string, { readonly node: N }>,
          ): Map<string, N> =>
            new Map([...entries].map(([key, entry]) => [key, entry.node]));
          const read = readMidgardCekSemanticData(
            {
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
            Buffer.from(tree.root),
          );
          expect(pathDepth(read)).toBe(depth);
        },
        CASE_TIMEOUT_MS,
      );
    });
  }
};
