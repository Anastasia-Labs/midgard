import {
  encodeMidgardCekDataNode,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
} from "@al-ft/midgard-core";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
  DataMap,
} from "@harmoniclabs/plutus-data";
import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekPlutusData,
  midgardCekDataMemorySize,
} from "../src/cek-constant.js";
import {
  commitMidgardCekDataTree,
  encodeMidgardCekDataTreeInteger,
} from "../src/cek-data-tree.js";

describe("V1 semantic Data commitment", () => {
  it("encodes the complete signed Cardano integer domain canonically", () => {
    const uint64Max = 0xffff_ffff_ffff_ffffn;
    const positiveBignum = uint64Max + 1n;
    const negativeMajorOneBoundary = -positiveBignum;
    const negativeBignum = negativeMajorOneBoundary - 1n;

    expect(
      encodeMidgardCekPlutusData(new DataI(uint64Max)).toString("hex"),
    ).toBe("1bffffffffffffffff");
    expect(
      encodeMidgardCekPlutusData(new DataI(positiveBignum)).toString("hex"),
    ).toBe("c249010000000000000000");
    expect(
      encodeMidgardCekPlutusData(new DataI(negativeMajorOneBoundary)).toString(
        "hex",
      ),
    ).toBe("3bffffffffffffffff");
    expect(
      encodeMidgardCekPlutusData(new DataI(negativeBignum)).toString("hex"),
    ).toBe("c349010000000000000000");
    const decodedNegative = dataFromCbor(
      encodeMidgardCekPlutusData(new DataI(negativeBignum)),
    );
    expect(decodedNegative).toBeInstanceOf(DataI);
    expect((decodedNegative as DataI).int).toBe(negativeBignum);

    const hugeMagnitude = (1n << 2_048n) - 1n;
    const positive = encodeMidgardCekPlutusData(new DataI(hugeMagnitude));
    const negative = encodeMidgardCekPlutusData(new DataI(-(1n << 2_048n)));
    // Cardano chunks a magnitude over 64 bytes: 256 bytes are four 64-byte
    // chunks inside an indefinite byte string.
    expect(positive.subarray(0, 4).toString("hex")).toBe("c25f5840");
    expect(negative.subarray(0, 4).toString("hex")).toBe("c35f5840");
    expect(positive.length).toBe(267);
    expect(negative.length).toBe(267);
    // Integer leaves use the same Cardano wire form; CML independently checks
    // the full signed magnitude, chunk headers, and terminator.
    for (const value of [hugeMagnitude, -(1n << 2_048n)]) {
      const integer = CML.BigInteger.from_str(value.toString());
      const data = CML.PlutusData.new_integer(integer);
      try {
        expect(encodeMidgardCekDataTreeInteger(value).toString("hex")).toBe(
          data.to_cbor_hex(),
        );
      } finally {
        data.free();
        integer.free();
      }
    }
    expect(midgardCekDataMemorySize(new DataI(hugeMagnitude))).toBe(261n);
    expect(midgardCekDataMemorySize(new DataI(-(1n << 2_048n)))).toBe(261n);
  });

  it("streams large leaves instead of imposing a whole-value cap", () => {
    const largeBytes = Buffer.alloc(10_000, 0xa5);
    const value = new DataConstr(128, [
      new DataList([new DataI(-1), new DataB(Buffer.alloc(65, 0x11))]),
      new DataB(largeBytes),
    ]);

    const committed = commitMidgardCekDataTree(value);
    expect(committed.cborLength).toBe(
      BigInt(encodeMidgardCekPlutusData(value).length),
    );
    expect(committed.memory).toBe(midgardCekDataMemorySize(value));
    expect(committed.blobNodes.size).toBeGreaterThan(3);
    expect(
      [...committed.blobNodes.values()]
        .filter((node) => node.kind === "chunk")
        .every(
          (node) => node.preimage.length <= MIDGARD_CEK_BLOB_CHUNK_BYTES + 3,
        ),
    ).toBe(true);
    expect(
      [...committed.dataNodes.values()].every(
        (entry) => entry.preimage.length < 256,
      ),
    ).toBe(true);

    const rootNode = committed.dataNodes.get(
      Buffer.from(committed.root).toString("hex"),
    );
    expect(rootNode?.node.kind).toBe("constrLarge");
  });

  it("commits the maximum accepted unary depth without using the JS call stack", () => {
    let value: Data = new DataI(0);
    for (let depth = 0; depth < 4_043; depth += 1) {
      value = new DataConstr(0, [value]);
    }

    const committed = commitMidgardCekDataTree(value);

    expect(committed.cborLength).toBe(16_173n);
    expect(committed.memory).toBe(16_177n);
    expect(committed.dataNodes.size).toBe(4_044);
    expect(committed.listNodes.size).toBe(4_043);
  });

  it("pins the TypeScript/Aiken semantic-root vector", () => {
    const value = new DataConstr(128, [
      new DataI(-1),
      new DataB(Buffer.alloc(65, 0x2a)),
    ]);
    const committed = commitMidgardCekDataTree(value);
    const rootNode = committed.dataNodes.get(
      Buffer.from(committed.root).toString("hex"),
    );
    expect(rootNode).toBeDefined();
    expect(rootNode?.preimage).toEqual(
      encodeMidgardCekDataNode(rootNode!.node),
    );
    expect(Buffer.from(committed.root).toString("hex")).toBe(
      "9ce9a6db13fa610a6efad613e5266cefe3740f2de1dd4a014884fa3f717d69de",
    );
  });

  it("uses definite serialiseData maps without reordering raw pairs", () => {
    const value = new DataMap([
      { fst: new DataB(Buffer.from("11", "hex")), snd: new DataI(1) },
      { fst: new DataB(Buffer.alloc(0)), snd: new DataI(2) },
    ]);
    const committed = commitMidgardCekDataTree(value);
    expect(encodeMidgardCekPlutusData(value).toString("hex")).toBe(
      "a24111014002",
    );
    expect(committed.cborLength).toBe(6n);
    expect(committed.memory).toBe(24n);
    expect(Buffer.from(committed.root).toString("hex")).toBe(
      "96cf66f3acdee22a9661894a6fdae1deb78e5b434338a19cdc23df2d73fabb51",
    );
  });
});
