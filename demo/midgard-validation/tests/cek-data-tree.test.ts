import {
  encodeMidgardCekDataNodeV1,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
} from "@al-ft/midgard-core";
import {
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  DataPair,
} from "@harmoniclabs/plutus-data";
import { describe, expect, it } from "vitest";

import {
  encodeMidgardCekPlutusDataV1,
  midgardCekDataMemorySizeV1,
} from "../src/cek-constant.js";
import { commitMidgardCekDataTreeV1 } from "../src/cek-data-tree.js";

describe("V1 semantic Data commitment", () => {
  it("streams large leaves instead of imposing a whole-value cap", () => {
    const largeBytes = Buffer.alloc(10_000, 0xa5);
    const value = new DataConstr(128, [
      new DataList([
        new DataI(-1),
        new DataB(Buffer.alloc(65, 0x11)),
      ]),
      new DataB(largeBytes),
    ]);

    const committed = commitMidgardCekDataTreeV1(value);
    expect(committed.cborLength).toBe(
      BigInt(encodeMidgardCekPlutusDataV1(value).length),
    );
    expect(committed.memory).toBe(midgardCekDataMemorySizeV1(value));
    expect(committed.blobNodes.size).toBeGreaterThan(3);
    expect(
      [...committed.blobNodes.values()]
        .filter((node) => node.kind === "chunk")
        .every(
          (node) =>
            node.preimage.length <= MIDGARD_CEK_BLOB_CHUNK_BYTES + 3,
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

  it("pins the TypeScript/Aiken semantic-root vector", () => {
    const value = new DataConstr(128, [
      new DataI(-1),
      new DataB(Buffer.alloc(65, 0x2a)),
    ]);
    const committed = commitMidgardCekDataTreeV1(value);
    const rootNode = committed.dataNodes.get(
      Buffer.from(committed.root).toString("hex"),
    );
    expect(rootNode).toBeDefined();
    expect(rootNode?.preimage).toEqual(
      encodeMidgardCekDataNodeV1(rootNode!.node),
    );
    expect(Buffer.from(committed.root).toString("hex")).toBe(
      "9ce9a6db13fa610a6efad613e5266cefe3740f2de1dd4a014884fa3f717d69de",
    );
  });

  it("uses definite serialiseData maps without reordering raw pairs", () => {
    const value = new DataMap([
      new DataPair(new DataB(Buffer.from("11", "hex")), new DataI(1)),
      new DataPair(new DataB(Buffer.alloc(0)), new DataI(2)),
    ]);
    const committed = commitMidgardCekDataTreeV1(value);
    expect(encodeMidgardCekPlutusDataV1(value).toString("hex")).toBe(
      "a24111014002",
    );
    expect(committed.cborLength).toBe(6n);
    expect(committed.memory).toBe(24n);
    expect(Buffer.from(committed.root).toString("hex")).toBe(
      "96cf66f3acdee22a9661894a6fdae1deb78e5b434338a19cdc23df2d73fabb51",
    );
  });
});
