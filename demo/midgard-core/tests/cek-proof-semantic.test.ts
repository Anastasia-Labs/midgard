import { describe, expect, it } from "vitest";

import { commitSemanticData } from "../src/cek-proof.commit-semantic-data.js";
import {
  assertSemanticDataEncodable,
  encodeSemanticData,
} from "../src/cek-proof.encode-semantic-data.js";
import { type SemanticDataValue } from "../src/cek-proof.program-material-task.js";
import { makeSemanticDataReconstructor } from "../src/cek-proof.reconstruct-semantic-data.js";
import {
  addIntegerNode,
  addListLink,
  addListNode,
  addRawData,
  EMPTY_LIST,
  emptySemanticMaterial,
  hexRoot,
  materialSource,
} from "./cek-proof-semantic.material.js";

describe("semantic Data encode and commit", () => {
  it("refuses a list with holes", () => {
    const holes = [1n, , 2n] as unknown as SemanticDataValue; // eslint-disable-line no-sparse-arrays
    expect(() => encodeSemanticData(holes)).toThrow(
      "CEK constant contains unknown semantic Data",
    );
    expect(() => commitSemanticData(holes)).toThrow(
      "CEK constant contains unknown semantic Data",
    );
  });

  it("refuses a value that contains itself instead of overflowing", () => {
    const list: SemanticDataValue[] = [1n];
    list.push(list);
    expect(() => encodeSemanticData(list)).toThrow("contains a cycle");
    expect(() => commitSemanticData(list)).toThrow("contains a cycle");
  });

  it("commits a shared (DAG) value in linear time", () => {
    let value: SemanticDataValue = 7n;
    // 2^28 leaves as a tree; lengths stay below the 32-bit node fields.
    for (let level = 0; level < 28; level += 1) value = [value, value];
    const started = performance.now();
    const summary = commitSemanticData(value);
    expect(performance.now() - started).toBeLessThan(1_000);
    // Each level is `9f <child> <child> ff`: length 3 * 2^k - 2.
    expect(summary.cborLength).toBe(3n * 2n ** 28n - 2n);
    expect(() => assertSemanticDataEncodable(value)).not.toThrow();
  });
});

describe("semantic Data reconstruction", () => {
  it("names the leaf on a non-integer leaf", () => {
    const material = emptySemanticMaterial();
    const summary = addRawData(material, { kind: "intCbor", cborHex: "01" });
    const node = material.dataNodes.get(hexRoot(summary.root))!;
    if (node.kind !== "integer") throw new Error("expected an integer node");
    material.blobs.set(hexRoot(node.cborRoot), Buffer.from("6161", "hex"));
    expect(() =>
      makeSemanticDataReconstructor(materialSource(material))(summary.root),
    ).toThrow("CEK semantic integer leaf is invalid");
  });

  it("rebuilds a shared (DAG) material in linear time", () => {
    const material = emptySemanticMaterial();
    let summary = addIntegerNode(material, 7n);
    for (let level = 0; level < 28; level += 1) {
      const pair = addListLink(
        material,
        summary,
        addListLink(material, summary, EMPTY_LIST),
      );
      summary = addListNode(material, pair);
    }
    const started = performance.now();
    const rebuilt = makeSemanticDataReconstructor(materialSource(material))(
      summary.root,
    );
    const committed = commitSemanticData(rebuilt);
    expect(performance.now() - started).toBeLessThan(1_000);
    expect(hexRoot(committed.root)).toBe(hexRoot(summary.root));
    expect(committed.cborLength).toBe(summary.cborLength);
  });
});
