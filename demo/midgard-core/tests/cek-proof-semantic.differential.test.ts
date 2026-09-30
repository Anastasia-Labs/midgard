import {
  type FuzzRng,
  makeFuzzRng,
  randomDataTree,
} from "@al-ft/midgard-test-support/plutus-data-fuzz";
import { describe, expect, it } from "vitest";

import { commitSemanticData } from "../src/cek-proof.commit-semantic-data.js";
import {
  assertSemanticDataEncodable,
  encodeSemanticData,
} from "../src/cek-proof.encode-semantic-data.js";
import { type SemanticDataValue } from "../src/cek-proof.program-material-task.js";
import { makeSemanticDataReconstructor } from "../src/cek-proof.reconstruct-semantic-data.js";
import {
  legacyCommitSemanticData,
  legacyEncodeSemanticData,
  makeLegacySemanticDataReconstructor,
} from "./cek-proof-semantic.legacy.js";
import {
  addIntegerNode,
  addListLink,
  addListNode,
  addRawData,
  EMPTY_LIST,
  emptySemanticMaterial,
  hexRoot,
  materialSource,
  type RawData,
  type SemanticMaterial,
} from "./cek-proof-semantic.material.js";

/**
 * The iterative semantic-Data encoder, commit walk and reconstructor against
 * the recursive versions they replaced (vendored in
 * `cek-proof-semantic.legacy.ts`): the same bytes, roots, lengths, memory and
 * rebuilt values, and the same first error.
 */

const SEEDED_CASES = 20_000;
/** Materials hash every node and blob, so fewer of them. */
const SEEDED_MATERIALS = 8_000;

const describeSemantic = (value: unknown): string => {
  if (typeof value === "bigint") return `${value}n`;
  if (typeof value === "string") return `h:${value}`;
  if (Array.isArray(value)) return `[${value.map(describeSemantic).join(",")}]`;
  if (value instanceof Map) {
    return `M{${[...(value as Map<unknown, unknown>)]
      .map(([k, v]) => `${describeSemantic(k)}=>${describeSemantic(v)}`)
      .join(",")}}`;
  }
  if (typeof value === "object" && value !== null && "kind" in value) {
    const constr = value as unknown as {
      constructor: bigint;
      fields: unknown[];
    };
    return `C${String(constr.constructor)}[${constr.fields
      .map(describeSemantic)
      .join(",")}]`;
  }
  return `?${String(value)}`;
};

const outcome = (run: () => unknown): string => {
  try {
    const result = run();
    if (Buffer.isBuffer(result)) return `ok ${result.toString("hex")}`;
    if (typeof result === "object" && result !== null && "root" in result) {
      const summary = result as {
        root: Uint8Array;
        cborLength: bigint;
        memory: bigint;
      };
      return `ok ${hexRoot(summary.root)}/${summary.cborLength}/${summary.memory}`;
    }
    return `ok ${describeSemantic(result)}`;
  } catch (error) {
    return `err ${error instanceof Error ? error.message : String(error)}`;
  }
};

/** Values the reconstructor never produces, to pin the error paths. */
const ODD_VALUES: readonly (() => unknown)[] = [
  () => undefined,
  () => null,
  () => 5,
  () => ({ kind: "other" }),
  () => ({ kind: "constr", constructor: -1n, fields: [] }),
  () => ({ kind: "constr", constructor: -200n, fields: [1n] }),
  () => "abc",
  () => "zz",
];

const semanticBuilders = (rng: FuzzRng) => {
  const odd = <T>(value: T): T =>
    rng.chance(0.02) ? (rng.pick(ODD_VALUES)() as T) : value;
  return {
    integer: (value: bigint) => odd<SemanticDataValue>(value),
    bytes: (hex: string) =>
      odd<SemanticDataValue>(rng.chance(0.1) ? hex.toUpperCase() : hex),
    list: (items: SemanticDataValue[]) => odd<SemanticDataValue>(items),
    map: (entries: [SemanticDataValue, SemanticDataValue][]) =>
      odd<SemanticDataValue>(new Map(entries)),
    constr: (constructor: bigint, fields: SemanticDataValue[]) =>
      odd<SemanticDataValue>({ kind: "constr", constructor, fields }),
  };
};

const rawBuilders = (rng: FuzzRng) => ({
  integer: (value: bigint): RawData =>
    value >= 0n && value < 24n && rng.chance(0.1)
      ? { kind: "intCbor", cborHex: `18${value.toString(16).padStart(2, "0")}` }
      : value,
  bytes: (hex: string): RawData => hex,
  list: (items: RawData[]): RawData => items,
  map: (entries: [RawData, RawData][]): RawData => ({ kind: "map", entries }),
  constr: (constructor: bigint, fields: RawData[]): RawData => ({
    kind: "constr",
    constructor,
    fields,
  }),
});

const LEAF_REPLACEMENTS = [
  "80",
  "a0",
  "d87980",
  "9f01ff",
  "41aa",
  "c28100",
  "1801",
  "3bffffffffffffffff",
  "c24100",
  "c34101",
  "20",
];

const pickKey = <V>(rng: FuzzRng, map: Map<string, V>): string | undefined =>
  map.size === 0 ? undefined : rng.pick([...map.keys()]);

/** One random corruption of the material, or none. */
const mutate = (rng: FuzzRng, material: SemanticMaterial): void => {
  const choice = rng.int(9);
  if (choice === 1) {
    const key = pickKey(rng, material.dataNodes);
    if (key !== undefined) material.dataNodes.delete(key);
  } else if (choice === 2) {
    const key = pickKey(rng, material.dataLists);
    if (key !== undefined) material.dataLists.delete(key);
  } else if (choice === 3) {
    const key = pickKey(rng, material.dataPairs);
    if (key !== undefined) material.dataPairs.delete(key);
  } else if (choice === 4) {
    const key = pickKey(rng, material.dataLists);
    if (key !== undefined) {
      const link = material.dataLists.get(key)!;
      material.dataLists.set(key, { ...link, length: link.length + 1n });
    }
  } else if (choice === 5) {
    const key = pickKey(rng, material.dataNodes);
    const node = key === undefined ? undefined : material.dataNodes.get(key);
    const delta = rng.chance(0.5) ? 1n : -1n;
    if (node?.kind === "list") {
      material.dataNodes.set(key!, {
        ...node,
        itemsCount: node.itemsCount + delta,
      });
    } else if (node?.kind === "map") {
      material.dataNodes.set(key!, {
        ...node,
        entriesCount: node.entriesCount + delta,
      });
    } else if (node?.kind === "constrSmall" || node?.kind === "constrLarge") {
      material.dataNodes.set(key!, {
        ...node,
        fieldsCount: node.fieldsCount + delta,
      });
    }
  } else if (choice === 6) {
    const key = pickKey(rng, material.dataLists);
    const other = pickKey(rng, material.dataLists);
    if (key !== undefined && other !== undefined) {
      const link = material.dataLists.get(key)!;
      material.dataLists.set(key, { ...link, tail: Buffer.from(other, "hex") });
    }
  } else if (choice === 7 || choice === 8) {
    const leaves = [...material.dataNodes.values()].flatMap((node) =>
      node.kind === "integer"
        ? [node.cborRoot]
        : node.kind === "constrLarge"
          ? [node.constructorCborRoot]
          : [],
    );
    if (leaves.length > 0) {
      material.blobs.set(
        hexRoot(rng.pick(leaves)),
        Buffer.from(rng.pick(LEAF_REPLACEMENTS), "hex"),
      );
    }
  }
};

describe("semantic Data encode and commit vs the recursive versions", () => {
  it(`agree on ${SEEDED_CASES} seeded trees`, () => {
    let accepted = 0;
    for (let i = 0; i < SEEDED_CASES; i += 1) {
      const rng = makeFuzzRng(0x5e4a0000 + i);
      const value = randomDataTree(rng, semanticBuilders(rng), {
        maxDepth: 1 + rng.int(6),
        maxWidth: 1 + rng.int(5),
      });
      const encoded = outcome(() => legacyEncodeSemanticData(value));
      expect(
        outcome(() => encodeSemanticData(value)),
        `encode ${i}`,
      ).toBe(encoded);
      expect(
        outcome(() => assertSemanticDataEncodable(value)).startsWith("err"),
        `assert ${i}`,
      ).toBe(encoded.startsWith("err"));
      const committed = outcome(() => legacyCommitSemanticData(value));
      expect(
        outcome(() => commitSemanticData(value)),
        `commit ${i}`,
      ).toBe(committed);
      if (committed.startsWith("ok")) accepted += 1;
    }
    expect(accepted).toBeGreaterThan(SEEDED_CASES / 2);
  }, 60_000);

  it("both refuse a list with holes", () => {
    const holes = [1n, , 2n] as unknown as SemanticDataValue; // eslint-disable-line no-sparse-arrays
    expect(() => legacyEncodeSemanticData(holes)).toThrow();
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

describe("semantic Data reconstruction vs the recursive version", () => {
  it(`agrees on ${SEEDED_MATERIALS} seeded materials, corrupted or not`, () => {
    let accepted = 0;
    for (let i = 0; i < SEEDED_MATERIALS; i += 1) {
      const rng = makeFuzzRng(0x4ec00000 + i);
      const raw = randomDataTree(rng, rawBuilders(rng), {
        maxDepth: 1 + rng.int(6),
        maxWidth: 1 + rng.int(5),
      });
      const material = emptySemanticMaterial();
      const root = addRawData(material, raw).root;
      mutate(rng, material);
      const roots = [
        root,
        ...Array.from({ length: 3 }, () =>
          material.dataNodes.size === 0
            ? root
            : Buffer.from(rng.pick([...material.dataNodes.keys()]), "hex"),
        ),
      ];
      const legacy = makeLegacySemanticDataReconstructor(
        materialSource(material),
      );
      const current = makeSemanticDataReconstructor(materialSource(material));
      for (const [index, target] of roots.entries()) {
        let expected = outcome(() => legacy(target));
        if (expected.includes("Maximum call stack")) {
          expected = "err CEK semantic Data node contains itself";
        }
        const actual = outcome(() => current(target));
        expect(actual, `seed ${i} root ${index}`).toBe(expected);
        if (index === 0 && expected.startsWith("ok")) {
          accepted += 1;
          const value = current(target);
          expect(outcome(() => commitSemanticData(value))).toBe(
            outcome(() => legacyCommitSemanticData(legacy(target))),
          );
        }
      }
    }
    expect(accepted).toBeGreaterThan(SEEDED_MATERIALS / 2);
  }, 60_000);

  it("still collapses duplicate map keys, so the root check refuses them", () => {
    const material = emptySemanticMaterial();
    const summary = addRawData(material, {
      kind: "map",
      entries: [
        [1n, 2n],
        [1n, 3n],
      ],
    });
    const rebuilt = makeSemanticDataReconstructor(materialSource(material))(
      summary.root,
    );
    expect(describeSemantic(rebuilt)).toBe("M{1n=>3n}");
    expect(hexRoot(commitSemanticData(rebuilt).root)).not.toBe(
      hexRoot(summary.root),
    );
  });

  it("names the leaf where Lucid would have failed on a non-integer leaf", () => {
    const material = emptySemanticMaterial();
    const summary = addRawData(material, { kind: "intCbor", cborHex: "01" });
    const node = material.dataNodes.get(hexRoot(summary.root))!;
    if (node.kind !== "integer") throw new Error("expected an integer node");
    material.blobs.set(hexRoot(node.cborRoot), Buffer.from("6161", "hex"));
    const source = materialSource(material);
    expect(() =>
      makeLegacySemanticDataReconstructor(source)(summary.root),
    ).toThrow();
    expect(() => makeSemanticDataReconstructor(source)(summary.root)).toThrow(
      "CEK semantic integer leaf is invalid",
    );
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
