import { Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  blueprintDefinitionName,
  parseBlueprintAbi,
  refines,
} from "../src/blueprint-abi/index.js";

const ref = (name: string) => ({
  $ref: `#/definitions/${name.replace(/\//gu, "~1")}`,
});

const validator = {
  title: "example.main.spend",
  compiledCode: "4e4d01000033222220051200120011",
  parameters: [{ title: "policy", schema: ref("cardano/assets/PolicyId") }],
  redeemer: { title: "redeemer", schema: ref("example/Window") },
};

// `Window { start: Int, end: Int, owner: ByteArray }`, a recursive `Tree`, a
// tuple, an opaque `Data` field and a map — the shapes Aiken emits.
const definitions = {
  Int: { dataType: "integer" },
  ByteArray: { title: "ByteArray", dataType: "bytes" },
  Data: { title: "Data", description: "Any Plutus data." },
  "cardano/assets/PolicyId": { title: "PolicyId", dataType: "bytes" },
  "example/Window": {
    title: "Window",
    anyOf: [
      {
        title: "Window",
        dataType: "constructor",
        index: 0,
        fields: [
          { title: "start", ...ref("Int") },
          { title: "end", ...ref("Int") },
          { title: "owner_key", ...ref("ByteArray") },
          { title: "payload", ...ref("Data") },
          { title: "pair", ...ref("Tuple<Int,ByteArray>") },
          { title: "weights", ...ref("Pairs<ByteArray,Int>") },
        ],
      },
    ],
  },
  "Tuple<Int,ByteArray>": {
    title: "Tuple",
    dataType: "list",
    items: [ref("Int"), ref("ByteArray")],
  },
  "Pairs<ByteArray,Int>": {
    dataType: "map",
    keys: ref("ByteArray"),
    values: ref("Int"),
  },
  "example/Tree": {
    title: "Tree",
    anyOf: [
      {
        title: "Leaf",
        dataType: "constructor",
        index: 0,
        fields: [{ title: "value", ...ref("Int") }],
      },
      {
        title: "Node",
        dataType: "constructor",
        index: 1,
        fields: [{ title: "children", ...ref("List<example/Tree>") }],
      },
    ],
  },
  "List<example/Tree>": { dataType: "list", items: ref("example/Tree") },
  "example/Positional": {
    title: "Positional",
    anyOf: [
      {
        title: "Positional",
        dataType: "constructor",
        index: 0,
        fields: [ref("Int")],
      },
    ],
  },
};

const abi = parseBlueprintAbi({ validators: [validator], definitions });

const WindowSchema = Data.Object({
  start: Data.Integer(),
  end: Data.Integer(),
  ownerKey: Data.Bytes({ minLength: 28, maxLength: 28 }),
  payload: Data.Object({ inner: Data.Integer() }),
  pair: Data.Tuple([Data.Integer(), Data.Bytes()]),
  weights: Data.Map(Data.Bytes(), Data.Integer()),
});

/** A copy of `schema` with constructor 0's fields `left` and `right` swapped. */
const withFieldsSwapped = (schema: unknown, left: number, right: number) => {
  const copy = structuredClone(schema) as {
    anyOf: [{ fields: unknown[] }];
  };
  const fields = copy.anyOf[0].fields;
  [fields[left], fields[right]] = [fields[right], fields[left]];
  return copy;
};

describe("parseBlueprintAbi", () => {
  it("reads validators, their parameters and argument schemas", () => {
    expect(abi.validator(validator.title)).toEqual({
      title: validator.title,
      compiledCode: validator.compiledCode,
      parameters: [
        { title: "policy", $ref: "#/definitions/cardano~1assets~1PolicyId" },
      ],
      redeemer: ref("example/Window"),
    });
    expect(
      parseBlueprintAbi({
        validators: [{ ...validator, parameters: undefined }],
      }).validators[0]?.parameters,
    ).toEqual([]);
  });

  it("refuses a malformed blueprint, a missing or duplicated title and an unknown $ref form", () => {
    expect(() => parseBlueprintAbi(null)).toThrow(
      "Blueprint must be a JSON object",
    );
    expect(() => parseBlueprintAbi({ definitions })).toThrow(
      "Blueprint must contain validators[]",
    );
    expect(() =>
      parseBlueprintAbi({ validators: [{ compiledCode: "00" }] }),
    ).toThrow("validators[0].title must be a string");
    expect(() =>
      parseBlueprintAbi({
        validators: [
          {
            ...validator,
            parameters: [{ title: "p", schema: { $ref: "other.json#/x" } }],
          },
        ],
      }),
    ).toThrow(/Unsupported blueprint \$ref/u);
    expect(() => abi.validator("absent.main.spend")).toThrow(/found 0/u);
    expect(() =>
      parseBlueprintAbi({ validators: [validator, validator] }).validator(
        validator.title,
      ),
    ).toThrow(/found 2/u);
  });

  it("resolves a definition by name or pointer and refuses a missing one", () => {
    expect(abi.ref("#/definitions/cardano~1assets~1PolicyId").name).toBe(
      "cardano/assets/PolicyId",
    );
    expect(abi.ref("example/Window").schema).toBe(
      definitions["example/Window"],
    );
    expect(() => abi.ref("example/Absent")).toThrow(
      'Blueprint is missing definition "example/Absent"',
    );
    expect(() => abi.ref("#/components/Int")).toThrow(
      /Unsupported blueprint \$ref/u,
    );
    expect(blueprintDefinitionName("#/definitions/List<a~1b~0c>")).toBe(
      "List<a/b~c>",
    );
    expect(() => blueprintDefinitionName("#/definitions/a/b")).toThrow(
      /Unsupported/u,
    );
  });

  it("lists constructors by index and refuses an absent constructor or an untitled field", () => {
    expect(abi.ref("example/Tree").constructors()).toEqual([
      { title: "Leaf", index: 0, fieldTitles: ["value"] },
      { title: "Node", index: 1, fieldTitles: ["children"] },
    ]);
    expect(abi.ref("example/Tree").constructorTitled("Node").index).toBe(1);
    expect(() => abi.ref("example/Tree").constructorTitled("Fork")).toThrow(
      'declares no constructor "Fork"',
    );
    expect(() => abi.ref("example/Positional").constructors()).toThrow(
      "field 0 has no title",
    );
    expect(() => abi.ref("Int").constructors()).toThrow(
      "declares no constructors",
    );
  });
});

describe("refines", () => {
  it("accepts a TypeScript schema that is the definition or narrower", () => {
    expect(refines(WindowSchema, abi.ref("example/Window"))).toEqual([]);
  });

  it("reports a reorder of two same-typed Int fields by title", () => {
    expect(
      refines(withFieldsSwapped(WindowSchema, 0, 1), abi.ref("example/Window")),
    ).toEqual([
      {
        path: "$.Window.start",
        message: 'field 0 is "start" in the blueprint, "end" in TypeScript',
      },
      {
        path: "$.Window.end",
        message: 'field 1 is "end" in the blueprint, "start" in TypeScript',
      },
    ]);
  });

  it("reports a TypeScript schema wider than the blueprint, an arity change and a tuple mismatch", () => {
    const wider = Data.Object({
      start: Data.Any(),
      end: Data.Integer(),
      ownerKey: Data.Bytes(),
      payload: Data.Any(),
      pair: Data.Tuple([Data.Integer(), Data.Integer()]),
      weights: Data.Map(Data.Bytes(), Data.Integer()),
    });
    expect(refines(wider, abi.ref("example/Window"))).toEqual([
      {
        path: "$.Window.start",
        message: "blueprint declares integer, TypeScript any Data",
      },
      {
        path: "$.Window.pair[1]",
        message: "blueprint declares bytes, TypeScript integer",
      },
    ]);
    expect(
      refines(
        Data.Object({ start: Data.Integer() }),
        abi.ref("example/Window"),
      ),
    ).toEqual([
      {
        path: "$.Window",
        message: "blueprint declares 6 field(s), TypeScript 1",
      },
    ]);
  });

  it("compares a recursive definition coinductively", () => {
    const tree: { anyOf: unknown[] } = { anyOf: [] };
    const node = { title: "children", dataType: "list", items: tree };
    tree.anyOf.push(
      {
        dataType: "constructor",
        index: 0,
        fields: [{ ...Data.Integer(), title: "value" }],
      },
      { dataType: "constructor", index: 1, fields: [node] },
    );
    expect(refines(tree, abi.ref("example/Tree"))).toEqual([]);

    const misnamed = { ...node, title: "kids" };
    tree.anyOf[1] = { dataType: "constructor", index: 1, fields: [misnamed] };
    expect(refines(tree, abi.ref("example/Tree"))).toEqual([
      {
        path: "$.Node.children",
        message: 'field 0 is "children" in the blueprint, "kids" in TypeScript',
      },
    ]);
  });

  it("throws on a reference the blueprint cannot resolve", () => {
    const dangling = parseBlueprintAbi({
      validators: [],
      definitions: {
        Outer: { dataType: "list", items: ref("Inner") },
        Odd: { dataType: "list", items: { $ref: "#/elsewhere/Inner" } },
      },
    });
    expect(() =>
      refines(Data.Array(Data.Integer()), dangling.ref("Outer")),
    ).toThrow('Blueprint is missing definition "Inner"');
    expect(() =>
      refines(Data.Array(Data.Integer()), dangling.ref("Odd")),
    ).toThrow(/Unsupported blueprint \$ref/u);
  });
});
