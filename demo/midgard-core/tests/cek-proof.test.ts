import "vitest";
import "../src/cek-proof.js";
import "../src/cek-semantic.js";
import "./cek-proof.make-nested-list-constant-program-material.js";
import "./cek-proof.make-semantic-constant-program-material.js";

import { describe, expect, it } from "vitest";

import {
  commitMidgardCekBlob,
  decodeMidgardCekProgramMaterialEntry,
  decodeMidgardProofSubmission,
  encodeMidgardCekBlobBranch,
  encodeMidgardCekBlobChunk,
  encodeMidgardCekProgramMaterialEntry,
  encodeMidgardCekSequenceNode,
  encodeMidgardCekTermNode,
  encodeMidgardCekValueNode,
  encodeMidgardProofSubmission,
  hashMidgardCekBlobChunk,
  hashMidgardCekBlsExpressionNode,
  hashMidgardCekContinuationFrame,
  hashMidgardCekEnvironmentNode,
  hashMidgardCekMachineState,
  hashMidgardCekSequenceNode,
  hashMidgardCekTermNode,
  hashMidgardCekValueNode,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
  MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES,
  midgardCekProgramMaterialDependencies,
  type MidgardCekProgramMaterialEntry,
  MidgardCekProgramMaterialMissingRootError,
  verifyMidgardCekProgramMaterial,
} from "../src/cek-proof.js";
import {
  encodeMidgardCekDataListNode,
  encodeMidgardCekDataNode,
  encodeMidgardCekDataPairNode,
  hashMidgardCekDataNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
} from "../src/cek-semantic.js";
import type { Hash32 } from "../src/codec/hash.js";
import {
  type FixtureData,
  hash,
  hex,
  makeBytesConstantProgramMaterial,
  makeNestedListConstantProgramMaterial,
  programMaterialEntry,
} from "./cek-proof.make-nested-list-constant-program-material.js";
import { makeSemanticConstantProgramMaterial } from "./cek-proof.make-semantic-constant-program-material.js";

describe("V1 CEK commitments", () => {
  it("matches the Aiken node, state, and program vectors", () => {
    const term = hashMidgardCekTermNode({
      kind: "application",
      function: hash(1),
      argument: hash(2),
    });
    const value = hashMidgardCekValueNode({
      kind: "lambda",
      body: hash(3),
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
    });
    const sequence = hashMidgardCekSequenceNode({
      head: term,
      tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
      length: 1n,
    });
    const environment = hashMidgardCekEnvironmentNode({
      value,
      tail: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT,
      length: 1n,
    });
    const continuation = hashMidgardCekContinuationFrame({
      kind: "applyArgument",
      argument: hash(4),
      environment,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
    });
    const state = hashMidgardCekMachineState({
      mode: "compute",
      executionIndex: 2n,
      focusRoot: term,
      environmentRoot: environment,
      continuationRoot: continuation,
      auxiliary: 0n,
      cpu: 16_000n,
      memory: 100n,
    });
    const applyValue = hashMidgardCekContinuationFrame({
      kind: "applyValue",
      value: hash(1),
      tail: hash(2),
    });
    const caseSelect = hashMidgardCekContinuationFrame({
      kind: "caseSelect",
      environment: hash(1),
      tail: hash(2),
      valuesCount: 3n,
    });
    const caseApply = hashMidgardCekContinuationFrame({
      kind: "caseApply",
      environment: hash(1),
      builtContinuation: hash(2),
    });
    const caseState = hashMidgardCekMachineState({
      mode: "caseApply",
      executionIndex: 0n,
      focusRoot: hash(1),
      environmentRoot: hash(2),
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      auxiliary: 3n,
      cpu: 4n,
      memory: 5n,
    });
    const semanticBuiltinState = hashMidgardCekMachineState({
      mode: "semanticBuiltin",
      executionIndex: 0n,
      focusRoot: hash(1),
      environmentRoot: hash(2),
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT,
      auxiliary: 3n,
      cpu: 4n,
      memory: 5n,
    });
    const blsLeaf = hashMidgardCekBlsExpressionNode({
      kind: "millerLoop",
      g1Value: hash(1),
      g2Value: hash(2),
    });
    const blsMultiply = hashMidgardCekBlsExpressionNode({
      kind: "multiply",
      left: hash(1),
      right: hash(2),
    });
    const blsValue = hashMidgardCekValueNode({
      kind: "blsMillerLoop",
      expressionRoot: blsLeaf,
    });

    expect(hex(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)).toBe(
      "8ab46e13655026ca6fe253b057ff678ebf9fa088097d6fa4c62276ba140f1743",
    );
    expect(hex(MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT)).toBe(
      "0b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7",
    );
    expect(hex(MIDGARD_CEK_EMPTY_CONTINUATION_ROOT)).toBe(
      "53163c160dcec15695dabe0bccf6afc7f0e12db206392865db2feb0497ac838b",
    );
    expect(hex(term)).toBe(
      "2a37aa5b923cf90c6f3c8849e8fe2b28adcda97ccd736af6bf35b8312035f431",
    );
    expect(hex(value)).toBe(
      "a103975a15b084afa3e69e3f71ff66b57d3ce83f992df1325d0958245d337941",
    );
    expect(hex(sequence)).toBe(
      "854e2610e77c03a89283632923ddb99af0f276bedae48865036e85c2ed1f23cb",
    );
    expect(hex(environment)).toBe(
      "4c2bb324a912cff2fc99c9056faa7d8bb72ab5dd512362fbd6a7383fe9c71a5c",
    );
    expect(hex(continuation)).toBe(
      "7082fd0df1eb9680517dd87620f9fa94e5ea4598a56676e224f20a833bea3513",
    );
    expect(hex(state)).toBe(
      "8a475241923d49a38fa1d6376dd1c40ebb8d16adc87f51bc4a6dea7d954f88d1",
    );
    expect(hex(applyValue)).toBe(
      "facc375c9390b9503ccb80c0500bb3b88dc62c5da62c05599e6035938535fe61",
    );
    expect(hex(caseSelect)).toBe(
      "e71825335e324be2d099381365d5bc42c769bb334df509e5adae883842b9c1ea",
    );
    expect(hex(caseApply)).toBe(
      "12aedf8df3ce9e44db4cc847cd52d0cff0b9b832ff4425954fa7c38c523e9888",
    );
    expect(hex(caseState)).toBe(
      "31884c469ef240c7bad0a586ca242f45fa1728d13ab8385a97e5e0a18b806eb6",
    );
    expect(hex(semanticBuiltinState)).toBe(
      "9d83a7edc397becce073970978dff4d6bae2c0334f0dc7c3525e35d901f47a13",
    );
    expect(hex(blsLeaf)).toBe(
      "6bd7d80222a87e5a09102534274099c501f82c5d898fe75f24fdc951c2fb3cc0",
    );
    expect(hex(blsMultiply)).toBe(
      "be5de65e45ff867ca8394e227ee30e4f8cfaee0d4cb1d2834fe1af943b806082",
    );
    expect(hex(blsValue)).toBe(
      "d93762ca25b0c585073f9e97f50873b76e13de1290182f6bb4744d29d9fb3fb7",
    );
    expect(hex(hashMidgardCekBlobChunk(Buffer.from("010203", "hex")))).toBe(
      "0bb4f6f24cd0080e59e98d57a13c6453e07991452b80f14ff1d9791c02db1fc9",
    );
  });

  it("fails closed on malformed hashes, oversized chunks, and unknown builtins", () => {
    expect(() =>
      hashMidgardCekTermNode({
        kind: "application",
        function: Buffer.alloc(31),
        argument: hash(2),
      }),
    ).toThrow(/must be 32 bytes/u);
    expect(() =>
      hashMidgardCekBlobChunk(Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES + 1)),
    ).toThrow(/at most 4095 bytes/u);
    expect(() =>
      hashMidgardCekTermNode({ kind: "builtin", tag: 101n }),
    ).toThrow(/between 0 and 100/u);
    expect(() =>
      hashMidgardCekSequenceNode({
        head: hash(1),
        tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
        length: 0n,
      }),
    ).toThrow(/must be positive/u);
  });

  it("extracts the exact ordered direct dependency set for every V1 material kind", () => {
    const one = hash(1);
    const two = hash(2);
    const three = hash(3);
    const four = hash(4);
    const five = hash(5);
    const six = hash(6);
    const dataList = (length: bigint, head: Hash32, tail: Hash32) =>
      encodeMidgardCekDataListNode({
        head,
        headCborLength: 1n,
        headMemory: 1n,
        tail,
        length,
        payloadCborLength: length,
        memory: length,
      });
    const dataPair = (
      length: bigint,
      key: Hash32,
      value: Hash32,
      tail: Hash32,
    ) =>
      encodeMidgardCekDataPairNode({
        key,
        keyCborLength: 1n,
        keyMemory: 1n,
        value,
        valueCborLength: 1n,
        valueMemory: 1n,
        tail,
        length,
        payloadCborLength: length * 2n,
        memory: length * 2n,
      });
    const cases: readonly {
      readonly name: string;
      readonly entry: MidgardCekProgramMaterialEntry;
      readonly expected: readonly Hash32[];
    }[] = [
      {
        name: "term variable",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "variable", index: 0n }),
        ),
        expected: [],
      },
      {
        name: "term error",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "error" }),
        ),
        expected: [],
      },
      {
        name: "term builtin",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "builtin", tag: 0n }),
        ),
        expected: [],
      },
      {
        name: "term delay",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "delay", body: one }),
        ),
        expected: [one],
      },
      {
        name: "term lambda",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "lambda", body: one }),
        ),
        expected: [one],
      },
      {
        name: "term force",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "force", term: one }),
        ),
        expected: [one],
      },
      {
        name: "term application",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "application",
            function: one,
            argument: two,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "term application deduplicates",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "application",
            function: one,
            argument: one,
          }),
        ),
        expected: [one],
      },
      {
        name: "term constant",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({ kind: "constant", value: three }),
        ),
        expected: [three],
      },
      {
        name: "empty term constr",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 0n,
            termsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty term constr",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 2n,
            termsRoot: four,
          }),
        ),
        expected: [four],
      },
      {
        name: "empty term case",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 0n,
            branchesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        expected: [one],
      },
      {
        name: "non-empty term case",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 2n,
            branchesRoot: four,
          }),
        ),
        expected: [one, four],
      },
      {
        name: "value",
        entry: programMaterialEntry(
          "value",
          encodeMidgardCekValueNode({
            kind: "constant",
            typeRoot: one,
            payloadRoot: two,
            payloadLength: 1n,
            semanticRoot: two,
            memory: 1n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "value deduplicates roots",
        entry: programMaterialEntry(
          "value",
          encodeMidgardCekValueNode({
            kind: "constant",
            typeRoot: one,
            payloadRoot: one,
            payloadLength: 1n,
            semanticRoot: one,
            memory: 1n,
          }),
        ),
        expected: [one],
      },
      {
        name: "one-item sequence",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            length: 1n,
          }),
        ),
        expected: [one],
      },
      {
        name: "multi-item sequence",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: two,
            length: 2n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "blob chunk",
        entry: programMaterialEntry(
          "blobChunk",
          encodeMidgardCekBlobChunk(Buffer.from("0102", "hex")),
        ),
        expected: [],
      },
      {
        name: "blob branch",
        entry: programMaterialEntry(
          "blobBranch",
          encodeMidgardCekBlobBranch({
            left: one,
            right: two,
            byteLength: 2n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "blob branch deduplicates",
        entry: programMaterialEntry(
          "blobBranch",
          encodeMidgardCekBlobBranch({
            left: one,
            right: one,
            byteLength: 2n,
          }),
        ),
        expected: [one],
      },
      {
        name: "empty small constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 0n,
            fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty small constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 1n,
            fieldsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [one],
      },
      {
        name: "empty large constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrLarge",
            constructorCborRoot: two,
            constructorCborLength: 1n,
            constructorMemory: 1n,
            fieldsCount: 0n,
            fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [two],
      },
      {
        name: "non-empty large constructor Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrLarge",
            constructorCborRoot: two,
            constructorCborLength: 1n,
            constructorMemory: 1n,
            fieldsCount: 1n,
            fieldsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [one, two],
      },
      {
        name: "empty map Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 0n,
            entriesRoot: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty map Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 1n,
            entriesRoot: three,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [three],
      },
      {
        name: "empty list Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 0n,
            itemsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [],
      },
      {
        name: "non-empty list Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 1n,
            itemsRoot: four,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [four],
      },
      {
        name: "integer Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "integer",
            cborRoot: five,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [five],
      },
      {
        name: "bytes Data",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "bytes",
            bytesRoot: six,
            bytesLength: 1n,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        expected: [six],
      },
      {
        name: "one-item Data list",
        entry: programMaterialEntry(
          "dataList",
          dataList(1n, one, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
        ),
        expected: [one],
      },
      {
        name: "multi-item Data list",
        entry: programMaterialEntry("dataList", dataList(2n, one, two)),
        expected: [one, two],
      },
      {
        name: "one-item Data pair",
        entry: programMaterialEntry(
          "dataPair",
          dataPair(1n, one, two, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
        ),
        expected: [one, two],
      },
      {
        name: "multi-item Data pair deduplicates",
        entry: programMaterialEntry("dataPair", dataPair(2n, one, one, two)),
        expected: [one, two],
      },
    ];

    expect(
      cases.map(({ name, entry }) => ({
        name,
        dependencies: midgardCekProgramMaterialDependencies(entry).map(hex),
      })),
    ).toEqual(
      cases.map(({ name, expected }) => ({
        name,
        dependencies: expected.map(hex),
      })),
    );
  });

  it("fails closed while extracting hostile material dependencies", () => {
    const one = hash(1);
    const two = hash(2);
    const dataList = (length: bigint, tail: Hash32) =>
      programMaterialEntry(
        "dataList",
        encodeMidgardCekDataListNode({
          head: one,
          headCborLength: 1n,
          headMemory: 1n,
          tail,
          length,
          payloadCborLength: 1n,
          memory: 1n,
        }),
      );
    const dataPair = (length: bigint, tail: Hash32) =>
      programMaterialEntry(
        "dataPair",
        encodeMidgardCekDataPairNode({
          key: one,
          keyCborLength: 1n,
          keyMemory: 1n,
          value: two,
          valueCborLength: 1n,
          valueMemory: 1n,
          tail,
          length,
          payloadCborLength: 2n,
          memory: 2n,
        }),
      );
    const hostile: readonly {
      readonly name: string;
      readonly entry: MidgardCekProgramMaterialEntry;
      readonly error: RegExp;
    }[] = [
      {
        name: "unauthenticated root",
        entry: {
          ...programMaterialEntry(
            "term",
            encodeMidgardCekTermNode({ kind: "error" }),
          ),
          root: hash(99),
        },
        error: /root does not match its preimage/u,
      },
      {
        name: "non-canonical preimage",
        entry: programMaterialEntry("term", Buffer.from("811806", "hex")),
        error: /Non-minimal CBOR/u,
      },
      {
        name: "runtime-only context constant",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "contextConstant",
            value: one,
          }),
        ),
        error: /runtime-only context constant/u,
      },
      {
        name: "split value roots",
        entry: programMaterialEntry(
          "value",
          encodeMidgardCekValueNode({
            kind: "constant",
            typeRoot: one,
            payloadRoot: one,
            payloadLength: 1n,
            semanticRoot: two,
            memory: 1n,
          }),
        ),
        error: /payload root must equal its canonical semantic root/u,
      },
      {
        name: "empty constr with non-canonical root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 0n,
            termsRoot: one,
          }),
        ),
        error: /empty CEK constr sequence/u,
      },
      {
        name: "non-empty constr with empty root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "constr",
            tag: 0n,
            termsCount: 1n,
            termsRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        error: /non-empty CEK constr sequence/u,
      },
      {
        name: "empty case with non-canonical root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 0n,
            branchesRoot: two,
          }),
        ),
        error: /empty CEK case sequence/u,
      },
      {
        name: "non-empty case with empty root",
        entry: programMaterialEntry(
          "term",
          encodeMidgardCekTermNode({
            kind: "case",
            scrutinee: one,
            branchesCount: 1n,
            branchesRoot: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
          }),
        ),
        error: /non-empty CEK case sequence/u,
      },
      {
        name: "one-item sequence with non-canonical tail",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: two,
            length: 1n,
          }),
        ),
        error: /one-item CEK sequence/u,
      },
      {
        name: "multi-item sequence with empty tail",
        entry: programMaterialEntry(
          "sequence",
          encodeMidgardCekSequenceNode({
            head: one,
            tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT,
            length: 2n,
          }),
        ),
        error: /multi-item CEK sequence/u,
      },
      {
        name: "empty Data constructor with non-canonical root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 0n,
            fieldsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /empty CEK Data constructor/u,
      },
      {
        name: "non-empty Data constructor with empty root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "constrSmall",
            constructor: 0n,
            fieldsCount: 1n,
            fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /non-empty CEK Data constructor/u,
      },
      {
        name: "empty Data map with non-canonical root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 0n,
            entriesRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /empty CEK Data map/u,
      },
      {
        name: "non-empty Data map with empty root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "map",
            entriesCount: 1n,
            entriesRoot: MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /non-empty CEK Data map/u,
      },
      {
        name: "empty Data list node with non-canonical root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 0n,
            itemsRoot: one,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /empty CEK Data list/u,
      },
      {
        name: "non-empty Data list node with empty root",
        entry: programMaterialEntry(
          "dataNode",
          encodeMidgardCekDataNode({
            kind: "list",
            itemsCount: 1n,
            itemsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            cborLength: 1n,
            memory: 1n,
          }),
        ),
        error: /non-empty CEK Data list/u,
      },
      {
        name: "zero-length Data list material",
        entry: dataList(0n, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
        error: /Data list material length must be positive/u,
      },
      {
        name: "one-item Data list with non-canonical tail",
        entry: dataList(1n, two),
        error: /one-item CEK Data list/u,
      },
      {
        name: "multi-item Data list with empty tail",
        entry: dataList(2n, MIDGARD_CEK_EMPTY_DATA_LIST_ROOT),
        error: /multi-item CEK Data list/u,
      },
      {
        name: "zero-length Data pair material",
        entry: dataPair(0n, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
        error: /Data pair material length must be positive/u,
      },
      {
        name: "one-item Data pair with non-canonical tail",
        entry: dataPair(1n, hash(3)),
        error: /one-item CEK Data pair list/u,
      },
      {
        name: "multi-item Data pair with empty tail",
        entry: dataPair(2n, MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT),
        error: /multi-item CEK Data pair list/u,
      },
    ];

    for (const { entry, error } of hostile) {
      expect(() => midgardCekProgramMaterialDependencies(entry)).toThrow(error);
    }
  });

  it("matches semantic constant payloads at the authenticated material boundary", () => {
    const validCases: readonly {
      readonly name: string;
      readonly typeTags: readonly number[];
      readonly payload: FixtureData;
      readonly memory: bigint;
    }[] = [
      { name: "integer", typeTags: [0], payload: 0n, memory: 1n },
      { name: "bytes", typeTags: [1], payload: "00ff", memory: 1n },
      {
        name: "UTF-8 string",
        typeTags: [2],
        payload: "c3a9",
        memory: 0n,
      },
      {
        name: "Unit",
        typeTags: [3],
        payload: { kind: "constr", constructor: 0n, fields: [] },
        memory: 1n,
      },
      {
        name: "Bool false",
        typeTags: [4],
        payload: { kind: "constr", constructor: 0n, fields: [] },
        memory: 1n,
      },
      {
        name: "Bool true",
        typeTags: [4],
        payload: { kind: "constr", constructor: 1n, fields: [] },
        memory: 1n,
      },
      {
        name: "list",
        typeTags: [5, 3],
        payload: [
          { kind: "constr", constructor: 0n, fields: [] },
          { kind: "constr", constructor: 0n, fields: [] },
        ],
        memory: 2n,
      },
      {
        name: "pair",
        typeTags: [6, 1, 0],
        payload: {
          kind: "constr",
          constructor: 0n,
          fields: ["aa", 0n],
        },
        memory: 2n,
      },
      {
        name: "Data",
        typeTags: [8],
        payload: { kind: "constr", constructor: 2n, fields: [0n] },
        memory: 9n,
      },
      {
        name: "G1",
        typeTags: [9],
        payload: "00".repeat(48),
        memory: 18n,
      },
      {
        name: "G2",
        typeTags: [10],
        payload: "00".repeat(96),
        memory: 36n,
      },
    ];
    for (const testCase of validCases) {
      const fixture = makeSemanticConstantProgramMaterial(
        testCase.typeTags,
        testCase.payload,
        testCase.memory,
      );
      const verified = verifyMidgardCekProgramMaterial(
        fixture.summary.termRoot,
        fixture.material,
      );
      expect(verified.constants, testCase.name).toHaveLength(1);
      expect(verified.constants[0]!.typeCbor, testCase.name).toEqual(
        fixture.typeCbor,
      );
      expect(verified.constants[0]!.payloadCbor, testCase.name).toEqual(
        fixture.payloadCbor,
      );
    }

    const invalidCases: readonly {
      readonly name: string;
      readonly typeTags: readonly number[];
      readonly payload: FixtureData;
      readonly memory: bigint;
    }[] = [
      {
        name: "integer rejects bytes",
        typeTags: [0],
        payload: "00",
        memory: 1n,
      },
      {
        name: "bytes rejects integer",
        typeTags: [1],
        payload: 0n,
        memory: 1n,
      },
      {
        name: "string rejects invalid UTF-8",
        typeTags: [2],
        payload: "ff",
        memory: 1n,
      },
      {
        name: "Unit rejects another constructor",
        typeTags: [3],
        payload: { kind: "constr", constructor: 1n, fields: [] },
        memory: 1n,
      },
      {
        name: "Bool rejects an unknown constructor",
        typeTags: [4],
        payload: { kind: "constr", constructor: 2n, fields: [] },
        memory: 1n,
      },
      {
        name: "list rejects a wrongly typed element",
        typeTags: [5, 3],
        payload: [0n],
        memory: 1n,
      },
      {
        name: "pair rejects a non-zero constructor",
        typeTags: [6, 1, 0],
        payload: {
          kind: "constr",
          constructor: 1n,
          fields: ["aa", 0n],
        },
        memory: 1n,
      },
      {
        name: "G1 rejects the wrong payload length",
        typeTags: [9],
        payload: "00".repeat(47),
        memory: 18n,
      },
      {
        name: "G2 rejects the wrong payload length",
        typeTags: [10],
        payload: "00".repeat(95),
        memory: 36n,
      },
      {
        name: "Miller-loop type rejects Data",
        typeTags: [11],
        payload: { kind: "constr", constructor: 0n, fields: [] },
        memory: 72n,
      },
    ];
    for (const testCase of invalidCases) {
      const fixture = makeSemanticConstantProgramMaterial(
        testCase.typeTags,
        testCase.payload,
        testCase.memory,
      );
      expect(
        () =>
          verifyMidgardCekProgramMaterial(
            fixture.summary.termRoot,
            fixture.material,
          ),
        testCase.name,
      ).toThrow(/payload does not match its semantic type/u);
    }
  });

  it("authenticates and traverses exact content-addressed program material", () => {
    const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
    const rawValue = Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES + 1, 0x5a);
    const payloadBytes = Buffer.concat([
      Buffer.from([0x5f]),
      ...Array.from({ length: Math.ceil(rawValue.length / 64) }, (_, index) => {
        const chunk = rawValue.subarray(index * 64, (index + 1) * 64);
        return Buffer.concat([Buffer.from([0x58, chunk.length]), chunk]);
      }),
      Buffer.from([0xff]),
    ]);
    const rawBlob = commitMidgardCekBlob(rawValue);
    const semanticNode = {
      kind: "bytes",
      bytesRoot: rawBlob.root,
      bytesLength: BigInt(rawValue.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(rawValue.length)),
      memory: 4n + BigInt(Math.ceil(rawValue.length / 8)),
    } as const;
    const semanticRoot = hashMidgardCekDataNode(semanticNode);
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: semanticRoot,
      payloadLength: BigInt(payloadBytes.length),
      semanticRoot,
      memory: BigInt(Math.ceil(rawValue.length / 8)),
    } as const;
    const valuePreimage = encodeMidgardCekValueNode(valueNode);
    const valueRoot = hashMidgardCekValueNode(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termPreimage = encodeMidgardCekTermNode(termNode);
    const termRoot = hashMidgardCekTermNode(termNode);

    const material: MidgardCekProgramMaterialEntry[] = [
      {
        kind: "term",
        root: termRoot,
        preimage: termPreimage,
      },
      {
        kind: "value",
        root: valueRoot,
        preimage: valuePreimage,
      },
      {
        kind: "dataNode",
        root: semanticRoot,
        preimage: encodeMidgardCekDataNode(semanticNode),
      },
      ...[...typeBlob.nodes.entries(), ...rawBlob.nodes.entries()].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];

    const verified = verifyMidgardCekProgramMaterial(termRoot, material);
    expect(verified.nodeCount).toBe(BigInt(material.length));
    expect(verified.constants).toHaveLength(1);
    expect(verified.constants[0]!.typeCbor).toEqual(
      Buffer.from("9f01ff", "hex"),
    );
    expect(verified.constants[0]!.payloadCbor).toEqual(payloadBytes);

    const unrelated = programMaterialEntry(
      "term",
      encodeMidgardCekTermNode({ kind: "error" }),
    );
    const availableByRoot = new Map(
      [...material, unrelated].map((entry) => [hex(entry.root), entry]),
    );
    const fetchedRoots: string[] = [];
    const gathered = new Set<string>();
    const pending: Hash32[] = [termRoot];
    for (let cursor = 0; cursor < pending.length; cursor += 1) {
      const root = pending[cursor]!;
      const key = hex(root);
      if (gathered.has(key)) continue;
      fetchedRoots.push(key);
      const entry = availableByRoot.get(key);
      if (entry === undefined) {
        throw new Error(`test material is missing ${key}`);
      }
      gathered.add(key);
      pending.push(...midgardCekProgramMaterialDependencies(entry));
    }
    expect(gathered).toEqual(verified.reachableRoots);
    expect(fetchedRoots).toHaveLength(gathered.size);
    expect(fetchedRoots).not.toContain(hex(unrelated.root));

    const maximumChunkEntry = {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunk(Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES)),
      preimage: encodeMidgardCekBlobChunk(
        Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES),
      ),
    } as const;
    expect(maximumChunkEntry.preimage).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES,
    );
    const encodedEntry =
      encodeMidgardCekProgramMaterialEntry(maximumChunkEntry);
    expect(encodedEntry).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES,
    );
    expect(decodeMidgardCekProgramMaterialEntry(encodedEntry)).toEqual(
      maximumChunkEntry,
    );
  });

  it("authenticates constructor indices beyond JavaScript's safe-integer range", () => {
    const constructor = 1n << 80n;
    const constructorCbor = Buffer.from("c24b0100000000000000000000", "hex");
    const payloadCbor = Buffer.concat([
      Buffer.from("d86682", "hex"),
      constructorCbor,
      Buffer.from([0x80]),
    ]);
    const typeBlob = commitMidgardCekBlob(Buffer.from("9f08ff", "hex"));
    const constructorBlob = commitMidgardCekBlob(constructorCbor);
    const semanticNode = {
      kind: "constrLarge",
      constructorCborRoot: constructorBlob.root,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: 6n,
      fieldsCount: 0n,
      fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
      cborLength: midgardCekDataConstrCborLength(constructor, 0n, 0n),
      memory: 4n,
    } as const;
    const semanticRoot = hashMidgardCekDataNode(semanticNode);
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: semanticRoot,
      payloadLength: BigInt(payloadCbor.length),
      semanticRoot,
      memory: 4n,
    } as const;
    const valueRoot = hashMidgardCekValueNode(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termRoot = hashMidgardCekTermNode(termNode);
    const material: MidgardCekProgramMaterialEntry[] = [
      {
        kind: "term",
        root: termRoot,
        preimage: encodeMidgardCekTermNode(termNode),
      },
      {
        kind: "value",
        root: valueRoot,
        preimage: encodeMidgardCekValueNode(valueNode),
      },
      {
        kind: "dataNode",
        root: semanticRoot,
        preimage: encodeMidgardCekDataNode(semanticNode),
      },
      ...[...typeBlob.nodes.entries(), ...constructorBlob.nodes.entries()].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];

    const verified = verifyMidgardCekProgramMaterial(termRoot, material);
    expect(verified.constants[0]!.payloadCbor).toEqual(payloadCbor);
    expect(verified.constants[0]!.memory).toBe(4n);
  });

  it("rejects the retired split payload/semantic-root representation", () => {
    const typeBlob = commitMidgardCekBlob(Buffer.from("9f01ff", "hex"));
    const payloadValue = Buffer.alloc(65, 0x5a);
    const payloadBytes = Buffer.concat([
      Buffer.from("5f5840", "hex"),
      payloadValue.subarray(0, 64),
      Buffer.from("415aff", "hex"),
    ]);
    const payloadBlob = commitMidgardCekBlob(payloadBytes);

    // Canonical V1 has one semantic payload root. A value that retains the
    // retired split whole-payload blob root must fail before graph traversal.
    const conflictingValue = Buffer.alloc(65, 0x5b);
    const conflictingBlob = commitMidgardCekBlob(conflictingValue);
    const conflictingSemanticNode = {
      kind: "bytes",
      bytesRoot: conflictingBlob.root,
      bytesLength: 65n,
      cborLength: midgardCekDataBytesCborLength(65n),
      memory: 69n,
    } as const;
    const conflictingSemanticRoot = hashMidgardCekDataNode(
      conflictingSemanticNode,
    );
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: payloadBlob.root,
      payloadLength: BigInt(payloadBytes.length),
      semanticRoot: conflictingSemanticRoot,
      memory: 65n,
    } as const;
    const valueRoot = hashMidgardCekValueNode(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termRoot = hashMidgardCekTermNode(termNode);
    const material: MidgardCekProgramMaterialEntry[] = [
      {
        kind: "term",
        root: termRoot,
        preimage: encodeMidgardCekTermNode(termNode),
      },
      {
        kind: "value",
        root: valueRoot,
        preimage: encodeMidgardCekValueNode(valueNode),
      },
      {
        kind: "dataNode",
        root: conflictingSemanticRoot,
        preimage: encodeMidgardCekDataNode(conflictingSemanticNode),
      },
      ...[
        ...typeBlob.nodes.entries(),
        ...payloadBlob.nodes.entries(),
        ...conflictingBlob.nodes.entries(),
      ].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntry => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];

    expect(() => verifyMidgardCekProgramMaterial(termRoot, material)).toThrow(
      /payload root must equal its canonical semantic root/u,
    );
  });

  it("classifies an absent local material root", () => {
    const missingNode = { kind: "error" } as const;
    const missingRoot = hashMidgardCekTermNode(missingNode);
    let missingError: unknown;
    try {
      verifyMidgardCekProgramMaterial(missingRoot, []);
    } catch (cause) {
      missingError = cause;
    }
    expect(missingError).toBeInstanceOf(
      MidgardCekProgramMaterialMissingRootError,
    );
    expect(
      (missingError as MidgardCekProgramMaterialMissingRootError).rootHex,
    ).toBe(Buffer.from(missingRoot).toString("hex"));
  });

  it("fails closed on incomplete, duplicate, unreachable, and malformed material", () => {
    const termNode = { kind: "error" } as const;
    const term = {
      kind: "term",
      root: hashMidgardCekTermNode(termNode),
      preimage: encodeMidgardCekTermNode(termNode),
    } as const;
    const submission = encodeMidgardProofSubmission({
      transactionCbor: Buffer.from("820102", "hex"),
    });
    expect(decodeMidgardProofSubmission(submission)).toEqual({
      transactionCbor: Buffer.from("820102", "hex"),
    });
    const unsupportedSubmission = Buffer.from(submission);
    unsupportedSubmission[1] = 23;
    expect(() => decodeMidgardProofSubmission(unsupportedSubmission)).toThrow(
      /unsupported V1 submission version 23/u,
    );
    expect(() => verifyMidgardCekProgramMaterial(term.root, [])).toThrow(
      /missing root/u,
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(term.root, [term, term]),
    ).toThrow(/duplicate/u);

    const extraNode = { kind: "builtin", tag: 0n } as const;
    const extra = {
      kind: "term",
      root: hashMidgardCekTermNode(extraNode),
      preimage: encodeMidgardCekTermNode(extraNode),
    } as const;
    expect(() =>
      verifyMidgardCekProgramMaterial(term.root, [term, extra]),
    ).toThrow(/unreachable/u);

    const encoded = encodeMidgardCekProgramMaterialEntry(term);
    const tampered = Buffer.from(encoded);
    tampered[tampered.length - 1] ^= 1;
    expect(() => decodeMidgardCekProgramMaterialEntry(tampered)).toThrow(
      /root does not match/u,
    );

    const nonCanonicalBlobPreimage = Buffer.from("5800", "hex");
    const malformedBlob = {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunk(Buffer.alloc(0)),
      preimage: nonCanonicalBlobPreimage,
    } as const;
    expect(() =>
      encodeMidgardCekProgramMaterialEntry(malformedBlob),
    ).not.toThrow();
    expect(() =>
      decodeMidgardCekProgramMaterialEntry(
        encodeMidgardCekProgramMaterialEntry(malformedBlob),
      ),
    ).toThrow(/root does not match/u);
  });

  it("keeps the authenticated runtime context term out of source programs", () => {
    const preimage = encodeMidgardCekTermNode({
      kind: "contextConstant",
      value: hash(1),
    });
    const root = hashMidgardCekTermNode({
      kind: "contextConstant",
      value: hash(1),
    });
    expect(() =>
      verifyMidgardCekProgramMaterial(root, [{ kind: "term", root, preimage }]),
    ).toThrow(/runtime-only context constant/u);
  });

  it("authenticates a semantic payload beyond the retired proof-envelope limit", () => {
    const large = makeBytesConstantProgramMaterial(9_000);
    expect(large.payloadCbor.length).toBeGreaterThan(9_215);

    const verified = verifyMidgardCekProgramMaterial(
      large.summary.termRoot,
      large.material,
    );
    expect(verified.constants).toHaveLength(1);
    expect(verified.constants[0]?.valueRoot).toEqual(large.valueRoot);
    expect(verified.constants[0]?.payloadCbor).toEqual(large.payloadCbor);
  });

  it("requires canonical indefinite semantic constant type lists", () => {
    const canonical = makeNestedListConstantProgramMaterial(0);
    const verified = verifyMidgardCekProgramMaterial(
      canonical.summary.termRoot,
      canonical.material,
    );
    expect(verified.constants[0]?.typeCbor).toEqual(
      Buffer.from("9f00ff", "hex"),
    );

    const nonCanonical = makeNestedListConstantProgramMaterial(
      0,
      Buffer.from("8100", "hex"),
    );
    expect(() =>
      verifyMidgardCekProgramMaterial(
        nonCanonical.summary.termRoot,
        nonCanonical.material,
      ),
    ).toThrow(/CEK constant type.*canonical/u);
  });

  it("parses the exact bounded semantic type iteratively and rejects one byte over deterministically", () => {
    const boundaryDepth = MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES - 3;
    const boundary = makeNestedListConstantProgramMaterial(boundaryDepth);
    const verified = verifyMidgardCekProgramMaterial(
      boundary.summary.termRoot,
      boundary.material,
    );
    expect(verified.constants[0]?.typeCbor).toHaveLength(
      MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES,
    );
    expect(verified.constants[0]?.payloadCbor).toEqual(boundary.payloadCbor);

    const over = makeNestedListConstantProgramMaterial(boundaryDepth + 1);
    let rejection: unknown;
    try {
      verifyMidgardCekProgramMaterial(over.summary.termRoot, over.material);
    } catch (cause) {
      rejection = cause;
    }
    expect(rejection).toBeInstanceOf(Error);
    expect(rejection).not.toBeInstanceOf(RangeError);
    expect((rejection as Error).message).toMatch(
      /declares 65 bytes, exceeding 64/u,
    );
  });
});
