import { describe, expect, it } from "vitest";

import {
  commitMidgardCekBlobV1,
  decodeMidgardCekProgramEnvelopeV1,
  decodeMidgardCekProgramMaterialDaEntryV1,
  decodeMidgardCekProgramMaterialEntryV1,
  decodeMidgardCekProgramMaterialSidecarV1,
  decodeMidgardProofSubmissionV1,
  encodeMidgardCekBlobChunkV1,
  encodeMidgardCekProgramEnvelopeV1,
  encodeMidgardCekProgramMaterialDaValueV1,
  encodeMidgardCekProgramMaterialEntryV1,
  encodeMidgardCekProgramMaterialSidecarV1,
  encodeMidgardCekTermNodeV1,
  encodeMidgardCekValueNodeV1,
  encodeMidgardProofSubmissionV1,
  hashMidgardCekBlobChunkV1,
  hashMidgardCekBlsExpressionNodeV1,
  hashMidgardCekContinuationFrameV1,
  hashMidgardCekEnvironmentNodeV1,
  hashMidgardCekMachineStateV1,
  hashMidgardCekProgramEnvelopeV1,
  hashMidgardCekSequenceNodeV1,
  hashMidgardCekTermNodeV1,
  hashMidgardCekValueNodeV1,
  mergeMidgardCekProgramMaterialSidecarsV1,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
  MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
  MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
  MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1,
  MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1,
  type MidgardCekProgramMaterialEntryV1,
  verifyMidgardCekProgramMaterialBundleV1,
  verifyMidgardCekProgramMaterialV1,
} from "../src/cek-proof.js";
import {
  encodeMidgardCekDataNodeV1,
  hashMidgardCekDataNodeV1,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
  midgardCekDataBytesCborLengthV1,
  midgardCekDataConstrCborLengthV1,
} from "../src/cek-semantic.js";
import type { Hash32 } from "../src/codec/hash.js";

const hash = (fill: number): Buffer => Buffer.alloc(32, fill);
const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

describe("V1 CEK commitments", () => {
  it("matches the Aiken node, state, and program vectors", () => {
    const term = hashMidgardCekTermNodeV1({
      kind: "application",
      function: hash(1),
      argument: hash(2),
    });
    const value = hashMidgardCekValueNodeV1({
      kind: "lambda",
      body: hash(3),
      environment: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
    });
    const sequence = hashMidgardCekSequenceNodeV1({
      head: term,
      tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
      length: 1n,
    });
    const environment = hashMidgardCekEnvironmentNodeV1({
      value,
      tail: MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1,
      length: 1n,
    });
    const continuation = hashMidgardCekContinuationFrameV1({
      kind: "applyArgument",
      argument: hash(4),
      environment,
      tail: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
    });
    const state = hashMidgardCekMachineStateV1({
      mode: "compute",
      executionIndex: 2n,
      focusRoot: term,
      environmentRoot: environment,
      continuationRoot: continuation,
      auxiliary: 0n,
      cpu: 16_000n,
      memory: 100n,
    });
    const program = hashMidgardCekProgramEnvelopeV1({
      uplcVersion: [1n, 1n, 0n],
      termRoot: term,
      nodeCount: 3n,
      materialByteLength: 144n,
    });
    const applyValue = hashMidgardCekContinuationFrameV1({
      kind: "applyValue",
      value: hash(1),
      tail: hash(2),
    });
    const caseSelect = hashMidgardCekContinuationFrameV1({
      kind: "caseSelect",
      environment: hash(1),
      tail: hash(2),
      valuesCount: 3n,
    });
    const caseApply = hashMidgardCekContinuationFrameV1({
      kind: "caseApply",
      environment: hash(1),
      builtContinuation: hash(2),
    });
    const caseState = hashMidgardCekMachineStateV1({
      mode: "caseApply",
      executionIndex: 0n,
      focusRoot: hash(1),
      environmentRoot: hash(2),
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      auxiliary: 3n,
      cpu: 4n,
      memory: 5n,
    });
    const semanticBuiltinState = hashMidgardCekMachineStateV1({
      mode: "semanticBuiltin",
      executionIndex: 0n,
      focusRoot: hash(1),
      environmentRoot: hash(2),
      continuationRoot: MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1,
      auxiliary: 3n,
      cpu: 4n,
      memory: 5n,
    });
    const blsLeaf = hashMidgardCekBlsExpressionNodeV1({
      kind: "millerLoop",
      g1Value: hash(1),
      g2Value: hash(2),
    });
    const blsMultiply = hashMidgardCekBlsExpressionNodeV1({
      kind: "multiply",
      left: hash(1),
      right: hash(2),
    });
    const blsValue = hashMidgardCekValueNodeV1({
      kind: "blsMillerLoop",
      expressionRoot: blsLeaf,
    });

    expect(hex(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1)).toBe(
      "8ab46e13655026ca6fe253b057ff678ebf9fa088097d6fa4c62276ba140f1743",
    );
    expect(hex(MIDGARD_CEK_EMPTY_ENVIRONMENT_ROOT_V1)).toBe(
      "0b986961db44e461e897c3b03109b7f23a5270e9de71c608e518a153d57a24a7",
    );
    expect(hex(MIDGARD_CEK_EMPTY_CONTINUATION_ROOT_V1)).toBe(
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
    expect(hex(program)).toBe(
      "e9d2696eff22d8078ae7bd71c83f6058e0db4d938a016314a2cd35feaccfdefa",
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
    expect(hex(hashMidgardCekBlobChunkV1(Buffer.from("010203", "hex")))).toBe(
      "0bb4f6f24cd0080e59e98d57a13c6453e07991452b80f14ff1d9791c02db1fc9",
    );
  });

  it("fails closed on malformed hashes, oversized chunks, and unknown builtins", () => {
    expect(() =>
      hashMidgardCekTermNodeV1({
        kind: "application",
        function: Buffer.alloc(31),
        argument: hash(2),
      }),
    ).toThrow(/must be 32 bytes/u);
    expect(() =>
      hashMidgardCekBlobChunkV1(Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES + 1)),
    ).toThrow(/at most 4095 bytes/u);
    expect(() =>
      hashMidgardCekTermNodeV1({ kind: "builtin", tag: 87n }),
    ).toThrow(/between 0 and 86/u);
    expect(() =>
      hashMidgardCekSequenceNodeV1({
        head: hash(1),
        tail: MIDGARD_CEK_EMPTY_SEQUENCE_ROOT_V1,
        length: 0n,
      }),
    ).toThrow(/must be positive/u);
  });

  it("decodes only the canonical bounded V1 program envelope", () => {
    const envelope = encodeMidgardCekProgramEnvelopeV1({
      uplcVersion: [1n, 1n, 0n],
      termRoot: hash(1),
      nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1,
      materialByteLength: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1,
    });
    expect(envelope).toHaveLength(MIDGARD_CEK_MAX_PROGRAM_ENVELOPE_BYTES_V1);
    expect(decodeMidgardCekProgramEnvelopeV1(envelope)).toEqual({
      uplcVersion: [1n, 1n, 0n],
      termRoot: hash(1),
      nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1,
      materialByteLength: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1,
    });

    const noncanonical = Buffer.concat([
      Buffer.from("8501830101005820", "hex"),
      hash(1),
      Buffer.from("18031890", "hex"),
    ]);
    expect(() => decodeMidgardCekProgramEnvelopeV1(noncanonical)).toThrow(
      /Non-minimal CBOR/u,
    );
    const unsupportedEnvelope = Buffer.concat([
      Buffer.from("8517830101005820", "hex"),
      hash(1),
      Buffer.from("031890", "hex"),
    ]);
    expect(() =>
      decodeMidgardCekProgramEnvelopeV1(unsupportedEnvelope),
    ).toThrow(/unsupported CEK program envelope version 23/u);
    expect(() =>
      decodeMidgardCekProgramEnvelopeV1(
        encodeMidgardCekProgramEnvelopeV1({
          uplcVersion: [1n, 0n, 0n],
          termRoot: hash(1),
          nodeCount: 3n,
          materialByteLength: 144n,
        }),
      ),
    ).toThrow(/only UPLC 1.1.0/u);
    expect(() =>
      decodeMidgardCekProgramEnvelopeV1(
        encodeMidgardCekProgramEnvelopeV1({
          uplcVersion: [1n, 1n, 0n],
          termRoot: hash(1),
          nodeCount: MIDGARD_CEK_MAX_PROGRAM_NODE_COUNT_V1 + 1n,
          materialByteLength: 144n,
        }),
      ),
    ).toThrow(/node count/u);
    expect(() =>
      decodeMidgardCekProgramEnvelopeV1(
        encodeMidgardCekProgramEnvelopeV1({
          uplcVersion: [1n, 1n, 0n],
          termRoot: hash(1),
          nodeCount: 3n,
          materialByteLength: MIDGARD_CEK_MAX_PROGRAM_MATERIAL_BYTES_V1 + 1n,
        }),
      ),
    ).toThrow(/material length/u);
  });

  it("authenticates and traverses exact content-addressed program material", () => {
    const typeBlob = commitMidgardCekBlobV1(Buffer.from("9f01ff", "hex"));
    const rawValue = Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES + 1, 0x5a);
    const payloadBytes = Buffer.concat([
      Buffer.from([0x5f]),
      ...Array.from({ length: Math.ceil(rawValue.length / 64) }, (_, index) => {
        const chunk = rawValue.subarray(index * 64, (index + 1) * 64);
        return Buffer.concat([Buffer.from([0x58, chunk.length]), chunk]);
      }),
      Buffer.from([0xff]),
    ]);
    const rawBlob = commitMidgardCekBlobV1(rawValue);
    const semanticNode = {
      kind: "bytes",
      bytesRoot: rawBlob.root,
      bytesLength: BigInt(rawValue.length),
      cborLength: midgardCekDataBytesCborLengthV1(BigInt(rawValue.length)),
      memory: 4n + BigInt(rawValue.length),
    } as const;
    const semanticRoot = hashMidgardCekDataNodeV1(semanticNode);
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: semanticRoot,
      payloadLength: BigInt(payloadBytes.length),
      semanticRoot,
      memory: BigInt(rawValue.length),
    } as const;
    const valuePreimage = encodeMidgardCekValueNodeV1(valueNode);
    const valueRoot = hashMidgardCekValueNodeV1(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termPreimage = encodeMidgardCekTermNodeV1(termNode);
    const termRoot = hashMidgardCekTermNodeV1(termNode);

    const material: MidgardCekProgramMaterialEntryV1[] = [
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
        preimage: encodeMidgardCekDataNodeV1(semanticNode),
      },
      ...[...typeBlob.nodes.entries(), ...rawBlob.nodes.entries()].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntryV1 => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    };

    const verified = verifyMidgardCekProgramMaterialV1(envelope, material);
    expect(verified.nodeCount).toBe(BigInt(material.length));
    expect(verified.constants).toHaveLength(1);
    expect(verified.constants[0]!.typeCbor).toEqual(
      Buffer.from("9f01ff", "hex"),
    );
    expect(verified.constants[0]!.payloadCbor).toEqual(payloadBytes);
    expect(
      verifyMidgardCekProgramMaterialBundleV1([envelope], material),
    ).toHaveLength(1);

    const maximumChunkEntry = {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunkV1(
        Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES),
      ),
      preimage: encodeMidgardCekBlobChunkV1(
        Buffer.alloc(MIDGARD_CEK_BLOB_CHUNK_BYTES),
      ),
    } as const;
    expect(maximumChunkEntry.preimage).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_PREIMAGE_BYTES_V1,
    );
    const encodedEntry =
      encodeMidgardCekProgramMaterialEntryV1(maximumChunkEntry);
    expect(encodedEntry).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_ENTRY_BYTES_V1,
    );
    expect(decodeMidgardCekProgramMaterialEntryV1(encodedEntry)).toEqual(
      maximumChunkEntry,
    );
    const daValue = encodeMidgardCekProgramMaterialDaValueV1(maximumChunkEntry);
    expect(daValue).toHaveLength(
      MIDGARD_CEK_MAX_PROGRAM_MATERIAL_DA_VALUE_BYTES_V1,
    );
    expect(
      decodeMidgardCekProgramMaterialDaEntryV1(maximumChunkEntry.root, daValue),
    ).toEqual(maximumChunkEntry);
    const unsupportedDaValue = Buffer.from(daValue);
    unsupportedDaValue[1] = 23;
    expect(() =>
      decodeMidgardCekProgramMaterialDaEntryV1(
        maximumChunkEntry.root,
        unsupportedDaValue,
      ),
    ).toThrow(/unsupported CEK program material DA value version 23/u);
  });

  it("authenticates constructor indices beyond JavaScript's safe-integer range", () => {
    const constructor = 1n << 80n;
    const constructorCbor = Buffer.from("c24b0100000000000000000000", "hex");
    const payloadCbor = Buffer.concat([
      Buffer.from("d86682", "hex"),
      constructorCbor,
      Buffer.from([0x80]),
    ]);
    const typeBlob = commitMidgardCekBlobV1(Buffer.from("9f08ff", "hex"));
    const constructorBlob = commitMidgardCekBlobV1(constructorCbor);
    const semanticNode = {
      kind: "constrLarge",
      constructorCborRoot: constructorBlob.root,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: 15n,
      fieldsCount: 0n,
      fieldsRoot: MIDGARD_CEK_EMPTY_DATA_LIST_ROOT_V1,
      cborLength: midgardCekDataConstrCborLengthV1(constructor, 0n, 0n),
      memory: 4n,
    } as const;
    const semanticRoot = hashMidgardCekDataNodeV1(semanticNode);
    const valueNode = {
      kind: "constant",
      typeRoot: typeBlob.root,
      payloadRoot: semanticRoot,
      payloadLength: BigInt(payloadCbor.length),
      semanticRoot,
      memory: 4n,
    } as const;
    const valueRoot = hashMidgardCekValueNodeV1(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termRoot = hashMidgardCekTermNodeV1(termNode);
    const material: MidgardCekProgramMaterialEntryV1[] = [
      {
        kind: "term",
        root: termRoot,
        preimage: encodeMidgardCekTermNodeV1(termNode),
      },
      {
        kind: "value",
        root: valueRoot,
        preimage: encodeMidgardCekValueNodeV1(valueNode),
      },
      {
        kind: "dataNode",
        root: semanticRoot,
        preimage: encodeMidgardCekDataNodeV1(semanticNode),
      },
      ...[...typeBlob.nodes.entries(), ...constructorBlob.nodes.entries()].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntryV1 => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    };

    const verified = verifyMidgardCekProgramMaterialV1(envelope, material);
    expect(verified.constants[0]!.payloadCbor).toEqual(payloadCbor);
    expect(verified.constants[0]!.memory).toBe(4n);
  });

  it("rejects the retired split payload/semantic-root representation", () => {
    const typeBlob = commitMidgardCekBlobV1(Buffer.from("9f01ff", "hex"));
    const payloadValue = Buffer.alloc(65, 0x5a);
    const payloadBytes = Buffer.concat([
      Buffer.from("5f5840", "hex"),
      payloadValue.subarray(0, 64),
      Buffer.from("415aff", "hex"),
    ]);
    const payloadBlob = commitMidgardCekBlobV1(payloadBytes);

    // Canonical V1 has one semantic payload root. A value that retains the
    // retired split whole-payload blob root must fail before graph traversal.
    const conflictingValue = Buffer.alloc(65, 0x5b);
    const conflictingBlob = commitMidgardCekBlobV1(conflictingValue);
    const conflictingSemanticNode = {
      kind: "bytes",
      bytesRoot: conflictingBlob.root,
      bytesLength: 65n,
      cborLength: midgardCekDataBytesCborLengthV1(65n),
      memory: 69n,
    } as const;
    const conflictingSemanticRoot = hashMidgardCekDataNodeV1(
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
    const valueRoot = hashMidgardCekValueNodeV1(valueNode);
    const termNode = { kind: "constant", value: valueRoot } as const;
    const termRoot = hashMidgardCekTermNodeV1(termNode);
    const material: MidgardCekProgramMaterialEntryV1[] = [
      {
        kind: "term",
        root: termRoot,
        preimage: encodeMidgardCekTermNodeV1(termNode),
      },
      {
        kind: "value",
        root: valueRoot,
        preimage: encodeMidgardCekValueNodeV1(valueNode),
      },
      {
        kind: "dataNode",
        root: conflictingSemanticRoot,
        preimage: encodeMidgardCekDataNodeV1(conflictingSemanticNode),
      },
      ...[
        ...typeBlob.nodes.entries(),
        ...payloadBlob.nodes.entries(),
        ...conflictingBlob.nodes.entries(),
      ].map(
        ([rootHex, node]): MidgardCekProgramMaterialEntryV1 => ({
          kind: node.kind === "chunk" ? "blobChunk" : "blobBranch",
          root: Buffer.from(rootHex, "hex") as Hash32,
          preimage: node.preimage,
        }),
      ),
    ];
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot,
      nodeCount: BigInt(material.length),
      materialByteLength: material.reduce(
        (total, entry) => total + BigInt(entry.preimage.length),
        0n,
      ),
    };

    expect(() => verifyMidgardCekProgramMaterialV1(envelope, material)).toThrow(
      /payload root must equal its canonical semantic root/u,
    );
  });

  it("fails closed on incomplete, duplicate, unreachable, and malformed material", () => {
    const termNode = { kind: "error" } as const;
    const term = {
      kind: "term",
      root: hashMidgardCekTermNodeV1(termNode),
      preimage: encodeMidgardCekTermNodeV1(termNode),
    } as const;
    const envelope = {
      uplcVersion: [1n, 1n, 0n] as const,
      termRoot: term.root,
      nodeCount: 1n,
      materialByteLength: BigInt(term.preimage.length),
    };
    const submission = encodeMidgardProofSubmissionV1({
      transactionCbor: Buffer.from("820102", "hex"),
      programMaterial: [term],
    });
    expect(decodeMidgardProofSubmissionV1(submission)).toEqual({
      transactionCbor: Buffer.from("820102", "hex"),
      programMaterial: [term],
    });
    const unsupportedSubmission = Buffer.from(submission);
    unsupportedSubmission[1] = 23;
    expect(() => decodeMidgardProofSubmissionV1(unsupportedSubmission)).toThrow(
      /unsupported V1 submission version 23/u,
    );
    const sidecar = encodeMidgardCekProgramMaterialSidecarV1([term]);
    expect(decodeMidgardCekProgramMaterialSidecarV1(sidecar)).toEqual([term]);
    expect(encodeMidgardCekProgramMaterialSidecarV1([]).toString("hex")).toBe(
      "820180",
    );
    expect(() =>
      decodeMidgardCekProgramMaterialSidecarV1(Buffer.from("821780", "hex")),
    ).toThrow(/unsupported V1 program material sidecar version 23/u);
    expect(() =>
      decodeMidgardCekProgramMaterialSidecarV1(
        Buffer.concat([sidecar, Buffer.from([0])]),
      ),
    ).toThrow(/trailing bytes/u);
    expect(
      mergeMidgardCekProgramMaterialSidecarsV1([sidecar, sidecar]),
    ).toEqual([term]);
    expect(() => verifyMidgardCekProgramMaterialV1(envelope, [])).toThrow(
      /missing root/u,
    );
    expect(() =>
      verifyMidgardCekProgramMaterialV1(envelope, [term, term]),
    ).toThrow(/duplicate/u);
    expect(() =>
      encodeMidgardProofSubmissionV1({
        transactionCbor: Buffer.from("820102", "hex"),
        programMaterial: [term, term],
      }),
    ).toThrow(/duplicate/u);

    const extraNode = { kind: "builtin", tag: 0n } as const;
    const extra = {
      kind: "term",
      root: hashMidgardCekTermNodeV1(extraNode),
      preimage: encodeMidgardCekTermNodeV1(extraNode),
    } as const;
    expect(() =>
      verifyMidgardCekProgramMaterialV1(envelope, [term, extra]),
    ).toThrow(/unreachable/u);
    expect(() => verifyMidgardCekProgramMaterialBundleV1([], [term])).toThrow(
      /without a program envelope/u,
    );
    expect(() =>
      verifyMidgardCekProgramMaterialV1({ ...envelope, nodeCount: 2n }, [term]),
    ).toThrow(/envelope declares 2/u);

    const encoded = encodeMidgardCekProgramMaterialEntryV1(term);
    const tampered = Buffer.from(encoded);
    tampered[tampered.length - 1] ^= 1;
    expect(() => decodeMidgardCekProgramMaterialEntryV1(tampered)).toThrow(
      /root does not match/u,
    );

    const nonCanonicalBlobPreimage = Buffer.from("5800", "hex");
    const malformedBlob = {
      kind: "blobChunk",
      root: hashMidgardCekBlobChunkV1(Buffer.alloc(0)),
      preimage: nonCanonicalBlobPreimage,
    } as const;
    expect(() =>
      encodeMidgardCekProgramMaterialEntryV1(malformedBlob),
    ).not.toThrow();
    expect(() =>
      decodeMidgardCekProgramMaterialEntryV1(
        encodeMidgardCekProgramMaterialEntryV1(malformedBlob),
      ),
    ).toThrow(/root does not match/u);
  });

  it("keeps the authenticated runtime context term out of source programs", () => {
    const preimage = encodeMidgardCekTermNodeV1({
      kind: "contextConstant",
      value: hash(1),
    });
    const root = hashMidgardCekTermNodeV1({
      kind: "contextConstant",
      value: hash(1),
    });
    expect(() =>
      verifyMidgardCekProgramMaterialV1(
        {
          uplcVersion: [1n, 1n, 0n],
          termRoot: root,
          nodeCount: 1n,
          materialByteLength: BigInt(preimage.length),
        },
        [{ kind: "term", root, preimage }],
      ),
    ).toThrow(/runtime-only context constant/u);
  });
});
