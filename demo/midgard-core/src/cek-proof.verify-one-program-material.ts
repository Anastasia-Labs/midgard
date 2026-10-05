import {
  commitSemanticData,
  decodeSemanticConstantType,
} from "./cek-proof.commit-semantic-data.js";
import { decodeMidgardCekProgramBlobPreimage } from "./cek-proof.decode-midgard-cek-program-blob-preimage.js";
import {
  type MidgardCekProgramMaterialEntry,
  type MidgardCekProgramMaterialKind,
} from "./cek-proof.decode-midgard-cek-program-envelope.js";
import {
  type MidgardCekDecodedProgramBlob,
  type MidgardCekDecodedProgramValue,
} from "./cek-proof.decode-midgard-cek-program-material-da-entry.js";
import {
  decodeMidgardCekProgramSequencePreimage,
  decodeMidgardCekProgramTermPreimage,
  decodeMidgardCekProgramValuePreimage,
} from "./cek-proof.decode-midgard-cek-program-term-preimage.js";
import { type MidgardCekProgramEnvelope } from "./cek-proof.encode-midgard-cek-continuation-frame.js";
import {
  exactHash,
  MIDGARD_CEK_BLOB_CHUNK_BYTES,
  MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES,
  MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES,
} from "./cek-proof.encode-midgard-cek-term-node.js";
import { MIDGARD_CEK_EMPTY_SEQUENCE_ROOT } from "./cek-proof.encode-midgard-cek-value-node.js";
import { encodeSemanticData } from "./cek-proof.encode-semantic-data.js";
import {
  greatestPowerOfTwoBelow,
  type MidgardCekProgramConstantMaterial,
  MidgardCekProgramMaterialMissingRootError,
  type MidgardCekProgramMaterialVerification,
  type NormalizedProgramMaterial,
  type ProgramMaterialBundleCache,
  type ProgramMaterialTask,
} from "./cek-proof.program-material-task.js";
import { makeSemanticDataReconstructor } from "./cek-proof.reconstruct-semantic-data.js";
import {
  semanticConstantMemory,
  semanticConstantPayloadMatchesType,
} from "./cek-proof.semantic-constant-payload-matches-type.js";
import {
  decodeMidgardCekDataListNode,
  decodeMidgardCekDataNode,
  decodeMidgardCekDataPairNode,
  MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
  MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
  midgardCekDataBytesCborLength,
  midgardCekDataConstrCborLength,
  midgardCekDataListCborLength,
  type MidgardCekDataListNode,
  midgardCekDataMapCborLength,
  type MidgardCekDataNode,
  type MidgardCekDataPairNode,
} from "./cek-semantic.js";
import { type Hash32 } from "./codec/hash.js";

const payloadLengthMismatch = (valueKey: string): string =>
  `CEK constant value ${valueKey} payload length does not match its semantic tree`;

export const verifyOneProgramMaterial = (
  envelope: MidgardCekProgramEnvelope,
  material: NormalizedProgramMaterial,
  cache: ProgramMaterialBundleCache,
  options: {
    readonly includeConstants: boolean;
    readonly onBlobMaterialized?: (rootHex: string, byteLength: bigint) => void;
    readonly onConstantMaterialized?: (
      valueRootHex: string,
      payloadByteLength: bigint,
    ) => void;
  },
): MidgardCekProgramMaterialVerification => {
  const reachable = new Set<string>();
  const dependencies = new Map<string, readonly string[]>();
  const decodedBlobs = new Map<string, MidgardCekDecodedProgramBlob>();
  const decodedValues = new Map<string, MidgardCekDecodedProgramValue>();
  const decodedDataNodes = new Map<string, MidgardCekDataNode>();
  const decodedDataLists = new Map<string, MidgardCekDataListNode>();
  const decodedDataPairs = new Map<string, MidgardCekDataPairNode>();
  const blobLengthExpectations = new Map<string, Set<bigint>>();
  const blobMaximumLengthExpectations = new Map<string, Set<bigint>>();
  const sequenceLengthExpectations = new Map<string, Set<bigint>>();
  const dataListLengthExpectations = new Map<string, Set<bigint>>();
  const dataPairLengthExpectations = new Map<string, Set<bigint>>();
  const tasks: ProgramMaterialTask[] = [
    {
      kind: "term",
      root: exactHash(envelope.termRoot, "cek_program.root") as Hash32,
    },
  ];

  const rootKey = (root: Uint8Array): string =>
    Buffer.from(root).toString("hex");
  const addDependency = (parent: string, child: Uint8Array): void => {
    const childKey = rootKey(child);
    const prior = dependencies.get(parent) ?? [];
    dependencies.set(parent, [...prior, childKey]);
  };
  const expectEntry = (
    task: ProgramMaterialTask,
  ): {
    readonly key: string;
    readonly entry: MidgardCekProgramMaterialEntry;
  } => {
    const key = rootKey(task.root);
    const entry = material.get(key);
    if (entry === undefined) {
      throw new MidgardCekProgramMaterialMissingRootError(task.root);
    }
    const expectedKinds =
      task.kind === "blob"
        ? (["blobChunk", "blobBranch"] as const)
        : ([task.kind] as const);
    if (
      !(expectedKinds as readonly MidgardCekProgramMaterialKind[]).includes(
        entry.kind,
      )
    ) {
      throw new Error(
        `CEK program material root ${key} has kind ${entry.kind}, expected ${expectedKinds.join(" or ")}`,
      );
    }
    return { key, entry };
  };
  const noteExpectation = (
    expectations: Map<string, Set<bigint>>,
    key: string,
    value: bigint | undefined,
  ): void => {
    if (value === undefined) return;
    const values = expectations.get(key) ?? new Set<bigint>();
    values.add(value);
    expectations.set(key, values);
  };

  for (let cursor = 0; cursor < tasks.length; cursor += 1) {
    const task = tasks[cursor]!;
    if (
      task.kind === "sequence" &&
      task.length === 0n &&
      Buffer.from(task.root).equals(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
    ) {
      continue;
    }
    if (
      task.kind === "dataList" &&
      task.length === 0n &&
      Buffer.from(task.root).equals(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)
    ) {
      continue;
    }
    if (
      task.kind === "dataPair" &&
      task.length === 0n &&
      Buffer.from(task.root).equals(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)
    ) {
      continue;
    }
    const { key, entry } = expectEntry(task);
    if (task.kind === "sequence") {
      noteExpectation(sequenceLengthExpectations, key, task.length);
    } else if (task.kind === "blob") {
      noteExpectation(blobLengthExpectations, key, task.byteLength);
      noteExpectation(blobMaximumLengthExpectations, key, task.maxByteLength);
    } else if (task.kind === "dataList") {
      noteExpectation(dataListLengthExpectations, key, task.length);
    } else if (task.kind === "dataPair") {
      noteExpectation(dataPairLengthExpectations, key, task.length);
    }
    if (reachable.has(key)) {
      continue;
    }
    reachable.add(key);
    dependencies.set(key, []);

    if (task.kind === "term") {
      const term = decodeMidgardCekProgramTermPreimage(entry.preimage);
      switch (term.kind) {
        case "variable":
        case "error":
        case "builtin":
          break;
        case "unaryTerm":
          addDependency(key, term.child);
          tasks.push({ kind: "term", root: term.child });
          break;
        case "application":
          addDependency(key, term.function);
          addDependency(key, term.argument);
          tasks.push(
            { kind: "term", root: term.function },
            { kind: "term", root: term.argument },
          );
          break;
        case "constant":
          addDependency(key, term.value);
          tasks.push({ kind: "value", root: term.value });
          break;
        case "contextConstant":
          throw new Error(
            "CEK source-program material contains a runtime-only context constant",
          );
        case "constr":
          if (
            term.count === 0n &&
            !Buffer.from(term.sequence).equals(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
          ) {
            throw new Error(
              "empty CEK constr sequence must use the canonical empty root",
            );
          }
          if (term.count > 0n) {
            addDependency(key, term.sequence);
            tasks.push({
              kind: "sequence",
              root: term.sequence,
              length: term.count,
            });
          }
          break;
        case "case":
          addDependency(key, term.scrutinee);
          tasks.push({ kind: "term", root: term.scrutinee });
          if (
            term.count === 0n &&
            !Buffer.from(term.sequence).equals(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
          ) {
            throw new Error(
              "empty CEK case sequence must use the canonical empty root",
            );
          }
          if (term.count > 0n) {
            addDependency(key, term.sequence);
            tasks.push({
              kind: "sequence",
              root: term.sequence,
              length: term.count,
            });
          }
          break;
      }
      continue;
    }

    if (task.kind === "value") {
      const value = decodeMidgardCekProgramValuePreimage(entry.preimage);
      decodedValues.set(key, value);
      if (!Buffer.from(value.payloadRoot).equals(value.semanticRoot)) {
        throw new Error(
          "CEK constant payload root must equal its canonical semantic root",
        );
      }
      if (
        value.payloadLength >
        BigInt(MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES)
      ) {
        throw new Error(
          `CEK source constant payload exceeds the ${MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES.toString()}-byte L1 proof envelope`,
        );
      }
      addDependency(key, value.typeRoot);
      addDependency(key, value.semanticRoot);
      tasks.push(
        {
          kind: "blob",
          root: value.typeRoot,
          maxByteLength: BigInt(MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES),
        },
        { kind: "dataNode", root: value.semanticRoot },
      );
      continue;
    }

    if (task.kind === "sequence") {
      const sequence = decodeMidgardCekProgramSequencePreimage(entry.preimage);
      if (sequence.length !== task.length) {
        throw new Error(
          `CEK sequence root ${key} declares ${sequence.length.toString()} items, expected ${task.length.toString()}`,
        );
      }
      addDependency(key, sequence.head);
      tasks.push({ kind: "term", root: sequence.head });
      if (sequence.length === 1n) {
        if (
          !Buffer.from(sequence.tail).equals(MIDGARD_CEK_EMPTY_SEQUENCE_ROOT)
        ) {
          throw new Error(
            "one-item CEK sequence must end at the canonical empty root",
          );
        }
      } else {
        addDependency(key, sequence.tail);
        tasks.push({
          kind: "sequence",
          root: sequence.tail,
          length: sequence.length - 1n,
        });
      }
      continue;
    }

    if (task.kind === "dataNode") {
      const node = decodeMidgardCekDataNode(entry.preimage);
      decodedDataNodes.set(key, node);
      if (node.kind === "constrSmall" || node.kind === "constrLarge") {
        if (node.fieldsCount === 0n) {
          if (
            !Buffer.from(node.fieldsRoot).equals(
              MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            )
          ) {
            throw new Error(
              "empty CEK Data constructor must use the canonical fields root",
            );
          }
        } else {
          addDependency(key, node.fieldsRoot);
          tasks.push({
            kind: "dataList",
            root: node.fieldsRoot as Hash32,
            length: node.fieldsCount,
          });
        }
        if (node.kind === "constrLarge") {
          addDependency(key, node.constructorCborRoot);
          tasks.push({
            kind: "blob",
            root: node.constructorCborRoot as Hash32,
            byteLength: node.constructorCborLength,
          });
        }
      } else if (node.kind === "map") {
        if (node.entriesCount === 0n) {
          if (
            !Buffer.from(node.entriesRoot).equals(
              MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT,
            )
          ) {
            throw new Error(
              "empty CEK Data map must use the canonical entries root",
            );
          }
        } else {
          addDependency(key, node.entriesRoot);
          tasks.push({
            kind: "dataPair",
            root: node.entriesRoot as Hash32,
            length: node.entriesCount,
          });
        }
      } else if (node.kind === "list") {
        if (node.itemsCount === 0n) {
          if (
            !Buffer.from(node.itemsRoot).equals(
              MIDGARD_CEK_EMPTY_DATA_LIST_ROOT,
            )
          ) {
            throw new Error(
              "empty CEK Data list must use the canonical items root",
            );
          }
        } else {
          addDependency(key, node.itemsRoot);
          tasks.push({
            kind: "dataList",
            root: node.itemsRoot as Hash32,
            length: node.itemsCount,
          });
        }
      } else if (node.kind === "integer") {
        addDependency(key, node.cborRoot);
        tasks.push({
          kind: "blob",
          root: node.cborRoot as Hash32,
          byteLength: node.cborLength,
        });
      } else {
        addDependency(key, node.bytesRoot);
        tasks.push({
          kind: "blob",
          root: node.bytesRoot as Hash32,
          byteLength: node.bytesLength,
        });
      }
      continue;
    }

    if (task.kind === "dataList") {
      const node = decodeMidgardCekDataListNode(entry.preimage);
      if (node.length !== task.length) {
        throw new Error(
          `CEK Data list root ${key} declares ${node.length.toString()} items, expected ${task.length.toString()}`,
        );
      }
      decodedDataLists.set(key, node);
      addDependency(key, node.head);
      tasks.push({ kind: "dataNode", root: node.head as Hash32 });
      if (node.length === 1n) {
        if (!Buffer.from(node.tail).equals(MIDGARD_CEK_EMPTY_DATA_LIST_ROOT)) {
          throw new Error(
            "one-item CEK Data list must end at the canonical empty root",
          );
        }
      } else {
        addDependency(key, node.tail);
        tasks.push({
          kind: "dataList",
          root: node.tail as Hash32,
          length: node.length - 1n,
        });
      }
      continue;
    }

    if (task.kind === "dataPair") {
      const node = decodeMidgardCekDataPairNode(entry.preimage);
      if (node.length !== task.length) {
        throw new Error(
          `CEK Data pair root ${key} declares ${node.length.toString()} items, expected ${task.length.toString()}`,
        );
      }
      decodedDataPairs.set(key, node);
      addDependency(key, node.key);
      addDependency(key, node.value);
      tasks.push(
        { kind: "dataNode", root: node.key as Hash32 },
        { kind: "dataNode", root: node.value as Hash32 },
      );
      if (node.length === 1n) {
        if (!Buffer.from(node.tail).equals(MIDGARD_CEK_EMPTY_DATA_PAIR_ROOT)) {
          throw new Error(
            "one-item CEK Data pair list must end at the canonical empty root",
          );
        }
      } else {
        addDependency(key, node.tail);
        tasks.push({
          kind: "dataPair",
          root: node.tail as Hash32,
          length: node.length - 1n,
        });
      }
      continue;
    }

    if (entry.kind !== "blobChunk" && entry.kind !== "blobBranch") {
      throw new Error("CEK blob task resolved to non-blob material");
    }
    const blob = decodeMidgardCekProgramBlobPreimage(
      entry.kind,
      entry.preimage,
    );
    const declaredBlobByteLength =
      blob.kind === "chunk" ? BigInt(blob.bytes.length) : blob.byteLength;
    if (
      task.maxByteLength !== undefined &&
      declaredBlobByteLength > task.maxByteLength
    ) {
      throw new Error(
        `CEK blob root ${key} declares ${declaredBlobByteLength.toString()} bytes, exceeding ${task.maxByteLength.toString()}`,
      );
    }
    decodedBlobs.set(key, blob);
    if (blob.kind === "branch") {
      addDependency(key, blob.left);
      addDependency(key, blob.right);
      tasks.push(
        { kind: "blob", root: blob.left },
        { kind: "blob", root: blob.right },
      );
    }
  }

  for (const [key, expected] of sequenceLengthExpectations) {
    const entry = material.get(key)!;
    const actual = decodeMidgardCekProgramSequencePreimage(
      entry.preimage,
    ).length;
    for (const length of expected) {
      if (actual !== length) {
        throw new Error(
          `CEK sequence root ${key} has inconsistent length expectations`,
        );
      }
    }
  }
  for (const [key, expected] of dataListLengthExpectations) {
    const actual = decodedDataLists.get(key)?.length;
    if (actual === undefined) {
      throw new Error(`CEK Data list root ${key} was not decoded`);
    }
    for (const length of expected) {
      if (actual !== length) {
        throw new Error(
          `CEK Data list root ${key} has inconsistent length expectations`,
        );
      }
    }
  }
  for (const [key, expected] of dataPairLengthExpectations) {
    const actual = decodedDataPairs.get(key)?.length;
    if (actual === undefined) {
      throw new Error(`CEK Data pair root ${key} was not decoded`);
    }
    for (const length of expected) {
      if (actual !== length) {
        throw new Error(
          `CEK Data pair root ${key} has inconsistent length expectations`,
        );
      }
    }
  }

  const colors = new Map<string, 1 | 2>();
  const postorder: string[] = [];
  for (const start of reachable) {
    if (colors.get(start) === 2) continue;
    const stack: Array<{ readonly key: string; readonly exit: boolean }> = [
      { key: start, exit: false },
    ];
    while (stack.length > 0) {
      const current = stack.pop()!;
      const color = colors.get(current.key);
      if (current.exit) {
        colors.set(current.key, 2);
        postorder.push(current.key);
        continue;
      }
      if (color === 2) continue;
      if (color === 1) {
        throw new Error("CEK program material graph contains a cycle");
      }
      colors.set(current.key, 1);
      stack.push({ key: current.key, exit: true });
      const children = dependencies.get(current.key) ?? [];
      for (let index = children.length - 1; index >= 0; index -= 1) {
        const child = children[index]!;
        if (colors.get(child) === 1) {
          throw new Error("CEK program material graph contains a cycle");
        }
        if (colors.get(child) !== 2) {
          stack.push({ key: child, exit: false });
        }
      }
    }
  }

  type BlobShape = {
    readonly byteLength: bigint;
    readonly leafCount: bigint;
    readonly lastLeafLength: number;
  };
  const blobShapes = new Map<string, BlobShape>();
  for (const key of postorder) {
    const blob = decodedBlobs.get(key);
    if (blob === undefined) continue;
    if (blob.kind === "chunk") {
      blobShapes.set(key, {
        byteLength: BigInt(blob.bytes.length),
        leafCount: 1n,
        lastLeafLength: blob.bytes.length,
      });
      continue;
    }
    const left = blobShapes.get(rootKey(blob.left));
    const right = blobShapes.get(rootKey(blob.right));
    if (left === undefined || right === undefined) {
      throw new Error("CEK blob branch child is not a canonical blob node");
    }
    if (
      left.byteLength === 0n ||
      right.byteLength === 0n ||
      left.lastLeafLength !== MIDGARD_CEK_BLOB_CHUNK_BYTES
    ) {
      throw new Error(
        "CEK blob branch must contain full non-final chunks and no empty child",
      );
    }
    const leafCount = left.leafCount + right.leafCount;
    if (left.leafCount !== greatestPowerOfTwoBelow(leafCount)) {
      throw new Error("CEK blob branch is not canonically left-balanced");
    }
    const byteLength = left.byteLength + right.byteLength;
    if (byteLength !== blob.byteLength) {
      throw new Error(
        "CEK blob branch byte length does not match its children",
      );
    }
    blobShapes.set(key, {
      byteLength,
      leafCount,
      lastLeafLength: right.lastLeafLength,
    });
  }

  for (const [key, expectedLengths] of blobLengthExpectations) {
    const shape = blobShapes.get(key);
    if (shape === undefined) {
      throw new Error(`CEK blob root ${key} was not reconstructed`);
    }
    for (const expected of expectedLengths) {
      if (shape.byteLength !== expected) {
        throw new Error(
          `CEK blob root ${key} has ${shape.byteLength.toString()} bytes, expected ${expected.toString()}`,
        );
      }
    }
  }
  for (const [key, maximumLengths] of blobMaximumLengthExpectations) {
    const shape = blobShapes.get(key);
    if (shape === undefined) {
      throw new Error(`CEK blob root ${key} was not reconstructed`);
    }
    for (const maximum of maximumLengths) {
      if (shape.byteLength > maximum) {
        throw new Error(
          `CEK blob root ${key} has ${shape.byteLength.toString()} bytes, exceeding ${maximum.toString()}`,
        );
      }
    }
  }

  const materializeBlob = (
    root: Uint8Array,
    maximumByteLength: bigint,
    fieldName: string,
  ): Buffer => {
    const key = rootKey(root);
    const shape = blobShapes.get(key);
    if (shape === undefined) {
      throw new Error(`${fieldName} blob is missing`);
    }
    if (shape.byteLength > maximumByteLength) {
      throw new Error(
        `${fieldName} blob has ${shape.byteLength.toString()} bytes, exceeding ${maximumByteLength.toString()}`,
      );
    }
    const cached = cache.materializedBlobs.get(key);
    if (cached !== undefined) return cached;

    const leaves: Buffer[] = [];
    const stack = [key];
    while (stack.length > 0) {
      const currentKey = stack.pop()!;
      const blob = decodedBlobs.get(currentKey);
      if (blob === undefined) {
        throw new Error(`${fieldName} blob has an incomplete branch`);
      }
      if (blob.kind === "chunk") {
        leaves.push(blob.bytes);
      } else {
        stack.push(rootKey(blob.right), rootKey(blob.left));
      }
    }
    const materialized = Buffer.concat(leaves, Number(shape.byteLength));
    cache.materializedBlobs.set(key, materialized);
    try {
      options.onBlobMaterialized?.(key, shape.byteLength);
    } catch {
      // Allocation observability must not change verification semantics.
    }
    return materialized;
  };

  for (const key of postorder) {
    const listNode = decodedDataLists.get(key);
    if (listNode !== undefined) {
      const head = decodedDataNodes.get(rootKey(listNode.head));
      if (head === undefined) {
        throw new Error("CEK Data list head is not a Data node");
      }
      const tail =
        listNode.length === 1n
          ? null
          : decodedDataLists.get(rootKey(listNode.tail));
      if (listNode.length > 1n && tail === undefined) {
        throw new Error("CEK Data list tail is not a list node");
      }
      const tailPayload = tail?.payloadCborLength ?? 0n;
      const tailMemory = tail?.memory ?? 0n;
      if (
        listNode.headCborLength !== head.cborLength ||
        listNode.headMemory !== head.memory ||
        listNode.payloadCborLength !== head.cborLength + tailPayload ||
        listNode.memory !== head.memory + tailMemory
      ) {
        throw new Error("CEK Data list cumulative summary is invalid");
      }
      continue;
    }

    const pairNode = decodedDataPairs.get(key);
    if (pairNode !== undefined) {
      const keyNode = decodedDataNodes.get(rootKey(pairNode.key));
      const valueNode = decodedDataNodes.get(rootKey(pairNode.value));
      if (keyNode === undefined || valueNode === undefined) {
        throw new Error("CEK Data map entry child is not a Data node");
      }
      const tail =
        pairNode.length === 1n
          ? null
          : decodedDataPairs.get(rootKey(pairNode.tail));
      if (pairNode.length > 1n && tail === undefined) {
        throw new Error("CEK Data map-entry tail is not a pair node");
      }
      const tailPayload = tail?.payloadCborLength ?? 0n;
      const tailMemory = tail?.memory ?? 0n;
      if (
        pairNode.keyCborLength !== keyNode.cborLength ||
        pairNode.keyMemory !== keyNode.memory ||
        pairNode.valueCborLength !== valueNode.cborLength ||
        pairNode.valueMemory !== valueNode.memory ||
        pairNode.payloadCborLength !==
          keyNode.cborLength + valueNode.cborLength + tailPayload ||
        pairNode.memory !== keyNode.memory + valueNode.memory + tailMemory
      ) {
        throw new Error("CEK Data map-entry cumulative summary is invalid");
      }
      continue;
    }

    const dataNode = decodedDataNodes.get(key);
    if (dataNode === undefined) continue;
    if (dataNode.kind === "constrSmall" || dataNode.kind === "constrLarge") {
      const fields =
        dataNode.fieldsCount === 0n
          ? null
          : decodedDataLists.get(rootKey(dataNode.fieldsRoot));
      if (dataNode.fieldsCount > 0n && fields === undefined) {
        throw new Error("CEK Data constructor fields are incomplete");
      }
      const fieldsPayload = fields?.payloadCborLength ?? 0n;
      const fieldsMemory = fields?.memory ?? 0n;
      const expectedLength =
        dataNode.kind === "constrSmall"
          ? midgardCekDataConstrCborLength(
              dataNode.constructor,
              dataNode.fieldsCount,
              fieldsPayload,
            )
          : 3n +
            dataNode.constructorCborLength +
            midgardCekDataListCborLength(dataNode.fieldsCount, fieldsPayload);
      if (
        dataNode.cborLength !== expectedLength ||
        dataNode.memory !== 4n + fieldsMemory
      ) {
        throw new Error("CEK Data constructor summary is invalid");
      }
      if (
        dataNode.kind === "constrLarge" &&
        (dataNode.constructorCborLength === 0n ||
          dataNode.constructorMemory < 5n)
      ) {
        throw new Error("CEK large Data constructor summary is invalid");
      }
      continue;
    }
    if (dataNode.kind === "map") {
      const entries =
        dataNode.entriesCount === 0n
          ? null
          : decodedDataPairs.get(rootKey(dataNode.entriesRoot));
      if (dataNode.entriesCount > 0n && entries === undefined) {
        throw new Error("CEK Data map entries are incomplete");
      }
      if (
        dataNode.cborLength !==
          midgardCekDataMapCborLength(
            dataNode.entriesCount,
            entries?.payloadCborLength ?? 0n,
          ) ||
        dataNode.memory !== 4n + (entries?.memory ?? 0n)
      ) {
        throw new Error("CEK Data map summary is invalid");
      }
      continue;
    }
    if (dataNode.kind === "list") {
      const items =
        dataNode.itemsCount === 0n
          ? null
          : decodedDataLists.get(rootKey(dataNode.itemsRoot));
      if (dataNode.itemsCount > 0n && items === undefined) {
        throw new Error("CEK Data list items are incomplete");
      }
      if (
        dataNode.cborLength !==
          midgardCekDataListCborLength(
            dataNode.itemsCount,
            items?.payloadCborLength ?? 0n,
          ) ||
        dataNode.memory !== 4n + (items?.memory ?? 0n)
      ) {
        throw new Error("CEK Data list summary is invalid");
      }
      continue;
    }
    if (dataNode.kind === "integer") {
      if (dataNode.cborLength === 0n || dataNode.memory < 5n) {
        throw new Error("CEK Data integer summary is invalid");
      }
      continue;
    }
    if (
      dataNode.cborLength !==
        midgardCekDataBytesCborLength(dataNode.bytesLength) ||
      dataNode.memory !==
        4n + (dataNode.bytesLength === 0n ? 1n : dataNode.bytesLength)
    ) {
      throw new Error("CEK Data bytes summary is invalid");
    }
  }

  const reconstructData = makeSemanticDataReconstructor({
    dataNodes: decodedDataNodes,
    dataLists: decodedDataLists,
    dataPairs: decodedDataPairs,
    materializeBlob,
  });

  const retainedConstants = new Map<
    string,
    {
      readonly typeCbor: Buffer;
      readonly payloadCbor: Buffer;
    }
  >();
  for (const [valueKey, value] of decodedValues) {
    let validated = cache.validatedConstants.get(valueKey);
    if (validated === undefined) {
      const typeCbor = materializeBlob(
        value.typeRoot,
        BigInt(MIDGARD_CEK_MAX_CONSTANT_TYPE_CBOR_BYTES),
        `CEK constant value ${valueKey} type`,
      );
      const declaredRoot = decodedDataNodes.get(rootKey(value.semanticRoot));
      if (
        declaredRoot !== undefined &&
        declaredRoot.cborLength !== value.payloadLength
      ) {
        throw new Error(payloadLengthMismatch(valueKey));
      }
      const decodedPayload = reconstructData(value.semanticRoot);
      // Committing first checks every node's length, so the encode below is
      // bounded by the payload length even when subtrees are shared.
      const semantic = commitSemanticData(decodedPayload);
      const payloadCbor = encodeSemanticData(decodedPayload);
      if (!Buffer.from(semantic.root).equals(value.semanticRoot)) {
        throw new Error(
          `CEK constant value ${valueKey} semantic root does not match its canonical payload`,
        );
      }
      if (semantic.cborLength !== value.payloadLength) {
        throw new Error(payloadLengthMismatch(valueKey));
      }
      const constantType = decodeSemanticConstantType(typeCbor);
      if (!semanticConstantPayloadMatchesType(constantType, decodedPayload)) {
        throw new Error(
          `CEK constant value ${valueKey} payload does not match its semantic type`,
        );
      }
      const memory = semanticConstantMemory(constantType, decodedPayload);
      if (memory !== value.memory) {
        throw new Error(
          `CEK constant value ${valueKey} memory does not match its semantic payload`,
        );
      }
      validated = {
        typeCbor: Buffer.from(typeCbor),
        payloadCbor: Buffer.from(payloadCbor),
      };
      cache.validatedConstants.set(valueKey, validated);
      try {
        options.onConstantMaterialized?.(valueKey, BigInt(payloadCbor.length));
      } catch {
        // Allocation observability must not change verification semantics.
      }
    }
    if (options.includeConstants) {
      retainedConstants.set(valueKey, {
        typeCbor: Buffer.from(validated.typeCbor),
        payloadCbor: Buffer.from(validated.payloadCbor),
      });
    }
  }

  const materialByteLength = [...reachable].reduce(
    (total, key) => total + BigInt(material.get(key)!.preimage.length),
    0n,
  );
  if (BigInt(reachable.size) !== envelope.nodeCount) {
    throw new Error(
      `CEK program reaches ${reachable.size.toString()} material nodes, envelope declares ${envelope.nodeCount.toString()}`,
    );
  }
  if (materialByteLength !== envelope.materialByteLength) {
    throw new Error(
      `CEK program reaches ${materialByteLength.toString()} material bytes, envelope declares ${envelope.materialByteLength.toString()}`,
    );
  }

  const constants = options.includeConstants
    ? [...decodedValues.entries()].map(
        ([valueKey, value]): MidgardCekProgramConstantMaterial => {
          const retained = retainedConstants.get(valueKey);
          if (retained === undefined) {
            throw new Error(`CEK constant value ${valueKey} was not retained`);
          }
          return Object.freeze({
            valueRoot: Buffer.from(valueKey, "hex") as Hash32,
            typeRoot: value.typeRoot,
            payloadRoot: value.payloadRoot,
            semanticRoot: value.semanticRoot,
            memory: value.memory,
            typeCbor: retained.typeCbor,
            payloadCbor: retained.payloadCbor,
          });
        },
      )
    : [];
  return Object.freeze({
    reachableRoots: reachable,
    nodeCount: BigInt(reachable.size),
    materialByteLength,
    constants: Object.freeze(constants),
  });
};
