import {
  buildMidgardValidationMerkleFrontier,
  buildMidgardValidationMerkleMembership,
  commitMidgardCekBlob,
  encodeCbor,
  hashMidgardCekDataNode,
  MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES,
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
  type MidgardCekDataNode,
  type MidgardValidationMerkleFrontier,
  summarizeMidgardCekLargeConstrData,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
  validateMidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import {
  type Data,
  DataB,
  DataConstr,
  dataFromCbor,
  DataI,
  DataList,
} from "@harmoniclabs/plutus-data";
import { Constr, Data as LucidData, fromHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  encodeMidgardCekPlutusData,
  midgardCekIntegerMemorySize,
} from "./cek-constant.js";
import {
  isByteStringLike,
  isPlutusDataMap,
  type PlutusDataMap,
} from "./plutus-data-narrowing.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
} from "./script-context-proof.js";

const FRAME_DOMAIN = Buffer.from("MidgardCekDataScanFrameV1", "ascii");
const CHILD_DOMAIN = Buffer.from("MidgardCekDataScanChildV1", "ascii");

const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

const boundedNatural = (
  value: number,
  fieldName: string,
  maximum = Number.MAX_SAFE_INTEGER,
): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > maximum) {
    throw new Error(`${fieldName} is outside its canonical bound`);
  }
  return value;
};

const nonNegativeBigint = (value: bigint, fieldName: string): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${fieldName} must be a non-negative bigint`);
  }
  return value;
};

const exactHashOrEmpty = (value: Uint8Array, fieldName: string): Buffer => {
  const exact = Buffer.from(value);
  if (exact.length !== 0 && exact.length !== 32) {
    throw new Error(`${fieldName} must be empty or exactly 32 bytes`);
  }
  return exact;
};

const validateSummary = (
  summary: MidgardCekDataSummary,
  fieldName: string,
  allowEmpty: boolean,
): void => {
  const root = Buffer.from(summary.root);
  if (
    (allowEmpty && root.length !== 0 && root.length !== 32) ||
    (!allowEmpty && root.length !== 32)
  ) {
    throw new Error(
      `${fieldName}.root must ${allowEmpty ? "be empty or " : ""}contain exactly 32 bytes`,
    );
  }
  nonNegativeBigint(summary.cborLength, `${fieldName}.cbor_length`);
  nonNegativeBigint(summary.memory, `${fieldName}.memory`);
  if (
    root.length === 0 &&
    (summary.cborLength !== 0n || summary.memory !== 0n)
  ) {
    throw new Error(`${fieldName} has a noncanonical empty root`);
  }
};

const boolDataCbor = (value: boolean): Buffer =>
  Buffer.from(fromHex(LucidData.to(new Constr(value ? 1 : 0, []))));

const summaryCbor = (summary: MidgardCekDataSummary): Buffer =>
  encodeCbor([Buffer.from(summary.root), summary.cborLength, summary.memory]);

export type MidgardCekDataScanFrame = {
  readonly kind: 0 | 1 | 2 | 3;
  readonly constructor: bigint;
  readonly tail: Buffer;
  readonly expectedChildren: number;
  readonly childCount: number;
  readonly childFrontier: MidgardValidationMerkleFrontier;
  readonly foldCursor: number;
  readonly sequence: MidgardCekDataSequenceSummary;
};

export type MidgardCekDataScanControl = {
  readonly rawHash: Buffer;
  readonly rawLength: number;
  readonly offset: number;
  readonly frameRoot: Buffer;
  readonly frameClosed: boolean;
  readonly result: MidgardCekDataSummary | null;
};

export type MidgardCekDataScanStep =
  | {
      readonly kind: "openConstructor";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrame | null;
      readonly constructor: bigint;
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "openList";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrame | null;
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "openMap";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrame | null;
    }
  | {
      readonly kind: "revealLeaf";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrame | null;
      readonly itemLength: number;
    }
  | {
      readonly kind: "closeSequence";
      readonly rawCbor: Buffer;
      readonly frame: MidgardCekDataScanFrame;
    }
  | {
      readonly kind: "foldList";
      readonly frame: MidgardCekDataScanFrame;
      readonly childIndex: number;
      readonly child: MidgardCekDataSummary;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "foldMap";
      readonly frame: MidgardCekDataScanFrame;
      readonly pairIndex: number;
      readonly key: MidgardCekDataSummary;
      readonly value: MidgardCekDataSummary;
      readonly keySiblings: readonly Buffer[];
      readonly valueSiblings: readonly Buffer[];
    }
  | {
      readonly kind: "finalizeFrame";
      readonly frame: MidgardCekDataScanFrame;
      readonly parent: MidgardCekDataScanFrame | null;
    };

export type MidgardCekDataScanTraceStep = {
  readonly control: MidgardCekDataScanControl;
  readonly step: MidgardCekDataScanStep;
};

export const validateMidgardCekDataScanControl = (
  control: MidgardCekDataScanControl,
): void => {
  if (Buffer.from(control.rawHash).length !== 32) {
    throw new Error("cek_data_scan.raw_hash must contain exactly 32 bytes");
  }
  const rawLength = boundedNatural(
    control.rawLength,
    "cek_data_scan.raw_length",
    MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES,
  );
  if (rawLength === 0) {
    throw new Error("cek_data_scan.raw_length must be positive");
  }
  const offset = boundedNatural(
    control.offset,
    "cek_data_scan.offset",
    rawLength,
  );
  const frameRoot = exactHashOrEmpty(
    control.frameRoot,
    "cek_data_scan.frame_root",
  );
  if (typeof control.frameClosed !== "boolean") {
    throw new Error("cek_data_scan.frame_closed must be boolean");
  }
  if (control.result === null) {
    if (frameRoot.length === 0 && (control.frameClosed || offset !== 0)) {
      throw new Error(
        "an empty data-scan stack must be at the canonical initial state",
      );
    }
    return;
  }
  validateSummary(control.result, "cek_data_scan.result", false);
  if (frameRoot.length !== 0 || control.frameClosed || offset !== rawLength) {
    throw new Error(
      "a completed data-scan result requires the canonical terminal state",
    );
  }
};

export const validateMidgardCekDataScanFrame = (
  frame: MidgardCekDataScanFrame,
): void => {
  const kind = boundedNatural(frame.kind, "cek_data_scan_frame.kind", 3);
  nonNegativeBigint(frame.constructor, "cek_data_scan_frame.constructor");
  if (kind !== 1 && frame.constructor !== 0n) {
    throw new Error(
      "only a constructor data-scan frame may bind a constructor index",
    );
  }
  exactHashOrEmpty(frame.tail, "cek_data_scan_frame.tail");
  const expectedChildren = boundedNatural(
    frame.expectedChildren,
    "cek_data_scan_frame.expected_children",
  );
  if (kind === 0 && expectedChildren !== 1) {
    throw new Error("the root data-scan frame must expect one child");
  }
  if (kind === 3 && expectedChildren % 2 !== 0) {
    throw new Error("a map data-scan frame must expect key/value pairs");
  }
  const childCount = boundedNatural(
    frame.childCount,
    "cek_data_scan_frame.child_count",
    expectedChildren,
  );
  validateMidgardValidationMerkleFrontier(frame.childFrontier);
  if (frame.childFrontier.count !== childCount) {
    throw new Error(
      "data-scan frame child count disagrees with its authenticated frontier",
    );
  }
  const maximumFoldCursor =
    kind === 3 ? expectedChildren / 2 : expectedChildren;
  const foldCursor = boundedNatural(
    frame.foldCursor,
    "cek_data_scan_frame.fold_cursor",
    maximumFoldCursor,
  );
  if (foldCursor > 0 && childCount !== expectedChildren) {
    throw new Error(
      "a folding data-scan frame must have all expected children",
    );
  }
  validateSummary(
    {
      root: frame.sequence.root,
      cborLength: frame.sequence.payloadCborLength,
      memory: frame.sequence.memory,
    },
    "cek_data_scan_frame.sequence",
    false,
  );
  if (frame.sequence.length !== BigInt(foldCursor)) {
    throw new Error(
      "data-scan frame fold cursor disagrees with its sequence length",
    );
  }
};

const emptySummary = (): MidgardCekDataSummary => ({
  root: Buffer.alloc(0),
  cborLength: 0n,
  memory: 0n,
});

export const encodeMidgardCekDataScanControl = (
  control: MidgardCekDataScanControl,
): Buffer => {
  validateMidgardCekDataScanControl(control);
  return encodeCborArrayRaw([
    encodeCborBytes(control.rawHash),
    encodeCborInteger(BigInt(control.rawLength)),
    encodeCborInteger(BigInt(control.offset)),
    encodeCborBytes(control.frameRoot),
    boolDataCbor(control.frameClosed),
    summaryCbor(control.result ?? emptySummary()),
  ]);
};

export const hashMidgardCekDataScanControl = (
  control: MidgardCekDataScanControl,
): Buffer => hash32(encodeMidgardCekDataScanControl(control));

export const hashMidgardCekDataScanFrame = (
  frame: MidgardCekDataScanFrame,
): Buffer => {
  validateMidgardCekDataScanFrame(frame);
  return hash32(
    Buffer.concat([
      FRAME_DOMAIN,
      encodeCbor([
        BigInt(frame.kind),
        frame.constructor,
        frame.tail,
        BigInt(frame.expectedChildren),
        BigInt(frame.childCount),
        frame.childFrontier.peaks.map((peak) => [
          BigInt(peak.height),
          peak.hash,
        ]),
        BigInt(frame.foldCursor),
        [
          Buffer.from(frame.sequence.root),
          frame.sequence.length,
          frame.sequence.payloadCborLength,
          frame.sequence.memory,
        ],
      ]),
    ]),
  );
};

export const hashMidgardCekDataScanChild = (
  childIndex: number,
  child: MidgardCekDataSummary,
): Buffer => {
  boundedNatural(childIndex, "cek_data_scan_child.index");
  validateSummary(child, "cek_data_scan_child.summary", false);
  return hash32(
    Buffer.concat([
      CHILD_DOMAIN,
      encodeCbor(BigInt(childIndex)),
      encodeCbor(Buffer.from(child.root)),
      encodeCbor(child.cborLength),
      encodeCbor(child.memory),
    ]),
  );
};

type MutableFrame = {
  frame: MidgardCekDataScanFrame;
  children: MidgardCekDataSummary[];
};

type StructuredData = DataConstr | DataList | PlutusDataMap;

type ScanWork =
  | {
      readonly kind: "enter";
      readonly data: Data;
      readonly parent: MutableFrame | null;
    }
  | {
      readonly kind: "exit";
      readonly data: StructuredData;
      readonly frame: MutableFrame;
      readonly parent: MutableFrame | null;
    };

const replaceFrame = (target: MutableFrame, source: MutableFrame): void => {
  target.frame = source.frame;
  target.children = source.children;
};

const frameWith = (
  value: MutableFrame,
  foldCursor: number,
  sequence: MidgardCekDataSequenceSummary,
): MutableFrame => ({
  frame: { ...value.frame, foldCursor, sequence },
  children: value.children,
});

const appendChild = (
  value: MutableFrame,
  child: MidgardCekDataSummary,
): MutableFrame => {
  const children = [...value.children, child];
  const leaves = children.map((item, index) =>
    hashMidgardCekDataScanChild(index, item),
  );
  return {
    frame: {
      ...value.frame,
      childCount: children.length,
      childFrontier: buildMidgardValidationMerkleFrontier(leaves),
    },
    children,
  };
};

const mapHeaderLength = (pairs: number): number => {
  if (pairs < 24) return 1;
  if (pairs <= 0xff) return 2;
  if (pairs <= 0xffff) return 3;
  if (pairs <= 0xffff_ffff) return 5;
  return 9;
};

const constructorHeaderLength = (constructor: bigint): number => {
  if (constructor <= 6n) return 3;
  if (constructor <= 127n) return 4;
  return 4 + encodeMidgardCekPlutusData(new DataI(constructor)).length;
};

const scalarBytes = (value: DataB): Uint8Array => {
  const candidate: unknown = value.bytes;
  if (!isByteStringLike(candidate)) {
    throw new Error("CEK Data scanner received an invalid byte leaf");
  }
  const bytes = candidate.toBuffer();
  if (!(bytes instanceof Uint8Array)) {
    throw new Error("CEK Data scanner byte leaf did not produce bytes");
  }
  return bytes;
};

const scalarSummary = (data: DataI | DataB): MidgardCekDataSummary => {
  let node: MidgardCekDataNode;
  if (data instanceof DataI) {
    const cbor = encodeMidgardCekPlutusData(data);
    node = {
      kind: "integer",
      cborRoot: commitMidgardCekBlob(cbor).root,
      cborLength: BigInt(cbor.length),
      memory: 4n + midgardCekIntegerMemorySize(data.int),
    };
  } else {
    const bytes = scalarBytes(data);
    node = {
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(bytes).root,
      bytesLength: BigInt(bytes.length),
      cborLength: midgardCekDataBytesCborLength(BigInt(bytes.length)),
      memory: midgardCekDataBytesMemory(BigInt(bytes.length)),
    };
  }
  return {
    root: Buffer.from(hashMidgardCekDataNode(node)),
    cborLength: node.cborLength,
    memory: node.memory,
  };
};

const structuredSummary = (
  data: StructuredData,
  sequence: MidgardCekDataSequenceSummary,
): MidgardCekDataSummary => {
  if (data instanceof DataConstr) {
    if (data.constr <= 127n) {
      return summarizeMidgardCekSmallConstrData(data.constr, sequence);
    }
    const constructorCbor = encodeMidgardCekPlutusData(new DataI(data.constr));
    return summarizeMidgardCekLargeConstrData({
      constructorCborRoot: commitMidgardCekBlob(constructorCbor).root,
      constructorCborLength: BigInt(constructorCbor.length),
      constructorMemory: 4n + midgardCekIntegerMemorySize(data.constr),
      fields: sequence,
    });
  }
  if (data instanceof DataList) {
    return summarizeMidgardCekListData(sequence);
  }
  return summarizeMidgardCekMapData(sequence);
};

/**
 * Produces the exact content-addressed scan accepted by the L1 Data scanner.
 * Every transition reveals at most the one independently bounded raw Data
 * preimage plus fixed-size frame/frontier material.
 */
export const buildMidgardCekDataScanTrace = (
  rawCbor: Uint8Array,
): {
  readonly initial: MidgardCekDataScanControl;
  readonly steps: readonly MidgardCekDataScanTraceStep[];
  readonly terminal: MidgardCekDataScanControl;
} => {
  const raw = Buffer.from(rawCbor);
  if (raw.length === 0 || raw.length > 9_215) {
    throw new Error("V1 Data scan preimage must contain 1..9215 bytes");
  }
  const rootData = dataFromCbor(raw);
  const initial: MidgardCekDataScanControl = {
    rawHash: hash32(raw),
    rawLength: raw.length,
    offset: 0,
    frameRoot: Buffer.alloc(0),
    frameClosed: false,
    result: null,
  };
  let control = initial;
  const steps: MidgardCekDataScanTraceStep[] = [];
  const emit = (step: MidgardCekDataScanStep): void => {
    steps.push({ control, step });
  };

  const canonical = encodeMidgardCekPlutusData(rootData);
  if (!canonical.equals(raw)) {
    throw new Error("V1 Data scan source is not canonical Data CBOR");
  }
  const work: ScanWork[] = [{ kind: "enter", data: rootData, parent: null }];
  while (work.length > 0) {
    const operation = work.pop()!;
    if (operation.kind === "enter") {
      const { data, parent } = operation;
      if (data instanceof DataI || data instanceof DataB) {
        const summary = scalarSummary(data);
        const encoded = encodeMidgardCekPlutusData(data);
        if (data instanceof DataB && scalarBytes(data).length > 9_215) {
          throw new Error(
            "CEK Data scanner byte leaf exceeds its proof envelope",
          );
        }
        emit({
          kind: "revealLeaf",
          rawCbor: raw,
          parent: parent?.frame ?? null,
          itemLength: encoded.length,
        });
        control = {
          ...control,
          offset: control.offset + encoded.length,
        };
        if (parent === null) {
          if (control.offset !== raw.length) {
            throw new Error("CEK scalar Data root has trailing bytes");
          }
          control = { ...control, result: summary };
          continue;
        }
        const nextParent = appendChild(parent, summary);
        replaceFrame(parent, nextParent);
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrame(parent.frame),
          frameClosed:
            parent.frame.kind === 3 &&
            parent.frame.childCount === parent.frame.expectedChildren,
        };
        continue;
      }

      if (
        !(data instanceof DataConstr) &&
        !(data instanceof DataList) &&
        !isPlutusDataMap(data)
      ) {
        throw new Error("CEK Data scanner received an unknown node");
      }
      if (data instanceof DataConstr && data.constr < 0n) {
        throw new Error("Plutus Data constructor must be non-negative");
      }
      const structuredData: StructuredData = data;
      const children: Data[] =
        data instanceof DataConstr
          ? [...data.fields]
          : data instanceof DataList
            ? [...data.list]
            : data.map.flatMap((pair) => [pair.fst, pair.snd]);
      const kind: 1 | 2 | 3 =
        data instanceof DataConstr ? 1 : data instanceof DataList ? 2 : 3;
      const constructor = data instanceof DataConstr ? data.constr : 0n;
      const frame: MutableFrame = {
        frame: {
          kind,
          constructor,
          tail:
            parent === null
              ? Buffer.alloc(0)
              : hashMidgardCekDataScanFrame(parent.frame),
          expectedChildren: children.length,
          childCount: 0,
          childFrontier: buildMidgardValidationMerkleFrontier([]),
          foldCursor: 0,
          sequence:
            kind === 3
              ? emptyMidgardCekDataPairSummary()
              : emptyMidgardCekDataListSummary(),
        },
        children: [],
      };
      if (kind === 1) {
        emit({
          kind: "openConstructor",
          rawCbor: raw,
          parent: parent?.frame ?? null,
          constructor,
          expectedChildren: children.length,
        });
        control = {
          ...control,
          offset: control.offset + constructorHeaderLength(constructor),
        };
      } else if (kind === 2) {
        emit({
          kind: "openList",
          rawCbor: raw,
          parent: parent?.frame ?? null,
          expectedChildren: children.length,
        });
        control = { ...control, offset: control.offset + 1 };
      } else {
        emit({
          kind: "openMap",
          rawCbor: raw,
          parent: parent?.frame ?? null,
        });
        control = {
          ...control,
          offset: control.offset + mapHeaderLength(children.length / 2),
        };
      }
      control = {
        ...control,
        frameRoot: hashMidgardCekDataScanFrame(frame.frame),
        frameClosed: children.length === 0,
      };
      work.push({ kind: "exit", data: structuredData, frame, parent });
      for (let index = children.length - 1; index >= 0; index -= 1) {
        work.push({ kind: "enter", data: children[index]!, parent: frame });
      }
      continue;
    }

    const { data, frame, parent } = operation;
    if (frame.frame.kind !== 3 && frame.frame.expectedChildren > 0) {
      emit({ kind: "closeSequence", rawCbor: raw, frame: frame.frame });
      control = {
        ...control,
        offset: control.offset + 1,
        frameClosed: true,
      };
    }

    const leaves = frame.children.map((child, index) =>
      hashMidgardCekDataScanChild(index, child),
    );
    if (frame.frame.kind === 3) {
      for (
        let pairIndex = frame.frame.expectedChildren / 2 - 1;
        pairIndex >= 0;
        pairIndex -= 1
      ) {
        const keyIndex = pairIndex * 2;
        const valueIndex = keyIndex + 1;
        const key = frame.children[keyIndex]!;
        const value = frame.children[valueIndex]!;
        emit({
          kind: "foldMap",
          frame: frame.frame,
          pairIndex,
          key,
          value,
          keySiblings: buildMidgardValidationMerkleMembership(leaves, keyIndex)
            .siblings,
          valueSiblings: buildMidgardValidationMerkleMembership(
            leaves,
            valueIndex,
          ).siblings,
        });
        replaceFrame(
          frame,
          frameWith(
            frame,
            frame.frame.foldCursor + 1,
            prependMidgardCekDataPairSummary(key, value, frame.frame.sequence),
          ),
        );
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrame(frame.frame),
        };
      }
    } else {
      for (
        let childIndex = frame.frame.expectedChildren - 1;
        childIndex >= 0;
        childIndex -= 1
      ) {
        const child = frame.children[childIndex]!;
        emit({
          kind: "foldList",
          frame: frame.frame,
          childIndex,
          child,
          siblings: buildMidgardValidationMerkleMembership(leaves, childIndex)
            .siblings,
        });
        replaceFrame(
          frame,
          frameWith(
            frame,
            frame.frame.foldCursor + 1,
            prependMidgardCekDataListSummary(child, frame.frame.sequence),
          ),
        );
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrame(frame.frame),
        };
      }
    }

    const summary = structuredSummary(data, frame.frame.sequence);
    emit({
      kind: "finalizeFrame",
      frame: frame.frame,
      parent: parent?.frame ?? null,
    });
    if (parent === null) {
      if (control.offset !== raw.length) {
        throw new Error("CEK structured Data root has trailing bytes");
      }
      control = {
        ...control,
        frameRoot: Buffer.alloc(0),
        frameClosed: false,
        result: summary,
      };
      continue;
    }
    const nextParent = appendChild(parent, summary);
    replaceFrame(parent, nextParent);
    control = {
      ...control,
      frameRoot: hashMidgardCekDataScanFrame(parent.frame),
      frameClosed:
        parent.frame.kind === 3 &&
        parent.frame.childCount === parent.frame.expectedChildren,
    };
  }
  if (control.result === null) {
    throw new Error("CEK Data scanner did not produce a terminal summary");
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};
