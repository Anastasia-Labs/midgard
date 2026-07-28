import {
  buildMidgardValidationMerkleFrontierV1,
  buildMidgardValidationMerkleMembershipV1,
  encodeCbor,
  MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1,
  type MidgardValidationMerkleFrontierV1,
  validateMidgardValidationMerkleFrontierV1,
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
  DataMap,
} from "@harmoniclabs/plutus-data";
import { Constr, Data as LucidData, fromHex } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { encodeMidgardCekPlutusDataV1 } from "./cek-constant.js";
import { commitMidgardCekDataTreeV1 } from "./cek-data-tree.js";
import {
  emptyMidgardCekDataListSummaryV1,
  emptyMidgardCekDataPairSummaryV1,
  type MidgardCekDataSequenceSummaryV1,
  type MidgardCekDataSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
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
  summary: MidgardCekDataSummaryV1,
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

const summaryCbor = (summary: MidgardCekDataSummaryV1): Buffer =>
  encodeCbor([Buffer.from(summary.root), summary.cborLength, summary.memory]);

export type MidgardCekDataScanFrameV1 = {
  readonly kind: 0 | 1 | 2 | 3;
  readonly constructor: bigint;
  readonly tail: Buffer;
  readonly expectedChildren: number;
  readonly childCount: number;
  readonly childFrontier: MidgardValidationMerkleFrontierV1;
  readonly foldCursor: number;
  readonly sequence: MidgardCekDataSequenceSummaryV1;
};

export type MidgardCekDataScanControlV1 = {
  readonly rawHash: Buffer;
  readonly rawLength: number;
  readonly offset: number;
  readonly frameRoot: Buffer;
  readonly frameClosed: boolean;
  readonly result: MidgardCekDataSummaryV1 | null;
};

export type MidgardCekDataScanStepV1 =
  | {
      readonly kind: "openConstructor";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrameV1 | null;
      readonly constructor: bigint;
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "openList";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrameV1 | null;
      readonly expectedChildren: number;
    }
  | {
      readonly kind: "openMap";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrameV1 | null;
    }
  | {
      readonly kind: "revealLeaf";
      readonly rawCbor: Buffer;
      readonly parent: MidgardCekDataScanFrameV1 | null;
      readonly itemLength: number;
    }
  | {
      readonly kind: "closeSequence";
      readonly rawCbor: Buffer;
      readonly frame: MidgardCekDataScanFrameV1;
    }
  | {
      readonly kind: "foldList";
      readonly frame: MidgardCekDataScanFrameV1;
      readonly childIndex: number;
      readonly child: MidgardCekDataSummaryV1;
      readonly siblings: readonly Buffer[];
    }
  | {
      readonly kind: "foldMap";
      readonly frame: MidgardCekDataScanFrameV1;
      readonly pairIndex: number;
      readonly key: MidgardCekDataSummaryV1;
      readonly value: MidgardCekDataSummaryV1;
      readonly keySiblings: readonly Buffer[];
      readonly valueSiblings: readonly Buffer[];
    }
  | {
      readonly kind: "finalizeFrame";
      readonly frame: MidgardCekDataScanFrameV1;
      readonly parent: MidgardCekDataScanFrameV1 | null;
    };

export type MidgardCekDataScanTraceStepV1 = {
  readonly control: MidgardCekDataScanControlV1;
  readonly step: MidgardCekDataScanStepV1;
};

export const validateMidgardCekDataScanControlV1 = (
  control: MidgardCekDataScanControlV1,
): void => {
  if (Buffer.from(control.rawHash).length !== 32) {
    throw new Error("cek_data_scan.raw_hash must contain exactly 32 bytes");
  }
  const rawLength = boundedNatural(
    control.rawLength,
    "cek_data_scan.raw_length",
    MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES_V1,
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

export const validateMidgardCekDataScanFrameV1 = (
  frame: MidgardCekDataScanFrameV1,
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
  validateMidgardValidationMerkleFrontierV1(frame.childFrontier);
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

const emptySummary = (): MidgardCekDataSummaryV1 => ({
  root: Buffer.alloc(0),
  cborLength: 0n,
  memory: 0n,
});

const exactSummary = (data: Data): MidgardCekDataSummaryV1 => {
  const tree = commitMidgardCekDataTreeV1(data);
  return {
    root: Buffer.from(tree.root),
    cborLength: tree.cborLength,
    memory: tree.memory,
  };
};

export const encodeMidgardCekDataScanControlV1 = (
  control: MidgardCekDataScanControlV1,
): Buffer => {
  validateMidgardCekDataScanControlV1(control);
  return encodeCborArrayRaw([
    encodeCborBytes(control.rawHash),
    encodeCborInteger(BigInt(control.rawLength)),
    encodeCborInteger(BigInt(control.offset)),
    encodeCborBytes(control.frameRoot),
    boolDataCbor(control.frameClosed),
    summaryCbor(control.result ?? emptySummary()),
  ]);
};

export const hashMidgardCekDataScanControlV1 = (
  control: MidgardCekDataScanControlV1,
): Buffer => hash32(encodeMidgardCekDataScanControlV1(control));

export const hashMidgardCekDataScanFrameV1 = (
  frame: MidgardCekDataScanFrameV1,
): Buffer => {
  validateMidgardCekDataScanFrameV1(frame);
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

export const hashMidgardCekDataScanChildV1 = (
  childIndex: number,
  child: MidgardCekDataSummaryV1,
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
  frame: MidgardCekDataScanFrameV1;
  children: MidgardCekDataSummaryV1[];
};

const frameWith = (
  value: MutableFrame,
  foldCursor: number,
  sequence: MidgardCekDataSequenceSummaryV1,
): MutableFrame => ({
  frame: { ...value.frame, foldCursor, sequence },
  children: value.children,
});

const appendChild = (
  value: MutableFrame,
  child: MidgardCekDataSummaryV1,
): MutableFrame => {
  const children = [...value.children, child];
  const leaves = children.map((item, index) =>
    hashMidgardCekDataScanChildV1(index, item),
  );
  return {
    frame: {
      ...value.frame,
      childCount: children.length,
      childFrontier: buildMidgardValidationMerkleFrontierV1(leaves),
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
  return 4 + encodeMidgardCekPlutusDataV1(new DataI(constructor)).length;
};

const scalarBytes = (value: DataB): Uint8Array => {
  const candidate = value.bytes as unknown;
  if (
    typeof candidate !== "object" ||
    candidate === null ||
    !("toBuffer" in candidate) ||
    typeof candidate.toBuffer !== "function"
  ) {
    throw new Error("CEK Data scanner received an invalid byte leaf");
  }
  return candidate.toBuffer();
};

/**
 * Produces the exact content-addressed scan accepted by the L1 Data scanner.
 * Every transition reveals at most the one independently bounded raw Data
 * preimage plus fixed-size frame/frontier material.
 */
export const buildMidgardCekDataScanTraceV1 = (
  rawCbor: Uint8Array,
): {
  readonly initial: MidgardCekDataScanControlV1;
  readonly steps: readonly MidgardCekDataScanTraceStepV1[];
  readonly terminal: MidgardCekDataScanControlV1;
} => {
  const raw = Buffer.from(rawCbor);
  if (raw.length === 0 || raw.length > 9_215) {
    throw new Error("V1 Data scan preimage must contain 1..9215 bytes");
  }
  const rootData = dataFromCbor(raw);
  const initial: MidgardCekDataScanControlV1 = {
    rawHash: hash32(raw),
    rawLength: raw.length,
    offset: 0,
    frameRoot: Buffer.alloc(0),
    frameClosed: false,
    result: null,
  };
  let control = initial;
  const steps: MidgardCekDataScanTraceStepV1[] = [];
  const emit = (step: MidgardCekDataScanStepV1): void => {
    steps.push({ control, step });
  };

  const scan = (
    data: Data,
    parent: MutableFrame | null,
  ): MutableFrame | null => {
    const summary = exactSummary(data);
    if (data instanceof DataI || data instanceof DataB) {
      const encoded = encodeMidgardCekPlutusDataV1(data);
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
        return null;
      }
      const nextParent = appendChild(parent, summary);
      control = {
        ...control,
        frameRoot: hashMidgardCekDataScanFrameV1(nextParent.frame),
        frameClosed:
          nextParent.frame.kind === 3 &&
          nextParent.frame.childCount === nextParent.frame.expectedChildren,
      };
      return nextParent;
    }

    const children: Data[] =
      data instanceof DataConstr
        ? [...data.fields]
        : data instanceof DataList
          ? [...data.list]
          : data instanceof DataMap
            ? data.map.flatMap((pair) => [pair.fst, pair.snd])
            : (() => {
                throw new Error("CEK Data scanner received an unknown node");
              })();
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
            : hashMidgardCekDataScanFrameV1(parent.frame),
        expectedChildren: children.length,
        childCount: 0,
        childFrontier: buildMidgardValidationMerkleFrontierV1([]),
        foldCursor: 0,
        sequence:
          kind === 3
            ? emptyMidgardCekDataPairSummaryV1()
            : emptyMidgardCekDataListSummaryV1(),
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
    let current = frame;
    control = {
      ...control,
      frameRoot: hashMidgardCekDataScanFrameV1(current.frame),
      frameClosed: children.length === 0,
    };
    for (const child of children) {
      const returned = scan(child, current);
      if (returned === null) {
        throw new Error("nested CEK Data scan lost its parent frame");
      }
      current = returned;
    }

    if (kind !== 3 && children.length > 0) {
      emit({ kind: "closeSequence", rawCbor: raw, frame: current.frame });
      control = {
        ...control,
        offset: control.offset + 1,
        frameClosed: true,
      };
    }

    const leaves = current.children.map((child, index) =>
      hashMidgardCekDataScanChildV1(index, child),
    );
    if (kind === 3) {
      for (
        let pairIndex = children.length / 2 - 1;
        pairIndex >= 0;
        pairIndex -= 1
      ) {
        const keyIndex = pairIndex * 2;
        const valueIndex = keyIndex + 1;
        const key = current.children[keyIndex]!;
        const value = current.children[valueIndex]!;
        emit({
          kind: "foldMap",
          frame: current.frame,
          pairIndex,
          key,
          value,
          keySiblings: buildMidgardValidationMerkleMembershipV1(
            leaves,
            keyIndex,
          ).siblings,
          valueSiblings: buildMidgardValidationMerkleMembershipV1(
            leaves,
            valueIndex,
          ).siblings,
        });
        current = frameWith(
          current,
          current.frame.foldCursor + 1,
          prependMidgardCekDataPairSummaryV1(
            key,
            value,
            current.frame.sequence,
          ),
        );
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrameV1(current.frame),
        };
      }
    } else {
      for (
        let childIndex = children.length - 1;
        childIndex >= 0;
        childIndex -= 1
      ) {
        const child = current.children[childIndex]!;
        emit({
          kind: "foldList",
          frame: current.frame,
          childIndex,
          child,
          siblings: buildMidgardValidationMerkleMembershipV1(leaves, childIndex)
            .siblings,
        });
        current = frameWith(
          current,
          current.frame.foldCursor + 1,
          prependMidgardCekDataListSummaryV1(child, current.frame.sequence),
        );
        control = {
          ...control,
          frameRoot: hashMidgardCekDataScanFrameV1(current.frame),
        };
      }
    }

    emit({
      kind: "finalizeFrame",
      frame: current.frame,
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
      return null;
    }
    const nextParent = appendChild(parent, summary);
    control = {
      ...control,
      frameRoot: hashMidgardCekDataScanFrameV1(nextParent.frame),
      frameClosed:
        nextParent.frame.kind === 3 &&
        nextParent.frame.childCount === nextParent.frame.expectedChildren,
    };
    return nextParent;
  };

  const canonical = encodeMidgardCekPlutusDataV1(rootData);
  if (!canonical.equals(raw)) {
    throw new Error("V1 Data scan source is not canonical Data CBOR");
  }
  scan(rootData, null);
  if (control.result === null) {
    throw new Error("CEK Data scanner did not produce a terminal summary");
  }
  return Object.freeze({
    initial,
    steps: Object.freeze(steps),
    terminal: control,
  });
};
