import {
  encodeCbor,
  MIDGARD_CEK_MAX_SOURCE_CONSTANT_PAYLOAD_BYTES,
  type MidgardValidationMerkleFrontier,
  validateMidgardValidationMerkleFrontier,
} from "@al-ft/midgard-core";
import {
  encodeCborArrayRaw,
  encodeCborBytes,
  encodeCborInteger,
} from "@al-ft/midgard-core/codec/cbor";
import { lucidDataToCborIterative } from "@al-ft/midgard-core/plutus-data-lucid-iterative";
import { Constr } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
} from "./script-context-proof.js";

const FRAME_DOMAIN = Buffer.from("MidgardCekDataScanFrameV1", "ascii");

export const CHILD_DOMAIN = Buffer.from("MidgardCekDataScanChildV1", "ascii");

export const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

export const boundedNatural = (
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

export const validateSummary = (
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
  lucidDataToCborIterative(new Constr(value ? 1 : 0, []));

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
