import { decodeMidgardAddressBytes } from "./codec/address.js";
import {
  encodeCbor,
  readCborBytes,
  readCborMapHeader,
  readCborUnsigned,
} from "./codec/cbor.js";
import { type MidgardLedgerOutputAsset } from "./ledger-output-commitment.js";
import {
  emptyMidgardValidationMerkleFrontier,
  type MidgardValidationMerkleFrontier,
  validateMidgardValidationMerkleFrontier,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_SCAN_VERSION = 1 as const;

export const MidgardLedgerOutputScanStages = Object.freeze({
  RequiredFields: 0,
  ValueHeader: 1,
  PolicyHeader: 2,
  Asset: 3,
  OptionalField: 4,
  DatumPayload: 5,
  ReferenceScriptPayload: 6,
  Terminal: 7,
} as const);

export type MidgardLedgerOutputScanStage =
  (typeof MidgardLedgerOutputScanStages)[keyof typeof MidgardLedgerOutputScanStages];

export type MidgardLedgerOutputScanControl = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_SCAN_VERSION;
  readonly stage: MidgardLedgerOutputScanStage;
  readonly cursor: number;
  readonly mapEntryCount: number;
  readonly optionalFieldCount: number;
  readonly address: Buffer;
  readonly lovelace: bigint;
  readonly cardanoValueSize: number;
  readonly policyRemaining: number;
  readonly assetRemaining: number;
  readonly policyAssetCursor: number;
  readonly previousPolicy: Buffer;
  readonly currentPolicy: Buffer;
  readonly previousAssetName: Buffer;
  readonly assetFrontier: MidgardValidationMerkleFrontier;
  readonly datumOffset: number;
  readonly datumLength: number;
  readonly payloadRemaining: number;
  readonly referenceScriptLanguage: -1 | 0 | 3 | 128;
  readonly referenceScriptItemOffset: number;
  readonly referenceScriptOffset: number;
  readonly referenceScriptLength: number;
};

export type MidgardLedgerOutputScanTraceStep = {
  readonly control: MidgardLedgerOutputScanControl;
  readonly next: MidgardLedgerOutputScanControl;
  readonly chunkIndex: number | null;
  readonly nextChunkIndex: number | null;
  readonly asset: MidgardLedgerOutputAsset | null;
};

export type MidgardLedgerOutputScanTrace = {
  readonly initial: MidgardLedgerOutputScanControl;
  readonly steps: readonly MidgardLedgerOutputScanTraceStep[];
  readonly terminal: MidgardLedgerOutputScanControl;
};

export const initialMidgardLedgerOutputScanControl =
  (): MidgardLedgerOutputScanControl => ({
    version: MIDGARD_LEDGER_OUTPUT_SCAN_VERSION,
    stage: MidgardLedgerOutputScanStages.RequiredFields,
    cursor: 0,
    mapEntryCount: 0,
    optionalFieldCount: 0,
    address: Buffer.alloc(0),
    lovelace: 0n,
    cardanoValueSize: 0,
    policyRemaining: 0,
    assetRemaining: 0,
    policyAssetCursor: 0,
    previousPolicy: Buffer.alloc(0),
    currentPolicy: Buffer.alloc(0),
    previousAssetName: Buffer.alloc(0),
    assetFrontier: emptyMidgardValidationMerkleFrontier(),
    datumOffset: -1,
    datumLength: 0,
    payloadRemaining: 0,
    referenceScriptLanguage: -1,
    referenceScriptItemOffset: -1,
    referenceScriptOffset: -1,
    referenceScriptLength: 0,
  });

const assertSafeControlInteger = ({
  value,
  field,
  minimum,
  maximum = Number.MAX_SAFE_INTEGER,
}: {
  readonly value: number;
  readonly field: string;
  readonly minimum: number;
  readonly maximum?: number;
}): void => {
  if (!Number.isSafeInteger(value) || value < minimum || value > maximum) {
    throw new Error(`Invalid V1 ledger output scan ${field}`);
  }
};

export const encodeMidgardLedgerOutputScanControl = (
  control: MidgardLedgerOutputScanControl,
): Buffer => {
  if (control.version !== MIDGARD_LEDGER_OUTPUT_SCAN_VERSION) {
    throw new Error("Invalid V1 ledger output scan version");
  }
  assertSafeControlInteger({
    value: control.stage,
    field: "stage",
    minimum: MidgardLedgerOutputScanStages.RequiredFields,
    maximum: MidgardLedgerOutputScanStages.Terminal,
  });
  assertSafeControlInteger({
    value: control.cursor,
    field: "cursor",
    minimum: 0,
  });
  assertSafeControlInteger({
    value: control.mapEntryCount,
    field: "map entry count",
    minimum: 0,
    maximum: 4,
  });
  if (control.mapEntryCount !== 0 && control.mapEntryCount < 2) {
    throw new Error("Invalid V1 ledger output scan map entry count");
  }
  assertSafeControlInteger({
    value: control.optionalFieldCount,
    field: "optional field count",
    minimum: 0,
    maximum: 2,
  });
  if (control.address.length !== 0) {
    decodeMidgardAddressBytes(control.address);
  }
  if (control.lovelace < 0n) {
    throw new Error("Invalid V1 ledger output scan lovelace");
  }
  for (const [field, value] of [
    ["Cardano Value size", control.cardanoValueSize],
    ["policy remaining", control.policyRemaining],
    ["asset remaining", control.assetRemaining],
    ["policy asset cursor", control.policyAssetCursor],
    ["datum length", control.datumLength],
    ["payload remaining", control.payloadRemaining],
    ["reference script length", control.referenceScriptLength],
  ] as const) {
    assertSafeControlInteger({ value, field, minimum: 0 });
  }
  for (const [field, bytes, maximum] of [
    ["previous policy", control.previousPolicy, 28],
    ["current policy", control.currentPolicy, 28],
    ["previous asset name", control.previousAssetName, 32],
  ] as const) {
    if (
      bytes.length > maximum ||
      (maximum === 28 && bytes.length !== 0 && bytes.length !== 28)
    ) {
      throw new Error(`Invalid V1 ledger output scan ${field}`);
    }
  }
  validateMidgardValidationMerkleFrontier(control.assetFrontier);
  for (const [field, value] of [
    ["datum offset", control.datumOffset],
    ["reference script item offset", control.referenceScriptItemOffset],
    ["reference script offset", control.referenceScriptOffset],
  ] as const) {
    assertSafeControlInteger({ value, field, minimum: -1 });
  }
  if (control.datumOffset === -1 && control.datumLength !== 0) {
    throw new Error("Invalid V1 ledger output scan datum span");
  }
  if (
    control.referenceScriptLanguage !== -1 &&
    control.referenceScriptLanguage !== 0 &&
    control.referenceScriptLanguage !== 3 &&
    control.referenceScriptLanguage !== 128
  ) {
    throw new Error("Invalid V1 ledger output scan reference language");
  }
  if (
    control.referenceScriptLanguage === -1
      ? control.referenceScriptItemOffset !== -1 ||
        control.referenceScriptOffset !== -1 ||
        control.referenceScriptLength !== 0
      : control.referenceScriptItemOffset < 0 ||
        control.referenceScriptOffset < control.referenceScriptItemOffset
  ) {
    throw new Error("Invalid V1 ledger output scan reference span");
  }
  return encodeCbor([
    1n,
    BigInt(control.stage),
    BigInt(control.cursor),
    BigInt(control.mapEntryCount),
    BigInt(control.optionalFieldCount),
    control.address,
    control.lovelace,
    BigInt(control.cardanoValueSize),
    BigInt(control.policyRemaining),
    BigInt(control.assetRemaining),
    BigInt(control.policyAssetCursor),
    control.previousPolicy,
    control.currentPolicy,
    control.previousAssetName,
    BigInt(control.assetFrontier.count),
    control.assetFrontier.peaks.map(({ height, hash }) => [
      BigInt(height),
      hash,
    ]),
    BigInt(control.datumOffset),
    BigInt(control.datumLength),
    BigInt(control.payloadRemaining),
    BigInt(control.referenceScriptLanguage),
    BigInt(control.referenceScriptItemOffset),
    BigInt(control.referenceScriptOffset),
    BigInt(control.referenceScriptLength),
  ]);
};

export const isWellFormedMidgardLedgerOutputScanControl = (
  control: MidgardLedgerOutputScanControl,
): boolean => {
  try {
    encodeMidgardLedgerOutputScanControl(control);
    return true;
  } catch {
    return false;
  }
};

export const absoluteOffset = ({
  control,
  windowOffset,
  localOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly windowOffset: number;
  readonly localOffset: number;
}): number => control.cursor + localOffset - windowOffset;

export const readKey = (
  window: Uint8Array,
  offset: number,
  expected: bigint,
): number => {
  const key = readCborUnsigned(window, offset, "ledger_output.key");
  if (key.value !== expected) {
    throw new Error(`V1 ledger output expected key ${expected.toString(10)}`);
  }
  return key.nextOffset;
};

export const optionalFieldsComplete = (
  control: MidgardLedgerOutputScanControl,
): boolean => control.optionalFieldCount + 2 === control.mapEntryCount;

export const encodedCborLength = (value: bigint | Uint8Array): number =>
  encodeCbor(value).length;

export const encodedMapHeaderLength = (entryCount: number): number => {
  if (entryCount < 24) return 1;
  if (entryCount <= 0xff) return 2;
  if (entryCount <= 0xffff) return 3;
  if (entryCount <= 0xffff_ffff) return 5;
  throw new Error("V1 Cardano Value map exceeds the uint32 envelope");
};

export const stepRequiredFields = ({
  control,
  window,
  windowOffset,
}: {
  readonly control: MidgardLedgerOutputScanControl;
  readonly window: Uint8Array;
  readonly windowOffset: number;
}): MidgardLedgerOutputScanControl => {
  const outputMap = readCborMapHeader(window, windowOffset, "ledger_output");
  if (outputMap.length < 2 || outputMap.length > 4) {
    throw new Error("V1 ledger output must contain two to four fields");
  }
  const addressOffset = readKey(window, outputMap.nextOffset, 0n);
  const address = readCborBytes(window, addressOffset, "ledger_output.address");
  decodeMidgardAddressBytes(address.value);
  return {
    ...control,
    stage: MidgardLedgerOutputScanStages.ValueHeader,
    cursor: absoluteOffset({
      control,
      windowOffset,
      localOffset: address.nextOffset,
    }),
    mapEntryCount: outputMap.length,
    address: address.value,
  };
};
