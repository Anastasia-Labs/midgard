import { encodeCbor } from "@al-ft/midgard-core";
import {
  type Data as PlutusData,
  DataConstr,
  DataList,
} from "@harmoniclabs/plutus-data";

import {
  bytes,
  emptyMidgardCekDataSummary,
  encodeMidgardCekDataSequenceSummary,
  encodeMidgardCekDataSummary,
  type MidgardCekContextControl,
  type MidgardCekFinalContextControl,
  requiredHash32,
  summarizeMidgardCekData,
  summarizeMidgardCekDataList,
  summarizeMidgardCekDataPairs,
} from "./cek-context.midgard-cek-context-control.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import {
  isPlutusDataMap,
  type PlutusDataMap,
} from "./plutus-data-narrowing.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "./script-context-proof.js";

export const initialMidgardCekContextControl = (input: {
  readonly languageTag: 3 | 128;
  readonly programTermRoot: Uint8Array;
  readonly programEnvelopeHash: Uint8Array;
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Uint8Array;
  readonly subject: Uint8Array;
  readonly redeemerLeaf: Uint8Array;
}): MidgardCekContextControl => ({
  stage: 0,
  languageTag: input.languageTag,
  programTermRoot: bytes(input.programTermRoot),
  programEnvelopeHash: requiredHash32(
    "CEK program envelope hash",
    input.programEnvelopeHash,
  ),
  purposeKind: input.purposeKind,
  purposeIndex: input.purposeIndex,
  scriptHash: bytes(input.scriptHash),
  subject: bytes(input.subject),
  redeemerLeaf: bytes(input.redeemerLeaf),
  redeemerContextControlHash: Buffer.alloc(0),
  executionMemoryLimit: 0n,
  executionCpuLimit: 0n,
  referenceItems: emptyMidgardCekDataListSummary(),
  spendItems: emptyMidgardCekDataListSummary(),
  outputItems: emptyMidgardCekDataListSummary(),
  signerItems: emptyMidgardCekDataListSummary(),
  observerCount: 0,
  observerItems:
    input.languageTag === 128
      ? emptyMidgardCekDataListSummary()
      : emptyMidgardCekDataPairSummary(),
  previousObserver: Buffer.alloc(0),
  observerSummary: emptyMidgardCekDataSummary(),
  mintCursor: 0,
  currentMintPolicy: Buffer.alloc(0),
  currentMintAssets: emptyMidgardCekDataPairSummary(),
  mintPolicies: emptyMidgardCekDataPairSummary(),
  mintSummary: emptyMidgardCekDataSummary(),
});

export const encodeMidgardCekContextControl = (
  control: MidgardCekContextControl,
): Buffer =>
  encodeCbor([
    BigInt(control.stage),
    BigInt(control.languageTag),
    control.programTermRoot,
    control.programEnvelopeHash,
    BigInt(control.purposeKind),
    control.purposeIndex,
    control.scriptHash,
    control.subject,
    control.redeemerLeaf,
    control.redeemerContextControlHash,
    control.executionMemoryLimit,
    control.executionCpuLimit,
    encodeMidgardCekDataSequenceSummary(control.referenceItems),
    encodeMidgardCekDataSequenceSummary(control.spendItems),
    encodeMidgardCekDataSequenceSummary(control.outputItems),
    encodeMidgardCekDataSequenceSummary(control.signerItems),
    BigInt(control.observerCount),
    encodeMidgardCekDataSequenceSummary(control.observerItems),
    control.previousObserver,
    encodeMidgardCekDataSummary(control.observerSummary),
    BigInt(control.mintCursor),
    control.currentMintPolicy,
    encodeMidgardCekDataSequenceSummary(control.currentMintAssets),
    encodeMidgardCekDataSequenceSummary(control.mintPolicies),
    encodeMidgardCekDataSummary(control.mintSummary),
  ]);

export const encodeMidgardCekValidationWitness = (input: {
  readonly nativeControlCbor: Uint8Array;
  readonly contextControl: MidgardCekContextControl | null;
  readonly executionCursor: number;
  readonly completedCpu: bigint;
  readonly completedMemory: bigint;
  readonly activeStateHash: Uint8Array | null;
  readonly executionCpuLimit: bigint;
  readonly executionMemoryLimit: bigint;
  readonly programEnvelopeHash: Uint8Array | null;
}): Buffer =>
  // The witness must never end with a possibly-empty bytestring: the Aiken
  // `cbor.deserialise` consumer rejects a zero-length final item at an
  // exhausted cursor, so the possibly-empty program envelope hash sits
  // before the two integer limits.
  encodeCbor([
    bytes(input.nativeControlCbor),
    input.contextControl === null
      ? Buffer.alloc(0)
      : encodeMidgardCekContextControl(input.contextControl),
    BigInt(input.executionCursor),
    input.completedCpu,
    input.completedMemory,
    input.activeStateHash === null
      ? Buffer.alloc(0)
      : bytes(input.activeStateHash),
    input.programEnvelopeHash === null
      ? Buffer.alloc(0)
      : bytes(input.programEnvelopeHash),
    input.executionCpuLimit,
    input.executionMemoryLimit,
  ]);

export type MidgardCekDecodedContext = {
  readonly context: PlutusData;
  readonly txInfo: PlutusData;
  readonly redeemer: PlutusData;
  readonly scriptInfo: PlutusData;
  readonly txInfoFields: readonly PlutusData[];
};

/** Decodes the context keeping every map's entry order and duplicate keys. */
export const decodeMidgardCekContext = (
  contextCbor: Uint8Array,
): MidgardCekDecodedContext => {
  const context = plutusDataFromCborIterative(contextCbor);
  if (!(context instanceof DataConstr) || context.constr !== 0n) {
    throw new Error("V1 script context must be constructor 0");
  }
  const contextFields = context.fields;
  if (contextFields.length !== 3) {
    throw new Error("V1 script context must contain three fields");
  }
  const txInfo = contextFields[0]!;
  if (!(txInfo instanceof DataConstr) || txInfo.constr !== 0n) {
    throw new Error("V1 transaction info must be constructor 0");
  }
  return {
    context,
    txInfo,
    redeemer: contextFields[1]!,
    scriptInfo: contextFields[2]!,
    txInfoFields: txInfo.fields,
  };
};

export const summarizeMidgardCekContextParts = (
  decoded: MidgardCekDecodedContext,
  languageTag: 3 | 128,
): {
  readonly context: MidgardCekDataSummary;
  readonly txInfo: MidgardCekDataSummary;
  readonly redeemer: MidgardCekDataSummary;
  readonly scriptInfo: MidgardCekDataSummary;
  readonly spendItems: MidgardCekDataSequenceSummary;
  readonly referenceItems: MidgardCekDataSequenceSummary;
  readonly outputItems: MidgardCekDataSequenceSummary;
  readonly observer: MidgardCekDataSummary;
  readonly signerItems: MidgardCekDataSequenceSummary;
  readonly mint: MidgardCekDataSummary;
  readonly redeemerItems: MidgardCekDataSequenceSummary;
  readonly tailFields: MidgardCekDataSequenceSummary;
} => {
  const fields = decoded.txInfoFields;
  const expected = languageTag === 128 ? 10 : 16;
  if (fields.length !== expected) {
    throw new Error(
      `V1 transaction info has ${fields.length.toString()} fields, expected ${expected.toString()}`,
    );
  }
  const asList = (value: PlutusData, field: string): readonly PlutusData[] => {
    if (!(value instanceof DataList)) {
      throw new Error(`V1 ${field} is not a Data list`);
    }
    return value.list;
  };
  const asMap = (value: PlutusData, field: string): PlutusDataMap => {
    if (!isPlutusDataMap(value)) {
      throw new Error(`V1 ${field} is not a Data map`);
    }
    return value;
  };
  const observerIndex = languageTag === 128 ? 5 : 6;
  const signerIndex = languageTag === 128 ? 6 : 8;
  const mintIndex = languageTag === 128 ? 7 : 4;
  const redeemerIndex = languageTag === 128 ? 8 : 9;
  const tailStart = languageTag === 128 ? 5 : 8;
  return {
    context: summarizeMidgardCekData(decoded.context),
    txInfo: summarizeMidgardCekData(decoded.txInfo),
    redeemer: summarizeMidgardCekData(decoded.redeemer),
    scriptInfo: summarizeMidgardCekData(decoded.scriptInfo),
    spendItems: summarizeMidgardCekDataList(asList(fields[0]!, "spend inputs")),
    referenceItems: summarizeMidgardCekDataList(
      asList(fields[1]!, "reference inputs"),
    ),
    outputItems: summarizeMidgardCekDataList(asList(fields[2]!, "outputs")),
    observer: summarizeMidgardCekData(fields[observerIndex]!),
    signerItems: summarizeMidgardCekDataList(
      asList(fields[signerIndex]!, "signers"),
    ),
    mint: summarizeMidgardCekData(fields[mintIndex]!),
    redeemerItems: summarizeMidgardCekDataPairs(
      asMap(fields[redeemerIndex]!, "redeemers"),
    ),
    tailFields: summarizeMidgardCekDataList(fields.slice(tailStart)),
  };
};

export const composeMidgardCekContextSummary = (
  control: MidgardCekFinalContextControl,
): MidgardCekDataSummary =>
  summarizeMidgardCekSmallConstrData(
    0n,
    [control.txInfo, control.redeemer, control.scriptInfo].reduceRight(
      (tail, field) => prependMidgardCekDataListSummary(field, tail),
      emptyMidgardCekDataListSummary(),
    ),
  );

export const asMidgardCekListSummary = summarizeMidgardCekListData;

export const asMidgardCekMapSummary = summarizeMidgardCekMapData;
