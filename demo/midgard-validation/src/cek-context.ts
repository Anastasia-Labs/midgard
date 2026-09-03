import { encodeCbor, MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import {
  Constr,
  Data,
  type Data as LucidDataValue,
  fromHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
  summarizeMidgardCekSmallConstrData,
} from "./script-context-proof.js";

const REDEEMER_CONTEXT_DOMAIN = Buffer.from(
  "MidgardCekRedeemerContextControlV1",
  "ascii",
);
const FINAL_CONTEXT_DOMAIN = Buffer.from(
  "MidgardCekFinalContextControlV1",
  "ascii",
);
const CONTEXT_PARTS_DOMAIN = Buffer.from(
  "MidgardCekContextPartsControlV1",
  "ascii",
);
const TX_INFO_ASSEMBLY_DOMAIN = Buffer.from(
  "MidgardCekTxInfoAssemblyControlV1",
  "ascii",
);

const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

const bytes = (value: Uint8Array): Buffer => Buffer.from(value);

const requiredHash32 = (field: string, value: Uint8Array): Buffer => {
  const exact = bytes(value);
  if (exact.length !== 32) {
    throw new Error(`${field} must be exactly 32 bytes`);
  }
  return exact;
};

export const emptyMidgardCekDataSummary = (): MidgardCekDataSummary => ({
  root: Buffer.alloc(0),
  cborLength: 0n,
  memory: 0n,
});

export const encodeMidgardCekDataSummary = (
  summary: MidgardCekDataSummary,
): readonly [Buffer, bigint, bigint] => [
  bytes(summary.root),
  summary.cborLength,
  summary.memory,
];

export const encodeMidgardCekDataSequenceSummary = (
  summary: MidgardCekDataSequenceSummary,
): readonly [Buffer, bigint, bigint, bigint] => [
  bytes(summary.root),
  summary.length,
  summary.payloadCborLength,
  summary.memory,
];

export const summarizeMidgardCekLucidData = (
  value: LucidDataValue,
): MidgardCekDataSummary => {
  const cbor = fromHex(Data.to(value));
  const tree = commitMidgardCekDataTree(dataFromCbor(cbor));
  return {
    root: Buffer.from(tree.root),
    cborLength: tree.cborLength,
    memory: tree.memory,
  };
};

export const summarizeMidgardCekLucidList = (
  values: readonly LucidDataValue[],
): MidgardCekDataSequenceSummary => {
  let summary = emptyMidgardCekDataListSummary();
  for (let index = values.length - 1; index >= 0; index -= 1) {
    summary = prependMidgardCekDataListSummary(
      summarizeMidgardCekLucidData(values[index]!),
      summary,
    );
  }
  return summary;
};

export const summarizeMidgardCekLucidMap = (
  value: ReadonlyMap<LucidDataValue, LucidDataValue>,
): MidgardCekDataSequenceSummary => {
  const entries = [...value.entries()];
  let summary = emptyMidgardCekDataPairSummary();
  for (let index = entries.length - 1; index >= 0; index -= 1) {
    const [key, mapped] = entries[index]!;
    summary = prependMidgardCekDataPairSummary(
      summarizeMidgardCekLucidData(key),
      summarizeMidgardCekLucidData(mapped),
      summary,
    );
  }
  return summary;
};

export const validateMidgardCekObserverCollection = (
  observers: readonly Uint8Array[],
): void => {
  if (observers.length > MIDGARD_CONSENSUS_LIMITS.maxRequiredObserverCount) {
    throw new Error(
      "CEK observer context exceeds the transaction-size-derived collection guardrail",
    );
  }
  let previous = Buffer.alloc(0);
  for (const value of observers) {
    const observer = bytes(value);
    if (observer.length !== 28) {
      throw new Error("CEK observer hash must be exactly 28 bytes");
    }
    if (previous.length > 0 && Buffer.compare(previous, observer) >= 0) {
      throw new Error(
        "CEK observer context must be strictly ordered and unique",
      );
    }
    previous = observer;
  }
};

export const prependMidgardCekObserverItem = (input: {
  readonly observerHash: Uint8Array;
  readonly midgardEncoding: boolean;
  readonly tail: MidgardCekDataSequenceSummary;
}): MidgardCekDataSequenceSummary => {
  const observerHash = bytes(input.observerHash);
  if (observerHash.length !== 28) {
    throw new Error("CEK observer hash must be exactly 28 bytes");
  }
  if (input.midgardEncoding) {
    return prependMidgardCekDataListSummary(
      summarizeMidgardCekLucidData(observerHash.toString("hex")),
      input.tail,
    );
  }
  return prependMidgardCekDataPairSummary(
    summarizeMidgardCekLucidData(new Constr(1, [observerHash.toString("hex")])),
    summarizeMidgardCekLucidData(0n),
    input.tail,
  );
};

export const finalizeMidgardCekObserverItems = (input: {
  readonly items: MidgardCekDataSequenceSummary;
  readonly midgardEncoding: boolean;
}): MidgardCekDataSummary =>
  input.midgardEncoding
    ? summarizeMidgardCekListData(input.items)
    : summarizeMidgardCekMapData(input.items);

export type MidgardCekRedeemerContextControl = {
  readonly cursor: number;
  readonly mapItems: MidgardCekDataSequenceSummary;
  readonly activeScanHash: Buffer;
  readonly activeRedeemerLeaf: Buffer;
  readonly activePurpose: MidgardCekDataSummary;
  readonly currentRedeemer: MidgardCekDataSummary;
};

export const initialMidgardCekRedeemerContextControl =
  (): MidgardCekRedeemerContextControl => ({
    cursor: 0,
    mapItems: emptyMidgardCekDataPairSummary(),
    activeScanHash: Buffer.alloc(0),
    activeRedeemerLeaf: Buffer.alloc(0),
    activePurpose: emptyMidgardCekDataSummary(),
    currentRedeemer: emptyMidgardCekDataSummary(),
  });

export const encodeMidgardCekRedeemerContextControl = (
  control: MidgardCekRedeemerContextControl,
): Buffer =>
  encodeCbor([
    BigInt(control.cursor),
    encodeMidgardCekDataSequenceSummary(control.mapItems),
    control.activeScanHash,
    control.activeRedeemerLeaf,
    encodeMidgardCekDataSummary(control.activePurpose),
    encodeMidgardCekDataSummary(control.currentRedeemer),
  ]);

export const hashMidgardCekRedeemerContextControl = (
  control: MidgardCekRedeemerContextControl,
): Buffer =>
  hash32(
    Buffer.concat([
      REDEEMER_CONTEXT_DOMAIN,
      encodeMidgardCekRedeemerContextControl(control),
    ]),
  );

export type MidgardCekFinalContextControl = {
  readonly txInfo: MidgardCekDataSummary;
  readonly redeemer: MidgardCekDataSummary;
  readonly scriptInfo: MidgardCekDataSummary;
};

export type MidgardCekContextPartsControl = {
  readonly redeemerItems: MidgardCekDataSequenceSummary;
  readonly redeemer: MidgardCekDataSummary;
  readonly scriptInfo: MidgardCekDataSummary;
};

export type MidgardCekTxInfoAssemblyControl = {
  readonly tailFields: MidgardCekDataSequenceSummary;
  readonly redeemer: MidgardCekDataSummary;
  readonly scriptInfo: MidgardCekDataSummary;
};

const encodeSummaryTriple = (control: {
  readonly redeemer: MidgardCekDataSummary;
  readonly scriptInfo: MidgardCekDataSummary;
  readonly txInfo?: MidgardCekDataSummary;
  readonly redeemerItems?: MidgardCekDataSequenceSummary;
  readonly tailFields?: MidgardCekDataSequenceSummary;
}): Buffer =>
  encodeCbor([
    control.txInfo !== undefined
      ? encodeMidgardCekDataSummary(control.txInfo)
      : control.redeemerItems !== undefined
        ? encodeMidgardCekDataSequenceSummary(control.redeemerItems)
        : encodeMidgardCekDataSequenceSummary(control.tailFields!),
    encodeMidgardCekDataSummary(control.redeemer),
    encodeMidgardCekDataSummary(control.scriptInfo),
  ]);

export const encodeMidgardCekFinalContextControl = (
  control: MidgardCekFinalContextControl,
): Buffer => encodeSummaryTriple(control);

export const hashMidgardCekFinalContextControl = (
  control: MidgardCekFinalContextControl,
): Buffer =>
  hash32(
    Buffer.concat([
      FINAL_CONTEXT_DOMAIN,
      encodeMidgardCekFinalContextControl(control),
    ]),
  );

export const encodeMidgardCekContextPartsControl = (
  control: MidgardCekContextPartsControl,
): Buffer => encodeSummaryTriple(control);

export const hashMidgardCekContextPartsControl = (
  control: MidgardCekContextPartsControl,
): Buffer =>
  hash32(
    Buffer.concat([
      CONTEXT_PARTS_DOMAIN,
      encodeMidgardCekContextPartsControl(control),
    ]),
  );

export const encodeMidgardCekTxInfoAssemblyControl = (
  control: MidgardCekTxInfoAssemblyControl,
): Buffer => encodeSummaryTriple(control);

export const hashMidgardCekTxInfoAssemblyControl = (
  control: MidgardCekTxInfoAssemblyControl,
): Buffer =>
  hash32(
    Buffer.concat([
      TX_INFO_ASSEMBLY_DOMAIN,
      encodeMidgardCekTxInfoAssemblyControl(control),
    ]),
  );

export type MidgardCekContextControl = {
  readonly stage: number;
  readonly languageTag: 3 | 128;
  readonly programTermRoot: Buffer;
  readonly programEnvelopeHash: Buffer;
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Buffer;
  readonly subject: Buffer;
  readonly redeemerLeaf: Buffer;
  readonly redeemerContextControlHash: Buffer;
  readonly executionMemoryLimit: bigint;
  readonly executionCpuLimit: bigint;
  readonly referenceItems: MidgardCekDataSequenceSummary;
  readonly spendItems: MidgardCekDataSequenceSummary;
  readonly outputItems: MidgardCekDataSequenceSummary;
  readonly signerItems: MidgardCekDataSequenceSummary;
  readonly observerCount: number;
  readonly observerItems: MidgardCekDataSequenceSummary;
  readonly previousObserver: Buffer;
  readonly observerSummary: MidgardCekDataSummary;
  readonly mintCursor: number;
  readonly currentMintPolicy: Buffer;
  readonly currentMintAssets: MidgardCekDataSequenceSummary;
  readonly mintPolicies: MidgardCekDataSequenceSummary;
  readonly mintSummary: MidgardCekDataSummary;
};

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
  readonly context: LucidDataValue;
  readonly txInfo: LucidDataValue;
  readonly redeemer: LucidDataValue;
  readonly scriptInfo: LucidDataValue;
  readonly txInfoFields: readonly LucidDataValue[];
};

export const decodeMidgardCekContext = (
  contextCbor: Uint8Array,
): MidgardCekDecodedContext => {
  const context = Data.from(Buffer.from(contextCbor).toString("hex"));
  if (!(context instanceof Constr) || context.index !== 0) {
    throw new Error("V1 script context must be constructor 0");
  }
  const contextFields = context.fields as readonly LucidDataValue[];
  if (contextFields.length !== 3) {
    throw new Error("V1 script context must contain three fields");
  }
  const txInfo = contextFields[0]!;
  if (!(txInfo instanceof Constr) || txInfo.index !== 0) {
    throw new Error("V1 transaction info must be constructor 0");
  }
  return {
    context,
    txInfo,
    redeemer: contextFields[1]!,
    scriptInfo: contextFields[2]!,
    txInfoFields: txInfo.fields as readonly LucidDataValue[],
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
  const asList = (
    value: LucidDataValue,
    field: string,
  ): readonly LucidDataValue[] => {
    if (!Array.isArray(value)) {
      throw new Error(`V1 ${field} is not a Data list`);
    }
    return value;
  };
  const asMap = (
    value: LucidDataValue,
    field: string,
  ): ReadonlyMap<LucidDataValue, LucidDataValue> => {
    if (!(value instanceof Map)) {
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
    context: summarizeMidgardCekLucidData(decoded.context),
    txInfo: summarizeMidgardCekLucidData(decoded.txInfo),
    redeemer: summarizeMidgardCekLucidData(decoded.redeemer),
    scriptInfo: summarizeMidgardCekLucidData(decoded.scriptInfo),
    spendItems: summarizeMidgardCekLucidList(
      asList(fields[0]!, "spend inputs"),
    ),
    referenceItems: summarizeMidgardCekLucidList(
      asList(fields[1]!, "reference inputs"),
    ),
    outputItems: summarizeMidgardCekLucidList(asList(fields[2]!, "outputs")),
    observer: summarizeMidgardCekLucidData(fields[observerIndex]!),
    signerItems: summarizeMidgardCekLucidList(
      asList(fields[signerIndex]!, "signers"),
    ),
    mint: summarizeMidgardCekLucidData(fields[mintIndex]!),
    redeemerItems: summarizeMidgardCekLucidMap(
      asMap(fields[redeemerIndex]!, "redeemers"),
    ),
    tailFields: summarizeMidgardCekLucidList(fields.slice(tailStart)),
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
