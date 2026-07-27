import { encodeCbor } from "@al-ft/midgard-core";
import { dataFromCbor } from "@harmoniclabs/plutus-data";
import {
  Constr,
  Data,
  type Data as LucidDataValue,
  fromHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import {
  commitMidgardCekDataTreeV1,
} from "./cek-data-tree.js";
import {
  emptyMidgardCekDataListSummaryV1,
  emptyMidgardCekDataPairSummaryV1,
  type MidgardCekDataSequenceSummaryV1,
  type MidgardCekDataSummaryV1,
  prependMidgardCekDataListSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekListDataV1,
  summarizeMidgardCekMapDataV1,
  summarizeMidgardCekSmallConstrDataV1,
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

export const emptyMidgardCekDataSummaryV1 =
  (): MidgardCekDataSummaryV1 => ({
    root: Buffer.alloc(0),
    cborLength: 0n,
    memory: 0n,
  });

export const encodeMidgardCekDataSummaryV1 = (
  summary: MidgardCekDataSummaryV1,
): readonly [Buffer, bigint, bigint] => [
  bytes(summary.root),
  summary.cborLength,
  summary.memory,
];

export const encodeMidgardCekDataSequenceSummaryV1 = (
  summary: MidgardCekDataSequenceSummaryV1,
): readonly [Buffer, bigint, bigint, bigint] => [
  bytes(summary.root),
  summary.length,
  summary.payloadCborLength,
  summary.memory,
];

export const summarizeMidgardCekLucidDataV1 = (
  value: LucidDataValue,
): MidgardCekDataSummaryV1 => {
  const cbor = fromHex(Data.to(value));
  const tree = commitMidgardCekDataTreeV1(dataFromCbor(cbor));
  return {
    root: Buffer.from(tree.root),
    cborLength: tree.cborLength,
    memory: tree.memory,
  };
};

export const summarizeMidgardCekLucidListV1 = (
  values: readonly LucidDataValue[],
): MidgardCekDataSequenceSummaryV1 => {
  let summary = emptyMidgardCekDataListSummaryV1();
  for (let index = values.length - 1; index >= 0; index -= 1) {
    summary = prependMidgardCekDataListSummaryV1(
      summarizeMidgardCekLucidDataV1(values[index]!),
      summary,
    );
  }
  return summary;
};

export const summarizeMidgardCekLucidMapV1 = (
  value: ReadonlyMap<LucidDataValue, LucidDataValue>,
): MidgardCekDataSequenceSummaryV1 => {
  const entries = [...value.entries()];
  let summary = emptyMidgardCekDataPairSummaryV1();
  for (let index = entries.length - 1; index >= 0; index -= 1) {
    const [key, mapped] = entries[index]!;
    summary = prependMidgardCekDataPairSummaryV1(
      summarizeMidgardCekLucidDataV1(key),
      summarizeMidgardCekLucidDataV1(mapped),
      summary,
    );
  }
  return summary;
};

export const MIDGARD_CEK_MAX_OBSERVER_COUNT_V1 = 16;

export const validateMidgardCekObserverCollectionV1 = (
  observers: readonly Uint8Array[],
): void => {
  if (observers.length > MIDGARD_CEK_MAX_OBSERVER_COUNT_V1) {
    throw new Error(
      `CEK observer context exceeds the semantic maximum of ${MIDGARD_CEK_MAX_OBSERVER_COUNT_V1}`,
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

export const prependMidgardCekObserverItemV1 = (input: {
  readonly observerHash: Uint8Array;
  readonly midgardEncoding: boolean;
  readonly tail: MidgardCekDataSequenceSummaryV1;
}): MidgardCekDataSequenceSummaryV1 => {
  const observerHash = bytes(input.observerHash);
  if (observerHash.length !== 28) {
    throw new Error("CEK observer hash must be exactly 28 bytes");
  }
  if (input.midgardEncoding) {
    return prependMidgardCekDataListSummaryV1(
      summarizeMidgardCekLucidDataV1(observerHash.toString("hex")),
      input.tail,
    );
  }
  return prependMidgardCekDataPairSummaryV1(
    summarizeMidgardCekLucidDataV1(
      new Constr(1, [observerHash.toString("hex")]),
    ),
    summarizeMidgardCekLucidDataV1(0n),
    input.tail,
  );
};

export const finalizeMidgardCekObserverItemsV1 = (input: {
  readonly items: MidgardCekDataSequenceSummaryV1;
  readonly midgardEncoding: boolean;
}): MidgardCekDataSummaryV1 =>
  input.midgardEncoding
    ? summarizeMidgardCekListDataV1(input.items)
    : summarizeMidgardCekMapDataV1(input.items);

export type MidgardCekRedeemerContextControlV1 = {
  readonly cursor: number;
  readonly mapItems: MidgardCekDataSequenceSummaryV1;
  readonly activeScanHash: Buffer;
  readonly activeRedeemerLeaf: Buffer;
  readonly activePurpose: MidgardCekDataSummaryV1;
  readonly currentRedeemer: MidgardCekDataSummaryV1;
};

export const initialMidgardCekRedeemerContextControlV1 =
  (): MidgardCekRedeemerContextControlV1 => ({
    cursor: 0,
    mapItems: emptyMidgardCekDataPairSummaryV1(),
    activeScanHash: Buffer.alloc(0),
    activeRedeemerLeaf: Buffer.alloc(0),
    activePurpose: emptyMidgardCekDataSummaryV1(),
    currentRedeemer: emptyMidgardCekDataSummaryV1(),
  });

export const encodeMidgardCekRedeemerContextControlV1 = (
  control: MidgardCekRedeemerContextControlV1,
): Buffer =>
  encodeCbor([
    BigInt(control.cursor),
    encodeMidgardCekDataSequenceSummaryV1(control.mapItems),
    control.activeScanHash,
    control.activeRedeemerLeaf,
    encodeMidgardCekDataSummaryV1(control.activePurpose),
    encodeMidgardCekDataSummaryV1(control.currentRedeemer),
  ]);

export const hashMidgardCekRedeemerContextControlV1 = (
  control: MidgardCekRedeemerContextControlV1,
): Buffer =>
  hash32(
    Buffer.concat([
      REDEEMER_CONTEXT_DOMAIN,
      encodeMidgardCekRedeemerContextControlV1(control),
    ]),
  );

export type MidgardCekFinalContextControlV1 = {
  readonly txInfo: MidgardCekDataSummaryV1;
  readonly redeemer: MidgardCekDataSummaryV1;
  readonly scriptInfo: MidgardCekDataSummaryV1;
};

export type MidgardCekContextPartsControlV1 = {
  readonly redeemerItems: MidgardCekDataSequenceSummaryV1;
  readonly redeemer: MidgardCekDataSummaryV1;
  readonly scriptInfo: MidgardCekDataSummaryV1;
};

export type MidgardCekTxInfoAssemblyControlV1 = {
  readonly tailFields: MidgardCekDataSequenceSummaryV1;
  readonly redeemer: MidgardCekDataSummaryV1;
  readonly scriptInfo: MidgardCekDataSummaryV1;
};

const encodeSummaryTriple = (control: {
  readonly redeemer: MidgardCekDataSummaryV1;
  readonly scriptInfo: MidgardCekDataSummaryV1;
  readonly txInfo?: MidgardCekDataSummaryV1;
  readonly redeemerItems?: MidgardCekDataSequenceSummaryV1;
  readonly tailFields?: MidgardCekDataSequenceSummaryV1;
}): Buffer =>
  encodeCbor([
    control.txInfo !== undefined
      ? encodeMidgardCekDataSummaryV1(control.txInfo)
      : control.redeemerItems !== undefined
        ? encodeMidgardCekDataSequenceSummaryV1(control.redeemerItems)
        : encodeMidgardCekDataSequenceSummaryV1(control.tailFields!),
    encodeMidgardCekDataSummaryV1(control.redeemer),
    encodeMidgardCekDataSummaryV1(control.scriptInfo),
  ]);

export const encodeMidgardCekFinalContextControlV1 = (
  control: MidgardCekFinalContextControlV1,
): Buffer => encodeSummaryTriple(control);

export const hashMidgardCekFinalContextControlV1 = (
  control: MidgardCekFinalContextControlV1,
): Buffer =>
  hash32(
    Buffer.concat([
      FINAL_CONTEXT_DOMAIN,
      encodeMidgardCekFinalContextControlV1(control),
    ]),
  );

export const encodeMidgardCekContextPartsControlV1 = (
  control: MidgardCekContextPartsControlV1,
): Buffer => encodeSummaryTriple(control);

export const hashMidgardCekContextPartsControlV1 = (
  control: MidgardCekContextPartsControlV1,
): Buffer =>
  hash32(
    Buffer.concat([
      CONTEXT_PARTS_DOMAIN,
      encodeMidgardCekContextPartsControlV1(control),
    ]),
  );

export const encodeMidgardCekTxInfoAssemblyControlV1 = (
  control: MidgardCekTxInfoAssemblyControlV1,
): Buffer => encodeSummaryTriple(control);

export const hashMidgardCekTxInfoAssemblyControlV1 = (
  control: MidgardCekTxInfoAssemblyControlV1,
): Buffer =>
  hash32(
    Buffer.concat([
      TX_INFO_ASSEMBLY_DOMAIN,
      encodeMidgardCekTxInfoAssemblyControlV1(control),
    ]),
  );

export type MidgardCekContextControlV1 = {
  readonly stage: number;
  readonly languageTag: 3 | 128;
  readonly programTermRoot: Buffer;
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Buffer;
  readonly subject: Buffer;
  readonly redeemerLeaf: Buffer;
  readonly redeemerContextControlHash: Buffer;
  readonly executionMemoryLimit: bigint;
  readonly executionCpuLimit: bigint;
  readonly referenceItems: MidgardCekDataSequenceSummaryV1;
  readonly spendItems: MidgardCekDataSequenceSummaryV1;
  readonly outputItems: MidgardCekDataSequenceSummaryV1;
  readonly signerItems: MidgardCekDataSequenceSummaryV1;
  readonly observerCount: number;
  readonly observerItems: MidgardCekDataSequenceSummaryV1;
  readonly previousObserver: Buffer;
  readonly observerSummary: MidgardCekDataSummaryV1;
  readonly mintCursor: number;
  readonly currentMintPolicy: Buffer;
  readonly currentMintAssets: MidgardCekDataSequenceSummaryV1;
  readonly mintPolicies: MidgardCekDataSequenceSummaryV1;
  readonly mintSummary: MidgardCekDataSummaryV1;
};

export const initialMidgardCekContextControlV1 = (input: {
  readonly languageTag: 3 | 128;
  readonly programTermRoot: Uint8Array;
  readonly purposeKind: 0 | 1 | 2 | 3;
  readonly purposeIndex: bigint;
  readonly scriptHash: Uint8Array;
  readonly subject: Uint8Array;
  readonly redeemerLeaf: Uint8Array;
}): MidgardCekContextControlV1 => ({
  stage: 0,
  languageTag: input.languageTag,
  programTermRoot: bytes(input.programTermRoot),
  purposeKind: input.purposeKind,
  purposeIndex: input.purposeIndex,
  scriptHash: bytes(input.scriptHash),
  subject: bytes(input.subject),
  redeemerLeaf: bytes(input.redeemerLeaf),
  redeemerContextControlHash: Buffer.alloc(0),
  executionMemoryLimit: 0n,
  executionCpuLimit: 0n,
  referenceItems: emptyMidgardCekDataListSummaryV1(),
  spendItems: emptyMidgardCekDataListSummaryV1(),
  outputItems: emptyMidgardCekDataListSummaryV1(),
  signerItems: emptyMidgardCekDataListSummaryV1(),
  observerCount: 0,
  observerItems:
    input.languageTag === 128
      ? emptyMidgardCekDataListSummaryV1()
      : emptyMidgardCekDataPairSummaryV1(),
  previousObserver: Buffer.alloc(0),
  observerSummary: emptyMidgardCekDataSummaryV1(),
  mintCursor: 0,
  currentMintPolicy: Buffer.alloc(0),
  currentMintAssets: emptyMidgardCekDataPairSummaryV1(),
  mintPolicies: emptyMidgardCekDataPairSummaryV1(),
  mintSummary: emptyMidgardCekDataSummaryV1(),
});

export const encodeMidgardCekContextControlV1 = (
  control: MidgardCekContextControlV1,
): Buffer =>
  encodeCbor([
    BigInt(control.stage),
    BigInt(control.languageTag),
    control.programTermRoot,
    BigInt(control.purposeKind),
    control.purposeIndex,
    control.scriptHash,
    control.subject,
    control.redeemerLeaf,
    control.redeemerContextControlHash,
    control.executionMemoryLimit,
    control.executionCpuLimit,
    encodeMidgardCekDataSequenceSummaryV1(control.referenceItems),
    encodeMidgardCekDataSequenceSummaryV1(control.spendItems),
    encodeMidgardCekDataSequenceSummaryV1(control.outputItems),
    encodeMidgardCekDataSequenceSummaryV1(control.signerItems),
    BigInt(control.observerCount),
    encodeMidgardCekDataSequenceSummaryV1(control.observerItems),
    control.previousObserver,
    encodeMidgardCekDataSummaryV1(control.observerSummary),
    BigInt(control.mintCursor),
    control.currentMintPolicy,
    encodeMidgardCekDataSequenceSummaryV1(control.currentMintAssets),
    encodeMidgardCekDataSequenceSummaryV1(control.mintPolicies),
    encodeMidgardCekDataSummaryV1(control.mintSummary),
  ]);

export const encodeMidgardCekValidationWitnessV1 = (input: {
  readonly nativeControlCbor: Uint8Array;
  readonly contextControl?: MidgardCekContextControlV1;
  readonly executionCursor: number;
  readonly completedCpu: bigint;
  readonly completedMemory: bigint;
  readonly activeStateHash?: Uint8Array;
  readonly executionCpuLimit?: bigint;
  readonly executionMemoryLimit?: bigint;
}): Buffer =>
  encodeCbor([
    bytes(input.nativeControlCbor),
    input.contextControl === undefined
      ? Buffer.alloc(0)
      : encodeMidgardCekContextControlV1(input.contextControl),
    BigInt(input.executionCursor),
    input.completedCpu,
    input.completedMemory,
    input.activeStateHash === undefined
      ? Buffer.alloc(0)
      : bytes(input.activeStateHash),
    input.executionCpuLimit ?? 0n,
    input.executionMemoryLimit ?? 0n,
  ]);

export type MidgardCekDecodedContextV1 = {
  readonly context: LucidDataValue;
  readonly txInfo: LucidDataValue;
  readonly redeemer: LucidDataValue;
  readonly scriptInfo: LucidDataValue;
  readonly txInfoFields: readonly LucidDataValue[];
};

export const decodeMidgardCekContextV1 = (
  contextCbor: Uint8Array,
): MidgardCekDecodedContextV1 => {
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

export const summarizeMidgardCekContextPartsV1 = (
  decoded: MidgardCekDecodedContextV1,
  languageTag: 3 | 128,
): {
  readonly context: MidgardCekDataSummaryV1;
  readonly txInfo: MidgardCekDataSummaryV1;
  readonly redeemer: MidgardCekDataSummaryV1;
  readonly scriptInfo: MidgardCekDataSummaryV1;
  readonly spendItems: MidgardCekDataSequenceSummaryV1;
  readonly referenceItems: MidgardCekDataSequenceSummaryV1;
  readonly outputItems: MidgardCekDataSequenceSummaryV1;
  readonly observer: MidgardCekDataSummaryV1;
  readonly signerItems: MidgardCekDataSequenceSummaryV1;
  readonly mint: MidgardCekDataSummaryV1;
  readonly redeemerItems: MidgardCekDataSequenceSummaryV1;
  readonly tailFields: MidgardCekDataSequenceSummaryV1;
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
    context: summarizeMidgardCekLucidDataV1(decoded.context),
    txInfo: summarizeMidgardCekLucidDataV1(decoded.txInfo),
    redeemer: summarizeMidgardCekLucidDataV1(decoded.redeemer),
    scriptInfo: summarizeMidgardCekLucidDataV1(decoded.scriptInfo),
    spendItems: summarizeMidgardCekLucidListV1(
      asList(fields[0]!, "spend inputs"),
    ),
    referenceItems: summarizeMidgardCekLucidListV1(
      asList(fields[1]!, "reference inputs"),
    ),
    outputItems: summarizeMidgardCekLucidListV1(
      asList(fields[2]!, "outputs"),
    ),
    observer: summarizeMidgardCekLucidDataV1(fields[observerIndex]!),
    signerItems: summarizeMidgardCekLucidListV1(
      asList(fields[signerIndex]!, "signers"),
    ),
    mint: summarizeMidgardCekLucidDataV1(fields[mintIndex]!),
    redeemerItems: summarizeMidgardCekLucidMapV1(
      asMap(fields[redeemerIndex]!, "redeemers"),
    ),
    tailFields: summarizeMidgardCekLucidListV1(
      fields.slice(tailStart),
    ),
  };
};

export const composeMidgardCekContextSummaryV1 = (
  control: MidgardCekFinalContextControlV1,
): MidgardCekDataSummaryV1 =>
  summarizeMidgardCekSmallConstrDataV1(
    0n,
    [
      control.txInfo,
      control.redeemer,
      control.scriptInfo,
    ].reduceRight(
      (tail, field) =>
        prependMidgardCekDataListSummaryV1(field, tail),
      emptyMidgardCekDataListSummaryV1(),
    ),
  );

export const asMidgardCekListSummaryV1 =
  summarizeMidgardCekListDataV1;
export const asMidgardCekMapSummaryV1 =
  summarizeMidgardCekMapDataV1;
