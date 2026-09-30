import { encodeCbor, MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core";
import {
  type Data as PlutusData,
  dataFromCbor,
} from "@harmoniclabs/plutus-data";
import {
  Constr,
  Data,
  type Data as LucidDataValue,
  fromHex,
} from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";

import { commitMidgardCekDataTree } from "./cek-data-tree.js";
import { type PlutusDataMap } from "./plutus-data-narrowing.js";
import {
  emptyMidgardCekDataListSummary,
  emptyMidgardCekDataPairSummary,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataListSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekListData,
  summarizeMidgardCekMapData,
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

export const TX_INFO_ASSEMBLY_DOMAIN = Buffer.from(
  "MidgardCekTxInfoAssemblyControlV1",
  "ascii",
);

const hash32 = (bytes: Uint8Array): Buffer =>
  Buffer.from(blake2b(bytes, { dkLen: 32 }));

export const bytes = (value: Uint8Array): Buffer => Buffer.from(value);

export const requiredHash32 = (field: string, value: Uint8Array): Buffer => {
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

export const summarizeMidgardCekData = (
  value: PlutusData,
): MidgardCekDataSummary => {
  const tree = commitMidgardCekDataTree(value);
  return {
    root: Buffer.from(tree.root),
    cborLength: tree.cborLength,
    memory: tree.memory,
  };
};

/**
 * For map-free Data only: Lucid's encoder sorts maps, so a map summarised
 * here need not be the one the script sees.
 */
export const summarizeMidgardCekLucidData = (
  value: LucidDataValue,
): MidgardCekDataSummary =>
  summarizeMidgardCekData(dataFromCbor(fromHex(Data.to(value))));

export const summarizeMidgardCekDataList = (
  values: readonly PlutusData[],
): MidgardCekDataSequenceSummary => {
  let summary = emptyMidgardCekDataListSummary();
  for (let index = values.length - 1; index >= 0; index -= 1) {
    summary = prependMidgardCekDataListSummary(
      summarizeMidgardCekData(values[index]!),
      summary,
    );
  }
  return summary;
};

export const summarizeMidgardCekDataPairs = (
  value: PlutusDataMap,
): MidgardCekDataSequenceSummary => {
  let summary = emptyMidgardCekDataPairSummary();
  for (let index = value.map.length - 1; index >= 0; index -= 1) {
    const entry = value.map[index]!;
    summary = prependMidgardCekDataPairSummary(
      summarizeMidgardCekData(entry.fst),
      summarizeMidgardCekData(entry.snd),
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

/**
 * The redeemer map is folded down the execution frontier. `purposeBound`
 * starts at the purpose count. Each select or skip step proves the execution
 * leaf at exactly `purposeBound - 1` against the native control's execution
 * frontier; that membership (`[execution-leaf]` / `[native-execution]`) is the
 * pin, so the frontier index is derived and never named by the witness. The
 * step is a select (auxiliary constructor 17) when the leaf carries a redeemer
 * item leaf (language 3 or 128), and a skip (constructor 40) when it is a
 * native execution (language 0, empty redeemer leaf); only one can apply at a
 * given bound. Each select or skip lowers the bound by exactly one. The item
 * steps between a select and its finish walk the selected item under
 * `activeScanHash` without moving the bound. The select's
 * `totalLength` is fixed by the item commitment through
 * `commitment_from_frontier_root` (`[item-length]`). The fold completes at
 * `cursor === redeemerCount`. Pairs are prepended, so the finished map
 * follows the frontier ascending, which is Cardano's (tag, index) ledger
 * order. On chain the monolithic verifier is `verify_cek_redeemer_data_step`
 * in `cek.ak`; the split chain is `cek-context-redeemer-select-authenticate`,
 * then `-select-initialize`, `-select-hash` and `-select-finish` for a
 * select, and select-authenticate routes a skip straight to
 * `cek-context-settle`.
 */
export type MidgardCekRedeemerContextControl = {
  readonly cursor: number;
  readonly mapItems: MidgardCekDataSequenceSummary;
  readonly activeScanHash: Buffer;
  readonly activeRedeemerLeaf: Buffer;
  readonly activePurpose: MidgardCekDataSummary;
  readonly currentRedeemer: MidgardCekDataSummary;
  readonly purposeBound: number;
};

export const initialMidgardCekRedeemerContextControl = (
  purposeCount: number,
): MidgardCekRedeemerContextControl => ({
  cursor: 0,
  mapItems: emptyMidgardCekDataPairSummary(),
  activeScanHash: Buffer.alloc(0),
  activeRedeemerLeaf: Buffer.alloc(0),
  activePurpose: emptyMidgardCekDataSummary(),
  currentRedeemer: emptyMidgardCekDataSummary(),
  purposeBound: purposeCount,
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
    BigInt(control.purposeBound),
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
