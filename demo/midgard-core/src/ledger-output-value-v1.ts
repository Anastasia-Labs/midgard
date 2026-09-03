import { commitMidgardCekBlob } from "./cek-proof.js";
import {
  emptyMidgardCekDataPairSummary,
  hashMidgardCekDataNode,
  midgardCekDataBytesCborLength,
  midgardCekDataBytesMemory,
  type MidgardCekDataSequenceSummary,
  type MidgardCekDataSummary,
  prependMidgardCekDataPairSummary,
  summarizeMidgardCekMapData,
} from "./cek-semantic.js";
import { encodeCbor, encodeCborArrayRaw } from "./codec/cbor.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  buildMidgardLedgerOutputAssetFrontier,
  hashMidgardLedgerOutputAssetLeaf,
  type MidgardLedgerOutputAsset,
} from "./ledger-output-commitment-v1.js";
import {
  buildMidgardValidationMerkleMembership,
  type MidgardValidationMerkleFrontier,
  verifyMidgardValidationMerkleMembership,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_VALUE_VERSION = 1 as const;

export const MidgardLedgerOutputValueStages = Object.freeze({
  Assets: 0,
  Finalize: 1,
  Terminal: 2,
} as const);

export type MidgardLedgerOutputValueStage =
  (typeof MidgardLedgerOutputValueStages)[keyof typeof MidgardLedgerOutputValueStages];

export type MidgardLedgerOutputValueControl = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_VALUE_VERSION;
  readonly stage: MidgardLedgerOutputValueStage;
  readonly assetRemaining: number;
  readonly currentPolicy: Buffer;
  readonly currentAssets: MidgardCekDataSequenceSummary;
  readonly valueEntries: MidgardCekDataSequenceSummary;
  readonly result: MidgardCekDataSummary | null;
};

export type MidgardLedgerOutputValueWitness = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
  readonly siblings: readonly Uint8Array[];
};

export type MidgardLedgerOutputValueTraceStep = {
  readonly control: MidgardLedgerOutputValueControl;
  readonly witness: MidgardLedgerOutputValueWitness | null;
  readonly next: MidgardLedgerOutputValueControl;
};

export type MidgardLedgerOutputValueTrace = {
  readonly assets: readonly MidgardLedgerOutputAsset[];
  readonly frontier: MidgardValidationMerkleFrontier;
  readonly initial: MidgardLedgerOutputValueControl;
  readonly steps: readonly MidgardLedgerOutputValueTraceStep[];
  readonly terminal: MidgardLedgerOutputValueControl;
};

const UINT32_MAX = 0xffff_ffff;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

const exactUint32 = (value: number, field: string): number => {
  if (!Number.isSafeInteger(value) || value < 0 || value > UINT32_MAX) {
    throw new Error(`Invalid V1 ledger output Value ${field}`);
  }
  return value;
};

const exactUint64 = (value: bigint, field: string): bigint => {
  if (value < 0n || value > UINT64_MAX) {
    throw new Error(`Invalid V1 ledger output Value ${field}`);
  }
  return value;
};

const exactSequence = (
  summary: MidgardCekDataSequenceSummary,
  field: string,
): MidgardCekDataSequenceSummary => ({
  root: ensureHash32(summary.root, `${field}.root`),
  length: BigInt(exactUint32(Number(summary.length), `${field}.length`)),
  payloadCborLength: exactUint64(
    summary.payloadCborLength,
    `${field}.payload_cbor_length`,
  ),
  memory: exactUint64(summary.memory, `${field}.memory`),
});

const exactSummary = (
  summary: MidgardCekDataSummary,
  field: string,
): MidgardCekDataSummary => ({
  root: ensureHash32(summary.root, `${field}.root`),
  cborLength: exactUint64(summary.cborLength, `${field}.cbor_length`),
  memory: exactUint64(summary.memory, `${field}.memory`),
});

const encodeSequence = (
  summary: MidgardCekDataSequenceSummary,
  field: string,
): Buffer => {
  const exact = exactSequence(summary, field);
  return encodeCbor([
    exact.root,
    exact.length,
    exact.payloadCborLength,
    exact.memory,
  ]);
};

const encodeOptionalSummary = (
  summary: MidgardCekDataSummary | null,
): Buffer => {
  if (summary === null) return Buffer.from("d87a80", "hex");
  const exact = exactSummary(summary, "result");
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    encodeCbor([exact.root, exact.cborLength, exact.memory]),
    Buffer.from([0xff]),
  ]);
};

const isEmptyPairSequence = (
  summary: MidgardCekDataSequenceSummary,
): boolean => {
  const empty = emptyMidgardCekDataPairSummary();
  return (
    summary.length === 0n &&
    summary.payloadCborLength === 0n &&
    summary.memory === 0n &&
    Buffer.from(summary.root).equals(Buffer.from(empty.root))
  );
};

export const isWellFormedMidgardLedgerOutputValueControl = (
  control: MidgardLedgerOutputValueControl,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_LEDGER_OUTPUT_VALUE_VERSION ||
      !Number.isSafeInteger(control.stage) ||
      control.stage < MidgardLedgerOutputValueStages.Assets ||
      control.stage > MidgardLedgerOutputValueStages.Terminal
    ) {
      return false;
    }
    exactUint32(control.assetRemaining, "asset remaining");
    if (
      control.currentPolicy.length !== 0 &&
      control.currentPolicy.length !== 28
    ) {
      return false;
    }
    exactSequence(control.currentAssets, "current_assets");
    exactSequence(control.valueEntries, "value_entries");
    if (control.result !== null) exactSummary(control.result, "result");
    if (control.stage === MidgardLedgerOutputValueStages.Terminal) {
      return (
        control.assetRemaining === 0 &&
        control.currentPolicy.length === 0 &&
        isEmptyPairSequence(control.currentAssets) &&
        isEmptyPairSequence(control.valueEntries) &&
        control.result !== null
      );
    }
    return (
      control.result === null &&
      (control.currentPolicy.length !== 0 ||
        isEmptyPairSequence(control.currentAssets)) &&
      (control.stage !== MidgardLedgerOutputValueStages.Finalize ||
        control.assetRemaining === 0)
    );
  } catch {
    return false;
  }
};

export const encodeMidgardLedgerOutputValueControl = (
  control: MidgardLedgerOutputValueControl,
): Buffer => {
  if (!isWellFormedMidgardLedgerOutputValueControl(control)) {
    throw new Error("Invalid V1 ledger output Value control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_LEDGER_OUTPUT_VALUE_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.assetRemaining)),
    encodeCbor(control.currentPolicy),
    encodeSequence(control.currentAssets, "current_assets"),
    encodeSequence(control.valueEntries, "value_entries"),
    encodeOptionalSummary(control.result),
  ]);
};

export const initialMidgardLedgerOutputValueControl = (
  assetCount: number,
): MidgardLedgerOutputValueControl => {
  const control = {
    version: MIDGARD_LEDGER_OUTPUT_VALUE_VERSION,
    stage: MidgardLedgerOutputValueStages.Assets,
    assetRemaining: exactUint32(assetCount, "asset count"),
    currentPolicy: Buffer.alloc(0),
    currentAssets: emptyMidgardCekDataPairSummary(),
    valueEntries: emptyMidgardCekDataPairSummary(),
    result: null,
  } satisfies MidgardLedgerOutputValueControl;
  if (!isWellFormedMidgardLedgerOutputValueControl(control)) {
    throw new Error("Invalid V1 ledger output Value source");
  }
  return control;
};

const integerMemory = (value: bigint): bigint => {
  const doubled = value < 0n ? (-value - 1n) * 2n : value * 2n;
  if (doubled === 0n) return 1n;
  return BigInt(Math.ceil(doubled.toString(2).length / 8));
};

const summarizeInteger = (value: bigint): MidgardCekDataSummary => {
  const cbor = encodeCbor(value);
  const memory = 4n + integerMemory(value);
  return {
    root: hashMidgardCekDataNode({
      kind: "integer",
      cborRoot: commitMidgardCekBlob(cbor).root,
      cborLength: BigInt(cbor.length),
      memory,
    }),
    cborLength: BigInt(cbor.length),
    memory,
  };
};

const summarizeBytes = (bytes: Uint8Array): MidgardCekDataSummary => {
  const exact = Buffer.from(bytes);
  const bytesLength = BigInt(exact.length);
  const cborLength = midgardCekDataBytesCborLength(bytesLength);
  const memory = midgardCekDataBytesMemory(bytesLength);
  return {
    root: hashMidgardCekDataNode({
      kind: "bytes",
      bytesRoot: commitMidgardCekBlob(exact).root,
      bytesLength,
      cborLength,
      memory,
    }),
    cborLength,
    memory,
  };
};

const finalizeCurrentPolicy = (
  control: MidgardLedgerOutputValueControl,
): MidgardCekDataSequenceSummary =>
  control.currentPolicy.length === 0
    ? control.valueEntries
    : prependMidgardCekDataPairSummary(
        summarizeBytes(control.currentPolicy),
        summarizeMidgardCekMapData(control.currentAssets),
        control.valueEntries,
      );

const advanced = (
  control: MidgardLedgerOutputValueControl,
): MidgardLedgerOutputValueControl | null =>
  isWellFormedMidgardLedgerOutputValueControl(control) ? control : null;

export const advanceMidgardLedgerOutputValue = ({
  control,
  assetFrontier,
  lovelace,
  witness,
}: {
  readonly control: MidgardLedgerOutputValueControl;
  readonly assetFrontier: MidgardValidationMerkleFrontier;
  readonly lovelace: bigint;
  readonly witness: MidgardLedgerOutputValueWitness | null;
}): MidgardLedgerOutputValueControl | null => {
  try {
    if (
      !isWellFormedMidgardLedgerOutputValueControl(control) ||
      control.assetRemaining > assetFrontier.count
    ) {
      return null;
    }
    exactUint64(lovelace, "lovelace");
    if (control.stage === MidgardLedgerOutputValueStages.Assets) {
      if (control.assetRemaining === 0) {
        return witness === null
          ? advanced({
              ...control,
              stage: MidgardLedgerOutputValueStages.Finalize,
            })
          : null;
      }
      if (
        witness === null ||
        witness.policyId.length !== 28 ||
        witness.assetName.length > 32 ||
        witness.quantity <= 0n ||
        witness.quantity > UINT64_MAX
      ) {
        return null;
      }
      const leafIndex = control.assetRemaining - 1;
      const leafHash = hashMidgardLedgerOutputAssetLeaf({
        policyId: witness.policyId,
        assetName: witness.assetName,
        quantity: witness.quantity,
      });
      if (
        !verifyMidgardValidationMerkleMembership({
          frontier: assetFrontier,
          leafIndex,
          leafHash,
          siblings: witness.siblings.map((sibling) =>
            ensureHash32(sibling, "ledger_output_value_v1.sibling"),
          ),
        })
      ) {
        return null;
      }
      const policyOrder =
        control.currentPolicy.length === 0
          ? -1
          : Buffer.compare(witness.policyId, control.currentPolicy);
      if (policyOrder > 0) return null;
      const valueEntries =
        policyOrder < 0 && control.currentPolicy.length !== 0
          ? finalizeCurrentPolicy(control)
          : control.valueEntries;
      const currentAssets =
        policyOrder < 0
          ? emptyMidgardCekDataPairSummary()
          : control.currentAssets;
      return advanced({
        ...control,
        assetRemaining: leafIndex,
        currentPolicy: Buffer.from(witness.policyId),
        currentAssets: prependMidgardCekDataPairSummary(
          summarizeBytes(witness.assetName),
          summarizeInteger(witness.quantity),
          currentAssets,
        ),
        valueEntries,
      });
    }
    if (control.stage === MidgardLedgerOutputValueStages.Finalize) {
      if (witness !== null) return null;
      let valueEntries = finalizeCurrentPolicy(control);
      if (lovelace !== 0n) {
        const emptyBytes = summarizeBytes(Buffer.alloc(0));
        const coinAssets = prependMidgardCekDataPairSummary(
          emptyBytes,
          summarizeInteger(lovelace),
          emptyMidgardCekDataPairSummary(),
        );
        valueEntries = prependMidgardCekDataPairSummary(
          emptyBytes,
          summarizeMidgardCekMapData(coinAssets),
          valueEntries,
        );
      }
      return advanced({
        ...control,
        stage: MidgardLedgerOutputValueStages.Terminal,
        currentPolicy: Buffer.alloc(0),
        currentAssets: emptyMidgardCekDataPairSummary(),
        valueEntries: emptyMidgardCekDataPairSummary(),
        result: summarizeMidgardCekMapData(valueEntries),
      });
    }
    return null;
  } catch {
    return null;
  }
};

export const finalizeMidgardLedgerOutputValue = (
  control: MidgardLedgerOutputValueControl,
): MidgardCekDataSummary | null =>
  isWellFormedMidgardLedgerOutputValueControl(control) &&
  control.stage === MidgardLedgerOutputValueStages.Terminal
    ? control.result
    : null;

export const buildMidgardLedgerOutputValueTrace = ({
  assets,
  lovelace,
}: {
  readonly assets: readonly MidgardLedgerOutputAsset[];
  readonly lovelace: bigint;
}): MidgardLedgerOutputValueTrace => {
  exactUint64(lovelace, "lovelace");
  const material = buildMidgardLedgerOutputAssetFrontier(assets);
  const initial = initialMidgardLedgerOutputValueControl(material.count);
  const steps: MidgardLedgerOutputValueTraceStep[] = [];
  let control = initial;
  while (control.assetRemaining > 0) {
    const leafIndex = control.assetRemaining - 1;
    const asset = assets[leafIndex]!;
    const membership = buildMidgardValidationMerkleMembership(
      material.leaves,
      leafIndex,
    );
    const witness: MidgardLedgerOutputValueWitness = {
      policyId: Buffer.from(asset.policyId),
      assetName: Buffer.from(asset.assetName),
      quantity: asset.quantity,
      siblings: membership.siblings,
    };
    const next = advanceMidgardLedgerOutputValue({
      control,
      assetFrontier: material.frontier,
      lovelace,
      witness,
    });
    if (next === null) {
      throw new Error("Canonical V1 ledger output Value fold failed");
    }
    steps.push({ control, witness, next });
    control = next;
  }
  for (let localStep = 0; localStep < 2; localStep += 1) {
    const next = advanceMidgardLedgerOutputValue({
      control,
      assetFrontier: material.frontier,
      lovelace,
      witness: null,
    });
    if (next === null) {
      throw new Error("Canonical V1 ledger output Value close failed");
    }
    steps.push({ control, witness: null, next });
    control = next;
  }
  if (finalizeMidgardLedgerOutputValue(control) === null) {
    throw new Error("Canonical V1 ledger output Value did not terminate");
  }
  return {
    assets,
    frontier: material.frontier,
    initial,
    steps,
    terminal: control,
  };
};
