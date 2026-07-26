import {
  commitMidgardCekBlobV1,
} from "./cek-proof.js";
import {
  emptyMidgardCekDataPairSummaryV1,
  hashMidgardCekDataNodeV1,
  midgardCekDataBytesCborLengthV1,
  midgardCekDataBytesMemoryV1,
  type MidgardCekDataSequenceSummaryV1,
  type MidgardCekDataSummaryV1,
  prependMidgardCekDataPairSummaryV1,
  summarizeMidgardCekMapDataV1,
} from "./cek-semantic.js";
import {
  encodeCbor,
  encodeCborArrayRaw,
} from "./codec/cbor.js";
import { ensureHash32 } from "./codec/hash.js";
import {
  buildMidgardLedgerOutputAssetFrontierV1,
  hashMidgardLedgerOutputAssetLeafV1,
  type MidgardLedgerOutputAssetV1,
} from "./ledger-output-commitment-v1.js";
import {
  buildMidgardValidationMerkleMembershipV1,
  type MidgardValidationMerkleFrontierV1,
  verifyMidgardValidationMerkleMembershipV1,
} from "./validation-merkle.js";

export const MIDGARD_LEDGER_OUTPUT_VALUE_V1_VERSION = 1 as const;

export const MidgardLedgerOutputValueStagesV1 = Object.freeze({
  Assets: 0,
  Finalize: 1,
  Terminal: 2,
} as const);

export type MidgardLedgerOutputValueStageV1 =
  (typeof MidgardLedgerOutputValueStagesV1)[keyof typeof MidgardLedgerOutputValueStagesV1];

export type MidgardLedgerOutputValueControlV1 = {
  readonly version: typeof MIDGARD_LEDGER_OUTPUT_VALUE_V1_VERSION;
  readonly stage: MidgardLedgerOutputValueStageV1;
  readonly assetRemaining: number;
  readonly currentPolicy: Buffer;
  readonly currentAssets: MidgardCekDataSequenceSummaryV1;
  readonly valueEntries: MidgardCekDataSequenceSummaryV1;
  readonly result: MidgardCekDataSummaryV1 | null;
};

export type MidgardLedgerOutputValueWitnessV1 = {
  readonly policyId: Buffer;
  readonly assetName: Buffer;
  readonly quantity: bigint;
  readonly siblings: readonly Uint8Array[];
};

export type MidgardLedgerOutputValueTraceStepV1 = {
  readonly control: MidgardLedgerOutputValueControlV1;
  readonly witness: MidgardLedgerOutputValueWitnessV1 | null;
  readonly next: MidgardLedgerOutputValueControlV1;
};

export type MidgardLedgerOutputValueTraceV1 = {
  readonly assets: readonly MidgardLedgerOutputAssetV1[];
  readonly frontier: MidgardValidationMerkleFrontierV1;
  readonly initial: MidgardLedgerOutputValueControlV1;
  readonly steps: readonly MidgardLedgerOutputValueTraceStepV1[];
  readonly terminal: MidgardLedgerOutputValueControlV1;
};

const UINT32_MAX = 0xffff_ffff;
const UINT64_MAX = 0xffff_ffff_ffff_ffffn;

const exactUint32 = (value: number, field: string): number => {
  if (
    !Number.isSafeInteger(value) ||
    value < 0 ||
    value > UINT32_MAX
  ) {
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
  summary: MidgardCekDataSequenceSummaryV1,
  field: string,
): MidgardCekDataSequenceSummaryV1 => ({
  root: ensureHash32(summary.root, `${field}.root`),
  length: BigInt(exactUint32(Number(summary.length), `${field}.length`)),
  payloadCborLength: exactUint64(
    summary.payloadCborLength,
    `${field}.payload_cbor_length`,
  ),
  memory: exactUint64(summary.memory, `${field}.memory`),
});

const exactSummary = (
  summary: MidgardCekDataSummaryV1,
  field: string,
): MidgardCekDataSummaryV1 => ({
  root: ensureHash32(summary.root, `${field}.root`),
  cborLength: exactUint64(
    summary.cborLength,
    `${field}.cbor_length`,
  ),
  memory: exactUint64(summary.memory, `${field}.memory`),
});

const encodeSequence = (
  summary: MidgardCekDataSequenceSummaryV1,
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
  summary: MidgardCekDataSummaryV1 | null,
): Buffer => {
  if (summary === null) return Buffer.from("d87a80", "hex");
  const exact = exactSummary(summary, "result");
  return Buffer.concat([
    Buffer.from("d8799f", "hex"),
    encodeCbor([
      exact.root,
      exact.cborLength,
      exact.memory,
    ]),
    Buffer.from([0xff]),
  ]);
};

const isEmptyPairSequence = (
  summary: MidgardCekDataSequenceSummaryV1,
): boolean => {
  const empty = emptyMidgardCekDataPairSummaryV1();
  return (
    summary.length === 0n &&
    summary.payloadCborLength === 0n &&
    summary.memory === 0n &&
    Buffer.from(summary.root).equals(Buffer.from(empty.root))
  );
};

export const isWellFormedMidgardLedgerOutputValueControlV1 = (
  control: MidgardLedgerOutputValueControlV1,
): boolean => {
  try {
    if (
      control.version !== MIDGARD_LEDGER_OUTPUT_VALUE_V1_VERSION ||
      !Number.isSafeInteger(control.stage) ||
      control.stage < MidgardLedgerOutputValueStagesV1.Assets ||
      control.stage > MidgardLedgerOutputValueStagesV1.Terminal
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
    if (
      control.stage === MidgardLedgerOutputValueStagesV1.Terminal
    ) {
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
      (control.stage !==
        MidgardLedgerOutputValueStagesV1.Finalize ||
        control.assetRemaining === 0)
    );
  } catch {
    return false;
  }
};

export const encodeMidgardLedgerOutputValueControlV1 = (
  control: MidgardLedgerOutputValueControlV1,
): Buffer => {
  if (!isWellFormedMidgardLedgerOutputValueControlV1(control)) {
    throw new Error("Invalid V1 ledger output Value control");
  }
  return encodeCborArrayRaw([
    encodeCbor(BigInt(MIDGARD_LEDGER_OUTPUT_VALUE_V1_VERSION)),
    encodeCbor(BigInt(control.stage)),
    encodeCbor(BigInt(control.assetRemaining)),
    encodeCbor(control.currentPolicy),
    encodeSequence(control.currentAssets, "current_assets"),
    encodeSequence(control.valueEntries, "value_entries"),
    encodeOptionalSummary(control.result),
  ]);
};

export const initialMidgardLedgerOutputValueControlV1 = (
  assetCount: number,
): MidgardLedgerOutputValueControlV1 => {
  const control = {
    version: MIDGARD_LEDGER_OUTPUT_VALUE_V1_VERSION,
    stage: MidgardLedgerOutputValueStagesV1.Assets,
    assetRemaining: exactUint32(assetCount, "asset count"),
    currentPolicy: Buffer.alloc(0),
    currentAssets: emptyMidgardCekDataPairSummaryV1(),
    valueEntries: emptyMidgardCekDataPairSummaryV1(),
    result: null,
  } satisfies MidgardLedgerOutputValueControlV1;
  if (!isWellFormedMidgardLedgerOutputValueControlV1(control)) {
    throw new Error("Invalid V1 ledger output Value source");
  }
  return control;
};

const integerMemory = (value: bigint): bigint => {
  const doubled = value < 0n ? (-value - 1n) * 2n : value * 2n;
  if (doubled === 0n) return 1n;
  return BigInt(Math.ceil(doubled.toString(2).length / 8));
};

const summarizeInteger = (
  value: bigint,
): MidgardCekDataSummaryV1 => {
  const cbor = encodeCbor(value);
  const memory = 4n + integerMemory(value);
  return {
    root: hashMidgardCekDataNodeV1({
      kind: "integer",
      cborRoot: commitMidgardCekBlobV1(cbor).root,
      cborLength: BigInt(cbor.length),
      memory,
    }),
    cborLength: BigInt(cbor.length),
    memory,
  };
};

const summarizeBytes = (
  bytes: Uint8Array,
): MidgardCekDataSummaryV1 => {
  const exact = Buffer.from(bytes);
  const bytesLength = BigInt(exact.length);
  const cborLength = midgardCekDataBytesCborLengthV1(bytesLength);
  const memory = midgardCekDataBytesMemoryV1(bytesLength);
  return {
    root: hashMidgardCekDataNodeV1({
      kind: "bytes",
      bytesRoot: commitMidgardCekBlobV1(exact).root,
      bytesLength,
      cborLength,
      memory,
    }),
    cborLength,
    memory,
  };
};

const finalizeCurrentPolicy = (
  control: MidgardLedgerOutputValueControlV1,
): MidgardCekDataSequenceSummaryV1 =>
  control.currentPolicy.length === 0
    ? control.valueEntries
    : prependMidgardCekDataPairSummaryV1(
        summarizeBytes(control.currentPolicy),
        summarizeMidgardCekMapDataV1(control.currentAssets),
        control.valueEntries,
      );

const advanced = (
  control: MidgardLedgerOutputValueControlV1,
): MidgardLedgerOutputValueControlV1 | null =>
  isWellFormedMidgardLedgerOutputValueControlV1(control)
    ? control
    : null;

export const advanceMidgardLedgerOutputValueV1 = ({
  control,
  assetFrontier,
  lovelace,
  witness,
}: {
  readonly control: MidgardLedgerOutputValueControlV1;
  readonly assetFrontier: MidgardValidationMerkleFrontierV1;
  readonly lovelace: bigint;
  readonly witness: MidgardLedgerOutputValueWitnessV1 | null;
}): MidgardLedgerOutputValueControlV1 | null => {
  try {
    if (
      !isWellFormedMidgardLedgerOutputValueControlV1(control) ||
      control.assetRemaining > assetFrontier.count
    ) {
      return null;
    }
    exactUint64(lovelace, "lovelace");
    if (
      control.stage === MidgardLedgerOutputValueStagesV1.Assets
    ) {
      if (control.assetRemaining === 0) {
        return witness === null
          ? advanced({
              ...control,
              stage: MidgardLedgerOutputValueStagesV1.Finalize,
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
      const leafHash = hashMidgardLedgerOutputAssetLeafV1({
        policyId: witness.policyId,
        assetName: witness.assetName,
        quantity: witness.quantity,
      });
      if (
        !verifyMidgardValidationMerkleMembershipV1({
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
          ? emptyMidgardCekDataPairSummaryV1()
          : control.currentAssets;
      return advanced({
        ...control,
        assetRemaining: leafIndex,
        currentPolicy: Buffer.from(witness.policyId),
        currentAssets: prependMidgardCekDataPairSummaryV1(
          summarizeBytes(witness.assetName),
          summarizeInteger(witness.quantity),
          currentAssets,
        ),
        valueEntries,
      });
    }
    if (
      control.stage === MidgardLedgerOutputValueStagesV1.Finalize
    ) {
      if (witness !== null) return null;
      let valueEntries = finalizeCurrentPolicy(control);
      if (lovelace !== 0n) {
        const emptyBytes = summarizeBytes(Buffer.alloc(0));
        const coinAssets = prependMidgardCekDataPairSummaryV1(
          emptyBytes,
          summarizeInteger(lovelace),
          emptyMidgardCekDataPairSummaryV1(),
        );
        valueEntries = prependMidgardCekDataPairSummaryV1(
          emptyBytes,
          summarizeMidgardCekMapDataV1(coinAssets),
          valueEntries,
        );
      }
      return advanced({
        ...control,
        stage: MidgardLedgerOutputValueStagesV1.Terminal,
        currentPolicy: Buffer.alloc(0),
        currentAssets: emptyMidgardCekDataPairSummaryV1(),
        valueEntries: emptyMidgardCekDataPairSummaryV1(),
        result: summarizeMidgardCekMapDataV1(valueEntries),
      });
    }
    return null;
  } catch {
    return null;
  }
};

export const finalizeMidgardLedgerOutputValueV1 = (
  control: MidgardLedgerOutputValueControlV1,
): MidgardCekDataSummaryV1 | null =>
  isWellFormedMidgardLedgerOutputValueControlV1(control) &&
  control.stage === MidgardLedgerOutputValueStagesV1.Terminal
    ? control.result
    : null;

export const buildMidgardLedgerOutputValueTraceV1 = ({
  assets,
  lovelace,
}: {
  readonly assets: readonly MidgardLedgerOutputAssetV1[];
  readonly lovelace: bigint;
}): MidgardLedgerOutputValueTraceV1 => {
  exactUint64(lovelace, "lovelace");
  const material = buildMidgardLedgerOutputAssetFrontierV1(assets);
  const initial = initialMidgardLedgerOutputValueControlV1(
    material.count,
  );
  const steps: MidgardLedgerOutputValueTraceStepV1[] = [];
  let control = initial;
  while (control.assetRemaining > 0) {
    const leafIndex = control.assetRemaining - 1;
    const asset = assets[leafIndex]!;
    const membership = buildMidgardValidationMerkleMembershipV1(
      material.leaves,
      leafIndex,
    );
    const witness: MidgardLedgerOutputValueWitnessV1 = {
      policyId: Buffer.from(asset.policyId),
      assetName: Buffer.from(asset.assetName),
      quantity: asset.quantity,
      siblings: membership.siblings,
    };
    const next = advanceMidgardLedgerOutputValueV1({
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
    const next = advanceMidgardLedgerOutputValueV1({
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
  if (finalizeMidgardLedgerOutputValueV1(control) === null) {
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
