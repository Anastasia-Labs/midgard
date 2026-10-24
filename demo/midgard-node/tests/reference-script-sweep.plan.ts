import * as SDK from "@al-ft/midgard-sdk";
import {
  type Assets,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  buildReferenceScriptSweepPlan,
  type LiveReferenceScriptDeployment,
  type ReferenceScriptSweepLimits,
  type ReferenceScriptSweepPlan,
  ReferenceScriptSweepRefusal,
  type RetiredAuthPolicyDisposition,
} from "../src/transactions/reference-script-sweep.js";

export const WALLET =
  "addr_test1qq7kh4kps5dknl2ntzp57r5yywjc6uld79nd9vqwa9hrsvwxtp05cv8ax7wyws8r3h8ut00q0axzvm3dlz0nd7exu8csgx4p7w";

export const RETIRED = "1a".repeat(28);

export const LIVE = "ef".repeat(28);

export const SIGNER = "3d".repeat(28);

/** Current preprod protocol parameters. */
export const PREPROD_LIMITS: ReferenceScriptSweepLimits = {
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  maxReferenceScriptBytesPerTx: 204_800,
  minFeeA: 44n,
  minFeeB: 155_381n,
  coinsPerUtxoByte: 4_310n,
  referenceScriptFee: {
    base: { numerator: 15n, denominator: 1n },
    range: 25_600,
    multiplier: { numerator: 6n, denominator: 5n },
  },
};

const QUARANTINE: RetiredAuthPolicyDisposition = {
  kind: "quarantine",
  reason: "expired",
};

export const NO_LIVE_SCRIPTS: LiveReferenceScriptDeployment = {
  authPolicyId: LIVE,
  targets: [],
};

const cborBytesHeader = (length: number): string =>
  length < 24
    ? (0x40 + length).toString(16).padStart(2, "0")
    : length < 0x100
      ? `58${length.toString(16).padStart(2, "0")}`
      : length < 0x10000
        ? `59${length.toString(16).padStart(4, "0")}`
        : `5a${length.toString(16).padStart(8, "0")}`;

/** A Plutus script whose single-CBOR program is `payloadBytes` bytes long. */
export const plutusScript = (seed: number, payloadBytes: number): Script => {
  const payload = seed
    .toString(16)
    .padStart(8, "0")
    .repeat(payloadBytes / 4);
  const single = `${cborBytesHeader(payloadBytes)}${payload}`;
  return {
    type: "PlutusV3",
    script: `${cborBytesHeader(single.length / 2)}${single}`,
  };
};

export const tokenName = (index: number): string =>
  Buffer.from(`Role${index.toString().padStart(3, "0")}`).toString("hex");

const txHash = (index: number): string => index.toString(16).padStart(64, "0");

export const refUtxo = ({
  index,
  policyId = RETIRED,
  script = plutusScript(index, 4_000),
  lovelace = 40_000_000n,
  extraAssets = {},
}: {
  readonly index: number;
  readonly policyId?: string;
  readonly script?: Script;
  readonly lovelace?: bigint;
  readonly extraAssets?: Assets;
}): UTxO => ({
  txHash: txHash(index),
  outputIndex: 0,
  address: WALLET,
  assets: {
    lovelace,
    [toUnit(policyId, tokenName(index))]: 1n,
    ...extraAssets,
  },
  scriptRef: script,
});

export const LIVE_ROLE_NAMES = Object.keys(
  SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
);

export const liveTarget = (
  roleIndex: number,
  script: Script,
): SDK.ReferenceScriptTarget => ({
  name: LIVE_ROLE_NAMES[roleIndex]!,
  script,
});

/** A reference script the live resolution accepts for `target`. */
export const liveRef = (
  index: number,
  target: SDK.ReferenceScriptTarget,
  address = WALLET,
): UTxO => ({
  txHash: txHash(index),
  outputIndex: 0,
  address,
  assets: {
    lovelace: 40_000_000n,
    [SDK.referenceScriptAuthUnit(LIVE, target.name)]: 1n,
  },
  scriptRef: target.script,
});

export const liveDeployment = (
  ...targets: SDK.ReferenceScriptTarget[]
): LiveReferenceScriptDeployment => ({ authPolicyId: LIVE, targets });

export const plainUtxo = (index: number): UTxO => ({
  txHash: txHash(index),
  outputIndex: 0,
  address: WALLET,
  assets: { lovelace: 100_000_000n },
});

export const plan = ({
  utxos,
  live = NO_LIVE_SCRIPTS,
  limits = PREPROD_LIMITS,
  disposition = QUARANTINE,
  maxReferenceScriptBytesPerBatch,
  maxInputsPerBatch,
  retiredAuthPolicyId = RETIRED,
}: {
  readonly utxos: readonly UTxO[];
  readonly live?: LiveReferenceScriptDeployment;
  readonly limits?: ReferenceScriptSweepLimits;
  readonly disposition?: RetiredAuthPolicyDisposition;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
  readonly retiredAuthPolicyId?: string;
}): ReferenceScriptSweepPlan =>
  buildReferenceScriptSweepPlan({
    utxos,
    referenceScriptsAddress: WALLET,
    returnAddress: WALLET,
    quarantineAddress: WALLET,
    retiredAuthPolicyId,
    live,
    limits,
    disposition,
    maxReferenceScriptBytesPerBatch,
    maxInputsPerBatch,
  });

export const refusalCheck = (run: () => unknown): string => {
  try {
    run();
  } catch (error) {
    if (error instanceof ReferenceScriptSweepRefusal) {
      return error.check;
    }
    throw error;
  }
  throw new Error("expected the sweep to be refused");
};

export const batchOutRefs = (sweep: ReferenceScriptSweepPlan): string[][] =>
  sweep.batches.map((batch) =>
    batch.inputs.map((utxo) => `${utxo.txHash}#${utxo.outputIndex}`),
  );

/** Applies one batch the way the chain would: inputs gone, quarantine and change added. */
export const applyBatch = (
  utxos: readonly UTxO[],
  sweep: ReferenceScriptSweepPlan,
  batchIndex: number,
): UTxO[] => {
  const batch = sweep.batches[batchIndex]!;
  const spent = new Set(batch.inputs.map((utxo) => utxo.txHash));
  const txId = txHash(900_000 + batchIndex);
  return [
    ...utxos.filter((utxo) => !spent.has(utxo.txHash)),
    ...batch.quarantineOutputs.map((assets, outputIndex) => ({
      txHash: txId,
      outputIndex,
      address: WALLET,
      assets: { ...assets },
    })),
    {
      txHash: txId,
      outputIndex: batch.quarantineOutputs.length,
      address: WALLET,
      assets: { lovelace: batch.netReclaimedLovelace },
    },
  ];
};
