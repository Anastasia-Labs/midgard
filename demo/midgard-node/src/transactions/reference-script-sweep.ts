/**
 * Reclaims the ADA locked in reference-script UTxOs that belong to a retired
 * deployment's reference-script auth policy.
 *
 * The sweep is scoped to exactly one retired policy and fails closed when that
 * policy, or any script it would spend, belongs to the live deployment. It
 * batches inputs under the protocol's per-transaction reference-script and
 * size limits, and re-plans from chain before every batch so an interrupted
 * run can simply be started again.
 */
import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applySingleCborEncoding,
  type Assets,
  calculateMinLovelaceFromUTxO,
  CML,
  getAddressDetails,
  type LucidEvolution,
  mintingPolicyToId,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { compareOutRefs, outRefLabel } from "../tx-context.js";
import {
  acceptsReferenceScriptUtxo,
  fetchReferenceScriptUtxosAt,
  nodeRuntimeReferenceScriptTargets,
  REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  type ReferenceScriptTarget,
  resolveReferenceScriptUtxo,
} from "./reference-scripts.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";
import { lovelaceOf } from "./wallet-hygiene.js";

/** Share of each ledger limit a planned batch may use. */
const LIMIT_MARGIN_NUMERATOR = 9;
const LIMIT_MARGIN_DENOMINATOR = 10;
export const REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH = 64;
/** Validity window of a burn transaction, in slots. */
export const REFERENCE_SCRIPT_SWEEP_BURN_VALIDITY_SLOTS = 600;

// Upper-bound byte costs for the size estimate. The built transaction is
// checked against the real limit before it is signed.
const TX_BASE_BYTES = 320; // body, fee, ttl, change output, one vkey witness
const TX_INPUT_BYTES = 40;
const TX_OUTPUT_OVERHEAD_BYTES = 80; // address and output map around a value
const TX_MINT_POLICY_BYTES = 40;
const TX_MINT_ASSET_OVERHEAD_BYTES = 12;
const TX_NATIVE_WITNESS_OVERHEAD_BYTES = 16;
const SIGNED_TX_WITNESS_ALLOWANCE_BYTES = 110;
const QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE = 4_000_000_000n;

type Rational = { readonly numerator: bigint; readonly denominator: bigint };

export type ReferenceScriptSweepLimits = {
  readonly maxTxSize: number;
  readonly maxValueSize: number;
  readonly maxReferenceScriptBytesPerTx: number;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly coinsPerUtxoByte: bigint;
  readonly referenceScriptFee: {
    readonly base: Rational;
    readonly range: number;
    readonly multiplier: Rational;
  };
};

/**
 * The live deployment's reference-script resolution: every target it resolves
 * by auth policy, role token and script. The sweep must leave each target
 * resolvable and must never spend a UTxO this resolution accepts.
 */
export type LiveReferenceScriptDeployment = {
  readonly authPolicyId: string;
  readonly targets: readonly ReferenceScriptTarget[];
};

export type ReferenceScriptSweepRefusalCheck =
  | "invalid-retired-policy"
  | "live-auth-policy"
  | "live-auth-token"
  | "live-resolved-outref"
  | "live-target-stranded"
  | "foreign-asset"
  | "oversized-reference-script"
  | "policy-script-mismatch"
  | "unfunded-batch";

/** A guard refused the sweep before anything was built. */
export class ReferenceScriptSweepRefusal extends Error {
  readonly check: ReferenceScriptSweepRefusalCheck;

  constructor(check: ReferenceScriptSweepRefusalCheck, message: string) {
    super(`Reference-script sweep refused (${check}): ${message}`);
    this.name = "ReferenceScriptSweepRefusal";
    this.check = check;
  }
}

export type RetiredAuthPolicyDisposition =
  | {
      readonly kind: "burn";
      readonly policyScript: Script;
      readonly validToSlot: number;
      readonly reason: string;
    }
  | { readonly kind: "quarantine"; readonly reason: string };

export type ReferenceScriptSweepBatch = {
  readonly batchIndex: number;
  readonly inputs: readonly UTxO[];
  readonly inputLovelace: bigint;
  readonly referenceScriptBytes: number;
  readonly estimatedTxBytes: number;
  readonly estimatedFee: bigint;
  readonly burnedAssets: Readonly<Assets>;
  readonly quarantineOutputs: readonly Readonly<Assets>[];
  readonly quarantineLovelace: bigint;
  readonly netReclaimedLovelace: bigint;
};

export type ReferenceScriptSweepPlan = {
  readonly retiredAuthPolicyId: string;
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly quarantineAddress: string;
  readonly disposition: RetiredAuthPolicyDisposition;
  readonly budgets: {
    readonly referenceScriptBytesPerBatch: number;
    readonly txBytesPerBatch: number;
    readonly valueBytesPerOutput: number;
    readonly inputsPerBatch: number;
  };
  readonly retainedUtxoCount: number;
  readonly batches: readonly ReferenceScriptSweepBatch[];
};

const normalizePolicyId = (policyId: string): string => {
  const normalized = policyId.trim().toLowerCase();
  if (!/^[0-9a-f]{56}$/u.test(normalized)) {
    throw new ReferenceScriptSweepRefusal(
      "invalid-retired-policy",
      `retired auth policy must be a 28-byte hex policy id, got "${policyId}"`,
    );
  }
  return normalized;
};

const unitPolicyId = (unit: string): string => unit.slice(0, 56);

const positiveUnitsUnderPolicy = (
  assets: Readonly<Assets>,
  policyId: string,
): readonly string[] =>
  Object.entries(assets)
    .filter(
      ([unit, amount]) =>
        unit !== "lovelace" && amount > 0n && unitPolicyId(unit) === policyId,
    )
    .map(([unit]) => unit);

/**
 * Bytes the ledger charges for a script carried by a spent or referenced
 * input: the serialized script for native scripts and the single-CBOR
 * program bytes for Plutus scripts.
 */
export const referenceScriptLedgerBytes = (script: Script): number =>
  script.type === "Native"
    ? script.script.length / 2
    : applySingleCborEncoding(script.script).length / 2;

const multiplyRational = (left: Rational, right: Rational): Rational => ({
  numerator: left.numerator * right.numerator,
  denominator: left.denominator * right.denominator,
});

const addRational = (left: Rational, right: Rational): Rational => ({
  numerator:
    left.numerator * right.denominator + right.numerator * left.denominator,
  denominator: left.denominator * right.denominator,
});

/** Conway's tiered reference-script fee (`tierRefScriptFee`). */
export const referenceScriptFee = (
  limits: ReferenceScriptSweepLimits,
  referenceScriptBytes: number,
): bigint => {
  const { base, range, multiplier } = limits.referenceScriptFee;
  let accumulated: Rational = { numerator: 0n, denominator: 1n };
  let tierPrice = base;
  let remaining = referenceScriptBytes;
  while (remaining >= range) {
    accumulated = addRational(
      accumulated,
      multiplyRational(
        { numerator: BigInt(range), denominator: 1n },
        tierPrice,
      ),
    );
    tierPrice = multiplyRational(tierPrice, multiplier);
    remaining -= range;
  }
  accumulated = addRational(
    accumulated,
    multiplyRational(
      { numerator: BigInt(remaining), denominator: 1n },
      tierPrice,
    ),
  );
  return accumulated.numerator / accumulated.denominator;
};

export const ledgerMinimumFee = (
  limits: ReferenceScriptSweepLimits,
  txBytes: number,
  referenceScriptBytes: number,
): bigint =>
  limits.minFeeA * BigInt(txBytes) +
  limits.minFeeB +
  referenceScriptFee(limits, referenceScriptBytes);

const naturalFromSnapshot = (value: string, field: string): number => {
  const parsed = Number(value);
  if (!Number.isSafeInteger(parsed) || parsed <= 0) {
    throw new Error(`Protocol parameter ${field} must be a positive integer`);
  }
  return parsed;
};

const rationalFromSnapshot = (value: {
  readonly numerator: string;
  readonly denominator: string;
}): Rational => ({
  numerator: BigInt(value.numerator),
  denominator: BigInt(value.denominator),
});

export const referenceScriptSweepLimitsFromProtocolParameters = (
  snapshot: DeploymentManifestCardanoProtocolParameters,
): ReferenceScriptSweepLimits => ({
  maxTxSize: naturalFromSnapshot(snapshot.maxTxSize, "maxTxSize"),
  maxValueSize: naturalFromSnapshot(snapshot.maxValueSize, "maxValueSize"),
  maxReferenceScriptBytesPerTx: naturalFromSnapshot(
    snapshot.referenceScriptFee.maximumSizeBytes,
    "referenceScriptFee.maximumSizeBytes",
  ),
  minFeeA: BigInt(snapshot.minFeeA),
  minFeeB: BigInt(snapshot.minFeeB),
  coinsPerUtxoByte: BigInt(snapshot.coinsPerUtxoByte),
  referenceScriptFee: {
    base: rationalFromSnapshot(snapshot.referenceScriptFee.base),
    range: naturalFromSnapshot(
      snapshot.referenceScriptFee.range,
      "referenceScriptFee.range",
    ),
    multiplier: rationalFromSnapshot(snapshot.referenceScriptFee.multiplier),
  },
});

const withMargin = (limit: number): number =>
  Math.floor((limit * LIMIT_MARGIN_NUMERATOR) / LIMIT_MARGIN_DENOMINATOR);

/** The live deployment as the node resolves it at runtime. */
export const liveReferenceScriptDeployment = (
  contracts: SDK.MidgardValidators,
): LiveReferenceScriptDeployment => ({
  authPolicyId: contracts.referenceScriptAuth.policyId.toLowerCase(),
  targets: nodeRuntimeReferenceScriptTargets(contracts),
});

/** Refuses a retired policy that is the live deployment's auth policy. */
export const assertRetiredPolicyIsNotLive = (
  retiredAuthPolicyId: string,
  live: LiveReferenceScriptDeployment,
): string => {
  const retired = normalizePolicyId(retiredAuthPolicyId);
  if (retired === live.authPolicyId.toLowerCase()) {
    throw new ReferenceScriptSweepRefusal(
      "live-auth-policy",
      `${retired} is the live deployment's reference-script auth policy`,
    );
  }
  return retired;
};

/**
 * Selects the UTxOs that carry a reference script and a token of the retired
 * policy, and refuses the whole sweep if spending them could break the live
 * deployment's reference-script resolution or would drop an asset the sweep
 * cannot account for. `utxos` is the whole wallet snapshot the plan uses.
 */
export const selectRetiredReferenceScriptUtxos = ({
  utxos,
  referenceScriptsAddress,
  retiredAuthPolicyId,
  live,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
  readonly retiredAuthPolicyId: string;
  readonly live: LiveReferenceScriptDeployment;
}): readonly UTxO[] => {
  const retired = assertRetiredPolicyIsNotLive(retiredAuthPolicyId, live);
  const livePolicy = live.authPolicyId.toLowerCase();
  const liveAuthPolicy = { policyId: livePolicy };
  const selected = utxos
    .filter(
      (utxo) =>
        utxo.scriptRef !== undefined &&
        utxo.scriptRef !== null &&
        positiveUnitsUnderPolicy(utxo.assets, retired).length > 0,
    )
    .sort(compareOutRefs);

  // No selected UTxO may be one the live resolution accepts for any target.
  const liveAccepted = selected.flatMap((utxo) => {
    const targets = live.targets.filter((target) =>
      acceptsReferenceScriptUtxo(
        utxo,
        referenceScriptsAddress,
        target,
        liveAuthPolicy,
      ),
    );
    return targets.length === 0
      ? []
      : [
          `${outRefLabel(utxo)}(${targets.map((target) => target.name).join("|")})`,
        ];
  });
  if (liveAccepted.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "live-resolved-outref",
      `selected UTxOs are ones the live deployment resolves: ${liveAccepted.join(",")}`,
    );
  }
  const liveTokenHolders = selected.filter(
    (utxo) => positiveUnitsUnderPolicy(utxo.assets, livePolicy).length > 0,
  );
  if (liveTokenHolders.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "live-auth-token",
      `selected UTxOs also hold live auth tokens: ${liveTokenHolders.map(outRefLabel).join(",")}`,
    );
  }
  // Every live target must still resolve from what the sweep leaves behind.
  const selectedOutRefs = new Set(selected.map(outRefLabel));
  const remaining = utxos.filter(
    (utxo) => !selectedOutRefs.has(outRefLabel(utxo)),
  );
  const stranded = live.targets.filter(
    (target) =>
      resolveReferenceScriptUtxo(
        remaining,
        referenceScriptsAddress,
        target,
        liveAuthPolicy,
      ) === undefined,
  );
  if (stranded.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "live-target-stranded",
      `live targets would have no resolvable reference script after the sweep: ${stranded.map((target) => target.name).join(",")}`,
    );
  }
  const foreignAssetHolders = selected.filter((utxo) =>
    Object.entries(utxo.assets).some(
      ([unit, amount]) =>
        unit !== "lovelace" && amount !== 0n && unitPolicyId(unit) !== retired,
    ),
  );
  if (foreignAssetHolders.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "foreign-asset",
      `selected UTxOs hold assets outside the retired policy: ${foreignAssetHolders.map(outRefLabel).join(",")}`,
    );
  }
  return selected;
};

type NativeAuthPolicyShape = {
  readonly signerKeyHash: string;
  readonly invalidHereafterSlot: bigint;
};

/** Reads the `all [sig, invalid-hereafter]` shape the deployment creates. */
const readNativeAuthPolicyShape = (
  script: Script,
): NativeAuthPolicyShape | undefined => {
  const native = CML.NativeScript.from_cbor_hex(script.script);
  try {
    const all = native.as_script_all();
    const conditions = all?.native_scripts();
    if (conditions === undefined || conditions.len() !== 2) {
      return undefined;
    }
    const signer = conditions.get(0).as_script_pubkey()?.ed25519_key_hash();
    const hereafter = conditions.get(1).as_script_invalid_hereafter()?.after();
    if (signer === undefined || hereafter === undefined) {
      return undefined;
    }
    return {
      signerKeyHash: signer.to_hex(),
      invalidHereafterSlot: hereafter,
    };
  } finally {
    native.free();
  }
};

/**
 * Burns the retired tokens when the policy can still be satisfied by the
 * reference wallet within a fresh validity window; otherwise quarantines them.
 */
export const decideRetiredAuthPolicyDisposition = ({
  retiredAuthPolicyId,
  policyScript,
  signerKeyHash,
  currentSlot,
}: {
  readonly retiredAuthPolicyId: string;
  readonly policyScript?: Script;
  readonly signerKeyHash: string;
  readonly currentSlot: number;
}): RetiredAuthPolicyDisposition => {
  if (policyScript === undefined) {
    return {
      kind: "quarantine",
      reason:
        "retired policy script not supplied (--retired-auth-policy-script); it cannot be witnessed for a burn",
    };
  }
  if (
    policyScript.type !== "Native" ||
    mintingPolicyToId(policyScript) !== retiredAuthPolicyId
  ) {
    throw new ReferenceScriptSweepRefusal(
      "policy-script-mismatch",
      `supplied policy script does not hash to ${retiredAuthPolicyId}`,
    );
  }
  const shape = readNativeAuthPolicyShape(policyScript);
  if (shape === undefined) {
    return {
      kind: "quarantine",
      reason:
        "retired policy is not the all[signature, invalid-hereafter] auth shape",
    };
  }
  if (shape.signerKeyHash !== signerKeyHash) {
    return {
      kind: "quarantine",
      reason: `retired policy signer ${shape.signerKeyHash} is not the reference wallet key ${signerKeyHash}`,
    };
  }
  const validToSlot = currentSlot + REFERENCE_SCRIPT_SWEEP_BURN_VALIDITY_SLOTS;
  if (BigInt(validToSlot) > shape.invalidHereafterSlot) {
    return {
      kind: "quarantine",
      reason: `retired policy expired: invalid hereafter slot ${shape.invalidHereafterSlot.toString()}, current slot ${currentSlot.toString()}`,
    };
  }
  return {
    kind: "burn",
    policyScript,
    validToSlot,
    reason: `retired policy is satisfiable until slot ${shape.invalidHereafterSlot.toString()}`,
  };
};

const cborHeaderBytes = (argument: bigint): number =>
  argument < 24n
    ? 1
    : argument < 0x100n
      ? 2
      : argument < 0x10000n
        ? 3
        : argument < 0x100000000n
          ? 5
          : 9;

/** Canonical CBOR size of a multi-asset value (`[coin, multiasset]`). */
export const valueCborBytes = (assets: Readonly<Assets>): number => {
  const byPolicy = new Map<string, (readonly [string, bigint])[]>();
  for (const [unit, amount] of Object.entries(assets)) {
    if (unit !== "lovelace" && amount > 0n) {
      const policy = unitPolicyId(unit);
      byPolicy.set(policy, [
        ...(byPolicy.get(policy) ?? []),
        [unit.slice(56), amount],
      ]);
    }
  }
  const coinBytes = cborHeaderBytes(assets.lovelace ?? 0n);
  if (byPolicy.size === 0) {
    return coinBytes;
  }
  let bytes = 1 + coinBytes + cborHeaderBytes(BigInt(byPolicy.size));
  for (const entries of byPolicy.values()) {
    bytes +=
      cborHeaderBytes(28n) + 28 + cborHeaderBytes(BigInt(entries.length));
    for (const [assetNameHex, amount] of entries) {
      const nameBytes = assetNameHex.length / 2;
      bytes +=
        cborHeaderBytes(BigInt(nameBytes)) +
        nameBytes +
        cborHeaderBytes(amount);
    }
  }
  return bytes;
};

const sortedTokenUnits = (utxos: readonly UTxO[]): readonly string[] =>
  utxos
    .flatMap((utxo) =>
      Object.entries(utxo.assets)
        .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
        .map(([unit]) => unit),
    )
    .sort();

const sumTokens = (utxos: readonly UTxO[]): Assets => {
  const totals: Assets = {};
  for (const utxo of utxos) {
    for (const [unit, amount] of Object.entries(utxo.assets)) {
      if (unit !== "lovelace" && amount > 0n) {
        totals[unit] = (totals[unit] ?? 0n) + amount;
      }
    }
  }
  return totals;
};

/**
 * Packs the batch's tokens into as few quarantine outputs as the value-size
 * budget allows, each carrying only its minimum ADA.
 */
const packQuarantineOutputs = ({
  tokens,
  quarantineAddress,
  valueBytesPerOutput,
  coinsPerUtxoByte,
}: {
  readonly tokens: Readonly<Assets>;
  readonly quarantineAddress: string;
  readonly valueBytesPerOutput: number;
  readonly coinsPerUtxoByte: bigint;
}): readonly Assets[] => {
  const outputs: Assets[] = [];
  let current: Assets = { lovelace: QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE };
  let currentCount = 0;
  for (const unit of Object.keys(tokens).sort()) {
    const candidate = { ...current, [unit]: tokens[unit]! };
    if (currentCount > 0 && valueCborBytes(candidate) > valueBytesPerOutput) {
      outputs.push(current);
      current = {
        lovelace: QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE,
        [unit]: tokens[unit]!,
      };
      currentCount = 1;
    } else {
      current = candidate;
      currentCount += 1;
    }
  }
  if (currentCount > 0) {
    outputs.push(current);
  }
  return outputs.map((assets) => ({
    ...assets,
    lovelace: calculateMinLovelaceFromUTxO(coinsPerUtxoByte, {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: quarantineAddress,
      assets,
    }),
  }));
};

const estimateTxBytes = ({
  inputCount,
  quarantineOutputs,
  burnedUnits,
  disposition,
}: {
  readonly inputCount: number;
  readonly quarantineOutputs: readonly Readonly<Assets>[];
  readonly burnedUnits: readonly string[];
  readonly disposition: RetiredAuthPolicyDisposition;
}): number => {
  const outputBytes = quarantineOutputs.reduce(
    (total, output) =>
      total + TX_OUTPUT_OVERHEAD_BYTES + valueCborBytes(output),
    0,
  );
  const burnBytes =
    disposition.kind === "burn" && burnedUnits.length > 0
      ? TX_MINT_POLICY_BYTES +
        burnedUnits.reduce(
          (total, unit) =>
            total + (unit.length - 56) / 2 + TX_MINT_ASSET_OVERHEAD_BYTES,
          0,
        ) +
        disposition.policyScript.script.length / 2 +
        TX_NATIVE_WITNESS_OVERHEAD_BYTES
      : 0;
  return TX_BASE_BYTES + inputCount * TX_INPUT_BYTES + outputBytes + burnBytes;
};

const negateAssets = (assets: Readonly<Assets>): Assets =>
  Object.fromEntries(
    Object.entries(assets).map(([unit, amount]) => [unit, -amount]),
  );

type BatchShape = Omit<ReferenceScriptSweepBatch, "batchIndex">;

const shapeBatch = ({
  inputs,
  disposition,
  quarantineAddress,
  limits,
  valueBytesPerOutput,
}: {
  readonly inputs: readonly UTxO[];
  readonly disposition: RetiredAuthPolicyDisposition;
  readonly quarantineAddress: string;
  readonly limits: ReferenceScriptSweepLimits;
  readonly valueBytesPerOutput: number;
}): BatchShape => {
  const tokens = sumTokens(inputs);
  const quarantineOutputs =
    disposition.kind === "burn"
      ? []
      : packQuarantineOutputs({
          tokens,
          quarantineAddress,
          valueBytesPerOutput,
          coinsPerUtxoByte: limits.coinsPerUtxoByte,
        });
  const burnedAssets = disposition.kind === "burn" ? tokens : {};
  const referenceScriptBytes = inputs.reduce(
    (total, utxo) =>
      total +
      (utxo.scriptRef === undefined || utxo.scriptRef === null
        ? 0
        : referenceScriptLedgerBytes(utxo.scriptRef)),
    0,
  );
  const estimatedTxBytes = estimateTxBytes({
    inputCount: inputs.length,
    quarantineOutputs,
    burnedUnits: sortedTokenUnits(inputs),
    disposition,
  });
  const inputLovelace = inputs.reduce(
    (total, utxo) => total + lovelaceOf(utxo),
    0n,
  );
  const estimatedFee = ledgerMinimumFee(
    limits,
    estimatedTxBytes,
    referenceScriptBytes,
  );
  const quarantineLovelace = quarantineOutputs.reduce(
    (total, output) => total + (output.lovelace ?? 0n),
    0n,
  );
  return {
    inputs,
    inputLovelace,
    referenceScriptBytes,
    estimatedTxBytes,
    estimatedFee,
    burnedAssets,
    quarantineOutputs,
    quarantineLovelace,
    netReclaimedLovelace: inputLovelace - estimatedFee - quarantineLovelace,
  };
};

export const buildReferenceScriptSweepPlan = ({
  utxos,
  referenceScriptsAddress,
  returnAddress,
  quarantineAddress,
  retiredAuthPolicyId,
  live,
  limits,
  disposition,
  maxReferenceScriptBytesPerBatch,
  maxInputsPerBatch = REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly quarantineAddress: string;
  readonly retiredAuthPolicyId: string;
  readonly live: LiveReferenceScriptDeployment;
  readonly limits: ReferenceScriptSweepLimits;
  readonly disposition: RetiredAuthPolicyDisposition;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
}): ReferenceScriptSweepPlan => {
  const retired = assertRetiredPolicyIsNotLive(retiredAuthPolicyId, live);
  const selected = selectRetiredReferenceScriptUtxos({
    utxos,
    referenceScriptsAddress,
    retiredAuthPolicyId: retired,
    live,
  });
  const protocolReferenceBudget = withMargin(
    limits.maxReferenceScriptBytesPerTx,
  );
  if (
    maxReferenceScriptBytesPerBatch !== undefined &&
    (!Number.isSafeInteger(maxReferenceScriptBytesPerBatch) ||
      maxReferenceScriptBytesPerBatch <= 0 ||
      maxReferenceScriptBytesPerBatch > protocolReferenceBudget)
  ) {
    throw new Error(
      `maxReferenceScriptBytesPerBatch must be a positive integer no larger than ${protocolReferenceBudget.toString()}`,
    );
  }
  if (!Number.isSafeInteger(maxInputsPerBatch) || maxInputsPerBatch <= 0) {
    throw new Error("maxInputsPerBatch must be a safe positive integer");
  }
  const budgets = {
    referenceScriptBytesPerBatch:
      maxReferenceScriptBytesPerBatch ?? protocolReferenceBudget,
    txBytesPerBatch: withMargin(limits.maxTxSize),
    valueBytesPerOutput: withMargin(limits.maxValueSize),
    inputsPerBatch: maxInputsPerBatch,
  };
  const shape = (inputs: readonly UTxO[]): BatchShape =>
    shapeBatch({
      inputs,
      disposition,
      quarantineAddress,
      limits,
      valueBytesPerOutput: budgets.valueBytesPerOutput,
    });
  const fits = (batch: BatchShape): boolean =>
    batch.inputs.length <= budgets.inputsPerBatch &&
    batch.referenceScriptBytes <= budgets.referenceScriptBytesPerBatch &&
    batch.estimatedTxBytes <= budgets.txBytesPerBatch;

  const shapes: BatchShape[] = [];
  let current: BatchShape | undefined;
  for (const utxo of selected) {
    const extended = shape([...(current?.inputs ?? []), utxo]);
    if (fits(extended)) {
      current = extended;
      continue;
    }
    const alone = shape([utxo]);
    if (!fits(alone)) {
      throw new ReferenceScriptSweepRefusal(
        "oversized-reference-script",
        `${outRefLabel(utxo)} alone exceeds the batch budget (reference_script_bytes=${alone.referenceScriptBytes.toString()},estimated_tx_bytes=${alone.estimatedTxBytes.toString()})`,
      );
    }
    if (current !== undefined) {
      shapes.push(current);
    }
    current = alone;
  }
  if (current !== undefined) {
    shapes.push(current);
  }
  const unfunded = shapes.filter((batch) => batch.netReclaimedLovelace <= 0n);
  if (unfunded.length > 0) {
    throw new ReferenceScriptSweepRefusal(
      "unfunded-batch",
      `batches cannot fund their fee and quarantine outputs: ${unfunded
        .map((batch) => batch.inputs.map(outRefLabel).join("+"))
        .join(",")}`,
    );
  }
  return {
    retiredAuthPolicyId: retired,
    referenceScriptsAddress,
    returnAddress,
    quarantineAddress,
    disposition,
    budgets,
    retainedUtxoCount: utxos.length - selected.length,
    batches: shapes.map((batch, batchIndex) => ({ batchIndex, ...batch })),
  };
};

export type ReferenceScriptSweepBatchSummary = {
  readonly batchIndex: number;
  readonly inputCount: number;
  readonly inputLovelace: bigint;
  readonly referenceScriptBytes: number;
  readonly estimatedTxBytes: number;
  readonly estimatedFee: bigint;
  readonly burnedAssetCount: number;
  readonly quarantineOutputCount: number;
  readonly quarantineLovelace: bigint;
  readonly netReclaimedLovelace: bigint;
  readonly inputOutRefs: readonly string[];
};

export type ReferenceScriptSweepPlanSummary = {
  readonly retiredAuthPolicyId: string;
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly quarantineAddress: string;
  readonly tokenDisposition: "burn" | "quarantine";
  readonly tokenDispositionReason: string;
  readonly budgets: ReferenceScriptSweepPlan["budgets"];
  readonly retainedUtxoCount: number;
  readonly batches: readonly ReferenceScriptSweepBatchSummary[];
  readonly totals: {
    readonly batchCount: number;
    readonly inputCount: number;
    readonly inputLovelace: bigint;
    readonly referenceScriptBytes: number;
    readonly estimatedFee: bigint;
    readonly burnedAssetCount: number;
    readonly quarantineOutputCount: number;
    readonly quarantineLovelace: bigint;
    readonly netReclaimedLovelace: bigint;
  };
};

const summarizeBatch = (
  batch: ReferenceScriptSweepBatch,
): ReferenceScriptSweepBatchSummary => ({
  batchIndex: batch.batchIndex,
  inputCount: batch.inputs.length,
  inputLovelace: batch.inputLovelace,
  referenceScriptBytes: batch.referenceScriptBytes,
  estimatedTxBytes: batch.estimatedTxBytes,
  estimatedFee: batch.estimatedFee,
  burnedAssetCount: Object.keys(batch.burnedAssets).length,
  quarantineOutputCount: batch.quarantineOutputs.length,
  quarantineLovelace: batch.quarantineLovelace,
  netReclaimedLovelace: batch.netReclaimedLovelace,
  inputOutRefs: batch.inputs.map(outRefLabel),
});

export const summarizeReferenceScriptSweepPlan = (
  plan: ReferenceScriptSweepPlan,
): ReferenceScriptSweepPlanSummary => {
  const batches = plan.batches.map(summarizeBatch);
  const sum = (pick: (batch: ReferenceScriptSweepBatchSummary) => bigint) =>
    batches.reduce((total, batch) => total + pick(batch), 0n);
  const count = (pick: (batch: ReferenceScriptSweepBatchSummary) => number) =>
    batches.reduce((total, batch) => total + pick(batch), 0);
  return {
    retiredAuthPolicyId: plan.retiredAuthPolicyId,
    referenceScriptsAddress: plan.referenceScriptsAddress,
    returnAddress: plan.returnAddress,
    quarantineAddress: plan.quarantineAddress,
    tokenDisposition: plan.disposition.kind,
    tokenDispositionReason: plan.disposition.reason,
    budgets: plan.budgets,
    retainedUtxoCount: plan.retainedUtxoCount,
    batches,
    totals: {
      batchCount: batches.length,
      inputCount: count((batch) => batch.inputCount),
      inputLovelace: sum((batch) => batch.inputLovelace),
      referenceScriptBytes: count((batch) => batch.referenceScriptBytes),
      estimatedFee: sum((batch) => batch.estimatedFee),
      burnedAssetCount: count((batch) => batch.burnedAssetCount),
      quarantineOutputCount: count((batch) => batch.quarantineOutputCount),
      quarantineLovelace: sum((batch) => batch.quarantineLovelace),
      netReclaimedLovelace: sum((batch) => batch.netReclaimedLovelace),
    },
  };
};

export type ReferenceScriptSweepOptions = {
  readonly retiredAuthPolicyId: string;
  readonly retiredAuthPolicyScript?: Script;
  readonly quarantineAddress?: string;
  readonly maxReferenceScriptBytesPerBatch?: number;
  readonly maxInputsPerBatch?: number;
  readonly execute: boolean;
  readonly acknowledgeRetirement: boolean;
};

export type ReferenceScriptSweepSubmittedBatch = {
  readonly txHash: string;
  readonly inputCount: number;
  readonly fee: bigint;
  readonly txBytes: number;
  readonly referenceScriptBytes: number;
  readonly tokenDisposition: "burn" | "quarantine";
};

export type ReferenceScriptSweepResult = {
  readonly dryRun: boolean;
  readonly plan: ReferenceScriptSweepPlanSummary;
  readonly submitted: readonly ReferenceScriptSweepSubmittedBatch[];
};

type ReferenceScriptSweepError =
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError;

const refusalToError = (cause: unknown): SDK.StateQueueError =>
  new SDK.StateQueueError({
    message:
      cause instanceof Error
        ? cause.message
        : "Reference-script sweep planning failed",
    cause,
  });

const walletPaymentKeyHash = (address: string): string => {
  const { paymentCredential } = getAddressDetails(address);
  if (paymentCredential?.type !== "Key") {
    throw new Error("Reference-script wallet must have a payment key");
  }
  return paymentCredential.hash;
};

const transactionInputOutRefs = (tx: CML.Transaction): readonly string[] => {
  const inputs = tx.body().inputs();
  return Array.from({ length: inputs.len() }, (_, index) => {
    const input = inputs.get(index);
    return `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  }).sort();
};

const buildBatchTransaction = (
  lucid: LucidEvolution,
  plan: ReferenceScriptSweepPlan,
  batch: ReferenceScriptSweepBatch,
  limits: ReferenceScriptSweepLimits,
) =>
  Effect.gen(function* () {
    const unsigned = yield* Effect.tryPromise({
      try: () => {
        let tx = lucid.newTx().collectFrom([...batch.inputs]);
        if (plan.disposition.kind === "burn") {
          tx = tx
            .mintAssets(negateAssets(batch.burnedAssets))
            .attach.MintingPolicy(plan.disposition.policyScript)
            .validTo(lucid.slotToUnixTime(plan.disposition.validToSlot));
        }
        for (const output of batch.quarantineOutputs) {
          tx = tx.pay.ToAddress(plan.quarantineAddress, { ...output });
        }
        return tx.complete({
          coinSelection: false,
          localUPLCEval: true,
          changeAddress: plan.returnAddress,
          presetWalletInputs: [...batch.inputs],
        });
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build reference-script sweep batch: ${String(cause)}`,
          cause,
        }),
    });
    const tx = unsigned.toTransaction();
    const txBytes = unsigned.toCBOR().length / 2;
    const fee = tx.body().fee();
    const inputOutRefs = transactionInputOutRefs(tx);
    const expectedOutRefs = batch.inputs.map(outRefLabel).sort();
    const minimumFee = ledgerMinimumFee(
      limits,
      txBytes + SIGNED_TX_WITNESS_ALLOWANCE_BYTES,
      batch.referenceScriptBytes,
    );
    if (
      inputOutRefs.join(",") !== expectedOutRefs.join(",") ||
      txBytes + SIGNED_TX_WITNESS_ALLOWANCE_BYTES > limits.maxTxSize ||
      batch.referenceScriptBytes > limits.maxReferenceScriptBytesPerTx ||
      fee < minimumFee
    ) {
      return yield* Effect.fail(
        new SDK.LucidError({
          message:
            "Built reference-script sweep batch does not match its plan or the ledger limits",
          cause: `inputs=${inputOutRefs.length.toString()}/${expectedOutRefs.length.toString()},tx_bytes=${txBytes.toString()},max_tx_bytes=${limits.maxTxSize.toString()},reference_script_bytes=${batch.referenceScriptBytes.toString()},fee=${fee.toString()},ledger_minimum_fee=${minimumFee.toString()}`,
        }),
      );
    }
    return { unsigned, txBytes, fee };
  });

/**
 * Plans (dry run) or executes the retired reference-script sweep. Execution
 * submits one batch at a time, waits for its confirmation and re-plans from
 * chain before the next batch.
 */
export const sweepRetiredReferenceScriptsProgram = ({
  lucid,
  referenceScriptsAddress,
  live,
  limits,
  options,
}: {
  readonly lucid: LucidEvolution;
  readonly referenceScriptsAddress: string;
  readonly live: LiveReferenceScriptDeployment;
  readonly limits: ReferenceScriptSweepLimits;
  readonly options: ReferenceScriptSweepOptions;
}): Effect.Effect<ReferenceScriptSweepResult, ReferenceScriptSweepError> =>
  Effect.gen(function* () {
    const retiredAuthPolicyId = yield* Effect.try({
      try: () =>
        assertRetiredPolicyIsNotLive(options.retiredAuthPolicyId, live),
      catch: refusalToError,
    });
    const returnAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: "Failed to resolve the reference-script wallet address",
          cause,
        }),
    });
    const signerKeyHash = yield* Effect.try({
      try: () => walletPaymentKeyHash(returnAddress),
      catch: refusalToError,
    });
    const quarantineAddress = options.quarantineAddress ?? returnAddress;
    const spentOutRefs = new Set<string>();

    const planFromChain = Effect.gen(function* () {
      const utxos = yield* fetchReferenceScriptUtxosAt(
        lucid,
        referenceScriptsAddress,
        `reference-script sweep UTxO fetch at ${referenceScriptsAddress}`,
        `Failed to fetch reference-script sweep UTxOs at ${referenceScriptsAddress}`,
      );
      return yield* Effect.try({
        try: () =>
          buildReferenceScriptSweepPlan({
            utxos: utxos.filter((utxo) => !spentOutRefs.has(outRefLabel(utxo))),
            referenceScriptsAddress,
            returnAddress,
            quarantineAddress,
            retiredAuthPolicyId,
            live,
            limits,
            disposition: decideRetiredAuthPolicyDisposition({
              retiredAuthPolicyId,
              policyScript: options.retiredAuthPolicyScript,
              signerKeyHash,
              currentSlot: lucid.currentSlot(),
            }),
            maxReferenceScriptBytesPerBatch:
              options.maxReferenceScriptBytesPerBatch,
            maxInputsPerBatch: options.maxInputsPerBatch,
          }),
        catch: refusalToError,
      });
    });

    const initialPlan = yield* planFromChain;
    const plan = summarizeReferenceScriptSweepPlan(initialPlan);
    if (!options.execute || initialPlan.batches.length === 0) {
      return { dryRun: !options.execute, plan, submitted: [] };
    }
    if (!options.acknowledgeRetirement) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Refusing to execute reference-script sweep without retirement acknowledgement",
          cause:
            "Pass --i-am-retiring-reference-scripts to confirm the retired auth policy's reference scripts are no longer live.",
        }),
      );
    }

    const submitted: ReferenceScriptSweepSubmittedBatch[] = [];
    // Each confirmed batch removes at least one selected input, so the
    // initial input count bounds the number of rounds; the extra round is the
    // final re-plan that finds nothing left.
    const maxRounds = plan.totals.inputCount + 1;
    for (let round = 0; round < maxRounds; round += 1) {
      // A pinned wallet view from the previous batch would hide fresh inputs
      // from the signer.
      lucid.clearUTxOOverride();
      const current = round === 0 ? initialPlan : yield* planFromChain;
      const batch = current.batches[0];
      if (batch === undefined) {
        break;
      }
      const built = yield* buildBatchTransaction(lucid, current, batch, limits);
      yield* Effect.logInfo(
        `Submitting reference-script sweep batch ${(submitted.length + 1).toString()}: inputs=${batch.inputs.length.toString()},fee=${built.fee.toString()},tx_bytes=${built.txBytes.toString()},reference_script_bytes=${batch.referenceScriptBytes.toString()},token_disposition=${current.disposition.kind}`,
      );
      const txHash = yield* handleSignSubmit(lucid, built.unsigned, {
        confirmationTimeoutMs: REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
        confirmationRetries: 0,
      });
      for (const input of batch.inputs) {
        spentOutRefs.add(outRefLabel(input));
      }
      submitted.push({
        txHash,
        inputCount: batch.inputs.length,
        fee: built.fee,
        txBytes: built.txBytes,
        referenceScriptBytes: batch.referenceScriptBytes,
        tokenDisposition: current.disposition.kind,
      });
    }
    return { dryRun: false, plan, submitted };
  });
