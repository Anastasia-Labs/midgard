import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  applySingleCborEncoding,
  type Assets,
  type Script,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  nodeRuntimeReferenceScriptTargets,
  type ReferenceScriptTarget,
} from "./reference-scripts.js";

/** Share of each ledger limit a planned batch may use. */
const LIMIT_MARGIN_NUMERATOR = 9;

const LIMIT_MARGIN_DENOMINATOR = 10;

export const REFERENCE_SCRIPT_SWEEP_MAX_INPUTS_PER_BATCH = 64;

/** Validity window of a burn transaction, in slots. */
export const REFERENCE_SCRIPT_SWEEP_BURN_VALIDITY_SLOTS = 600;

// Upper-bound byte costs for the size estimate. The built transaction is
// checked against the real limit before it is signed.
export const TX_BASE_BYTES = 320;
// body, fee, ttl, change output, one vkey witness
export const TX_INPUT_BYTES = 40;

export const TX_OUTPUT_OVERHEAD_BYTES = 80;
// address and output map around a value
export const TX_MINT_POLICY_BYTES = 40;

export const TX_MINT_ASSET_OVERHEAD_BYTES = 12;

export const TX_NATIVE_WITNESS_OVERHEAD_BYTES = 16;

export const SIGNED_TX_WITNESS_ALLOWANCE_BYTES = 110;

export const QUARANTINE_OUTPUT_PLACEHOLDER_LOVELACE = 4_000_000_000n;

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

export const unitPolicyId = (unit: string): string => unit.slice(0, 56);

export const positiveUnitsUnderPolicy = (
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

export const withMargin = (limit: number): number =>
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
