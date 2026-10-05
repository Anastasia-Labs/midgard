import * as SDK from "@al-ft/midgard-sdk";
import { type ReferenceScriptAuthPolicyRef } from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { runProviderStepWithRetry } from "../provider-retry.js";
import { compareOutRefs, outRefLabel } from "../tx-context.js";
import {
  hasPositiveNonLovelaceAsset,
  isPlainAdaOnlyUtxo,
  lovelaceOf,
} from "./wallet-hygiene.js";

export type ReferenceScriptTarget = SDK.ReferenceScriptTarget;

export type ReferenceScriptResolved = SDK.ReferenceScriptResolved;

export type ReferenceScriptPublicationBatchPlan = {
  readonly batchIndex: number;
  readonly targetNames: readonly string[];
  readonly targetCount: number;
  readonly requiredPlainBalance: bigint;
};

export type ReferenceScriptDeploymentPlan = {
  readonly scopeName: string;
  readonly existingTargetNames: readonly string[];
  readonly missingTargetNames: readonly string[];
  readonly batches: readonly ReferenceScriptPublicationBatchPlan[];
  readonly currentPlainBalance: bigint;
  readonly requiredPlainBalance: bigint;
  readonly topUpLovelace: bigint;
  readonly submitCount: number;
  readonly maxTargetsPerBatch: number;
};

export const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE =
  SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;

export const REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE = 50_000_000n;

export const REFERENCE_SCRIPT_PUBLICATION_DEFAULT_MAX_TARGETS_PER_BATCH = 4;

export const WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES = 24;

export const WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY = "5 seconds";

export const WALLET_OUTREF_RECONCILE_MAX_RETRIES = 4;

export const WALLET_OUTREF_RECONCILE_RETRY_DELAY = "750 millis";

const REFERENCE_SCRIPT_PROVIDER_FETCH_RETRY = {
  maxAttempts: 8,
  baseDelayMs: 750,
  maxDelayMs: 8_000,
  jitterRatio: 0.25,
} as const;

export const REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT =
  SDK.REFERENCE_SCRIPT_PER_TARGET_READ_LIMIT;

export const REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS = 30 * 60 * 1_000;

export const REFERENCE_SCRIPT_CONFIRMATION_OPTIONS = {
  confirmationTimeoutMs: REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  confirmationRetries: 0,
} as const;

export type ReferenceScriptWalletBucketSummary = {
  readonly utxoCount: number;
  readonly lovelace: bigint;
  readonly outRefs: readonly string[];
  readonly nonLovelaceAssetUnitCount: number;
};

export type ReferenceScriptWalletStatusSummary = {
  readonly referenceScriptsAddress: string;
  readonly total: ReferenceScriptWalletBucketSummary;
  readonly plainAdaOnly: ReferenceScriptWalletBucketSummary;
  readonly scriptRefOrTokenBearing: ReferenceScriptWalletBucketSummary;
  readonly otherIgnored: ReferenceScriptWalletBucketSummary;
  readonly sweepHint?: {
    readonly dryRunCommand: string;
    readonly executeCommand: string;
  };
};

export const isSameScriptRef = SDK.isSameScriptRef;

export const hasReferenceScriptAuthRole = SDK.hasReferenceScriptAuthRole;

export const acceptsReferenceScriptUtxo = SDK.acceptsReferenceScriptUtxo;

export const resolveReferenceScriptUtxo = SDK.resolveReferenceScriptUtxo;

export const fetchReferenceScriptUtxosAt = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  label: string,
  failureMessage: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  runProviderStepWithRetry(
    label,
    Effect.tryPromise({
      try: () => lucid.utxosAt(referenceScriptsAddress),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: failureMessage,
          cause,
        }),
    }),
    REFERENCE_SCRIPT_PROVIDER_FETCH_RETRY,
  );

/**
 * The SDK's resolution (each target read through its own role token, or the
 * wallet once for a large set), with every provider read under the node's
 * retry policy.
 */
export const fetchReferenceScriptUtxosProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  targets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  SDK.fetchReferenceScriptUtxosProgram(
    lucid,
    referenceScriptsAddress,
    targets,
    authPolicy,
    (label, read) =>
      runProviderStepWithRetry(
        label,
        read,
        REFERENCE_SCRIPT_PROVIDER_FETCH_RETRY,
      ),
  );

export const referenceScriptByName = (
  resolved: readonly ReferenceScriptResolved[],
  name: string,
): UTxO => {
  const found = resolved.find((candidate) => candidate.name === name);
  if (found === undefined) {
    throw new Error(`Missing resolved reference script: ${name}`);
  }
  return found.utxo;
};

export const utxoOutRefKey = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): string => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

export const filterPlainWalletUtxos = (
  utxos: readonly UTxO[],
): readonly UTxO[] => utxos.filter(isPlainAdaOnlyUtxo);

export const sumWalletLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + lovelaceOf(utxo), 0n);

const isScriptRefOrTokenBearingUtxo = (utxo: UTxO): boolean =>
  utxo.scriptRef !== undefined || hasPositiveNonLovelaceAsset(utxo);

const countNonLovelaceAssetUnits = (utxos: readonly UTxO[]): number =>
  new Set(
    utxos.flatMap((utxo) =>
      Object.entries(utxo.assets)
        .filter(([unit, amount]) => unit !== "lovelace" && amount > 0n)
        .map(([unit]) => unit),
    ),
  ).size;

const summarizeReferenceScriptWalletBucket = (
  utxos: readonly UTxO[],
): ReferenceScriptWalletBucketSummary => {
  const sorted = [...utxos].sort(compareOutRefs);
  return {
    utxoCount: sorted.length,
    lovelace: sumWalletLovelace(sorted),
    outRefs: sorted.map(outRefLabel),
    nonLovelaceAssetUnitCount: countNonLovelaceAssetUnits(sorted),
  };
};

const referenceScriptSweepHint =
  (): ReferenceScriptWalletStatusSummary["sweepHint"] => ({
    dryRunCommand:
      "node dist/index.js sweep-reference-script-wallet --retired-auth-policy <retired-policy-id>",
    executeCommand:
      "node dist/index.js sweep-reference-script-wallet --retired-auth-policy <retired-policy-id> --execute --i-am-retiring-reference-scripts",
  });

export const buildReferenceScriptWalletStatus = ({
  utxos,
  referenceScriptsAddress,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
}): ReferenceScriptWalletStatusSummary => {
  const plainAdaOnly = utxos.filter(isPlainAdaOnlyUtxo);
  const scriptRefOrTokenBearing = utxos.filter(isScriptRefOrTokenBearingUtxo);
  const accounted = new Set([
    ...plainAdaOnly.map(utxoOutRefKey),
    ...scriptRefOrTokenBearing.map(utxoOutRefKey),
  ]);
  const otherIgnored = utxos.filter(
    (utxo) => !accounted.has(utxoOutRefKey(utxo)),
  );
  const trappedSummary = summarizeReferenceScriptWalletBucket(
    scriptRefOrTokenBearing,
  );
  return {
    referenceScriptsAddress,
    total: summarizeReferenceScriptWalletBucket(utxos),
    plainAdaOnly: summarizeReferenceScriptWalletBucket(plainAdaOnly),
    scriptRefOrTokenBearing: trappedSummary,
    otherIgnored: summarizeReferenceScriptWalletBucket(otherIgnored),
    ...(trappedSummary.lovelace > 0n
      ? { sweepHint: referenceScriptSweepHint() }
      : {}),
  };
};

export const formatReferenceScriptWalletStatusCause = (
  status: ReferenceScriptWalletStatusSummary,
): string =>
  [
    `reference_script_wallet=${status.referenceScriptsAddress}`,
    `total_lovelace=${status.total.lovelace.toString()}`,
    `plain_ada_lovelace=${status.plainAdaOnly.lovelace.toString()}`,
    `plain_ada_utxos=${status.plainAdaOnly.utxoCount.toString()}`,
    `scriptref_or_token_lovelace=${status.scriptRefOrTokenBearing.lovelace.toString()}`,
    `scriptref_or_token_utxos=${status.scriptRefOrTokenBearing.utxoCount.toString()}`,
    status.sweepHint === undefined
      ? "sweep_hint=none"
      : `sweep_hint_dry_run="${status.sweepHint.dryRunCommand}",sweep_hint_execute="${status.sweepHint.executeCommand}"`,
  ].join(",");

export const resolveReferenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint => SDK.referenceScriptPublicationFundingTarget(missingTargetCount);

export const selectWalletFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] =>
  SDK.selectReferenceScriptFundingUtxos(utxos, targetLovelace);
