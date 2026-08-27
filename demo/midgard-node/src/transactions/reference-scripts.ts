import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assertReferenceScriptAuthMinimumRemaining,
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  type ReferenceScriptAuthMintingPolicy,
  type ReferenceScriptAuthPolicyRef,
  referenceScriptAuthTokenNameText,
} from "@al-ft/midgard-sdk";
import {
  type Assets,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@/deployment-manifest-v1.js";
import { loadPhasMembershipWithdrawalScript } from "@/phas-membership.js";
import { runProviderStepWithRetry } from "@/provider-retry.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "@/transactions/utils.js";
import {
  hasPositiveNonLovelaceAsset,
  isPlainAdaOnlyUtxo,
  lovelaceOf,
} from "@/transactions/wallet-hygiene.js";
import { compareOutRefs, outRefLabel } from "@/tx-context.js";

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

const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE =
  SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;
const REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE = 50_000_000n;
export const REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE = 3_000_000n;
export const REFERENCE_SCRIPT_SWEEP_DEFAULT_MAX_ASSETS_PER_TOKEN_OUTPUT = 32;
const REFERENCE_SCRIPT_PUBLICATION_BALANCE_INSUFFICIENT_PATTERN =
  /UTxO Balance Insufficient/i;
const REFERENCE_SCRIPT_PUBLICATION_TX_SIZE_EXCEEDED_PATTERN =
  /Max transaction size of \d+ exceeded\. Found: \d+/i;
const REFERENCE_SCRIPT_PUBLICATION_BALANCE_GAP_PATTERN =
  /Inputs:\s*Value\s*\{\s*coin:\s*(\d+)[\s\S]*?Outputs:\s*Value\s*\{\s*coin:\s*(\d+)/i;
const REFERENCE_SCRIPT_PUBLICATION_DEFAULT_MAX_TARGETS_PER_BATCH = 4;
const WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES = 24;
const WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY = "5 seconds";
const WALLET_OUTREF_RECONCILE_MAX_RETRIES = 4;
const WALLET_OUTREF_RECONCILE_RETRY_DELAY = "750 millis";
const REFERENCE_SCRIPT_PROVIDER_FETCH_RETRY = {
  maxAttempts: 8,
  baseDelayMs: 750,
  maxDelayMs: 8_000,
  jitterRatio: 0.25,
} as const;
export const REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS = 30 * 60 * 1_000;
const REFERENCE_SCRIPT_CONFIRMATION_OPTIONS = {
  confirmationTimeoutMs: REFERENCE_SCRIPT_CONFIRMATION_TIMEOUT_MS,
  confirmationRetries: 0,
} as const;

export const REFERENCE_SCRIPT_COMMAND_NAMES = [
  "node-runtime",
  "protocol-init",
  "reference-script-auth",
  "hub-oracle",
  "da",
  "state-queue",
  "scheduler",
  "registered-operators",
  "active-operators",
  "retired-operators",
  "deposit",
  "withdrawal",
  "settlement",
  "phas-membership",
  "reserve",
  "payout",
] as const;

export type ReferenceScriptCommandName =
  (typeof REFERENCE_SCRIPT_COMMAND_NAMES)[number];

export type ReferenceScriptSweepTokenOutputSummary = {
  readonly nonLovelaceAssetCount: number;
  readonly lovelace: bigint;
  readonly units: readonly string[];
};

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

export type ReferenceScriptSweepSummary = {
  readonly dryRun: boolean;
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly burnAddress?: string;
  readonly includePlainUtxos: boolean;
  readonly sweepableUtxoCount: number;
  readonly retainedUtxoCount: number;
  readonly inputLovelace: bigint;
  readonly tokenOutputLovelace: bigint;
  readonly quarantineLovelace: bigint;
  readonly nonLovelaceAssetCount: number;
  readonly nonLovelaceAssets: Readonly<Assets>;
  readonly tokenOutputs: readonly ReferenceScriptSweepTokenOutputSummary[];
  readonly sweepableOutRefs: readonly string[];
  readonly retainedOutRefs: readonly string[];
  readonly walletStatus: ReferenceScriptWalletStatusSummary;
  readonly txHash?: string;
};

export type ReferenceScriptSweepPlan = {
  readonly summary: ReferenceScriptSweepSummary;
  readonly sweepableUtxos: readonly UTxO[];
  readonly tokenOutputs: readonly Readonly<Assets>[];
};

export type ReferenceScriptSweepOptions = {
  readonly burnAddress?: string;
  readonly execute?: boolean;
  readonly acknowledgeRetirement?: boolean;
  readonly includePlainUtxos?: boolean;
  readonly tokenOutputLovelace?: bigint;
  readonly maxAssetsPerTokenOutput?: number;
};

export const isSameScriptRef = SDK.isSameScriptRef;

export const hasReferenceScriptAuthRole = SDK.hasReferenceScriptAuthRole;

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

export const fetchReferenceScriptUtxosProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  targets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const referenceScriptUtxos = yield* fetchReferenceScriptUtxosAt(
      lucid,
      referenceScriptsAddress,
      `reference-script UTxO fetch at ${referenceScriptsAddress}`,
      `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress}`,
    );
    return yield* Effect.forEach(targets, (target) =>
      Effect.gen(function* () {
        const resolved = [...referenceScriptUtxos]
          .filter(
            (utxo) =>
              isSameScriptRef(utxo.scriptRef, target.script) &&
              hasReferenceScriptAuthRole(utxo, target, authPolicy),
          )
          .sort(compareOutRefs)[0];
        if (resolved === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message: "Missing reference script",
              cause: `${target.name} at ${referenceScriptsAddress} with role token ${referenceScriptAuthTokenNameText(
                target.name,
              )}`,
            }),
          );
        }
        return {
          name: target.name,
          utxo: resolved,
        };
      }),
    );
  }).pipe(
    Effect.mapError((cause) =>
      cause instanceof SDK.StateQueueError
        ? cause
        : new SDK.StateQueueError({
            message: "Failed to resolve required reference scripts",
            cause,
          }),
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

const isReferenceScriptPublicationTxTooLarge = (cause: unknown): boolean =>
  REFERENCE_SCRIPT_PUBLICATION_TX_SIZE_EXCEEDED_PATTERN.test(
    formatUnknownError(cause),
  );

const isReferenceScriptPublicationBalanceInsufficient = (
  cause: unknown,
): boolean =>
  REFERENCE_SCRIPT_PUBLICATION_BALANCE_INSUFFICIENT_PATTERN.test(
    formatUnknownError(cause),
  );

const resolveReferenceScriptPublicationAdditionalFunding = (
  cause: unknown,
): bigint | undefined => {
  const match = REFERENCE_SCRIPT_PUBLICATION_BALANCE_GAP_PATTERN.exec(
    formatUnknownError(cause),
  );
  if (match === null) {
    return undefined;
  }
  const inputs = BigInt(match[1]);
  const outputs = BigInt(match[2]);
  if (outputs <= inputs) {
    return undefined;
  }
  return outputs - inputs + SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;
};

export const utxoOutRefKey = (utxo: UTxO): string =>
  `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const filterPlainWalletUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  utxos.filter(isPlainAdaOnlyUtxo);

const sumWalletLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((total, utxo) => total + lovelaceOf(utxo), 0n);

export const isReferenceScriptSweepCandidate = (utxo: UTxO): boolean =>
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
    dryRunCommand: "node dist/index.js sweep-reference-script-wallet",
    executeCommand:
      "node dist/index.js sweep-reference-script-wallet --execute --i-am-retiring-reference-scripts --burn-address <quarantine-address>",
  });

export const buildReferenceScriptWalletStatus = ({
  utxos,
  referenceScriptsAddress,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
}): ReferenceScriptWalletStatusSummary => {
  const plainAdaOnly = utxos.filter(isPlainAdaOnlyUtxo);
  const scriptRefOrTokenBearing = utxos.filter(isReferenceScriptSweepCandidate);
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

const formatReferenceScriptWalletStatusCause = (
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

const addPositiveAsset = (
  assets: Assets,
  unit: string,
  amount: bigint,
): void => {
  if (amount <= 0n) {
    return;
  }
  assets[unit] = (assets[unit] ?? 0n) + amount;
};

const sumNonLovelaceAssets = (utxos: readonly UTxO[]): Assets => {
  const totals: Assets = {};
  for (const utxo of utxos) {
    for (const [unit, amount] of Object.entries(utxo.assets)) {
      if (unit !== "lovelace") {
        addPositiveAsset(totals, unit, amount);
      }
    }
  }
  return totals;
};

const sortedPositiveAssetEntries = (
  assets: Readonly<Assets>,
): readonly (readonly [string, bigint])[] =>
  Object.entries(assets)
    .filter(([, amount]) => amount > 0n)
    .sort(([leftUnit], [rightUnit]) => leftUnit.localeCompare(rightUnit));

const chunkNonLovelaceAssets = (
  assets: Readonly<Assets>,
  maxAssetsPerOutput: number,
  tokenOutputLovelace: bigint,
): readonly Readonly<Assets>[] => {
  const entries = sortedPositiveAssetEntries(assets);
  const outputs: Assets[] = [];
  for (let index = 0; index < entries.length; index += maxAssetsPerOutput) {
    const outputAssets: Assets = { lovelace: tokenOutputLovelace };
    for (const [unit, amount] of entries.slice(
      index,
      index + maxAssetsPerOutput,
    )) {
      outputAssets[unit] = amount;
    }
    outputs.push(outputAssets);
  }
  return outputs;
};

const summarizeTokenOutput = (
  outputAssets: Readonly<Assets>,
): ReferenceScriptSweepTokenOutputSummary => {
  const entries = sortedPositiveAssetEntries(outputAssets).filter(
    ([unit]) => unit !== "lovelace",
  );
  return {
    nonLovelaceAssetCount: entries.length,
    lovelace: outputAssets.lovelace ?? 0n,
    units: entries.map(([unit]) => unit),
  };
};

export const buildReferenceScriptSweepPlan = ({
  utxos,
  referenceScriptsAddress,
  returnAddress,
  burnAddress,
  dryRun,
  includePlainUtxos = false,
  tokenOutputLovelace = REFERENCE_SCRIPT_SWEEP_DEFAULT_TOKEN_OUTPUT_LOVELACE,
  maxAssetsPerTokenOutput = REFERENCE_SCRIPT_SWEEP_DEFAULT_MAX_ASSETS_PER_TOKEN_OUTPUT,
}: {
  readonly utxos: readonly UTxO[];
  readonly referenceScriptsAddress: string;
  readonly returnAddress: string;
  readonly burnAddress?: string;
  readonly dryRun: boolean;
  readonly includePlainUtxos?: boolean;
  readonly tokenOutputLovelace?: bigint;
  readonly maxAssetsPerTokenOutput?: number;
}): ReferenceScriptSweepPlan => {
  if (tokenOutputLovelace <= 0n) {
    throw new Error("tokenOutputLovelace must be greater than zero");
  }
  if (
    !Number.isSafeInteger(maxAssetsPerTokenOutput) ||
    maxAssetsPerTokenOutput <= 0
  ) {
    throw new Error("maxAssetsPerTokenOutput must be a safe positive integer");
  }
  const sweepableUtxos = [...utxos]
    .filter((utxo) =>
      includePlainUtxos ? true : isReferenceScriptSweepCandidate(utxo),
    )
    .sort(compareOutRefs);
  const sweepableOutRefs = new Set(sweepableUtxos.map(utxoOutRefKey));
  const retainedUtxos = [...utxos]
    .filter((utxo) => !sweepableOutRefs.has(utxoOutRefKey(utxo)))
    .sort(compareOutRefs);
  const nonLovelaceAssets = sumNonLovelaceAssets(sweepableUtxos);
  const tokenOutputs = chunkNonLovelaceAssets(
    nonLovelaceAssets,
    maxAssetsPerTokenOutput,
    tokenOutputLovelace,
  );
  const quarantineLovelace = BigInt(tokenOutputs.length) * tokenOutputLovelace;
  const walletStatus = buildReferenceScriptWalletStatus({
    utxos,
    referenceScriptsAddress,
  });

  return {
    summary: {
      dryRun,
      referenceScriptsAddress,
      returnAddress,
      ...(burnAddress === undefined ? {} : { burnAddress }),
      includePlainUtxos,
      sweepableUtxoCount: sweepableUtxos.length,
      retainedUtxoCount: retainedUtxos.length,
      inputLovelace: sumWalletLovelace(sweepableUtxos),
      tokenOutputLovelace,
      quarantineLovelace,
      nonLovelaceAssetCount:
        sortedPositiveAssetEntries(nonLovelaceAssets).length,
      nonLovelaceAssets,
      tokenOutputs: tokenOutputs.map(summarizeTokenOutput),
      sweepableOutRefs: sweepableUtxos.map(outRefLabel),
      retainedOutRefs: retainedUtxos.map(outRefLabel),
      walletStatus,
    },
    sweepableUtxos,
    tokenOutputs,
  };
};

const resolveReferenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint => SDK.referenceScriptPublicationFundingTarget(missingTargetCount);

const orderWalletFundingUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  SDK.orderReferenceScriptFundingUtxos(utxos);

export const selectWalletFundingUtxos = (
  utxos: readonly UTxO[],
  targetLovelace: bigint,
): readonly UTxO[] =>
  SDK.selectReferenceScriptFundingUtxos(utxos, targetLovelace);

export const buildReferenceScriptDeploymentPlan = ({
  scopeName,
  targets,
  existingTargetNames,
  walletUtxos,
  maxTargetsPerBatch = REFERENCE_SCRIPT_PUBLICATION_DEFAULT_MAX_TARGETS_PER_BATCH,
}: {
  readonly scopeName: string;
  readonly targets: readonly ReferenceScriptTarget[];
  readonly existingTargetNames: ReadonlySet<string>;
  readonly walletUtxos: readonly UTxO[];
  readonly maxTargetsPerBatch?: number;
}): ReferenceScriptDeploymentPlan => {
  if (!Number.isSafeInteger(maxTargetsPerBatch) || maxTargetsPerBatch <= 0) {
    throw new Error("maxTargetsPerBatch must be a safe positive integer");
  }
  const missingTargets = targets.filter(
    (target) => !existingTargetNames.has(target.name),
  );
  const currentPlainBalance = sumWalletLovelace(
    filterPlainWalletUtxos(walletUtxos),
  );
  const requiredPlainBalance =
    missingTargets.length === 0
      ? 0n
      : resolveReferenceScriptPublicationFundingTarget(missingTargets.length) >
          REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE
        ? resolveReferenceScriptPublicationFundingTarget(missingTargets.length)
        : REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE;
  const batches: ReferenceScriptPublicationBatchPlan[] = [];
  for (
    let index = 0;
    index < missingTargets.length;
    index += maxTargetsPerBatch
  ) {
    const batchTargets = missingTargets.slice(
      index,
      index + maxTargetsPerBatch,
    );
    batches.push({
      batchIndex: batches.length,
      targetNames: batchTargets.map(({ name }) => name),
      targetCount: batchTargets.length,
      requiredPlainBalance: resolveReferenceScriptPublicationFundingTarget(
        batchTargets.length,
      ),
    });
  }
  return {
    scopeName,
    existingTargetNames: targets
      .filter((target) => existingTargetNames.has(target.name))
      .map(({ name }) => name),
    missingTargetNames: missingTargets.map(({ name }) => name),
    batches,
    currentPlainBalance,
    requiredPlainBalance,
    topUpLovelace:
      currentPlainBalance >= requiredPlainBalance
        ? 0n
        : requiredPlainBalance - currentPlainBalance,
    submitCount: batches.length,
    maxTargetsPerBatch,
  };
};

const mergeWalletUtxosPreservingScriptRefs = (
  liveUtxos: readonly UTxO[],
  cachedUtxos: readonly UTxO[],
): readonly UTxO[] => {
  const cachedByOutRef = new Map(
    cachedUtxos.map((utxo) => [utxoOutRefKey(utxo), utxo]),
  );
  return liveUtxos.map((utxo) => {
    if (utxo.scriptRef !== undefined) {
      return utxo;
    }
    const cached = cachedByOutRef.get(utxoOutRefKey(utxo));
    if (cached?.scriptRef === undefined) {
      return utxo;
    }
    return {
      ...utxo,
      scriptRef: cached.scriptRef,
    };
  });
};

const reconcileLiveWalletUtxos = (
  lucid: LucidEvolution,
  utxos: readonly UTxO[],
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    if (utxos.length === 0) {
      return [];
    }
    const uniqueOutRefs = Array.from(
      new Map(
        utxos.map((utxo) => [
          utxoOutRefKey(utxo),
          {
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          },
        ]),
      ).values(),
    );
    let live: readonly UTxO[] | null = null;
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OUTREF_RECONCILE_MAX_RETRIES;
      attempt += 1
    ) {
      const liveAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosByOutRef(uniqueOutRefs),
          catch: (cause) => cause,
        }),
      );
      if (liveAttempt._tag === "Right") {
        live = liveAttempt.right;
        break;
      }
      lastCause = liveAttempt.left;
      if (attempt + 1 < WALLET_OUTREF_RECONCILE_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet UTxO out-ref reconciliation failed (attempt ${(attempt + 1).toString()}/${WALLET_OUTREF_RECONCILE_MAX_RETRIES.toString()}); retrying in ${WALLET_OUTREF_RECONCILE_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OUTREF_RECONCILE_RETRY_DELAY);
      }
    }
    if (live === null) {
      yield* Effect.logWarning(
        `Wallet UTxO out-ref reconciliation exhausted retries; using wallet snapshot as fallback. attempts=${WALLET_OUTREF_RECONCILE_MAX_RETRIES.toString()},last_cause=${String(lastCause)}`,
      );
      return utxos;
    }
    if (utxos.length > 0 && live.length === 0) {
      yield* Effect.logWarning(
        "Wallet UTxO out-ref reconciliation returned zero live entries from a non-empty snapshot; keeping wallet snapshot to avoid false empty-input failures.",
      );
      return utxos;
    }
    return mergeWalletUtxosPreservingScriptRefs(live, utxos);
  });

const fetchReconciledWalletUtxos = (
  lucid: LucidEvolution,
  failureMessage: string,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxosRaw = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: failureMessage,
          cause,
        }),
    });
    return yield* reconcileLiveWalletUtxos(lucid, walletUtxosRaw);
  });

const refreshWalletUtxosFromOwnAddress = (
  lucid: LucidEvolution,
  {
    scopeName,
    failureMessage,
    minimumPlainBalance,
  }: {
    readonly scopeName: string;
    readonly failureMessage: string;
    readonly minimumPlainBalance?: bigint;
  },
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => lucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve wallet address while refreshing ${scopeName}`,
          cause,
        }),
    });
    const cachedWalletUtxos = yield* Effect.tryPromise({
      try: () => lucid.wallet().getUtxos(),
      catch: () => [] as readonly UTxO[],
    }).pipe(Effect.catchAll(() => Effect.succeed([] as readonly UTxO[])));

    let refreshedWalletUtxos: readonly UTxO[] | null = null;
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES;
      attempt += 1
    ) {
      const atAddressAttempt = yield* Effect.either(
        Effect.tryPromise({
          try: () => lucid.utxosAt(walletAddress),
          catch: (cause) => cause,
        }),
      );
      if (atAddressAttempt._tag === "Right") {
        const mergedWalletUtxos = mergeWalletUtxosPreservingScriptRefs(
          atAddressAttempt.right,
          cachedWalletUtxos,
        );
        yield* Effect.sync(() => lucid.overrideUTxOs([...mergedWalletUtxos]));
        const plainBalance = sumWalletLovelace(
          filterPlainWalletUtxos(mergedWalletUtxos),
        );
        if (
          minimumPlainBalance === undefined ||
          plainBalance >= minimumPlainBalance
        ) {
          refreshedWalletUtxos = mergedWalletUtxos;
          break;
        }
        lastCause = `wallet_address=${walletAddress},plain_balance=${plainBalance.toString()},required_plain_balance=${minimumPlainBalance.toString()}`;
      } else {
        lastCause = atAddressAttempt.left;
      }

      if (attempt + 1 < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet own-address refresh for ${scopeName} did not reach the required state (attempt ${(attempt + 1).toString()}/${WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES.toString()}); retrying in ${WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY);
      }
    }

    if (refreshedWalletUtxos !== null) {
      return refreshedWalletUtxos;
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: failureMessage,
        cause: lastCause ?? `wallet_address=${walletAddress}`,
      }),
    );
  });

export const resolveSpendableWalletUtxos = (
  lucid: LucidEvolution,
  excludedOutRefKeys: ReadonlySet<string>,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletUtxos = yield* fetchReconciledWalletUtxos(
      lucid,
      "Failed to fetch wallet UTxOs for transaction input preset",
    );
    // Reference-script publications must remain available for `.readFrom(...)`.
    return filterPlainWalletUtxos(walletUtxos).filter(
      (utxo) => !excludedOutRefKeys.has(utxoOutRefKey(utxo)),
    );
  });

const resolveLiveWalletUtxo = (
  lucid: LucidEvolution,
  utxo: UTxO,
): Effect.Effect<UTxO | undefined, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const resolved = yield* Effect.tryPromise({
      try: () =>
        lucid.utxosByOutRef([
          {
            txHash: utxo.txHash,
            outputIndex: utxo.outputIndex,
          },
        ]),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to resolve wallet UTxO by out-ref while validating script reference",
          cause,
        }),
    });
    const live = resolved.find(
      (candidate) =>
        candidate.txHash === utxo.txHash &&
        candidate.outputIndex === utxo.outputIndex,
    );
    if (live === undefined) {
      return undefined;
    }
    if (live.scriptRef === undefined && utxo.scriptRef !== undefined) {
      return {
        ...live,
        scriptRef: utxo.scriptRef,
      };
    }
    return live;
  });

const resolveExistingReferenceScriptPublication = (
  lucid: LucidEvolution,
  referenceScriptUtxos: readonly UTxO[],
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<ReferenceScriptResolved | undefined, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const existingCandidates = referenceScriptUtxos
      .filter(
        (utxo) =>
          isSameScriptRef(utxo.scriptRef, target.script) &&
          hasReferenceScriptAuthRole(utxo, target, authPolicy),
      )
      .sort(compareOutRefs)
      .reverse();
    for (const existingCandidate of existingCandidates) {
      const existing = yield* resolveLiveWalletUtxo(lucid, existingCandidate);
      if (
        existing !== undefined &&
        isSameScriptRef(existing.scriptRef, target.script) &&
        hasReferenceScriptAuthRole(existing, target, authPolicy)
      ) {
        return {
          name: target.name,
          utxo: existing,
        };
      }
    }
    return undefined;
  });

const ensureReferenceScriptWalletWorkingCapital = (
  fundingLucid: LucidEvolution,
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  requiredPlainBalance: bigint,
  reservedFundingOutRefKeys: ReadonlySet<string> = new Set<string>(),
): Effect.Effect<
  void,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    const referenceScriptWalletUtxos = yield* refreshWalletUtxosFromOwnAddress(
      referenceScriptsLucid,
      {
        scopeName: `${scopeName} reference scripts`,
        failureMessage: `Failed to fetch wallet UTxOs while preparing ${scopeName} reference scripts`,
      },
    );
    const currentPlainBalance = sumWalletLovelace(
      filterPlainWalletUtxos(referenceScriptWalletUtxos),
    );
    const targetPlainBalance =
      requiredPlainBalance > REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE
        ? requiredPlainBalance
        : REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE;
    if (currentPlainBalance >= targetPlainBalance) {
      return;
    }

    const referenceScriptAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while preparing ${scopeName} reference scripts`,
          cause,
        }),
    });
    const walletStatus = buildReferenceScriptWalletStatus({
      utxos: referenceScriptWalletUtxos,
      referenceScriptsAddress: referenceScriptAddress,
    });
    const fundingAddress = yield* Effect.tryPromise({
      try: () => fundingLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve funding wallet address while preparing ${scopeName} reference scripts`,
          cause,
        }),
    });
    if (fundingAddress === referenceScriptAddress) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Reference-script wallet plain balance is below the required working-capital floor while preparing ${scopeName} reference scripts`,
          cause: `plain_balance=${currentPlainBalance.toString()},required=${targetPlainBalance.toString()},wallet_address=${referenceScriptAddress},reason=same-wallet-funding-would-risk-scriptref-spend,${formatReferenceScriptWalletStatusCause(walletStatus)}`,
        }),
      );
    }

    const topUpAmount = targetPlainBalance - currentPlainBalance;
    const fundingInputs = yield* resolveSpendableWalletUtxos(
      fundingLucid,
      reservedFundingOutRefKeys,
    );
    if (fundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `No operator wallet funding UTxOs available to replenish ${scopeName} reference scripts`,
          cause: `reference_script_wallet=${referenceScriptAddress},required_top_up=${topUpAmount.toString()},reserved_funding_outrefs=[${[
            ...reservedFundingOutRefKeys,
          ].join(
            ",",
          )}],${formatReferenceScriptWalletStatusCause(walletStatus)}`,
        }),
      );
    }
    const selectedFundingInputs = selectWalletFundingUtxos(
      fundingInputs,
      topUpAmount + SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
    );
    if (selectedFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Failed to select operator wallet funding UTxOs to replenish ${scopeName} reference scripts`,
          cause: `reference_script_wallet=${referenceScriptAddress},required_top_up=${topUpAmount.toString()},reserved_funding_outrefs=[${[
            ...reservedFundingOutRefKeys,
          ].join(
            ",",
          )}],${formatReferenceScriptWalletStatusCause(walletStatus)}`,
        }),
      );
    }

    yield* Effect.logInfo(
      `Replenishing reference-script wallet for ${scopeName}: current_plain_balance=${currentPlainBalance.toString()},target_plain_balance=${targetPlainBalance.toString()},top_up_amount=${topUpAmount.toString()},reserved_funding_outrefs=[${[
        ...reservedFundingOutRefKeys,
      ].join(",")}],${formatReferenceScriptWalletStatusCause(walletStatus)}`,
    );
    const unsigned =
      yield* SDK.completeReferenceScriptWalletReplenishmentTxProgram({
        lucid: fundingLucid,
        selectedFundingInputs,
        referenceScriptAddress,
        topUpAmount,
      });
    const txHash = yield* handleSignSubmit(
      fundingLucid,
      unsigned,
      REFERENCE_SCRIPT_CONFIRMATION_OPTIONS,
    );
    yield* refreshWalletUtxosFromOwnAddress(referenceScriptsLucid, {
      scopeName: `${scopeName} reference scripts after replenishment`,
      failureMessage: `Failed to refresh reference-script wallet after replenishing ${scopeName} reference scripts`,
      minimumPlainBalance: targetPlainBalance,
    });
    yield* Effect.logInfo(
      `Reference-script wallet replenishment confirmed for ${scopeName}: txHash=${txHash},top_up_amount=${topUpAmount.toString()}`,
    );
  });

const publishMissingReferenceScriptTargets = (
  lucid: LucidEvolution,
  walletAddress: string,
  referenceScriptsAddress: string,
  walletUtxos: readonly UTxO[],
  fundingCandidateUtxos: readonly UTxO[],
  missingTargets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthMintingPolicy,
): Effect.Effect<
  readonly ReferenceScriptResolved[],
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    const orderedFundingCandidates = orderWalletFundingUtxos(
      fundingCandidateUtxos,
    );
    let selectedFundingInputs = selectWalletFundingUtxos(
      fundingCandidateUtxos,
      resolveReferenceScriptPublicationFundingTarget(missingTargets.length),
    );
    if (selectedFundingInputs.length === 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to select non-reference wallet UTxOs for reference-script publication",
          cause: missingTargets.map(({ name }) => name).join(","),
        }),
      );
    }
    let nextFundingCandidateIndex = selectedFundingInputs.length;
    const [txHash, localReferenceOutputs, walletOutputs] = yield* Effect.gen(
      function* () {
        while (true) {
          yield* Effect.sync(() =>
            lucid.overrideUTxOs([...selectedFundingInputs]),
          );
          const buildAttempt = yield* Effect.either(
            Effect.gen(function* () {
              const { tx: unsigned, layout } =
                yield* SDK.completeReferenceScriptPublicationTxProgram({
                  lucid,
                  selectedFundingInputs,
                  walletAddress,
                  referenceScriptsAddress,
                  missingTargets,
                  authPolicy,
                });
              const txHash = yield* handleSignSubmit(
                lucid,
                unsigned,
                REFERENCE_SCRIPT_CONFIRMATION_OPTIONS,
              );
              return [
                txHash,
                layout.localReferenceOutputs,
                layout.walletOutputs,
              ] as const;
            }),
          ).pipe(
            Effect.ensuring(
              Effect.sync(() => lucid.overrideUTxOs([...walletUtxos])),
            ),
          );

          if (buildAttempt._tag === "Right") {
            return buildAttempt.right;
          }
          if (
            !isReferenceScriptPublicationBalanceInsufficient(
              buildAttempt.left,
            ) ||
            nextFundingCandidateIndex >= orderedFundingCandidates.length
          ) {
            return yield* Effect.fail(buildAttempt.left);
          }

          const nextFundingInput =
            orderedFundingCandidates[nextFundingCandidateIndex];
          nextFundingCandidateIndex += 1;
          selectedFundingInputs = [...selectedFundingInputs, nextFundingInput];
          yield* Effect.logWarning(
            `Reference-script publication for ${missingTargets
              .map(({ name }) => name)
              .join(
                ", ",
              )} needed more wallet funding than the seed estimate; retrying with ${selectedFundingInputs.length.toString()} funding input(s).`,
          );
        }
      },
    );
    const restoredWalletUtxos = [
      ...walletUtxos.filter(
        (utxo) =>
          !selectedFundingInputs.some(
            (selected) =>
              selected.txHash === utxo.txHash &&
              selected.outputIndex === utxo.outputIndex,
          ),
      ),
      ...walletOutputs.map((output) => ({
        txHash,
        ...output,
      })),
    ];
    yield* Effect.sync(() => lucid.overrideUTxOs(restoredWalletUtxos));
    return yield* Effect.forEach(missingTargets, (target) =>
      Effect.gen(function* () {
        const localReferenceOutput = localReferenceOutputs.get(target.name);
        if (localReferenceOutput === undefined) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Reference-script publication transaction did not contain the expected script-ref output",
              cause: `${target.name},txHash=${txHash}`,
            }),
          );
        }
        const localReferenceUtxo: UTxO = {
          txHash,
          ...localReferenceOutput,
        };
        const liveReference = yield* Effect.either(
          resolveLiveWalletUtxo(lucid, localReferenceUtxo),
        );
        if (
          liveReference._tag === "Right" &&
          liveReference.right !== undefined &&
          isSameScriptRef(liveReference.right.scriptRef, target.script) &&
          hasReferenceScriptAuthRole(liveReference.right, target, authPolicy)
        ) {
          return {
            name: target.name,
            utxo: liveReference.right,
          };
        }
        if (
          liveReference._tag === "Right" &&
          liveReference.right !== undefined &&
          hasReferenceScriptAuthRole(liveReference.right, target, authPolicy)
        ) {
          return {
            name: target.name,
            utxo: {
              ...liveReference.right,
              scriptRef: target.script,
            },
          };
        }
        return {
          name: target.name,
          utxo: {
            ...localReferenceUtxo,
            scriptRef: target.script,
          },
        };
      }),
    );
  });

const REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES = [
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "missingNativeScriptTx",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "crossBlockDuplicateEvent",
  "l2TxMistag",
  "withdrawnInput",
] as const satisfies readonly (keyof SDK.FaultProofContracts)[];

const TRANSITION_TRACE_FINAL_CONTRACT_NAMES = [
  "fraudProofTransitionTraceControl",
  "fraudProofTransitionTraceSource",
  "fraudProofTransitionTraceWithdrawal",
  "fraudProofTransitionTraceForced",
  "fraudProofTransitionTraceAcceptedTransaction",
  "fraudProofTransitionTraceDeposit",
  "fraudProofTransitionTraceL1Event",
  "fraudProofTransitionTraceDuplicate",
] as const;

const REFERENCE_SCRIPT_ROLE_BY_CONTRACT_NAME: ReadonlyMap<string, string> =
  new Map<string, string>(
    Object.entries(
      DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
    ).map(([role, contractName]) => [contractName, role] as const),
  );

const upperFirst = (value: string): string =>
  `${value.slice(0, 1).toUpperCase()}${value.slice(1)}`;

const faultProofStepContractName = (
  category: (typeof REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES)[number],
  stepIndex: number,
): string =>
  `fraudProof${upperFirst(category)}${
    stepIndex === 0 ? "" : `Step${(stepIndex + 1).toString().padStart(2, "0")}`
  }`;

const manifestReferenceScriptTarget = (
  contractName: string,
  script: SDK.ReferenceScriptTarget["script"],
): ReferenceScriptTarget => {
  const name = REFERENCE_SCRIPT_ROLE_BY_CONTRACT_NAME.get(contractName);
  if (name === undefined) {
    throw new Error(
      `Contract is missing a canonical reference-script role: ${contractName}`,
    );
  }
  referenceScriptAuthTokenNameText(name);
  return { name, script };
};

const registeredFraudProofReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly ReferenceScriptTarget[] => [
  ...REGISTERED_LINEAR_FAULT_PROOF_CATEGORIES.flatMap((category) =>
    contracts.fraudProofContracts[category].steps.map((validator, stepIndex) =>
      manifestReferenceScriptTarget(
        faultProofStepContractName(category, stepIndex),
        validator.spendingScript,
      ),
    ),
  ),
  manifestReferenceScriptTarget(
    "fraudProofTransitionTrace",
    contracts.fraudProofContracts.transitionTrace.route.spendingScript,
  ),
  ...contracts.fraudProofContracts.transitionTrace.finals.map(
    (validator, index) =>
      manifestReferenceScriptTarget(
        TRANSITION_TRACE_FINAL_CONTRACT_NAMES[index],
        validator.spendingScript,
      ),
  ),
];

export const nodeRuntimeReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly ReferenceScriptTarget[] => [
  {
    name: "reference-script-auth minting",
    script: contracts.referenceScriptAuth.mintingScript,
  },
  {
    name: "hub-oracle minting",
    script: contracts.hubOracle.mintingScript,
  },
  {
    name: "da-params-governor spending",
    script: contracts.daParamsGovernor.spendingScript,
  },
  {
    name: "da-params-governor minting",
    script: contracts.daParamsGovernor.mintingScript,
  },
  {
    name: "da-attestation spending",
    script: contracts.daAttestation.spendingScript,
  },
  {
    name: "da-attestation minting",
    script: contracts.daAttestation.mintingScript,
  },
  {
    name: "scheduler spending",
    script: contracts.scheduler.spendingScript,
  },
  {
    name: "scheduler minting",
    script: contracts.scheduler.mintingScript,
  },
  {
    name: "state-queue spending",
    script: contracts.stateQueue.spendingScript,
  },
  {
    name: "state-queue minting",
    script: contracts.stateQueue.mintingScript,
  },
  {
    name: "registered-operators spending",
    script: contracts.registeredOperators.spendingScript,
  },
  {
    name: "registered-operators minting",
    script: contracts.registeredOperators.mintingScript,
  },
  {
    name: "active-operators spending",
    script: contracts.activeOperators.spendingScript,
  },
  {
    name: "active-operators minting",
    script: contracts.activeOperators.mintingScript,
  },
  {
    name: "retired-operators spending",
    script: contracts.retiredOperators.spendingScript,
  },
  {
    name: "retired-operators minting",
    script: contracts.retiredOperators.mintingScript,
  },
  {
    name: "fraud-proof-catalogue minting",
    script: contracts.fraudProofCatalogue.mintingScript,
  },
  {
    name: "deposit minting",
    script: contracts.deposit.mintingScript,
  },
  {
    name: "deposit spending",
    script: contracts.deposit.spendingScript,
  },
  {
    name: "withdrawal minting",
    script: contracts.withdrawal.mintingScript,
  },
  {
    name: "withdrawal spending",
    script: contracts.withdrawal.spendingScript,
  },
  {
    name: "settlement minting",
    script: contracts.settlement.mintingScript,
  },
  {
    name: "membership proof withdrawal",
    script: loadPhasMembershipWithdrawalScript(),
  },
  {
    name: "reserve spending",
    script: contracts.reserve.spendingScript,
  },
  {
    name: "reserve observer",
    script: contracts.reserve.withdrawalScript,
  },
  {
    name: "payout spending",
    script: contracts.payout.spendingScript,
  },
  {
    name: "payout minting",
    script: contracts.payout.mintingScript,
  },
  // #579 removed all three "V1 transaction-field" reference scripts with their
  // contracts; the §8.6 certificate carries its own roles elsewhere in this
  // list. Only the CEK publication survives inside this guard, so the guard now
  // reads off `cekProgramMaterial` rather than the retired preimage lock. The
  // test is unchanged in meaning: under the always-succeeds contract set that
  // validator IS the tx-order spend script, and this list must not publish a
  // stand-in as though it were a distinct deployed reference script.
  ...(contracts.cekProgramMaterial.spendingScriptHash ===
  contracts.txOrder.spendingScriptHash
    ? []
    : [
        {
          name: "V1 immutable CEK program-material publication",
          script: contracts.cekProgramMaterial.spendingScript,
        },
      ]),
  ...(contracts.fraudProofs.validationTraceDispute === undefined
    ? []
    : [
        {
          name: "V1 validation-trace dispute",
          script: contracts.fraudProofs.validationTraceDispute.spendingScript,
        },
        {
          name: "V1 validation-trace source",
          script:
            contracts.fraudProofs.validationTraceDispute.source.spendingScript,
        },
        {
          name: "V1 validation-trace game",
          script:
            contracts.fraudProofs.validationTraceDispute.game.spendingScript,
        },
        {
          name: "V1 validation-trace boundary",
          script:
            contracts.fraudProofs.validationTraceDispute.boundary
              .spendingScript,
        },
        {
          name: "V1 validation-trace timeout",
          script:
            contracts.fraudProofs.validationTraceDispute.timeout.spendingScript,
        },
        {
          name: "V1 validation-trace award",
          script:
            contracts.fraudProofs.validationTraceDispute.award.spendingScript,
        },
      ]),
  ...registeredFraudProofReferenceScriptTargets(contracts),
];

export const referenceScriptTargetsByCommand = (
  contracts: SDK.MidgardValidators,
): Readonly<
  Record<ReferenceScriptCommandName, readonly ReferenceScriptTarget[]>
> => ({
  "node-runtime": nodeRuntimeReferenceScriptTargets(contracts),
  "protocol-init": [
    {
      name: "reference-script-auth minting",
      script: contracts.referenceScriptAuth.mintingScript,
    },
    {
      name: "hub-oracle minting",
      script: contracts.hubOracle.mintingScript,
    },
    {
      name: "da-params-governor minting",
      script: contracts.daParamsGovernor.mintingScript,
    },
    {
      name: "da-attestation minting",
      script: contracts.daAttestation.mintingScript,
    },
    {
      name: "scheduler minting",
      script: contracts.scheduler.mintingScript,
    },
    {
      name: "state-queue minting",
      script: contracts.stateQueue.mintingScript,
    },
    {
      name: "registered-operators minting",
      script: contracts.registeredOperators.mintingScript,
    },
    {
      name: "active-operators minting",
      script: contracts.activeOperators.mintingScript,
    },
    {
      name: "retired-operators minting",
      script: contracts.retiredOperators.mintingScript,
    },
    {
      name: "fraud-proof-catalogue minting",
      script: contracts.fraudProofCatalogue.mintingScript,
    },
  ],
  "hub-oracle": [
    {
      name: "hub-oracle minting",
      script: contracts.hubOracle.mintingScript,
    },
  ],
  "reference-script-auth": [
    {
      name: "reference-script-auth minting",
      script: contracts.referenceScriptAuth.mintingScript,
    },
  ],
  da: [
    {
      name: "da-params-governor spending",
      script: contracts.daParamsGovernor.spendingScript,
    },
    {
      name: "da-params-governor minting",
      script: contracts.daParamsGovernor.mintingScript,
    },
    {
      name: "da-attestation spending",
      script: contracts.daAttestation.spendingScript,
    },
    {
      name: "da-attestation minting",
      script: contracts.daAttestation.mintingScript,
    },
  ],
  "state-queue": [
    {
      name: "state-queue spending",
      script: contracts.stateQueue.spendingScript,
    },
    {
      name: "state-queue minting",
      script: contracts.stateQueue.mintingScript,
    },
  ],
  scheduler: [
    {
      name: "scheduler spending",
      script: contracts.scheduler.spendingScript,
    },
    {
      name: "scheduler minting",
      script: contracts.scheduler.mintingScript,
    },
  ],
  "registered-operators": [
    {
      name: "registered-operators spending",
      script: contracts.registeredOperators.spendingScript,
    },
    {
      name: "registered-operators minting",
      script: contracts.registeredOperators.mintingScript,
    },
  ],
  "active-operators": [
    {
      name: "active-operators spending",
      script: contracts.activeOperators.spendingScript,
    },
    {
      name: "active-operators minting",
      script: contracts.activeOperators.mintingScript,
    },
  ],
  "retired-operators": [
    {
      name: "retired-operators spending",
      script: contracts.retiredOperators.spendingScript,
    },
    {
      name: "retired-operators minting",
      script: contracts.retiredOperators.mintingScript,
    },
  ],
  deposit: [
    {
      name: "deposit minting",
      script: contracts.deposit.mintingScript,
    },
    {
      name: "deposit spending",
      script: contracts.deposit.spendingScript,
    },
  ],
  withdrawal: [
    {
      name: "withdrawal minting",
      script: contracts.withdrawal.mintingScript,
    },
    {
      name: "withdrawal spending",
      script: contracts.withdrawal.spendingScript,
    },
  ],
  settlement: [
    {
      name: "settlement minting",
      script: contracts.settlement.mintingScript,
    },
  ],
  "phas-membership": [
    {
      name: "membership proof withdrawal",
      script: loadPhasMembershipWithdrawalScript(),
    },
  ],
  reserve: [
    {
      name: "reserve spending",
      script: contracts.reserve.spendingScript,
    },
    {
      name: "reserve observer",
      script: contracts.reserve.withdrawalScript,
    },
  ],
  payout: [
    {
      name: "payout spending",
      script: contracts.payout.spendingScript,
    },
    {
      name: "payout minting",
      script: contracts.payout.mintingScript,
    },
  ],
});

export const ensureReferenceScriptTargetsProgram = (
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  targets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthMintingPolicy,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
  configuredReferenceScriptsAddress?: string,
  minAuthPolicyRemainingMs: number = REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  reservedFundingOutRefKeys: ReadonlySet<string> = new Set<string>(),
): Effect.Effect<
  readonly ReferenceScriptResolved[],
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while resolving ${scopeName} reference scripts`,
          cause,
        }),
    });
    const referenceScriptsAddress =
      configuredReferenceScriptsAddress ?? walletAddress;
    const fetchReferenceScriptWalletUtxos = (): Effect.Effect<
      readonly UTxO[],
      SDK.StateQueueError
    > =>
      refreshWalletUtxosFromOwnAddress(referenceScriptsLucid, {
        scopeName: `${scopeName} reference scripts`,
        failureMessage: `Failed to fetch wallet UTxOs while resolving ${scopeName} reference scripts`,
      });
    const fetchReferenceScriptPublicationUtxos = (): Effect.Effect<
      readonly UTxO[],
      SDK.StateQueueError
    > =>
      fetchReferenceScriptUtxosAt(
        referenceScriptsLucid,
        referenceScriptsAddress,
        `${scopeName} reference-script publication UTxO fetch at ${referenceScriptsAddress}`,
        `Failed to fetch reference-script UTxOs at ${referenceScriptsAddress} while resolving ${scopeName}`,
      );

    const publishMissingTargetsInBatches = (
      missingTargets: readonly ReferenceScriptTarget[],
      ensureWorkingCapital: boolean,
    ): Effect.Effect<
      readonly ReferenceScriptResolved[],
      | SDK.StateQueueError
      | SDK.LucidError
      | TxConfirmError
      | TxSignError
      | TxSubmitError
    > =>
      Effect.gen(function* () {
        if (missingTargets.length === 0) {
          return [];
        }
        yield* Effect.try({
          try: () =>
            assertReferenceScriptAuthMinimumRemaining({
              policy: authPolicy,
              nowMs: Date.now(),
              minRemainingMs: minAuthPolicyRemainingMs,
              scopeName,
              targetNames: missingTargets.map(({ name }) => name),
            }),
          catch: (cause) =>
            new SDK.StateQueueError({
              message:
                "Reference-script auth policy is too close to expiry for publication",
              cause,
            }),
        });
        let requiredPlainBalance =
          resolveReferenceScriptPublicationFundingTarget(missingTargets.length);
        while (true) {
          if (ensureWorkingCapital) {
            yield* ensureReferenceScriptWalletWorkingCapital(
              fundingLucid,
              referenceScriptsLucid,
              scopeName,
              requiredPlainBalance,
              reservedFundingOutRefKeys,
            );
          }
          const walletUtxos = yield* fetchReferenceScriptWalletUtxos();
          if (walletUtxos.length === 0) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message: `No wallet UTxOs available while publishing ${scopeName} reference scripts`,
                cause: "wallet-has-no-live-utxos",
              }),
            );
          }
          const plainFundingCandidateUtxos =
            filterPlainWalletUtxos(walletUtxos);
          if (plainFundingCandidateUtxos.length === 0) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message: `No plain wallet UTxOs available while publishing ${scopeName} reference scripts`,
                cause: "wallet-has-no-plain-utxos",
              }),
            );
          }
          yield* Effect.try({
            try: () =>
              assertReferenceScriptAuthMinimumRemaining({
                policy: authPolicy,
                nowMs: Date.now(),
                minRemainingMs: minAuthPolicyRemainingMs,
                scopeName,
                targetNames: missingTargets.map(({ name }) => name),
              }),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Reference-script auth policy is too close to expiry for publication",
                cause,
              }),
          });
          const publishAttempt = yield* Effect.either(
            publishMissingReferenceScriptTargets(
              referenceScriptsLucid,
              walletAddress,
              referenceScriptsAddress,
              walletUtxos,
              plainFundingCandidateUtxos,
              missingTargets,
              authPolicy,
            ),
          );
          if (publishAttempt._tag === "Right") {
            return publishAttempt.right;
          }
          if (
            isReferenceScriptPublicationBalanceInsufficient(publishAttempt.left)
          ) {
            if (!ensureWorkingCapital) {
              return yield* Effect.fail(
                new SDK.StateQueueError({
                  message:
                    "Planned reference-script publication batch was underfunded",
                  cause: `scope=${scopeName},targets=[${missingTargets.map(({ name }) => name).join(",")}],planner_miss=${formatUnknownError(publishAttempt.left)}`,
                }),
              );
            }
            const additionalFunding =
              resolveReferenceScriptPublicationAdditionalFunding(
                publishAttempt.left,
              );
            if (additionalFunding !== undefined) {
              requiredPlainBalance =
                sumWalletLovelace(plainFundingCandidateUtxos) +
                additionalFunding;
              yield* Effect.logWarning(
                `Reference-script publication for ${scopeName} needed more dedicated-wallet funding than the current working-capital floor; retrying after replenishing an additional ${additionalFunding.toString()} lovelace.`,
              );
              continue;
            }
          }
          if (
            missingTargets.length === 1 ||
            !isReferenceScriptPublicationTxTooLarge(publishAttempt.left)
          ) {
            return yield* Effect.fail(publishAttempt.left);
          }
          const splitIndex = Math.ceil(missingTargets.length / 2);
          const leftTargets = missingTargets.slice(0, splitIndex);
          const rightTargets = missingTargets.slice(splitIndex);
          yield* Effect.logWarning(
            `Reference-script publication for ${scopeName} exceeded max tx size; retrying in smaller batches: left=[${leftTargets
              .map(({ name }) => name)
              .join(", ")}], right=[${rightTargets
              .map(({ name }) => name)
              .join(", ")}]`,
          );
          const leftPublications = yield* publishMissingTargetsInBatches(
            leftTargets,
            ensureWorkingCapital,
          );
          const rightPublications = yield* publishMissingTargetsInBatches(
            rightTargets,
            ensureWorkingCapital,
          );
          return [...leftPublications, ...rightPublications];
        }
      });

    const walletUtxos = yield* fetchReferenceScriptPublicationUtxos();
    const existingPublications = yield* Effect.forEach(targets, (target) =>
      resolveExistingReferenceScriptPublication(
        referenceScriptsLucid,
        walletUtxos,
        target,
        authPolicy,
      ),
    );
    const existingByName = new Map(
      existingPublications
        .filter(
          (publication): publication is ReferenceScriptResolved =>
            publication !== undefined,
        )
        .map((publication) => [publication.name, publication]),
    );
    const missingTargets = targets.filter(
      (target) => !existingByName.has(target.name),
    );
    let createdByName = new Map<string, ReferenceScriptResolved>();
    if (missingTargets.length > 0) {
      const planningWalletUtxos = yield* fetchReferenceScriptWalletUtxos();
      const deploymentPlan = buildReferenceScriptDeploymentPlan({
        scopeName,
        targets,
        existingTargetNames: new Set(existingByName.keys()),
        walletUtxos: planningWalletUtxos,
      });
      yield* Effect.logInfo(
        `Reference-script deployment plan for ${scopeName}: existing=${deploymentPlan.existingTargetNames.length.toString()},missing=${deploymentPlan.missingTargetNames.length.toString()},batches=${deploymentPlan.batches.length.toString()},submit_count=${deploymentPlan.submitCount.toString()},current_plain_lovelace=${deploymentPlan.currentPlainBalance.toString()},required_plain_lovelace=${deploymentPlan.requiredPlainBalance.toString()},top_up_lovelace=${deploymentPlan.topUpLovelace.toString()},max_targets_per_batch=${deploymentPlan.maxTargetsPerBatch.toString()}`,
      );
      yield* ensureReferenceScriptWalletWorkingCapital(
        fundingLucid,
        referenceScriptsLucid,
        scopeName,
        deploymentPlan.requiredPlainBalance,
        reservedFundingOutRefKeys,
      );
      const created: ReferenceScriptResolved[] = [];
      for (const batch of deploymentPlan.batches) {
        const batchTargetNames = new Set(batch.targetNames);
        const batchTargets = missingTargets.filter((target) =>
          batchTargetNames.has(target.name),
        );
        yield* Effect.logInfo(
          `Publishing planned reference-script batch for ${scopeName}: batch_index=${batch.batchIndex.toString()},target_count=${batch.targetCount.toString()},targets=[${batch.targetNames.join(",")}]`,
        );
        created.push(
          ...(yield* publishMissingTargetsInBatches(batchTargets, true)),
        );
      }
      createdByName = new Map(
        created.map((publication) => [publication.name, publication]),
      );
    }
    const resolvedPublications: ReferenceScriptResolved[] = [];
    for (const target of targets) {
      const publication =
        existingByName.get(target.name) ?? createdByName.get(target.name);
      if (publication === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Missing resolved reference script publication",
            cause: `${scopeName}:${target.name}`,
          }),
        );
      }
      resolvedPublications.push(publication);
    }
    return resolvedPublications;
  });

export const deployReferenceScriptCommandProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  commandName: ReferenceScriptCommandName,
  authPolicy: ReferenceScriptAuthMintingPolicy,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
  referenceScriptsAddress?: string,
  minAuthPolicyRemainingMs: number = REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  reservedFundingOutRefKeys: ReadonlySet<string> = new Set<string>(),
): Effect.Effect<
  readonly ReferenceScriptResolved[],
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  ensureReferenceScriptTargetsProgram(
    referenceScriptsLucid,
    commandName,
    referenceScriptTargetsByCommand(contracts)[commandName],
    authPolicy,
    fundingLucid,
    referenceScriptsAddress,
    minAuthPolicyRemainingMs,
    reservedFundingOutRefKeys,
  );

export const planReferenceScriptCommandProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  commandName: ReferenceScriptCommandName,
  authPolicy: ReferenceScriptAuthMintingPolicy,
  referenceScriptsAddress?: string,
): Effect.Effect<ReferenceScriptDeploymentPlan, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while planning ${commandName} reference scripts`,
          cause,
        }),
    });
    const resolvedReferenceScriptsAddress =
      referenceScriptsAddress ?? walletAddress;
    const targets = referenceScriptTargetsByCommand(contracts)[commandName];
    const referenceScriptUtxos = yield* fetchReferenceScriptUtxosAt(
      referenceScriptsLucid,
      resolvedReferenceScriptsAddress,
      `${commandName} reference-script deployment plan UTxO fetch at ${resolvedReferenceScriptsAddress}`,
      `Failed to fetch reference-script UTxOs while planning ${commandName}`,
    );
    const existingPublications = yield* Effect.forEach(targets, (target) =>
      resolveExistingReferenceScriptPublication(
        referenceScriptsLucid,
        referenceScriptUtxos,
        target,
        authPolicy,
      ),
    );
    const existingTargetNames = new Set(
      existingPublications
        .filter(
          (publication): publication is ReferenceScriptResolved =>
            publication !== undefined,
        )
        .map(({ name }) => name),
    );
    const walletUtxos = yield* refreshWalletUtxosFromOwnAddress(
      referenceScriptsLucid,
      {
        scopeName: `${commandName} reference-script deployment plan`,
        failureMessage: `Failed to fetch wallet UTxOs while planning ${commandName} reference scripts`,
      },
    );
    return buildReferenceScriptDeploymentPlan({
      scopeName: commandName,
      targets,
      existingTargetNames,
      walletUtxos,
    });
  });

export const referenceScriptWalletStatusProgram = (
  referenceScriptsLucid: LucidEvolution,
  referenceScriptsAddress: string,
): Effect.Effect<ReferenceScriptWalletStatusSummary, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const utxos = yield* fetchReferenceScriptUtxosAt(
      referenceScriptsLucid,
      referenceScriptsAddress,
      `reference-script wallet status UTxO fetch at ${referenceScriptsAddress}`,
      `Failed to fetch reference-script wallet status UTxOs at ${referenceScriptsAddress}`,
    );
    return buildReferenceScriptWalletStatus({
      utxos,
      referenceScriptsAddress,
    });
  });

export const sweepReferenceScriptWalletProgram = (
  referenceScriptsLucid: LucidEvolution,
  referenceScriptsAddress: string,
  options: ReferenceScriptSweepOptions,
): Effect.Effect<
  ReferenceScriptSweepSummary,
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  Effect.gen(function* () {
    const execute = options.execute === true;
    const returnAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message:
            "Failed to resolve reference-script wallet address for sweep return",
          cause,
        }),
    });
    const utxos = yield* fetchReferenceScriptUtxosAt(
      referenceScriptsLucid,
      referenceScriptsAddress,
      `reference-script sweep UTxO fetch at ${referenceScriptsAddress}`,
      `Failed to fetch reference-script sweep UTxOs at ${referenceScriptsAddress}`,
    );
    const plan = buildReferenceScriptSweepPlan({
      utxos,
      referenceScriptsAddress,
      returnAddress,
      burnAddress: options.burnAddress,
      dryRun: !execute,
      includePlainUtxos: options.includePlainUtxos,
      tokenOutputLovelace: options.tokenOutputLovelace,
      maxAssetsPerTokenOutput: options.maxAssetsPerTokenOutput,
    });

    if (!execute || plan.sweepableUtxos.length === 0) {
      return plan.summary;
    }
    if (options.acknowledgeRetirement !== true) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Refusing to execute reference-script sweep without retirement acknowledgement",
          cause:
            "Pass --i-am-retiring-reference-scripts to confirm that the published reference scripts at L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS are no longer live.",
        }),
      );
    }
    if (
      plan.summary.nonLovelaceAssetCount > 0 &&
      options.burnAddress === undefined
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Refusing to execute reference-script sweep without a token burn/quarantine address",
          cause:
            "Pass --burn-address so non-lovelace assets can be moved off the reference-script wallet.",
        }),
      );
    }
    if (plan.summary.inputLovelace <= plan.summary.quarantineLovelace) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Reference-script sweep inputs do not contain enough lovelace for token quarantine outputs",
          cause: `input_lovelace=${plan.summary.inputLovelace.toString()},quarantine_lovelace=${plan.summary.quarantineLovelace.toString()}`,
        }),
      );
    }

    const unsigned = yield* Effect.tryPromise({
      try: () => {
        let tx = referenceScriptsLucid
          .newTx()
          .collectFrom([...plan.sweepableUtxos]);
        if (options.burnAddress !== undefined) {
          for (const tokenOutput of plan.tokenOutputs) {
            tx = tx.pay.ToAddress(options.burnAddress, { ...tokenOutput });
          }
        }
        return tx.complete({
          coinSelection: false,
          localUPLCEval: true,
          presetWalletInputs: [...plan.sweepableUtxos],
        });
      },
      catch: (cause) =>
        new SDK.LucidError({
          message: `Failed to build reference-script sweep transaction: ${String(cause)}`,
          cause,
        }),
    });
    const txHash = yield* handleSignSubmit(
      referenceScriptsLucid,
      unsigned,
      REFERENCE_SCRIPT_CONFIRMATION_OPTIONS,
    );
    return {
      ...plan.summary,
      dryRun: false,
      txHash,
    };
  });

export const ensureNodeRuntimeReferenceScriptsProgram = (
  referenceScriptsLucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  authPolicy: ReferenceScriptAuthMintingPolicy,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
  referenceScriptsAddress?: string,
  minAuthPolicyRemainingMs: number = REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
): Effect.Effect<
  readonly ReferenceScriptResolved[],
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError
> =>
  ensureReferenceScriptTargetsProgram(
    referenceScriptsLucid,
    "node-runtime",
    nodeRuntimeReferenceScriptTargets(contracts),
    authPolicy,
    fundingLucid,
    referenceScriptsAddress,
    minAuthPolicyRemainingMs,
  );

export const resolveReferenceScriptTargetsProgram = (
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  targets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthPolicyRef,
  configuredReferenceScriptsAddress?: string,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const walletAddress = yield* Effect.tryPromise({
      try: () => referenceScriptsLucid.wallet().address(),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to resolve reference-script wallet address while resolving ${scopeName} reference scripts`,
          cause,
        }),
    });
    const referenceScriptsAddress =
      configuredReferenceScriptsAddress ?? walletAddress;
    return yield* fetchReferenceScriptUtxosProgram(
      referenceScriptsLucid,
      referenceScriptsAddress,
      targets,
      authPolicy,
    );
  });

export const verifyNodeRuntimeReferenceScriptsProgram = (
  lucid: LucidEvolution,
  referenceScriptsAddress: string,
  contracts: SDK.MidgardValidators,
  authPolicy: ReferenceScriptAuthPolicyRef,
): Effect.Effect<readonly ReferenceScriptResolved[], SDK.StateQueueError> =>
  Effect.gen(function* () {
    const targets = nodeRuntimeReferenceScriptTargets(contracts);
    const referenceScriptUtxos = yield* fetchReferenceScriptUtxosAt(
      lucid,
      referenceScriptsAddress,
      `node-runtime reference-script UTxO fetch at ${referenceScriptsAddress}`,
      `Failed to fetch node-runtime reference-script UTxOs at ${referenceScriptsAddress}`,
    );
    const resolved: ReferenceScriptResolved[] = [];
    const missing: string[] = [];
    for (const target of targets) {
      const utxo = [...referenceScriptUtxos]
        .filter(
          (candidate) =>
            isSameScriptRef(candidate.scriptRef, target.script) &&
            hasReferenceScriptAuthRole(candidate, target, authPolicy),
        )
        .sort(compareOutRefs)[0];
      if (utxo === undefined) {
        missing.push(target.name);
      } else {
        resolved.push({ name: target.name, utxo });
      }
    }
    if (missing.length > 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Missing node-runtime reference scripts",
          cause: `address=${referenceScriptsAddress};missing=[${missing.join(",")}]`,
        }),
      );
    }
    return resolved;
  });
