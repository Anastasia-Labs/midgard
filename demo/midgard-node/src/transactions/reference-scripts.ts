import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assertReferenceScriptAuthMinimumRemaining,
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  type ReferenceScriptAuthMintingPolicy,
  type ReferenceScriptAuthPolicyRef,
  referenceScriptAuthTokenNameText,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type PublishedDeployableScript,
  publishedDeployableScripts,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  type ReferenceScriptCommandName,
} from "../deployable-scripts.js";
import { runProviderStepWithRetry } from "../provider-retry.js";
import { compareOutRefs, outRefLabel } from "../tx-context.js";
import {
  publishReferenceScripts,
  referencePublicationFundingRequired,
  referencePublicationLaneCount,
  type ReferencePublicationOptions,
  referencePublicationOptions,
  referencePublicationPreparationFeeAllowance,
} from "./reference-publication.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";
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

const SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE =
  SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE;
const REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE = 50_000_000n;
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

export { REFERENCE_SCRIPT_COMMAND_NAMES, type ReferenceScriptCommandName };

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

/**
 * Whether the live resolution accepts `utxo` for `target`: it sits at the
 * reference-script address, holds the target's role token under the auth
 * policy, and carries the target's script.
 */
export const acceptsReferenceScriptUtxo = (
  utxo: UTxO,
  referenceScriptsAddress: string,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): boolean =>
  utxo.address === referenceScriptsAddress &&
  hasReferenceScriptAuthRole(utxo, target, authPolicy) &&
  isSameScriptRef(utxo.scriptRef, target.script);

/** The UTxO the live resolution picks for `target`, if any. */
export const resolveReferenceScriptUtxo = (
  utxos: readonly UTxO[],
  referenceScriptsAddress: string,
  target: ReferenceScriptTarget,
  authPolicy: ReferenceScriptAuthPolicyRef,
): UTxO | undefined =>
  utxos
    .filter((utxo) =>
      acceptsReferenceScriptUtxo(
        utxo,
        referenceScriptsAddress,
        target,
        authPolicy,
      ),
    )
    .sort(compareOutRefs)[0];

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
        const resolved = resolveReferenceScriptUtxo(
          referenceScriptUtxos,
          referenceScriptsAddress,
          target,
          authPolicy,
        );
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

export const utxoOutRefKey = (
  utxo: Pick<UTxO, "txHash" | "outputIndex">,
): string => `${utxo.txHash}#${utxo.outputIndex.toString()}`;

const filterPlainWalletUtxos = (utxos: readonly UTxO[]): readonly UTxO[] =>
  utxos.filter(isPlainAdaOnlyUtxo);

const sumWalletLovelace = (utxos: readonly UTxO[]): bigint =>
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

const resolveReferenceScriptPublicationFundingTarget = (
  missingTargetCount: number,
): bigint => SDK.referenceScriptPublicationFundingTarget(missingTargetCount);

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
          hasReferenceScriptAuthRole(utxo, target, authPolicy) &&
          isSameScriptRef(utxo.scriptRef, target.script),
      )
      .sort(compareOutRefs)
      .reverse();
    for (const existingCandidate of existingCandidates) {
      const existing = yield* resolveLiveWalletUtxo(lucid, existingCandidate);
      if (
        existing !== undefined &&
        hasReferenceScriptAuthRole(existing, target, authPolicy) &&
        isSameScriptRef(existing.scriptRef, target.script)
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
  preparationFeeAllowance: (plainFundingCount: number) => bigint = () => 0n,
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
    const plainUtxos = filterPlainWalletUtxos(referenceScriptWalletUtxos);
    const currentPlainBalance = sumWalletLovelace(plainUtxos);
    const workingCapital = (plainFundingCount: number): bigint => {
      const required =
        requiredPlainBalance + preparationFeeAllowance(plainFundingCount);
      return required > REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE
        ? required
        : REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE;
    };
    if (currentPlainBalance >= workingCapital(plainUtxos.length)) {
      return;
    }
    // The top-up adds one more plain output for publication to consolidate.
    const targetPlainBalance = workingCapital(plainUtxos.length + 1);

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

const toReferenceScriptTarget = ({
  role,
  script,
}: PublishedDeployableScript): ReferenceScriptTarget => ({
  name: role,
  script,
});

/** Every reference script the node runtime needs, in publication order. */
export const nodeRuntimeReferenceScriptTargets = (
  contracts: SDK.MidgardValidators,
): readonly ReferenceScriptTarget[] =>
  publishedDeployableScripts(contracts).map(toReferenceScriptTarget);

/**
 * Per-command subsets of the node-runtime targets, each in publication order.
 * Which commands need a script is declared by its catalogue entry.
 */
export const referenceScriptTargetsByCommand = (
  contracts: SDK.MidgardValidators,
): Readonly<
  Record<ReferenceScriptCommandName, readonly ReferenceScriptTarget[]>
> => {
  const published = publishedDeployableScripts(contracts);
  return Object.fromEntries(
    REFERENCE_SCRIPT_COMMAND_NAMES.map((commandName) => [
      commandName,
      (commandName === "node-runtime"
        ? published
        : published.filter(({ commands }) =>
            (commands as readonly ReferenceScriptCommandName[]).includes(
              commandName,
            ),
          )
      ).map(toReferenceScriptTarget),
    ]),
  ) as unknown as Record<
    ReferenceScriptCommandName,
    readonly ReferenceScriptTarget[]
  >;
};

export const ensureReferenceScriptTargetsProgram = (
  referenceScriptsLucid: LucidEvolution,
  scopeName: string,
  targets: readonly ReferenceScriptTarget[],
  authPolicy: ReferenceScriptAuthMintingPolicy,
  fundingLucid: LucidEvolution = referenceScriptsLucid,
  configuredReferenceScriptsAddress?: string,
  minAuthPolicyRemainingMs: number = REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  reservedFundingOutRefKeys: ReadonlySet<string> = new Set<string>(),
  publicationOptions?: ReferencePublicationOptions,
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
          message: "Failed to resolve reference publisher address",
          cause,
        }),
    });
    const referenceScriptsAddress =
      configuredReferenceScriptsAddress ?? walletAddress;
    const existing = yield* fetchReferenceScriptUtxosAt(
      referenceScriptsLucid,
      referenceScriptsAddress,
      scopeName,
      "Failed to discover published references",
    );
    const missing = targets.filter(
      (target) =>
        !existing.some(
          (utxo) =>
            utxo.address === referenceScriptsAddress &&
            hasReferenceScriptAuthRole(utxo, target, authPolicy) &&
            isSameScriptRef(utxo.scriptRef, target.script),
        ),
    );
    if (missing.length > 0) {
      yield* Effect.try({
        try: () =>
          assertReferenceScriptAuthMinimumRemaining({
            policy: authPolicy,
            nowMs: Date.now(),
            minRemainingMs: minAuthPolicyRemainingMs,
            scopeName,
            targetNames: missing.map((t) => t.name),
          }),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Reference-script authority cannot publish missing references",
            cause,
          }),
      });
      yield* ensureReferenceScriptWalletWorkingCapital(
        fundingLucid,
        referenceScriptsLucid,
        scopeName,
        referencePublicationFundingRequired(
          referenceScriptsLucid,
          referenceScriptsAddress,
          missing,
          authPolicy,
        ) +
          2n * SDK.SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE +
          SDK.SCRIPT_REF_OUTPUT_LOVELACE,
        reservedFundingOutRefKeys,
        (plainFundingCount) =>
          referencePublicationPreparationFeeAllowance(
            referenceScriptsLucid,
            plainFundingCount,
            referencePublicationLaneCount(
              (
                publicationOptions ??
                referencePublicationOptions(referenceScriptsLucid)
              ).mode,
            ),
          ),
      );
    }
    yield* Effect.tryPromise({
      try: (signal) =>
        publishReferenceScripts({
          lucid: referenceScriptsLucid,
          address: referenceScriptsAddress,
          targets,
          authPolicy,
          reserved: reservedFundingOutRefKeys,
          minAuthPolicyRemainingMs,
          signal,
          options:
            publicationOptions ??
            referencePublicationOptions(referenceScriptsLucid),
        }),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Reference-script publication failed: ${formatUnknownError(cause)}`,
          cause,
        }),
    });
    return yield* fetchReferenceScriptUtxosProgram(
      referenceScriptsLucid,
      referenceScriptsAddress,
      targets,
      authPolicy,
    );
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
    new Set(
      Object.values(SDK.requireEventHistoryContracts(contracts)).map(
        ({ recipe }) =>
          utxoOutRefKey({
            txHash: recipe.initializationNonce.transactionId,
            outputIndex: Number(recipe.initializationNonce.outputIndex),
          }),
      ),
    ),
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
      const utxo = resolveReferenceScriptUtxo(
        referenceScriptUtxos,
        referenceScriptsAddress,
        target,
        authPolicy,
      );
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
