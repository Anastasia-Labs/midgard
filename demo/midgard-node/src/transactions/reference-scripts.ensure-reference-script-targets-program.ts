import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import {
  assertReferenceScriptAuthMinimumRemaining,
  REFERENCE_SCRIPT_AUTH_MIN_REMAINING_MS,
  type ReferenceScriptAuthMintingPolicy,
  type ReferenceScriptAuthPolicyRef,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type ReferenceScriptCommandName } from "../deployable-scripts.js";
import {
  findSubmitOutcomeUnknown,
  isRetryableProviderError,
  type ProviderRetryOptions,
  runProviderStepWithRetry,
} from "../provider-retry.js";
import { IntentJournal } from "../services/intent-journal.js";
import {
  publishReferenceScripts,
  referencePublicationFundingRequired,
  referencePublicationLaneCount,
  type ReferencePublicationOptions,
  referencePublicationOptions,
  referencePublicationPreparationFeeAllowance,
} from "./reference-publication.js";
import {
  ensureReferenceScriptWalletWorkingCapital,
  nodeRuntimeReferenceScriptTargets,
  referenceScriptTargetsByCommand,
  resolveExistingReferenceScriptPublication,
} from "./reference-scripts.ensure-reference-script-wallet-working-capital.js";
import {
  buildReferenceScriptWalletStatus,
  fetchReferenceScriptUtxosAt,
  fetchReferenceScriptUtxosProgram,
  hasReferenceScriptAuthRole,
  isSameScriptRef,
  type ReferenceScriptDeploymentPlan,
  type ReferenceScriptResolved,
  type ReferenceScriptTarget,
  type ReferenceScriptWalletStatusSummary,
  utxoOutRefKey,
} from "./reference-scripts.fetch-reference-script-utxos-program.js";
import {
  awaitWalletViewUtxos,
  buildReferenceScriptDeploymentPlan,
} from "./reference-scripts.wallet-view-utxos.js";
import { TxConfirmError, TxSignError, TxSubmitError } from "./utils.js";

/**
 * How often a publication pass interrupted by a transient provider failure
 * is resumed. Every pass re-reads the published references, re-checks the
 * auth deadline and crosses the previous validity window before it spends,
 * so a resumed pass publishes only what is still missing.
 */
export const REFERENCE_SCRIPT_PUBLICATION_RETRY: ProviderRetryOptions = {
  maxAttempts: 5,
  baseDelayMs: 2_000,
  maxDelayMs: 30_000,
};

const hasCause = (
  error: unknown,
  matches: (cause: Error) => boolean,
): boolean => {
  let current: unknown = error;
  for (let depth = 0; depth < 16 && current !== undefined; depth += 1) {
    if (current instanceof Error && matches(current)) {
      return true;
    }
    current =
      typeof current === "object" && current !== null && "cause" in current
        ? (current as { readonly cause?: unknown }).cause
        : undefined;
  }
  return false;
};

const isAuthDeadlineFailure = (cause: Error): boolean =>
  cause.name === "ReferenceScriptAuthDeadlineError";

// publishReferenceScripts' own refusal after 30 minutes without progress. It
// names no provider failure, only reads like one ("timed out").
const isReconciliationTimeout = (cause: Error): boolean =>
  cause.message.startsWith("Reference publication reconciliation timed out");

/**
 * Resumes a publication pass only after a transient provider failure. An
 * auth policy too close to expiry is never retried: waiting cannot add time.
 * Nor is a reconciliation that already waited out its own progress deadline.
 */
const isResumablePublicationFailure = (error: unknown): boolean =>
  !hasCause(error, isAuthDeadlineFailure) &&
  !hasCause(error, isReconciliationTimeout) &&
  isRetryableProviderError(error);

/** A transaction a failed pass sent with an unknown outcome, and that failure. */
type SentWithUnknownOutcome = Readonly<{ txHash: string; failure: unknown }>;

/**
 * The transaction `failure` sent with an unknown outcome (the provider's
 * `L1SubmitOutcomeUnknownError`), by the id the provider or the submit seam's
 * `TxSubmitError` names; `undefined` for any other failure.
 */
const sentWithUnknownOutcome = (
  failure: unknown,
): SentWithUnknownOutcome | undefined => {
  const outcomeUnknown = findSubmitOutcomeUnknown(failure);
  if (outcomeUnknown === undefined) return undefined;
  const txHash =
    outcomeUnknown.txHash ??
    (failure instanceof TxSubmitError ? failure.txHash : undefined);
  return txHash === undefined ? undefined : { txHash, failure };
};

/**
 * Settles a transaction sent with an unknown outcome before a pass resumes:
 * by its exact id's status, the next pass runs once it landed (its re-read
 * sees a landed top-up, so none is built again) or once it can no longer
 * land (absent, or phase-2 invalid). While it is pending, or its status is
 * unreadable, this fails with a resumable error and nothing is rebuilt.
 */
const settleSentWithUnknownOutcome = (
  lucid: Pick<LucidEvolution, "transactionStatus">,
  scopeName: string,
  sent: SentWithUnknownOutcome,
): Effect.Effect<void, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const status = yield* Effect.tryPromise({
      try: () => lucid.transactionStatus(sent.txHash),
      catch: (cause) =>
        new SDK.StateQueueError({
          message: `Failed to read the status of ${sent.txHash}, sent with an unknown outcome while preparing ${scopeName} reference scripts`,
          cause: new AggregateError([cause, sent.failure]),
        }),
    });
    if (status.status === "pending") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: `Transaction ${sent.txHash}, sent with an unknown outcome while preparing ${scopeName} reference scripts, is still pending; nothing is rebuilt meanwhile`,
          cause: sent.failure,
        }),
      );
    }
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
  publicationOptions?: ReferencePublicationOptions,
  publicationRetry: ProviderRetryOptions = REFERENCE_SCRIPT_PUBLICATION_RETRY,
): Effect.Effect<
  readonly ReferenceScriptResolved[],
  | SDK.StateQueueError
  | SDK.LucidError
  | TxConfirmError
  | TxSignError
  | TxSubmitError,
  IntentJournal
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
    const publicationPass = Effect.gen(function* () {
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
      const journal = yield* IntentJournal;
      yield* Effect.tryPromise({
        try: (signal) =>
          publishReferenceScripts({
            journal,
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
    });
    let unsettled: SentWithUnknownOutcome | undefined;
    const resumablePass = Effect.gen(function* () {
      if (unsettled !== undefined) {
        yield* settleSentWithUnknownOutcome(
          referenceScriptsLucid,
          scopeName,
          unsettled,
        );
        unsettled = undefined;
      }
      yield* publicationPass;
    }).pipe(
      Effect.tapError((failure) =>
        Effect.sync(() => {
          unsettled ??= sentWithUnknownOutcome(failure);
        }),
      ),
    );
    yield* runProviderStepWithRetry(
      `${scopeName} reference-script publication`,
      resumablePass,
      { ...publicationRetry, isRetryable: isResumablePublicationFailure },
    );
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
  | TxSubmitError,
  IntentJournal
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
): Effect.Effect<
  ReferenceScriptDeploymentPlan,
  SDK.StateQueueError,
  IntentJournal
> =>
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
    const walletUtxos = yield* awaitWalletViewUtxos(referenceScriptsLucid, {
      scopeName: `${commandName} reference-script deployment plan`,
      failureMessage: `Failed to fetch wallet UTxOs while planning ${commandName} reference scripts`,
    });
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
  | TxSubmitError,
  IntentJournal
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
