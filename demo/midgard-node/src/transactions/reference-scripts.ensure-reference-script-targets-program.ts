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
  buildReferenceScriptDeploymentPlan,
  refreshWalletUtxosFromOwnAddress,
} from "./reference-scripts.refresh-wallet-utxos-from-own-address.js";
import { TxConfirmError, TxSignError, TxSubmitError } from "./utils.js";

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
