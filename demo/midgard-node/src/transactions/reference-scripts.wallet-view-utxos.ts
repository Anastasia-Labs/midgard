import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { IntentJournal } from "../services/intent-journal.js";
import type { NodeWalletView } from "../services/intent-journal.wallet-view.js";
import {
  filterPlainWalletUtxos,
  REFERENCE_SCRIPT_PUBLICATION_DEFAULT_MAX_TARGETS_PER_BATCH,
  REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE,
  type ReferenceScriptDeploymentPlan,
  type ReferenceScriptPublicationBatchPlan,
  type ReferenceScriptTarget,
  resolveReferenceScriptPublicationFundingTarget,
  sumWalletLovelace,
  utxoOutRefKey,
  WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES,
  WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY,
} from "./reference-scripts.fetch-reference-script-utxos-program.js";
import { readSelectedWalletView } from "./utils.wallet-view.js";

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

const readViewOfSelectedWallet = (
  lucid: LucidEvolution,
  failureMessage: string,
): Effect.Effect<NodeWalletView, SDK.StateQueueError, IntentJournal> =>
  readSelectedWalletView(lucid).pipe(
    Effect.mapError(
      (cause) =>
        new SDK.StateQueueError({
          message: `${failureMessage}: ${cause.message}`,
          cause,
        }),
    ),
  );

/**
 * The wallet view's UTxOs (plan §8.5) of the wallet selected on `lucid`.
 * With `minimumPlainBalance`, it reads again until the view's plain balance
 * reaches it (a top-up the follower has not applied yet), and fails after
 * the retries.
 */
export const awaitWalletViewUtxos = (
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
): Effect.Effect<readonly UTxO[], SDK.StateQueueError, IntentJournal> =>
  Effect.gen(function* () {
    let lastCause: unknown = null;
    for (
      let attempt = 0;
      attempt < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES;
      attempt += 1
    ) {
      const view = yield* Effect.either(
        readViewOfSelectedWallet(lucid, failureMessage),
      );
      if (view._tag === "Right") {
        const plainBalance = sumWalletLovelace(
          filterPlainWalletUtxos(view.right.utxos),
        );
        if (
          minimumPlainBalance === undefined ||
          plainBalance >= minimumPlainBalance
        ) {
          return view.right.utxos;
        }
        lastCause = `wallet_address=${view.right.address},plain_balance=${plainBalance.toString()},required_plain_balance=${minimumPlainBalance.toString()}`;
      } else {
        lastCause = view.left;
      }

      if (attempt + 1 < WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES) {
        yield* Effect.logWarning(
          `Wallet view for ${scopeName} did not reach the required state (attempt ${(attempt + 1).toString()}/${WALLET_OWN_ADDRESS_REFRESH_MAX_RETRIES.toString()}); retrying in ${WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY}. cause=${String(lastCause)}`,
        );
        yield* Effect.sleep(WALLET_OWN_ADDRESS_REFRESH_RETRY_DELAY);
      }
    }
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: failureMessage,
        cause: lastCause,
      }),
    );
  });

/**
 * The plain (no script reference, ADA-only) UTxOs of the selected wallet's
 * view that are not in `excludedOutRefKeys`: funding inputs for a build.
 * Reference-script publications stay available for `.readFrom(...)`.
 */
export const resolveSpendableWalletUtxos = (
  lucid: LucidEvolution,
  excludedOutRefKeys: ReadonlySet<string>,
): Effect.Effect<readonly UTxO[], SDK.StateQueueError, IntentJournal> =>
  readViewOfSelectedWallet(
    lucid,
    "Failed to read the wallet view for transaction input preset",
  ).pipe(
    Effect.map((view) =>
      filterPlainWalletUtxos(view.utxos).filter(
        (utxo) => !excludedOutRefKeys.has(utxoOutRefKey(utxo)),
      ),
    ),
  );

export const resolveLiveWalletUtxo = (
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
