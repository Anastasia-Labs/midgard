import * as SDK from "@al-ft/midgard-sdk";
import { type ReferenceScriptAuthPolicyRef } from "@al-ft/midgard-sdk";
import {
  getAddressDetails,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type PublishedDeployableScript,
  publishedDeployableScripts,
  REFERENCE_SCRIPT_COMMAND_NAMES,
  type ReferenceScriptCommandName,
} from "../deployable-scripts.js";
import {
  type IntentJournal,
  journaledIntent,
  openPlan,
} from "../services/intent-journal.js";
import { compareOutRefs } from "../tx-context.js";
import {
  buildReferenceScriptWalletStatus,
  filterPlainWalletUtxos,
  formatReferenceScriptWalletStatusCause,
  hasReferenceScriptAuthRole,
  isSameScriptRef,
  REFERENCE_SCRIPT_CONFIRMATION_OPTIONS,
  REFERENCE_SCRIPT_WALLET_WORKING_CAPITAL_LOVELACE,
  type ReferenceScriptResolved,
  type ReferenceScriptTarget,
  SCRIPT_REF_PUBLICATION_FUNDING_BUFFER_LOVELACE,
  selectWalletFundingUtxos,
  sumWalletLovelace,
} from "./reference-scripts.fetch-reference-script-utxos-program.js";
import {
  refreshWalletUtxosFromOwnAddress,
  resolveLiveWalletUtxo,
  resolveSpendableWalletUtxos,
} from "./reference-scripts.refresh-wallet-utxos-from-own-address.js";
import {
  handleSignSubmit,
  TxConfirmError,
  TxSignError,
  TxSubmitError,
} from "./utils.js";

export const resolveExistingReferenceScriptPublication = (
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

export const ensureReferenceScriptWalletWorkingCapital = (
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
  | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    // S5: the plan opens before the wallet read the top-up rests on.
    const plan = yield* openPlan;
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
      // The target (§8.4 E2): the plain balance at the reference-script
      // address this top-up reaches.
      journaledIntent(
        "reference_funding",
        `reference_funding:${scopeName}:${getAddressDetails(referenceScriptAddress).address.hex}:${targetPlainBalance.toString()}`,
        plan,
      ),
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
