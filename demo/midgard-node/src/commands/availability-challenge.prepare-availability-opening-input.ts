import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import {
  availabilityPreparationAmounts,
  required,
} from "./availability-challenge.plan-availability-command-action.js";

/**
 * `prepare`: pays the actor, from any of its coins (token-bearing ones
 * included), the ADA-only coins one challenge of `snapshot`'s header needs:
 * output 0 the exact opening input for `open --funding-out-ref`, output 1
 * the collateral for every step's `--collateral-out-ref`, output 2 the
 * working capital the removal path draws on. Spent tokens return as change.
 * Coins the journal still reserves are never spent.
 */
export const prepareAvailabilityOpeningInput = async (input: {
  readonly lucid: LucidEvolution;
  readonly deployment: SDK.DaAvailabilityDeployment;
  readonly snapshot: SDK.DaAvailabilityChallengeSnapshot;
  readonly context: SDK.DaAvailabilityOperationContext;
  readonly headerHash: string;
  readonly reservedOutRefs: ReadonlySet<string>;
}) => {
  const { lucid, deployment } = input;
  const queue = required(
    input.snapshot.queue,
    "the authenticated queue header",
  );
  const liveQueue = await SDK.fetchSortedStateQueueUTxOs(lucid, {
    stateQueueAddress: deployment.contracts.stateQueue.spendingScriptAddress,
    stateQueuePolicyId: deployment.contracts.stateQueue.policyId,
  });
  const targetIndex = liveQueue.findIndex(
    ({ utxo }) =>
      utxo.txHash === queue.utxo.txHash &&
      utxo.outputIndex === queue.utxo.outputIndex,
  );
  if (targetIndex < 1)
    throw new Error(
      "Availability preparation cannot find the current challenged queue header",
    );
  const protocol = required(
    lucid.config().protocolParameters,
    "live protocol parameters",
  );
  const amounts = availabilityPreparationAmounts({
    parameters: deployment.parameters,
    collateralPercentage: protocol.collateralPercentage,
    coinsPerUtxoByte: protocol.coinsPerUtxoByte,
    walletAddress: await lucid.wallet().address(),
    remainingRemovalSteps: liveQueue.length - targetIndex,
  });
  const result = await SDK.runDaAvailabilityOperation(input.context, {
    headerHash: input.headerHash,
    action: "prepare",
    build: async (_signal, scope) => {
      const validFrom = BigInt(Math.max(0, Date.now() - 60_000));
      return SDK.buildDaAvailabilityFundingPreparationTx(
        lucid,
        {
          outputLovelace: amounts.openingLovelace,
          feeLovelace: amounts.feeLovelace,
          collateralLovelace: amounts.collateralLovelace,
          workingCapitalLovelace: amounts.workingCapitalLovelace,
          walletInputs: (await lucid.wallet().getUtxos()).filter(
            (u) => !input.reservedOutRefs.has(`${u.txHash}#${u.outputIndex}`),
          ),
          validFrom,
          validTo: validFrom + 120_000n,
        },
        scope,
      );
    },
  });
  return {
    ...result,
    openingOutRef: result.expectedOutRefs[0] ?? null,
    collateralOutRef: result.expectedOutRefs[1] ?? null,
    workingCapitalOutRef: result.expectedOutRefs[2] ?? null,
    openingLovelace: amounts.openingLovelace.toString(),
    collateralLovelace: amounts.collateralLovelace.toString(),
    workingCapitalLovelace: amounts.workingCapitalLovelace.toString(),
    feeLovelace: amounts.feeLovelace.toString(),
  };
};
