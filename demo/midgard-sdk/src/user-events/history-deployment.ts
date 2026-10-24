import { normalizeOutRef, type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { type Network } from "@lucid-evolution/lucid";

import {
  type AuthenticatedValidator,
  type MidgardValidators,
  type SpendingValidator,
  type WithdrawalValidator,
} from "../common.js";
import {
  type FaultProofBlueprint,
  makeAuthenticatedValidator,
  makeSpendingValidator,
  makeWithdrawalValidator,
} from "../fraud-proof/contracts/blueprint.js";
import {
  applyEventHistoryValidators,
  type EventHistoryRecipe,
} from "./history.js";
import { type EventHistoryPayloadBounds } from "./history-payload.js";
import { type EventHistoryDeployment } from "./history-query.js";

/** Every script needed to operate one list, bound to its exact initialization recipe. */
export type EventHistoryContracts = Readonly<{
  recipe: EventHistoryRecipe;
  list: AuthenticatedValidator & WithdrawalValidator;
  retention: SpendingValidator;
  retirement: WithdrawalValidator;
}>;

export type EventHistoryContractPair = Readonly<{
  deposit: EventHistoryContracts;
  withdrawal: EventHistoryContracts;
}>;

export const eventHistoryDeploymentFromContracts = (
  history: EventHistoryContracts,
): EventHistoryDeployment => ({
  policyId: history.list.policyId,
  address: history.list.spendingScriptAddress,
  retentionAddress: history.retention.spendingScriptAddress,
  inlineLimitBytes: history.recipe.inlineLimitBytes,
});

export const requireEventHistoryContracts = (
  contracts: Pick<MidgardValidators, "eventHistory">,
): EventHistoryContractPair => {
  if (contracts.eventHistory === null)
    throw new Error(
      "Authenticated event history is unavailable in the always-succeeds scaffold",
    );
  return contracts.eventHistory;
};

/** Both roots consume the deployment nonce in the same atomic bootstrap.
 * Bounds and protection duration remain explicit deployment decisions. */
export const buildEventHistoryDeployments = ({
  blueprint,
  network,
  hubOraclePolicyId,
  initializationNonce,
  protectionDurationMs,
  bounds,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly initializationNonce: OutRefLike;
  readonly protectionDurationMs: bigint;
  readonly bounds: EventHistoryPayloadBounds;
}): EventHistoryContractPair => {
  const nonce = normalizeOutRef(initializationNonce);
  const build = (kind: EventHistoryRecipe["kind"]): EventHistoryContracts => {
    const recipe: EventHistoryRecipe = {
      hubPolicyId: hubOraclePolicyId,
      kind,
      initializationNonce: {
        transactionId: nonce.txHash,
        outputIndex: BigInt(nonce.outputIndex),
      },
      protectionDurationMs,
      ...bounds,
    };
    const applied = applyEventHistoryValidators(blueprint, network, recipe);
    return {
      recipe,
      list: {
        ...makeAuthenticatedValidator(
          network,
          applied.validator.script,
          applied.validator.script,
        ),
        ...makeWithdrawalValidator(applied.validator.script),
      },
      retention: makeSpendingValidator(
        network,
        applied.retention.validator.script,
      ),
      retirement: makeWithdrawalValidator(applied.retirement.validator.script),
    };
  };
  return { deposit: build("Deposit"), withdrawal: build("Withdrawal") };
};
