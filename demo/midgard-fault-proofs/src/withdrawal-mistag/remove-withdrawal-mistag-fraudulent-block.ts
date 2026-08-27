/** Explicit pre-registration bridge into the category-agnostic removal path. */
import { WITHDRAWAL_MISTAG_TEST_CATEGORY_ID_V1 } from "@al-ft/midgard-sdk";

import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { WithdrawalMistagContractsV1 } from "./contracts-v1.js";

export const submitRemoveWithdrawalMistagFraudulentBlock = async ({
  contracts,
  firstStepDeploymentEntry,
  ...args
}: Omit<Parameters<typeof submitRemoveFraudulentBlock>[0], "fraudCategory"> & {
  readonly contracts: WithdrawalMistagContractsV1;
  /** Harness deployment entry that publishes the applied step-01 script. */
  readonly firstStepDeploymentEntry: string;
}) =>
  await submitRemoveFraudulentBlock({
    ...args,
    fraudCategory: {
      name: "withdrawal-mistag",
      categoryId: WITHDRAWAL_MISTAG_TEST_CATEGORY_ID_V1,
      firstStepDeploymentEntry,
      firstStepScriptHash: contracts.steps[0].spendingScriptHash,
      fraudProof: {
        policyId: contracts.fraudProof.policyId,
        spendingScriptHash: contracts.fraudProof.spendingScriptHash,
        spendingScriptAddress: contracts.fraudProof.spendingScriptAddress,
      },
    },
  });
