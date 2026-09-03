import { submitRemoveFraudulentBlock } from "../remove-fraudulent-block.js";
import type { WithdrawalMistagContracts } from "./contracts-v1.js";

export const submitRemoveWithdrawalMistagFraudulentBlock = async ({
  contracts: _contracts,
  firstStepDeploymentEntry: _firstStepDeploymentEntry,
  ...args
}: Omit<Parameters<typeof submitRemoveFraudulentBlock>[0], "fraudCategory"> & {
  /** Deprecated fixture inputs; canonical production resolution ignores them. */
  readonly contracts?: WithdrawalMistagContracts;
  readonly firstStepDeploymentEntry?: string;
}) =>
  await submitRemoveFraudulentBlock({
    ...args,
    fraudCategory: "withdrawalMistag",
  });
