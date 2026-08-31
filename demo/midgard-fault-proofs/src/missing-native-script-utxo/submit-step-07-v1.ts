import {
  type SubmitMissingNativeScriptTxStep08ResultV1,
  submitMissingNativeScriptTxStep08V1,
} from "../missing-native-script-tx/submit-missing-native-script-tx-step-08.js";
import type { MissingNativeScriptUtxoContractsV1 } from "./contracts-v1.js";
import { missingNativeScriptUtxoStagedContractsV1 } from "./staged-submit-adapter-v1.js";

type Args = Omit<
  Parameters<typeof submitMissingNativeScriptTxStep08V1>[0],
  "contracts"
> & {
  readonly contracts: MissingNativeScriptUtxoContractsV1;
};

/** Resumes or terminally finalizes the bounded Q33 semantic scan. */
export const submitMissingNativeScriptUtxoStep07V1 = async ({
  contracts,
  ...args
}: Args): Promise<SubmitMissingNativeScriptTxStep08ResultV1> =>
  await submitMissingNativeScriptTxStep08V1({
    ...args,
    contracts: missingNativeScriptUtxoStagedContractsV1(contracts),
  });
