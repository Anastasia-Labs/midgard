import {
  submitMissingNativeScriptTxStep08,
  type SubmitMissingNativeScriptTxStep08Result,
} from "../missing-native-script-tx/submit-missing-native-script-tx-step-08.js";
import type { MissingNativeScriptUtxoContracts } from "./contracts.js";
import { missingNativeScriptUtxoStagedContracts } from "./staged-submit-adapter.js";

type Args = Omit<
  Parameters<typeof submitMissingNativeScriptTxStep08>[0],
  "contracts"
> & {
  readonly contracts: MissingNativeScriptUtxoContracts;
};

/** Resumes or terminally finalizes the bounded Q33 semantic scan. */
export const submitMissingNativeScriptUtxoStep07 = async ({
  contracts,
  ...args
}: Args): Promise<SubmitMissingNativeScriptTxStep08Result> =>
  await submitMissingNativeScriptTxStep08({
    ...args,
    contracts: missingNativeScriptUtxoStagedContracts(contracts),
  });
