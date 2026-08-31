import {
  type SubmitMissingNativeScriptTxStep06StartGrammarResultV1,
  submitMissingNativeScriptTxStep06StartGrammarV1,
} from "../missing-native-script-tx/submit-missing-native-script-tx-step-06-staged.js";
import {
  type SubmitMissingNativeScriptTxStep07ResultV1,
  submitMissingNativeScriptTxStep07V1,
} from "../missing-native-script-tx/submit-missing-native-script-tx-step-07.js";
import type { MissingNativeScriptUtxoContractsV1 } from "./contracts-v1.js";
import { missingNativeScriptUtxoStagedContractsV1 } from "./staged-submit-adapter-v1.js";

type StartArgs = Omit<
  Parameters<typeof submitMissingNativeScriptTxStep06StartGrammarV1>[0],
  "contracts"
> & {
  readonly contracts: MissingNativeScriptUtxoContractsV1;
};

/** Starts Q33 grammar certification from step 05 and enters step 06. */
export const submitMissingNativeScriptUtxoStep05StartGrammarV1 = async ({
  contracts,
  ...args
}: StartArgs): Promise<SubmitMissingNativeScriptTxStep06StartGrammarResultV1> =>
  await submitMissingNativeScriptTxStep06StartGrammarV1({
    ...args,
    contracts: missingNativeScriptUtxoStagedContractsV1(contracts),
  });

type ResumeArgs = Omit<
  Parameters<typeof submitMissingNativeScriptTxStep07V1>[0],
  "contracts"
> & {
  readonly contracts: MissingNativeScriptUtxoContractsV1;
};

/** Resumes Q33 grammar certification or crosses into semantic scanning. */
export const submitMissingNativeScriptUtxoStep06V1 = async ({
  contracts,
  ...args
}: ResumeArgs): Promise<SubmitMissingNativeScriptTxStep07ResultV1> =>
  await submitMissingNativeScriptTxStep07V1({
    ...args,
    contracts: missingNativeScriptUtxoStagedContractsV1(contracts),
  });
