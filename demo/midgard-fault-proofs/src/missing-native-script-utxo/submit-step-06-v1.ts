import {
  submitMissingNativeScriptTxStep06StartGrammar,
  type SubmitMissingNativeScriptTxStep06StartGrammarResult,
} from "../missing-native-script-tx/submit-missing-native-script-tx-step-06-staged.js";
import {
  submitMissingNativeScriptTxStep07,
  type SubmitMissingNativeScriptTxStep07Result,
} from "../missing-native-script-tx/submit-missing-native-script-tx-step-07.js";
import type { MissingNativeScriptUtxoContracts } from "./contracts-v1.js";
import { missingNativeScriptUtxoStagedContracts } from "./staged-submit-adapter-v1.js";

type StartArgs = Omit<
  Parameters<typeof submitMissingNativeScriptTxStep06StartGrammar>[0],
  "contracts"
> & {
  readonly contracts: MissingNativeScriptUtxoContracts;
};

/** Starts Q33 grammar certification from step 05 and enters step 06. */
export const submitMissingNativeScriptUtxoStep05StartGrammar = async ({
  contracts,
  ...args
}: StartArgs): Promise<SubmitMissingNativeScriptTxStep06StartGrammarResult> =>
  await submitMissingNativeScriptTxStep06StartGrammar({
    ...args,
    contracts: missingNativeScriptUtxoStagedContracts(contracts),
  });

type ResumeArgs = Omit<
  Parameters<typeof submitMissingNativeScriptTxStep07>[0],
  "contracts"
> & {
  readonly contracts: MissingNativeScriptUtxoContracts;
};

/** Resumes Q33 grammar certification or crosses into semantic scanning. */
export const submitMissingNativeScriptUtxoStep06 = async ({
  contracts,
  ...args
}: ResumeArgs): Promise<SubmitMissingNativeScriptTxStep07Result> =>
  await submitMissingNativeScriptTxStep07({
    ...args,
    contracts: missingNativeScriptUtxoStagedContracts(contracts),
  });
