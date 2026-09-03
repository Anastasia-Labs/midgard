import { submitInit, type SubmitInitResult } from "../submit-init.js";

type Base = Parameters<typeof submitInit>[0];
export type SubmitMissingNativeScriptUtxoInitParams = Omit<
  Base,
  "fraudCategory"
>;
export type SubmitMissingNativeScriptUtxoInitResult = SubmitInitResult;

export const submitMissingNativeScriptUtxoInit = async (
  params: SubmitMissingNativeScriptUtxoInitParams,
): Promise<SubmitMissingNativeScriptUtxoInitResult> =>
  await submitInit({
    ...params,
    fraudCategory: "missingNativeScriptUtxo",
  });
