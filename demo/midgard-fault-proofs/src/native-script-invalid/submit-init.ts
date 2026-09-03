import { submitInit, type SubmitInitResult } from "../submit-init.js";

type Base = Parameters<typeof submitInit>[0];
export type SubmitNativeScriptInvalidInitParams = Omit<Base, "fraudCategory">;
export type SubmitNativeScriptInvalidInitResult = SubmitInitResult;

export const submitNativeScriptInvalidInit = async (
  params: SubmitNativeScriptInvalidInitParams,
): Promise<SubmitNativeScriptInvalidInitResult> =>
  await submitInit({
    ...params,
    fraudCategory: "nativeScriptInvalid",
  });
