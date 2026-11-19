import type { InitBuild } from "../submit-init.resolve-non-existent-input-no-index-init.js";
import { buildInit } from "../submit-init.submit-init.js";

type Base = Parameters<typeof buildInit>[0];
export type SubmitNativeScriptInvalidInitParams = Omit<Base, "fraudCategory">;
export type NativeScriptInvalidInitBuild = InitBuild;

export const buildNativeScriptInvalidInit = async (
  params: SubmitNativeScriptInvalidInitParams,
): Promise<NativeScriptInvalidInitBuild> =>
  await buildInit({
    ...params,
    fraudCategory: "nativeScriptInvalid",
  });
