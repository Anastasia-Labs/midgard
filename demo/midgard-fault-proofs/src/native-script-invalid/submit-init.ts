import { buildInit, type InitBuild } from "../submit-init.js";

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
