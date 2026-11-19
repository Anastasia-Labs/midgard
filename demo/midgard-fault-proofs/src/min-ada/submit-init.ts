import { buildInit, type InitBuild } from "../submit-init.js";

type Base = Parameters<typeof buildInit>[0];
export type SubmitMinAdaInitParams = Omit<Base, "fraudCategory">;
export type MinAdaInitBuild = InitBuild;

export const buildMinAdaInit = async (
  params: SubmitMinAdaInitParams,
): Promise<MinAdaInitBuild> =>
  await buildInit({
    ...params,
    fraudCategory: "minAda",
  });
