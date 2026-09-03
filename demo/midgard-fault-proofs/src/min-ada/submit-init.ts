import { submitInit, type SubmitInitResult } from "../submit-init.js";

type Base = Parameters<typeof submitInit>[0];
export type SubmitMinAdaInitParams = Omit<Base, "fraudCategory">;
export type SubmitMinAdaInitResult = SubmitInitResult;

export const submitMinAdaInit = async (
  params: SubmitMinAdaInitParams,
): Promise<SubmitMinAdaInitResult> =>
  await submitInit({
    ...params,
    fraudCategory: "minAda",
  });
