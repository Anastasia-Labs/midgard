import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { MissingScriptSourceContractsV1 } from "./contracts-v1.js";

export const submitMissingScriptSourceInitV1 = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: MissingScriptSourceContractsV1;
  },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
