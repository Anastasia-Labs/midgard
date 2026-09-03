import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { MissingScriptSourceContracts } from "./contracts-v1.js";

export const submitMissingScriptSourceInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: MissingScriptSourceContracts;
  },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
