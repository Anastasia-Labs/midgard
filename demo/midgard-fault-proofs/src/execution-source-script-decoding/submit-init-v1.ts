import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ExecutionSourceScriptDecodingContracts } from "./contracts-v1.js";

export const submitExecutionSourceScriptDecodingInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: ExecutionSourceScriptDecodingContracts;
  },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
