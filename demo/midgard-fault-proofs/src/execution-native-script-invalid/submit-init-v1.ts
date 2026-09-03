import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ExecutionNativeScriptInvalidContracts } from "./contracts-v1.js";

export const submitExecutionNativeScriptInvalidInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: ExecutionNativeScriptInvalidContracts;
  },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
