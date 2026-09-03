import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { UnusedRedeemerContracts } from "./contracts-v1.js";

export const submitUnusedRedeemerInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & { readonly contracts: UnusedRedeemerContracts },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
