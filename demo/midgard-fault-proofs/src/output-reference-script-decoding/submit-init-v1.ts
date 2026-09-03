import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { OutputReferenceScriptDecodingContracts } from "./contracts-v1.js";

export const submitOutputReferenceScriptDecodingInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: OutputReferenceScriptDecodingContracts;
  },
) => await submitNativeScriptDecodingInit(args);
