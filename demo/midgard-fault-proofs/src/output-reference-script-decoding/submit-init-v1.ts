import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { OutputReferenceScriptDecodingContractsV1 } from "./contracts-v1.js";

export const submitOutputReferenceScriptDecodingInitV1 = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: OutputReferenceScriptDecodingContractsV1;
  },
) => await submitNativeScriptDecodingInit(args);
