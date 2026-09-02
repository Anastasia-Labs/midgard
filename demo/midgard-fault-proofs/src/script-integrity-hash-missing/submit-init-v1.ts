import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ScriptIntegrityHashMissingContractsV1 } from "./contracts-v1.js";

/** Registered-category init using the shared computation-thread mint. */
export const submitScriptIntegrityHashMissingInitV1 = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: ScriptIntegrityHashMissingContractsV1;
  },
) => await submitNativeScriptDecodingInit(args);
