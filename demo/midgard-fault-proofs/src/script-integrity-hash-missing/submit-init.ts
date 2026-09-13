import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts.js";

/** Registered-category init using the shared computation-thread mint. */
export const submitScriptIntegrityHashMissingInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: ScriptIntegrityHashMissingContracts;
  },
) => await submitNativeScriptDecodingInit(args);
