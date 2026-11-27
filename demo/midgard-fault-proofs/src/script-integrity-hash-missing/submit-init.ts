import { buildNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { ScriptIntegrityHashMissingContracts } from "./contracts.js";

/** Registered-category init using the shared computation-thread mint. */
export const buildScriptIntegrityHashMissingInit = async (
  args: Omit<
    Parameters<typeof buildNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: ScriptIntegrityHashMissingContracts;
  },
) => await buildNativeScriptDecodingInit(args);
