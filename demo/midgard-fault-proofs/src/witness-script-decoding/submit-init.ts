import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { WitnessScriptDecodingContracts } from "./contracts.js";

/** Explicit deployed-manifest init; the shared minting transaction is family-neutral. */
export const submitWitnessScriptDecodingInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & { readonly contracts: WitnessScriptDecodingContracts },
) => await submitNativeScriptDecodingInit(args);
