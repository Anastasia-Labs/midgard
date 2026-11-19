import { buildNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { WitnessScriptDecodingContracts } from "./contracts.js";

/** Explicit deployed-manifest init; the shared minting transaction is family-neutral. */
export const buildWitnessScriptDecodingInit = async (
  args: Omit<
    Parameters<typeof buildNativeScriptDecodingInit>[0],
    "contracts"
  > & { readonly contracts: WitnessScriptDecodingContracts },
) => await buildNativeScriptDecodingInit(args);
