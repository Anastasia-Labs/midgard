import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { WitnessScriptDecodingContractsV1 } from "./contracts-v1.js";

/** Explicit deployed-manifest init; the shared minting transaction is family-neutral. */
export const submitWitnessScriptDecodingInitV1 = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & { readonly contracts: WitnessScriptDecodingContractsV1 },
) => await submitNativeScriptDecodingInit(args);
