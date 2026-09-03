import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { UnusedScriptWitnessContracts } from "./contracts-v1.js";

export const submitUnusedScriptWitnessInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & { readonly contracts: UnusedScriptWitnessContracts },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
