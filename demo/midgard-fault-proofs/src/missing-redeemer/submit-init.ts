import { submitNativeScriptDecodingInit } from "../native-script-decoding/submit-native-script-decoding-init.js";
import type { MissingRedeemerContracts } from "./contracts.js";

/**
 * Generic computation-thread `Init` for the seven-role chain: the catalogue
 * category must register the very step-01 the chain deploys, and the thread
 * is minted at that step's address.
 */
export const submitMissingRedeemerInit = async (
  args: Omit<
    Parameters<typeof submitNativeScriptDecodingInit>[0],
    "contracts"
  > & {
    readonly contracts: MissingRedeemerContracts;
  },
) =>
  await submitNativeScriptDecodingInit(
    args as unknown as Parameters<typeof submitNativeScriptDecodingInit>[0],
  );
