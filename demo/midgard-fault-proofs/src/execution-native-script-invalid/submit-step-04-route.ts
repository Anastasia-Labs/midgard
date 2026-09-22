import {
  decodeMidgardAddressWitnessItem,
  decodeMidgardVersionedScript,
} from "@al-ft/midgard-core";

import { executionNativeScriptInvalidUsesDirectRoute } from "./evidence-machine.js";
import { submitExecutionNativeScriptInvalidStep04StartSignerScan } from "./submit-step-04.js";
import { submitExecutionNativeScriptInvalidStep04Direct } from "./submit-step-04-direct.js";

type Step04Input = Omit<
  Parameters<typeof submitExecutionNativeScriptInvalidStep04Direct>[0],
  "addressWitnessVerificationKeys"
>;

/** Dispatch the authenticated step-4 material to its admissible builder. */
export const submitExecutionNativeScriptInvalidStep04 = async (
  input: Step04Input,
) => {
  const script = decodeMidgardVersionedScript(input.scriptItemCbor);
  if (
    executionNativeScriptInvalidUsesDirectRoute({
      signerCount: input.addressWitnessItems.length,
      scriptBytes: script.scriptBytes.length,
    })
  ) {
    return submitExecutionNativeScriptInvalidStep04Direct({
      ...input,
      addressWitnessVerificationKeys: input.addressWitnessItems.map(
        (item) => decodeMidgardAddressWitnessItem(item).verificationKey,
      ),
    });
  }
  return submitExecutionNativeScriptInvalidStep04StartSignerScan(input);
};
