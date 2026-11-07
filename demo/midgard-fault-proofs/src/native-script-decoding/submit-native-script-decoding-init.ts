/**
 * `native-script-decoding` thread init (offchain plan §4.2, design §10 Q2).
 *
 * Delegates to the shared `submitResolvedInit` in `src/submit-init.ts` with
 * the family's explicit contracts record and catalogue category: the
 * `nativeScriptDecoding` entry in the production `submitInit` category union is
 * parent-owned and lands at registration.
 */
import {
  submitResolvedInit,
  type SubmitResolvedInitParams,
  type SubmitResolvedInitResult,
} from "../submit-init.js";
import {
  NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  type NativeScriptDecodingContracts,
  type NativeScriptDecodingStepContract,
} from "./contracts.js";
import {
  type NativeScriptDecodingCatalogueCategory,
  nativeScriptDecodingSubmitError,
} from "./submit-common.js";

export type NativeScriptDecodingInitContracts = Omit<
  NativeScriptDecodingContracts,
  "steps"
> & {
  readonly steps: readonly [
    NativeScriptDecodingStepContract,
    ...NativeScriptDecodingStepContract[],
  ];
};

export type SubmitNativeScriptDecodingInitResult = SubmitResolvedInitResult;

export const submitNativeScriptDecodingInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: NativeScriptDecodingInitContracts;
    readonly category: NativeScriptDecodingCatalogueCategory;
  },
): Promise<SubmitNativeScriptDecodingInitResult> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw nativeScriptDecodingSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await submitResolvedInit({
    ...params,
    label: NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  });
};
