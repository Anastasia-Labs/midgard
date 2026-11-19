/**
 * `native-script-decoding` thread init (offchain plan §4.2, design §10 Q2).
 *
 * Delegates to the shared `buildResolvedInit` in `src/submit-init.ts` with
 * the family's explicit contracts record and catalogue category: the
 * `nativeScriptDecoding` entry in the production `buildInit` category union is
 * parent-owned and lands at registration.
 */
import {
  buildResolvedInit,
  type ResolvedInitBuild,
  type SubmitResolvedInitParams,
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

export type NativeScriptDecodingInitBuild = ResolvedInitBuild;

export const buildNativeScriptDecodingInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: NativeScriptDecodingInitContracts;
    readonly category: NativeScriptDecodingCatalogueCategory;
  },
): Promise<NativeScriptDecodingInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw nativeScriptDecodingSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: NATIVE_SCRIPT_DECODING_CATEGORY_LABEL,
  });
};
