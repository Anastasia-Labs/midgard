/**
 * `missing-signature` thread init (offchain plan §4.2, plan §2.2 D1).
 *
 * Delegates to the shared `buildResolvedInit` in `src/submit-init.ts` with
 * the family's explicit contracts record and catalogue category: the
 * `missingSignature` entry in the production `buildInit` category union is
 * parent-owned and lands at registration.
 */
import {
  buildResolvedInit,
  type ResolvedInitBuild,
  type SubmitResolvedInitParams,
} from "../submit-init.js";
import {
  MISSING_SIGNATURE_CATEGORY_LABEL,
  type MissingSignatureContracts,
} from "./contracts.js";
import {
  type MissingSignatureCatalogueCategory,
  missingSignatureSubmitError,
} from "./submit-common.js";

export type MissingSignatureInitBuild = ResolvedInitBuild;

export const buildMissingSignatureInit = async (
  params: Omit<SubmitResolvedInitParams, "contracts" | "category"> & {
    readonly contracts: MissingSignatureContracts;
    readonly category: MissingSignatureCatalogueCategory;
  },
): Promise<MissingSignatureInitBuild> => {
  const { contracts, category } = params;
  if (category.scriptHash !== contracts.steps[0].spendingScriptHash) {
    throw missingSignatureSubmitError(
      `catalogue category registers ${category.scriptHash}, but the deployed step-01 hashes to ${contracts.steps[0].spendingScriptHash}.`,
    );
  }
  return await buildResolvedInit({
    ...params,
    label: MISSING_SIGNATURE_CATEGORY_LABEL,
  });
};
