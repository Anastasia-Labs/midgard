import {
  type DeploymentManifestFraudProofCatalogueCategory,
  type DeploymentManifestFraudProofCatalogueCategoryIdentity,
  verifyDeploymentManifestFraudProofCatalogueIdentity,
  verifyReferenceScriptPublicationAuthority,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
} from "@al-ft/midgard-sdk";
import { validatorToScriptHash } from "@lucid-evolution/lucid";

import {
  requireExactKeys,
  requireLowercaseHex,
  requireLowercaseVariableHex,
  requireNonEmptyString,
  requireNonNegativeSafeInteger,
  requireObject,
  requirePositiveSafeInteger,
} from "./deployment-manifest.require-out-ref-string.js";

export const validateReferenceScriptAuthPolicy = (
  candidate: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["policyId", "nativeScript", "tokenNames", "postTimelockAudit"],
    [],
    "referenceScriptAuthPolicy",
  );
  const policyId = requireLowercaseHex(
    candidate.policyId,
    28,
    "referenceScriptAuthPolicy.policyId",
  );
  const nativeScript = requireObject(
    candidate.nativeScript,
    "referenceScriptAuthPolicy.nativeScript",
  );
  requireExactKeys(
    nativeScript,
    [
      "type",
      "cborHex",
      "expiresAtSlot",
      "expiresAtUnixTime",
      "timelockDurationMs",
    ],
    [],
    "referenceScriptAuthPolicy.nativeScript",
  );
  if (nativeScript.type !== "Native") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.nativeScript.type must be Native",
    );
  }
  const nativeScriptCbor = requireLowercaseVariableHex(
    nativeScript.cborHex,
    "referenceScriptAuthPolicy.nativeScript.cborHex",
  );
  const expiresAtSlot = requireNonNegativeSafeInteger(
    nativeScript.expiresAtSlot,
    "referenceScriptAuthPolicy.nativeScript.expiresAtSlot",
  );
  requireNonNegativeSafeInteger(
    nativeScript.expiresAtUnixTime,
    "referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime",
  );
  requirePositiveSafeInteger(
    nativeScript.timelockDurationMs,
    "referenceScriptAuthPolicy.nativeScript.timelockDurationMs",
  );
  let derivedPolicyId: string;
  try {
    derivedPolicyId = validatorToScriptHash({
      type: "Native",
      script: nativeScriptCbor,
    });
  } catch (cause) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.nativeScript.cborHex is invalid: ${String(cause)}`,
    );
  }
  if (derivedPolicyId !== policyId) {
    throw new Error(
      `Deployment manifest referenceScriptAuthPolicy.policyId mismatch: expected ${derivedPolicyId}`,
    );
  }

  const tokenNames = requireObject(
    candidate.tokenNames,
    "referenceScriptAuthPolicy.tokenNames",
  );
  const tokenNameKeys = Object.keys(REFERENCE_SCRIPT_AUTH_TOKEN_NAMES);
  requireExactKeys(
    tokenNames,
    tokenNameKeys,
    [],
    "referenceScriptAuthPolicy.tokenNames",
  );
  for (const [role, expectedTokenName] of Object.entries(
    REFERENCE_SCRIPT_AUTH_TOKEN_NAMES,
  )) {
    if (tokenNames[role] !== expectedTokenName) {
      throw new Error(
        `Deployment manifest referenceScriptAuthPolicy.tokenNames.${role} must equal ${expectedTokenName}`,
      );
    }
  }

  const postTimelockAudit = requireObject(
    candidate.postTimelockAudit,
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  requireExactKeys(
    postTimelockAudit,
    ["required", "rule"],
    [],
    "referenceScriptAuthPolicy.postTimelockAudit",
  );
  if (typeof postTimelockAudit.required !== "boolean") {
    throw new Error(
      "Deployment manifest referenceScriptAuthPolicy.postTimelockAudit.required must be a boolean",
    );
  }
  verifyReferenceScriptPublicationAuthority({
    cborHex: nativeScriptCbor,
    expiresAtSlot,
    postTimelockAuditRequired: postTimelockAudit.required,
  });
  requireNonEmptyString(
    postTimelockAudit.rule,
    "referenceScriptAuthPolicy.postTimelockAudit.rule",
  );
};

export const validateFraudProofCatalogue = (
  candidate: Record<string, unknown>,
  contracts: Record<string, unknown>,
): void => {
  requireExactKeys(
    candidate,
    ["root", "categories"],
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  const root = requireLowercaseHex(
    candidate.root,
    32,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
  );
  const categories = requireObject(
    candidate.categories,
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  requireExactKeys(
    categories,
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
    [],
    "contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  const contractNameByCategory = {
    doubleSpend: "fraudProofDoubleSpend",
    nonExistentInput: "fraudProofNonExistentInput",
    nonExistentInputNoIndex: "fraudProofNonExistentInputNoIndex",
    invalidRange: "fraudProofInvalidRange",
    transitionTrace: "fraudProofTransitionTrace",
    zeroInput: "fraudProofZeroInput",
    validationTraceDispute: "validationTraceDispute",
    daHashPreimage: "fraudProofDaHashPreimage",
    noReferenceInput: "fraudProofNoReferenceInput",
    referenceInputNoIdx: "fraudProofReferenceInputNoIdx",
    invalidSignature: "fraudProofInvalidSignature",
    fabricatedDeposit: "fraudProofFabricatedDeposit",
    fabricatedWithdrawal: "fraudProofFabricatedWithdrawal",
    nativeScriptDecoding: "fraudProofNativeScriptDecoding",
    missingSignature: "fraudProofMissingSignature",
    withdrawnReferenceInput: "fraudProofWithdrawnReferenceInput",
    canonicalDecodability: "fraudProofCanonicalDecodability",
    committedFieldShape: "fraudProofCommittedFieldShape",
    minFee: "fraudProofMinFee",
    withdrawalMistag: "fraudProofWithdrawalMistag",
    doubleWithdraw: "fraudProofDoubleWithdraw",
    crossBlockDuplicateEvent: "fraudProofCrossBlockDuplicateEvent",
    l2TxMistag: "fraudProofL2TxMistag",
    withdrawnInput: "fraudProofWithdrawnInput",
    valueNotPreserved: "fraudProofValueNotPreserved",
    inputSetUniqueness: "fraudProofInputSetUniqueness",
    mintAuthorization: "fraudProofMintAuthorization",
    networkId: "fraudProofNetworkId",
    nativeScriptInvalid: "fraudProofNativeScriptInvalid",
    minAda: "fraudProofMinAda",
    fieldPreimageLengthMismatch: "fraudProofFieldPreimageLengthMismatch",
    fieldItemWidthIllegal: "fraudProofFieldItemWidthIllegal",
    witnessScriptDecoding: "fraudProofWitnessScriptDecoding",
    scriptIntegrityHashMissing: "fraudProofScriptIntegrityHashMissing",
    transactionOutputNonCanonical: "fraudProofTransactionOutputNonCanonical",
    mintItemNonCanonical: "fraudProofMintItemNonCanonical",
    resolvedOutputNonCanonical: "fraudProofResolvedOutputNonCanonical",
    mintDeclaredAssetLimit: "fraudProofMintDeclaredAssetLimit",
    spendInputSignerMissing: "fraudProofSpendInputSignerMissing",
    protectedOutputSignerMissing: "fraudProofProtectedOutputSignerMissing",
    observersForbiddenOnUntaggedNetwork:
      "fraudProofObserversForbiddenOnUntaggedNetwork",
    outputReferenceScriptDecoding: "fraudProofOutputReferenceScriptDecoding",
    executionSourceScriptDecoding: "fraudProofExecutionSourceScriptDecoding",
    observerOrderInvalid: "fraudProofObserverOrderInvalid",
    redeemerCanonicity: "fraudProofRedeemerCanonicity",
    receivePurposeLanguage: "fraudProofReceivePurposeLanguage",
    unusedScriptWitness: "fraudProofUnusedScriptWitness",
    missingScriptSource: "fraudProofMissingScriptSource",
    missingRedeemer: "fraudProofMissingRedeemer",
    unusedRedeemer: "fraudProofUnusedRedeemer",
    executionNativeScriptInvalid: "fraudProofExecutionNativeScriptInvalid",
    scriptIntegrityHashMismatch: "fraudProofScriptIntegrityHashMismatch",
    distinctAssetAccumulationLimit: "fraudProofDistinctAssetAccumulationLimit",
  } as const;
  const parsedCategories = {} as Record<
    DeploymentManifestFraudProofCatalogueCategory,
    DeploymentManifestFraudProofCatalogueCategoryIdentity
  >;
  for (const categoryName of FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER) {
    const field = `contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${categoryName}`;
    const category = requireObject(categories[categoryName], field);
    requireExactKeys(
      category,
      ["categoryId", "scriptHash", "membershipProofCbor"],
      [],
      field,
    );
    const categoryId = requireLowercaseHex(
      category.categoryId,
      4,
      `${field}.categoryId`,
    );
    const scriptHash = requireLowercaseHex(
      category.scriptHash,
      28,
      `${field}.scriptHash`,
    );
    const membershipProofCbor = requireLowercaseVariableHex(
      category.membershipProofCbor,
      `${field}.membershipProofCbor`,
    );
    parsedCategories[categoryName] = {
      categoryId,
      scriptHash,
      membershipProofCbor,
    };
    const expectedContract = requireObject(
      contracts[contractNameByCategory[categoryName]],
      `contracts.${contractNameByCategory[categoryName]}`,
    );
    if (expectedContract.scriptHash !== scriptHash) {
      throw new Error(
        `Deployment manifest ${field}.scriptHash must match contracts.${contractNameByCategory[categoryName]}.scriptHash`,
      );
    }
  }
  verifyDeploymentManifestFraudProofCatalogueIdentity({
    root,
    categories: parsedCategories,
  });
};
