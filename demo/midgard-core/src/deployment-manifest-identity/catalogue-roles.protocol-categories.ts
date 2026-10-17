export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER =
  Object.freeze([
    "doubleSpend",
    "nonExistentInput",
    "nonExistentInputNoIndex",
    "invalidRange",
    "transitionTrace",
    "zeroInput",
    "validationTraceDispute",
    "daHashPreimage",
    "noReferenceInput",
    "referenceInputNoIdx",
    "invalidSignature",
    "fabricatedDeposit",
    "fabricatedWithdrawal",
    "nativeScriptDecoding",
    "missingSignature",
    "withdrawnReferenceInput",
    "canonicalDecodability",
    "committedFieldShape",
    "minFee",
    "withdrawalMistag",
    "doubleWithdraw",
    "l2TxMistag",
    "withdrawnInput",
    "valueNotPreserved",
    "inputSetUniqueness",
    "mintAuthorization",
    "networkId",
    "nativeScriptInvalid",
    "minAda",
    "fieldPreimageLengthMismatch",
    "fieldItemWidthIllegal",
    "witnessScriptDecoding",
    "scriptIntegrityHashMissing",
    "transactionOutputNonCanonical",
    "resolvedOutputNonCanonical",
    "mintDeclaredAssetLimit",
    "spendInputSignerMissing",
    "protectedOutputSignerMissing",
    "observersForbiddenOnUntaggedNetwork",
    "observerOrderInvalid",
    "redeemerCanonicity",
    "outputReferenceScriptDecoding",
    "executionSourceScriptDecoding",
    "receivePurposeLanguage",
    "unusedScriptWitness",
    "missingScriptSource",
    "missingRedeemer",
    "unusedRedeemer",
    "executionNativeScriptInvalid",
    "scriptIntegrityHashMismatch",
    "distinctAssetAccumulationLimit",
    "mintItemNonCanonical",
  ] as const);

export type DeploymentManifestFraudProofCatalogueCategory =
  (typeof DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER)[number];

/** Frozen wire identities; category array position is presentation only. */
export const DEPLOYMENT_MANIFEST_FRAUD_PROOF_CATALOGUE_CATEGORY_IDS =
  Object.freeze({
    doubleSpend: "00000000",
    nonExistentInput: "00000001",
    nonExistentInputNoIndex: "00000002",
    invalidRange: "00000003",
    transitionTrace: "00000004",
    zeroInput: "00000005",
    validationTraceDispute: "00000006",
    daHashPreimage: "00000007",
    noReferenceInput: "00000008",
    referenceInputNoIdx: "00000009",
    invalidSignature: "0000000a",
    fabricatedDeposit: "0000000b",
    fabricatedWithdrawal: "0000000c",
    nativeScriptDecoding: "0000000d",
    missingSignature: "0000000e",
    withdrawnReferenceInput: "00000010",
    canonicalDecodability: "00000011",
    committedFieldShape: "00000012",
    minFee: "00000013",
    withdrawalMistag: "00000014",
    doubleWithdraw: "00000015",
    l2TxMistag: "00000017",
    withdrawnInput: "00000018",
    valueNotPreserved: "00000019",
    inputSetUniqueness: "0000001a",
    mintAuthorization: "0000001b",
    networkId: "0000001c",
    nativeScriptInvalid: "0000001e",
    minAda: "0000001f",
    fieldPreimageLengthMismatch: "00000020",
    fieldItemWidthIllegal: "00000021",
    witnessScriptDecoding: "00000022",
    scriptIntegrityHashMissing: "00000023",
    transactionOutputNonCanonical: "00000029",
    mintItemNonCanonical: "00000036",
    resolvedOutputNonCanonical: "00000026",
    mintDeclaredAssetLimit: "0000002c",
    spendInputSignerMissing: "00000027",
    protectedOutputSignerMissing: "0000002b",
    observersForbiddenOnUntaggedNetwork: "00000024",
    outputReferenceScriptDecoding: "0000002a",
    executionSourceScriptDecoding: "00000031",
    observerOrderInvalid: "00000025",
    redeemerCanonicity: "00000028",
    receivePurposeLanguage: "00000034",
    unusedScriptWitness: "0000002f",
    missingScriptSource: "0000002d",
    missingRedeemer: "0000002e",
    unusedRedeemer: "00000030",
    executionNativeScriptInvalid: "00000032",
    scriptIntegrityHashMismatch: "00000033",
    distinctAssetAccumulationLimit: "00000035",
  } as const satisfies Readonly<
    Record<DeploymentManifestFraudProofCatalogueCategory, string>
  >);

export type DeploymentManifestFraudProofCatalogueCategoryIdentity = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

/** `categories` holds exactly the deployment's catalogue (the profile's set). */
export type DeploymentManifestFraudProofCatalogueIdentity = {
  readonly root: string;
  readonly categories: Readonly<
    Partial<
      Record<
        DeploymentManifestFraudProofCatalogueCategory,
        DeploymentManifestFraudProofCatalogueCategoryIdentity
      >
    >
  >;
};
