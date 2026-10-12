import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";

export const WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION =
  "midgard-watcher-signed-deployment-identity-v1" as const;

export const WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION =
  "midgard-watcher-deployment-release-bindings-v1" as const;

export const WATCHER_DEPLOYMENT_IDENTITY_SIGNATURE_DOMAIN =
  "midgard-watcher-deployment-identity-signature-v1" as const;

export const WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-deployment-protocol-script-authority-v1" as const;

export const WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-deployment-protocol-parameter-authority-v1" as const;

export const WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-deployment-availability-challenge-authority-v1" as const;

export const HEX_28 = /^[0-9a-f]{56}$/u;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export const HEX_64 = /^[0-9a-f]{128}$/u;

export const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

const COMMITMENT_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;

export const CATALOGUE_CATEGORY_TO_CONTRACT = Object.freeze({
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
} as const);

export const REFERENCE_SCRIPT_ROLES = Object.freeze(
  Object.keys(DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
);

export type WatcherDeploymentIdentityErrorCode =
  | "canonical_manifest_invalid"
  | "durable_marker_mismatch"
  | "duplicate_trust_root"
  | "invalid_field"
  | "invalid_signature"
  | "invalid_trust_root"
  | "missing_durable_marker"
  | "missing_field"
  | "mismatched_identity"
  | "unknown_field"
  | "untrusted_signer";

export type WatcherDeploymentIdentityDiagnostic = Readonly<{
  code: WatcherDeploymentIdentityErrorCode;
  path: string;
  message: string;
}>;

export class WatcherDeploymentIdentityError extends Error {
  readonly code: WatcherDeploymentIdentityErrorCode;
  readonly path: string;

  constructor(code: WatcherDeploymentIdentityErrorCode, path: string) {
    super(`Watcher deployment identity rejected: ${code} at ${path}`);
    this.name = "WatcherDeploymentIdentityError";
    this.code = code;
    this.path = path;
  }
}

export const fail = (
  code: WatcherDeploymentIdentityErrorCode,
  path: string,
): never => {
  throw new WatcherDeploymentIdentityError(code, path);
};

export const watcherDeploymentIdentityDiagnostic = (
  error: unknown,
): WatcherDeploymentIdentityDiagnostic => {
  if (error instanceof WatcherDeploymentIdentityError) {
    return {
      code: error.code,
      path: error.path,
      message: error.message,
    };
  }
  return {
    code: "canonical_manifest_invalid",
    path: "$.manifest",
    message:
      "Watcher deployment identity rejected: canonical_manifest_invalid at $.manifest",
  };
};

type JsonRecord = Record<string, unknown>;

export const plainRecord = (value: unknown, path: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const prototype = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("invalid_field", path);
  }
  const record = value as JsonRecord;
  if (Reflect.ownKeys(record).length !== Object.keys(record).length) {
    fail("invalid_field", path);
  }
  return record;
};

export const exactRecord = (
  value: unknown,
  path: string,
  requiredKeys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const allowed = new Set(requiredKeys);
  for (const key of Object.keys(record)) {
    if (!allowed.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_field", `${path}.${key}`);
    }
  }
  return record;
};

export const exactString = (
  value: unknown,
  path: string,
  pattern: RegExp,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    fail("invalid_field", path);
  }
  return value as string;
};

export const exactDynamicHexMap = (
  value: unknown,
  path: string,
  requiredKeys: readonly string[] | null,
  valuePattern: RegExp,
): Readonly<Record<string, string>> => {
  const record = plainRecord(value, path);
  const keys = Object.keys(record).sort();
  if (keys.length === 0) {
    fail("missing_field", path);
  }
  if (
    requiredKeys !== null &&
    (keys.length !== requiredKeys.length ||
      keys.some((key, index) => key !== [...requiredKeys].sort()[index]))
  ) {
    fail("mismatched_identity", path);
  }
  const parsed = Object.fromEntries(
    keys.map((key) => {
      if (requiredKeys === null && !COMMITMENT_NAME.test(key)) {
        fail("invalid_field", `${path}.${key}`);
      }
      return [key, exactString(record[key], `${path}.${key}`, valuePattern)];
    }),
  );
  return Object.freeze(parsed);
};

export const equalStringMaps = (
  actual: Readonly<Record<string, string>>,
  expected: Readonly<Record<string, string>>,
): boolean => {
  const actualKeys = Object.keys(actual).sort();
  const expectedKeys = Object.keys(expected).sort();
  return (
    actualKeys.length === expectedKeys.length &&
    actualKeys.every(
      (key, index) =>
        key === expectedKeys[index] && actual[key] === expected[key],
    )
  );
};

export type WatcherDeploymentTrustRoot = Readonly<{
  trustRootId: string;
  publicKeySpkiDerHex: string;
}>;

export type WatcherReferenceScriptIdentity = Readonly<{
  scriptHash: string;
  outRef: string;
}>;

export type WatcherFraudProofCatalogueIdentity = Readonly<{
  root: string;
  categories: Readonly<
    Record<
      keyof typeof CATALOGUE_CATEGORY_TO_CONTRACT,
      Readonly<{ categoryId: string; scriptHash: string }>
    >
  >;
}>;

export type WatcherDeploymentIdentityPolicy = Readonly<{
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  hubOracleOneShotOutRef: string;
  appliedScriptHashes: Readonly<Record<string, string>>;
  referenceScripts: Readonly<Record<string, WatcherReferenceScriptIdentity>>;
  fraudProofCatalogue: WatcherFraudProofCatalogueIdentity;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  daMode: "authenticated_committee_v1";
  daIdentityDigest: string;

  fundingProfileBundleDigest: string;
  blueprintHash: string;
}>;

export type ParsedPolicy = WatcherDeploymentIdentityPolicy;
