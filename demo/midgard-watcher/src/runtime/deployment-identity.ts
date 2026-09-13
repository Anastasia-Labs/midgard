import {
  createHash,
  createPublicKey,
  timingSafeEqual,
  verify as verifySignature,
} from "node:crypto";

import { MIDGARD_CONSENSUS_PROFILE_DIGEST } from "@al-ft/midgard-core/consensus-profile";
import {
  assertDeploymentMarkerMatches,
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifestAvailabilityChallenge,
  type DeploymentManifestCardanoProtocolParameters,
  type DeploymentMarker,
  makeDeploymentMarker,
  parseDeploymentManifestAvailabilityChallenge,
  parseDeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestEconomics,
  verifyDeploymentManifestIdentity,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_AUTHORITY,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
  type FraudProofReleaseEconomicsAuthority,
  type FraudProofReleaseFinalityAuthority,
  type ReleaseFraudProofEconomicsPolicy,
  type ReleaseL1FinalityPolicy,
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  validateVerifiedFraudProofReleaseFinalityPolicy,
  type VerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";
import {
  type AuthenticatedValidator,
  buildDepositValidators,
  buildHubOracleMintingValidator,
  buildTxOrderValidators,
  buildWithdrawalValidators,
  HUB_ORACLE_ASSET_NAME,
  parseFaultProofBlueprint,
} from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { parseWatcherStrictJsonValue } from "./config.js";

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

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const HEX_64 = /^[0-9a-f]{128}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const COMMITMENT_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;

const CATALOGUE_CATEGORY_TO_CONTRACT = Object.freeze({
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
  missingNativeScriptTx: "fraudProofMissingNativeScriptTx",
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
  missingNativeScriptUtxo: "fraudProofMissingNativeScriptUtxo",
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

const REFERENCE_SCRIPT_ROLES = Object.freeze(
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

const fail = (
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

const plainRecord = (value: unknown, path: string): JsonRecord => {
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

const exactRecord = (
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

const exactString = (value: unknown, path: string, pattern: RegExp): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    fail("invalid_field", path);
  }
  return value as string;
};

const exactDynamicHexMap = (
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

const equalStringMaps = (
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

type ParsedPolicy = WatcherDeploymentIdentityPolicy;

const parseReferenceScriptPolicy = (
  value: unknown,
  path: string,
): Readonly<Record<string, WatcherReferenceScriptIdentity>> => {
  const record = plainRecord(value, path);
  const actualRoles = Object.keys(record).sort();
  const expectedRoles = [...REFERENCE_SCRIPT_ROLES].sort();
  if (
    actualRoles.length !== expectedRoles.length ||
    actualRoles.some((role, index) => role !== expectedRoles[index])
  ) {
    fail("mismatched_identity", path);
  }
  return Object.freeze(
    Object.fromEntries(
      actualRoles.map((role) => {
        const identity = exactRecord(record[role], `${path}.${role}`, [
          "scriptHash",
          "outRef",
        ]);
        return [
          role,
          Object.freeze({
            scriptHash: exactString(
              identity.scriptHash,
              `${path}.${role}.scriptHash`,
              HEX_28,
            ),
            outRef: exactString(
              identity.outRef,
              `${path}.${role}.outRef`,
              OUT_REF,
            ),
          }),
        ];
      }),
    ),
  );
};

const parseCataloguePolicy = (
  value: unknown,
  path: string,
): WatcherFraudProofCatalogueIdentity => {
  const catalogue = exactRecord(value, path, ["root", "categories"]);
  const categories = exactRecord(
    catalogue.categories,
    `${path}.categories`,
    Object.keys(CATALOGUE_CATEGORY_TO_CONTRACT),
  );
  return Object.freeze({
    root: exactString(catalogue.root, `${path}.root`, HEX_32),
    categories: Object.freeze(
      Object.fromEntries(
        Object.keys(CATALOGUE_CATEGORY_TO_CONTRACT).map((category) => {
          const entry = exactRecord(
            categories[category],
            `${path}.categories.${category}`,
            ["categoryId", "scriptHash"],
          );
          return [
            category,
            Object.freeze({
              categoryId: exactString(
                entry.categoryId,
                `${path}.categories.${category}.categoryId`,
                /^[0-9a-f]{8}$/u,
              ),
              scriptHash: exactString(
                entry.scriptHash,
                `${path}.categories.${category}.scriptHash`,
                HEX_28,
              ),
            }),
          ];
        }),
      ),
    ) as WatcherFraudProofCatalogueIdentity["categories"],
  });
};

const parsePolicy = (value: WatcherDeploymentIdentityPolicy): ParsedPolicy => {
  const policy = exactRecord(value, "$.policy", [
    "network",
    "hubOracleOneShotOutRef",
    "appliedScriptHashes",
    "referenceScripts",
    "fraudProofCatalogue",
    "ruleBundleCommitment",
    "programCommitments",
    "daMode",
    "daIdentityDigest",

    "fundingProfileBundleDigest",
    "blueprintHash",
  ]);
  if (
    policy.network !== "Mainnet" &&
    policy.network !== "Preprod" &&
    policy.network !== "Preview" &&
    policy.network !== "Custom"
  ) {
    fail("invalid_field", "$.policy.network");
  }
  const network = policy.network as ParsedPolicy["network"];
  if (policy.daMode !== "authenticated_committee_v1") {
    fail("invalid_field", "$.policy.daMode");
  }
  return Object.freeze({
    network,
    hubOracleOneShotOutRef: exactString(
      policy.hubOracleOneShotOutRef,
      "$.policy.hubOracleOneShotOutRef",
      OUT_REF,
    ),
    appliedScriptHashes: exactDynamicHexMap(
      policy.appliedScriptHashes,
      "$.policy.appliedScriptHashes",
      DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
      HEX_28,
    ),
    referenceScripts: parseReferenceScriptPolicy(
      policy.referenceScripts,
      "$.policy.referenceScripts",
    ),
    fraudProofCatalogue: parseCataloguePolicy(
      policy.fraudProofCatalogue,
      "$.policy.fraudProofCatalogue",
    ),
    ruleBundleCommitment: exactString(
      policy.ruleBundleCommitment,
      "$.policy.ruleBundleCommitment",
      HEX_32,
    ),
    programCommitments: exactDynamicHexMap(
      policy.programCommitments,
      "$.policy.programCommitments",
      null,
      HEX_32,
    ),
    daMode: "authenticated_committee_v1",
    daIdentityDigest: exactString(
      policy.daIdentityDigest,
      "$.policy.daIdentityDigest",
      HEX_32,
    ),

    fundingProfileBundleDigest: exactString(
      policy.fundingProfileBundleDigest,
      "$.policy.fundingProfileBundleDigest",
      HEX_32,
    ),
    blueprintHash: exactString(
      policy.blueprintHash,
      "$.policy.blueprintHash",
      HEX_32,
    ),
  });
};

type ReleaseBindings = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION;
  fundingProfileBundleDigest: string;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  da: Readonly<{
    mode: "authenticated_committee_v1";
    identityDigest: string;
  }>;
  artifacts: Readonly<{
    blueprintHash: string;
  }>;
}>;

const parseReleaseBindings = (
  value: unknown,
  path: string,
): ReleaseBindings => {
  const bindings = exactRecord(value, path, [
    "schemaVersion",
    "ruleBundleCommitment",
    "programCommitments",
    "da",
    "artifacts",
    "fundingProfileBundleDigest",
  ]);
  if (
    bindings.schemaVersion !==
    WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION
  ) {
    fail("invalid_field", `${path}.schemaVersion`);
  }
  const da = exactRecord(bindings.da, `${path}.da`, ["mode", "identityDigest"]);
  if (da.mode !== "authenticated_committee_v1") {
    fail("invalid_field", `${path}.da.mode`);
  }
  const artifacts = exactRecord(bindings.artifacts, `${path}.artifacts`, [
    "blueprintHash",
  ]);
  return Object.freeze({
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    fundingProfileBundleDigest: exactString(
      bindings.fundingProfileBundleDigest,
      `${path}.fundingProfileBundleDigest`,
      HEX_32,
    ),
    ruleBundleCommitment: exactString(
      bindings.ruleBundleCommitment,
      `${path}.ruleBundleCommitment`,
      HEX_32,
    ),
    programCommitments: exactDynamicHexMap(
      bindings.programCommitments,
      `${path}.programCommitments`,
      null,
      HEX_32,
    ),
    da: Object.freeze({
      mode: "authenticated_committee_v1",
      identityDigest: exactString(
        da.identityDigest,
        `${path}.da.identityDigest`,
        HEX_32,
      ),
    }),
    artifacts: Object.freeze({
      blueprintHash: exactString(
        artifacts.blueprintHash,
        `${path}.artifacts.blueprintHash`,
        HEX_32,
      ),
    }),
  });
};

const signaturePayload = (
  manifestId: string,
  bindings: ReleaseBindings,
): Buffer => {
  const identityDigest = computeDeploymentManifestJsonDigest({
    manifestId,
    releaseBindings: bindings,
  });
  return Buffer.from(
    `${WATCHER_DEPLOYMENT_IDENTITY_SIGNATURE_DOMAIN}\0${identityDigest}`,
    "utf8",
  );
};

export const makeWatcherDeploymentIdentitySignaturePayload = (
  manifestId: string,
  releaseBindings: unknown,
): Buffer =>
  signaturePayload(
    exactString(manifestId, "$.manifestId", HEX_32),
    parseReleaseBindings(releaseBindings, "$.releaseBindings"),
  );

type ParsedTrustRoot = Readonly<{
  trustRootId: string;
  publicKeySpkiDer: Buffer;
}>;

const parseTrustRoots = (
  value: readonly WatcherDeploymentTrustRoot[],
): ReadonlyMap<string, ParsedTrustRoot> => {
  if (!Array.isArray(value) || value.length === 0 || value.length > 16) {
    fail("invalid_trust_root", "$.trustRoots");
  }
  const roots = new Map<string, ParsedTrustRoot>();
  value.forEach((entry, index) => {
    const path = `$.trustRoots[${index.toString()}]`;
    const root = exactRecord(entry, path, [
      "trustRootId",
      "publicKeySpkiDerHex",
    ]);
    const trustRootId = exactString(
      root.trustRootId,
      `${path}.trustRootId`,
      HEX_32,
    );
    if (roots.has(trustRootId)) {
      fail("duplicate_trust_root", `${path}.trustRootId`);
    }
    const publicKeySpkiDerHex = exactString(
      root.publicKeySpkiDerHex,
      `${path}.publicKeySpkiDerHex`,
      /^(?:[0-9a-f]{2}){44}$/u,
    );
    const publicKeySpkiDer = Buffer.from(publicKeySpkiDerHex, "hex");
    const derivedId = createHash("sha256")
      .update(publicKeySpkiDer)
      .digest("hex");
    if (
      !timingSafeEqual(
        Buffer.from(derivedId, "hex"),
        Buffer.from(trustRootId, "hex"),
      )
    ) {
      fail("invalid_trust_root", `${path}.trustRootId`);
    }
    try {
      const publicKey = createPublicKey({
        key: publicKeySpkiDer,
        format: "der",
        type: "spki",
      });
      if (publicKey.asymmetricKeyType !== "ed25519") {
        fail("invalid_trust_root", `${path}.publicKeySpkiDerHex`);
      }
    } catch {
      fail("invalid_trust_root", `${path}.publicKeySpkiDerHex`);
    }
    roots.set(trustRootId, Object.freeze({ trustRootId, publicKeySpkiDer }));
  });
  return roots;
};

const mapManifestContracts = (
  manifest: JsonRecord,
): Readonly<Record<string, string>> => {
  const contracts = plainRecord(manifest.contracts, "$.manifest.contracts");
  const hashes: Record<string, string> = {};
  for (const contractName of DEPLOYMENT_MANIFEST_CONTRACT_NAMES) {
    const entry = plainRecord(
      contracts[contractName],
      `$.manifest.contracts.${contractName}`,
    );
    hashes[contractName] = exactString(
      entry.scriptHash,
      `$.manifest.contracts.${contractName}.scriptHash`,
      HEX_28,
    );
  }
  return hashes;
};

const assertReferenceScripts = (
  manifest: JsonRecord,
  expected: Readonly<Record<string, WatcherReferenceScriptIdentity>>,
): void => {
  const scripts = plainRecord(
    manifest.referenceScripts,
    "$.manifest.referenceScripts",
  );
  for (const role of REFERENCE_SCRIPT_ROLES) {
    const entry = plainRecord(
      scripts[role],
      `$.manifest.referenceScripts.${role}`,
    );
    if (
      entry.scriptHash !== expected[role]?.scriptHash ||
      entry.outRef !== expected[role]?.outRef ||
      entry.status !== "confirmed"
    ) {
      fail("mismatched_identity", `$.manifest.referenceScripts.${role}`);
    }
  }
};

const assertCatalogue = (
  manifest: JsonRecord,
  expected: WatcherFraudProofCatalogueIdentity,
): void => {
  const contracts = plainRecord(manifest.contracts, "$.manifest.contracts");
  const catalogueContract = plainRecord(
    contracts.fraudProofCatalogueMint,
    "$.manifest.contracts.fraudProofCatalogueMint",
  );
  const catalogue = plainRecord(
    catalogueContract.fraudProofCatalogue,
    "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue",
  );
  if (catalogue.root !== expected.root) {
    fail(
      "mismatched_identity",
      "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.root",
    );
  }
  const categories = plainRecord(
    catalogue.categories,
    "$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories",
  );
  for (const category of Object.keys(CATALOGUE_CATEGORY_TO_CONTRACT) as Array<
    keyof typeof CATALOGUE_CATEGORY_TO_CONTRACT
  >) {
    const entry = plainRecord(
      categories[category],
      `$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${category}`,
    );
    const expectedEntry = expected.categories[category];
    const contractName = CATALOGUE_CATEGORY_TO_CONTRACT[category];
    const deployedContract = plainRecord(
      contracts[contractName],
      `$.manifest.contracts.${contractName}`,
    );
    const deployedScriptHash = exactString(
      deployedContract.scriptHash,
      `$.manifest.contracts.${contractName}.scriptHash`,
      HEX_28,
    );
    if (
      entry.categoryId !== expectedEntry.categoryId ||
      entry.scriptHash !== expectedEntry.scriptHash ||
      entry.scriptHash !== deployedScriptHash
    ) {
      fail(
        "mismatched_identity",
        `$.manifest.contracts.fraudProofCatalogueMint.fraudProofCatalogue.categories.${category}`,
      );
    }
  }
};

const assertPolicyBindings = (
  manifest: JsonRecord,
  bindings: ReleaseBindings,
  policy: ParsedPolicy,
): void => {
  if (manifest.network !== policy.network) {
    fail("mismatched_identity", "$.manifest.network");
  }
  if (manifest.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_DIGEST) {
    fail("mismatched_identity", "$.manifest.consensusProfileDigest");
  }
  const oneShot = plainRecord(
    manifest.hubOracleOneShot,
    "$.manifest.hubOracleOneShot",
  );
  if (
    oneShot.outRef !== policy.hubOracleOneShotOutRef ||
    oneShot.status !== "consumed_by_init"
  ) {
    fail("mismatched_identity", "$.manifest.hubOracleOneShot");
  }
  const actualScriptHashes = mapManifestContracts(manifest);
  if (!equalStringMaps(actualScriptHashes, policy.appliedScriptHashes)) {
    fail("mismatched_identity", "$.manifest.contracts");
  }
  assertReferenceScripts(manifest, policy.referenceScripts);
  assertCatalogue(manifest, policy.fraudProofCatalogue);

  if (
    bindings.ruleBundleCommitment !== policy.ruleBundleCommitment ||
    !equalStringMaps(bindings.programCommitments, policy.programCommitments)
  ) {
    fail("mismatched_identity", "$.releaseBindings");
  }
  if (
    bindings.da.mode !== policy.daMode ||
    bindings.da.identityDigest !== policy.daIdentityDigest ||
    computeDeploymentManifestJsonDigest(manifest.da) !== policy.daIdentityDigest
  ) {
    fail("mismatched_identity", "$.releaseBindings.da");
  }
  if (
    bindings.fundingProfileBundleDigest !== policy.fundingProfileBundleDigest
  ) {
    fail("mismatched_identity", "$.releaseBindings.fundingProfileBundleDigest");
  }
  const artifacts = plainRecord(manifest.artifacts, "$.manifest.artifacts");
  if (
    bindings.artifacts.blueprintHash !== policy.blueprintHash ||
    artifacts.blueprintHash !== policy.blueprintHash
  ) {
    fail("mismatched_identity", "$.releaseBindings.artifacts");
  }
};

const verifyCanonicalManifest = (value: unknown): JsonRecord => {
  const candidate = (() => {
    try {
      return verifyDeploymentManifestIdentity(value);
    } catch {
      return fail("canonical_manifest_invalid", "$.manifest");
    }
  })();

  // Validate the exact signed manifest, including contract bytes, parameters,
  // references, and blueprint identity. No release evidence is required.
  try {
    verifyFinalizedDeploymentManifest(candidate);
  } catch {
    fail("canonical_manifest_invalid", "$.manifest");
  }
  return candidate;
};

export type VerifiedWatcherDeploymentIdentity = Readonly<{
  manifestId: string;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  trustRootId: string;
  blueprintHash: string;
  fundingProfileBundleDigest: string;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  durableMarker: DeploymentMarker;
}>;

export type WatcherDeploymentProtocolScriptAuthority = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION;
  deploymentFingerprint: string;
  network: "Mainnet" | "Preprod" | "Preview" | "Custom";
  hubOracleOneShotOutRef: string;
  protocolScriptHashes: Readonly<{
    hubOracleMint: string;
    stateQueueSpend: string;
    stateQueueMint: string;
    correctionLockSpend: string;
    fraudProofSpend: string;
    fraudProofMint: string;
    referenceScriptAuthMint: string;
    availabilityChallengeSpend: string;
    availabilityChallengeMint: string;
    availabilityChallengeBondWithdraw: string;
    availabilityChallengeOpenWithdraw: string;
    availabilityChallengeSettleWithdraw: string;
    availabilityChallengeCloseWithdraw: string;
    availabilityChallengeTimeoutWithdraw: string;
  }>;
  referenceScripts: Readonly<Record<string, WatcherReferenceScriptIdentity>>;
  authorityDigest: string;
}>;

export type WatcherDeploymentProtocolParameterAuthority = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION;
  deploymentFingerprint: string;
  snapshot: DeploymentManifestCardanoProtocolParameters;
  snapshotDigest: string;
  authorityDigest: string;
}>;

export type WatcherDeploymentAvailabilityChallengeAuthority = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION;
  deploymentFingerprint: string;
  parameters: DeploymentManifestAvailabilityChallenge;
  parametersDigest: string;
  authorityDigest: string;
}>;

const authenticatedWatcherDeploymentIdentities = new WeakSet<object>();
const authenticatedWatcherProtocolScriptAuthorities = new WeakSet<object>();
const authenticatedWatcherProtocolParameterAuthorities = new WeakSet<object>();
const authenticatedWatcherReleaseFinalityAuthorities = new WeakSet<object>();
const authenticatedWatcherReleaseEconomicsAuthorities = new WeakSet<object>();
const authenticatedWatcherAvailabilityChallengeAuthorities =
  new WeakSet<object>();
const protocolScriptAuthorityByDeploymentIdentity = new WeakMap<
  object,
  WatcherDeploymentProtocolScriptAuthority
>();
const appliedScriptHashesByDeploymentIdentity = new WeakMap<
  object,
  Readonly<Record<string, string>>
>();
const releaseFinalityAuthorityByDeploymentIdentity = new WeakMap<
  object,
  FraudProofReleaseFinalityAuthority
>();
const releaseEconomicsAuthorityByDeploymentIdentity = new WeakMap<
  object,
  FraudProofReleaseEconomicsAuthority
>();
const protocolParameterAuthorityByDeploymentIdentity = new WeakMap<
  object,
  WatcherDeploymentProtocolParameterAuthority
>();
const availabilityChallengeAuthorityByDeploymentIdentity = new WeakMap<
  object,
  WatcherDeploymentAvailabilityChallengeAuthority
>();

const USER_EVENT_SIGNED_CONTRACT_NAMES = [
  "hubOracleMint",
  "depositMint",
  "depositSpend",
  "withdrawalMint",
  "withdrawalSpend",
  "txOrderMint",
  "txOrderSpend",
  "fieldPreimageCertificateMint",
] as const;
type UserEventSignedContractName =
  (typeof USER_EVENT_SIGNED_CONTRACT_NAMES)[number];
type SignedUserEventScript = Readonly<{
  type: string;
  cborHex: string;
  scriptHash: string;
}>;
const userEventScriptsByDeploymentIdentity = new WeakMap<
  object,
  Readonly<{
    blueprintSha256: string;
    contracts: Readonly<
      Record<UserEventSignedContractName, SignedUserEventScript>
    >;
  }>
>();

declare const watcherUserEventScriptBindingBrand: unique symbol;
/** Live script-application authority only; this carries no activation or history proof. */
export type WatcherUserEventScriptBinding = Readonly<{
  [watcherUserEventScriptBindingBrand]: true;
}>;
type UserEventScriptIdentity = Readonly<{
  policyId: string;
  spendScriptHash: string;
  addressHex: string;
}>;
type WatcherUserEventScriptBindingDescription = Readonly<{
  deploymentFingerprint: string;
  blueprintHash: string;
  blueprintSha256: string;
  network: VerifiedWatcherDeploymentIdentity["network"];
  canonicalOneShotOutRef: string;
  hub: Readonly<{ policyId: string; assetName: string; addressHex: string }>;
  deposit: UserEventScriptIdentity;
  withdrawal: UserEventScriptIdentity;
  forcedOrder: UserEventScriptIdentity;
  certificatePolicyId: string;
}>;
const userEventScriptBindings = new WeakMap<
  object,
  Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    description: WatcherUserEventScriptBindingDescription;
  }>
>();

const typedArrayByteLength = Object.getOwnPropertyDescriptor(
  Object.getPrototypeOf(Uint8Array.prototype),
  "byteLength",
)!.get!;

/** Admit the exact signed blueprint bytes and independently derive every event script. */
export const verifyWatcherUserEventScriptBinding = (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly blueprintBytes: Uint8Array;
}): WatcherUserEventScriptBinding => {
  const { deploymentIdentity, blueprintBytes } = input;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const signed =
    userEventScriptsByDeploymentIdentity.get(deploymentIdentity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.userEventScripts");
  const byteLength = (() => {
    try {
      return typedArrayByteLength.call(blueprintBytes) as number;
    } catch {
      return fail("invalid_field", "$.blueprintBytes");
    }
  })();
  if (
    !(blueprintBytes instanceof Uint8Array) ||
    byteLength < 1 ||
    byteLength > 64 * 1024 * 1024
  ) {
    fail("invalid_field", "$.blueprintBytes");
  }
  const bytes = new Uint8Array(blueprintBytes);
  if (
    createHash("sha256").update(bytes).digest("hex") !== signed.blueprintSha256
  ) {
    fail("mismatched_identity", "$.blueprintBytes.sha256");
  }
  const blueprint = (() => {
    try {
      const raw = plainRecord(
        parseWatcherStrictJsonValue(
          new TextDecoder("utf-8", { fatal: true }).decode(bytes),
        ),
        "$.blueprint",
      );
      if (
        !Array.isArray(raw.validators) ||
        raw.validators.length < 1 ||
        raw.validators.length > 4096
      ) {
        fail("invalid_field", "$.blueprint.validators");
      }
      return parseFaultProofBlueprint(raw);
    } catch {
      return fail("invalid_field", "$.blueprint");
    }
  })();
  const protocol = watcherDeploymentProtocolScriptAuthority(deploymentIdentity);
  const derived = (() => {
    try {
      const hub = buildHubOracleMintingValidator({
        blueprint,
        oneShotOutRef: parseOutRefLabel(protocol.hubOracleOneShotOutRef),
      });
      const parameters = {
        blueprint,
        network: deploymentIdentity.network,
        hubOraclePolicyId: hub.policyId,
      };
      return {
        hub,
        deposit: buildDepositValidators(parameters),
        withdrawal: buildWithdrawalValidators(parameters),
        ...buildTxOrderValidators(parameters),
      };
    } catch {
      return fail("invalid_field", "$.blueprint.userEventScripts");
    }
  })();
  const actual: Record<UserEventSignedContractName, SignedUserEventScript> = {
    hubOracleMint: {
      type: derived.hub.mintingScript.type,
      cborHex: derived.hub.mintingScriptCBOR,
      scriptHash: derived.hub.policyId,
    },
    depositMint: {
      type: derived.deposit.mintingScript.type,
      cborHex: derived.deposit.mintingScriptCBOR,
      scriptHash: derived.deposit.policyId,
    },
    depositSpend: {
      type: derived.deposit.spendingScript.type,
      cborHex: derived.deposit.spendingScriptCBOR,
      scriptHash: derived.deposit.spendingScriptHash,
    },
    withdrawalMint: {
      type: derived.withdrawal.mintingScript.type,
      cborHex: derived.withdrawal.mintingScriptCBOR,
      scriptHash: derived.withdrawal.policyId,
    },
    withdrawalSpend: {
      type: derived.withdrawal.spendingScript.type,
      cborHex: derived.withdrawal.spendingScriptCBOR,
      scriptHash: derived.withdrawal.spendingScriptHash,
    },
    txOrderMint: {
      type: derived.txOrder.mintingScript.type,
      cborHex: derived.txOrder.mintingScriptCBOR,
      scriptHash: derived.txOrder.policyId,
    },
    txOrderSpend: {
      type: derived.txOrder.spendingScript.type,
      cborHex: derived.txOrder.spendingScriptCBOR,
      scriptHash: derived.txOrder.spendingScriptHash,
    },
    fieldPreimageCertificateMint: {
      type: derived.fieldPreimageCertificate.mintingScript.type,
      cborHex: derived.fieldPreimageCertificate.mintingScriptCBOR,
      scriptHash: derived.fieldPreimageCertificate.policyId,
    },
  };
  for (const name of USER_EVENT_SIGNED_CONTRACT_NAMES) {
    const expected = signed.contracts[name];
    const script = actual[name];
    if (
      expected.type !== "PlutusV3" ||
      script.type !== expected.type ||
      script.cborHex !== expected.cborHex ||
      script.scriptHash !== expected.scriptHash
    ) {
      fail("mismatched_identity", `$.manifest.contracts.${name}`);
    }
  }
  const eventIdentity = (
    validator: AuthenticatedValidator,
  ): UserEventScriptIdentity =>
    Object.freeze({
      policyId: validator.policyId,
      spendScriptHash: validator.spendingScriptHash,
      addressHex: CML.Address.from_bech32(
        validator.spendingScriptAddress,
      ).to_hex(),
    });
  const description: WatcherUserEventScriptBindingDescription = Object.freeze({
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: deploymentIdentity.blueprintHash,
    blueprintSha256: signed.blueprintSha256,
    network: deploymentIdentity.network,
    canonicalOneShotOutRef: protocol.hubOracleOneShotOutRef,
    hub: Object.freeze({
      policyId: derived.hub.policyId,
      assetName: HUB_ORACLE_ASSET_NAME,
      addressHex: CML.Address.from_bech32(
        credentialToAddress(
          deploymentIdentity.network,
          scriptHashToCredential(derived.hub.policyId),
        ),
      ).to_hex(),
    }),
    deposit: eventIdentity(derived.deposit),
    withdrawal: eventIdentity(derived.withdrawal),
    forcedOrder: eventIdentity(derived.txOrder),
    certificatePolicyId: derived.fieldPreimageCertificate.policyId,
  });
  const binding = Object.freeze({}) as WatcherUserEventScriptBinding;
  userEventScriptBindings.set(
    binding,
    Object.freeze({ deploymentIdentity: deploymentIdentity, description }),
  );
  return binding;
};

/** The returned frozen description cannot be substituted for the admitted handle. */
export const readWatcherUserEventScriptBinding = (input: {
  readonly binding: WatcherUserEventScriptBinding;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
}): WatcherUserEventScriptBindingDescription => {
  const { binding, deploymentIdentity } = input;
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const admitted = userEventScriptBindings.get(binding);
  if (
    admitted === undefined ||
    admitted.deploymentIdentity !== deploymentIdentity
  ) {
    return fail("invalid_field", "$.userEventScriptBinding");
  }
  return admitted.description;
};

/**
 * Refuses structural casts at production authority boundaries. Only the
 * signature/policy verifier in this module can admit an identity object.
 */
export const assertVerifiedWatcherDeploymentIdentity = (
  identity: VerifiedWatcherDeploymentIdentity,
): void => {
  if (!authenticatedWatcherDeploymentIdentities.has(identity)) {
    fail("invalid_field", "$.verifiedDeploymentIdentity");
  }
};

/**
 * Refuses a structural policy/hash bundle. Only the signed deployment verifier
 * can mint the script authority consumed by production state-queue sources.
 */
export const assertWatcherDeploymentProtocolScriptAuthority = (
  authority: WatcherDeploymentProtocolScriptAuthority,
): void => {
  if (!authenticatedWatcherProtocolScriptAuthorities.has(authority)) {
    fail("invalid_field", "$.protocolScriptAuthority");
  }
};

export const watcherDeploymentProtocolScriptAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): WatcherDeploymentProtocolScriptAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    protocolScriptAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.protocolScripts")
  );
};

/** Exact applied scripts from the already verified deployment manifest. */
export const watcherDeploymentAppliedScriptHashes = (
  identity: VerifiedWatcherDeploymentIdentity,
): Readonly<Record<string, string>> => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    appliedScriptHashesByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.appliedScriptHashes")
  );
};

export const assertWatcherDeploymentProtocolParameterAuthority = (
  authority: WatcherDeploymentProtocolParameterAuthority,
): void => {
  if (!authenticatedWatcherProtocolParameterAuthorities.has(authority)) {
    fail("invalid_field", "$.protocolParameterAuthority");
  }
};

export const watcherDeploymentProtocolParameterAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): WatcherDeploymentProtocolParameterAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    protocolParameterAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.protocolParameters")
  );
};

export const assertWatcherDeploymentAvailabilityChallengeAuthority = (
  authority: WatcherDeploymentAvailabilityChallengeAuthority,
): void => {
  if (!authenticatedWatcherAvailabilityChallengeAuthorities.has(authority)) {
    fail("invalid_field", "$.availabilityChallengeAuthority");
  }
};

/**
 * Returns the exact Q58 geometry, bonds, owner, response classes and every
 * lifecycle fee ceiling admitted by the signed deployment manifest. A plain
 * object with the same fields is not production authority.
 */
export const watcherDeploymentAvailabilityChallengeAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): WatcherDeploymentAvailabilityChallengeAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    availabilityChallengeAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.availabilityChallenge")
  );
};

/**
 * Returns the release-finality authority minted by the signed deployment
 * verifier. The authority method authenticates its receiver, so spreading or
 * structurally copying the object cannot preserve workflow authority.
 */
export const watcherDeploymentReleaseFinalityAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): FraudProofReleaseFinalityAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    releaseFinalityAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.releaseFinality")
  );
};

/**
 * Returns the exact release-economics authority minted by signed deployment
 * verification. Structural copies cannot select a different collateral floor
 * or other F04 amount after launch.
 */
export const watcherDeploymentReleaseEconomicsAuthority = (
  identity: VerifiedWatcherDeploymentIdentity,
): FraudProofReleaseEconomicsAuthority => {
  assertVerifiedWatcherDeploymentIdentity(identity);
  return (
    releaseEconomicsAuthorityByDeploymentIdentity.get(identity) ??
    fail("invalid_field", "$.verifiedDeploymentIdentity.releaseEconomics")
  );
};

export const verifyWatcherDeploymentIdentity = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicy;
  readonly trustRoots: readonly WatcherDeploymentTrustRoot[];
  readonly durableMarker: unknown;
}): VerifiedWatcherDeploymentIdentity => {
  const policy = parsePolicy(input.policy);
  const trustRoots = parseTrustRoots(input.trustRoots);
  const envelope = exactRecord(input.signedIdentity, "$", [
    "schemaVersion",
    "manifest",
    "releaseBindings",
    "attestation",
  ]);
  if (
    envelope.schemaVersion !== WATCHER_SIGNED_DEPLOYMENT_IDENTITY_SCHEMA_VERSION
  ) {
    fail("invalid_field", "$.schemaVersion");
  }
  const manifest = verifyCanonicalManifest(envelope.manifest);
  const manifestId = exactString(
    manifest.manifestId,
    "$.manifest.manifestId",
    HEX_32,
  );
  const bindings = parseReleaseBindings(
    envelope.releaseBindings,
    "$.releaseBindings",
  );
  const attestation = exactRecord(envelope.attestation, "$.attestation", [
    "algorithm",
    "trustRootId",
    "signature",
  ]);
  if (attestation.algorithm !== "ed25519") {
    fail("invalid_field", "$.attestation.algorithm");
  }
  const trustRootId = exactString(
    attestation.trustRootId,
    "$.attestation.trustRootId",
    HEX_32,
  );
  const trustRoot =
    trustRoots.get(trustRootId) ??
    fail("untrusted_signer", "$.attestation.trustRootId");
  const signature = Buffer.from(
    exactString(attestation.signature, "$.attestation.signature", HEX_64),
    "hex",
  );
  let signatureValid = false;
  try {
    signatureValid = verifySignature(
      null,
      signaturePayload(manifestId, bindings),
      createPublicKey({
        key: trustRoot.publicKeySpkiDer,
        format: "der",
        type: "spki",
      }),
      signature,
    );
  } catch {
    fail("invalid_signature", "$.attestation.signature");
  }
  if (!signatureValid) {
    fail("invalid_signature", "$.attestation.signature");
  }
  assertPolicyBindings(manifest, bindings, policy);

  const expectedMarker = makeDeploymentMarker(manifestId);
  if (input.durableMarker === null || input.durableMarker === undefined) {
    fail("missing_durable_marker", "$.durableMarker");
  }
  try {
    assertDeploymentMarkerMatches(
      expectedMarker,
      input.durableMarker,
      "watcher durable store",
    );
  } catch {
    fail("durable_marker_mismatch", "$.durableMarker");
  }

  const verified = Object.freeze({
    manifestId,
    network: policy.network,
    trustRootId,
    blueprintHash: policy.blueprintHash,
    fundingProfileBundleDigest: policy.fundingProfileBundleDigest,
    ruleBundleCommitment: policy.ruleBundleCommitment,
    programCommitments: policy.programCommitments,
    durableMarker: expectedMarker,
  });
  const protocolScriptHashes = Object.freeze({
    hubOracleMint: policy.appliedScriptHashes.hubOracleMint!,
    stateQueueSpend: policy.appliedScriptHashes.stateQueueSpend!,
    stateQueueMint: policy.appliedScriptHashes.stateQueueMint!,
    correctionLockSpend: policy.appliedScriptHashes.correctionLockSpend!,
    fraudProofSpend: policy.appliedScriptHashes.fraudProofSpend!,
    fraudProofMint: policy.appliedScriptHashes.fraudProofMint!,
    referenceScriptAuthMint:
      policy.appliedScriptHashes.referenceScriptAuthMint!,
    availabilityChallengeSpend:
      policy.appliedScriptHashes.availabilityChallengeSpend!,
    availabilityChallengeMint:
      policy.appliedScriptHashes.availabilityChallengeMint!,
    availabilityChallengeBondWithdraw:
      policy.appliedScriptHashes.availabilityChallengeBondWithdraw!,
    availabilityChallengeOpenWithdraw:
      policy.appliedScriptHashes.availabilityChallengeOpenWithdraw!,
    availabilityChallengeSettleWithdraw:
      policy.appliedScriptHashes.availabilityChallengeSettleWithdraw!,
    availabilityChallengeCloseWithdraw:
      policy.appliedScriptHashes.availabilityChallengeCloseWithdraw!,
    availabilityChallengeTimeoutWithdraw:
      policy.appliedScriptHashes.availabilityChallengeTimeoutWithdraw!,
  });
  appliedScriptHashesByDeploymentIdentity.set(
    verified,
    Object.freeze({ ...policy.appliedScriptHashes }),
  );
  if (Object.values(protocolScriptHashes).some((hash) => !HEX_28.test(hash))) {
    fail("mismatched_identity", "$.policy.appliedScriptHashes");
  }
  const referenceScripts = Object.freeze(
    Object.fromEntries(
      Object.entries(policy.referenceScripts)
        .sort(([left], [right]) => left.localeCompare(right))
        .map(([role, reference]) => [role, Object.freeze({ ...reference })]),
    ),
  );
  const authorityInput = Object.freeze({
    schemaVersion: WATCHER_DEPLOYMENT_PROTOCOL_SCRIPT_AUTHORITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    network: policy.network,
    hubOracleOneShotOutRef: policy.hubOracleOneShotOutRef,
    protocolScriptHashes,
    referenceScripts,
  });
  const protocolScriptAuthority = Object.freeze({
    ...authorityInput,
    authorityDigest: computeDeploymentManifestJsonDigest(authorityInput),
  });
  const manifestProtocolParameters = exactRecord(
    manifest.cardanoProtocolParameters,
    "$.manifest.cardanoProtocolParameters",
    ["snapshot", "digest"],
  );
  const protocolParameterSnapshot =
    parseDeploymentManifestCardanoProtocolParameters(
      manifestProtocolParameters.snapshot,
    );
  const protocolParameterSnapshotDigest = exactString(
    manifestProtocolParameters.digest,
    "$.manifest.cardanoProtocolParameters.digest",
    HEX_32,
  );
  if (
    computeDeploymentManifestJsonDigest(protocolParameterSnapshot) !==
    protocolParameterSnapshotDigest
  ) {
    fail("mismatched_identity", "$.manifest.cardanoProtocolParameters.digest");
  }
  const protocolParameterAuthorityInput = Object.freeze({
    schemaVersion:
      WATCHER_DEPLOYMENT_PROTOCOL_PARAMETER_AUTHORITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    snapshot: protocolParameterSnapshot,
    snapshotDigest: protocolParameterSnapshotDigest,
  });
  const protocolParameterAuthority = Object.freeze({
    ...protocolParameterAuthorityInput,
    authorityDigest: computeDeploymentManifestJsonDigest(
      protocolParameterAuthorityInput,
    ),
  });
  const availabilityChallengeParameters =
    parseDeploymentManifestAvailabilityChallenge(
      manifest.availabilityChallenge,
    );
  const availabilityChallengeAuthorityInput = Object.freeze({
    schemaVersion:
      WATCHER_DEPLOYMENT_AVAILABILITY_CHALLENGE_AUTHORITY_SCHEMA_VERSION,
    deploymentFingerprint: manifestId,
    parameters: availabilityChallengeParameters,
    parametersDigest: computeDeploymentManifestJsonDigest(
      availabilityChallengeParameters,
    ),
  });
  const availabilityChallengeAuthority = Object.freeze({
    ...availabilityChallengeAuthorityInput,
    authorityDigest: computeDeploymentManifestJsonDigest(
      availabilityChallengeAuthorityInput,
    ),
  });
  const manifestL1Finality = exactRecord(
    manifest.l1Finality,
    "$.manifest.l1Finality",
    ["confirmationDepth", "automaticRecoveryMaxDepth", "deepRollbackPolicy"],
  );
  const releaseFinalityPolicy = Object.freeze({
    confirmationDepth: manifestL1Finality.confirmationDepth,
    automaticRecoveryMaxDepth: manifestL1Finality.automaticRecoveryMaxDepth,
    deepRollbackPolicy: manifestL1Finality.deepRollbackPolicy,
  }) as ReleaseL1FinalityPolicy;
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifestId,
    blueprintHash: exactString(
      plainRecord(manifest.artifacts, "$.manifest.artifacts").blueprintHash,
      "$.manifest.artifacts.blueprintHash",
      HEX_32,
    ),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(
      releaseFinalityPolicy,
    ),
    policy: releaseFinalityPolicy,
  });
  const releaseFinalityAuthority: FraudProofReleaseFinalityAuthority =
    Object.freeze({
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      async verifyForWorkflow(
        this: FraudProofReleaseFinalityAuthority,
        input: { readonly deploymentFingerprint: string },
      ): Promise<VerifiedFraudProofReleaseFinalityPolicy> {
        if (!authenticatedWatcherReleaseFinalityAuthorities.has(this)) {
          fail("invalid_field", "$.releaseFinalityAuthority");
        }
        if (input.deploymentFingerprint !== manifestId) {
          fail(
            "mismatched_identity",
            "$.releaseFinalityAuthority.deploymentFingerprint",
          );
        }
        return releaseFinality;
      },
    });
  const manifestEconomics = parseDeploymentManifestEconomics(
    manifest.economics,
  );
  const releaseEconomicsPolicy = Object.freeze({
    profile: manifestEconomics.profile,
    requiredBondLovelace: manifestEconomics.requiredBondLovelace.toString(),
    slashingPenaltyLovelace:
      manifestEconomics.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace:
      manifestEconomics.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      manifestEconomics.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      manifestEconomics.proverCollateralFloorLovelace.toString(),
  }) satisfies ReleaseFraudProofEconomicsPolicy;
  const releaseEconomics = validateVerifiedFraudProofReleaseEconomicsPolicy({
    schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: manifestId,
    blueprintHash: exactString(
      plainRecord(manifest.artifacts, "$.manifest.artifacts").blueprintHash,
      "$.manifest.artifacts.blueprintHash",
      HEX_32,
    ),
    policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(
      releaseEconomicsPolicy,
    ),
    policy: releaseEconomicsPolicy,
  });
  const releaseEconomicsAuthority: FraudProofReleaseEconomicsAuthority =
    Object.freeze({
      authorityVersion: FRAUD_PROOF_RELEASE_ECONOMICS_AUTHORITY,
      async verifyForWorkflow(
        this: FraudProofReleaseEconomicsAuthority,
        input: { readonly deploymentFingerprint: string },
      ): Promise<VerifiedFraudProofReleaseEconomicsPolicy> {
        if (!authenticatedWatcherReleaseEconomicsAuthorities.has(this)) {
          fail("invalid_field", "$.releaseEconomicsAuthority");
        }
        if (input.deploymentFingerprint !== manifestId) {
          fail(
            "mismatched_identity",
            "$.releaseEconomicsAuthority.deploymentFingerprint",
          );
        }
        return releaseEconomics;
      },
    });
  const signedContracts = plainRecord(
    manifest.contracts,
    "$.manifest.contracts",
  );
  const signedUserEventContracts = Object.freeze(
    Object.fromEntries(
      USER_EVENT_SIGNED_CONTRACT_NAMES.map((name) => {
        const entry = plainRecord(
          signedContracts[name],
          `$.manifest.contracts.${name}`,
        );
        const contract = plainRecord(
          entry.contract,
          `$.manifest.contracts.${name}.contract`,
        );
        return [
          name,
          Object.freeze({
            type: exactString(
              contract.type,
              `$.manifest.contracts.${name}.contract.type`,
              /^.+$/u,
            ),
            cborHex: exactString(
              contract.cborHex,
              `$.manifest.contracts.${name}.contract.cborHex`,
              /^(?:[0-9a-f]{2})+$/u,
            ),
            scriptHash: exactString(
              entry.scriptHash,
              `$.manifest.contracts.${name}.scriptHash`,
              HEX_28,
            ),
          }),
        ];
      }),
    ),
  ) as Readonly<Record<UserEventSignedContractName, SignedUserEventScript>>;
  userEventScriptsByDeploymentIdentity.set(
    verified,
    Object.freeze({
      blueprintSha256: policy.blueprintHash,
      contracts: signedUserEventContracts,
    }),
  );
  authenticatedWatcherDeploymentIdentities.add(verified);
  authenticatedWatcherProtocolScriptAuthorities.add(protocolScriptAuthority);
  authenticatedWatcherProtocolParameterAuthorities.add(
    protocolParameterAuthority,
  );
  authenticatedWatcherReleaseFinalityAuthorities.add(releaseFinalityAuthority);
  authenticatedWatcherReleaseEconomicsAuthorities.add(
    releaseEconomicsAuthority,
  );
  authenticatedWatcherAvailabilityChallengeAuthorities.add(
    availabilityChallengeAuthority,
  );
  protocolScriptAuthorityByDeploymentIdentity.set(
    verified,
    protocolScriptAuthority,
  );
  protocolParameterAuthorityByDeploymentIdentity.set(
    verified,
    protocolParameterAuthority,
  );
  releaseFinalityAuthorityByDeploymentIdentity.set(
    verified,
    releaseFinalityAuthority,
  );
  releaseEconomicsAuthorityByDeploymentIdentity.set(
    verified,
    releaseEconomicsAuthority,
  );
  availabilityChallengeAuthorityByDeploymentIdentity.set(
    verified,
    availabilityChallengeAuthority,
  );
  return verified;
};
