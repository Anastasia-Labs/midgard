import {
  createHash,
  createPublicKey,
  timingSafeEqual,
  verify as verifySignature,
} from "node:crypto";

import {
  MIDGARD_CONSENSUS_PROFILE_V1_DIGEST,
  MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
} from "../../midgard-core/src/consensus-profile-v1.js";
import {
  assertDeploymentMarkerV1Matches,
  computeDeploymentManifestV1Id,
  computeDeploymentManifestV1JsonDigest,
  DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentMarkerV1,
  makeDeploymentMarkerV1,
  verifyDeploymentManifestV1Identity,
  verifyFinalizedDeploymentManifestV1,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";

export const WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION =
  "midgard-watcher-signed-deployment-identity-v1" as const;
export const WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION =
  "midgard-watcher-deployment-release-bindings-v1" as const;
export const WATCHER_DEPLOYMENT_IDENTITY_SIGNATURE_DOMAIN =
  "midgard-watcher-deployment-identity-signature-v1" as const;

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
} as const);

const REFERENCE_SCRIPT_ROLES = Object.freeze(
  Object.keys(DEPLOYMENT_MANIFEST_V1_REFERENCE_SCRIPT_CONTRACT_BY_ROLE),
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
  | "release_evidence_unavailable"
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

export type WatcherDeploymentTrustRootV1 = Readonly<{
  trustRootId: string;
  publicKeySpkiDerHex: string;
}>;

export type WatcherReferenceScriptIdentityV1 = Readonly<{
  scriptHash: string;
  outRef: string;
}>;

export type WatcherFraudProofCatalogueIdentityV1 = Readonly<{
  root: string;
  categories: Readonly<
    Record<
      keyof typeof CATALOGUE_CATEGORY_TO_CONTRACT,
      Readonly<{ categoryId: string; scriptHash: string }>
    >
  >;
}>;

export type WatcherDeploymentIdentityPolicyV1 = Readonly<{
  network: "Mainnet" | "Preprod" | "Preview";
  hubOracleOneShotOutRef: string;
  appliedScriptHashes: Readonly<Record<string, string>>;
  referenceScripts: Readonly<Record<string, WatcherReferenceScriptIdentityV1>>;
  fraudProofCatalogue: WatcherFraudProofCatalogueIdentityV1;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  daMode: "authenticated_committee_v1";
  daIdentityDigest: string;
  releaseEvidenceDigest: string;
  blueprintHash: string;
}>;

type ParsedPolicy = WatcherDeploymentIdentityPolicyV1;

const parseReferenceScriptPolicy = (
  value: unknown,
  path: string,
): Readonly<Record<string, WatcherReferenceScriptIdentityV1>> => {
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
): WatcherFraudProofCatalogueIdentityV1 => {
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
    ) as WatcherFraudProofCatalogueIdentityV1["categories"],
  });
};

const parsePolicy = (
  value: WatcherDeploymentIdentityPolicyV1,
): ParsedPolicy => {
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
    "releaseEvidenceDigest",
    "blueprintHash",
  ]);
  if (
    policy.network !== "Mainnet" &&
    policy.network !== "Preprod" &&
    policy.network !== "Preview"
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
      DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES,
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
    releaseEvidenceDigest: exactString(
      policy.releaseEvidenceDigest,
      "$.policy.releaseEvidenceDigest",
      HEX_32,
    ),
    blueprintHash: exactString(
      policy.blueprintHash,
      "$.policy.blueprintHash",
      HEX_32,
    ),
  });
};

type ReleaseBindingsV1 = Readonly<{
  schemaVersion: typeof WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  da: Readonly<{
    mode: "authenticated_committee_v1";
    identityDigest: string;
  }>;
  releaseEvidence: Readonly<{
    digest: string;
    blueprintHash: string;
  }>;
}>;

const parseReleaseBindings = (
  value: unknown,
  path: string,
): ReleaseBindingsV1 => {
  const bindings = exactRecord(value, path, [
    "schemaVersion",
    "ruleBundleCommitment",
    "programCommitments",
    "da",
    "releaseEvidence",
  ]);
  if (
    bindings.schemaVersion !==
    WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION
  ) {
    fail("invalid_field", `${path}.schemaVersion`);
  }
  const da = exactRecord(bindings.da, `${path}.da`, ["mode", "identityDigest"]);
  if (da.mode !== "authenticated_committee_v1") {
    fail("invalid_field", `${path}.da.mode`);
  }
  const releaseEvidence = exactRecord(
    bindings.releaseEvidence,
    `${path}.releaseEvidence`,
    ["digest", "blueprintHash"],
  );
  return Object.freeze({
    schemaVersion: WATCHER_DEPLOYMENT_RELEASE_BINDINGS_V1_SCHEMA_VERSION,
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
    releaseEvidence: Object.freeze({
      digest: exactString(
        releaseEvidence.digest,
        `${path}.releaseEvidence.digest`,
        HEX_32,
      ),
      blueprintHash: exactString(
        releaseEvidence.blueprintHash,
        `${path}.releaseEvidence.blueprintHash`,
        HEX_32,
      ),
    }),
  });
};

const signaturePayload = (
  manifestId: string,
  bindings: ReleaseBindingsV1,
): Buffer => {
  const identityDigest = computeDeploymentManifestV1JsonDigest({
    manifestId,
    releaseBindings: bindings,
  });
  return Buffer.from(
    `${WATCHER_DEPLOYMENT_IDENTITY_SIGNATURE_DOMAIN}\0${identityDigest}`,
    "utf8",
  );
};

export const makeWatcherDeploymentIdentitySignaturePayloadV1 = (
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
  value: readonly WatcherDeploymentTrustRootV1[],
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
  for (const contractName of DEPLOYMENT_MANIFEST_V1_CONTRACT_NAMES) {
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
  expected: Readonly<Record<string, WatcherReferenceScriptIdentityV1>>,
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
  expected: WatcherFraudProofCatalogueIdentityV1,
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
  bindings: ReleaseBindingsV1,
  policy: ParsedPolicy,
): void => {
  if (manifest.network !== policy.network) {
    fail("mismatched_identity", "$.manifest.network");
  }
  if (manifest.consensusProfileDigest !== MIDGARD_CONSENSUS_PROFILE_V1_DIGEST) {
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
    computeDeploymentManifestV1JsonDigest(manifest.da) !==
      policy.daIdentityDigest
  ) {
    fail("mismatched_identity", "$.releaseBindings.da");
  }
  const proofEvidence = plainRecord(
    manifest.proofEvidence,
    "$.manifest.proofEvidence",
  );
  if (
    bindings.releaseEvidence.digest !== policy.releaseEvidenceDigest ||
    bindings.releaseEvidence.blueprintHash !== policy.blueprintHash ||
    proofEvidence.digest !== policy.releaseEvidenceDigest ||
    proofEvidence.blueprintHash !== policy.blueprintHash
  ) {
    fail("mismatched_identity", "$.releaseBindings.releaseEvidence");
  }
};

const verifyCanonicalManifest = (value: unknown): JsonRecord => {
  const candidate = (() => {
    try {
      return verifyDeploymentManifestV1Identity(value);
    } catch {
      return fail("canonical_manifest_invalid", "$.manifest");
    }
  })();

  // The finalized decoder owns all strict nested-shape, script-byte/hash,
  // reference-script, catalogue, DA, one-shot, and profile invariants. During
  // pre-release construction its compiled digest is null, so validate the
  // exact signed candidate structurally with only that release slot replaced;
  // the original identity and its non-null release digest are independently
  // authenticated and policy-pinned below.
  const proofEvidence = plainRecord(
    candidate.proofEvidence,
    "$.manifest.proofEvidence",
  );
  const structuralIdentity: JsonRecord = {
    ...candidate,
    proofEvidence: {
      ...proofEvidence,
      digest: MIDGARD_V1_RELEASE_EVIDENCE_DIGEST,
    },
  };
  const { manifestId: _manifestId, ...identityInput } = structuralIdentity;
  try {
    verifyFinalizedDeploymentManifestV1({
      ...identityInput,
      manifestId: computeDeploymentManifestV1Id(identityInput),
    });
  } catch {
    fail("canonical_manifest_invalid", "$.manifest");
  }
  return candidate;
};

export type VerifiedWatcherDeploymentIdentityV1 = Readonly<{
  manifestId: string;
  network: "Mainnet" | "Preprod" | "Preview";
  trustRootId: string;
  releaseEvidenceDigest: string;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  durableMarker: DeploymentMarkerV1;
}>;

export const verifyWatcherDeploymentIdentityV1 = (input: {
  readonly signedIdentity: unknown;
  readonly policy: WatcherDeploymentIdentityPolicyV1;
  readonly trustRoots: readonly WatcherDeploymentTrustRootV1[];
  readonly durableMarker: unknown;
}): VerifiedWatcherDeploymentIdentityV1 => {
  const policy = parsePolicy(input.policy);
  const trustRoots = parseTrustRoots(input.trustRoots);
  const envelope = exactRecord(input.signedIdentity, "$", [
    "schemaVersion",
    "manifest",
    "releaseBindings",
    "attestation",
  ]);
  if (
    envelope.schemaVersion !==
    WATCHER_SIGNED_DEPLOYMENT_IDENTITY_V1_SCHEMA_VERSION
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
  if (policy.releaseEvidenceDigest.length === 0) {
    fail("release_evidence_unavailable", "$.policy.releaseEvidenceDigest");
  }
  assertPolicyBindings(manifest, bindings, policy);

  const expectedMarker = makeDeploymentMarkerV1(manifestId);
  if (input.durableMarker === null || input.durableMarker === undefined) {
    fail("missing_durable_marker", "$.durableMarker");
  }
  try {
    assertDeploymentMarkerV1Matches(
      expectedMarker,
      input.durableMarker,
      "watcher durable store",
    );
  } catch {
    fail("durable_marker_mismatch", "$.durableMarker");
  }

  return Object.freeze({
    manifestId,
    network: policy.network,
    trustRootId,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    ruleBundleCommitment: policy.ruleBundleCommitment,
    programCommitments: policy.programCommitments,
    durableMarker: expectedMarker,
  });
};
