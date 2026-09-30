import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  CATALOGUE_CATEGORY_TO_CONTRACT,
  exactDynamicHexMap,
  exactRecord,
  exactString,
  fail,
  HEX_28,
  HEX_32,
  OUT_REF,
  type ParsedPolicy,
  plainRecord,
  REFERENCE_SCRIPT_ROLES,
  WATCHER_DEPLOYMENT_IDENTITY_SIGNATURE_DOMAIN,
  WATCHER_DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  type WatcherDeploymentIdentityPolicy,
  type WatcherFraudProofCatalogueIdentity,
  type WatcherReferenceScriptIdentity,
} from "./deployment-identity.catalogue-category-to-contract.js";

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

export const parsePolicy = (
  value: WatcherDeploymentIdentityPolicy,
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

export type ReleaseBindings = Readonly<{
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

export const parseReleaseBindings = (
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

export const signaturePayload = (
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

export type ParsedTrustRoot = Readonly<{
  trustRootId: string;
  publicKeySpkiDer: Buffer;
}>;
