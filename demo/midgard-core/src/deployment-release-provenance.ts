import {
  RELEASE_HEX_32,
  releaseFail,
  releaseRecord,
  releaseString,
  releaseStringMap,
} from "./deployment-release-bindings.js";

export const DEPLOYMENT_RELEASE_PROVENANCE_SCHEMA_VERSION =
  "midgard-deployment-release-provenance-v1" as const;
export const RELEASE_REVISION = /^[0-9a-f]{40}$/u;
// Tags alone are never a release identity. Repository and sha256 digest are required.
export const RELEASE_IMAGE_REFERENCE =
  /^[a-z0-9][a-z0-9./:_-]*@sha256:[0-9a-f]{64}$/u;
export type DeploymentReleaseProvenance = Readonly<{
  schemaVersion: typeof DEPLOYMENT_RELEASE_PROVENANCE_SCHEMA_VERSION;
  sourceRevision: string;
  deploymentProfileName: "preprod-public" | "mainnet";
  deploymentProfileDigest: string;
  blueprintHash: string;
  aikenSourceHash: string;
  compiler: Readonly<{
    forkRepository: string;
    forkRevision: string;
    cargoLockHash: string;
  }>;
  roleImages: Readonly<Record<string, string>>;
  evidence: Readonly<{
    independentBlueprintRebuildDigest: string;
    frozenBuildDigest: string;
    parameterRegistryDigest: string;
  }>;
}>;

/** Parses metadata authenticated by the existing release key, not build proof. */
export const parseDeploymentReleaseProvenance = (
  value: unknown,
): DeploymentReleaseProvenance => {
  const path = "$.releaseProvenance";
  const record = releaseRecord(value, path, [
    "schemaVersion",
    "sourceRevision",
    "deploymentProfileName",
    "deploymentProfileDigest",
    "blueprintHash",
    "aikenSourceHash",
    "compiler",
    "roleImages",
    "evidence",
  ]);
  if (record.schemaVersion !== DEPLOYMENT_RELEASE_PROVENANCE_SCHEMA_VERSION)
    releaseFail("invalid_field", `${path}.schemaVersion`);
  if (
    record.deploymentProfileName !== "preprod-public" &&
    record.deploymentProfileName !== "mainnet"
  )
    releaseFail("invalid_field", `${path}.deploymentProfileName`);
  const compiler = releaseRecord(record.compiler, `${path}.compiler`, [
    "forkRepository",
    "forkRevision",
    "cargoLockHash",
  ]);
  const evidence = releaseRecord(record.evidence, `${path}.evidence`, [
    "independentBlueprintRebuildDigest",
    "frozenBuildDigest",
    "parameterRegistryDigest",
  ]);
  return Object.freeze({
    schemaVersion: DEPLOYMENT_RELEASE_PROVENANCE_SCHEMA_VERSION,
    sourceRevision: releaseString(
      record.sourceRevision,
      `${path}.sourceRevision`,
      RELEASE_REVISION,
    ),
    deploymentProfileName: record.deploymentProfileName as
      | "preprod-public"
      | "mainnet",
    deploymentProfileDigest: releaseString(
      record.deploymentProfileDigest,
      `${path}.deploymentProfileDigest`,
      RELEASE_HEX_32,
    ),
    blueprintHash: releaseString(
      record.blueprintHash,
      `${path}.blueprintHash`,
      RELEASE_HEX_32,
    ),
    aikenSourceHash: releaseString(
      record.aikenSourceHash,
      `${path}.aikenSourceHash`,
      RELEASE_HEX_32,
    ),
    compiler: Object.freeze({
      forkRepository: releaseString(
        compiler.forkRepository,
        `${path}.compiler.forkRepository`,
        /^https:\/\/[a-zA-Z0-9./_-]+$/u,
      ),
      forkRevision: releaseString(
        compiler.forkRevision,
        `${path}.compiler.forkRevision`,
        RELEASE_REVISION,
      ),
      cargoLockHash: releaseString(
        compiler.cargoLockHash,
        `${path}.compiler.cargoLockHash`,
        RELEASE_HEX_32,
      ),
    }),
    roleImages: releaseStringMap(
      record.roleImages,
      `${path}.roleImages`,
      RELEASE_IMAGE_REFERENCE,
    ),
    evidence: Object.freeze({
      independentBlueprintRebuildDigest: releaseString(
        evidence.independentBlueprintRebuildDigest,
        `${path}.evidence.independentBlueprintRebuildDigest`,
        RELEASE_HEX_32,
      ),
      frozenBuildDigest: releaseString(
        evidence.frozenBuildDigest,
        `${path}.evidence.frozenBuildDigest`,
        RELEASE_HEX_32,
      ),
      parameterRegistryDigest: releaseString(
        evidence.parameterRegistryDigest,
        `${path}.evidence.parameterRegistryDigest`,
        RELEASE_HEX_32,
      ),
    }),
  });
};
