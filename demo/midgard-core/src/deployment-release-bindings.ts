import { computeDeploymentManifestJsonDigest } from "./deployment-manifest-identity.js";

export const DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION =
  "midgard-watcher-deployment-release-bindings-v1" as const;
export const SIGNED_DEPLOYMENT_RELEASE_SCHEMA_VERSION =
  "midgard-watcher-signed-deployment-identity-v1" as const;
export const DEPLOYMENT_RELEASE_SIGNATURE_DOMAIN =
  "midgard-watcher-deployment-identity-signature-v1" as const;
export const RELEASE_HEX_32 = /^[0-9a-f]{64}$/u;

type BindingErrorCode = "invalid_field" | "unknown_field" | "missing_field";
export class DeploymentReleaseBindingError extends Error {
  constructor(
    readonly code: BindingErrorCode,
    readonly path: string,
  ) {
    super(`Deployment release rejected: ${code} at ${path}`);
  }
}
export const releaseFail = (code: BindingErrorCode, path: string): never => {
  throw new DeploymentReleaseBindingError(code, path);
};
export const releaseRecord = (
  value: unknown,
  path: string,
  required: readonly string[],
  optional: readonly string[] = [],
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return releaseFail("invalid_field", path);
  }
  const prototype: unknown = Object.getPrototypeOf(value);
  if (prototype !== Object.prototype && prototype !== null) {
    return releaseFail("invalid_field", path);
  }
  const record = value as Record<string, unknown>;
  if (Reflect.ownKeys(record).length !== Object.keys(record).length) {
    return releaseFail("invalid_field", path);
  }
  const allowed = new Set([...required, ...optional]);
  for (const key of Object.keys(record)) {
    if (!allowed.has(key)) releaseFail("unknown_field", `${path}.${key}`);
  }
  for (const key of required) {
    if (!Object.prototype.hasOwnProperty.call(record, key))
      releaseFail("missing_field", `${path}.${key}`);
  }
  return record;
};
export const releaseString = (
  value: unknown,
  path: string,
  pattern: RegExp,
): string => {
  if (typeof value !== "string" || !pattern.test(value))
    return releaseFail("invalid_field", path);
  return value;
};
export const releaseStringMap = (
  value: unknown,
  path: string,
  pattern: RegExp,
): Readonly<Record<string, string>> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return releaseFail("invalid_field", path);
  }
  const record = releaseRecord(value, path, Object.keys(value));
  const keys = Object.keys(record).sort();
  if (keys.length === 0) return releaseFail("missing_field", path);
  return Object.freeze(
    Object.fromEntries(
      keys.map((key) => {
        releaseString(
          key,
          `${path}.${key}`,
          /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u,
        );
        return [key, releaseString(record[key], `${path}.${key}`, pattern)];
      }),
    ),
  );
};

export type DeploymentReleaseBindings = Readonly<{
  schemaVersion: typeof DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION;
  fundingProfileBundleDigest: string;
  ruleBundleCommitment: string;
  programCommitments: Readonly<Record<string, string>>;
  da: Readonly<{ mode: "authenticated_committee_v1"; identityDigest: string }>;
  artifacts: Readonly<{
    blueprintHash: string;
    releaseProvenanceDigest?: string;
  }>;
}>;

/** Existing release signature schema, also consumed by public startup. */
export const parseDeploymentReleaseBindings = (
  value: unknown,
  path = "$.releaseBindings",
): DeploymentReleaseBindings => {
  const record = releaseRecord(value, path, [
    "schemaVersion",
    "fundingProfileBundleDigest",
    "ruleBundleCommitment",
    "programCommitments",
    "da",
    "artifacts",
  ]);
  if (record.schemaVersion !== DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION)
    releaseFail("invalid_field", `${path}.schemaVersion`);
  const da = releaseRecord(record.da, `${path}.da`, ["mode", "identityDigest"]);
  if (da.mode !== "authenticated_committee_v1")
    releaseFail("invalid_field", `${path}.da.mode`);
  const artifacts = releaseRecord(
    record.artifacts,
    `${path}.artifacts`,
    ["blueprintHash"],
    ["releaseProvenanceDigest"],
  );
  return Object.freeze({
    schemaVersion: DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    fundingProfileBundleDigest: releaseString(
      record.fundingProfileBundleDigest,
      `${path}.fundingProfileBundleDigest`,
      RELEASE_HEX_32,
    ),
    ruleBundleCommitment: releaseString(
      record.ruleBundleCommitment,
      `${path}.ruleBundleCommitment`,
      RELEASE_HEX_32,
    ),
    programCommitments: releaseStringMap(
      record.programCommitments,
      `${path}.programCommitments`,
      RELEASE_HEX_32,
    ),
    da: Object.freeze({
      mode: "authenticated_committee_v1",
      identityDigest: releaseString(
        da.identityDigest,
        `${path}.da.identityDigest`,
        RELEASE_HEX_32,
      ),
    }),
    artifacts: Object.freeze({
      blueprintHash: releaseString(
        artifacts.blueprintHash,
        `${path}.artifacts.blueprintHash`,
        RELEASE_HEX_32,
      ),
      ...(Object.prototype.hasOwnProperty.call(
        artifacts,
        "releaseProvenanceDigest",
      )
        ? {
            releaseProvenanceDigest: releaseString(
              artifacts.releaseProvenanceDigest,
              `${path}.artifacts.releaseProvenanceDigest`,
              RELEASE_HEX_32,
            ),
          }
        : {}),
    }),
  });
};

export const makeDeploymentReleaseSignaturePayload = (
  manifestId: string,
  releaseBindings: unknown,
): Buffer => {
  const digest = computeDeploymentManifestJsonDigest({
    manifestId: releaseString(manifestId, "$.manifestId", RELEASE_HEX_32),
    releaseBindings: parseDeploymentReleaseBindings(releaseBindings),
  });
  return Buffer.from(
    `${DEPLOYMENT_RELEASE_SIGNATURE_DOMAIN}\0${digest}`,
    "utf8",
  );
};
