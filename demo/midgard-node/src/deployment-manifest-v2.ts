import { createHash } from "node:crypto";

export const DEPLOYMENT_MANIFEST_SCHEMA_VERSION =
  "midgard-deployment-manifest-v2";

const DEPLOYMENT_MANIFEST_NETWORKS = new Set([
  "Mainnet",
  "Preprod",
  "Preview",
  "Custom",
]);

export type DeploymentManifestV2Value = {
  readonly schemaVersion: typeof DEPLOYMENT_MANIFEST_SCHEMA_VERSION;
  readonly manifestId: string;
  readonly network: string;
  readonly referenceScriptDeployAddress: string;
  readonly hubOracleOneShot: {
    readonly txHash: string;
    readonly outputIndex: number;
    readonly outRef: string;
  };
  readonly referenceScriptAuthPolicy: {
    readonly policyId: string;
    readonly nativeScript?: unknown;
    readonly tokenNames?: unknown;
  };
  readonly contracts: Readonly<
    Record<
      string,
      {
        readonly scriptHash?: unknown;
        readonly contract?: unknown;
      }
    >
  >;
  readonly referenceScripts: Readonly<Record<string, unknown>>;
  readonly steps: Readonly<Record<string, unknown>>;
};

const stableJson = (value: unknown): string => {
  if (value === null || typeof value !== "object") {
    return JSON.stringify(value);
  }
  if (Array.isArray(value)) {
    return `[${value.map(stableJson).join(",")}]`;
  }
  const entries = Object.entries(value as Record<string, unknown>)
    .filter(([, entryValue]) => entryValue !== undefined)
    .sort(([left], [right]) => left.localeCompare(right));
  return `{${entries
    .map(
      ([key, entryValue]) => `${JSON.stringify(key)}:${stableJson(entryValue)}`,
    )
    .join(",")}}`;
};

const deploymentManifestIdentityInput = (
  manifest: Omit<DeploymentManifestV2Value, "manifestId">,
): unknown => ({
  schemaVersion: manifest.schemaVersion,
  network: manifest.network,
  referenceScriptDeployAddress: manifest.referenceScriptDeployAddress,
  hubOracleOneShot: {
    txHash: manifest.hubOracleOneShot.txHash,
    outputIndex: manifest.hubOracleOneShot.outputIndex,
    outRef: manifest.hubOracleOneShot.outRef,
  },
  referenceScriptAuthPolicy: {
    policyId: manifest.referenceScriptAuthPolicy.policyId,
    nativeScript: manifest.referenceScriptAuthPolicy.nativeScript,
    tokenNames: manifest.referenceScriptAuthPolicy.tokenNames,
  },
  contracts: Object.fromEntries(
    Object.entries(manifest.contracts)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([name, entry]) => [
        name,
        {
          scriptHash: entry.scriptHash,
          contract: entry.contract,
        },
      ]),
  ),
});

export const computeDeploymentManifestId = (
  manifest: Omit<DeploymentManifestV2Value, "manifestId">,
): string =>
  createHash("sha256")
    .update(stableJson(deploymentManifestIdentityInput(manifest)))
    .digest("hex");

const requireObject = (
  value: unknown,
  field: string,
): Record<string, unknown> => {
  if (typeof value === "object" && value !== null && !Array.isArray(value)) {
    return value as Record<string, unknown>;
  }
  throw new Error(`Deployment manifest ${field} must be an object`);
};

const requireNonEmptyString = (value: unknown, field: string): string => {
  if (typeof value === "string" && value.length > 0) {
    return value;
  }
  throw new Error(`Deployment manifest ${field} must be a non-empty string`);
};

export const parseDeploymentManifestV2Value = (
  value: unknown,
): DeploymentManifestV2Value => {
  const candidate = requireObject(value, "value");
  if (candidate.schemaVersion !== DEPLOYMENT_MANIFEST_SCHEMA_VERSION) {
    throw new Error(
      `Deployment manifest schemaVersion must be ${DEPLOYMENT_MANIFEST_SCHEMA_VERSION}`,
    );
  }
  const network = requireNonEmptyString(candidate.network, "network");
  if (!DEPLOYMENT_MANIFEST_NETWORKS.has(network)) {
    throw new Error(
      "Deployment manifest network must be Mainnet, Preprod, Preview, or Custom",
    );
  }
  requireNonEmptyString(
    candidate.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
  );
  const hubOracleOneShot = requireObject(
    candidate.hubOracleOneShot,
    "hubOracleOneShot",
  );
  const txHash = requireNonEmptyString(
    hubOracleOneShot.txHash,
    "hubOracleOneShot.txHash",
  );
  if (!/^[0-9a-fA-F]{64}$/.test(txHash)) {
    throw new Error(
      "Deployment manifest hubOracleOneShot.txHash must be 32-byte hex",
    );
  }
  const outputIndex = hubOracleOneShot.outputIndex;
  if (
    typeof outputIndex !== "number" ||
    !Number.isSafeInteger(outputIndex) ||
    outputIndex < 0
  ) {
    throw new Error(
      "Deployment manifest hubOracleOneShot.outputIndex must be a non-negative safe integer",
    );
  }
  const expectedOutRef = `${txHash.toLowerCase()}#${outputIndex.toString()}`;
  if (hubOracleOneShot.outRef !== expectedOutRef) {
    throw new Error(
      `Deployment manifest hubOracleOneShot.outRef mismatch: expected ${expectedOutRef}`,
    );
  }
  const referenceScriptAuthPolicy = requireObject(
    candidate.referenceScriptAuthPolicy,
    "referenceScriptAuthPolicy",
  );
  requireNonEmptyString(
    referenceScriptAuthPolicy.policyId,
    "referenceScriptAuthPolicy.policyId",
  );
  requireObject(candidate.contracts, "contracts");
  requireObject(candidate.referenceScripts, "referenceScripts");
  requireObject(candidate.steps, "steps");
  const manifestId = requireNonEmptyString(candidate.manifestId, "manifestId");
  if (!/^[0-9a-f]{64}$/.test(manifestId)) {
    throw new Error(
      "Deployment manifest manifestId must be lowercase SHA-256 hex",
    );
  }
  const parsed = candidate as DeploymentManifestV2Value;
  const { manifestId: _manifestId, ...identityInput } = parsed;
  const expectedManifestId = computeDeploymentManifestId(identityInput);
  if (manifestId !== expectedManifestId) {
    throw new Error(
      `Deployment manifest id mismatch: expected ${expectedManifestId}, found ${manifestId}`,
    );
  }
  return parsed;
};
