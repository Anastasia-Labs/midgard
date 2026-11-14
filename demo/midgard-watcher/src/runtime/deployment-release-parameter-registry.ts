import {
  computeDeploymentManifestJsonDigest,
  DEPLOYMENT_MANIFEST_CONTRACT_NAMES,
  DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  type DeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  releaseRecord,
  releaseString,
} from "@al-ft/midgard-core/deployment-release";
import {
  parseFaultProofBlueprint,
  verifyBlueprintDeploymentRegistry,
} from "@al-ft/midgard-sdk";

/** Checks actual applied bytes, ordered parameters, complete raw census and references. */
export const verifyDeploymentReleaseParameterRegistry = (
  value: unknown,
  blueprintJson: string | Uint8Array,
  manifest: DeploymentManifest,
) => {
  const registry = releaseRecord(value, "$.parameterRegistry", [
    "schemaVersion",
    "manifestId",
    "blueprintHash",
    "deployment",
    "referenceScripts",
  ]);
  if (
    registry.schemaVersion !== "midgard-deployment-parameter-registry-v1" ||
    registry.manifestId !== manifest.manifestId ||
    registry.blueprintHash !== manifest.artifacts.blueprintHash
  )
    throw new Error(
      "Parameter registry belongs to a different deployed release",
    );
  const deployment = verifyBlueprintDeploymentRegistry(
    parseFaultProofBlueprint(
      JSON.parse(Buffer.from(blueprintJson).toString("utf8")),
    ),
    registry.deployment,
  );
  const names = deployment.contracts.map((entry) => entry.contract).sort();
  const expectedNames = Object.keys(manifest.contracts).sort();
  if (
    names.length !== expectedNames.length ||
    names.some((name, index) => name !== expectedNames[index])
  )
    throw new Error(
      "Parameter registry does not cover the complete deployment manifest",
    );
  for (const entry of deployment.contracts) {
    const name = DEPLOYMENT_MANIFEST_CONTRACT_NAMES.find(
      (name) => name === entry.contract,
    );
    const contract = name === undefined ? undefined : manifest.contracts[name];
    const role =
      Object.entries(
        DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
      ).find(([, name]) => name === entry.contract)?.[0] ?? null;
    if (
      contract === undefined ||
      contract.contract.type !== entry.script.type ||
      contract.contract.cborHex !== entry.script.script ||
      contract.scriptHash !== entry.scriptHash ||
      entry.referenceScriptRole !== role
    )
      throw new Error(
        `Parameter registry differs from deployed contract ${entry.contract}`,
      );
  }
  if (
    computeDeploymentManifestJsonDigest(registry.referenceScripts) !==
    computeDeploymentManifestJsonDigest(manifest.referenceScripts)
  )
    throw new Error(
      "Parameter registry reference records differ from deployment manifest",
    );
  return Object.freeze({
    schemaVersion: "midgard-deployment-parameter-registry-v1",
    manifestId: releaseString(
      registry.manifestId,
      "$.parameterRegistry.manifestId",
      /^[0-9a-f]{64}$/u,
    ),
    blueprintHash: manifest.artifacts.blueprintHash,
    deployment,
    referenceScripts: manifest.referenceScripts,
  });
};
