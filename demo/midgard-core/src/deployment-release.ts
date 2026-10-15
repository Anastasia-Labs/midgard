import { createHash, createPublicKey, verify } from "node:crypto";

import {
  computeDeploymentManifestJsonDigest,
  verifyFinalizedDeploymentManifest,
} from "./deployment-manifest-identity.js";
import {
  makeDeploymentReleaseSignaturePayload,
  parseDeploymentReleaseBindings,
  RELEASE_HEX_32,
  releaseFail,
  releaseRecord,
  releaseString,
  SIGNED_DEPLOYMENT_RELEASE_SCHEMA_VERSION,
} from "./deployment-release-bindings.js";
import { parseDeploymentReleaseProvenance } from "./deployment-release-provenance.js";

export * from "./deployment-release-bindings.js";
export * from "./deployment-release-provenance.js";

export type DeploymentReleaseTrustRoot = Readonly<{
  trustRootId: string;
  publicKeySpkiDerHex: string;
}>;

/** Uses externally pinned roots; authority-file roots never establish trust. */
export const verifySignedDeploymentRelease = (input: {
  readonly signedIdentity: unknown;
  readonly trustRoots: readonly DeploymentReleaseTrustRoot[];
}) => {
  const envelope = releaseRecord(input.signedIdentity, "$", [
    "schemaVersion",
    "manifest",
    "releaseBindings",
    "attestation",
  ]);
  if (envelope.schemaVersion !== SIGNED_DEPLOYMENT_RELEASE_SCHEMA_VERSION)
    releaseFail("invalid_field", "$.schemaVersion");
  const manifest = verifyFinalizedDeploymentManifest(envelope.manifest);
  const bindings = parseDeploymentReleaseBindings(envelope.releaseBindings);
  const attestation = releaseRecord(envelope.attestation, "$.attestation", [
    "algorithm",
    "trustRootId",
    "signature",
  ]);
  if (attestation.algorithm !== "ed25519")
    releaseFail("invalid_field", "$.attestation.algorithm");
  const trustRootId = releaseString(
    attestation.trustRootId,
    "$.attestation.trustRootId",
    RELEASE_HEX_32,
  );
  if (
    !Array.isArray(input.trustRoots) ||
    input.trustRoots.length === 0 ||
    input.trustRoots.length > 16
  )
    throw new Error("Invalid external release trust roots");
  const roots = new Map<string, ReturnType<typeof createPublicKey>>();
  input.trustRoots.forEach((value, index) => {
    const path = `$.trustRoots[${index.toString()}]`;
    const record = releaseRecord(value, path, [
      "trustRootId",
      "publicKeySpkiDerHex",
    ]);
    const id = releaseString(
      record.trustRootId,
      `${path}.trustRootId`,
      RELEASE_HEX_32,
    );
    const der = Buffer.from(
      releaseString(
        record.publicKeySpkiDerHex,
        `${path}.publicKeySpkiDerHex`,
        /^(?:[0-9a-f]{2}){44}$/u,
      ),
      "hex",
    );
    const key = createPublicKey({ key: der, format: "der", type: "spki" });
    if (
      roots.has(id) ||
      key.asymmetricKeyType !== "ed25519" ||
      createHash("sha256").update(der).digest("hex") !== id
    )
      throw new Error("Invalid external release trust root");
    roots.set(id, key);
  });
  const key = roots.get(trustRootId);
  if (key === undefined) throw new Error("Untrusted deployment release signer");
  const signature = Buffer.from(
    releaseString(
      attestation.signature,
      "$.attestation.signature",
      /^[0-9a-f]{128}$/u,
    ),
    "hex",
  );
  if (
    !verify(
      null,
      makeDeploymentReleaseSignaturePayload(manifest.manifestId, bindings),
      key,
      signature,
    )
  )
    throw new Error("Invalid deployment release signature");
  if (
    bindings.artifacts.blueprintHash !== manifest.artifacts.blueprintHash ||
    bindings.da.identityDigest !==
      computeDeploymentManifestJsonDigest(manifest.da)
  )
    throw new Error(
      "Deployment release bindings differ from finalized manifest",
    );
  return Object.freeze({ manifest, bindings, trustRootId });
};

/** Authenticates release metadata and configured identities before public bind. */
export const verifyPublicDeploymentRelease = (input: {
  readonly signedIdentity: unknown;
  readonly trustRoots: readonly DeploymentReleaseTrustRoot[];
  readonly provenance: unknown;
  readonly finalizedManifest: unknown;
  readonly expected: Readonly<{
    sourceRevision: string;
    manifestId: string;
    roleImages: Readonly<Record<string, string>>;
  }>;
}) => {
  const release = verifySignedDeploymentRelease(input);
  const manifest = verifyFinalizedDeploymentManifest(input.finalizedManifest);
  const provenance = parseDeploymentReleaseProvenance(input.provenance);
  if (
    release.bindings.artifacts.releaseProvenanceDigest !==
    computeDeploymentManifestJsonDigest(provenance)
  )
    throw new Error(
      "Release provenance is absent or differs from signed authority",
    );
  if (
    manifest.manifestId !== release.manifest.manifestId ||
    manifest.manifestId !== input.expected.manifestId
  )
    throw new Error(
      "Configured manifest differs from signed deployment release",
    );
  if (
    provenance.sourceRevision !== input.expected.sourceRevision ||
    provenance.deploymentProfileName !== manifest.deploymentProfile.name ||
    provenance.deploymentProfileDigest !== manifest.deploymentProfileDigest ||
    provenance.blueprintHash !== manifest.artifacts.blueprintHash
  )
    throw new Error(
      "Configured source/profile/blueprint differs from signed release provenance",
    );
  const expectedImages = input.expected.roleImages;
  if (Object.keys(expectedImages).length === 0)
    throw new Error("Expected public role images are required");
  for (const [role, image] of Object.entries(expectedImages)) {
    if (provenance.roleImages[role] !== image)
      throw new Error(
        `Configured ${role} image differs from signed release provenance`,
      );
  }
  return Object.freeze({
    manifestId: manifest.manifestId,
    trustRootId: release.trustRootId,
    provenance,
  });
};
