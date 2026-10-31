import { createHash, generateKeyPairSync, sign } from "node:crypto";

import { describe, expect, it } from "vitest";

import { computeDeploymentManifestJsonDigest } from "../src/deployment-manifest-identity.js";
import {
  DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
  DEPLOYMENT_RELEASE_SIGNATURE_DOMAIN,
  makeDeploymentReleaseSignaturePayload,
  parseDeploymentReleaseBindings,
  parseDeploymentReleaseProvenance,
  SIGNED_DEPLOYMENT_RELEASE_SCHEMA_VERSION,
  verifySignedDeploymentRelease,
} from "../src/deployment-release.js";
import { finalizedManifest } from "./deployment-manifest-identity.finalized-manifest.js";

const provenance = () => ({
  schemaVersion: "midgard-deployment-release-provenance-v1",
  sourceRevision: "ab".repeat(20),
  deploymentProfileName: "preprod-public",
  deploymentProfileDigest: "cd".repeat(32),
  blueprintHash: "ef".repeat(32),
  aikenSourceHash: "11".repeat(32),
  compiler: {
    forkRepository: "https://github.com/Anastasia-Labs/aiken",
    forkRevision: "22".repeat(20),
    cargoLockHash: "33".repeat(32),
  },
  roleImages: {
    node: `example/midgard@sha256:${"44".repeat(32)}`,
    postgres: `postgres@sha256:${"55".repeat(32)}`,
    ingress: `caddy@sha256:${"66".repeat(32)}`,
  },
  evidence: {
    independentBlueprintRebuildDigest: "77".repeat(32),
    frozenBuildDigest: "88".repeat(32),
    parameterRegistryDigest: "99".repeat(32),
  },
});
const fixture = () => {
  const manifest = finalizedManifest();
  const bindings = {
    schemaVersion: DEPLOYMENT_RELEASE_BINDINGS_SCHEMA_VERSION,
    fundingProfileBundleDigest: "ab".repeat(32),
    ruleBundleCommitment: "bc".repeat(32),
    programCommitments: { "validation-machine-v1": "cd".repeat(32) },
    da: {
      mode: "authenticated_committee_v1",
      identityDigest: computeDeploymentManifestJsonDigest(manifest.da),
    },
    artifacts: {
      blueprintHash: manifest.artifacts.blueprintHash,
      releaseProvenanceDigest:
        computeDeploymentManifestJsonDigest(provenance()),
    },
  };
  const keys = generateKeyPairSync("ed25519");
  const publicKeySpkiDerHex = keys.publicKey
    .export({ format: "der", type: "spki" })
    .toString("hex");
  const trustRootId = createHash("sha256")
    .update(Buffer.from(publicKeySpkiDerHex, "hex"))
    .digest("hex");
  const signedIdentity = {
    schemaVersion: SIGNED_DEPLOYMENT_RELEASE_SCHEMA_VERSION,
    manifest,
    releaseBindings: bindings,
    attestation: {
      algorithm: "ed25519",
      trustRootId,
      signature: sign(
        null,
        makeDeploymentReleaseSignaturePayload(manifest.manifestId, bindings),
        keys.privateKey,
      ).toString("hex"),
    },
  };
  return { signedIdentity, trustRoots: [{ trustRootId, publicKeySpkiDerHex }] };
};

describe("shared persistent release authority", () => {
  it("authenticates the provenance digest through the existing signature domain", () => {
    const input = fixture();
    const verified = verifySignedDeploymentRelease(input);
    expect(verified.bindings.artifacts.releaseProvenanceDigest).toBe(
      computeDeploymentManifestJsonDigest(provenance()),
    );
    expect(verified.manifest.manifestId).toBe(
      input.signedIdentity.manifest.manifestId,
    );
  });
  it("retains the existing payload bytes for explicitly nonpublic releases", () => {
    const { signedIdentity } = fixture();
    const { releaseProvenanceDigest: _digest, ...artifacts } =
      signedIdentity.releaseBindings.artifacts;
    const bindings = { ...signedIdentity.releaseBindings, artifacts };
    expect(
      makeDeploymentReleaseSignaturePayload(
        signedIdentity.manifest.manifestId,
        bindings,
      ).toString(),
    ).toBe(
      `${DEPLOYMENT_RELEASE_SIGNATURE_DOMAIN}\0${computeDeploymentManifestJsonDigest({ manifestId: signedIdentity.manifest.manifestId, releaseBindings: bindings })}`,
    );
  });
  it("refuses a substituted provenance digest under the original signature", () => {
    const input = fixture();
    input.signedIdentity.releaseBindings.artifacts.releaseProvenanceDigest =
      "ff".repeat(32);
    expect(() => verifySignedDeploymentRelease(input)).toThrow(
      "Invalid deployment release signature",
    );
  });
  it("refuses a valid self-signed replacement without the external root", () => {
    const input = fixture();
    const replacement = fixture();
    expect(() =>
      verifySignedDeploymentRelease({
        ...replacement,
        trustRoots: input.trustRoots,
      }),
    ).toThrow("Untrusted deployment release signer");
  });
  it("refuses duplicate external trust roots", () => {
    const input = fixture();
    expect(() =>
      verifySignedDeploymentRelease({
        ...input,
        trustRoots: [...input.trustRoots, ...input.trustRoots],
      }),
    ).toThrow("Invalid external release trust root");
  });
  it("does not drop unknown signed artifact fields", () => {
    const input = fixture();
    expect(() =>
      parseDeploymentReleaseBindings({
        ...input.signedIdentity.releaseBindings,
        artifacts: {
          ...input.signedIdentity.releaseBindings.artifacts,
          image: "unbound",
        },
      }),
    ).toThrow("unknown_field");
  });
});

describe("public release provenance framing", () => {
  it("keeps exact source and digest-pinned role identities", () => {
    const value = provenance();
    expect(parseDeploymentReleaseProvenance(value)).toEqual(value);
  });
  it("frames the Preprod rehearsal as a public release", () => {
    const value = {
      ...provenance(),
      deploymentProfileName: "preprod-rehearsal",
    };
    expect(parseDeploymentReleaseProvenance(value)).toEqual(value);
  });
  it.each([
    [
      "mutable image",
      { ...provenance(), roleImages: { node: "example/midgard:latest" } },
    ],
    ["short revision", { ...provenance(), sourceRevision: "ab1234" }],
    [
      "testing profile",
      { ...provenance(), deploymentProfileName: "preprod-testing" },
    ],
    ["missing build evidence", { ...provenance(), evidence: {} }],
    [
      "unknown evidence",
      {
        ...provenance(),
        evidence: { ...provenance().evidence, verified: true },
      },
    ],
  ])("refuses %s", (_label, value) => {
    expect(() => parseDeploymentReleaseProvenance(value)).toThrow();
  });
});
