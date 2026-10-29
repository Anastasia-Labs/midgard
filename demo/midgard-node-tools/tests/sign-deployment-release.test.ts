import { generateKeyPairSync, type KeyObject } from "node:crypto";
import { mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  computeDeploymentManifestJsonDigest,
  verifyFinalizedDeploymentManifest,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DeploymentReleaseTrustRoot,
  verifySignedDeploymentRelease,
} from "@al-ft/midgard-core/deployment-release";
import { verifyMountedPublicRegistry } from "midgard-node/commands/listen-public-http";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import {
  loadWatcherVerifiedDeploymentAuthority,
  loadWatcherWorkflowFundingProfileOverlay,
} from "midgard-watcher";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  signDeploymentRelease,
  SIGNED_RELEASE_FILES,
} from "../src/sign-deployment-release.js";

type Fixture = Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
type Authority = {
  signedIdentity: {
    manifest: Record<string, unknown>;
    releaseBindings: { programCommitments: Record<string, string> };
    attestation: { signature: string; trustRootId: string };
  };
  trustRoots: DeploymentReleaseTrustRoot[];
};

let fixture: Fixture;
let directory: string;
let inputs: { deploymentInfo: string; blueprint: string };

const PROGRAMS = { "computation-thread-policy-v1": "cd".repeat(32) };

const pem = (key: KeyObject) =>
  key.export({ format: "pem", type: "pkcs8" }) as string;

/** A release input naming a persistent signing key file, as an operator writes it. */
const releaseInput = async (
  name: string,
  key: KeyObject,
  overrides: Record<string, unknown> = {},
) => {
  const signingKeyFile = join(directory, `${name}.pem`);
  await writeFile(signingKeyFile, pem(key), { mode: 0o600 });
  const registryFile = join(directory, `${name}-parameters.json`);
  await writeFile(registryFile, JSON.stringify(fixture.retained));
  const path = join(directory, `${name}-release-input.json`);
  await writeFile(
    path,
    JSON.stringify({
      signingKeyFile,
      programCommitments: PROGRAMS,
      fundingProfiles: [],
      parameterRegistryFile: registryFile,
      ...overrides,
    }),
  );
  return path;
};

const sign = async (name: string, key: KeyObject) => {
  const output = join(directory, name);
  const result = await signDeploymentRelease({
    releaseInput: await releaseInput(name, key),
    ...inputs,
    output,
  });
  return { output, result };
};

const readJson = async <T = unknown>(path: string) =>
  JSON.parse(await readFile(path, "utf8")) as T;

const file = (output: string, name: keyof typeof SIGNED_RELEASE_FILES) =>
  join(output, SIGNED_RELEASE_FILES[name]);

/** Loads a release the way the watcher process does. */
const loadInWatcher = async (
  output: string,
  authority = file(output, "authority"),
) => {
  const verified = await loadWatcherVerifiedDeploymentAuthority({
    path: authority,
    ruleBundlePath: file(output, "rules"),
  });
  await loadWatcherWorkflowFundingProfileOverlay({
    bundlePath: file(output, "funding"),
    deploymentIdentity: verified.deploymentIdentity,
  });
  return verified.deploymentIdentity;
};

beforeAll(async () => {
  fixture = await makeGenuineDeploymentRegistryForTest();
  directory = await mkdtemp(join(tmpdir(), "midgard-sign-release-"));
  inputs = {
    deploymentInfo: join(directory, "contract-deployment-info.json"),
    blueprint: join(directory, "plutus.json"),
  };
  await writeFile(inputs.deploymentInfo, JSON.stringify(fixture.manifest));
  await writeFile(inputs.blueprint, fixture.blueprintJson);
}, 120_000);
afterAll(() => rm(directory, { recursive: true, force: true }));

describe("sign-deployment-release", () => {
  it("writes a release that the node's release verifiers and the watcher accept", async () => {
    const { output, result } = await sign(
      "accepted",
      generateKeyPairSync("ed25519").privateKey,
    );
    expect(result).toMatchObject({
      manifestId: fixture.manifest.manifestId,
      deploymentProfile: fixture.manifest.deploymentProfile.name,
    });
    // No provenance: the test build compiles a testing profile, which has none.
    expect(await readdir(output)).toEqual(
      Object.entries(SIGNED_RELEASE_FILES)
        .filter(([name]) => name !== "provenance")
        .map(([, file]) => file)
        .sort(),
    );
    const authority = await readJson<Authority>(file(output, "authority"));
    const trustRoots = await readJson<DeploymentReleaseTrustRoot[]>(
      file(output, "trustRoots"),
    );
    const release = verifySignedDeploymentRelease({
      signedIdentity: authority.signedIdentity,
      trustRoots,
    });
    expect(release.trustRootId).toBe(result.trustRootId);
    expect(release.manifest.manifestId).toBe(fixture.manifest.manifestId);
    for (const manifest of ["manifest", "deploymentInfo"] as const)
      expect(
        verifyFinalizedDeploymentManifest(
          await readJson(file(output, manifest)),
        ).manifestId,
      ).toBe(fixture.manifest.manifestId);
    const parameters = await readJson(file(output, "parameterRegistry"));
    const classification = await readJson(file(output, "classification"));
    expect(() =>
      verifyMountedPublicRegistry(parameters, classification, {
        manifestId: fixture.manifest.manifestId,
        blueprintHash: fixture.manifest.artifacts.blueprintHash,
        parameterRegistryDigest:
          computeDeploymentManifestJsonDigest(parameters),
      }),
    ).not.toThrow();
    expect((await loadInWatcher(output)).manifestId).toBe(
      fixture.manifest.manifestId,
    );
  });

  it("refuses a release with a tampered signed field", async () => {
    const { output } = await sign(
      "tampered",
      generateKeyPairSync("ed25519").privateKey,
    );
    const authority = await readJson<Authority>(file(output, "authority"));
    const trustRoots = await readJson<DeploymentReleaseTrustRoot[]>(
      file(output, "trustRoots"),
    );
    authority.signedIdentity.releaseBindings.programCommitments[
      "computation-thread-policy-v1"
    ] = "ce".repeat(32);
    const tampered = join(directory, "tampered-authority.json");
    await writeFile(tampered, JSON.stringify(authority));
    expect(() =>
      verifySignedDeploymentRelease({
        signedIdentity: authority.signedIdentity,
        trustRoots,
      }),
    ).toThrow("Invalid deployment release signature");
    await expect(loadInWatcher(output, tampered)).rejects.toThrow(
      "invalid_signature at $.attestation.signature",
    );
  });

  it("refuses a release signed by a key other than the trusted one", async () => {
    const trusted = await sign(
      "trusted",
      generateKeyPairSync("ed25519").privateKey,
    );
    const other = await sign(
      "other",
      generateKeyPairSync("ed25519").privateKey,
    );
    const otherAuthority = await readJson<Authority>(
      file(other.output, "authority"),
    );
    const trustedRoots = await readJson<DeploymentReleaseTrustRoot[]>(
      file(trusted.output, "trustRoots"),
    );
    expect(() =>
      verifySignedDeploymentRelease({
        signedIdentity: otherAuthority.signedIdentity,
        trustRoots: trustedRoots,
      }),
    ).toThrow("Untrusted deployment release signer");
    // The other key's signature presented under the trusted key's root.
    const trustedAuthority = await readJson<Authority>(
      file(trusted.output, "authority"),
    );
    trustedAuthority.signedIdentity.attestation.signature =
      otherAuthority.signedIdentity.attestation.signature;
    expect(() =>
      verifySignedDeploymentRelease({
        signedIdentity: trustedAuthority.signedIdentity,
        trustRoots: trustedRoots,
      }),
    ).toThrow("Invalid deployment release signature");
    const forged = join(directory, "forged-authority.json");
    await writeFile(forged, JSON.stringify(trustedAuthority));
    await expect(loadInWatcher(trusted.output, forged)).rejects.toThrow(
      "invalid_signature at $.attestation.signature",
    );
  });

  it("refuses a signing key that is not ed25519 and an existing output directory", async () => {
    await expect(
      signDeploymentRelease({
        releaseInput: await releaseInput(
          "ec-key",
          generateKeyPairSync("ec", { namedCurve: "P-256" }).privateKey,
        ),
        ...inputs,
        output: join(directory, "ec-key"),
      }),
    ).rejects.toThrow("Release signing key must be a persistent ed25519 key");
    const { output } = await sign(
      "existing",
      generateKeyPairSync("ed25519").privateKey,
    );
    await expect(
      signDeploymentRelease({
        releaseInput: await releaseInput(
          "existing-again",
          generateKeyPairSync("ed25519").privateKey,
        ),
        ...inputs,
        output,
      }),
    ).rejects.toThrow("EEXIST");
  });
});
