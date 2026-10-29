import { createHash, generateKeyPairSync, type KeyObject } from "node:crypto";
import { access, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { authenticatePublicHttpRelease } from "midgard-node/commands/listen-public-http";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  signDeploymentRelease,
  SIGNED_RELEASE_FILES,
} from "../src/sign-deployment-release.js";

// Runs in the `preprod-public-release` project, which compiles the
// preprod-public profile against a blueprint built for it.

type Fixture = Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;

let fixture: Fixture;
let directory: string;
let inputs: { deploymentInfo: string; blueprint: string };

const SOURCE_REVISION = "a1".repeat(20);
const image = (name: string, byte: string) =>
  `registry.example/midgard/${name}@sha256:${byte.repeat(32)}`;
const ROLE_IMAGES = {
  node: image("node", "01"),
  postgres: image("postgres", "02"),
  ingress: image("ingress", "03"),
};

/**
 * Release provenance for the fixture deployment, as the release build writes
 * it. It carries no build proof, so a test may construct it.
 */
const provenance = (overrides: Record<string, unknown> = {}) => ({
  schemaVersion: "midgard-deployment-release-provenance-v1",
  sourceRevision: SOURCE_REVISION,
  deploymentProfileName: fixture.manifest.deploymentProfile.name,
  deploymentProfileDigest: fixture.manifest.deploymentProfileDigest,
  blueprintHash: fixture.manifest.artifacts.blueprintHash,
  aikenSourceHash: "11".repeat(32),
  compiler: {
    forkRepository: "https://github.com/example/aiken",
    forkRevision: "b2".repeat(20),
    cargoLockHash: "22".repeat(32),
  },
  roleImages: ROLE_IMAGES,
  evidence: {
    independentBlueprintRebuildDigest: "33".repeat(32),
    frozenBuildDigest: "44".repeat(32),
    parameterRegistryDigest: computeDeploymentManifestJsonDigest(
      fixture.retained,
    ),
  },
  ...overrides,
});

const pem = (key: KeyObject) =>
  key.export({ format: "pem", type: "pkcs8" }) as string;

/** A public release input naming the key, provenance and registry files. */
const releaseInput = async (
  name: string,
  key: KeyObject,
  releaseProvenance: unknown,
) => {
  const signingKeyFile = join(directory, `${name}.pem`);
  await writeFile(signingKeyFile, pem(key), { mode: 0o600 });
  const parameterRegistryFile = join(directory, `${name}-parameters.json`);
  await writeFile(parameterRegistryFile, JSON.stringify(fixture.retained));
  const provenanceFile = join(directory, `${name}-provenance.json`);
  await writeFile(provenanceFile, JSON.stringify(releaseProvenance));
  const path = join(directory, `${name}-release-input.json`);
  await writeFile(
    path,
    JSON.stringify({
      signingKeyFile,
      provenanceFile,
      parameterRegistryFile,
      programCommitments: {
        "computation-thread-policy-v1": createHash("sha256")
          .update(
            JSON.stringify({
              computationThreadPolicyId:
                fixture.manifest.contracts.computationThreadMint!.scriptHash,
            }),
          )
          .digest("hex"),
      },
      fundingProfiles: [],
    }),
  );
  return path;
};

const sign = async (name: string, releaseProvenance: unknown) => {
  const output = join(directory, name);
  const result = await signDeploymentRelease({
    releaseInput: await releaseInput(
      name,
      generateKeyPairSync("ed25519").privateKey,
      releaseProvenance,
    ),
    ...inputs,
    output,
  });
  return { output, result };
};

const file = (output: string, name: keyof typeof SIGNED_RELEASE_FILES) =>
  join(output, SIGNED_RELEASE_FILES[name]);

/** The environment an operator gives public listen for a signed release. */
const publicListenEnv = (output: string, manifestId: string) => ({
  MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: file(output, "authority"),
  MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE: file(output, "trustRoots"),
  MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE: file(output, "provenance"),
  MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE: file(output, "manifest"),
  MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE: file(output, "parameterRegistry"),
  MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: file(output, "classification"),
  MIDGARD_PUBLIC_MANIFEST_ID: manifestId,
  MIDGARD_PUBLIC_RELEASE_REVISION: SOURCE_REVISION,
  MIDGARD_PUBLIC_NODE_IMAGE: ROLE_IMAGES.node,
  MIDGARD_PUBLIC_POSTGRES_IMAGE: ROLE_IMAGES.postgres,
  MIDGARD_PUBLIC_INGRESS_IMAGE: ROLE_IMAGES.ingress,
});

const PUBLIC_LISTEN_REFUSAL =
  "public release signature, compiled profile, provenance or configured identity is invalid";

beforeAll(async () => {
  fixture = await makeGenuineDeploymentRegistryForTest();
  directory = await mkdtemp(join(tmpdir(), "midgard-sign-public-release-"));
  inputs = {
    deploymentInfo: join(directory, "contract-deployment-info.json"),
    blueprint: join(directory, "plutus.json"),
  };
  await writeFile(inputs.deploymentInfo, JSON.stringify(fixture.manifest));
  await writeFile(inputs.blueprint, fixture.blueprintJson);
}, 300_000);
afterAll(() => rm(directory, { recursive: true, force: true }));

describe("sign-deployment-release for a public profile", () => {
  it("writes a release that public listen authenticates, and refuses a changed identity, classification or trust root", async () => {
    expect(fixture.manifest.deploymentProfile.name).toBe("preprod-public");
    const { output, result } = await sign("accepted", provenance());
    const env = publicListenEnv(output, result.manifestId);
    expect(() => authenticatePublicHttpRelease(env)).not.toThrow();

    expect(() =>
      authenticatePublicHttpRelease({
        ...env,
        MIDGARD_PUBLIC_RELEASE_REVISION: "c3".repeat(20),
      }),
    ).toThrow(PUBLIC_LISTEN_REFUSAL);

    const classification = JSON.parse(
      await readFile(file(output, "classification"), "utf8"),
    ) as Record<string, string>;
    const [title] = Object.keys(classification);
    expect(title).toBeDefined();
    // A copy elsewhere is accepted; the same copy with one reason edited is not.
    const copied = join(directory, "copied-classification.json");
    await writeFile(copied, JSON.stringify(classification));
    expect(() =>
      authenticatePublicHttpRelease({
        ...env,
        MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: copied,
      }),
    ).not.toThrow();
    const edited = join(directory, "edited-classification.json");
    await writeFile(
      edited,
      JSON.stringify({ ...classification, [title!]: "Edited reason" }),
    );
    expect(() =>
      authenticatePublicHttpRelease({
        ...env,
        MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: edited,
      }),
    ).toThrow(PUBLIC_LISTEN_REFUSAL);

    const copiedRoots = join(directory, "copied-trust-roots.json");
    await writeFile(
      copiedRoots,
      await readFile(file(output, "trustRoots"), "utf8"),
    );
    expect(() =>
      authenticatePublicHttpRelease({
        ...env,
        MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE: copiedRoots,
      }),
    ).not.toThrow();
    const otherKey = Buffer.from(
      generateKeyPairSync("ed25519").publicKey.export({
        format: "der",
        type: "spki",
      }),
    );
    const otherRoots = join(directory, "other-trust-roots.json");
    await writeFile(
      otherRoots,
      JSON.stringify([
        {
          trustRootId: createHash("sha256").update(otherKey).digest("hex"),
          publicKeySpkiDerHex: otherKey.toString("hex"),
        },
      ]),
    );
    expect(() =>
      authenticatePublicHttpRelease({
        ...env,
        MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE: otherRoots,
      }),
    ).toThrow(PUBLIC_LISTEN_REFUSAL);
  });

  it("refuses, and leaves no directory, when public listen would refuse the release", async () => {
    // Public listen pins the node, Postgres and ingress images; provenance
    // without an ingress image signs, but no public node can start from it.
    const { ingress: _ingress, ...withoutIngress } = ROLE_IMAGES;
    await expect(
      sign("no-ingress-image", provenance({ roleImages: withoutIngress })),
    ).rejects.toThrow(
      `Public listen refuses the signed release: E_PUBLIC_HTTP_CONFIG: ${PUBLIC_LISTEN_REFUSAL}`,
    );
    await expect(access(join(directory, "no-ingress-image"))).rejects.toThrow(
      "ENOENT",
    );
  });
});
