import { execFileSync } from "node:child_process";
import { randomBytes } from "node:crypto";
import { mkdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import { loadStackConfig } from "../src/full-stack/config.js";
import { runStackController } from "../src/full-stack/controller.js";
import { stackPaths } from "../src/full-stack/deployment.js";
import { INGRESS_SUBNET } from "../src/full-stack/public-node.js";
import { SIGNED_RELEASE_FILES } from "../src/sign-deployment-release.js";
import { removeConfigFixtures } from "./full-stack-config.fixtures.js";
import {
  createPublicTls,
  type PublicFixture,
  publicFixture,
  removePublicTls,
  tls,
} from "./full-stack-public-config.fixtures.js";

// Runs in the `preprod-public-stack` project: this build compiles the
// preprod-public profile, with the fixture's wallets on its allow-list.
afterEach(removeConfigFixtures);
beforeAll(createPublicTls);
afterAll(removePublicTls);

describe("preprod-public inputs refused before any build or spend", () => {
  const rows: [
    string,
    (value: PublicFixture) => void | Promise<void>,
    string,
  ][] = [
    [
      "a missing public section",
      ({ config }) => {
        delete config.public;
      },
      "needs the public section",
    ],
    [
      "a plain HTTP endpoint",
      ({ config }) => {
        config.endpoint = "http://127.0.0.1:3000";
      },
      "serves HTTPS",
    ],
    [
      "a node secret in the .env",
      ({ env, secrets }) => {
        env.ADMIN_API_KEY = secrets.ADMIN_API_KEY!;
      },
      "Keep ADMIN_API_KEY in public.nodeConfigFile",
    ],
    [
      "a setting the stack derives",
      ({ env }) => {
        env.MIDGARD_CONFIG_FILE = "/run/secrets/node-config.json";
      },
      "The stack sets MIDGARD_CONFIG_FILE",
    ],
    [
      "a user wallet seed",
      ({ env }) => {
        env.L1_USER_SEED_PHRASE = entropyToMnemonic("20".repeat(16));
      },
      "cannot hold L1_USER_SEED_PHRASE",
    ],
    [
      "a private input inside the Docker build context",
      ({ config, directory }) => {
        config.public!.nodeConfigFile = join(directory, "node-secrets.yaml");
      },
      "public.nodeConfigFile is inside the Docker build context",
    ],
    [
      "a missing node configuration",
      ({ config, root }) => {
        config.public!.nodeConfigFile = join(root, "absent.yaml");
      },
      "absent.yaml is missing",
    ],
    [
      "a readable node configuration",
      ({ secrets, writeSecrets }) => writeSecrets(secrets, 0o644),
      "must be private: chmod 600",
    ],
    [
      "a missing settlement seed",
      ({ secrets, writeSecrets }) => {
        const { L1_SETTLEMENT_SEED_PHRASE: _, ...rest } = secrets;
        return writeSecrets(rest);
      },
      "public L1_SETTLEMENT_SEED_PHRASE must come from the mounted configuration",
    ],
    [
      "a missing admin key",
      ({ secrets, writeSecrets }) => {
        const { ADMIN_API_KEY: _, ...rest } = secrets;
        return writeSecrets(rest);
      },
      "public ADMIN_API_KEY must come from the mounted configuration",
    ],
    [
      "a non-secret setting in the node configuration",
      ({ secrets, writeSecrets }) => writeSecrets({ ...secrets, PORT: "3000" }),
      "move PORT to",
    ],
    [
      "a weak admin key",
      ({ secrets, writeSecrets }) =>
        writeSecrets({ ...secrets, ADMIN_API_KEY: "0".repeat(64) }),
      "admin credentials require a random 32-byte",
    ],
    [
      "a missing release revision",
      ({ env }) => {
        delete env.MIDGARD_PUBLIC_RELEASE_REVISION;
      },
      "an exact public release revision is required",
    ],
    [
      "a node image without a digest",
      ({ env }) => {
        env.MIDGARD_PUBLIC_NODE_IMAGE = "example.invalid/node:latest";
      },
      "MIDGARD_PUBLIC_NODE_IMAGE must be digest-pinned",
    ],
    [
      "a missing ingress image",
      ({ env }) => {
        delete env.MIDGARD_PUBLIC_INGRESS_IMAGE;
      },
      "MIDGARD_PUBLIC_INGRESS_IMAGE must be digest-pinned",
    ],
    [
      "a missing public origin",
      ({ env }) => {
        delete env.MIDGARD_PUBLIC_API_ORIGIN;
      },
      "an explicit HTTPS public origin is required",
    ],
    [
      "a certificate that does not cover the origin",
      ({ env }) => {
        env.MIDGARD_PUBLIC_API_ORIGIN = "https://other.example.test";
      },
      "TLS certificate and private key must match the public origin",
    ],
    [
      "a missing TLS key",
      ({ config, root }) => {
        config.public!.tlsKeyFile = join(root, "absent.key");
      },
      "absent.key is missing",
    ],
    [
      "a CA that did not issue the certificate",
      ({ config, root }) => {
        config.public!.tlsCaFile = join(root, "other-ca.pem");
      },
      `public.tlsCaFile does not verify the TLS certificate for ${tls.host}`,
    ],
    [
      "missing release trust roots",
      ({ config, root }) => {
        config.public!.trustRootsFile = join(root, "absent.json");
      },
      "public.trustRootsFile",
    ],
    [
      "trust roots without the release signing key",
      ({ other, writeTrustRoots }) => writeTrustRoots([other]),
      "public.trustRootsFile does not list the public key of the release signing key",
    ],
    [
      "a release input without a provenance",
      ({ releaseInput, writeReleaseInput }) => {
        const { provenanceFile: _, ...rest } = releaseInput;
        return writeReleaseInput(rest);
      },
      "names its release provenance as an absolute provenanceFile",
    ],
    [
      "a release input naming another parameter registry",
      ({ releaseInput, root, writeReleaseInput }) =>
        writeReleaseInput({
          ...releaseInput,
          parameterRegistryFile: join(root, "parameters.json"),
        }),
      "signs the parameter registry its deployment writes",
    ],
    [
      "a provenance for another source revision",
      ({ provenance, writeProvenance }) =>
        writeProvenance({ ...provenance, sourceRevision: "f".repeat(40) }),
      "has sourceRevision ffff",
    ],
    [
      "a provenance for another node image",
      ({ provenance, writeProvenance }) =>
        writeProvenance({
          ...provenance,
          roleImages: {
            ...provenance.roleImages,
            node: `example.invalid/node@sha256:${"d".repeat(64)}`,
          },
        }),
      "has roleImages.node example.invalid/node@sha256:dddd",
    ],
    [
      "a provenance for another profile",
      ({ provenance, writeProvenance }) =>
        writeProvenance({
          ...provenance,
          deploymentProfileDigest: "66".repeat(32),
        }),
      "has deploymentProfileDigest 6666",
    ],
    [
      "a provenance with a malformed compiler fork",
      ({ provenance, writeProvenance }) =>
        writeProvenance({
          ...provenance,
          compiler: {
            ...provenance.compiler,
            forkRepository: "http://github.com/example/aiken",
          },
        }),
      "$.releaseProvenance.compiler.forkRepository",
    ],
    [
      "a missing provenance",
      ({ root, releaseInput, writeReleaseInput }) =>
        writeReleaseInput({
          ...releaseInput,
          provenanceFile: join(root, "absent-provenance.json"),
        }),
      "absent-provenance.json is missing",
    ],
    [
      "a release directory whose parent is missing",
      ({ config, root }) => {
        config.public!.releaseDirectory = join(root, "absent/release");
      },
      "The parent of public.releaseDirectory",
    ],
    [
      "a partly written release directory",
      async ({ config }) => {
        await mkdir(config.public!.releaseDirectory);
        await writeFile(
          join(config.public!.releaseDirectory, "deployment-authority.json"),
          "{}",
        );
      },
      "exists but is not a complete signed release",
    ],
    [
      "a missing blueprint classification",
      ({ config }) => rm(config.public!.blueprintClassificationFile),
      "public.blueprintClassificationFile",
    ],
    [
      "a single validation worker",
      ({ env }) => {
        env.VALIDATION_WORKER_POOL_SIZE = "1";
      },
      "VALIDATION_WORKER_POOL_SIZE of at least 2",
    ],
    [
      "disabled HTTP rate limits",
      ({ env }) => {
        env.HTTP_RATE_LIMIT_ENABLED = "false";
      },
      "cannot disable HTTP rate limits",
    ],
    [
      "an ingress port taken by the node",
      ({ config }) => {
        config.public!.ingressHostPort = 3000;
      },
      "public.ingressHostPort must differ",
    ],
    [
      // Every input above is present and valid in the fixture, so this last
      // check is reached only when all of them passed.
      "an operator wallet outside the compiled allow-list",
      ({ secrets, writeSecrets }) =>
        writeSecrets({
          ...secrets,
          L1_OPERATOR_SEED_PHRASE: entropyToMnemonic("30".repeat(16)),
        }),
      "operator wallet must match the compiled public allow-list",
    ],
    [
      "a preprod-testing environment in this preprod-public build",
      ({ config, env }) => {
        delete config.public;
        env.MIDGARD_DEPLOYMENT_PROFILE = "preprod-testing";
      },
      "This checkout is compiled for preprod-public",
    ],
  ];
  it.each(rows)("refuses %s", async (_, mutate, message) => {
    const value = await publicFixture();
    await mutate(value);
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).rejects.toThrow(message);
  });
  it("accepts a complete public input without its registry digest, with or without a signed release", async () => {
    const value = await publicFixture();
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    const loaded = await loadStackConfig(value.path);
    // The node secrets reach the host deployment commands from the private file.
    expect(loaded.env.ADMIN_API_KEY).toBe(value.secrets.ADMIN_API_KEY);
    // Naming the registry the deployment will write, before it exists.
    await value.writeReleaseInput({
      ...value.releaseInput,
      parameterRegistryFile: `${stackPaths(value).manifest}.blueprint-registry.json`,
    });
    await expect(loadStackConfig(value.path)).resolves.toBeDefined();
    // A complete earlier signing is reopened by the public-release step.
    await mkdir(value.config.public!.releaseDirectory);
    for (const name of Object.values(SIGNED_RELEASE_FILES))
      await writeFile(join(value.config.public!.releaseDirectory, name), "{}");
    value.config.public!.ingressHostPort = 18443;
    await value.writeConfig();
    await expect(loadStackConfig(value.path)).resolves.toBeDefined();
  });
  it("refuses an ingress whose subnet another Docker network holds", async () => {
    const value = await publicFixture();
    value.config.public!.ingressHostPort = 18443;
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    const network = `midgard-subnet-test-${randomBytes(4).toString("hex")}`;
    execFileSync("docker", [
      "network",
      "create",
      "--subnet",
      INGRESS_SUBNET,
      network,
    ]);
    try {
      await expect(loadStackConfig(value.path)).rejects.toThrow(
        `Docker network ${network} already uses ${INGRESS_SUBNET}, which overlaps the public ingress subnet`,
      );
    } finally {
      execFileSync("docker", ["network", "rm", network]);
    }
    await expect(loadStackConfig(value.path)).resolves.toBeDefined();
  });
  it("refuses the same input with --check", async () => {
    const value = await publicFixture();
    delete value.env.MIDGARD_PUBLIC_RELEASE_REVISION;
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    await expect(
      runStackController({ config: value.path, check: true }),
    ).rejects.toThrow("an exact public release revision is required");
  });
});
