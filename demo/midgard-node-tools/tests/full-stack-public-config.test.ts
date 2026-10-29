import { execFileSync } from "node:child_process";
import {
  createHash,
  createPrivateKey,
  createPublicKey,
  generateKeyPairSync,
  type KeyObject,
  randomBytes,
} from "node:crypto";
import {
  chmod,
  copyFile,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { entropyToMnemonic } from "bip39";
import { afterAll, afterEach, beforeAll, describe, expect, it } from "vitest";

import { loadStackConfig } from "../src/full-stack/config.js";
import { runStackController } from "../src/full-stack/controller.js";
import { SIGNED_RELEASE_FILES } from "../src/sign-deployment-release.js";
import {
  configFixture as fixture,
  removeConfigFixtures,
} from "./full-stack-config.fixtures.js";

// Runs in the `preprod-public-stack` project: this build compiles the
// preprod-public profile, with the fixture's wallets on its allow-list.
afterEach(removeConfigFixtures);

/** A CA and a certificate it issued for the public origin, made once. */
const tls = { directory: "", host: "node.example.test" };
beforeAll(async () => {
  tls.directory = await mkdtemp(join(tmpdir(), "midgard-stack-tls-"));
  const file = (name: string) => join(tls.directory, name);
  const openssl = (args: string[]) =>
    execFileSync("openssl", args, { stdio: "ignore" });
  for (const ca of ["ca", "other-ca"])
    openssl([
      "req",
      "-x509",
      "-newkey",
      "ec",
      "-pkeyopt",
      "ec_paramgen_curve:P-256",
      "-nodes",
      "-keyout",
      file(`${ca}.key`),
      "-out",
      file(`${ca}.pem`),
      "-days",
      "2",
      "-subj",
      `/CN=${ca}`,
    ]);
  openssl([
    "req",
    "-newkey",
    "ec",
    "-pkeyopt",
    "ec_paramgen_curve:P-256",
    "-nodes",
    "-keyout",
    file("node.key"),
    "-out",
    file("node.csr"),
    "-subj",
    `/CN=${tls.host}`,
  ]);
  await writeFile(file("san.cnf"), `subjectAltName=DNS:${tls.host}\n`);
  openssl([
    "x509",
    "-req",
    "-in",
    file("node.csr"),
    "-CA",
    file("ca.pem"),
    "-CAkey",
    file("ca.key"),
    "-CAcreateserial",
    "-out",
    file("node.pem"),
    "-days",
    "2",
    "-extfile",
    file("san.cnf"),
  ]);
});
afterAll(() => rm(tls.directory, { recursive: true, force: true }));

const PUBLIC_SECRETS = [
  "L1_OPERATOR_SEED_PHRASE",
  "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  "L1_SETTLEMENT_SEED_PHRASE",
];

/**
 * A preprod-public stack whose inputs are all present and well formed; only
 * its operator wallet is not on the compiled allow-list, so loading reaches
 * that last check exactly when every earlier input passed.
 */
async function publicFixture() {
  const value = await fixture("preprod-public-stack.example.json");
  const { config, directory, env } = value;
  // Private inputs live outside the Docker build context, as the stack requires.
  const root = join(directory, "logs/public");
  // The stack signs the release here; it does not exist before the first run.
  const release = join(root, "release");
  await mkdir(root, { recursive: true });
  const file = (name: string) => join(root, name);
  for (const name of ["node.pem", "node.key", "ca.pem", "other-ca.pem"])
    await copyFile(join(tls.directory, name), file(name));
  await chmod(file("node.key"), 0o600);
  const secrets: Record<string, string> = {
    ADMIN_API_KEY: randomBytes(32).toString("hex"),
    POSTGRES_PASSWORD: randomBytes(18).toString("base64url"),
  };
  for (const name of PUBLIC_SECRETS) {
    secrets[name] = env[name]!;
    delete env[name];
  }
  const writeSecrets = (values: Record<string, string>, mode = 0o600) =>
    writeFile(
      file("node-secrets.yaml"),
      Object.entries(values)
        .map(([key, secret]) => `${key}: ${JSON.stringify(secret)}`)
        .join("\n"),
      { mode },
    ).then(() => chmod(file("node-secrets.yaml"), mode));
  await writeSecrets(secrets);
  const writeTrustRoots = (keys: KeyObject[]) =>
    writeFile(
      file("trust-roots.json"),
      JSON.stringify(
        keys.map((key) => {
          const der = key.export({ format: "der", type: "spki" });
          return {
            trustRootId: createHash("sha256").update(der).digest("hex"),
            publicKeySpkiDerHex: der.toString("hex"),
          };
        }),
      ),
    );
  // The release signing key the stack signs with, and another trusted key.
  const signer = createPublicKey(
    createPrivateKey(await readFile(value.release.signingKeyFile)),
  );
  const other = generateKeyPairSync("ed25519").publicKey;
  await writeTrustRoots([other, signer]);
  await writeFile(file("blueprint-classification.json"), "{}");
  Object.assign(env, {
    MIDGARD_DEPLOYMENT_PROFILE: "preprod-public",
    MIDGARD_PUBLIC_API_ORIGIN: `https://${tls.host}`,
    MIDGARD_PUBLIC_RELEASE_REVISION: "a".repeat(40),
    VALIDATION_WORKER_POOL_SIZE: "4",
  });
  for (const role of ["NODE", "POSTGRES", "INGRESS"])
    env[`MIDGARD_PUBLIC_${role}_IMAGE`] =
      `example.invalid/${role.toLowerCase()}@sha256:${"c".repeat(64)}`;
  config.public = {
    nodeConfigFile: file("node-secrets.yaml"),
    tlsCertFile: file("node.pem"),
    tlsKeyFile: file("node.key"),
    tlsCaFile: file("ca.pem"),
    trustRootsFile: file("trust-roots.json"),
    blueprintClassificationFile: file("blueprint-classification.json"),
    releaseDirectory: release,
    ingressHostPort: null,
  };
  return { ...value, root, secrets, writeSecrets, writeTrustRoots, other };
}

type PublicFixture = Awaited<ReturnType<typeof publicFixture>>;
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
  it("accepts a complete public input, with or without a signed release", async () => {
    const value = await publicFixture();
    await writeFile(value.config.envFile, value.envText());
    await value.writeConfig();
    const loaded = await loadStackConfig(value.path);
    // The node secrets reach the host deployment commands from the private file.
    expect(loaded.env.ADMIN_API_KEY).toBe(value.secrets.ADMIN_API_KEY);
    // A complete earlier signing is reopened by the public-release step.
    await mkdir(value.config.public!.releaseDirectory);
    for (const name of Object.values(SIGNED_RELEASE_FILES))
      await writeFile(join(value.config.public!.releaseDirectory, name), "{}");
    value.config.public!.ingressHostPort = 18443;
    await value.writeConfig();
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
