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

import {
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";

import { configFixture as fixture } from "./full-stack-config.fixtures.js";

/** A CA and a certificate it issued for the public origin, made once. */
export const tls = { directory: "", host: "node.example.test" };
export async function createPublicTls() {
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
}
export const removePublicTls = () =>
  rm(tls.directory, { recursive: true, force: true });

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
export async function publicFixture() {
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
  // The operator's provenance: the stack writes its parameter registry digest.
  const provenance = {
    schemaVersion: "midgard-deployment-release-provenance-v1",
    sourceRevision: env.MIDGARD_PUBLIC_RELEASE_REVISION!,
    deploymentProfileName: SELECTED_DEPLOYMENT_PROFILE.name,
    deploymentProfileDigest: SELECTED_DEPLOYMENT_PROFILE_DIGEST,
    blueprintHash: "11".repeat(32),
    aikenSourceHash: "22".repeat(32),
    compiler: {
      forkRepository: "https://github.com/example/aiken",
      forkRevision: "b".repeat(40),
      cargoLockHash: "33".repeat(32),
    },
    roleImages: {
      node: env.MIDGARD_PUBLIC_NODE_IMAGE!,
      postgres: env.MIDGARD_PUBLIC_POSTGRES_IMAGE!,
      ingress: env.MIDGARD_PUBLIC_INGRESS_IMAGE!,
    },
    evidence: {
      independentBlueprintRebuildDigest: "44".repeat(32),
      frozenBuildDigest: "55".repeat(32),
    },
  };
  const writeProvenance = (value: unknown) =>
    writeFile(file("provenance.json"), JSON.stringify(value));
  await writeProvenance(provenance);
  const releaseInput = {
    ...value.release,
    provenanceFile: file("provenance.json"),
  };
  const writeReleaseInput = (input: Record<string, unknown>) =>
    writeFile(config.watcher.releaseInput!, JSON.stringify(input));
  await writeReleaseInput(releaseInput);
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
  return {
    ...value,
    root,
    secrets,
    writeSecrets,
    writeTrustRoots,
    other,
    provenance,
    writeProvenance,
    releaseInput,
    writeReleaseInput,
  };
}

export type PublicFixture = Awaited<ReturnType<typeof publicFixture>>;
