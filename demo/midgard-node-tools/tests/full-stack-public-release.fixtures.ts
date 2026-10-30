import { createHash, generateKeyPairSync, randomBytes } from "node:crypto";
import {
  access,
  mkdir,
  mkdtemp,
  readFile,
  rm,
  writeFile,
} from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import { makeGenuineDeploymentRegistryForTest } from "midgard-node/tests/helpers/deployment-parameter-registry";

import { stackRegistryPath } from "../src/full-stack/release.js";
import { runtimeSteps } from "../src/full-stack/runtime.js";
import { classificationOf } from "../src/sign-deployment-release.js";
import {
  RecordingProcesses,
  stackEnvironment,
  stackFixture,
} from "./full-stack-fixtures.js";

// Public stacks for the `preprod-public-release` project, which compiles the
// preprod-public profile against a blueprint built for it.

export const nodeRoot = fileURLToPath(
  new URL("../../midgard-node", import.meta.url),
);
const exampleTemplate = fileURLToPath(
  new URL(
    "../../midgard-watcher/watcher-process.example.json",
    import.meta.url,
  ),
);

let fixture: Awaited<ReturnType<typeof makeGenuineDeploymentRegistryForTest>>;
let directory: string;
/** Builds the deployment every public stack shares; returns its registry fixture. */
export async function createPublicReleaseFixture() {
  fixture = await makeGenuineDeploymentRegistryForTest();
  directory = await mkdtemp(join(tmpdir(), "midgard-stack-public-release-"));
  return fixture;
}
export const removePublicReleaseFixture = () =>
  rm(directory, { recursive: true, force: true });

const SOURCE_REVISION = "a1".repeat(20);
const image = (name: string, byte: string) =>
  `registry.example/midgard/${name}@sha256:${byte.repeat(32)}`;
export const ROLE_IMAGES = {
  node: image("node", "01"),
  postgres: image("postgres", "02"),
  ingress: image("ingress", "03"),
};

/**
 * A preprod-public stack whose deployment is final: the blueprint above the
 * node directory, the finalized manifest in it, the node's secrets in the
 * stack environment as phase 1 leaves them, and a release input naming the
 * signing key and the operator's provenance, which leaves out the parameter
 * registry digest the stack completes.
 */
export async function publicStack(
  name: string,
  ingressHostPort: number | null,
) {
  const checkout = join(directory, name);
  // The example watcher process, following at the compiled profile's depth.
  const template = JSON.parse(await readFile(exampleTemplate, "utf8"));
  template.watcherConfig.l1.finality.depth =
    SELECTED_DEPLOYMENT_PROFILE.l1_finality.confirmation_depth;
  await mkdir(checkout, { recursive: true });
  const processTemplate = join(checkout, "watcher-process.json");
  await writeFile(processTemplate, JSON.stringify(template));
  const { config } = await stackFixture((sample) => {
    sample.watcher.processTemplate = processTemplate;
    sample.watcher.releaseDirectory = join(checkout, "watcher-release");
    sample.watcher.releaseInput = join(checkout, "release-input.json");
    sample.da.members = sample.da.members.slice(0, 1);
    sample.timeoutMs = 1;
  });
  config.nodeRoot = join(checkout, "demo/midgard-node");
  // The stack reads the node directory's own .env, as the existing Compose stack does.
  config.envFile = join(config.nodeRoot, ".env");
  config.runDirectory = join(checkout, "run");
  const file = (path: string) => join(checkout, "private", path);
  await mkdir(join(checkout, "private"), { recursive: true });
  await mkdir(join(checkout, "onchain/aiken"), { recursive: true });
  await mkdir(join(config.nodeRoot, "deploymentInfo"), { recursive: true });
  await writeFile(
    join(checkout, "onchain/aiken/plutus.json"),
    fixture.blueprintJson,
  );
  await writeFile(
    join(config.nodeRoot, "deploymentInfo/contract-deployment-info.json"),
    JSON.stringify(fixture.manifest),
  );
  const signer = generateKeyPairSync("ed25519");
  const der = signer.publicKey.export({ format: "der", type: "spki" });
  await writeFile(
    file("signer.pem"),
    signer.privateKey.export({ format: "pem", type: "pkcs8" }),
    { mode: 0o600 },
  );
  await writeFile(
    file("trust-roots.json"),
    JSON.stringify([
      {
        trustRootId: createHash("sha256").update(der).digest("hex"),
        publicKeySpkiDerHex: der.toString("hex"),
      },
    ]),
  );
  // The parameter registry the deployment step wrote beside the manifest.
  await writeFile(stackRegistryPath(config), JSON.stringify(fixture.retained));
  // The operator supplies every provenance field but the registry digest.
  const provenance = {
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
    } as Record<string, string>,
  };
  const writeProvenance = (value: unknown) =>
    writeFile(file("provenance.json"), JSON.stringify(value));
  await writeProvenance(provenance);
  await writeFile(
    config.watcher.releaseInput!,
    JSON.stringify({
      signingKeyFile: file("signer.pem"),
      provenanceFile: file("provenance.json"),
      programCommitments: {},
      fundingProfiles: [],
    }),
  );
  // The classification the reference-script deployment used.
  await writeFile(
    file("blueprint-classification.json"),
    JSON.stringify(classificationOf(fixture.retained)),
  );
  config.public = {
    nodeConfigFile: file("node-secrets.yaml"),
    tlsCertFile: file("node.pem"),
    tlsKeyFile: file("node.key"),
    tlsCaFile: file("ca.pem"),
    trustRootsFile: file("trust-roots.json"),
    blueprintClassificationFile: file("blueprint-classification.json"),
    releaseDirectory: file("release"),
    ingressHostPort,
  };
  const env: Record<string, string> = {
    ...stackEnvironment(config),
    MIDGARD_DEPLOYMENT_PROFILE: "preprod-public",
    MIDGARD_PUBLIC_API_ORIGIN: "https://node.example.test",
    MIDGARD_PUBLIC_RELEASE_REVISION: SOURCE_REVISION,
    MIDGARD_PUBLIC_NODE_IMAGE: ROLE_IMAGES.node,
    MIDGARD_PUBLIC_POSTGRES_IMAGE: ROLE_IMAGES.postgres,
    MIDGARD_PUBLIC_INGRESS_IMAGE: ROLE_IMAGES.ingress,
    POSTGRES_USER: "midgard",
    POSTGRES_DB: "midgard",
    // What the deployment steps restore before the services run.
    HUB_ORACLE_ONE_SHOT_TX_HASH: "ab".repeat(32),
    HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX: "0",
    L1_ORIGIN: `7.${"cd".repeat(32)}`,
  };
  // Phase 1 moves the node's secrets from its private file into the stack environment.
  for (const secret of PUBLIC_NODE_SECRET_NAMES)
    env[secret] ??= randomBytes(24).toString("base64url");
  await writeFile(
    config.envFile,
    Object.entries(env)
      .map(([key, value]) => `${key}=${value}\n`)
      .join(""),
    { mode: 0o600 },
  );
  const processes = new RecordingProcesses(config, env);
  const [release, configuration, , services] = runtimeSteps(processes);
  return {
    processes,
    release: release!,
    configuration: configuration!,
    services: services!,
    releaseDirectory: config.public.releaseDirectory,
    classification: file("blueprint-classification.json"),
    provenance,
    writeProvenance,
  };
}

export const exists = (path: string) =>
  access(path).then(
    () => true,
    () => false,
  );
