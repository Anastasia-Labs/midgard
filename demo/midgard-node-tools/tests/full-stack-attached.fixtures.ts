import { execFile } from "node:child_process";
import { randomBytes } from "node:crypto";
import { copyFile, mkdir, readFile, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import { createServer as createHttpsServer } from "node:https";
import type { AddressInfo } from "node:net";
import { dirname, isAbsolute, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import { formatL1Origin } from "@al-ft/midgard-core/l1-origin";
import { entropyToMnemonic } from "bip39";
import { PUBLIC_NODE_SECRET_NAMES } from "midgard-node/commands/listen-public-http";
import {
  computeDeploymentManifestDaCommitteeSignersHash,
  computeDeploymentManifestId,
} from "midgard-node/deployment-manifest";
import { writeFinalizedDeploymentInfo } from "midgard-node/tests/da-libp2p-runtime-manifest.write-finalized-deployment-info";

import { attachedSteps } from "../src/full-stack/attached.js";
import { loadAttachedStackConfig } from "../src/full-stack/attached-config.js";
import { configureStackCommittee } from "../src/full-stack/committee.js";
import type { StackConfig } from "../src/full-stack/config.js";
import { generateDaServices } from "../src/full-stack/da-services.js";
import { StackProcesses } from "../src/full-stack/process.js";
import { preparePublicImages } from "../src/full-stack/public-runtime.js";
import { runStackWorkflow } from "../src/full-stack/workflow.js";
import { SIGNED_RELEASE_FILES } from "../src/sign-deployment-release.js";
import {
  configFixture,
  removeConfigFixtures,
} from "./full-stack-config.fixtures.js";
import {
  type RecordedCall,
  RecordingProcesses,
} from "./full-stack-fixtures.js";
import { publicFixture, tls } from "./full-stack-public-config.fixtures.js";

/** The checkout's node directory, which holds the base Compose files. */
const checkoutNodeRoot = fileURLToPath(
  new URL("../../midgard-node/", import.meta.url),
);

const servers: Server[] = [];
/** Closes every fixture server and removes every fixture; call it after each test. */
export async function removeAttachedFixtures() {
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
  await removeConfigFixtures();
}

/**
 * Operator-2's node API on loopback: unready until its container starts. A
 * public stack's second instance serves HTTPS with the stack's certificate.
 */
async function nodeApi(https: boolean, reserved: ReadonlySet<number>) {
  const state = { started: false };
  const handler = (
    request: { url?: string },
    response: { statusCode: number; end: (body: string) => void },
  ) => {
    const ready =
      request.url === "/readyz" && state.started
        ? { ready: true, reasons: [], settlement: { state: "waiting" } }
        : undefined;
    response.statusCode = ready === undefined ? 503 : 200;
    response.end(JSON.stringify(ready ?? { ready: false }));
  };
  const server = https
    ? createHttpsServer(
        {
          cert: await readFile(join(tls.directory, "node.pem")),
          key: await readFile(join(tls.directory, "node.key")),
        },
        handler,
      )
    : createServer(handler);
  servers.push(server);
  // An OS-assigned port can land on one the stack reserves for its DA
  // services, which the stack's own port check then refuses. Listen again
  // until the port is free of them.
  for (;;) {
    await new Promise<void>((resolve) =>
      server.listen(0, "127.0.0.1", resolve),
    );
    const { port } = server.address() as AddressInfo;
    if (!reserved.has(port)) return { state, port };
    await new Promise<void>((resolve) => server.close(() => resolve()));
  }
}

/** The stack's release settings, which a public stack's second instance runs. */
export const SHARED_RELEASE = [
  "MIDGARD_PUBLIC_API_ORIGIN",
  "MIDGARD_PUBLIC_RELEASE_REVISION",
  "MIDGARD_PUBLIC_NODE_IMAGE",
  "MIDGARD_PUBLIC_POSTGRES_IMAGE",
  "MIDGARD_PUBLIC_INGRESS_IMAGE",
];

export const OPERATOR2_TRANSPORT = "STACK_OPERATOR2_DA_TRANSPORT";
const OPERATOR2_DA_HOST_PORT = 39012;
export const NODE_WALLETS = {
  operator: "L1_OPERATOR_SEED_PHRASE",
  merge: "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  reference: "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  settlement: "L1_SETTLEMENT_SEED_PHRASE",
};

/**
 * The stack's deployment, initialized for its committee, its L1 origin and
 * the producer manifests its runtime configuration generated.
 */
async function deployStack(
  primary: { config: StackConfig; directory: string },
  primaryEnv: Record<string, string>,
  publicManifest: Record<string, unknown> | undefined,
) {
  const committee = await configureStackCommittee(primary.config, primaryEnv);
  const forCommittee = (raw: Record<string, unknown>) => {
    const da = raw.da as {
      committeeVkeys: string[];
      committeeSignersHash: string;
      threshold: number;
    };
    da.committeeVkeys = committee.map((member) => member.daVkey);
    da.committeeSignersHash = computeDeploymentManifestDaCommitteeSignersHash(
      da.committeeVkeys,
    );
    da.threshold = 2;
  };
  const deployment =
    publicManifest === undefined
      ? await writeFinalizedDeploymentInfo(forCommittee)
      : await refinalize(publicManifest, forCommittee, primary.directory);
  const manifest = JSON.parse(await readFile(deployment.path, "utf8")) as {
    referenceScriptDeployAddress: string;
    hubOracleOneShot: { txHash: string; outputIndex: number };
  };
  const primaryManifest = join(
    primary.directory,
    "deploymentInfo/contract-deployment-info.json",
  );
  await mkdir(join(primary.directory, "deploymentInfo"));
  await copyFile(deployment.path, primaryManifest);
  // The stack's origin step recorded its L1 origin for the deployment's nonce.
  const origin = { slot: 7, blockHash: "cd".repeat(32) };
  await mkdir(primary.config.runDirectory, { recursive: true });
  await writeFile(
    join(primary.config.runDirectory, "l1-origin.json"),
    JSON.stringify({
      origin,
      nonceTxHash: manifest.hubOracleOneShot.txHash,
      nonceBlock: { slot: 8, blockHash: "ef".repeat(32) },
    }),
  );
  await generateDaServices(
    new StackProcesses(primary.config, {
      ...primaryEnv,
      L1_ORIGIN: formatL1Origin(origin),
    }),
  );
  return { primaryManifest, manifest, origin };
}

/** A finalized manifest, changed and finalized again under its new identity. */
async function refinalize(
  manifest: Record<string, unknown>,
  change: (raw: Record<string, unknown>) => void,
  directory: string,
) {
  const raw = structuredClone(manifest);
  change(raw);
  delete raw.manifestId;
  raw.manifestId = computeDeploymentManifestId(
    raw as unknown as Parameters<typeof computeDeploymentManifestId>[0],
  );
  const path = join(directory, "finalized-manifest.json");
  await writeFile(path, `${JSON.stringify(raw, null, 2)}\n`);
  return { path };
}

/**
 * A stack of the compiled production profile (after createPublicTls) whose
 * public release is signed, as a run leaves it.
 */
async function publicStackFixture() {
  const value = await publicFixture(
    `${SELECTED_DEPLOYMENT_PROFILE.name}-stack.example.json`,
  );
  await writeFile(value.config.envFile, value.envText());
  const release = value.config.public!.releaseDirectory;
  await mkdir(release);
  for (const name of Object.values(SIGNED_RELEASE_FILES))
    await writeFile(join(release, name), "{}");
  return value;
}

/**
 * A running preprod-testing stack (its deployment, L1 origin and generated
 * producer manifests on disk) and operator-2's checkout beside it, with its
 * own node wallets, DA identity, ports and Compose project.
 */
export async function attachedFixture(
  options: {
    admitted?: boolean;
    /**
     * Makes the stack a public one of the compiled production profile. With
     * a genuine finalized manifest it is deployed with it (a production build
     * builds manifests only from the real blueprint); without one, only its
     * configuration and signed release exist, which loading needs.
     */
    public?: { manifest?: Record<string, unknown> };
  } = {},
) {
  const isPublic = options.public !== undefined;
  const primary = isPublic ? await publicStackFixture() : await configFixture();
  const operator2 = await configFixture();
  const daPorts = primary.config.da.ports;
  const api = await nodeApi(
    isPublic,
    new Set([
      daPorts.database,
      daPorts.retainedTransport,
      ...primary.config.da.members.flatMap((_, index) => [
        daPorts.committeeApiBase + index,
        daPorts.committeeTransportBase + index,
      ]),
    ]),
  );
  const { peerId } = await loadDaLibp2pIdentity(`seed:${"0b".repeat(32)}`);
  const admittedProducer = `/ip4/127.0.0.1/tcp/${OPERATOR2_DA_HOST_PORT}/p2p/${peerId}`;
  if (options.admitted !== false)
    primary.config.da.admittedProducers = [admittedProducer];
  await primary.writeConfig();
  // A public stack's run holds its node secrets from its node configuration.
  const primaryEnv = {
    ...primary.env,
    ...("secrets" in primary
      ? (primary.secrets as Record<string, string>)
      : {}),
  };
  const deployed =
    isPublic && options.public?.manifest === undefined
      ? undefined
      : await deployStack(primary, primaryEnv, options.public?.manifest);
  // Operator-2's checkout: its own node environment.
  const env: Record<string, string> = {
    NETWORK: "Preprod",
    MIDGARD_DEPLOYMENT_PROFILE: "preprod-testing",
    RUN_GENESIS_ON_STARTUP: "false",
    MIN_FEE_A: "10",
    MIN_FEE_B: "10",
    COMPOSE_PROJECT_NAME: "midgard-node-operator2",
    MIDGARD_NODE_API_HOST_PORT: String(api.port),
    MIDGARD_NODE_METRICS_HOST_PORT: "9564",
    MIDGARD_NODE_DA_HOST_PORT: String(OPERATOR2_DA_HOST_PORT),
    MIDGARD_POSTGRES_HOST_PORT: "25533",
    POSTGRES_USER: "midgard",
    POSTGRES_PASSWORD: "operator2-test-password",
    POSTGRES_DB: "midgard",
    [OPERATOR2_TRANSPORT]: `seed:${"0b".repeat(32)}`,
  };
  for (const [index, seedEnv] of Object.values(NODE_WALLETS).entries())
    env[seedEnv] = entropyToMnemonic(
      (index + 0x20).toString(16).padStart(32, "0"),
    );
  // A public stack's second instance runs the stack's production profile and
  // keeps its node secrets in its own private node configuration.
  const nodeConfigFile = join(operator2.directory, "logs/private/node.yaml");
  if (isPublic) {
    Object.assign(env, {
      MIDGARD_DEPLOYMENT_PROFILE: primary.env.MIDGARD_DEPLOYMENT_PROFILE!,
      VALIDATION_WORKER_POOL_SIZE: "4",
      ADMIN_API_KEY: randomBytes(32).toString("hex"),
      POSTGRES_PASSWORD: randomBytes(18).toString("base64url"),
    });
    await mkdir(dirname(nodeConfigFile), { recursive: true });
    // The instance renders its TLS ingress with its node directory's generator.
    await mkdir(join(operator2.directory, "scripts"), { recursive: true });
    await copyFile(
      join(checkoutNodeRoot, "scripts/public-ingress.mjs"),
      join(operator2.directory, "scripts/public-ingress.mjs"),
    );
  }
  const secretNames: readonly string[] = PUBLIC_NODE_SECRET_NAMES;
  const isSecret = (key: string) => isPublic && secretNames.includes(key);
  const envFile = join(operator2.directory, ".env");
  const writeEnv = async () => {
    await writeFile(
      envFile,
      Object.entries(env)
        .filter(([key]) => !isSecret(key))
        .map(([key, value]) => `${key}=${value}`)
        .join("\n"),
    );
    if (isPublic)
      await writeFile(
        nodeConfigFile,
        Object.entries(env)
          .filter(([key]) => isSecret(key))
          .map(([key, value]) => `${key}: ${JSON.stringify(value)}`)
          .join("\n"),
        { mode: 0o600 },
      );
  };
  await writeEnv();
  const attached: Record<string, unknown> & {
    wallets: Record<string, { seedEnv: string; minimumLovelace: string }>;
  } = {
    attachTo: primary.path,
    nodeRoot: operator2.directory,
    envFile,
    runDirectory: join(operator2.directory, "logs/operator2"),
    endpoint: `${isPublic ? "https" : "http"}://127.0.0.1:${api.port}`,
    ...(isPublic ? { nodeConfigFile } : {}),
    timeoutMs: 1,
    wallets: Object.fromEntries(
      Object.entries(NODE_WALLETS).map(([role, seedEnv]) => [
        role,
        { seedEnv, minimumLovelace: "1000000000" },
      ]),
    ),
    producerTransportEnv: OPERATOR2_TRANSPORT,
  };
  const path = join(operator2.directory, "attached.json");
  const writeAttached = () => writeFile(path, JSON.stringify(attached));
  await writeAttached();
  return {
    primary,
    // Absent for a public stack loaded without a deployment.
    ...(deployed as NonNullable<typeof deployed>),
    operator2: operator2.directory,
    api,
    peerId,
    admittedProducer,
    env,
    writeEnv,
    nodeConfigFile,
    attached,
    writeAttached,
    path,
  };
}

export type AttachedFixture = Awaited<ReturnType<typeof attachedFixture>>;

/** `docker compose <args>` over a recorded Compose call's files and env file. */
function dockerCompose(
  call: RecordedCall,
  projectDirectory: string,
  args: string[],
) {
  const files = call.args.flatMap((arg, index) =>
    arg === "-f" ? [call.args[index + 1]!] : [],
  );
  const envFile = call.args[call.args.indexOf("--env-file") + 1]!;
  return promisify(execFile)(
    "docker",
    [
      "compose",
      "--project-directory",
      projectDirectory,
      "--env-file",
      envFile,
      ...files.flatMap((file) => [
        "-f",
        isAbsolute(file) ? file : join(checkoutNodeRoot, file),
      ]),
      ...args,
    ],
    { env: { PATH: process.env.PATH!, HOME: process.env.HOME! } },
  );
}

/** The services and environment Docker would run for a recorded Compose call. */
async function renderCompose(call: RecordedCall, projectDirectory: string) {
  const { stdout } = await dockerCompose(call, projectDirectory, [
    "config",
    "--format",
    "json",
  ]);
  return JSON.parse(stdout) as {
    services: Record<
      string,
      {
        image?: string;
        depends_on?: Record<string, unknown>;
        ports?: { host_ip?: string; target: number }[];
        volumes?: { source: string; target: string; read_only?: boolean }[];
        environment?: Record<string, string>;
        networks?: Record<string, { aliases?: string[] } | null>;
      }
    >;
    networks?: Record<string, { name: string; external?: boolean }>;
  };
}

/** Compose's configuration hash of one service for a recorded Compose call: `up -d` recreates its container exactly when this changes. */
export async function composeServiceHash(
  call: RecordedCall,
  projectDirectory: string,
  service: string,
) {
  const { stdout } = await dockerCompose(call, projectDirectory, [
    "config",
    "--hash",
    service,
  ]);
  const [name, hash] = stdout.trim().split(/\s+/u);
  if (name !== service || hash === undefined)
    throw new Error(`Unexpected compose hash output ${stdout}`);
  return hash;
}

/** The stack's Postgres as operator-2's controller reads it. */
function fakePostgres() {
  let identity: { run_id: string; manifest_id: string } | null = null;
  return (call: RecordedCall) => {
    const query = call.args.at(-1)!;
    const created =
      /VALUES \(true,'([^']+)','([^']+)'\)/u.exec(query) ?? undefined;
    if (created !== undefined && identity === null)
      identity = { run_id: created[1]!, manifest_id: created[2]! };
    if (query.includes("to_regclass")) return { exists: identity !== null };
    if (query.includes("full_stack_controller_identity")) return identity;
    return { empty: true };
  };
}

export async function attachedRun() {
  const fixture = await attachedFixture();
  return runAttached(fixture, await loadAttachedStackConfig(fixture.path));
}

/** Operator-2's workflow, recorded, over a loaded second-instance configuration. */
export async function runAttached(
  fixture: AttachedFixture,
  loaded: {
    config: StackConfig;
    env: Record<string, string>;
    intentDigest: string;
  },
) {
  const { config, env, intentDigest } = loaded;
  const processes = new RecordingProcesses(config, env);
  // The prerequisites pull a public stack's release images for its Compose files.
  if (config.public !== undefined) await preparePublicImages(processes);
  const operator = { state: "none" };
  let rendered: Awaited<ReturnType<typeof renderCompose>> | undefined;
  Object.assign(processes.responses, {
    "provider-preflight": { ok: true },
    "storage-identity": fakePostgres(),
    ...Object.fromEntries(
      Object.keys(NODE_WALLETS).map((role) => [
        `wallet-${role}`,
        {
          totals: { lovelace: "2000000000" },
          utxos: [
            {
              assets: { lovelace: "2000000000" },
              inlineDatum: null,
              referenceScriptHash: null,
              dataHash: null,
            },
          ],
        },
      ]),
    ),
    "deployment-status": {
      manifest: { ok: true },
      protocol: { status: "complete", nonce: "spent", hubOracleWitness: null },
    },
    "operator-observation": () => operator,
    "operator-register-or-resume": () => {
      operator.state = "active";
      return { registered: true };
    },
    "native-owner-pin": { sha: "c".repeat(64) },
    "runtime-start": async (call: RecordedCall) => {
      rendered = await renderCompose(call, fixture.operator2);
      fixture.api.state.started = true;
    },
  });
  const run = () =>
    runStackWorkflow(
      { directory: config.runDirectory, intentDigest },
      attachedSteps(processes),
    );
  return { fixture, processes, run, rendered: () => rendered! };
}
