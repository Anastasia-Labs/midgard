import { execFile } from "node:child_process";
import { copyFile, mkdir, readFile, writeFile } from "node:fs/promises";
import { createServer, type Server } from "node:http";
import type { AddressInfo } from "node:net";
import { isAbsolute, join } from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import { formatL1Origin } from "@al-ft/midgard-core/l1-origin";
import { entropyToMnemonic } from "bip39";
import { computeDeploymentManifestDaCommitteeSignersHash } from "midgard-node/deployment-manifest";
import { writeFinalizedDeploymentInfo } from "midgard-node/tests/da-libp2p-runtime-manifest.write-finalized-deployment-info";

import { attachedSteps } from "../src/full-stack/attached.js";
import { loadAttachedStackConfig } from "../src/full-stack/attached-config.js";
import { configureStackCommittee } from "../src/full-stack/committee.js";
import { generateDaServices } from "../src/full-stack/da-services.js";
import { StackProcesses } from "../src/full-stack/process.js";
import { runStackWorkflow } from "../src/full-stack/workflow.js";
import {
  configFixture,
  removeConfigFixtures,
} from "./full-stack-config.fixtures.js";
import {
  type RecordedCall,
  RecordingProcesses,
} from "./full-stack-fixtures.js";

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

/** Operator-2's node API on loopback: unready until its container starts. */
async function nodeApi() {
  const state = { started: false };
  const server = createServer((request, response) => {
    const ready =
      request.url === "/readyz" && state.started
        ? { ready: true, reasons: [], settlement: { state: "waiting" } }
        : undefined;
    response.statusCode = ready === undefined ? 503 : 200;
    response.end(JSON.stringify(ready ?? { ready: false }));
  });
  servers.push(server);
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  return { state, port: (server.address() as AddressInfo).port };
}

export const OPERATOR2_TRANSPORT = "STACK_OPERATOR2_DA_TRANSPORT";
const OPERATOR2_DA_HOST_PORT = 39012;
export const NODE_WALLETS = {
  operator: "L1_OPERATOR_SEED_PHRASE",
  merge: "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  reference: "L1_REFERENCE_SCRIPT_SEED_PHRASE",
  settlement: "L1_SETTLEMENT_SEED_PHRASE",
};

/**
 * A running preprod-testing stack (its deployment, L1 origin and generated
 * producer manifests on disk) and operator-2's checkout beside it, with its
 * own node wallets, DA identity, ports and Compose project.
 */
export async function attachedFixture(options: { admitted?: boolean } = {}) {
  const primary = await configFixture();
  const operator2 = await configFixture();
  const api = await nodeApi();
  const { peerId } = await loadDaLibp2pIdentity(`seed:${"0b".repeat(32)}`);
  const admittedProducer = `/ip4/127.0.0.1/tcp/${OPERATOR2_DA_HOST_PORT}/p2p/${peerId}`;
  if (options.admitted !== false)
    primary.config.da.admittedProducers = [admittedProducer];
  await primary.writeConfig();
  // The stack's deployment, initialized for its committee.
  const committee = await configureStackCommittee(primary.config, primary.env);
  const deployment = await writeFinalizedDeploymentInfo((raw) => {
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
  });
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
  // Its runtime configuration generated the producer manifests.
  await generateDaServices(
    new StackProcesses(primary.config, {
      ...primary.env,
      L1_ORIGIN: formatL1Origin(origin),
    }),
  );
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
  const envFile = join(operator2.directory, ".env");
  const writeEnv = () =>
    writeFile(
      envFile,
      Object.entries(env)
        .map(([key, value]) => `${key}=${value}`)
        .join("\n"),
    );
  await writeEnv();
  const attached = {
    attachTo: primary.path,
    nodeRoot: operator2.directory,
    envFile,
    runDirectory: join(operator2.directory, "logs/operator2"),
    endpoint: `http://127.0.0.1:${api.port}`,
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
    primaryManifest,
    manifest,
    origin,
    operator2: operator2.directory,
    api,
    peerId,
    admittedProducer,
    env,
    writeEnv,
    attached,
    writeAttached,
    path,
  };
}

/** The services and environment Docker would run for a recorded Compose call. */
async function renderCompose(call: RecordedCall, projectDirectory: string) {
  const files = call.args.flatMap((arg, index) =>
    arg === "-f" ? [call.args[index + 1]!] : [],
  );
  const envFile = call.args[call.args.indexOf("--env-file") + 1]!;
  const { stdout } = await promisify(execFile)(
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
      "config",
      "--format",
      "json",
    ],
    { env: { PATH: process.env.PATH!, HOME: process.env.HOME! } },
  );
  return JSON.parse(stdout) as {
    services: Record<
      string,
      {
        depends_on?: Record<string, unknown>;
        volumes?: { source: string; target: string }[];
      }
    >;
  };
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
  const { config, env, intentDigest } = await loadAttachedStackConfig(
    fixture.path,
  );
  const processes = new RecordingProcesses(config, env);
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
      protocol: { complete: true, empty: false },
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
