import { execFile } from "node:child_process";
import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import {
  createServer,
  type IncomingMessage,
  type ServerResponse,
} from "node:http";
import { createServer as createHttpsServer } from "node:https";
import type { AddressInfo, Server } from "node:net";
import { tmpdir } from "node:os";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs";
import {
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import type { DaPayloadsDB } from "midgard-node/database/index";
import { parseManifestFixture } from "midgard-node/tests/da-payload-libp2p-producer.manifest-fixture";
import { makeWatcherDeploymentAuthorityFixture } from "midgard-watcher/tests/support/deployment-authority-fixture";
import { openWatcherWorkflowStoreForTest } from "midgard-watcher/tests/support/watcher-workflow-store";
import { expect } from "vitest";

import type { OperatorNodeControl } from "./operator-pause.js";
import type { StackJourneyContext } from "./stack-context.js";
import { stackDaProducer } from "./stack-da-producer.js";
import {
  WATCHER_CONTAINER_PROCESS_CONFIG_PATH,
  type WatcherEvidenceExec,
} from "./watcher-evidence-reader.js";

const watcherImage = fileURLToPath(
  new URL("../../../midgard-watcher/", import.meta.url),
);
/**
 * The watcher container: its Node, run locally in the watcher package, with
 * `mountedProcessConfig` mounted at the container's process configuration path.
 */
export const watcherContainer =
  (mountedProcessConfig: string): WatcherEvidenceExec =>
  ({ script, request }) => {
    const parsed = JSON.parse(request) as { processConfigPath: string };
    expect(parsed.processConfigPath).toBe(
      WATCHER_CONTAINER_PROCESS_CONFIG_PATH,
    );
    return new Promise((resolve, reject) => {
      execFile(
        process.execPath,
        [
          "--input-type=module",
          "-e",
          script,
          JSON.stringify({
            ...parsed,
            processConfigPath: mountedProcessConfig,
          }),
        ],
        { cwd: watcherImage, encoding: "utf8" },
        (error, stdout, stderr) =>
          error === null ? resolve(stdout) : reject(new Error(stderr)),
      );
    });
  };

const authorityFixture = makeWatcherDeploymentAuthorityFixture();
export const deploymentIdentity = authorityFixture.result;
export const fingerprint = deploymentIdentity.manifestId;

const directories: string[] = [];
const servers: Server[] = [];
/** Removes every stack's state; for `afterAll`. */
export const removeStackDirectories = () =>
  Promise.all(
    directories.map((path) => rm(path, { recursive: true, force: true })),
  );
/** Stops every stack watcher's operations endpoint. */
export const closeStackWatchers = async () => {
  await Promise.all(
    servers
      .splice(0)
      .map((server) => new Promise((resolve) => server.close(resolve))),
  );
};

/** The stack's Shelley genesis cadence, as its node configuration declares it. */
export const STACK_GENESIS = Object.freeze({
  networkMagic: 424242,
  slotLength: 0.2,
  activeSlotsCoeff: 0.5,
});

/** The stack configuration's prover funding minimum, in lovelace. */
export const PROVER_MINIMUM_LOVELACE = 3_000_000_000n;

/**
 * A public stack's TLS: the CA its configuration names, and the certificate
 * for the public origin's hostname that its HTTPS ports serve on loopback.
 */
export type StackTls = Readonly<{
  caFile: string;
  certFile: string;
  keyFile: string;
  servername: string;
}>;

/**
 * A running stack watcher: its operations endpoint and its mounted state.
 * Its prover holds `proverLovelace`, by default the configured minimum.
 */
export const runningStackWatcher = async (
  status: Record<string, unknown>,
  {
    proverLovelace = PROVER_MINIMUM_LOVELACE,
    tls,
  }: {
    proverLovelace?: bigint;
    /** A public stack's TLS: our node then serves its API over HTTPS only. */
    tls?: StackTls;
  } = {},
) => {
  const root = await mkdtemp(join(tmpdir(), "attached-session-"));
  directories.push(root);
  const storePath = join(root, "watcher.sqlite");
  const processConfigPath = join(root, "watcher-process.json");
  await writeFile(
    processConfigPath,
    JSON.stringify({
      workflowJournalDirectory: join(root, "workflows"),
      watcherConfig: { storage: { path: storePath } },
    }),
  );
  const served: { status: Record<string, unknown>; nodeReadyz: unknown } = {
    status,
    nodeReadyz: { ready: true, reasons: [] },
  };
  const requests: string[] = [];
  const server = createServer((request, response) => {
    requests.push(request.url ?? "");
    const body =
      request.url === "/v1/status"
        ? served.status
        : request.url === "/v1/metrics"
          ? { queuedProofCount: "0" }
          : undefined;
    response.statusCode = body === undefined ? 404 : 200;
    response.end(JSON.stringify(body ?? {}));
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  servers.push(server);
  const operationsEndpoint = `http://127.0.0.1:${(server.address() as AddressInfo).port}`;
  // Our node's /readyz, as `served.nodeReadyz` answers it.
  const nodeRequests: string[] = [];
  const answerNode = (request: IncomingMessage, response: ServerResponse) => {
    nodeRequests.push(request.url ?? "");
    response.statusCode = request.url === "/readyz" ? 200 : 404;
    response.end(
      JSON.stringify(request.url === "/readyz" ? served.nodeReadyz : {}),
    );
  };
  const node =
    tls === undefined
      ? createServer(answerNode)
      : createHttpsServer(
          {
            cert: await readFile(tls.certFile),
            key: await readFile(tls.keyFile),
          },
          answerNode,
        );
  await new Promise<void>((resolve) => node.listen(0, "127.0.0.1", resolve));
  servers.push(node);
  const nodeEndpoint = `${tls === undefined ? "http" : "https"}://127.0.0.1:${(node.address() as AddressInfo).port}`;
  const runDirectory = join(root, "run/fault-journeys/drill-1");
  // The stack node's configuration, its genesis, and the deployment manifest.
  const nodeConfigPath = join(root, "node/config.json");
  const manifestPath = join(root, "contract-deployment-info.json");
  await mkdir(join(root, "node/genesis"), { recursive: true });
  await writeFile(
    nodeConfigPath,
    JSON.stringify({ ShelleyGenesisFile: "genesis/shelley.json" }),
  );
  await writeFile(
    join(root, "node/genesis/shelley.json"),
    JSON.stringify(STACK_GENESIS),
  );
  await writeFile(
    manifestPath,
    JSON.stringify(authorityFixture.signedIdentity.manifest),
  );
  // Operator-2's wallet, which the drill signs with.
  const operator2Account = generateEmulatorAccount({ lovelace: 50_000_000n });
  // The stack watcher's prover, which the stack funds and the drill never does.
  const prover = generateEmulatorAccount({ lovelace: proverLovelace });
  const emulator = new Emulator([operator2Account, prover]);
  const operatorLucid = await Lucid(emulator, "Custom");
  operatorLucid.selectWallet.fromSeed(operator2Account.seedPhrase);
  // Operator-2's DA producer, whose transport the test never opens.
  const producerTransportOpened = "operator-2's DA producer transport opened";
  let producerTransportOpens = 0;
  // The parts of a stack context an attached session reads.
  const context = {
    mode: "stack",
    runDirectory,
    layout: {
      manifestPath,
      node: { nodeConfigPath },
      proverAddress: prover.address,
    },
    provider: emulator,
    kupoUrl: "http://127.0.0.1:1442",
    ogmiosUrl: "ws://127.0.0.1:1337",
    release: { deploymentIdentity },
    deployment: {
      manifest: { manifestId: fingerprint },
      blueprintPath: join(root, "plutus.json"),
      operatorLucid,
    },
    // A public stack's host tools trust its CA for the public origin's name.
    ...(tls === undefined
      ? {}
      : { nodeTls: { caFile: tls.caFile, servername: tls.servername } }),
    stackConfig: {
      endpoint: nodeEndpoint,
      ...(tls === undefined ? {} : { public: { tlsCaFile: tls.caFile } }),
      wallets: {
        prover: {
          seedEnv: "STACK_PROVER_SEED",
          minimumLovelace: PROVER_MINIMUM_LOVELACE.toString(),
        },
      },
    },
    watcher: {
      composeProject: "midgard-node-drill",
      processConfigPath,
      operationsEndpoint,
    },
    daProducer: stackDaProducer(parseManifestFixture(), async () => {
      producerTransportOpens += 1;
      throw new Error(producerTransportOpened);
    }),
  } as unknown as StackJourneyContext;
  const operator2 = stoppableOperatorNode();
  return {
    root,
    storePath,
    runDirectory,
    context,
    proverAddress: prover.address,
    served,
    requests,
    nodeRequests,
    /** A transaction operator-2's wallet signs, paying itself. */
    signAsOperator2: async () =>
      (
        await operatorLucid
          .newTx()
          .pay.ToAddress(operator2Account.address, { lovelace: 2_000_000n })
          .complete()
      ).sign
        .withWallet()
        .complete(),
    /**
     * A publication as operator-2's DA producer. Its transport is never
     * really opened: opening it throws `producerTransportOpened`.
     */
    publishAsOperator2: () =>
      context.daProducer.publish({} as DaPayloadsDB.InsertInput),
    producerTransportOpened,
    producerTransportOpens: () => producerTransportOpens,
    operator2,
    exec: watcherContainer(processConfigPath),
    /** How the journey opens a session on this stack, with operator-2's node. */
    options: (exec = watcherContainer(processConfigPath)) => ({
      exec,
      operator2: {
        composeProject: "midgard-node-op2",
        endpoint: "http://127.0.0.1:3100",
      },
      operator2Control: operator2.control,
    }),
  };
};

/** Operator-2's node as its Compose service: stop and start flip it. */
export const stoppableOperatorNode = () => {
  const calls: string[] = [];
  let running = true;
  const control: OperatorNodeControl = {
    name: "midgard-node-op2/midgard-node",
    stop: () => {
      calls.push("stop");
      running = false;
      return Promise.resolve();
    },
    start: () => {
      calls.push("start");
      running = true;
      return Promise.resolve();
    },
    runningServices: () => Promise.resolve(running ? ["midgard-node"] : []),
    readReadiness: () =>
      Promise.resolve(
        running
          ? {
              reachable: true,
              httpStatus: 200,
              body: { ready: true, reasons: [] },
            }
          : { reachable: false },
      ),
  };
  return { control, calls, running: () => running };
};

export const liveStatus = () => ({
  deploymentFingerprint: fingerprint,
  liveness: "live",
  readiness: "not_ready",
  readinessReasons: ["l1_follower_catching_up"],
  l1Readiness: [{ reason: "l1_follower_catching_up", detail: "block 7" }],
});

/** A `started` row of a doubleSpend workflow on `headerHash`, in `storePath`. */
export const seedWorkflowStarted = async (
  storePath: string,
  headerHash: string,
) => {
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: fingerprint,
    category: "doubleSpend" as const,
    target: { kind: "state_queue_header" as const, headerHash },
  };
  const workflows = openWatcherWorkflowStoreForTest(storePath);
  await workflows.forObjective({ category: "doubleSpend", headerHash }).append(
    {
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId: computeFraudProofWorkflowId(identity),
      identity,
      sequence: 0,
      recordedAt: "2026-10-29T00:00:00.000Z",
      event: { kind: "started" },
    },
    0,
  );
  workflows.close();
};
