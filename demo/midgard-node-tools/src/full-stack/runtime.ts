import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { assertNodeFollowerEnvironment } from "../l1-origin.js";
import {
  generateDaServices,
  stackDaReaderGrantSql,
  writePrivateEnv,
} from "./da-services.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { containerNativeLedger } from "./native-ledger.js";
import { getJson, poll, type StackProcesses } from "./process.js";
import { committeeIsReady, stackIsReady } from "./readiness.js";
import { storageIdentityStep } from "./storage.js";
import { generateWatcherServices } from "./watcher-services.js";
import type { StackStep } from "./workflow.js";

type RuntimeConfiguration = {
  inputDigest: string;
  compose: string;
  operationsEndpoint: string;
  committeeServices: string[];
};
export const runtimeConfigurationPath = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "runtime.json");
export async function readRuntimeConfiguration(processes: StackProcesses) {
  const value = (await readJsonIfPresent(
    runtimeConfigurationPath(processes),
  )) as RuntimeConfiguration;
  if (
    !value?.compose ||
    !value.operationsEndpoint ||
    !Array.isArray(value.committeeServices)
  )
    throw new Error("Runtime configuration is missing");
  return value;
}
/**
 * Digest of what generation reads besides the deployment: the stack
 * configuration and the files it names. Ports and templates may change between
 * runs without changing the run intent. Release inputs are included so that
 * changed measurements are checked against the saved release again.
 */
export async function runtimeInputDigest(processes: StackProcesses) {
  const { config } = processes;
  const hash = createHash("sha256").update(JSON.stringify(config));
  // The committee environments carry the run's origin.
  hash.update(`\0L1_ORIGIN\0${processes.env.L1_ORIGIN ?? ""}`);
  for (const path of [
    config.envFile,
    config.watcher.composeEnvFile,
    config.watcher.processTemplate,
    ...(config.watcher.releaseInput ? [config.watcher.releaseInput] : []),
  ]) {
    const bytes = await readFile(path).catch((error: NodeJS.ErrnoException) => {
      if (error.code === "ENOENT") return Buffer.from("absent");
      throw error;
    });
    hash
      .update(`\0${path}\0`)
      .update(createHash("sha256").update(bytes).digest("hex"));
  }
  return hash.digest("hex");
}
const servicesDirectory = (processes: StackProcesses) =>
  join(processes.config.runDirectory, "services");
/** Host node commands use the generated producer manifest, also after a resume skips generation. */
export function restoreRuntimeEnvironment(processes: StackProcesses) {
  processes.env.MIDGARD_DEPLOYMENT_MANIFEST_PATH = join(
    servicesDirectory(processes),
    "producer.json",
  );
}
/** Committee peer ids by signer index, from the generated committee manifest. */
async function committeeExpectation(processes: StackProcesses) {
  const committee = (await readJsonIfPresent(
    join(servicesDirectory(processes), "committee-0.json"),
  )) as {
    deployment: { fingerprint: string };
    da_committee: { members: { signer_index: number; peer_id: string }[] };
  };
  const peerIds: string[] = [];
  for (const member of committee.da_committee.members)
    peerIds[member.signer_index] = member.peer_id;
  return { manifestId: committee.deployment.fingerprint, peerIds };
}
/**
 * The node's container environment. Every stack-only secret uses the STACK_
 * namespace (checkStackEnvironment), so blanking that namespace removes other
 * roles' secrets and never a node setting. Blank values also override the base
 * Compose file's own env_file. It refuses an environment that leaves the
 * node's L1 follower unconfigured.
 */
function nodeEnvironment(processes: StackProcesses, ownerSha256: string) {
  const { config, env } = processes;
  const nodeEnv = {
    ...Object.fromEntries(
      Object.entries(env).map(([key, value]) => [
        key,
        key.startsWith("STACK_") ? "" : value,
      ]),
    ),
    DA_LIBP2P_PRIVATE_KEY_SOURCE: env[config.da.producerTransportEnv]!,
    MIN_QUEUE_LENGTH_FOR_MERGING: "1",
    POSTGRES_HOST: "postgres",
    POSTGRES_PORT: "5432",
    MIDGARD_DEPLOYMENT_MANIFEST_PATH: "/app/stack/producer.json",
    MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH:
      "/app/deploymentInfo/contract-deployment-info.json",
    MPF_NATIVE_OWNER_BINARY_PATH: "/app/native/architecture-g-owner",
    ...containerNativeLedger(processes).env,
    MPF_NATIVE_OWNER_BINARY_SHA256: ownerSha256,
  };
  // The node's follower needs the origin and the nonce the deployment steps restored.
  assertNodeFollowerEnvironment(nodeEnv);
  return nodeEnv;
}
/** One read of every service's readiness; undefined while any is not ready. */
async function runtimeReadinessReader(processes: StackProcesses) {
  const runtime = await readRuntimeConfiguration(processes);
  const committee = await committeeExpectation(processes);
  const { manifestId } = (await readJsonIfPresent(
    stackPaths(processes).manifest,
  )) as { manifestId: string };
  return async () => {
    const node = (await getJson(`${processes.config.endpoint}/readyz`)) as
      | {
          ready?: boolean;
          reasons?: unknown[];
          settlement?: { state?: string };
        }
      | undefined;
    const watcher = (await getJson(
      `${runtime.operationsEndpoint}/v1/status`,
    )) as
      | {
          liveness?: string;
          readiness?: string;
          readinessReasons?: unknown[];
        }
      | undefined;
    const committees = await Promise.all(
      processes.config.da.members.map((_, index) =>
        getJson(
          `http://127.0.0.1:${processes.config.da.ports.committeeApiBase + index}/readyz`,
        ),
      ),
    );
    if (
      !stackIsReady({
        node,
        watcher,
        committees,
        manifestId,
        committeePeerIds: committee.peerIds,
      })
    )
      return undefined;
    return { node, watcher, committees };
  };
}
export async function confirmRuntimeReadiness(processes: StackProcesses) {
  return poll(
    "complete stack readiness",
    processes.config.timeoutMs,
    await runtimeReadinessReader(processes),
  );
}

export function runtimeSteps(processes: StackProcesses): StackStep[] {
  return [
    {
      id: "runtime-configuration",
      // Generation also reads inputs the intent does not bind, such as ports and
      // templates, so a saved configuration is reused only while their digest matches.
      reconcile: async (record) => {
        if (
          record?.status === "complete" ||
          (record?.status === "running" && record.data !== null)
        ) {
          const saved = (await readJsonIfPresent(
            runtimeConfigurationPath(processes),
          )) as Partial<RuntimeConfiguration> | undefined;
          if (saved?.inputDigest !== (await runtimeInputDigest(processes)))
            return { status: "retry" };
          restoreRuntimeEnvironment(processes);
          return { status: "complete", data: record.data };
        }
        return { status: "retry" };
      },
      execute: async () => {
        const inputDigest = await runtimeInputDigest(processes);
        const da = await generateDaServices(processes);
        const member = (await readJsonIfPresent(
          join(da.directory, "committee-0.json"),
        )) as { public_retained_da: { announce_multiaddrs: string[] } };
        const watcher = await generateWatcherServices(
          processes,
          member.public_retained_da.announce_multiaddrs[0]!,
        );
        const nodeEnvPath = join(da.directory, "node.env");
        const nativeLedger = containerNativeLedger(processes);
        // The services step writes node.env once, with the owner pin of the image it built.
        const nodeEnvFile = [{ path: nodeEnvPath, required: false }];
        const compose = join(da.directory, "compose.json");
        await writeDurableJson(compose, {
          services: {
            ...da.services,
            ...watcher.services,
            "midgard-node": {
              env_file: nodeEnvFile,
              volumes: [
                `${da.producer}:/app/stack/producer.json:ro`,
                ...nativeLedger.volumes,
              ],
            },
            "midgard-node-migrate": {
              env_file: nodeEnvFile,
              volumes: nativeLedger.volumes,
            },
          },
          volumes: {
            "da-postgres-data": {},
            ...da.volumes,
            ...watcher.volumes,
          },
        });
        await processes.compose(
          "compose-validate",
          ["config", "--quiet"],
          compose,
        );
        const configuration = {
          inputDigest,
          compose,
          operationsEndpoint: watcher.operationsEndpoint,
          committeeServices: processes.config.da.members.map(
            (_, index) => `da-committee-${index}`,
          ),
        };
        await writeDurableJson(
          runtimeConfigurationPath(processes),
          configuration,
        );
        restoreRuntimeEnvironment(processes);
        return configuration;
      },
    },
    storageIdentityStep(processes),
    {
      id: "services",
      reconcile: async (record) => {
        if (
          record?.status === "complete" ||
          (record?.status === "running" && record.data !== null)
        ) {
          // One snapshot confirms a running stack without rebuilding it; a
          // stopped or unready one is started again, and only execute waits.
          // Services started from other generated configuration are started
          // again too, so Compose recreates what changed and node.env is rewritten.
          try {
            const { inputDigest } = await readRuntimeConfiguration(processes);
            if (
              (record.data as { inputDigest?: unknown }).inputDigest !==
              inputDigest
            )
              return { status: "retry" };
            const snapshot = await (await runtimeReadinessReader(processes))();
            if (snapshot !== undefined)
              return { status: "complete", data: { inputDigest, ...snapshot } };
          } catch {
            // Missing or unreadable expectations restart the services too.
          }
          return { status: "retry" };
        }
        return { status: "retry" };
      },
      execute: async () => {
        const runtime = await readRuntimeConfiguration(processes);
        await processes.compose(
          "services-build",
          [
            "build",
            "midgard-node",
            "midgard-node-migrate",
            "da-committee-0",
            "watcher",
          ],
          runtime.compose,
        );
        // Pin the actual owner in the built image, never copy a hash from another checkout.
        const pin = (await processes.compose(
          "native-owner-pin",
          [
            "run",
            "--rm",
            "--no-deps",
            "--entrypoint",
            "node",
            "midgard-node",
            "-e",
            "console.log(JSON.stringify({sha:require('node:fs').readFileSync('/app/native/architecture-g-owner.sha256','utf8').trim()}))",
          ],
          runtime.compose,
        )) as { sha: string };
        if (!/^[0-9a-f]{64}$/.test(pin.sha))
          throw new Error("Invalid native owner image pin");
        await writePrivateEnv(
          join(servicesDirectory(processes), "node.env"),
          nodeEnvironment(processes, pin.sha),
        );
        await processes.compose(
          "da-database-start",
          ["up", "-d", "--wait", "da-postgres"],
          runtime.compose,
        );
        for (const service of runtime.committeeServices.slice(0, 1))
          await processes.compose(
            `${service}-wallet-preflight`,
            [
              "run",
              "--rm",
              "--no-deps",
              service,
              "dist/index.js",
              "l1-wallet-preflight",
              "--json",
            ],
            runtime.compose,
          );
        await processes.compose(
          "committee-start",
          ["up", "-d", ...runtime.committeeServices],
          runtime.compose,
        );
        const expected = await committeeExpectation(processes);
        await poll(
          "committee readiness",
          processes.config.timeoutMs,
          async () => {
            const ready = await Promise.all(
              runtime.committeeServices.map((_, index) =>
                getJson(
                  `http://127.0.0.1:${processes.config.da.ports.committeeApiBase + index}/readyz`,
                ),
              ),
            );
            return ready.every((value, index) =>
              committeeIsReady(value, index, expected),
            )
              ? ready
              : undefined;
          },
        );
        // The committee nodes have created their tables, so the public reader
        // can be granted exactly the two it serves, and only then started.
        // Signer indexes, and so the member databases, are 0..n-1.
        for (const signerIndex of processes.config.da.members.keys())
          await processes.compose(
            `public-reader-grant-${signerIndex}`,
            [
              "exec",
              "-T",
              "da-postgres",
              "psql",
              "-v",
              "ON_ERROR_STOP=1",
              "-U",
              "midgard_da_writer",
              "-d",
              `midgard_da_${signerIndex}`,
              "-c",
              stackDaReaderGrantSql(signerIndex),
            ],
            runtime.compose,
          );
        await processes.compose(
          "public-reader-start",
          ["up", "-d", "public-retained-da"],
          runtime.compose,
        );
        // Bind only before the producer container first starts. Docker holds its
        // port even while the node restarts or is not ready yet, so ask Compose.
        // `ps --format json` prints one object per container, and nothing when none match.
        const alreadyRunning =
          (await processes.compose(
            "producer-container-state",
            [
              "ps",
              "--format",
              "json",
              "--status",
              "running",
              "--status",
              "restarting",
              "midgard-node",
            ],
            runtime.compose,
          )) !== null;
        const hostManifest = join(
          processes.config.runDirectory,
          "services/producer-host.json",
        );
        await processes.node(
          "producer-da-preflight",
          [
            "da-libp2p-preflight",
            "--mode",
            alreadyRunning ? "dial-only" : "bind-listen",
            "--json",
          ],
          {
            MIDGARD_DEPLOYMENT_MANIFEST_PATH: hostManifest,
            DA_LIBP2P_PRIVATE_KEY_SOURCE:
              processes.env[processes.config.da.producerTransportEnv]!,
          },
        );
        await processes.compose(
          "runtime-start",
          ["up", "-d", "midgard-node", "watcher"],
          runtime.compose,
        );
        return {
          inputDigest: runtime.inputDigest,
          ...(await confirmRuntimeReadiness(processes)),
        };
      },
    },
  ];
}
