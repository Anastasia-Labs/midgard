import { createHash } from "node:crypto";
import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { assertNodeFollowerEnvironment } from "../l1-origin.js";
import { mountedFilesLabels } from "./compose-yaml.js";
import {
  generateDaServices,
  stackDaReaderGrantSql,
  writePrivateEnv,
} from "./da-services.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { containerNativeLedger } from "./native-ledger.js";
import { pinNativeOwner, preflightProducerTransport } from "./node-start.js";
import { getJson, getReadiness, poll, type StackProcesses } from "./process.js";
import {
  publicNodeEnvironment,
  publicNodeServices,
  publicReleaseStep,
  publicRuntimeInputs,
  writePublicNodeConfig,
} from "./public-runtime.js";
import {
  committeeIsReady,
  committeeStopReason,
  stackIsReady,
} from "./readiness.js";
import { storageIdentityStep } from "./storage.js";
import { generateWatcherServices } from "./watcher-services.js";
import type { StackStep } from "./workflow.js";

type RuntimeConfiguration = {
  inputDigest: string;
  compose: string;
  operationsEndpoint: string;
  committeeServices: string[];
  /** A preprod-public stack's extra services, such as its TLS ingress. */
  publicServices?: string[];
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
    ...publicRuntimeInputs(config),
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
export const servicesDirectory = (processes: StackProcesses) =>
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
export function nodeEnvironment(
  processes: StackProcesses,
  ownerSha256: string,
) {
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
/**
 * The testing node's Compose services in the generated services file: the
 * locally built node and its migration, reading node.env, with the producer
 * manifest and the in-stack Cardano node's socket and configuration.
 */
export async function stackNodeServices(
  processes: StackProcesses,
  input: { envFile: unknown; producer: string },
) {
  const { volumes } = containerNativeLedger(processes);
  return {
    "midgard-node": {
      env_file: input.envFile,
      labels: await mountedFilesLabels([input.producer]),
      volumes: [`${input.producer}:/app/stack/producer.json:ro`, ...volumes],
    },
    "midgard-node-migrate": { env_file: input.envFile, volumes },
  };
}
/** One read of every service's readiness; undefined while any is not ready. */
async function runtimeReadinessReader(processes: StackProcesses) {
  const runtime = await readRuntimeConfiguration(processes);
  const committee = await committeeExpectation(processes);
  const { manifestId } = (await readJsonIfPresent(
    stackPaths(processes).manifest,
  )) as { manifestId: string };
  return async () => {
    // An unready node answers 503 and still says why (see nodeIsReady).
    const node = (await getReadiness(`${processes.config.endpoint}/readyz`)) as
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
    // A preprod-public node starts only with a release signed for this
    // deployment; the runtime configuration binds the signed files.
    ...(processes.config.public === undefined
      ? []
      : [publicReleaseStep(processes)]),
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
        // The services step writes node.env once, with the owner pin of the image it built.
        const nodeEnvFile = [{ path: nodeEnvPath, required: false }];
        const compose = join(da.directory, "compose.json");
        // A preprod-public node runs its release image with mounted secrets and TLS.
        const publicNode =
          processes.config.public === undefined
            ? undefined
            : await publicNodeServices(processes, {
                servicesDirectory: da.directory,
                envFile: nodeEnvFile,
                producer: da.producer,
              });
        await writeDurableJson(compose, {
          services: {
            ...da.services,
            ...watcher.services,
            ...(publicNode?.services ??
              (await stackNodeServices(processes, {
                envFile: nodeEnvFile,
                producer: da.producer,
              }))),
          },
          volumes: {
            "da-postgres-data": {},
            ...da.volumes,
            ...watcher.volumes,
          },
          ...(publicNode === undefined
            ? {}
            : { networks: publicNode.networks }),
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
          ...(publicNode === undefined
            ? {}
            : { publicServices: publicNode.startServices }),
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
          // One snapshot confirms a running stack this run built without
          // restarting it; a stopped or unready one is started again, and only
          // execute waits. Services built by an earlier run or started from
          // other generated configuration are started again too, so Compose
          // recreates what changed and node.env is rewritten.
          try {
            const { inputDigest } = await readRuntimeConfiguration(processes);
            const saved = record.data as {
              inputDigest?: unknown;
              buildId?: unknown;
            };
            if (
              saved.inputDigest !== inputDigest ||
              saved.buildId !== processes.buildId
            )
              return { status: "retry" };
            const snapshot = await (await runtimeReadinessReader(processes))();
            if (snapshot !== undefined)
              return {
                status: "complete",
                data: { inputDigest, buildId: processes.buildId, ...snapshot },
              };
          } catch {
            // Missing or unreadable expectations restart the services too.
          }
          return { status: "retry" };
        }
        return { status: "retry" };
      },
      execute: async () => {
        const runtime = await readRuntimeConfiguration(processes);
        const isPublic = processes.config.public !== undefined;
        // A preprod-public node runs the release image the prerequisites pulled.
        await processes.compose(
          "services-build",
          [
            "build",
            ...(isPublic ? [] : ["midgard-node", "midgard-node-migrate"]),
            "da-committee-0",
            "watcher",
          ],
          runtime.compose,
        );
        // The pin below runs the node container, which mounts this file.
        if (isPublic)
          await writePublicNodeConfig(processes, servicesDirectory(processes));
        const ownerSha256 = await pinNativeOwner(
          processes,
          servicesDirectory(processes),
          runtime.compose,
        );
        const nodeEnv = nodeEnvironment(processes, ownerSha256);
        await writePrivateEnv(
          join(servicesDirectory(processes), "node.env"),
          isPublic ? await publicNodeEnvironment(processes, nodeEnv) : nodeEnv,
        );
        await processes.compose(
          "da-database-start",
          ["up", "-d", "--wait", "da-postgres"],
          runtime.compose,
        );
        // The committee checks its own submitter wallet once its L1 follower
        // has created its store and seeded the wallet; nothing reads those
        // facts before it runs.
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
                getReadiness(
                  `http://127.0.0.1:${processes.config.da.ports.committeeApiBase + index}/readyz`,
                ),
              ),
            );
            ready.forEach((value, index) => {
              const reason = committeeStopReason(value);
              if (reason !== undefined)
                throw new Error(
                  `${runtime.committeeServices[index]} cannot become ready by waiting: ${reason}, then run e2e-stack again`,
                );
            });
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
        await preflightProducerTransport(
          processes,
          servicesDirectory(processes),
          runtime.compose,
        );
        await processes.compose(
          "runtime-start",
          [
            "up",
            "-d",
            "midgard-node",
            "watcher",
            ...(runtime.publicServices ?? []),
          ],
          runtime.compose,
        );
        return {
          inputDigest: runtime.inputDigest,
          buildId: processes.buildId,
          ...(await confirmRuntimeReadiness(processes)),
        };
      },
    },
  ];
}
