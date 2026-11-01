import { createHash } from "node:crypto";
import { mkdir, readFile } from "node:fs/promises";
import { join } from "node:path";

import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import {
  type DaLibp2pRuntimeManifest,
  parseDaLibp2pRuntimeManifest,
} from "@al-ft/midgard-core/da-transport";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { writeTextFileAtomic } from "midgard-node/files/atomic-write";

import { composeYaml, override } from "./compose-yaml.js";
import { writePrivateEnv } from "./da-services.js";
import {
  deploymentStatus,
  deploymentSteps,
  restoreDeploymentEnvironment,
  stackPaths,
} from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { shareWithContainers } from "./native-ledger.js";
import { pinNativeOwner, preflightProducerTransport } from "./node-start.js";
import { getJson, poll, type StackProcesses } from "./process.js";
import { nodeIsReady } from "./readiness.js";
import {
  nodeEnvironment,
  restoreRuntimeEnvironment,
  runtimeConfigurationPath,
  servicesDirectory,
  stackNodeServices,
} from "./runtime.js";
import { storageIdentityStep } from "./storage.js";
import type { StackStep } from "./workflow.js";

/**
 * A second instance on a running stack: another operator's node. It joins
 * the stack's deployment instead of deploying (no nonce, origin, reference
 * scripts or initialization), reads the stack's Cardano node, and runs only
 * its node and the node's Postgres, under its own Compose project.
 */
export function attachedSteps(processes: StackProcesses): StackStep[] {
  const shared = Object.fromEntries(
    deploymentSteps(processes).map((step) => [step.id, step]),
  );
  return [
    attachedProvidersStep(processes),
    shared.storage!,
    shared.wallets!,
    attachStep(processes),
    shared.operator!,
    attachedConfigurationStep(processes),
    storageIdentityStep(processes),
    attachedServicesStep(processes),
  ];
}

const primaryOf = (processes: StackProcesses) => {
  const primary = processes.config.attachedTo;
  if (primary === undefined) throw new Error("Not a second instance");
  return primary;
};
const primaryPaths = (processes: StackProcesses) => {
  const primary = primaryOf(processes);
  return {
    manifest: join(
      primary.nodeRoot,
      "deploymentInfo/contract-deployment-info.json",
    ),
    origin: join(primary.runDirectory, "l1-origin.json"),
    services: join(primary.runDirectory, "services"),
  };
};
const readIfPresent = (path: string) =>
  readFile(path).catch((error: NodeJS.ErrnoException) => {
    if (error.code === "ENOENT") return undefined;
    throw error;
  });

/** Its own Postgres; the Cardano node is the stack's, already running. */
function attachedProvidersStep(processes: StackProcesses): StackStep {
  const preflight = () =>
    processes.node("provider-preflight", ["l1-provider-preflight", "--json"]);
  return {
    id: "providers",
    reconcile: async (record) => {
      if (record?.status === "running" && record.data !== null) {
        const report = (await preflight()) as { ok?: boolean };
        if (report?.ok === true) return { status: "complete", data: report };
      }
      return { status: "retry" };
    },
    execute: async () => {
      await processes.compose("providers-start", [
        "up",
        "-d",
        "--wait",
        "postgres",
      ]);
      return preflight();
    },
  };
}

/**
 * The deployment the node joins, as the stack recorded it: the same manifest
 * bytes and L1 origin, and the reference-script deploy address it names.
 */
async function restoreAttachedEnvironment(processes: StackProcesses) {
  await restoreDeploymentEnvironment(processes);
  const manifest = (await readJsonIfPresent(stackPaths(processes).manifest)) as
    | { referenceScriptDeployAddress?: string }
    | undefined;
  if (manifest?.referenceScriptDeployAddress)
    processes.env.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS =
      manifest.referenceScriptDeployAddress;
}

function attachStep(processes: StackProcesses): StackStep {
  const own = stackPaths(processes).manifest;
  const ownOrigin = join(processes.config.runDirectory, "l1-origin.json");
  return {
    id: "attach",
    reconcile: async () => {
      const bytes = await readIfPresent(own);
      if (bytes === undefined) return { status: "retry" };
      const theirs = primaryPaths(processes);
      if (
        !bytes.equals((await readIfPresent(theirs.manifest)) ?? Buffer.from(""))
      )
        throw new Error(
          `${own} is not the deployment manifest of the stack this instance attaches to; preserve both`,
        );
      const origin = await readIfPresent(ownOrigin);
      if (
        origin === undefined ||
        !origin.equals((await readIfPresent(theirs.origin)) ?? Buffer.from(""))
      )
        throw new Error(
          `${ownOrigin} is not the L1 origin of the stack this instance attaches to`,
        );
      await restoreAttachedEnvironment(processes);
      const status = await deploymentStatus(processes);
      if (!status.manifest.ok || !status.protocol.complete)
        throw new Error(
          "The stack's deployment does not verify against this instance's environment; preserve its data",
        );
      const { manifestId } = JSON.parse(bytes.toString("utf8")) as {
        manifestId: string;
      };
      return { status: "complete", data: { manifestId } };
    },
    execute: async () => {
      const theirs = primaryPaths(processes);
      const bytes = await readIfPresent(theirs.manifest);
      const origin = await readIfPresent(theirs.origin);
      const manifest =
        bytes === undefined
          ? undefined
          : (JSON.parse(bytes.toString("utf8")) as {
              steps?: { initProtocol?: { status?: string } };
            });
      if (
        manifest?.steps?.initProtocol?.status !== "complete" ||
        origin === undefined
      )
        throw new Error(
          "The stack this instance attaches to has not initialized its deployment yet",
        );
      verifyFinalizedDeploymentManifest(manifest);
      // Its L1 origin first: the manifest is what marks the attachment.
      await writeTextFileAtomic(ownOrigin, origin, { mode: 0o600 });
      await mkdir(join(processes.config.nodeRoot, "deploymentInfo"), {
        recursive: true,
      });
      await writeTextFileAtomic(own, bytes!, { mode: 0o644 });
      // The node container reads it as its own user.
      await shareWithContainers(own);
      return { attached: true };
    },
  };
}

/**
 * The stack's producer manifest, published as this node: its own peer, and
 * on the host its own DA port. The committee and the deployment are the
 * stack's.
 */
function rebindProducer(
  manifest: DaLibp2pRuntimeManifest,
  peerId: string,
  port: number | undefined,
) {
  const address = (value: string) => {
    const match =
      /^(\/(?:ip4|ip6|dns4|dns6)\/[^/]+\/tcp\/)(\d+)(\/p2p\/.+)?$/u.exec(value);
    if (match === null) throw new Error(`Unexpected producer address ${value}`);
    return `${match[1]}${port ?? match[2]}${match[3] === undefined ? "" : `/p2p/${peerId}`}`;
  };
  const { da_transport: transport } = manifest;
  return parseDaLibp2pRuntimeManifest({
    ...manifest,
    runtime_topology: {
      ...manifest.runtime_topology,
      producer_peer_id: peerId,
    },
    da_transport: {
      ...transport,
      listen_multiaddrs: transport.listen_multiaddrs.map(address),
      announce_multiaddrs: transport.announce_multiaddrs.map(address),
    },
  });
}

/** What the generated configuration reads besides the deployment. */
async function attachedInputDigest(processes: StackProcesses) {
  const hash = createHash("sha256").update(JSON.stringify(processes.config));
  const { services } = primaryPaths(processes);
  for (const path of [
    processes.config.envFile,
    join(services, "producer.json"),
    join(services, "producer-host.json"),
  ])
    hash.update(`\0${path}\0`).update(
      createHash("sha256")
        .update((await readIfPresent(path)) ?? "absent")
        .digest("hex"),
    );
  return hash.digest("hex");
}

function attachedConfigurationStep(processes: StackProcesses): StackStep {
  return {
    id: "runtime-configuration",
    reconcile: async (record) => {
      if (
        record?.status === "complete" ||
        (record?.status === "running" && record.data !== null)
      ) {
        const saved = (await readJsonIfPresent(
          runtimeConfigurationPath(processes),
        )) as { inputDigest?: string } | undefined;
        if (saved?.inputDigest !== (await attachedInputDigest(processes)))
          return { status: "retry" };
        restoreRuntimeEnvironment(processes);
        return { status: "complete", data: record.data };
      }
      return { status: "retry" };
    },
    execute: async () => {
      const { config, env } = processes;
      const inputDigest = await attachedInputDigest(processes);
      const directory = servicesDirectory(processes);
      await mkdir(directory, { recursive: true, mode: 0o700 });
      const { peerId } = await loadDaLibp2pIdentity(
        env[config.da.producerTransportEnv]!,
      );
      for (const [name, port] of [
        ["producer.json", undefined],
        ["producer-host.json", Number(env.MIDGARD_NODE_DA_HOST_PORT ?? 39002)],
      ] as const) {
        const source = parseDaLibp2pRuntimeManifest(
          JSON.parse(
            await readFile(
              join(primaryPaths(processes).services, name),
              "utf8",
            ),
          ),
        );
        if (source.runtime_topology.producer_peer_id === peerId)
          throw new Error(
            `${config.da.producerTransportEnv} is the stack's own producer identity; give this node its own`,
          );
        await writeTextFileAtomic(
          join(directory, name),
          `${JSON.stringify(rebindProducer(source, peerId, port), null, 2)}\n`,
        );
      }
      // Only the node, its migration and its Postgres: never the stack's
      // Cardano node, which this node reads through its socket.
      const node = await stackNodeServices(processes, {
        envFile: [{ path: join(directory, "node.env"), required: false }],
        producer: join(directory, "producer.json"),
      });
      const compose = join(directory, "compose.yaml");
      await writeTextFileAtomic(
        compose,
        `${composeYaml({
          services: {
            ...node,
            "midgard-node": {
              ...node["midgard-node"],
              depends_on: override({
                postgres: { condition: "service_healthy" },
                "midgard-node-migrate": {
                  condition: "service_completed_successfully",
                },
              }),
            },
          },
        })}\n`,
        { mode: 0o644 },
      );
      await processes.compose(
        "compose-validate",
        ["config", "--quiet"],
        compose,
      );
      const configuration = { inputDigest, compose };
      await writeDurableJson(
        runtimeConfigurationPath(processes),
        configuration,
      );
      restoreRuntimeEnvironment(processes);
      return configuration;
    },
  };
}

async function readAttachedConfiguration(processes: StackProcesses) {
  const value = (await readJsonIfPresent(
    runtimeConfigurationPath(processes),
  )) as { inputDigest?: string; compose?: string } | undefined;
  if (!value?.inputDigest || !value.compose)
    throw new Error("Runtime configuration is missing");
  return value as { inputDigest: string; compose: string };
}

const readNode = async (processes: StackProcesses) => {
  const node = await getJson(`${processes.config.endpoint}/readyz`);
  return nodeIsReady(node) ? node : undefined;
};

function attachedServicesStep(processes: StackProcesses): StackStep {
  return {
    id: "services",
    reconcile: async (record) => {
      if (
        record?.status === "complete" ||
        (record?.status === "running" && record.data !== null)
      ) {
        try {
          const { inputDigest } = await readAttachedConfiguration(processes);
          if (
            (record.data as { inputDigest?: unknown }).inputDigest ===
            inputDigest
          ) {
            const node = await readNode(processes);
            if (node !== undefined)
              return { status: "complete", data: { inputDigest, node } };
          }
        } catch {
          // Missing or unreadable expectations start the node again.
        }
      }
      return { status: "retry" };
    },
    execute: async () => {
      const runtime = await readAttachedConfiguration(processes);
      const directory = servicesDirectory(processes);
      await processes.compose(
        "services-build",
        ["build", "midgard-node", "midgard-node-migrate"],
        runtime.compose,
      );
      await writePrivateEnv(
        join(directory, "node.env"),
        nodeEnvironment(
          processes,
          await pinNativeOwner(processes, directory, runtime.compose),
        ),
      );
      await preflightProducerTransport(processes, directory, runtime.compose);
      await processes.compose(
        "runtime-start",
        ["up", "-d", "midgard-node"],
        runtime.compose,
      );
      return {
        inputDigest: runtime.inputDigest,
        node: await poll("node readiness", processes.config.timeoutMs, () =>
          readNode(processes),
        ),
      };
    },
  };
}
