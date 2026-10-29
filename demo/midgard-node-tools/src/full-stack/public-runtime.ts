import { constants } from "node:fs";
import { access, mkdir, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  authenticatePublicHttpRelease,
  PUBLIC_NODE_SECRET_NAMES,
} from "midgard-node/commands/listen-public-http";

import type { StackConfig } from "./config.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent, writeDurableJson } from "./journal.js";
import { nativeLedgerPaths } from "./native-ledger.js";
import { publicRolesPath, type StackProcesses } from "./process.js";
import { PUBLIC_RELEASE_FILES, publicOriginHost } from "./public-node.js";
import type { StackStep } from "./workflow.js";

/** The ingress network: the node trusts the ingress address to name clients. */
const INGRESS_SUBNET = "10.91.7.0/28";
const INGRESS_ADDRESS = "10.91.7.2";
const NODE_INGRESS_ADDRESS = "10.91.7.3";

/** Where the public node container reads its private and release files. */
const CONTAINER = {
  config: "/run/secrets/node-config.json",
  tlsCert: "/run/public/tls-cert.pem",
  tlsKey: "/run/secrets/tls-key.pem",
  tlsCa: "/run/public/tls-ca.pem",
  trustRoots: "/run/public/release-trust-roots.json",
  release: "/run/public/release",
  transport: "/usr/local/bin/midgard-l1-node-transport",
};

/** The public node's container user: the host user, which owns its private files. */
const hostUser = () => `${process.getuid!()}:${process.getgid!()}`;

/**
 * Pulls the release's digest-pinned images and pins the node, its migration
 * and Postgres to them, before any service starts.
 */
export async function preparePublicImages(processes: StackProcesses) {
  const { env, config } = processes;
  const images = [
    env.MIDGARD_PUBLIC_NODE_IMAGE!,
    env.MIDGARD_PUBLIC_POSTGRES_IMAGE!,
    ...(config.public!.ingressHostPort === null
      ? []
      : [env.MIDGARD_PUBLIC_INGRESS_IMAGE!]),
  ];
  for (const [index, image] of images.entries())
    await processes.command(
      `public-image-pull-${index}`,
      "docker",
      ["pull", image],
      {},
      config.nodeRoot,
      "host",
    );
  const node = {
    image: env.MIDGARD_PUBLIC_NODE_IMAGE!,
    pull_policy: "missing",
    user: hostUser(),
    cap_drop: ["ALL"],
    security_opt: ["no-new-privileges:true"],
  };
  await writeDurableJson(publicRolesPath(processes.config), {
    services: {
      "midgard-node": node,
      "midgard-node-migrate": node,
      postgres: {
        image: env.MIDGARD_PUBLIC_POSTGRES_IMAGE!,
        pull_policy: "missing",
      },
    },
  });
}

const releaseFile = (processes: StackProcesses, name: string) =>
  join(processes.config.public!.releaseDirectory, name);

/** The deployment's finalized manifest and the parameter registry its deployment wrote. */
function finalizedDeployment(processes: StackProcesses) {
  const manifest = stackPaths(processes).manifest;
  return { manifest, registry: `${manifest}.blueprint-registry.json` };
}

/** The node's release check, run on the host against the files the container mounts. */
async function verifyPublicRelease(processes: StackProcesses) {
  const { manifest, registry } = finalizedDeployment(processes);
  const { manifestId } = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(manifest),
  );
  authenticatePublicHttpRelease({
    ...processes.env,
    MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: releaseFile(
      processes,
      PUBLIC_RELEASE_FILES.authority,
    ),
    MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE: releaseFile(
      processes,
      PUBLIC_RELEASE_FILES.provenance,
    ),
    MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE:
      processes.config.public!.trustRootsFile,
    MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE: manifest,
    MIDGARD_PUBLIC_MANIFEST_ID: manifestId,
    MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE: registry,
    MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: releaseFile(
      processes,
      PUBLIC_RELEASE_FILES.classification,
    ),
  });
  return { manifestId };
}

/**
 * Phase 2, just before the services start: the release signature over the
 * manifest this deployment finalized. Until it verifies, the run stops here
 * and says what to sign; a rerun confirms the earlier steps from their saved
 * state and chain observations, so no transaction is repeated.
 */
export function publicReleaseStep(processes: StackProcesses): StackStep {
  return {
    id: "public-release",
    reconcile: async () => {
      try {
        return {
          status: "complete",
          data: await verifyPublicRelease(processes),
        };
      } catch {
        return { status: "retry" };
      }
    },
    execute: async () => {
      const { manifest, registry } = finalizedDeployment(processes);
      const { manifestId } = verifyFinalizedDeploymentManifest(
        await readJsonIfPresent(manifest),
      );
      let reason: string;
      try {
        await verifyPublicRelease(processes);
        return { manifestId };
      } catch (error) {
        reason = (error as Error).message;
      }
      const { env } = processes;
      throw new Error(
        [
          "Sign the release now: the deployment is on chain, and the public node starts only with a release signed for it.",
          `Release check: ${reason}`,
          "Produce with the release signing command (R3):",
          `  ${releaseFile(processes, PUBLIC_RELEASE_FILES.authority)}`,
          `  ${releaseFile(processes, PUBLIC_RELEASE_FILES.provenance)}`,
          "signing:",
          `  finalized manifest ${manifest} (manifest id ${manifestId})`,
          `  deployment parameter registry ${registry}`,
          `  blueprint classification ${releaseFile(processes, PUBLIC_RELEASE_FILES.classification)}`,
          `  source revision ${env.MIDGARD_PUBLIC_RELEASE_REVISION}`,
          `  node image ${env.MIDGARD_PUBLIC_NODE_IMAGE}`,
          `  postgres image ${env.MIDGARD_PUBLIC_POSTGRES_IMAGE}`,
          `  ingress image ${env.MIDGARD_PUBLIC_INGRESS_IMAGE}`,
          `with a key listed in ${processes.config.public!.trustRootsFile}.`,
          "Then rerun the same e2e-stack command; confirmed on-chain steps are not repeated.",
        ].join("\n"),
      );
    },
  };
}

/** The private configuration the public node container reads, in the run directory. */
export const publicNodeConfigPath = (servicesDirectory: string) =>
  join(servicesDirectory, "node-config.json");

/** Writes the node container's private configuration: its secrets and DA identity. */
export async function writePublicNodeConfig(
  processes: StackProcesses,
  servicesDirectory: string,
) {
  const { env, config } = processes;
  const path = publicNodeConfigPath(servicesDirectory);
  await writeFile(
    path,
    JSON.stringify({
      ...Object.fromEntries(
        PUBLIC_NODE_SECRET_NAMES.map((name) => [name, env[name]!]),
      ),
      DA_LIBP2P_PRIVATE_KEY_SOURCE: env[config.da.producerTransportEnv]!,
    }),
    { mode: 0o600 },
  );
  // A container runs as the host user and writes its database under the node directory.
  const db = join(config.nodeRoot, "db");
  await mkdir(db, { recursive: true, mode: 0o700 });
  await access(db, constants.W_OK).catch(() => {
    throw new Error(
      `${db} must be writable by the host user, which runs the public node`,
    );
  });
}

/**
 * The public node container's environment: the testing node's, without
 * secrets (they come from the mounted configuration, and a process setting
 * would override it), plus the mounted public inputs.
 */
export async function publicNodeEnvironment(
  processes: StackProcesses,
  nodeEnv: Record<string, string>,
) {
  const { manifestId } = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(stackPaths(processes).manifest),
  );
  const env: Record<string, string> = { ...nodeEnv };
  for (const name of [
    ...PUBLIC_NODE_SECRET_NAMES,
    "DA_LIBP2P_PRIVATE_KEY_SOURCE",
  ])
    delete env[name];
  return {
    ...env,
    MIDGARD_DOTENV_MODE: "disabled",
    MIDGARD_CONFIG_FILE: CONTAINER.config,
    MIDGARD_PUBLIC_TLS_CERT_FILE: CONTAINER.tlsCert,
    MIDGARD_PUBLIC_TLS_KEY_FILE: CONTAINER.tlsKey,
    MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE: CONTAINER.trustRoots,
    MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: `${CONTAINER.release}/${PUBLIC_RELEASE_FILES.authority}`,
    MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE: `${CONTAINER.release}/${PUBLIC_RELEASE_FILES.provenance}`,
    MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: `${CONTAINER.release}/${PUBLIC_RELEASE_FILES.classification}`,
    MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE:
      nodeEnv.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH!,
    MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE: `${nodeEnv.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH!}.blueprint-registry.json`,
    MIDGARD_PUBLIC_MANIFEST_ID: manifestId,
    // The release image carries its own pinned L1 transport.
    L1_NODE_TRANSPORT_BINARY_PATH: CONTAINER.transport,
    ...(processes.config.public!.ingressHostPort === null
      ? {}
      : { HTTP_TRUSTED_PROXY_PEERS: INGRESS_ADDRESS }),
  };
}

type PublicIngressModule = {
  renderPublicIngress: (env: Record<string, string | undefined>) => string;
};

/**
 * The public node's Compose services: the release image (from
 * public-roles.json), its private and release mounts, an HTTPS health check
 * and, when configured, the TLS ingress in front of it.
 */
export async function publicNodeServices(
  processes: StackProcesses,
  input: { servicesDirectory: string; envFile: unknown; producer: string },
) {
  const { config, env } = processes;
  const inputs = config.public!;
  const ledger = nativeLedgerPaths(processes);
  const port = env.PORT ?? "3000";
  const volumes = [
    `${publicNodeConfigPath(input.servicesDirectory)}:${CONTAINER.config}:ro`,
    `${inputs.tlsCertFile}:${CONTAINER.tlsCert}:ro`,
    `${inputs.tlsKeyFile}:${CONTAINER.tlsKey}:ro`,
    `${inputs.tlsCaFile}:${CONTAINER.tlsCa}:ro`,
    `${inputs.trustRootsFile}:${CONTAINER.trustRoots}:ro`,
    `${inputs.releaseDirectory}:${CONTAINER.release}:ro`,
    `${ledger.socketDirectory}:/ipc`,
    `${ledger.directory}:/cardano-config:ro`,
  ];
  const ingress = inputs.ingressHostPort !== null;
  const services: Record<string, unknown> = {
    "midgard-node": {
      env_file: input.envFile,
      volumes: [`${input.producer}:/app/stack/producer.json:ro`, ...volumes],
      healthcheck: {
        test: [
          "CMD",
          "node",
          "-e",
          `require('node:https').get({host:'127.0.0.1',port:${port},path:'/readyz',ca:require('node:fs').readFileSync('${CONTAINER.tlsCa}'),servername:'${publicOriginHost(env)}',minVersion:'TLSv1.3'},(r)=>process.exit(r.statusCode===200?0:1)).on('error',()=>process.exit(1))`,
        ],
      },
      ...(ingress
        ? {
            networks: {
              midgard_net: {},
              public_ingress: { ipv4_address: NODE_INGRESS_ADDRESS },
            },
          }
        : {}),
    },
    "midgard-node-migrate": { env_file: input.envFile, volumes },
  };
  if (!ingress) return { services, networks: {}, startServices: [] };
  const { renderPublicIngress } = (await import(
    pathToFileURL(join(config.nodeRoot, "scripts/public-ingress.mjs")).href
  )) as PublicIngressModule;
  const ingressConfig = join(input.servicesDirectory, "public-ingress.conf");
  await writeFile(ingressConfig, renderPublicIngress({ ...env, PORT: port }), {
    mode: 0o644,
  });
  services["public-ingress"] = {
    image: env.MIDGARD_PUBLIC_INGRESS_IMAGE!,
    pull_policy: "missing",
    user: hostUser(),
    read_only: true,
    cap_drop: ["ALL"],
    security_opt: ["no-new-privileges:true"],
    entrypoint: [],
    command: ["nginx", "-g", "daemon off;"],
    ports: [`${inputs.ingressHostPort}:8443`],
    volumes: [
      `${ingressConfig}:/etc/nginx/nginx.conf:ro`,
      `${inputs.tlsCertFile}:${CONTAINER.tlsCert}:ro`,
      `${inputs.tlsKeyFile}:${CONTAINER.tlsKey}:ro`,
      `${inputs.tlsCaFile}:${CONTAINER.tlsCa}:ro`,
    ],
    tmpfs: ["/tmp:rw,noexec,nosuid,size=32m"],
    networks: { public_ingress: { ipv4_address: INGRESS_ADDRESS } },
    depends_on: { "midgard-node": { condition: "service_healthy" } },
    restart: "unless-stopped",
  };
  return {
    services,
    networks: {
      public_ingress: { ipam: { config: [{ subnet: INGRESS_SUBNET }] } },
    },
    startServices: ["public-ingress"],
  };
}

/** The public files whose contents the generated runtime configuration depends on. */
export const publicRuntimeInputs = (config: StackConfig) =>
  config.public === undefined
    ? []
    : [
        config.public.nodeConfigFile,
        config.public.tlsCertFile,
        config.public.tlsKeyFile,
        config.public.tlsCaFile,
        config.public.trustRootsFile,
        ...Object.values(PUBLIC_RELEASE_FILES).map((name) =>
          join(config.public!.releaseDirectory, name),
        ),
      ];
