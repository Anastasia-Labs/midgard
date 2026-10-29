import { constants } from "node:fs";
import { access, lstat, mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { pathToFileURL } from "node:url";

import { canonicalJson } from "@al-ft/midgard-core/canonical-json";
import { verifyFinalizedDeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  authenticatePublicHttpRelease,
  PUBLIC_NODE_SECRET_NAMES,
} from "midgard-node/commands/listen-public-http";
import { writeTextFileAtomic } from "midgard-node/files/atomic-write";

import {
  classificationOf,
  signDeploymentRelease,
  SIGNED_RELEASE_FILES,
} from "../sign-deployment-release.js";
import type { StackConfig } from "./config.js";
import { stackPaths } from "./deployment.js";
import { readJsonIfPresent } from "./journal.js";
import { nativeLedgerPaths } from "./native-ledger.js";
import { publicRolesPath, type StackProcesses } from "./process.js";
import { INGRESS_SUBNET, publicOriginHost } from "./public-node.js";
import {
  proverFundingKeyHash,
  readReleaseInput,
  stackBlueprintPath,
} from "./release.js";
import type { StackStep } from "./workflow.js";

/** Addresses on the ingress network (INGRESS_SUBNET). */
const INGRESS_ADDRESS = "10.91.7.2";
const NODE_INGRESS_ADDRESS = "10.91.7.3";

/** Where the public node and Postgres containers read their private and release files. */
const CONTAINER = {
  config: "/run/secrets/node-config.json",
  tlsCert: "/run/public/tls-cert.pem",
  tlsKey: "/run/secrets/tls-key.pem",
  tlsCa: "/run/public/tls-ca.pem",
  trustRoots: "/run/public/release-trust-roots.json",
  release: "/run/public/release",
  transport: "/usr/local/bin/midgard-l1-node-transport",
  postgresPassword: "/run/secrets/postgres-password",
};

/** The public node's container user: the host user, which owns its private files. */
const hostUser = () => `${process.getuid!()}:${process.getgid!()}`;

/** A value Compose replaces instead of merging with the base file's (`!override`). */
const OVERRIDE = Symbol("override");
type Overridden = { [OVERRIDE]: unknown };
const override = (value: unknown): Overridden => ({ [OVERRIDE]: value });

/** YAML flow style: JSON, plus the Compose merge tag JSON cannot carry. */
function composeYaml(value: unknown): string {
  if (value !== null && typeof value === "object" && OVERRIDE in value)
    return `!override ${composeYaml((value as Overridden)[OVERRIDE])}`;
  if (Array.isArray(value)) return `[${value.map(composeYaml).join(", ")}]`;
  if (value !== null && typeof value === "object")
    return `{${Object.entries(value)
      .map(([key, item]) => `${JSON.stringify(key)}: ${composeYaml(item)}`)
      .join(", ")}}`;
  return JSON.stringify(value);
}

/** The Postgres password file the public Postgres container reads, in the run directory. */
export const publicPostgresPasswordPath = (config: StackConfig) =>
  join(config.runDirectory, "postgres-password");

/**
 * The public stack's Compose overlay on the base files: the release's
 * digest-pinned role images, the node and its migration as the host user,
 * host ports bound to loopback, and Postgres reading its password from a
 * private file rather than its environment. The node API stays on every
 * interface only without the TLS ingress, which otherwise is its public port;
 * the DA port serves the stack's own committee, which dials the loopback.
 */
export async function writePublicRoles(processes: StackProcesses) {
  const { env, config } = processes;
  const loopback = "127.0.0.1:";
  const api = config.public!.ingressHostPort === null ? "" : loopback;
  const node = {
    image: env.MIDGARD_PUBLIC_NODE_IMAGE!,
    pull_policy: "missing",
    user: hostUser(),
    cap_drop: ["ALL"],
    security_opt: ["no-new-privileges:true"],
  };
  const password = publicPostgresPasswordPath(config);
  await writeTextFileAtomic(password, env.POSTGRES_PASSWORD!, { mode: 0o600 });
  await writeTextFileAtomic(
    publicRolesPath(config),
    `${composeYaml({
      services: {
        "midgard-node": {
          ...node,
          ports: override([
            `${api}\${MIDGARD_NODE_API_HOST_PORT:-\${PORT:-3000}}:\${PORT:-3000}`,
            `${loopback}\${MIDGARD_NODE_METRICS_HOST_PORT:-9464}:9464`,
            `${loopback}\${MIDGARD_NODE_DA_HOST_PORT:-39002}:39002`,
          ]),
        },
        "midgard-node-migrate": node,
        postgres: {
          image: env.MIDGARD_PUBLIC_POSTGRES_IMAGE!,
          pull_policy: "missing",
          ports: override([
            `${loopback}\${MIDGARD_POSTGRES_HOST_PORT:-5433}:5432`,
          ]),
          environment: override({
            POSTGRES_USER: "${POSTGRES_USER}",
            POSTGRES_DB: "${POSTGRES_DB}",
            POSTGRES_PASSWORD_FILE: CONTAINER.postgresPassword,
            PGDATA: "/var/lib/postgresql/data/pgdata",
          }),
          volumes: [`${password}:${CONTAINER.postgresPassword}:ro`],
        },
      },
    })}\n`,
    { mode: 0o644 },
  );
}

/**
 * Pulls the release's digest-pinned images and writes the overlay that pins
 * the node, its migration and Postgres to them, before any service starts.
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
  await writePublicRoles(processes);
}

const releaseFile = (
  processes: StackProcesses,
  name: keyof typeof SIGNED_RELEASE_FILES,
) =>
  join(processes.config.public!.releaseDirectory, SIGNED_RELEASE_FILES[name]);

const sameJson = (left: unknown, right: unknown) =>
  canonicalJson(left, "classification") ===
  canonicalJson(right, "classification");

/** The classification the reference-script deployment used, from phase 1. */
const deployedClassification = async (processes: StackProcesses) =>
  JSON.parse(
    await readFile(
      processes.config.public!.blueprintClassificationFile,
      "utf8",
    ),
  ) as unknown;

/**
 * Opens the public release as the node does, against the operator's trust
 * roots, for the manifest this deployment finalized, and with the
 * classification its reference-script deployment used.
 */
async function verifyPublicRelease(processes: StackProcesses) {
  const { manifestId } = verifyFinalizedDeploymentManifest(
    await readJsonIfPresent(stackPaths(processes).manifest),
  );
  authenticatePublicHttpRelease({
    ...processes.env,
    MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: releaseFile(processes, "authority"),
    MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE: releaseFile(
      processes,
      "provenance",
    ),
    MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE:
      processes.config.public!.trustRootsFile,
    MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE: releaseFile(processes, "manifest"),
    MIDGARD_PUBLIC_MANIFEST_ID: manifestId,
    MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE: releaseFile(
      processes,
      "parameterRegistry",
    ),
    MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: releaseFile(
      processes,
      "classification",
    ),
  });
  if (
    !sameJson(
      JSON.parse(
        await readFile(releaseFile(processes, "classification"), "utf8"),
      ),
      await deployedClassification(processes),
    )
  )
    throw new Error(
      "its blueprint classification differs from public.blueprintClassificationFile",
    );
  return { manifestId };
}

/** Reopens an existing public release; never signs over one. */
async function reopenPublicRelease(processes: StackProcesses) {
  const directory = processes.config.public!.releaseDirectory;
  try {
    return await verifyPublicRelease(processes);
  } catch (error) {
    throw new Error(
      `public.releaseDirectory ${directory} holds a release this deployment cannot use (${(error as Error).message}); move it aside so the stack signs a new one`,
    );
  }
}

/**
 * Phase 2, once the deployment is final: the stack signs the public release
 * for it into public.releaseDirectory with the release input's signing key,
 * through the same code as the release signing command, which also opens
 * the written release as public listen does. A resume reopens and verifies
 * the release already there and never signs again.
 */
export function publicReleaseStep(processes: StackProcesses): StackStep {
  const directory = processes.config.public!.releaseDirectory;
  return {
    id: "public-release",
    reconcile: async () =>
      (await lstat(directory).catch(() => undefined)) === undefined
        ? { status: "retry" }
        : { status: "complete", data: await reopenPublicRelease(processes) },
    execute: async () => {
      if ((await lstat(directory).catch(() => undefined)) !== undefined)
        return reopenPublicRelease(processes);
      const releaseInput = processes.config.watcher.releaseInput!;
      // The release's classification comes from its registry; the
      // reference-script deployment already used the operator's.
      const { deploymentParameterRegistry } =
        await readReleaseInput(releaseInput);
      if (deploymentParameterRegistry === undefined)
        throw new Error(
          `The release input ${releaseInput} names no parameterRegistryFile; a public release signs this deployment's parameter registry`,
        );
      if (
        !sameJson(
          classificationOf(deploymentParameterRegistry),
          await deployedClassification(processes),
        )
      )
        throw new Error(
          `The release input's parameter registry classifies the unused validators differently from public.blueprintClassificationFile ${processes.config.public!.blueprintClassificationFile}, which the reference-script deployment used`,
        );
      await signDeploymentRelease({
        releaseInput,
        deploymentInfo: stackPaths(processes).manifest,
        blueprint: stackBlueprintPath(processes.config),
        output: directory,
        fundingPaymentKeyHash: proverFundingKeyHash(processes),
      });
      return reopenPublicRelease(processes);
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
    MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE: `${CONTAINER.release}/${SIGNED_RELEASE_FILES.authority}`,
    MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE: `${CONTAINER.release}/${SIGNED_RELEASE_FILES.provenance}`,
    MIDGARD_BLUEPRINT_CLASSIFICATION_FILE: `${CONTAINER.release}/${SIGNED_RELEASE_FILES.classification}`,
    MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE: `${CONTAINER.release}/${SIGNED_RELEASE_FILES.manifest}`,
    MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE: `${CONTAINER.release}/${SIGNED_RELEASE_FILES.parameterRegistry}`,
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
 * public-roles.yaml), its private and release mounts, an HTTPS health check
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
        config.public.blueprintClassificationFile,
        ...Object.values(SIGNED_RELEASE_FILES).map((name) =>
          join(config.public!.releaseDirectory, name),
        ),
      ];
