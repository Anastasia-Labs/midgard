import { execFile } from "node:child_process";
import { createPrivateKey, createPublicKey } from "node:crypto";
import { lstat, readdir, readFile, stat } from "node:fs/promises";
import type { AddressInfo } from "node:net";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { connect, createServer } from "node:tls";
import { promisify } from "node:util";

import { DEPLOYMENT_PROFILES } from "@al-ft/midgard-core/deployment-profile";
import {
  assertPublicCredentials,
  assertPublicOperatorWallets,
  assertPublicReleaseIdentity,
  loadPublicMountedConfig,
  loadPublicTls,
  PUBLIC_NODE_SECRET_NAMES,
  PUBLIC_RELEASE_INPUT_LIMITS,
  type PublicHttpProfile,
  readMountedHttpFile,
} from "midgard-node/commands/listen-public-http";
import { resolveValidationWorkerPoolSize } from "midgard-node/services/config.node-config-dep";

import { SIGNED_RELEASE_FILES } from "../sign-deployment-release.js";
import type { StackConfig } from "./config.js";

/** The preprod-public node's inputs, the stack configuration's `public` section. */
export type PublicStackInputs = {
  /** Private file (0600, YAML or JSON) holding exactly the node's six secrets. */
  nodeConfigFile: string;
  /** Certificate, private key (0600) and issuing CA for the public origin. */
  tlsCertFile: string;
  tlsKeyFile: string;
  tlsCaFile: string;
  /** External Ed25519 trust roots that verify the signed release. */
  trustRootsFile: string;
  /** The unused-validator classification the reference-script deployment reads. */
  blueprintClassificationFile: string;
  /** Where the stack signs the public release; it must not exist before signing. */
  releaseDirectory: string;
  /** Host port of the optional TLS ingress; null runs the node without one. */
  ingressHostPort: number | null;
};

const PATH_KEYS = [
  "nodeConfigFile",
  "tlsCertFile",
  "tlsKeyFile",
  "tlsCaFile",
  "trustRootsFile",
  "blueprintClassificationFile",
  "releaseDirectory",
] as const;

/** Settings the stack derives for the public node; the .env must not set them. */
const STACK_DERIVED_SETTINGS = [
  "MIDGARD_CONFIG_FILE",
  "MIDGARD_PUBLIC_TLS_CERT_FILE",
  "MIDGARD_PUBLIC_TLS_KEY_FILE",
  "MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE",
  "MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE",
  "MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE",
  "MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE",
  "MIDGARD_PUBLIC_MANIFEST_ID",
  "MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE",
  "MIDGARD_BLUEPRINT_CLASSIFICATION_FILE",
  "HTTP_TRUSTED_PROXY_PEERS",
];

export function parsePublicStackInputs(value: unknown): PublicStackInputs {
  if (!value || typeof value !== "object" || Array.isArray(value))
    throw new Error("public must be an object");
  const input = value as Record<string, unknown>;
  const keys = [...PATH_KEYS, "ingressHostPort"];
  if (Object.keys(input).sort().join() !== [...keys].sort().join())
    throw new Error(`public needs exactly these fields: ${keys.join(", ")}`);
  const paths = Object.fromEntries(
    PATH_KEYS.map((key) => {
      const path = input[key];
      if (typeof path !== "string" || !isAbsolute(path))
        throw new Error(`public.${key} must be an absolute path`);
      return [key, resolve(path)];
    }),
  ) as Record<(typeof PATH_KEYS)[number], string>;
  const port = input.ingressHostPort;
  if (
    port !== null &&
    (typeof port !== "number" ||
      !Number.isSafeInteger(port) ||
      port < 1 ||
      port > 65535)
  )
    throw new Error("public.ingressHostPort must be a TCP port or null");
  return { ...paths, ingressHostPort: port };
}

/** Prefixes the input a node refusal concerns, so the operator knows what to fix. */
function refuse<T>(label: string, check: () => T): T {
  try {
    return check();
  } catch (error) {
    throw new Error(`${label}: ${(error as Error).message}`);
  }
}

const inside = (path: string, directory: string) => {
  const offset = relative(directory, path);
  return offset === "" || (!offset.startsWith("..") && offset !== path);
};

/** The public origin's hostname, which the node certificate must cover. */
export const publicOriginHost = (env: Record<string, string>) =>
  new URL(env.MIDGARD_PUBLIC_API_ORIGIN!).hostname;

/** A private input: a regular file of this user's that only it can read. */
async function assertPrivateFile(label: string, path: string) {
  const stat = await lstat(path).catch(() => undefined);
  if (stat === undefined) throw new Error(`${label} ${path} is missing`);
  if (!stat.isFile())
    throw new Error(`${label} ${path} must be a regular file, not a link`);
  if (stat.uid !== process.getuid!())
    throw new Error(
      `${label} ${path} must belong to the user running the stack: chown $(id -u):$(id -g) ${path}`,
    );
  if ((stat.mode & 0o077) !== 0)
    throw new Error(`${label} ${path} must be private: chmod 600 ${path}`);
}

/**
 * The public release directory: absent until the stack signs into it, or a
 * complete earlier signing that the public-release step verifies.
 */
async function assertReleaseDirectory(directory: string) {
  const parent = await stat(dirname(directory)).catch(() => undefined);
  if (!parent?.isDirectory())
    throw new Error(
      `The parent of public.releaseDirectory ${directory} must be an existing directory`,
    );
  const existing = await lstat(directory).catch(() => undefined);
  if (existing === undefined) return;
  const present = existing.isDirectory() ? await readdir(directory) : [];
  const missing = Object.values(SIGNED_RELEASE_FILES).filter(
    (name) => !present.includes(name),
  );
  if (missing.length > 0)
    throw new Error(
      `public.releaseDirectory ${directory} exists but is not a complete signed release (missing ${missing.join(", ")}); move it aside so the stack signs a new one`,
    );
}

/** The release signing key the stack signs with must be one of the trust roots public listen trusts. */
async function assertSignerIsTrusted(config: StackConfig, roots: unknown[]) {
  if (!config.watcher.releaseInput)
    throw new Error(
      "A preprod-public stack signs its release with watcher.releaseInput; set it",
    );
  const { signingKeyFile } = JSON.parse(
    await readFile(config.watcher.releaseInput, "utf8"),
  ) as { signingKeyFile?: string };
  if (typeof signingKeyFile !== "string" || !isAbsolute(signingKeyFile))
    throw new Error("Release input needs an absolute signingKeyFile");
  const publicKey = createPublicKey(
    createPrivateKey(await readFile(signingKeyFile)),
  )
    .export({ format: "der", type: "spki" })
    .toString("hex");
  if (
    !roots.some(
      (root) =>
        (root as { publicKeySpkiDerHex?: unknown } | null)
          ?.publicKeySpkiDerHex === publicKey,
    )
  )
    throw new Error(
      `public.trustRootsFile does not list the public key of the release signing key ${signingKeyFile}`,
    );
}

/** A database directory left by a container that ran as another user. */
async function assertNodeDatabaseOwner(nodeRoot: string) {
  const db = join(nodeRoot, "db");
  const owner = await stat(db).catch(() => undefined);
  if (owner !== undefined && owner.uid !== process.getuid!())
    throw new Error(
      `${db} belongs to uid ${owner.uid}, but the public node runs as the host user: sudo chown -R $(id -u):$(id -g) ${db}`,
    );
}

/** The ingress network: the node trusts the ingress address to name clients. */
export const INGRESS_SUBNET = "10.91.7.0/28";

const ipv4Range = (cidr: string) => {
  const [address, bits] = cidr.split("/");
  const octets = (address ?? "").split(".").map(Number);
  const size = Number(bits);
  if (
    octets.length !== 4 ||
    octets.some((octet) => !(octet >= 0 && octet <= 255)) ||
    !(size >= 0 && size <= 32)
  )
    return undefined;
  const start = octets.reduce((value, octet) => value * 256 + octet, 0);
  const span = 2 ** (32 - size);
  const first = start - (start % span);
  return { first, last: first + span - 1 };
};

/**
 * The ingress subnet is fixed, so another Docker network on it would make
 * Compose refuse to create the ingress network after the deployment spent.
 * Only this stack's own ingress network may already hold it. When Docker
 * cannot be asked, the check passes and Compose reports the collision itself.
 */
async function assertIngressSubnetFree(env: Record<string, string>) {
  const docker = (args: string[]) =>
    promisify(execFile)("docker", args, { timeout: 10_000 }).then(
      ({ stdout }) => stdout,
    );
  let networks: {
    Name?: string;
    Labels?: Record<string, string> | null;
    IPAM?: { Config?: { Subnet?: string }[] | null };
  }[];
  try {
    const ids = (await docker(["network", "ls", "--quiet"]))
      .split("\n")
      .filter(Boolean);
    networks =
      ids.length === 0
        ? []
        : JSON.parse(await docker(["network", "inspect", ...ids]));
  } catch {
    return;
  }
  const project = env.COMPOSE_PROJECT_NAME || "midgard-node";
  const ingress = ipv4Range(INGRESS_SUBNET)!;
  for (const network of networks) {
    const labels = network.Labels ?? {};
    if (
      labels["com.docker.compose.project"] === project &&
      labels["com.docker.compose.network"] === "public_ingress"
    )
      continue;
    for (const { Subnet } of network.IPAM?.Config ?? []) {
      const range = Subnet === undefined ? undefined : ipv4Range(Subnet);
      if (
        range !== undefined &&
        range.first <= ingress.last &&
        ingress.first <= range.last
      )
        throw new Error(
          `Docker network ${network.Name} already uses ${Subnet}, which overlaps the public ingress subnet ${INGRESS_SUBNET}; remove that network (docker network rm ${network.Name}) or run without the ingress (public.ingressHostPort null)`,
        );
    }
  }
}

/** A real loopback TLS handshake proves the CA verifies the certificate for the origin. */
async function assertCaVerifiesCertificate(
  tls: { cert: Buffer; key: Buffer },
  ca: Buffer,
  servername: string,
) {
  const server = createServer(
    { cert: tls.cert, key: tls.key, minVersion: "TLSv1.3" },
    (socket) => socket.end(),
  );
  await new Promise<void>((done, fail) =>
    server.once("error", fail).listen(0, "127.0.0.1", done),
  );
  try {
    const { port } = server.address() as AddressInfo;
    await new Promise<void>((done, fail) => {
      const socket = connect(
        { host: "127.0.0.1", port, ca, servername, minVersion: "TLSv1.3" },
        () => {
          socket.end();
          done();
        },
      );
      socket.once("error", fail);
    });
  } catch {
    throw new Error(
      `public.tlsCaFile does not verify the TLS certificate for ${servername}`,
    );
  } finally {
    await new Promise((done) => server.close(done));
  }
}

/**
 * Phase 1: every public input a deployment needs before it spends anything.
 * Runs in loadStackConfig, so `--check` and a full run refuse alike. Moves
 * the node's secrets from the private node configuration into `env` for the
 * host deployment commands. The release itself is signed later
 * (`publicReleaseStep`): it signs the manifest this deployment produces.
 */
export async function checkPublicInputs(
  config: StackConfig,
  env: Record<string, string>,
) {
  const inputs = config.public;
  if (inputs === undefined)
    throw new Error(
      `A preprod-public stack needs the public section: ${[...PATH_KEYS, "ingressHostPort"].join(", ")}`,
    );
  if (new URL(config.endpoint).protocol !== "https:")
    throw new Error(
      "A preprod-public node serves HTTPS; use an https:// loopback endpoint",
    );
  for (const name of PUBLIC_NODE_SECRET_NAMES)
    if (name in env)
      throw new Error(
        `Keep ${name} in public.nodeConfigFile, not in ${config.envFile}`,
      );
  for (const name of STACK_DERIVED_SETTINGS)
    if (name in env)
      throw new Error(
        `The stack sets ${name} for a preprod-public node; remove it from ${config.envFile}`,
      );
  if ("L1_USER_SEED_PHRASE" in env)
    throw new Error(
      "A preprod-public operator cannot hold L1_USER_SEED_PHRASE; remove it",
    );
  const context = resolve(config.nodeRoot, "..");
  const ignored = join(config.nodeRoot, "logs");
  for (const key of [
    "nodeConfigFile",
    "tlsKeyFile",
    "releaseDirectory",
  ] as const)
    if (inside(inputs[key], context) && !inside(inputs[key], ignored))
      throw new Error(
        `public.${key} is inside the Docker build context ${context}; use a path under ${ignored} or outside the checkout`,
      );
  if (inputs.releaseDirectory === config.watcher.releaseDirectory)
    throw new Error(
      "public.releaseDirectory must differ from watcher.releaseDirectory",
    );
  await assertPrivateFile("public.nodeConfigFile", inputs.nodeConfigFile);
  await assertPrivateFile("public.tlsKeyFile", inputs.tlsKeyFile);
  const fileEnv = refuse("public.nodeConfigFile", () =>
    loadPublicMountedConfig(inputs.nodeConfigFile),
  );
  const extra = Object.keys(fileEnv).filter(
    (name) =>
      !["MIDGARD_CONFIG_FILE", "MIDGARD_DOTENV_MODE"].includes(name) &&
      !(PUBLIC_NODE_SECRET_NAMES as readonly string[]).includes(name),
  );
  if (extra.length > 0)
    throw new Error(
      `public.nodeConfigFile holds only ${PUBLIC_NODE_SECRET_NAMES.join(", ")}; move ${extra.join(", ")} to ${config.envFile}`,
    );
  for (const name of PUBLIC_NODE_SECRET_NAMES) env[name] = fileEnv[name]!;
  refuse("public.nodeConfigFile", () =>
    assertPublicCredentials(env.ADMIN_API_KEY!, env.POSTGRES_PASSWORD!),
  );
  refuse(config.envFile, () => assertPublicReleaseIdentity(env));
  const tls = refuse("public TLS inputs", () =>
    loadPublicTls({
      ...env,
      MIDGARD_PUBLIC_TLS_CERT_FILE: inputs.tlsCertFile,
      MIDGARD_PUBLIC_TLS_KEY_FILE: inputs.tlsKeyFile,
    }),
  );
  const ca = refuse("public.tlsCaFile", () =>
    readMountedHttpFile(inputs.tlsCaFile, false),
  );
  await assertCaVerifiesCertificate(tls, ca, publicOriginHost(env));
  const roots = refuse("public.trustRootsFile", () => {
    const value = JSON.parse(
      readMountedHttpFile(
        inputs.trustRootsFile,
        false,
        PUBLIC_RELEASE_INPUT_LIMITS.trustRoots,
      ).toString("utf8"),
    ) as unknown;
    if (!Array.isArray(value) || value.length === 0 || value.length > 16)
      throw new Error("list one to sixteen release trust roots");
    return value as unknown[];
  });
  await assertSignerIsTrusted(config, roots);
  await assertReleaseDirectory(inputs.releaseDirectory);
  await assertNodeDatabaseOwner(config.nodeRoot);
  const classification = inputs.blueprintClassificationFile;
  refuse("public.blueprintClassificationFile", () => {
    const value = JSON.parse(
      readMountedHttpFile(
        classification,
        false,
        PUBLIC_RELEASE_INPUT_LIMITS.classification,
      ).toString("utf8"),
    ) as unknown;
    if (!value || typeof value !== "object" || Array.isArray(value))
      throw new Error("map each unused validator title to its reason");
  });
  // The reference-script deployment reads the classification from the host.
  env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE = classification;
  const workers = env.VALIDATION_WORKER_POOL_SIZE;
  if (
    resolveValidationWorkerPoolSize(
      workers === undefined ? undefined : Number(workers),
    ) < 2
  )
    throw new Error(
      "A preprod-public node needs VALIDATION_WORKER_POOL_SIZE of at least 2",
    );
  if (env.HTTP_RATE_LIMIT_ENABLED?.trim().toLowerCase() === "false")
    throw new Error("A preprod-public node cannot disable HTTP rate limits");
  if (inputs.ingressHostPort !== null) {
    const taken = [
      Number(new URL(config.endpoint).port),
      config.da.ports.database,
      config.da.ports.retainedTransport,
      ...config.da.members.flatMap((_, index) => [
        config.da.ports.committeeApiBase + index,
        config.da.ports.committeeTransportBase + index,
      ]),
    ];
    if (taken.includes(inputs.ingressHostPort))
      throw new Error(
        "public.ingressHostPort must differ from the node and DA ports",
      );
    await assertIngressSubnetFree(env);
  }
  // Last: the operator and merge wallets must be on the compiled allow-list.
  refuse("public.nodeConfigFile", () =>
    assertPublicOperatorWallets(
      [env.L1_OPERATOR_SEED_PHRASE!, env.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX!],
      "Preprod",
      DEPLOYMENT_PROFILES["preprod-public"] as unknown as PublicHttpProfile,
    ),
  );
}
