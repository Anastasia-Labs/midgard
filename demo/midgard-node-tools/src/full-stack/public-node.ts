import { createPrivateKey, createPublicKey } from "node:crypto";
import { lstat, readdir, readFile, stat } from "node:fs/promises";
import type { AddressInfo } from "node:net";
import { dirname, isAbsolute, join, relative, resolve } from "node:path";
import { connect, createServer } from "node:tls";

import {
  SELECTED_DEPLOYMENT_PROFILE,
  SELECTED_DEPLOYMENT_PROFILE_DIGEST,
} from "@al-ft/midgard-core/deployment-profile";
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
import { resolveValidationWorkerPoolSize } from "midgard-node/services/config.validation";

import { SIGNED_RELEASE_FILES } from "../sign-deployment-release.js";
import type { StackConfig } from "./config.js";
import { publicOriginHost } from "./node-tls.js";
import { PUBLIC_INPUT_KEYS } from "./public-inputs.js";
import { readOperatorProvenance, stackRegistryPath } from "./release.js";

/** Settings the stack derives for the public node; the .env must not set them. */
const STACK_DERIVED_SETTINGS = [
  "MIDGARD_CONFIG_FILE",
  "HTTP_SOCKET_PATH",
  "MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE",
  "MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE",
  "MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE",
  "MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE",
  "MIDGARD_PUBLIC_MANIFEST_ID",
  "MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE",
  "MIDGARD_BLUEPRINT_CLASSIFICATION_FILE",
];

/** Prefixes the input a node refusal concerns, so the operator knows what to fix. */
function refuse<T>(label: string, check: () => T): T {
  try {
    return check();
  } catch (error) {
    throw new Error(`${label}: ${(error as Error).message}`);
  }
}

export const inside = (path: string, directory: string) => {
  const offset = relative(directory, path);
  return offset === "" || (!offset.startsWith("..") && offset !== path);
};

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
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} stack signs its release with watcher.releaseInput; set it`,
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

/**
 * The release provenance the operator supplies for the public release, read
 * before the deployment exists: everything but its parameter registry
 * digest, which the stack writes from the registry its deployment produces.
 * Its blueprint hash is checked once the blueprint is built (prerequisites).
 */
async function assertOperatorProvenance(
  config: StackConfig,
  env: Record<string, string>,
) {
  const { provenanceFile, parameterRegistryFile } = JSON.parse(
    await readFile(config.watcher.releaseInput!, "utf8"),
  ) as { provenanceFile?: unknown; parameterRegistryFile?: unknown };
  if (typeof provenanceFile !== "string" || !isAbsolute(provenanceFile))
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} release input names its release provenance as an absolute provenanceFile`,
    );
  const registry = stackRegistryPath(config);
  if (parameterRegistryFile !== undefined && parameterRegistryFile !== registry)
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} release signs the parameter registry its deployment writes; leave parameterRegistryFile out or set it to ${registry}`,
    );
  const { provenance } = await readOperatorProvenance(provenanceFile);
  const expected: [string, string | undefined, string, string][] = [
    [
      "sourceRevision",
      provenance.sourceRevision,
      env.MIDGARD_PUBLIC_RELEASE_REVISION!,
      "MIDGARD_PUBLIC_RELEASE_REVISION",
    ],
    [
      "deploymentProfileName",
      provenance.deploymentProfileName,
      SELECTED_DEPLOYMENT_PROFILE.name,
      "the compiled profile",
    ],
    [
      "deploymentProfileDigest",
      provenance.deploymentProfileDigest,
      SELECTED_DEPLOYMENT_PROFILE_DIGEST,
      "the compiled profile",
    ],
    ...(["node", "postgres", "ingress"] as const).map(
      (role) =>
        [
          `roleImages.${role}`,
          provenance.roleImages[role],
          env[`MIDGARD_PUBLIC_${role.toUpperCase()}_IMAGE`]!,
          `MIDGARD_PUBLIC_${role.toUpperCase()}_IMAGE`,
        ] as [string, string | undefined, string, string],
    ),
  ];
  for (const [field, actual, wanted, source] of expected)
    if (actual !== wanted)
      throw new Error(
        `Release provenance ${provenanceFile} has ${field} ${String(actual)}, but ${source} is ${wanted}`,
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
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} stack needs the public section: ${PUBLIC_INPUT_KEYS.join(", ")}`,
    );
  if (new URL(config.endpoint).protocol !== "https:")
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} node serves HTTPS; use an https:// loopback endpoint`,
    );
  assertPublicNodeEnvFile(config.envFile, env, "public.nodeConfigFile");
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
  await assertPrivateFile("public.tlsKeyFile", inputs.tlsKeyFile);
  await loadPublicNodeSecrets(
    "public.nodeConfigFile",
    inputs.nodeConfigFile,
    config.envFile,
    env,
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
  await assertOperatorProvenance(config, env);
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
  assertPublicNodeLimits(env);
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
  }
  // Last: the operator and merge wallets must be on the compiled allow-list.
  assertAllowListedOperatorWallets("public.nodeConfigFile", env);
}

/** A public node's .env holds none of its secrets and none of the settings the stack derives. */
export function assertPublicNodeEnvFile(
  envFile: string,
  env: Record<string, string>,
  nodeConfigLabel: string,
) {
  for (const name of PUBLIC_NODE_SECRET_NAMES)
    if (name in env)
      throw new Error(`Keep ${name} in ${nodeConfigLabel}, not in ${envFile}`);
  for (const name of STACK_DERIVED_SETTINGS)
    if (name in env)
      throw new Error(
        `The stack sets ${name} for a ${SELECTED_DEPLOYMENT_PROFILE.name} node; remove it from ${envFile}`,
      );
  // The node serves only on its socket, where the ingress names each client.
  if ("HTTP_TRUSTED_PROXY_PEERS" in env)
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} node serves only behind its TLS ingress, which names each client; remove HTTP_TRUSTED_PROXY_PEERS from ${envFile}`,
    );
  if ("L1_USER_SEED_PHRASE" in env)
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} operator cannot hold L1_USER_SEED_PHRASE; remove it`,
    );
}

/** Moves a public node's secrets from its private node configuration into `env`. */
export async function loadPublicNodeSecrets(
  label: string,
  nodeConfigFile: string,
  envFile: string,
  env: Record<string, string>,
) {
  await assertPrivateFile(label, nodeConfigFile);
  const fileEnv = refuse(label, () => loadPublicMountedConfig(nodeConfigFile));
  const extra = Object.keys(fileEnv).filter(
    (name) =>
      !["MIDGARD_CONFIG_FILE", "MIDGARD_DOTENV_MODE"].includes(name) &&
      !(PUBLIC_NODE_SECRET_NAMES as readonly string[]).includes(name),
  );
  if (extra.length > 0)
    throw new Error(
      `${label} holds only ${PUBLIC_NODE_SECRET_NAMES.join(", ")}; move ${extra.join(", ")} to ${envFile}`,
    );
  for (const name of PUBLIC_NODE_SECRET_NAMES) env[name] = fileEnv[name]!;
  refuse(label, () =>
    assertPublicCredentials(env.ADMIN_API_KEY!, env.POSTGRES_PASSWORD!),
  );
}

/** The validation capacity and rate limits a public node may not lower. */
export function assertPublicNodeLimits(env: Record<string, string>) {
  const workers = env.VALIDATION_WORKER_POOL_SIZE;
  if (
    resolveValidationWorkerPoolSize(
      workers === undefined ? undefined : Number(workers),
    ) < 2
  )
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} node needs VALIDATION_WORKER_POOL_SIZE of at least 2`,
    );
  if (env.HTTP_RATE_LIMIT_ENABLED?.trim().toLowerCase() === "false")
    throw new Error(
      `A ${SELECTED_DEPLOYMENT_PROFILE.name} node cannot disable HTTP rate limits`,
    );
}

/** The node's operator and merge wallets must be on the compiled allow-list. */
export function assertAllowListedOperatorWallets(
  label: string,
  env: Record<string, string>,
) {
  refuse(label, () =>
    assertPublicOperatorWallets(
      [env.L1_OPERATOR_SEED_PHRASE!, env.L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX!],
      SELECTED_DEPLOYMENT_PROFILE.network,
      SELECTED_DEPLOYMENT_PROFILE as unknown as PublicHttpProfile,
    ),
  );
}
