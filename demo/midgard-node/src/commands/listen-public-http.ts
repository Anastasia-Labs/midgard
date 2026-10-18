import { X509Certificate } from "node:crypto";
import { closeSync, constants, fstatSync, openSync, readSync } from "node:fs";
import { createServer as createHttpServer, type Server } from "node:http";
import { createServer as createHttpsServer } from "node:https";
import { isAbsolute } from "node:path";
import { createSecureContext } from "node:tls";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type DeploymentProfile,
  SELECTED_DEPLOYMENT_PROFILE,
} from "@al-ft/midgard-core/deployment-profile";
import {
  type DeploymentReleaseTrustRoot,
  verifyPublicDeploymentRelease,
} from "@al-ft/midgard-core/deployment-release";
import { loadRuntimeConfig } from "@al-ft/midgard-core/runtime-config";
import { getAddressDetails } from "@lucid-evolution/lucid";

import {
  type NodeConfigDep,
  seedPhraseAddress,
} from "../services/config.node-config-dep.js";

// The current genuine public120 constructor has525 contracts: serialized manifest
//10,057,128B, parameter registry20,226,934B and classification72,078B. Each
// artifact has its own bounded envelope; secrets/config/certificates remain64KiB.
export const PUBLIC_RELEASE_INPUT_LIMITS = Object.freeze({
  authority: 32 * 1_048_576,
  finalizedManifest: 16 * 1_048_576,
  parameterRegistry: 32 * 1_048_576,
  classification: 128 * 1_024,
  trustRoots: 16 * 1_024,
  provenance: 65_536,
  contractDeploymentInfo: 16 * 1_048_576,
});
export const PUBLIC_RELEASE_JSON_MAX_BYTES =
  PUBLIC_RELEASE_INPUT_LIMITS.parameterRegistry;

export const isPublicDeploymentProfile = (name: string): boolean =>
  name === "mainnet" || name === "preprod-public";
export class PublicHttpConfigError extends Error {
  constructor(readonly reason: string) {
    super(`E_PUBLIC_HTTP_CONFIG: ${reason}`);
  }
}
const fail = (reason: string): never => {
  throw new PublicHttpConfigError(reason);
};

/** Read fixed-size regular mounted files; file contents never enter diagnostics. */
export const readMountedHttpFile = (
  path: string | undefined,
  secret: boolean,
  maxBytes = 65_536,
): Buffer => {
  if (path === undefined || !isAbsolute(path))
    return fail("a mounted absolute file path is required");
  if (
    !Number.isSafeInteger(maxBytes) ||
    maxBytes < 1 ||
    maxBytes > PUBLIC_RELEASE_JSON_MAX_BYTES
  )
    return fail("mounted file limit is invalid");
  let fd: number | undefined;
  try {
    fd = openSync(path, constants.O_RDONLY | constants.O_NOFOLLOW);
    const stat = fstatSync(fd);
    if (
      !stat.isFile() ||
      stat.size === 0 ||
      stat.size > maxBytes ||
      (secret && (stat.mode & 0o077) !== 0)
    )
      return fail(
        "mounted secret files require private permissions and bounded nonempty content",
      );
    const buffer = Buffer.alloc(maxBytes + 1);
    let count = 0;
    while (count < buffer.length) {
      const read = readSync(fd, buffer, count, buffer.length - count, null);
      if (read === 0) break;
      count += read;
    }
    if (count === 0 || count > maxBytes)
      return fail("mounted file exceeds its content limit");
    return buffer.subarray(0, count);
  } catch {
    return fail(
      "mounted file is missing, unreadable or invalid (contents redacted)",
    );
  } finally {
    if (fd !== undefined) closeSync(fd);
  }
};

const mountedReleaseJson = (
  path: string | undefined,
  maxBytes: number,
): unknown => {
  try {
    return JSON.parse(
      readMountedHttpFile(path, false, maxBytes).toString("utf8"),
    ) as unknown;
  } catch {
    return fail("mounted public release JSON is invalid (contents redacted)");
  }
};
/** Compare mounted parameter/classification bytes to the authenticated release authority.
 * Actual script reconstruction remains the strict deployment constructor's responsibility.
 */
export const verifyMountedPublicRegistry = (
  parameters: unknown,
  classification: unknown,
  expectedRelease: Readonly<{
    manifestId: string;
    blueprintHash: string;
    parameterRegistryDigest: string;
  }>,
): void => {
  if (
    typeof parameters !== "object" ||
    parameters === null ||
    !("schemaVersion" in parameters) ||
    parameters.schemaVersion !== "midgard-deployment-parameter-registry-v1" ||
    !("manifestId" in parameters) ||
    parameters.manifestId !== expectedRelease.manifestId ||
    !("blueprintHash" in parameters) ||
    parameters.blueprintHash !== expectedRelease.blueprintHash ||
    computeDeploymentManifestJsonDigest(parameters) !==
      expectedRelease.parameterRegistryDigest
  )
    return fail(
      "mounted parameter registry differs from signed public release provenance",
    );
  if (
    typeof classification !== "object" ||
    classification === null ||
    Array.isArray(classification) ||
    Object.keys(classification).length > 1024 ||
    Object.values(classification).some(
      (reason: unknown) =>
        typeof reason !== "string" || reason.trim().length === 0,
    )
  )
    return fail("explicit mounted blueprint classification is required");
  if (
    !("deployment" in parameters) ||
    typeof parameters.deployment !== "object" ||
    parameters.deployment === null ||
    !("validators" in parameters.deployment) ||
    !Array.isArray(parameters.deployment.validators)
  )
    return fail(
      "mounted parameter registry has no complete validator classification",
    );
  const expected = new Map<string, string>();
  const validators: readonly unknown[] = parameters.deployment.validators;
  for (const validator of validators) {
    if (
      typeof validator !== "object" ||
      validator === null ||
      !("disposition" in validator)
    )
      return fail("mounted validator classification is invalid");
    if (validator.disposition !== "not_deployed") continue;
    if (
      !("title" in validator) ||
      !("reason" in validator) ||
      typeof validator.title !== "string" ||
      typeof validator.reason !== "string" ||
      validator.reason.trim().length === 0 ||
      expected.has(validator.title)
    )
      return fail("mounted validator classification is invalid");
    expected.set(validator.title, validator.reason);
  }
  if (
    Object.keys(classification).length !== expected.size ||
    Object.entries(classification).some(
      ([title, reason]) => expected.get(title) !== reason,
    )
  )
    return fail(
      "mounted blueprint classification differs from the signed parameter registry",
    );
};

export const authenticatePublicHttpRelease = (env: NodeJS.ProcessEnv): void => {
  const authority = mountedReleaseJson(
    env.MIDGARD_PUBLIC_RELEASE_AUTHORITY_FILE,
    PUBLIC_RELEASE_INPUT_LIMITS.authority,
  );
  const roots = mountedReleaseJson(
    env.MIDGARD_PUBLIC_RELEASE_TRUST_ROOTS_FILE,
    PUBLIC_RELEASE_INPUT_LIMITS.trustRoots,
  );
  if (
    typeof authority !== "object" ||
    authority === null ||
    !("signedIdentity" in authority) ||
    !Array.isArray(roots) ||
    roots.length === 0 ||
    roots.length > 16
  )
    return fail("public signed identity and external trust roots are required");
  const trustRoots: DeploymentReleaseTrustRoot[] = roots.map(
    (entry: unknown) => {
      if (
        typeof entry !== "object" ||
        entry === null ||
        Object.keys(entry).sort().join(",") !==
          "publicKeySpkiDerHex,trustRootId" ||
        !("publicKeySpkiDerHex" in entry) ||
        !("trustRootId" in entry) ||
        typeof entry.publicKeySpkiDerHex !== "string" ||
        typeof entry.trustRootId !== "string"
      )
        return fail("external public release trust roots are invalid");
      return {
        trustRootId: entry.trustRootId,
        publicKeySpkiDerHex: entry.publicKeySpkiDerHex,
      };
    },
  );
  try {
    const release = verifyPublicDeploymentRelease({
      signedIdentity: authority.signedIdentity,
      trustRoots,
      provenance: mountedReleaseJson(
        env.MIDGARD_PUBLIC_RELEASE_PROVENANCE_FILE,
        PUBLIC_RELEASE_INPUT_LIMITS.provenance,
      ),
      finalizedManifest: mountedReleaseJson(
        env.MIDGARD_PUBLIC_FINALIZED_MANIFEST_FILE,
        PUBLIC_RELEASE_INPUT_LIMITS.finalizedManifest,
      ),
      expected: {
        sourceRevision: env.MIDGARD_PUBLIC_RELEASE_REVISION ?? "",
        manifestId: env.MIDGARD_PUBLIC_MANIFEST_ID ?? "",
        roleImages: {
          node: env.MIDGARD_PUBLIC_NODE_IMAGE ?? "",
          postgres: env.MIDGARD_PUBLIC_POSTGRES_IMAGE ?? "",
          ingress: env.MIDGARD_PUBLIC_INGRESS_IMAGE ?? "",
        },
      },
    });
    verifyMountedPublicRegistry(
      mountedReleaseJson(
        env.MIDGARD_DEPLOYMENT_PARAMETER_REGISTRY_FILE,
        PUBLIC_RELEASE_INPUT_LIMITS.parameterRegistry,
      ),
      mountedReleaseJson(
        env.MIDGARD_BLUEPRINT_CLASSIFICATION_FILE,
        PUBLIC_RELEASE_INPUT_LIMITS.classification,
      ),
      {
        manifestId: release.manifestId,
        blueprintHash: release.provenance.blueprintHash,
        parameterRegistryDigest:
          release.provenance.evidence.parameterRegistryDigest,
      },
    );
  } catch {
    return fail(
      "public release signature, compiled profile, provenance or configured identity is invalid",
    );
  }
};

export type PublicHttpProfile = Omit<DeploymentProfile, "allow_lists"> & {
  readonly allow_lists: { readonly operators: readonly string[] | null };
};

export type HttpListenerTransport = {
  readonly createServer: () => Server;
  readonly hostname: string;
};

/** Resolve and authenticate local public settings before acquiring any listener or provider. */
export const publicHttpTransport = (
  config: NodeConfigDep,
  {
    env = process.env,
    profile = SELECTED_DEPLOYMENT_PROFILE,
    testOnlyReleaseVerifier,
  }: {
    readonly env?: NodeJS.ProcessEnv;
    readonly profile?: PublicHttpProfile;
    readonly testOnlyReleaseVerifier?: (env: NodeJS.ProcessEnv) => void;
  } = {},
): HttpListenerTransport => {
  if (
    testOnlyReleaseVerifier !== undefined &&
    (isPublicDeploymentProfile(SELECTED_DEPLOYMENT_PROFILE.name) ||
      profile === SELECTED_DEPLOYMENT_PROFILE)
  )
    return fail(
      "test release dependencies are unavailable in the compiled public runtime",
    );
  if (!isPublicDeploymentProfile(profile.name))
    return { createServer: createHttpServer, hostname: "0.0.0.0" };
  if (env.MIDGARD_DOTENV_MODE !== "disabled")
    return fail("public deployments require disabled checkout dotenv loading");
  if (
    env.MIDGARD_CONFIG_MODE !== undefined &&
    env.MIDGARD_CONFIG_MODE !== "enabled"
  )
    return fail("public mounted configuration loading cannot be disabled");
  readMountedHttpFile(env.MIDGARD_CONFIG_FILE, true);
  const fileEnv: NodeJS.ProcessEnv = {
    MIDGARD_CONFIG_FILE: env.MIDGARD_CONFIG_FILE,
    MIDGARD_DOTENV_MODE: "disabled",
  };
  try {
    loadRuntimeConfig({ env: fileEnv });
  } catch {
    return fail("mounted public configuration is invalid (contents redacted)");
  }
  const secretNames = [
    "ADMIN_API_KEY",
    "POSTGRES_PASSWORD",
    "L1_OPERATOR_SEED_PHRASE",
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
    "L1_REFERENCE_SCRIPT_SEED_PHRASE",
    "L1_SETTLEMENT_SEED_PHRASE",
  ] as const;
  for (const name of secretNames) {
    if (fileEnv[name] === undefined || fileEnv[name] !== config[name])
      return fail(`public ${name} must come from the mounted configuration`);
  }
  if (
    !/^[0-9a-f]{64,128}$/u.test(config.ADMIN_API_KEY) ||
    /^(.)\1+$/u.test(config.ADMIN_API_KEY)
  )
    return fail(
      "admin credentials require a random 32-byte or stronger hex key",
    );
  if (
    config.POSTGRES_PASSWORD.length < 24 ||
    /^(postgres|password|changeme|admin)/iu.test(config.POSTGRES_PASSWORD) ||
    /^(.)\1+$/u.test(config.POSTGRES_PASSWORD)
  )
    return fail("database credentials must be nondefault and strong");
  if (
    fileEnv.L1_USER_SEED_PHRASE !== undefined ||
    env.L1_USER_SEED_PHRASE !== undefined
  )
    return fail("the public operator cannot hold a user wallet seed");
  for (const name of [
    "L1_OPERATOR_SEED_PHRASE",
    "L1_OPERATOR_SEED_PHRASE_FOR_MERGE_TX",
  ] as const) {
    const credential = getAddressDetails(
      seedPhraseAddress(config[name], config.NETWORK),
    ).paymentCredential;
    if (
      credential?.type !== "Key" ||
      (profile.allow_lists.operators !== null &&
        !profile.allow_lists.operators.some((hash) => hash === credential.hash))
    )
      return fail("operator wallet must match the compiled public allow-list");
  }
  if (
    config.RUN_GENESIS_ON_STARTUP ||
    config.VALIDATION_WORKER_POOL_SIZE < 2 ||
    config.DEPLOYMENT_ECONOMICS_PROFILE !== profile.economics.profile ||
    profile.economics.requiredBondLovelace <= 0
  )
    return fail(
      "public runtime requires strict economics, empty genesis and reserved validation capacity",
    );
  if (!/^[0-9a-f]{40}$/u.test(env.MIDGARD_PUBLIC_RELEASE_REVISION ?? ""))
    return fail("an exact public release revision is required");
  for (const name of [
    "MIDGARD_PUBLIC_NODE_IMAGE",
    "MIDGARD_PUBLIC_POSTGRES_IMAGE",
    "MIDGARD_PUBLIC_INGRESS_IMAGE",
  ] as const) {
    if (!/^[-a-zA-Z0-9._/:]+@sha256:[0-9a-f]{64}$/u.test(env[name] ?? ""))
      return fail(`${name} must be digest-pinned`);
  }
  if (env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH === undefined)
    return fail("the deployed contract identity must be mounted explicitly");
  readMountedHttpFile(
    env.MIDGARD_CONTRACT_DEPLOYMENT_INFO_PATH,
    false,
    PUBLIC_RELEASE_INPUT_LIMITS.contractDeploymentInfo,
  );
  (testOnlyReleaseVerifier ?? authenticatePublicHttpRelease)(env);
  let origin: URL;
  try {
    origin = new URL(env.MIDGARD_PUBLIC_API_ORIGIN ?? "");
  } catch {
    return fail("an explicit HTTPS public origin is required");
  }
  if (
    origin.protocol !== "https:" ||
    origin.port !== "" ||
    origin.username !== "" ||
    origin.password !== "" ||
    origin.pathname !== "/" ||
    origin.search !== "" ||
    origin.hash !== ""
  )
    return fail(
      "public origin must be an HTTPS origin without credentials, path or query",
    );
  const cert = readMountedHttpFile(env.MIDGARD_PUBLIC_TLS_CERT_FILE, false);
  const key = readMountedHttpFile(env.MIDGARD_PUBLIC_TLS_KEY_FILE, true);
  try {
    const identity = new X509Certificate(cert);
    if (
      identity.checkHost(origin.hostname) === undefined ||
      Date.parse(identity.validFrom) > Date.now() ||
      Date.parse(identity.validTo) <= Date.now()
    )
      return fail(
        "TLS certificate must be current and cover the public origin",
      );
    createSecureContext({ cert, key, minVersion: "TLSv1.3" });
  } catch {
    return fail(
      "TLS certificate and private key must match the public origin (contents redacted)",
    );
  }
  return {
    hostname: "0.0.0.0",
    createServer: () => {
      const server = createHttpsServer({
        cert,
        key,
        minVersion: "TLSv1.3",
        requestTimeout: 30_000,
        headersTimeout: 10_000,
        maxHeaderSize: 8_192,
      });
      server.maxConnections = 256;
      server.keepAliveTimeout = 5_000;
      server.maxRequestsPerSocket = 100;
      return server;
    },
  };
};
