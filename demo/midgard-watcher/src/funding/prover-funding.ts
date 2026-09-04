import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
  deriveDeploymentManifestCardanoProtocolParametersFromOgmios,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  assertVerifiedWatcherDeploymentIdentity,
  assertWatcherDeploymentProtocolParameterAuthority,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolParameterAuthority,
} from "../runtime/deployment-identity.js";

export const WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY =
  "midgard-watcher-production-protocol-parameter-runtime-authority-v1" as const;

export type WatcherProtocolParameterRuntimeAuthority = Readonly<{
  schemaVersion: typeof WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY;
  deploymentFingerprint: string;
  source: "local_ogmios";
  sourceEndpoint: string;
  snapshot: DeploymentManifestCardanoProtocolParameters;
  snapshotDigest: string;
  authorityDigest: string;
}>;

const admittedRuntimeAuthorities = new WeakSet<object>();

export const assertWatcherProtocolParameterRuntimeAuthority = (
  authority: WatcherProtocolParameterRuntimeAuthority,
): void => {
  if (!admittedRuntimeAuthorities.has(authority)) {
    throw new Error(
      "prover funding protocol-parameter runtime authority is not admitted",
    );
  }
};

const canonicalLoopbackOgmiosUrl = (value: string): string => {
  if (value.trim() !== value) {
    throw new Error("prover funding Ogmios URL is not canonical");
  }
  const parsed = new URL(value);
  if (!/^https?:$/u.test(parsed.protocol)) {
    throw new Error("prover funding requires Ogmios HTTP");
  }
  const hostname = parsed.hostname.toLowerCase();
  if (
    hostname !== "localhost" &&
    hostname !== "127.0.0.1" &&
    hostname !== "::1" &&
    hostname !== "[::1]"
  ) {
    throw new Error("prover funding requires loopback Ogmios");
  }
  if (parsed.username !== "" || parsed.password !== "") {
    throw new Error("prover funding Ogmios URL must not contain credentials");
  }
  parsed.hash = "";
  parsed.pathname = "/";
  parsed.search = "";
  return parsed.toString().replace(/\/$/u, "");
};

const queryLiveProtocolParameters = async ({
  endpoint,
  timeoutMs,
  fetchImpl,
}: {
  readonly endpoint: string;
  readonly timeoutMs: number;
  readonly fetchImpl: typeof fetch;
}): Promise<unknown> => {
  if (
    !Number.isSafeInteger(timeoutMs) ||
    timeoutMs < 100 ||
    timeoutMs > 120_000
  ) {
    throw new Error("prover funding Ogmios timeout is out of bounds");
  }
  const id = "midgard-watcher-prover-funding-parameters-v1";
  const response = await fetchImpl(endpoint, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method: "queryLedgerState/protocolParameters",
      id,
    }),
    signal: AbortSignal.timeout(timeoutMs),
  });
  const body = await response.text();
  if (!response.ok) {
    throw new Error(
      `prover funding Ogmios query failed with HTTP ${response.status.toString()}`,
    );
  }
  let value: unknown;
  try {
    value = JSON.parse(body) as unknown;
  } catch (cause) {
    throw new Error("prover funding Ogmios response is not JSON", { cause });
  }
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype
  ) {
    throw new Error("prover funding Ogmios response is not a plain object");
  }
  const envelope = value as Readonly<Record<string, unknown>>;
  if (
    envelope.jsonrpc !== "2.0" ||
    envelope.id !== id ||
    Object.prototype.hasOwnProperty.call(envelope, "error") ||
    !Object.prototype.hasOwnProperty.call(envelope, "result")
  ) {
    throw new Error("prover funding Ogmios response identity is invalid");
  }
  return value;
};

const createRuntimeAuthority = async ({
  deploymentIdentity,
  ogmiosUrl,
  timeoutMs,
  fetchImpl,
}: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly ogmiosUrl: string;
  readonly timeoutMs: number;
  readonly fetchImpl: typeof fetch;
}): Promise<WatcherProtocolParameterRuntimeAuthority> => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const signed =
    watcherDeploymentProtocolParameterAuthority(deploymentIdentity);
  assertWatcherDeploymentProtocolParameterAuthority(signed);
  const endpoint = canonicalLoopbackOgmiosUrl(ogmiosUrl);
  const live = deriveDeploymentManifestCardanoProtocolParametersFromOgmios(
    await queryLiveProtocolParameters({ endpoint, timeoutMs, fetchImpl }),
  );
  const liveDigest = computeDeploymentManifestJsonDigest(live);
  if (
    liveDigest !== signed.snapshotDigest ||
    computeDeploymentManifestJsonDigest(signed.snapshot) !== liveDigest
  ) {
    throw new Error(
      "live local-node protocol parameters differ from the signed deployment",
    );
  }
  const identity = Object.freeze({
    schemaVersion: WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY,
    deploymentFingerprint: deploymentIdentity.manifestId,
    source: "local_ogmios" as const,
    sourceEndpoint: endpoint,
    snapshot: live,
    snapshotDigest: liveDigest,
  });
  const authority = Object.freeze({
    ...identity,
    authorityDigest: computeDeploymentManifestJsonDigest(identity),
  });
  admittedRuntimeAuthorities.add(authority);
  return authority;
};

export const createWatcherProtocolParameterRuntimeAuthority = async (
  input: Readonly<{
    deploymentIdentity: VerifiedWatcherDeploymentIdentity;
    ogmiosUrl: string;
    timeoutMs: number;
  }>,
): Promise<WatcherProtocolParameterRuntimeAuthority> =>
  await createRuntimeAuthority({ ...input, fetchImpl: fetch });

/** Narrow transport seam. It cannot admit a structural deployment identity. */
export const unsafeCreateWatcherProtocolParameterRuntimeAuthorityForTest =
  async (
    input: Readonly<{
      deploymentIdentity: VerifiedWatcherDeploymentIdentity;
      ogmiosUrl: string;
      timeoutMs: number;
      fetchImpl: typeof fetch;
    }>,
  ): Promise<WatcherProtocolParameterRuntimeAuthority> =>
    await createRuntimeAuthority(input);
