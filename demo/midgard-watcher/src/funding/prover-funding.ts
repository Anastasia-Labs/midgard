import type { DatabaseSync } from "node:sqlite";

import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
  deriveDeploymentManifestCardanoProtocolParametersFromOgmios,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  isTransientOgmiosJsonRpcFailure,
  OgmiosJsonRpcError,
} from "@al-ft/midgard-core/ogmios-json-rpc-error";
import { LocalKupmiosTransportUnavailableError } from "@al-ft/midgard-fault-proofs";

import {
  assertVerifiedWatcherDeploymentIdentity,
  assertWatcherDeploymentProtocolParameterAuthority,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentProtocolParameterAuthority,
} from "../runtime/deployment-identity.js";
import { createWatcherProtocolParameterHistoryStorage } from "./prover-funding-parameter-history.js";
import {
  type WatcherProverFundingReservationRecord,
  WatcherProverFundingUnavailableError,
} from "./prover-funding-reservation.js";

export const WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY =
  "midgard-watcher-production-protocol-parameter-runtime-authority-v1" as const;

export type WatcherProtocolParameterRuntimeAuthority = Readonly<{
  schemaVersion: typeof WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY;
  deploymentFingerprint: string;
  source: "local_ogmios" | "signed_deployment" | "authenticated_history";
  sourceEndpoint: string;
  snapshot: DeploymentManifestCardanoProtocolParameters;
  snapshotDigest: string;
  authorityDigest: string;
}>;

const admittedRuntimeAuthorities = new WeakSet<object>();
const refreshRuntimeAuthorities = new WeakMap<
  object,
  () => Promise<WatcherProtocolParameterRuntimeAuthority>
>();

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

/**
 * Throws the funding outage for a JSON-RPC `error` answer whose code says the
 * node cannot answer now (syncing, crossing an era, lost its node). Ogmios's
 * HTTP endpoint answers every JSON-RPC error with HTTP 400, so only the code
 * tells such an answer apart from a refused request; a refusal returns here
 * and stays hard at the caller.
 */
const throwIfTransientJsonRpcAnswer = (body: string): void => {
  let value: unknown;
  try {
    value = JSON.parse(body) as unknown;
  } catch {
    return;
  }
  if (
    typeof value !== "object" ||
    value === null ||
    !Object.prototype.hasOwnProperty.call(value, "error")
  )
    return;
  const message =
    "Current local funding parameters are temporarily unavailable";
  const answer = new OgmiosJsonRpcError(
    "prover funding Ogmios answered with a JSON-RPC error",
    (value as { readonly error: unknown }).error,
  );
  if (!isTransientOgmiosJsonRpcFailure(answer)) return;
  throw new WatcherProverFundingUnavailableError(message, {
    cause: new LocalKupmiosTransportUnavailableError(message, {
      cause: answer,
    }),
  });
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
  let response: Response;
  let body: string;
  try {
    response = await fetchImpl(endpoint, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({
        jsonrpc: "2.0",
        method: "queryLedgerState/protocolParameters",
        id,
      }),
      signal: AbortSignal.timeout(timeoutMs),
    });
    body = await response.text();
  } catch (cause) {
    // This try covers only trusted read transport, never parsers or signers.
    // The transport cause keeps it classified as an L1 transient as well.
    const message =
      "Current local funding parameters are temporarily unavailable";
    throw new WatcherProverFundingUnavailableError(message, {
      cause: new LocalKupmiosTransportUnavailableError(message, { cause }),
    });
  }
  if (!response.ok) {
    const message = `prover funding Ogmios query failed with HTTP ${response.status.toString()}`;
    // A busy or restarting Ogmios, not an answer about the parameters.
    if (
      response.status === 408 ||
      response.status === 425 ||
      response.status === 429 ||
      response.status >= 500
    )
      throw new WatcherProverFundingUnavailableError(message, {
        cause: new LocalKupmiosTransportUnavailableError(message),
      });
    throwIfTransientJsonRpcAnswer(body);
    throw new Error(message);
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
  throwIfTransientJsonRpcAnswer(body);
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
  // The signed snapshot authenticates deployment-time measurements, not future
  // L1 fees. Local native startup continues to authenticate the chain identity.
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
  refreshRuntimeAuthorities.set(authority, () =>
    createRuntimeAuthority({
      deploymentIdentity,
      ogmiosUrl,
      timeoutMs,
      fetchImpl,
    }),
  );
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

/** One bounded local query; transport and admission failures remain failures. */
export const refreshWatcherProtocolParameterRuntimeAuthority = async (
  authority: WatcherProtocolParameterRuntimeAuthority,
): Promise<WatcherProtocolParameterRuntimeAuthority> => {
  assertWatcherProtocolParameterRuntimeAuthority(authority);
  const refresh = refreshRuntimeAuthorities.get(authority);
  if (refresh === undefined)
    throw new Error(
      "Historical funding parameters cannot authorize a fresh live query",
    );
  const current = await refresh();
  return current.snapshotDigest === authority.snapshotDigest
    ? authority
    : current;
};

/** Authenticated historical evidence for exact lease recovery, never fresh funding. */
export const watcherSignedDeploymentProtocolParameterRecoveryAuthority = (
  deploymentIdentity: VerifiedWatcherDeploymentIdentity,
): WatcherProtocolParameterRuntimeAuthority => {
  assertVerifiedWatcherDeploymentIdentity(deploymentIdentity);
  const signed =
    watcherDeploymentProtocolParameterAuthority(deploymentIdentity);
  assertWatcherDeploymentProtocolParameterAuthority(signed);
  const identity = Object.freeze({
    schemaVersion: WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY,
    deploymentFingerprint: deploymentIdentity.manifestId,
    source: "signed_deployment" as const,
    sourceEndpoint: "",
    snapshot: signed.snapshot,
    snapshotDigest: signed.snapshotDigest,
  });
  const authority = Object.freeze({
    ...identity,
    authorityDigest: computeDeploymentManifestJsonDigest(identity),
  });
  admittedRuntimeAuthorities.add(authority);
  return authority;
};

export type WatcherProtocolParameterHistory = Readonly<{
  read(
    record: WatcherProverFundingReservationRecord,
  ): WatcherProtocolParameterRuntimeAuthority | null;
  readCapacity(
    record: WatcherProverFundingReservationRecord,
  ): WatcherProtocolParameterRuntimeAuthority | null;
  rememberCapacity(
    record: WatcherProverFundingReservationRecord,
    authority: WatcherProtocolParameterRuntimeAuthority,
  ): void;
  remember(
    record: WatcherProverFundingReservationRecord,
    authority: WatcherProtocolParameterRuntimeAuthority,
  ): void;
}>;
const admittedParameterHistories = new WeakSet<object>();
export const assertWatcherProtocolParameterHistory = (
  history: WatcherProtocolParameterHistory,
): void => {
  if (!admittedParameterHistories.has(history))
    throw new Error("Funding parameter history is not admitted");
};

/** Only authenticated durable evidence can re-admit a historical local snapshot. */
export const createWatcherProtocolParameterHistory = (input: {
  readonly database: DatabaseSync;
  readonly authenticationKey: Uint8Array;
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
}): WatcherProtocolParameterHistory => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  const storage = createWatcherProtocolParameterHistoryStorage({
    ...input,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
  });
  const read = (
    record: WatcherProverFundingReservationRecord,
    capacity = false,
  ) => {
    const snapshot = capacity
      ? storage.readCapacity(record)
      : storage.read(record);
    if (snapshot === null) return null;
    const identity = Object.freeze({
      schemaVersion: WATCHER_PROTOCOL_PARAMETER_RUNTIME_AUTHORITY,
      deploymentFingerprint: input.deploymentIdentity.manifestId,
      source: "authenticated_history" as const,
      sourceEndpoint: "",
      snapshot,
      snapshotDigest: computeDeploymentManifestJsonDigest(snapshot),
    });
    const authority = Object.freeze({
      ...identity,
      authorityDigest: computeDeploymentManifestJsonDigest(identity),
    });
    admittedRuntimeAuthorities.add(authority);
    return authority;
  };
  const history: WatcherProtocolParameterHistory = Object.freeze({
    read,
    readCapacity: (record) => read(record, true),
    rememberCapacity: (record, authority) => {
      assertWatcherProtocolParameterRuntimeAuthority(authority);
      if (
        authority.source !== "local_ogmios" ||
        authority.deploymentFingerprint !== input.deploymentIdentity.manifestId
      )
        throw new Error(
          "Funding capacity requires admitted live local parameters",
        );
      storage.remember(record, authority.snapshot, true);
    },
    remember: (record, authority) => {
      assertWatcherProtocolParameterRuntimeAuthority(authority);
      if (
        authority.deploymentFingerprint !== input.deploymentIdentity.manifestId
      )
        throw new Error("Funding parameter history changed deployment");
      storage.remember(record, authority.snapshot);
    },
  });
  admittedParameterHistories.add(history);
  return history;
};
