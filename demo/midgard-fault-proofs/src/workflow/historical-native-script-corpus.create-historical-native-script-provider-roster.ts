import { createHash } from "node:crypto";

import { FraudProofL1UnavailableError } from "./l1-source.js";
import { isNetworkFailure } from "./network-failure.js";
import { type FraudProofRawL1Point } from "./raw-l1-snapshot.js";

export const HISTORICAL_NATIVE_SCRIPT_CORPUS =
  "midgard-production-historical-native-script-corpus-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_CHECKPOINT =
  "midgard-production-historical-native-script-checkpoint-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE =
  "midgard-production-historical-native-script-checkpoint-store-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_MAC_DOMAIN =
  "midgard-production-historical-native-script-checkpoint-mac-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE =
  "midgard-production-historical-native-script-history-source-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD =
  "midgard-production-historical-native-script-history-record-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_PROVIDER_ROSTER =
  "midgard-production-historical-native-script-provider-roster-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_PREIMAGE =
  "midgard-production-historical-native-script-preimage-v1" as const;

export const HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE =
  "midgard-production-historical-native-script-corpus-preimage-v1" as const;

export type HistoricalNativeScriptOccurrence = Readonly<{
  headerHash: string;
  txId: string;
  source: "transaction_witness" | "reference_script";
  itemIndex: number;
}>;

export type HistoricalNativeScriptCorpusEntry = Readonly<{
  scriptHash: string;
  scriptBytesHex: string;
  occurrences: readonly HistoricalNativeScriptOccurrence[];
}>;

export type HistoricalNativeScriptCorpus = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_CORPUS;
  throughHeaderHash: string;
  /** Oldest-to-newest exact hash chain, excluding the all-zero sentinel. */
  headerHashes: readonly string[];
  payloadEnvelopeSha256s: readonly string[];
  entries: readonly HistoricalNativeScriptCorpusEntry[];
  providerRosterDigest: string;
  corpusDigest: string;
  checkpointDigest: string;
  /** Exact challenged history, independent of the current cache checkpoint. */
  evidenceDigest: string;
}>;

export type HistoricalNativeScriptCheckpoint = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_CHECKPOINT;
  deploymentFingerprint: string;
  throughHeaderHash: string;
  throughUtxosRoot: string;
  throughPayloadEnvelopeCborHex: string;
  throughPayloadEnvelopeSha256: string;
  headerHashes: readonly string[];
  payloadEnvelopeSha256s: readonly string[];
  entries: readonly HistoricalNativeScriptCorpusEntry[];
  providerRosterDigest: string;
  predecessorCheckpointDigest: string | null;
  checkpointDigest: string;
}>;

export interface HistoricalNativeScriptCheckpointStore {
  readonly storeVersion: typeof HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE;
  readonly durability:
    | "unsafe_process_memory_test_v1"
    | "authenticated_sqlite_v1";
  load(input: {
    readonly deploymentFingerprint: string;
  }): Promise<unknown | null>;
  compareAndSwap(input: {
    readonly deploymentFingerprint: string;
    readonly expectedCheckpointDigest: string | null;
    readonly next: HistoricalNativeScriptCheckpoint;
  }): Promise<"stored" | "stale">;
}

export const admittedCheckpointStores = new WeakSet<object>();

export const admittedDurableCheckpointStores = new WeakSet<object>();

export type HistoricalNativeScriptHistoryProviderIdentity = Readonly<{
  sourceId: string;
  operatorIdentitySha256: string;
  authorityEndpoint: string;
}>;

export type HistoricalNativeScriptProviderRoster = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_PROVIDER_ROSTER;
  deploymentFingerprint: string;
  sourceMode: "external_provider_quorum";
  consistencyPolicy: "exact_bytes_all_providers_v1";
  providers: readonly HistoricalNativeScriptHistoryProviderIdentity[];
  rosterDigest: string;
}>;

const admittedProviderRosters = new WeakSet<object>();

const providerRosterWithoutDigest = (
  roster: Omit<HistoricalNativeScriptProviderRoster, "rosterDigest">,
) => ({
  schemaVersion: roster.schemaVersion,
  deploymentFingerprint: roster.deploymentFingerprint,
  sourceMode: roster.sourceMode,
  consistencyPolicy: roster.consistencyPolicy,
  providers: roster.providers,
});

/** Freezes the exact external quorum in the verified watcher application overlay. */
export const createHistoricalNativeScriptProviderRoster = ({
  deploymentFingerprint,
  providers,
}: {
  readonly deploymentFingerprint: string;
  readonly providers: readonly HistoricalNativeScriptHistoryProviderIdentity[];
}): HistoricalNativeScriptProviderRoster => {
  if (!/^[0-9a-f]{64}$/u.test(deploymentFingerprint)) {
    throw new Error(
      "historical provider roster deployment fingerprint is invalid",
    );
  }
  if (providers.length < 2 || providers.length > 4) {
    throw new Error(
      "historical provider roster requires two to four providers",
    );
  }
  const sourceIds = new Set<string>();
  const operators = new Set<string>();
  const endpoints = new Set<string>();
  const canonicalProviders = providers.map((provider) => {
    let endpoint: URL;
    try {
      endpoint = new URL(provider.authorityEndpoint);
    } catch {
      throw new Error("historical provider roster endpoint is invalid");
    }
    endpoint.pathname = endpoint.pathname.replace(/\/+$/u, "") || "/";
    const authorityEndpoint = endpoint.toString().replace(/\/$/u, "");
    if (
      provider.sourceId.length === 0 ||
      provider.sourceId.trim() !== provider.sourceId ||
      sourceIds.has(provider.sourceId) ||
      !/^[0-9a-f]{64}$/u.test(provider.operatorIdentitySha256) ||
      operators.has(provider.operatorIdentitySha256) ||
      endpoint.protocol !== "https:" ||
      endpoint.username.length !== 0 ||
      endpoint.password.length !== 0 ||
      endpoint.search.length !== 0 ||
      endpoint.hash.length !== 0 ||
      ["127.0.0.1", "::1", "[::1]", "localhost"].includes(
        endpoint.hostname.toLowerCase(),
      ) ||
      endpoints.has(authorityEndpoint)
    ) {
      throw new Error(
        "historical provider roster identities/endpoints are invalid or not independent",
      );
    }
    sourceIds.add(provider.sourceId);
    operators.add(provider.operatorIdentitySha256);
    endpoints.add(authorityEndpoint);
    return Object.freeze({
      sourceId: provider.sourceId,
      operatorIdentitySha256: provider.operatorIdentitySha256,
      authorityEndpoint,
    });
  });
  const withoutDigest = Object.freeze({
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_PROVIDER_ROSTER,
    deploymentFingerprint,
    sourceMode: "external_provider_quorum" as const,
    consistencyPolicy: "exact_bytes_all_providers_v1" as const,
    providers: Object.freeze(canonicalProviders),
  });
  const roster = Object.freeze({
    ...withoutDigest,
    rosterDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedProviderRosters.add(roster);
  return roster;
};

export const requireHistoricalNativeScriptProviderRoster = (
  providerRoster: HistoricalNativeScriptProviderRoster,
): HistoricalNativeScriptProviderRoster => {
  if (
    !admittedProviderRosters.has(providerRoster) ||
    providerRoster.rosterDigest !==
      createHash("sha256")
        .update(JSON.stringify(providerRosterWithoutDigest(providerRoster)))
        .digest("hex")
  ) {
    throw new Error(
      "historical source requires an admitted immutable provider roster",
    );
  }
  return providerRoster;
};

export interface HistoricalNativeScriptHistoryProvider {
  readonly sourceMode: "local_archival_index" | "external_provider";
  readonly sourceId: string;
  readonly operatorIdentitySha256: string | null;
  readonly authorityEndpoint: string;
  fetchPayloadByHeaderHash(input: {
    readonly deploymentFingerprint: string;
    readonly headerHash: string;
  }): Promise<unknown>;
}

export const admittedHistoryProviders = new WeakSet<object>();

/** Concrete immutable transport created only from an admitted application roster. */
export const createHistoricalNativeScriptHttpHistoryProvider = ({
  sourceMode,
  sourceId,
  authorityEndpoint,
  operatorIdentitySha256,
}: {
  readonly sourceMode: HistoricalNativeScriptHistoryProvider["sourceMode"];
  readonly sourceId: string;
  readonly authorityEndpoint: string;
  readonly operatorIdentitySha256: string | null;
}): HistoricalNativeScriptHistoryProvider => {
  let endpoint: URL;
  try {
    endpoint = new URL(authorityEndpoint);
  } catch {
    throw new Error("historical provider endpoint is not a URL");
  }
  endpoint.hash = "";
  endpoint.search = "";
  endpoint.pathname = endpoint.pathname.replace(/\/+$/u, "") || "/";
  const loopback =
    endpoint.hostname === "127.0.0.1" ||
    endpoint.hostname === "::1" ||
    endpoint.hostname === "localhost";
  if (
    sourceId.length === 0 ||
    sourceId.trim() !== sourceId ||
    (sourceMode === "local_archival_index" &&
      (!loopback || !["http:", "https:"].includes(endpoint.protocol))) ||
    (sourceMode === "external_provider" &&
      (endpoint.protocol !== "https:" || loopback)) ||
    (sourceMode === "local_archival_index"
      ? operatorIdentitySha256 !== null
      : operatorIdentitySha256 === null ||
        !/^[0-9a-f]{64}$/u.test(operatorIdentitySha256))
  ) {
    throw new Error(
      "historical provider identity, endpoint, or mode is invalid",
    );
  }
  const canonicalEndpoint = endpoint.toString().replace(/\/$/u, "");
  const provider: HistoricalNativeScriptHistoryProvider = Object.freeze({
    sourceMode,
    sourceId,
    operatorIdentitySha256,
    authorityEndpoint: canonicalEndpoint,
    fetchPayloadByHeaderHash: async ({
      deploymentFingerprint,
      headerHash,
    }: Parameters<
      HistoricalNativeScriptHistoryProvider["fetchPayloadByHeaderHash"]
    >[0]) => {
      const url = new URL(
        `${canonicalEndpoint}/midgard/v1/historical-payload/${deploymentFingerprint}/${headerHash}`,
      );
      return await fetchHistoricalProviderRecord({ url, sourceId });
    },
  });
  admittedHistoryProviders.add(provider);
  return provider;
};

/**
 * A history provider that did not answer, answered 404 or 408/425/429/5xx.
 * Archive providers restart and are written one after the other, so this is
 * the absence of a record, never evidence about one. It is a transport error
 * to every caller that already waits on one; the exact-bytes comparison
 * between providers is untouched.
 */
export class HistoricalNativeScriptProviderUnavailableError extends FraudProofL1UnavailableError {
  constructor(
    readonly sourceId: string,
    message: string,
    options?: Readonly<{ cause?: unknown }>,
  ) {
    super(message, options);
    this.name = "HistoricalNativeScriptProviderUnavailableError";
  }
}

const HISTORICAL_PROVIDER_DEADLINE_MS = 30_000;
const HISTORICAL_PROVIDER_RETRY_FIRST_MS = 100;
const HISTORICAL_PROVIDER_RETRY_CAP_MS = 2_000;

const transientHistoricalStatus = (status: number): boolean =>
  status === 404 ||
  status === 408 ||
  status === 425 ||
  status === 429 ||
  status >= 500;

/**
 * Reads one provider's record, retrying a transient outcome with capped
 * backoff inside the one 30 s deadline the single read had. Any other HTTP
 * status or an unreadable body fails at once, as before.
 */
export const fetchHistoricalProviderRecord = async ({
  url,
  sourceId,
  fetchImpl = fetch,
  deadlineMs = HISTORICAL_PROVIDER_DEADLINE_MS,
  sleep = async (ms) =>
    await new Promise<void>((resolve) => {
      setTimeout(resolve, ms).unref();
    }),
}: {
  readonly url: URL;
  readonly sourceId: string;
  readonly fetchImpl?: typeof fetch;
  readonly deadlineMs?: number;
  readonly sleep?: (ms: number) => Promise<void>;
}): Promise<unknown> => {
  const signal = AbortSignal.timeout(deadlineMs);
  const startedAt = performance.now();
  let delayMs = HISTORICAL_PROVIDER_RETRY_FIRST_MS;
  let last: Readonly<{ message: string; cause?: unknown }>;
  while (true) {
    try {
      const response = await fetchImpl(url, {
        method: "GET",
        headers: { accept: "application/json" },
        signal,
      });
      if (response.ok) return (await response.json()) as unknown;
      const message = `historical provider ${sourceId} returned HTTP ${response.status.toString()}`;
      await response.body?.cancel().catch(() => undefined);
      if (!transientHistoricalStatus(response.status)) throw new Error(message);
      last = { message };
    } catch (cause) {
      const timedOut =
        signal.aborted ||
        (cause instanceof DOMException && cause.name === "TimeoutError");
      if (!timedOut && !isNetworkFailure(cause)) throw cause;
      last = {
        message: timedOut
          ? `historical provider ${sourceId} did not answer within ${deadlineMs.toString()}ms`
          : `historical provider ${sourceId} transport is unavailable`,
        cause,
      };
      if (timedOut) break;
    }
    const remainingMs = deadlineMs - (performance.now() - startedAt);
    if (remainingMs <= delayMs) break;
    await sleep(delayMs);
    delayMs = Math.min(HISTORICAL_PROVIDER_RETRY_CAP_MS, delayMs * 2);
  }
  throw new HistoricalNativeScriptProviderUnavailableError(
    sourceId,
    last.message,
    last.cause === undefined ? undefined : { cause: last.cause },
  );
};

export interface HistoricalNativeScriptHistorySource {
  readonly sourceVersion: typeof HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE;
  readonly sourceMode: "external_provider_quorum";
  readonly deploymentFingerprint: string;
  readonly providerRosterDigest: string;
  fetchPayloadByHeaderHash(input: { readonly headerHash: string }): Promise<
    Readonly<{
      payloadEnvelopeCbor: Buffer;
      inclusionPoint: FraudProofRawL1Point;
      authorityDigest: string;
    }>
  >;
}
