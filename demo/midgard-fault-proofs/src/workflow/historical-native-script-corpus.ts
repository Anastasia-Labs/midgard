import { createHash, createHmac, timingSafeEqual } from "node:crypto";
import { existsSync, lstatSync, mkdirSync, realpathSync } from "node:fs";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  decodeMidgardAddressBytes,
  decodeMidgardFieldPreimage,
  decodeMidgardLedgerOutputCommitment,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  decodeMidgardVersionedScriptListPreimage,
  hashMidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  assertSecurityGradeEvidence,
  GENESIS_HEADER_HASH,
  MIN_ADA_VIOLATION_ID,
  MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
  missingNativeScriptIsAbsent,
  missingNativeScriptTxVersionedScriptHash,
} from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerEntryOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  reconstructDaPayload,
  type TransitionTraceReconstruction,
} from "../transition-trace/reconstruct.js";
import type { CanonicalViolationDetection } from "./classification.js";
import {
  admitFraudProofRawL1Point,
  type FraudProofRawL1Point,
} from "./raw-l1-snapshot.js";

export const HISTORICAL_NATIVE_SCRIPT_CORPUS =
  "midgard-production-historical-native-script-corpus-v1" as const;
export const HISTORICAL_NATIVE_SCRIPT_CHECKPOINT =
  "midgard-production-historical-native-script-checkpoint-v1" as const;
export const HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE =
  "midgard-production-historical-native-script-checkpoint-store-v1" as const;
const HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_MAC_DOMAIN =
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

const admittedCheckpointStores = new WeakSet<object>();
const admittedDurableCheckpointStores = new WeakSet<object>();

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

const admittedHistoryProviders = new WeakSet<object>();

/** Concrete immutable transport created only from an admitted application roster. */
const createHistoricalNativeScriptHttpHistoryProvider = ({
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
      const response = await fetch(url, {
        method: "GET",
        headers: { accept: "application/json" },
        signal: AbortSignal.timeout(30_000),
      });
      if (!response.ok) {
        throw new Error(
          `historical provider ${sourceId} returned HTTP ${response.status.toString()}`,
        );
      }
      return (await response.json()) as unknown;
    },
  });
  admittedHistoryProviders.add(provider);
  return provider;
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

const admittedHistorySources = new WeakSet<object>();

const exactHistoryRecord = ({
  value,
  provider,
  deploymentFingerprint,
  headerHash,
  providerRosterDigest,
}: {
  readonly value: unknown;
  readonly provider: HistoricalNativeScriptHistoryProvider;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly providerRosterDigest: string;
}) => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length ||
    Object.keys(value).sort().join(",") !==
      "deploymentFingerprint,headerHash,inclusionPoint,payloadEnvelopeCborHex,schemaVersion"
  ) {
    throw new Error(
      `historical source ${provider.sourceId} returned a non-record`,
    );
  }
  const parsed = value as Readonly<Record<string, unknown>>;
  if (
    Object.keys(parsed).sort().join(",") !==
      "deploymentFingerprint,headerHash,inclusionPoint,payloadEnvelopeCborHex,schemaVersion" ||
    parsed.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_HISTORY_RECORD ||
    parsed.deploymentFingerprint !== deploymentFingerprint ||
    parsed.headerHash !== headerHash ||
    typeof parsed.payloadEnvelopeCborHex !== "string" ||
    !/^(?:[0-9a-f]{2})+$/u.test(parsed.payloadEnvelopeCborHex)
  ) {
    throw new Error(
      `historical source ${provider.sourceId} changed the deployment/header or raw payload shape`,
    );
  }
  const inclusionPoint = admitFraudProofRawL1Point(
    parsed.inclusionPoint,
    `historical source ${provider.sourceId} inclusion point`,
  );
  const payloadEnvelopeCbor = Buffer.from(parsed.payloadEnvelopeCborHex, "hex");
  const candidate = Object.freeze({
    payloadEnvelopeCbor,
    inclusionPoint,
    authorityDigest: createHash("sha256")
      .update(
        JSON.stringify({
          deploymentFingerprint,
          providerRosterDigest,
          headerHash,
          payloadEnvelopeCborHex: parsed.payloadEnvelopeCborHex,
          inclusionPoint,
        }),
      )
      .digest("hex"),
  });
  return candidate;
};

/** Admits the immutable external archive quorum frozen by the application. */
export const createHistoricalNativeScriptHistorySource = ({
  providerRoster,
}: {
  readonly providerRoster: HistoricalNativeScriptProviderRoster;
}): HistoricalNativeScriptHistorySource => {
  requireHistoricalNativeScriptProviderRoster(providerRoster);
  const deploymentFingerprint = providerRoster.deploymentFingerprint;
  const providers = providerRoster.providers.map((provider) =>
    createHistoricalNativeScriptHttpHistoryProvider({
      sourceMode: "external_provider",
      sourceId: provider.sourceId,
      authorityEndpoint: provider.authorityEndpoint,
      operatorIdentitySha256: provider.operatorIdentitySha256,
    }),
  );
  const source: HistoricalNativeScriptHistorySource = Object.freeze({
    sourceVersion: HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE,
    sourceMode: "external_provider_quorum",
    deploymentFingerprint,
    providerRosterDigest: providerRoster.rosterDigest,
    fetchPayloadByHeaderHash: async ({
      headerHash,
    }: Parameters<
      HistoricalNativeScriptHistorySource["fetchPayloadByHeaderHash"]
    >[0]) => {
      if (!/^[0-9a-f]{56}$/u.test(headerHash)) {
        throw new Error("historical source header hash is invalid");
      }
      const candidates = await Promise.all(
        providers.map(async (provider) =>
          exactHistoryRecord({
            value: await provider.fetchPayloadByHeaderHash({
              deploymentFingerprint,
              headerHash,
            }),
            provider,
            deploymentFingerprint,
            headerHash,
            providerRosterDigest: providerRoster.rosterDigest,
          }),
        ),
      );
      const first = candidates[0]!;
      if (
        candidates.some(
          (candidate) => candidate.authorityDigest !== first.authorityDigest,
        )
      ) {
        throw new Error(
          "historical external providers disagree on exact raw history",
        );
      }
      return first;
    },
  });
  admittedHistorySources.add(source);
  return source;
};

/** Explicitly volatile test seam; production authority rejects this brand. */
export const unsafeCreateInMemoryHistoricalNativeScriptCheckpointStoreForTest =
  (): HistoricalNativeScriptCheckpointStore => {
    let checkpoint: HistoricalNativeScriptCheckpoint | null = null;
    const store: HistoricalNativeScriptCheckpointStore = Object.freeze({
      storeVersion: HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE,
      durability: "unsafe_process_memory_test_v1",
      load: async () => checkpoint,
      compareAndSwap: async ({
        deploymentFingerprint,
        expectedCheckpointDigest,
        next,
      }: Parameters<
        HistoricalNativeScriptCheckpointStore["compareAndSwap"]
      >[0]) => {
        if (next.deploymentFingerprint !== deploymentFingerprint) {
          throw new Error("historical checkpoint changed deployment identity");
        }
        if (
          (checkpoint?.checkpointDigest ?? null) !== expectedCheckpointDigest
        ) {
          return "stale";
        }
        checkpoint = next;
        return "stored";
      },
    });
    admittedCheckpointStores.add(store);
    return store;
  };

/**
 * Durable local archival-index checkpoint. SQLite's BEGIN IMMEDIATE gives the
 * deployment row a real compare-and-swap boundary across watcher processes.
 */
export const createSqliteHistoricalNativeScriptCheckpointStore = ({
  path,
  rollbackAuthenticationKey,
}: {
  readonly path: string;
  /** Authenticated key exposed by the watcher trusted-head runtime. */
  readonly rollbackAuthenticationKey: Uint8Array;
}): HistoricalNativeScriptCheckpointStore => {
  if (
    !isAbsolute(path) ||
    normalize(path) !== path ||
    path === "/" ||
    path === "/tmp" ||
    path.startsWith("/tmp/")
  ) {
    throw new Error(
      "historical native-script checkpoint requires a canonical durable SQLite path",
    );
  }
  if (rollbackAuthenticationKey.byteLength !== 32) {
    throw new Error(
      "historical native-script checkpoint rollback authentication key must be 32 bytes",
    );
  }
  const checkpointAuthenticationKey = createHmac(
    "sha256",
    Buffer.from(rollbackAuthenticationKey),
  )
    .update(HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_MAC_DOMAIN)
    .digest();
  const checkpointAuthenticationKeyId = createHash("sha256")
    .update(checkpointAuthenticationKey)
    .digest("hex");
  const checkpointAuthenticationMac = (
    deploymentFingerprint: string,
    checkpointJson: string,
  ): string =>
    createHmac("sha256", checkpointAuthenticationKey)
      .update(
        `${HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE}\u0000${checkpointAuthenticationKeyId}\u0000${deploymentFingerprint}\u0000${checkpointJson}`,
      )
      .digest("hex");
  type StoredCheckpointRow = Readonly<{
    checkpoint_digest: string;
    checkpoint_json: string;
    checkpoint_authentication_key_id: string;
    checkpoint_authentication_mac: string;
  }>;
  const requireAuthenticatedStoredCheckpointRow = (
    deploymentFingerprint: string,
    row: StoredCheckpointRow,
  ): unknown => {
    const expected = Buffer.from(
      checkpointAuthenticationMac(deploymentFingerprint, row.checkpoint_json),
      "hex",
    );
    const claimed = Buffer.from(row.checkpoint_authentication_mac, "hex");
    if (
      row.checkpoint_authentication_key_id !== checkpointAuthenticationKeyId ||
      claimed.byteLength !== expected.byteLength ||
      !timingSafeEqual(claimed, expected)
    ) {
      throw new Error(
        "historical native-script checkpoint authentication failed",
      );
    }
    return JSON.parse(row.checkpoint_json) as unknown;
  };
  const directory = dirname(path);
  mkdirSync(directory, { recursive: true, mode: 0o700 });
  if (realpathSync(directory) !== directory) {
    throw new Error(
      "historical native-script checkpoint directory is not canonical",
    );
  }
  if (
    existsSync(path) &&
    (lstatSync(path).isSymbolicLink() || realpathSync(path) !== path)
  ) {
    throw new Error(
      "historical native-script checkpoint path traverses a symlink",
    );
  }
  const open = (): DatabaseSync => {
    const database = new DatabaseSync(path, {
      open: true,
      readOnly: false,
      enableForeignKeyConstraints: true,
    });
    database.exec(`
      PRAGMA journal_mode = WAL;
      PRAGMA synchronous = FULL;
      PRAGMA trusted_schema = OFF;
      PRAGMA busy_timeout = 5000;
      CREATE TABLE IF NOT EXISTS fraud_proof_native_script_checkpoint_v1 (
        deployment_fingerprint TEXT PRIMARY KEY CHECK(length(deployment_fingerprint) = 64),
        checkpoint_digest TEXT NOT NULL CHECK(length(checkpoint_digest) = 64),
        checkpoint_json TEXT NOT NULL CHECK(length(checkpoint_json) > 0),
        checkpoint_authentication_key_id TEXT NOT NULL CHECK(length(checkpoint_authentication_key_id) = 64),
        checkpoint_authentication_mac TEXT NOT NULL CHECK(length(checkpoint_authentication_mac) = 64)
      ) STRICT;
    `);
    return database;
  };
  const store: HistoricalNativeScriptCheckpointStore = Object.freeze({
    storeVersion: HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE,
    durability: "authenticated_sqlite_v1",
    load: async ({
      deploymentFingerprint,
    }: Parameters<HistoricalNativeScriptCheckpointStore["load"]>[0]) => {
      const database = open();
      try {
        const row = database
          .prepare(
            "SELECT checkpoint_digest, checkpoint_json, checkpoint_authentication_key_id, checkpoint_authentication_mac FROM fraud_proof_native_script_checkpoint_v1 WHERE deployment_fingerprint = ?",
          )
          .get(deploymentFingerprint) as StoredCheckpointRow | undefined;
        if (row === undefined) return null;
        return requireAuthenticatedStoredCheckpointRow(
          deploymentFingerprint,
          row,
        );
      } finally {
        database.close();
      }
    },
    compareAndSwap: async ({
      deploymentFingerprint,
      expectedCheckpointDigest,
      next,
    }: Parameters<
      HistoricalNativeScriptCheckpointStore["compareAndSwap"]
    >[0]) => {
      await requireCheckpoint({ value: next, deploymentFingerprint });
      const database = open();
      try {
        database.exec("BEGIN IMMEDIATE");
        const row = database
          .prepare(
            "SELECT checkpoint_digest, checkpoint_json, checkpoint_authentication_key_id, checkpoint_authentication_mac FROM fraud_proof_native_script_checkpoint_v1 WHERE deployment_fingerprint = ?",
          )
          .get(deploymentFingerprint) as StoredCheckpointRow | undefined;
        if (row !== undefined) {
          requireAuthenticatedStoredCheckpointRow(deploymentFingerprint, row);
        }
        if ((row?.checkpoint_digest ?? null) !== expectedCheckpointDigest) {
          database.exec("ROLLBACK");
          return "stale";
        }
        const checkpointJson = JSON.stringify(next);
        database
          .prepare(
            `INSERT INTO fraud_proof_native_script_checkpoint_v1
                (deployment_fingerprint, checkpoint_digest, checkpoint_json, checkpoint_authentication_key_id, checkpoint_authentication_mac)
               VALUES (?, ?, ?, ?, ?)
               ON CONFLICT(deployment_fingerprint) DO UPDATE SET
                 checkpoint_digest = excluded.checkpoint_digest,
                 checkpoint_json = excluded.checkpoint_json,
                 checkpoint_authentication_key_id = excluded.checkpoint_authentication_key_id,
                 checkpoint_authentication_mac = excluded.checkpoint_authentication_mac`,
          )
          .run(
            deploymentFingerprint,
            next.checkpointDigest,
            checkpointJson,
            checkpointAuthenticationKeyId,
            checkpointAuthenticationMac(deploymentFingerprint, checkpointJson),
          );
        database.exec("COMMIT");
        return "stored";
      } catch (cause) {
        try {
          database.exec("ROLLBACK");
        } catch {
          // The original SQLite failure is the useful diagnostic.
        }
        throw cause;
      } finally {
        database.close();
      }
    },
  });
  admittedCheckpointStores.add(store);
  admittedDurableCheckpointStores.add(store);
  return store;
};

export const requireHistoricalNativeScriptHistoryAuthority = ({
  deploymentFingerprint,
  checkpointStore,
  historySource,
}: {
  readonly deploymentFingerprint: string;
  readonly checkpointStore: HistoricalNativeScriptCheckpointStore;
  readonly historySource: HistoricalNativeScriptHistorySource;
}): Readonly<{ providerRosterDigest: string }> => {
  if (
    !/^[0-9a-f]{64}$/u.test(deploymentFingerprint) ||
    checkpointStore.storeVersion !==
      HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE ||
    checkpointStore.durability !== "authenticated_sqlite_v1" ||
    !admittedCheckpointStores.has(checkpointStore) ||
    !admittedDurableCheckpointStores.has(checkpointStore) ||
    historySource.sourceVersion !== HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE ||
    historySource.sourceMode !== "external_provider_quorum" ||
    historySource.deploymentFingerprint !== deploymentFingerprint ||
    !/^[0-9a-f]{64}$/u.test(historySource.providerRosterDigest) ||
    !admittedHistorySources.has(historySource)
  ) {
    throw new Error(
      "historical native-script authority is not the admitted deployment overlay",
    );
  }
  return Object.freeze({
    providerRosterDigest: historySource.providerRosterDigest,
  });
};

export type AdmittedHistoricalNativeScriptCorpus = Readonly<{
  currentEvidence: CanonicalBlockEvidence;
  /** Oldest-to-newest, including the challenged/current block. */
  reconstructions: readonly TransitionTraceReconstruction[];
}>;

const admittedCorpusInternals = new WeakMap<
  object,
  AdmittedHistoricalNativeScriptCorpus
>();

const sha256 = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

const occurrenceOrder = (
  left: HistoricalNativeScriptOccurrence,
  right: HistoricalNativeScriptOccurrence,
): number =>
  left.headerHash.localeCompare(right.headerHash) ||
  left.txId.localeCompare(right.txId) ||
  left.source.localeCompare(right.source) ||
  left.itemIndex - right.itemIndex;

const buildCorpusEntries = (
  reconstructions: readonly TransitionTraceReconstruction[],
): readonly HistoricalNativeScriptCorpusEntry[] => {
  const scripts = new Map<
    string,
    {
      scriptBytesHex: string;
      occurrences: HistoricalNativeScriptOccurrence[];
    }
  >();
  for (const reconstruction of reconstructions) {
    for (const transaction of reconstruction.transactions) {
      const native = decodeMidgardNativeTxFullFromCanonicalCbor(
        transaction.fullTransactionCbor,
      );
      const record = (
        script: ReturnType<
          typeof decodeMidgardVersionedScriptListPreimage
        >[number],
        occurrence: HistoricalNativeScriptOccurrence,
      ) => {
        if (script.language !== "NativeCardano") return;
        const scriptHash = hashMidgardVersionedScript(script);
        const scriptBytesHex = Buffer.from(script.scriptBytes).toString("hex");
        const existing = scripts.get(scriptHash);
        if (
          existing !== undefined &&
          existing.scriptBytesHex !== scriptBytesHex
        ) {
          throw new Error(
            `historical native-script corpus found conflicting preimages for ${scriptHash}`,
          );
        }
        if (existing === undefined) {
          scripts.set(scriptHash, {
            scriptBytesHex,
            occurrences: [occurrence],
          });
        } else {
          existing.occurrences.push(occurrence);
        }
      };
      decodeMidgardVersionedScriptListPreimage(
        native.witnessSet.scriptTxWitsPreimageCbor,
        `historical transaction ${transaction.txId} script witnesses`,
      ).forEach((script, itemIndex) =>
        record(script, {
          headerHash: reconstruction.headerHash,
          txId: transaction.txId,
          source: "transaction_witness",
          itemIndex,
        }),
      );
      const outputItems = decodeMidgardFieldPreimage(
        native.body.outputsPreimageCbor,
      );
      outputItems.forEach((outputBytes, itemIndex) => {
        const script = decodeMidgardTxOutput(outputBytes).script_ref;
        if (script !== undefined) {
          record(script, {
            headerHash: reconstruction.headerHash,
            txId: transaction.txId,
            source: "reference_script",
            itemIndex,
          });
        }
      });
    }
  }
  return Object.freeze(
    [...scripts.entries()]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([scriptHash, entry]) =>
        Object.freeze({
          scriptHash,
          scriptBytesHex: entry.scriptBytesHex,
          occurrences: Object.freeze(
            [...entry.occurrences]
              .sort(occurrenceOrder)
              .map((occurrence) => Object.freeze({ ...occurrence })),
          ),
        }),
      ),
  );
};

const checkpointWithoutDigest = (
  checkpoint: HistoricalNativeScriptCheckpoint,
) => ({
  schemaVersion: checkpoint.schemaVersion,
  deploymentFingerprint: checkpoint.deploymentFingerprint,
  throughHeaderHash: checkpoint.throughHeaderHash,
  throughUtxosRoot: checkpoint.throughUtxosRoot,
  throughPayloadEnvelopeCborHex: checkpoint.throughPayloadEnvelopeCborHex,
  throughPayloadEnvelopeSha256: checkpoint.throughPayloadEnvelopeSha256,
  headerHashes: checkpoint.headerHashes,
  payloadEnvelopeSha256s: checkpoint.payloadEnvelopeSha256s,
  entries: checkpoint.entries,
  providerRosterDigest: checkpoint.providerRosterDigest,
  predecessorCheckpointDigest: checkpoint.predecessorCheckpointDigest,
});

const checkpointDigestV1 = (
  checkpoint: HistoricalNativeScriptCheckpoint,
): string =>
  createHash("sha256")
    .update(JSON.stringify(checkpointWithoutDigest(checkpoint)))
    .digest("hex");

const requireCheckpoint = async ({
  value,
  deploymentFingerprint,
}: {
  readonly value: unknown;
  readonly deploymentFingerprint: string;
}): Promise<HistoricalNativeScriptCheckpoint | null> => {
  if (value === null) return null;
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length ||
    Object.keys(value).sort().join(",") !==
      "checkpointDigest,deploymentFingerprint,entries,headerHashes,payloadEnvelopeSha256s,predecessorCheckpointDigest,providerRosterDigest,schemaVersion,throughHeaderHash,throughPayloadEnvelopeCborHex,throughPayloadEnvelopeSha256,throughUtxosRoot"
  ) {
    throw new Error(
      "historical native-script checkpoint is not an exact record",
    );
  }
  const checkpoint = value as HistoricalNativeScriptCheckpoint;
  if (
    checkpoint.schemaVersion !== HISTORICAL_NATIVE_SCRIPT_CHECKPOINT ||
    checkpoint.deploymentFingerprint !== deploymentFingerprint ||
    !/^[0-9a-f]{56}$/u.test(checkpoint.throughHeaderHash) ||
    !/^[0-9a-f]{64}$/u.test(checkpoint.throughUtxosRoot) ||
    !/^(?:[0-9a-f]{2})+$/u.test(checkpoint.throughPayloadEnvelopeCborHex) ||
    !/^[0-9a-f]{64}$/u.test(checkpoint.throughPayloadEnvelopeSha256) ||
    !Array.isArray(checkpoint.headerHashes) ||
    !Array.isArray(checkpoint.payloadEnvelopeSha256s) ||
    checkpoint.headerHashes.length === 0 ||
    checkpoint.headerHashes.length !==
      checkpoint.payloadEnvelopeSha256s.length ||
    checkpoint.headerHashes.at(-1) !== checkpoint.throughHeaderHash ||
    checkpoint.payloadEnvelopeSha256s.at(-1) !==
      checkpoint.throughPayloadEnvelopeSha256 ||
    !Array.isArray(checkpoint.entries) ||
    !/^[0-9a-f]{64}$/u.test(checkpoint.providerRosterDigest) ||
    (checkpoint.predecessorCheckpointDigest !== null &&
      !/^[0-9a-f]{64}$/u.test(checkpoint.predecessorCheckpointDigest)) ||
    checkpoint.checkpointDigest !== checkpointDigestV1(checkpoint)
  ) {
    throw new Error(
      "historical native-script checkpoint identity, shape, or digest is invalid",
    );
  }
  const uniqueHeaders = new Set(checkpoint.headerHashes);
  if (
    uniqueHeaders.size !== checkpoint.headerHashes.length ||
    checkpoint.headerHashes.some((hash) => !/^[0-9a-f]{56}$/u.test(hash)) ||
    checkpoint.payloadEnvelopeSha256s.some(
      (hash) => !/^[0-9a-f]{64}$/u.test(hash),
    )
  ) {
    throw new Error("historical native-script checkpoint chain is malformed");
  }
  const envelope = Buffer.from(checkpoint.throughPayloadEnvelopeCborHex, "hex");
  if (sha256(envelope) !== checkpoint.throughPayloadEnvelopeSha256) {
    throw new Error(
      "historical native-script checkpoint payload digest changed",
    );
  }
  const reconstruction = await reconstructDaPayload({
    payloadEnvelopeCbor: envelope,
    expectedHeaderHash: checkpoint.throughHeaderHash,
  });
  if (reconstruction.header.utxosRoot !== checkpoint.throughUtxosRoot) {
    throw new Error("historical native-script checkpoint UTxO root changed");
  }
  return checkpoint;
};

const mergeCorpusEntries = (
  earlier: readonly HistoricalNativeScriptCorpusEntry[],
  later: readonly HistoricalNativeScriptCorpusEntry[],
): readonly HistoricalNativeScriptCorpusEntry[] => {
  const merged = new Map<
    string,
    {
      scriptBytesHex: string;
      occurrences: HistoricalNativeScriptOccurrence[];
    }
  >();
  for (const entry of [...earlier, ...later]) {
    const current = merged.get(entry.scriptHash);
    if (
      current !== undefined &&
      current.scriptBytesHex !== entry.scriptBytesHex
    ) {
      throw new Error(
        `historical native-script checkpoint conflicts at ${entry.scriptHash}`,
      );
    }
    const target = current ?? {
      scriptBytesHex: entry.scriptBytesHex,
      occurrences: [],
    };
    if (current === undefined) merged.set(entry.scriptHash, target);
    for (const occurrence of entry.occurrences) {
      const identity = JSON.stringify(occurrence);
      if (
        !target.occurrences.some(
          (candidate) => JSON.stringify(candidate) === identity,
        )
      ) {
        target.occurrences.push(occurrence);
      }
    }
  }
  return Object.freeze(
    [...merged.entries()]
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([scriptHash, entry]) =>
        Object.freeze({
          scriptHash,
          scriptBytesHex: entry.scriptBytesHex,
          occurrences: Object.freeze(
            [...entry.occurrences]
              .sort(occurrenceOrder)
              .map((occurrence) => Object.freeze({ ...occurrence })),
          ),
        }),
      ),
  );
};

const entriesThroughHeaders = (
  entries: readonly HistoricalNativeScriptCorpusEntry[],
  headers: ReadonlySet<string>,
): readonly HistoricalNativeScriptCorpusEntry[] =>
  Object.freeze(
    entries.flatMap((entry) => {
      const occurrences = entry.occurrences.filter((occurrence) =>
        headers.has(occurrence.headerHash),
      );
      return occurrences.length === 0
        ? []
        : [
            Object.freeze({
              ...entry,
              occurrences: Object.freeze(occurrences),
            }),
          ];
    }),
  );

const fetchHistoricalPayload = async ({
  headerHash,
  sources,
  historySource,
  retries,
}: {
  readonly headerHash: string;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly historySource: HistoricalNativeScriptHistorySource;
  readonly retries?: number;
}): Promise<Buffer> => {
  void retries;
  if (sources.length > 0) {
    const results = await Promise.all(
      sources.map(
        async (source) => await source.fetchPayloadByHeaderHash(headerHash),
      ),
    );
    const successes = results.filter(
      (result): result is Extract<typeof result, { readonly ok: true }> =>
        result.ok,
    );
    if (successes.length > 0) {
      successes.forEach((success) =>
        assertSecurityGradeEvidence(success.provenance),
      );
      const first = successes[0]!.payloadEnvelopeCbor;
      if (
        successes.some((success) => !success.payloadEnvelopeCbor.equals(first))
      ) {
        throw new Error(
          "public retained-DA sources disagree on historical bytes",
        );
      }
      return first;
    }
    const attempts = results.flatMap((result) => result.attempts);
    if (
      attempts.length === 0 ||
      attempts.some((attempt) => attempt.status !== "not_found")
    ) {
      throw new Error(
        "public retained-DA history failed without authenticated retention absence",
      );
    }
  }
  const archived = await historySource.fetchPayloadByHeaderHash({ headerHash });
  return archived.payloadEnvelopeCbor;
};

/**
 * Extends a deployment-bound complete checkpoint with the exact contiguous
 * retained-DA segment that is still available. Bootstrap may walk to genesis,
 * but ordinary derivation never depends on permanent retained DA.
 */
export const resolveHistoricalNativeScriptCorpus = async ({
  deploymentFingerprint,
  checkpointStore,
  historySource,
  currentEvidence,
  sources,
  retries,
}: {
  readonly deploymentFingerprint: string;
  readonly checkpointStore: HistoricalNativeScriptCheckpointStore;
  readonly historySource: HistoricalNativeScriptHistorySource;
  readonly currentEvidence: CanonicalBlockEvidence;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
}): Promise<HistoricalNativeScriptCorpus> => {
  assertSecurityGradeEvidence(currentEvidence.provenance.l1);
  assertSecurityGradeEvidence(currentEvidence.provenance.da);
  requireHistoricalNativeScriptHistoryAuthority({
    deploymentFingerprint,
    checkpointStore,
    historySource,
  });
  const checkpoint = await requireCheckpoint({
    value: await checkpointStore.load({ deploymentFingerprint }),
    deploymentFingerprint,
  });
  if (
    checkpoint !== null &&
    checkpoint.providerRosterDigest !== historySource.providerRosterDigest
  ) {
    throw new Error(
      "historical native-script checkpoint belongs to a different provider roster",
    );
  }
  const currentPayloadEnvelopeCbor = await fetchHistoricalPayload({
    headerHash: currentEvidence.headerHash,
    sources,
    historySource,
    ...(retries === undefined ? {} : { retries }),
  });
  if (
    sha256(currentPayloadEnvelopeCbor) !== currentEvidence.payloadEnvelopeSha256
  ) {
    throw new Error("historical current retained-DA payload changed");
  }
  const storedCurrentIndex =
    checkpoint?.headerHashes.indexOf(currentEvidence.headerHash) ?? -1;
  if (storedCurrentIndex >= 0) {
    if (
      checkpoint!.payloadEnvelopeSha256s[storedCurrentIndex] !==
      currentEvidence.payloadEnvelopeSha256
    ) {
      throw new Error("historical checkpoint disagrees on challenged payload");
    }
    const headers = Object.freeze(
      checkpoint!.headerHashes.slice(0, storedCurrentIndex + 1),
    );
    const payloadEnvelopeSha256s = Object.freeze(
      checkpoint!.payloadEnvelopeSha256s.slice(0, storedCurrentIndex + 1),
    );
    const entries = entriesThroughHeaders(
      checkpoint!.entries,
      new Set(headers),
    );
    const newestFirst = [currentEvidence.reconstruction];
    if (currentEvidence.header.prevHeaderHash !== GENESIS_HEADER_HASH) {
      const payloadEnvelopeCbor = await fetchHistoricalPayload({
        headerHash: currentEvidence.header.prevHeaderHash,
        sources,
        historySource,
        ...(retries === undefined ? {} : { retries }),
      });
      const predecessor = await reconstructDaPayload({
        payloadEnvelopeCbor,
        expectedHeaderHash: currentEvidence.header.prevHeaderHash,
      });
      if (
        predecessor.header.utxosRoot !== currentEvidence.header.prevUtxosRoot
      ) {
        throw new Error("historical challenged predecessor root changed");
      }
      newestFirst.push(predecessor);
    }
    const reconstructions = Object.freeze([...newestFirst].reverse());
    const withoutDigest = {
      schemaVersion: HISTORICAL_NATIVE_SCRIPT_CORPUS,
      throughHeaderHash: currentEvidence.headerHash,
      headerHashes: headers,
      payloadEnvelopeSha256s,
      entries,
      providerRosterDigest: historySource.providerRosterDigest,
      checkpointDigest: checkpoint!.checkpointDigest,
    } as const;
    const corpus = Object.freeze({
      ...withoutDigest,
      corpusDigest: createHash("sha256")
        .update(JSON.stringify(withoutDigest))
        .digest("hex"),
    });
    admittedCorpusInternals.set(corpus, { currentEvidence, reconstructions });
    return corpus;
  }
  const newestFirst: TransitionTraceReconstruction[] = [
    currentEvidence.reconstruction,
  ];
  const envelopeShaByHeader = new Map<string, string>([
    [currentEvidence.headerHash, currentEvidence.payloadEnvelopeSha256],
  ]);
  const seen = new Set<string>([currentEvidence.headerHash]);
  const targetHeaderHash = checkpoint?.throughHeaderHash ?? GENESIS_HEADER_HASH;
  let expectedHeaderHash = currentEvidence.header.prevHeaderHash;
  while (expectedHeaderHash !== targetHeaderHash) {
    if (expectedHeaderHash === GENESIS_HEADER_HASH) {
      throw new Error(
        "historical checkpoint is not an ancestor of the challenged block",
      );
    }
    if (seen.has(expectedHeaderHash)) {
      throw new Error("historical retained-DA header chain contains a cycle");
    }
    seen.add(expectedHeaderHash);
    const payloadEnvelopeCbor = await fetchHistoricalPayload({
      headerHash: expectedHeaderHash,
      sources,
      historySource,
      ...(retries === undefined ? {} : { retries }),
    });
    const reconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor,
      expectedHeaderHash,
    });
    const child = newestFirst[newestFirst.length - 1]!;
    if (
      child.header.prevHeaderHash !== reconstruction.headerHash ||
      child.header.prevUtxosRoot !== reconstruction.header.utxosRoot
    ) {
      throw new Error(
        "historical retained-DA predecessor does not match the child's committed hash/root",
      );
    }
    newestFirst.push(reconstruction);
    envelopeShaByHeader.set(
      reconstruction.headerHash,
      sha256(payloadEnvelopeCbor),
    );
    expectedHeaderHash = reconstruction.header.prevHeaderHash;
  }
  if (checkpoint !== null) {
    const checkpointReconstruction = await reconstructDaPayload({
      payloadEnvelopeCbor: Buffer.from(
        checkpoint.throughPayloadEnvelopeCborHex,
        "hex",
      ),
      expectedHeaderHash: checkpoint.throughHeaderHash,
    });
    const child = newestFirst[newestFirst.length - 1]!;
    if (
      child.header.prevHeaderHash !== checkpointReconstruction.headerHash ||
      child.header.prevUtxosRoot !== checkpointReconstruction.header.utxosRoot
    ) {
      throw new Error(
        "historical checkpoint does not join the retained segment",
      );
    }
    newestFirst.push(checkpointReconstruction);
    envelopeShaByHeader.set(
      checkpoint.throughHeaderHash,
      checkpoint.throughPayloadEnvelopeSha256,
    );
  }
  const reconstructions = Object.freeze([...newestFirst].reverse());
  const appended = reconstructions.filter(
    (reconstruction) =>
      checkpoint === null ||
      reconstruction.headerHash !== checkpoint.throughHeaderHash,
  );
  const headerHashes = Object.freeze([
    ...(checkpoint?.headerHashes ?? []),
    ...appended.map((reconstruction) => reconstruction.headerHash),
  ]);
  const payloadEnvelopeSha256s = Object.freeze([
    ...(checkpoint?.payloadEnvelopeSha256s ?? []),
    ...appended.map(
      (reconstruction) => envelopeShaByHeader.get(reconstruction.headerHash)!,
    ),
  ]);
  const entries = mergeCorpusEntries(
    checkpoint?.entries ?? [],
    buildCorpusEntries(appended),
  );
  const nextWithoutDigest = {
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_CHECKPOINT,
    deploymentFingerprint,
    throughHeaderHash: currentEvidence.headerHash,
    throughUtxosRoot: currentEvidence.header.utxosRoot,
    throughPayloadEnvelopeCborHex: currentPayloadEnvelopeCbor.toString("hex"),
    throughPayloadEnvelopeSha256: currentEvidence.payloadEnvelopeSha256,
    headerHashes,
    payloadEnvelopeSha256s,
    entries,
    providerRosterDigest: historySource.providerRosterDigest,
    predecessorCheckpointDigest: checkpoint?.checkpointDigest ?? null,
  } as const;
  const next = Object.freeze({
    ...nextWithoutDigest,
    checkpointDigest: createHash("sha256")
      .update(JSON.stringify(nextWithoutDigest))
      .digest("hex"),
  });
  if (
    (await checkpointStore.compareAndSwap({
      deploymentFingerprint,
      expectedCheckpointDigest: checkpoint?.checkpointDigest ?? null,
      next,
    })) !== "stored"
  ) {
    throw new Error(
      "historical native-script checkpoint advanced concurrently; refetch required",
    );
  }
  const withoutDigest = {
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_CORPUS,
    throughHeaderHash: currentEvidence.headerHash,
    headerHashes,
    payloadEnvelopeSha256s,
    entries,
    providerRosterDigest: historySource.providerRosterDigest,
    checkpointDigest: next.checkpointDigest,
  } as const;
  const corpus = Object.freeze({
    ...withoutDigest,
    corpusDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedCorpusInternals.set(corpus, {
    currentEvidence,
    reconstructions,
  });
  return corpus;
};

export const requireHistoricalNativeScriptCorpus = (
  corpus: HistoricalNativeScriptCorpus,
): AdmittedHistoricalNativeScriptCorpus => {
  const internals = admittedCorpusInternals.get(corpus);
  if (internals === undefined) {
    throw new Error(
      "historical native-script corpus was not derived from authenticated complete history",
    );
  }
  return internals;
};

export type HistoricalNativeScriptCorpusPreimage = Readonly<{
  schemaVersion: typeof HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE;
  throughHeaderHash: string;
  scriptHash: string;
  scriptBytesHex: string;
  occurrences: readonly HistoricalNativeScriptOccurrence[];
  providerRosterDigest: string;
  corpusDigest: string;
  checkpointDigest: string;
  preimageDigest: string;
}>;

const admittedCorpusPreimages = new WeakSet<object>();

const corpusPreimageWithoutDigest = (
  preimage: Omit<HistoricalNativeScriptCorpusPreimage, "preimageDigest">,
) => ({
  schemaVersion: preimage.schemaVersion,
  throughHeaderHash: preimage.throughHeaderHash,
  scriptHash: preimage.scriptHash,
  scriptBytesHex: preimage.scriptBytesHex,
  occurrences: preimage.occurrences,
  providerRosterDigest: preimage.providerRosterDigest,
  corpusDigest: preimage.corpusDigest,
  checkpointDigest: preimage.checkpointDigest,
});

/** Opaque occurrence-bearing preimage derived only from an admitted corpus. */
export const historicalNativeScriptPreimageFromCorpus = ({
  corpus,
  scriptHash,
}: {
  readonly corpus: HistoricalNativeScriptCorpus;
  readonly scriptHash: string;
}): HistoricalNativeScriptCorpusPreimage | null => {
  requireHistoricalNativeScriptCorpus(corpus);
  if (!/^[0-9a-f]{56}$/u.test(scriptHash)) {
    throw new Error("historical native-script preimage hash is invalid");
  }
  const matches = corpus.entries.filter(
    (candidate) => candidate.scriptHash === scriptHash,
  );
  if (matches.length === 0) return null;
  if (matches.length !== 1) {
    throw new Error(
      "historical native-script corpus contains duplicate preimages",
    );
  }
  const entry = matches[0]!;
  if (
    entry.occurrences.length === 0 ||
    missingNativeScriptTxVersionedScriptHash(
      Buffer.from(entry.scriptBytesHex, "hex"),
    ) !== scriptHash
  ) {
    throw new Error(
      "historical native-script corpus preimage/hash binding changed",
    );
  }
  const withoutDigest = Object.freeze({
    schemaVersion: HISTORICAL_NATIVE_SCRIPT_CORPUS_PREIMAGE,
    throughHeaderHash: corpus.throughHeaderHash,
    scriptHash,
    scriptBytesHex: entry.scriptBytesHex,
    occurrences: Object.freeze(
      entry.occurrences.map((occurrence) => Object.freeze({ ...occurrence })),
    ),
    providerRosterDigest: corpus.providerRosterDigest,
    corpusDigest: corpus.corpusDigest,
    checkpointDigest: corpus.checkpointDigest,
  });
  const preimage = Object.freeze({
    ...withoutDigest,
    preimageDigest: createHash("sha256")
      .update(JSON.stringify(withoutDigest))
      .digest("hex"),
  });
  admittedCorpusPreimages.add(preimage);
  return preimage;
};

export const requireHistoricalNativeScriptCorpusPreimage = (
  preimage: HistoricalNativeScriptCorpusPreimage,
): HistoricalNativeScriptCorpusPreimage => {
  if (
    !admittedCorpusPreimages.has(preimage) ||
    preimage.preimageDigest !==
      createHash("sha256")
        .update(JSON.stringify(corpusPreimageWithoutDigest(preimage)))
        .digest("hex")
  ) {
    throw new Error(
      "historical native-script corpus preimage lacks authenticated corpus authority",
    );
  }
  return preimage;
};

export const historicalNativeScriptBytesFromCorpus = ({
  corpus,
  scriptHash,
}: {
  readonly corpus: HistoricalNativeScriptCorpus;
  readonly scriptHash: string;
}): Uint8Array | null => {
  requireHistoricalNativeScriptCorpus(corpus);
  const entry = corpus.entries.find(
    (candidate) => candidate.scriptHash === scriptHash,
  );
  return entry === undefined ? null : Buffer.from(entry.scriptBytesHex, "hex");
};

const ledgerOutRefKey = (bytes: Uint8Array): string => {
  const outRef = decodeMidgardSpendInputItem(bytes);
  return `${Buffer.from(outRef.txId).toString("hex")}#${outRef.outputIndex.toString()}`;
};

/**
 * Complete Q33 detector over every accepted spend in the challenged block.
 * The predecessor ledger and the native-script hash-to-preimage authority are
 * both retained behind the admitted, contiguous history capability.
 */
export const detectMissingNativeScriptUtxoFromHistoricalCorpus = async ({
  evidence,
  corpus,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly corpus: HistoricalNativeScriptCorpus;
}): Promise<readonly CanonicalViolationDetection[]> => {
  const history = requireHistoricalNativeScriptCorpus(corpus);
  if (
    history.currentEvidence !== evidence ||
    corpus.throughHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "missing-native-script-utxo detector requires the exact admitted current history",
    );
  }
  const predecessor = history.reconstructions.at(-2);
  if (predecessor === undefined) return Object.freeze([]);
  if (
    predecessor.headerHash !== evidence.header.prevHeaderHash ||
    predecessor.header.utxosRoot !== evidence.header.prevUtxosRoot
  ) {
    throw new Error(
      "missing-native-script-utxo detector predecessor changed after history admission",
    );
  }
  const predecessorOutputs = new Map(
    predecessor.utxos.map((entry) => {
      const material = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      });
      return [
        ledgerOutRefKey(entry.key),
        decodeMidgardLedgerOutputCommitment(material.descriptorCbor),
      ] as const;
    }),
  );
  const knownNativeScripts = new Set(
    corpus.entries.map((entry) => entry.scriptHash),
  );
  const detections: CanonicalViolationDetection[] = [];
  evidence.transactions.forEach((transaction, transactionIndex) => {
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(
      Buffer.from(transaction.txCbor, "hex"),
    );
    if (native.validity !== "TxIsValid") return;
    const scriptWitnessItems = decodeMidgardFieldPreimage(
      native.witnessSet.scriptTxWitsPreimageCbor,
    );
    decodeMidgardFieldPreimage(native.body.spendInputsPreimageCbor).forEach(
      (inputBytes, inputIndex) => {
        const descriptor = predecessorOutputs.get(ledgerOutRefKey(inputBytes));
        if (descriptor === undefined) return;
        const credential = decodeMidgardAddressBytes(
          descriptor.address,
        ).paymentCredential;
        if (credential.kind !== "Script") return;
        const scriptHash = credential.hash.toString("hex");
        if (
          !knownNativeScripts.has(scriptHash) ||
          !missingNativeScriptIsAbsent({
            scriptTxWitsItems: scriptWitnessItems,
            expectedMissingScriptHash: scriptHash,
          })
        ) {
          return;
        }
        detections.push(
          Object.freeze({
            detectionId: `${MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID}:${transaction.nodeTxId}:${inputIndex.toString()}`,
            headerHash: evidence.headerHash,
            violationId: MISSING_NATIVE_SCRIPT_UTXO_VIOLATION_ID,
            position: BigInt(transactionIndex),
            diagnostic: `accepted transaction ${transaction.nodeTxId} spends predecessor native-script output ${ledgerOutRefKey(inputBytes)} without witness ${scriptHash}`,
          }),
        );
      },
    );
  });
  return Object.freeze(detections);
};

/** Complete MIN-ADA-UTXO introduction scan against the authenticated predecessor. */
export const detectMinAdaUtxoFromHistoricalCorpus = ({
  evidence,
  corpus,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly corpus: HistoricalNativeScriptCorpus;
}): readonly CanonicalViolationDetection[] => {
  const history = requireHistoricalNativeScriptCorpus(corpus);
  if (history.currentEvidence !== evidence) {
    throw new Error(
      "min-ada detector requires the exact admitted current history",
    );
  }
  const predecessor = history.reconstructions.at(-2);
  if (
    predecessor !== undefined &&
    (predecessor.headerHash !== evidence.header.prevHeaderHash ||
      predecessor.header.utxosRoot !== evidence.header.prevUtxosRoot)
  ) {
    throw new Error("min-ada detector predecessor changed after admission");
  }
  const predecessorKeys = new Set(
    (predecessor?.utxos ?? []).map((entry) =>
      Buffer.from(entry.key).toString("hex"),
    ),
  );
  return Object.freeze(
    evidence.reconstruction.utxos.flatMap((entry, position) => {
      const keyHex = Buffer.from(entry.key).toString("hex");
      if (predecessorKeys.has(keyHex)) return [];
      const descriptor = buildCanonicalMidgardLedgerEntryOutputMaterial({
        outRef: entry.key,
        outputCbor: entry.value,
      }).descriptorCbor;
      const decoded = decodeMidgardLedgerOutputCommitment(descriptor);
      if (
        outputMeetsMinAda(
          MIDGARD_COINS_PER_UTXO_BYTE,
          BigInt(decoded.totalLength),
          decoded.lovelace,
        )
      ) {
        return [];
      }
      const outRef = decodeMidgardSpendInputItem(entry.key);
      const transactionId = Buffer.from(outRef.txId).toString("hex");
      return [
        Object.freeze({
          detectionId: `${MIN_ADA_VIOLATION_ID}:utxo:${transactionId}:${outRef.outputIndex.toString()}`,
          headerHash: evidence.headerHash,
          violationId: MIN_ADA_VIOLATION_ID,
          position: BigInt(position),
          diagnostic: `post-state UTxO ${transactionId}#${outRef.outputIndex.toString()} was introduced below the exact min-Ada floor`,
        }),
      ];
    }),
  );
};
