import { createHash, createHmac, timingSafeEqual } from "node:crypto";
import { existsSync, lstatSync, mkdirSync, realpathSync } from "node:fs";
import { dirname, isAbsolute, normalize } from "node:path";
import { DatabaseSync } from "node:sqlite";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type TransitionTraceReconstruction } from "../transition-trace/reconstruct.js";
import {
  admittedCheckpointStores,
  admittedDurableCheckpointStores,
  HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_MAC_DOMAIN,
  HISTORICAL_NATIVE_SCRIPT_CHECKPOINT_STORE,
  HISTORICAL_NATIVE_SCRIPT_HISTORY_SOURCE,
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  type HistoricalNativeScriptOccurrence,
} from "./historical-native-script-corpus.create-historical-native-script-provider-roster.js";
import {
  admittedHistorySources,
  requireCheckpoint,
} from "./historical-native-script-corpus.require-checkpoint.js";

/**
 * Durable local archival-index checkpoint. SQLite's BEGIN IMMEDIATE gives the
 * deployment row a real compare-and-swap boundary across watcher processes.
 */
export const createSqliteHistoricalNativeScriptCheckpointStore = ({
  path,
  rollbackAuthenticationKey,
}: {
  readonly path: string;
  /** The watcher's rollback authentication key (`storage.rollbackAuthorityKeySource`). */
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

export const admittedCorpusInternals = new WeakMap<
  object,
  AdmittedHistoricalNativeScriptCorpus
>();

export const occurrenceOrder = (
  left: HistoricalNativeScriptOccurrence,
  right: HistoricalNativeScriptOccurrence,
): number =>
  left.headerHash.localeCompare(right.headerHash) ||
  left.txId.localeCompare(right.txId) ||
  left.source.localeCompare(right.source) ||
  left.itemIndex - right.itemIndex;
