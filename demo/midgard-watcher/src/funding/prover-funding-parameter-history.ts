import { createHmac, timingSafeEqual } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import type { WatcherProverFundingReservationRecord } from "./prover-funding-reservation.js";

/** Persistence contains historical facts, never live transaction authority. */
export const createWatcherProtocolParameterHistoryStorage = (input: {
  readonly database: DatabaseSync;
  readonly authenticationKey: Uint8Array;
  readonly deploymentFingerprint: string;
}) => {
  if (input.authenticationKey.length !== 32)
    throw new Error(
      "Funding parameter history authentication key must contain 32 bytes",
    );
  const key = Uint8Array.from(input.authenticationKey);
  input.database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_parameters_v1 (
      reservation_id TEXT PRIMARY KEY,
      canonical_json TEXT NOT NULL,
      authentication_tag TEXT NOT NULL CHECK(length(authentication_tag) = 64),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
  `);
  input.database
    .exec(`CREATE TABLE IF NOT EXISTS watcher_prover_funding_capacity_v1 (
    reservation_id TEXT PRIMARY KEY,
    canonical_json TEXT NOT NULL,
    authentication_tag TEXT NOT NULL CHECK(length(authentication_tag) = 64),
    FOREIGN KEY (reservation_id) REFERENCES watcher_prover_funding_reservation_v1(reservation_id) ON DELETE CASCADE
  ) STRICT;`);
  const table = (capacity: boolean) =>
    capacity
      ? "watcher_prover_funding_capacity_v1"
      : "watcher_prover_funding_parameters_v1";
  const tag = (payload: string) =>
    createHmac("sha256", key)
      .update("midgard-watcher-funding-parameter-history-v1\n")
      .update(payload)
      .digest();
  const read = (
    record: WatcherProverFundingReservationRecord,
    capacity = false,
  ): DeploymentManifestCardanoProtocolParameters | null => {
    const row = input.database
      .prepare(
        `SELECT canonical_json, authentication_tag FROM ${table(capacity)} WHERE reservation_id = ?`,
      )
      .get(record.reservationId);
    if (row === undefined) return null;
    if (
      typeof row.canonical_json !== "string" ||
      typeof row.authentication_tag !== "string" ||
      !/^[0-9a-f]{64}$/u.test(row.authentication_tag) ||
      !timingSafeEqual(
        tag(row.canonical_json),
        Buffer.from(row.authentication_tag, "hex"),
      )
    )
      throw new Error("Funding parameter history authentication mismatch");
    const value = JSON.parse(row.canonical_json) as Record<string, unknown>;
    if (
      value.capacity !== capacity ||
      value.deploymentFingerprint !== input.deploymentFingerprint ||
      value.deploymentFingerprint !== record.deploymentFingerprint ||
      value.reservationId !== record.reservationId ||
      value.policyDigest !== record.policyDigest ||
      value.reservationBasisDigest !== record.reservationBasisDigest ||
      watcherCanonicalJson(value) !== row.canonical_json
    )
      throw new Error(
        "Funding parameter history reservation identity mismatch",
      );
    const snapshot = parseDeploymentManifestCardanoProtocolParameters(
      value.snapshot,
    );
    if (computeDeploymentManifestJsonDigest(snapshot) !== value.snapshotDigest)
      throw new Error("Funding parameter history snapshot digest mismatch");
    return snapshot;
  };
  return Object.freeze({
    read,
    readCapacity: (record: WatcherProverFundingReservationRecord) =>
      read(record, true),
    remember(
      record: WatcherProverFundingReservationRecord,
      snapshot: DeploymentManifestCardanoProtocolParameters,
      capacity = false,
    ): void {
      if (record.deploymentFingerprint !== input.deploymentFingerprint)
        throw new Error("Funding parameter history changed deployment");
      const existing = read(record, capacity);
      if (existing !== null) {
        if (
          capacity &&
          BigInt(existing.maxCollateralInputs) >=
            BigInt(snapshot.maxCollateralInputs)
        )
          return;
        if (
          !capacity &&
          computeDeploymentManifestJsonDigest(existing) !==
            computeDeploymentManifestJsonDigest(snapshot)
        )
          throw new Error(
            "Funding parameter history changed original snapshot",
          );
        if (!capacity) return;
      }
      const payload = watcherCanonicalJson({
        capacity,
        deploymentFingerprint: input.deploymentFingerprint,
        reservationId: record.reservationId,
        policyDigest: record.policyDigest,
        reservationBasisDigest: record.reservationBasisDigest,
        snapshot,
        snapshotDigest: computeDeploymentManifestJsonDigest(snapshot),
      });
      input.database
        .prepare(
          `INSERT INTO ${table(capacity)} (reservation_id, canonical_json, authentication_tag) VALUES (?, ?, ?) ${capacity ? `ON CONFLICT(reservation_id) DO UPDATE SET canonical_json = excluded.canonical_json, authentication_tag = excluded.authentication_tag WHERE CAST(json_extract(excluded.canonical_json, '$.snapshot.maxCollateralInputs') AS INTEGER) > CAST(json_extract(watcher_prover_funding_capacity_v1.canonical_json, '$.snapshot.maxCollateralInputs') AS INTEGER)` : ""}`,
        )
        .run(record.reservationId, payload, tag(payload).toString("hex"));
    },
  });
};
