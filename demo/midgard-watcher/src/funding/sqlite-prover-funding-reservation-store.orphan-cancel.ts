import type { DatabaseSync } from "node:sqlite";

import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import { transactionInputOutRefs } from "./sqlite-prover-funding-reservation-store.derive-signed-transition.js";

/** The one signed cancel an orphaned reservation holds: the exact bytes the
 * watcher signed, recorded before they are ever submitted. */
export type ProverFundingOrphanCancel = Readonly<{
  transactionHash: string;
  signedTransactionCborHex: string;
  /** The out-refs it spends. */
  inputs: readonly string[];
}>;

/** The signed bytes, their hash and the out-refs they spend, or a throw. */
export const decodeProverFundingOrphanCancel = (
  signedTransactionCborHex: string,
): ProverFundingOrphanCancel => {
  if (!/^(?:[0-9a-f]{2})+$/u.test(signedTransactionCborHex))
    throw new Error("prover funding cancel is malformed");
  let transaction: CML.Transaction;
  try {
    transaction = CML.Transaction.from_cbor_hex(signedTransactionCborHex);
  } catch {
    throw new Error("prover funding cancel is malformed");
  }
  if (transaction.to_cbor_hex() !== signedTransactionCborHex)
    throw new Error("prover funding cancel is not a lossless encoding");
  const body = transaction.body();
  return Object.freeze({
    transactionHash: CML.hash_transaction(body).to_hex(),
    signedTransactionCborHex,
    inputs: transactionInputOutRefs(body.inputs()),
  });
};

type CancelRow = Readonly<{
  reservation_id: unknown;
  record_digest: unknown;
  canonical_json: unknown;
}>;

/**
 * Rows of `watcher_prover_funding_cancel_v1`: at most one per reservation,
 * removed with it. Each read verifies the row's digest and recomputes the
 * hash from the recorded bytes.
 */
export const createProverFundingCancelRows = (database: DatabaseSync) => {
  database.exec(`
    CREATE TABLE IF NOT EXISTS watcher_prover_funding_cancel_v1 (
      reservation_id TEXT PRIMARY KEY CHECK (length(reservation_id) = 64),
      record_digest TEXT NOT NULL CHECK (length(record_digest) = 64),
      canonical_json TEXT NOT NULL CHECK (length(canonical_json) > 0),
      FOREIGN KEY (reservation_id)
        REFERENCES watcher_prover_funding_reservation_v1(reservation_id)
        ON DELETE CASCADE
    ) STRICT;
  `);
  const select = database.prepare(
    "SELECT * FROM watcher_prover_funding_cancel_v1 WHERE reservation_id = ?",
  );
  const insert = database.prepare(`
    INSERT INTO watcher_prover_funding_cancel_v1(
      reservation_id, record_digest, canonical_json
    ) VALUES (?, ?, ?)
  `);
  const remove = database.prepare(
    "DELETE FROM watcher_prover_funding_cancel_v1 WHERE reservation_id = ?",
  );
  const read = (reservationId: string): ProverFundingOrphanCancel | null => {
    const row = select.get(reservationId) as CancelRow | undefined;
    if (row === undefined) return null;
    if (
      row.reservation_id !== reservationId ||
      typeof row.canonical_json !== "string"
    )
      throw new Error("prover funding cancel row is malformed");
    const value = JSON.parse(row.canonical_json) as {
      transactionHash?: unknown;
      signedTransactionCborHex?: unknown;
    };
    if (
      watcherCanonicalJson(value) !== row.canonical_json ||
      computeDeploymentManifestJsonDigest(value) !== row.record_digest ||
      typeof value.signedTransactionCborHex !== "string"
    )
      throw new Error("prover funding cancel row digest mismatch");
    const cancel = decodeProverFundingOrphanCancel(
      value.signedTransactionCborHex,
    );
    if (cancel.transactionHash !== value.transactionHash)
      throw new Error("prover funding cancel row digest mismatch");
    return cancel;
  };
  return Object.freeze({
    read,
    has: (reservationId: string) => select.get(reservationId) !== undefined,
    insert: (reservationId: string, cancel: ProverFundingOrphanCancel) => {
      const value = {
        transactionHash: cancel.transactionHash,
        signedTransactionCborHex: cancel.signedTransactionCborHex,
      };
      insert.run(
        reservationId,
        computeDeploymentManifestJsonDigest(value),
        watcherCanonicalJson(value),
      );
    },
    remove: (reservationId: string) => {
      remove.run(reservationId);
    },
  });
};

export type ProverFundingCancelRows = ReturnType<
  typeof createProverFundingCancelRows
>;
