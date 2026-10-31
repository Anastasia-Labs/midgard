import { createHash } from "node:crypto";
import type { DatabaseSync } from "node:sqlite";

import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { WATCHER_CARDANO_SECURITY_PARAMETER_K } from "../runtime/config.js";

/** Absence age is usable only within uninterrupted admitted runtime history. */
export const createWatcherReplayTranscriptAbsence = (
  database: DatabaseSync,
) => {
  database.exec(`CREATE TABLE IF NOT EXISTS watcher_replay_transcript_absence (
    identity TEXT PRIMARY KEY,
    block_no TEXT NOT NULL,
    block_hash TEXT NOT NULL,
    slot TEXT NOT NULL,
    pins_digest TEXT NOT NULL,
    checksum TEXT NOT NULL
  ) STRICT;`);
  const clear = (identity?: string) => {
    if (identity === undefined)
      database.prepare("DELETE FROM watcher_replay_transcript_absence").run();
    else
      database
        .prepare(
          "DELETE FROM watcher_replay_transcript_absence WHERE identity = ?",
        )
        .run(identity);
  };
  // Restart cannot prove that the old witness is on this session's branch.
  clear();
  const seal = (values: readonly string[]) =>
    createHash("sha256").update(JSON.stringify(values)).digest("hex");
  return {
    clear,
    expired: (
      identity: string,
      pinsDigest: string,
      observation: WatcherAuthenticatedStateQueueObservation,
    ) => {
      const prior = database
        .prepare(
          "SELECT * FROM watcher_replay_transcript_absence WHERE identity = ?",
        )
        .get(identity) as
        | {
            block_no: string;
            block_hash: string;
            slot: string;
            pins_digest: string;
            checksum: string;
          }
        | undefined;
      if (
        prior !== undefined &&
        prior.checksum !==
          seal([
            identity,
            prior.block_no,
            prior.block_hash,
            prior.slot,
            prior.pins_digest,
          ])
      )
        throw new Error("replay transcript absence witness is corrupt");
      const point = observation.nativePoint;
      if (
        prior === undefined ||
        prior.pins_digest !== pinsDigest ||
        BigInt(point.blockNo) < BigInt(prior.block_no) ||
        (point.blockNo === prior.block_no &&
          point.blockHash !== prior.block_hash)
      ) {
        database
          .prepare(
            "INSERT OR REPLACE INTO watcher_replay_transcript_absence VALUES (?, ?, ?, ?, ?, ?)",
          )
          .run(
            identity,
            point.blockNo,
            point.blockHash,
            point.slot,
            pinsDigest,
            seal([
              identity,
              point.blockNo,
              point.blockHash,
              point.slot,
              pinsDigest,
            ]),
          );
        return false;
      }
      return (
        BigInt(point.blockNo) - BigInt(prior.block_no) >
        BigInt(WATCHER_CARDANO_SECURITY_PARAMETER_K)
      );
    },
  };
};
