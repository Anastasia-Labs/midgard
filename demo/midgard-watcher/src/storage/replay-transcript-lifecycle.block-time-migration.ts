import type { DatabaseSync } from "node:sqlite";

import type { WatcherReplayTranscriptLifecyclePin } from "./replay-transcript-lifecycle.js";

type PriorPin = Omit<WatcherReplayTranscriptLifecyclePin, "block_time"> & {
  identity: string;
};

/** Preserves old clocks as coverage; an unknown commit clock never authorizes retirement. */
export const addReplayTranscriptBlockTimeColumn = (
  database: DatabaseSync,
  hash: (fields: readonly unknown[]) => string,
): void => {
  const columns = database
    .prepare("PRAGMA table_info(watcher_replay_transcript_operation)")
    .all();
  if (columns.some((column) => column.name === "block_time")) return;
  const identities = database
    .prepare(
      "SELECT identity, pin_count, unresolved_legacy, pins_digest FROM watcher_replay_transcript_lifecycle",
    )
    .all() as {
    identity: string;
    pin_count: number;
    unresolved_legacy: number;
    pins_digest: string;
  }[];
  const pins = database.prepare(
    "SELECT * FROM watcher_replay_transcript_operation WHERE identity = ? ORDER BY operation_digest",
  );
  database.exec(
    "ALTER TABLE watcher_replay_transcript_operation ADD COLUMN block_time TEXT",
  );
  for (const state of identities) {
    const prior = pins.all(state.identity) as PriorPin[];
    if (
      ![0, 1].includes(state.unresolved_legacy) ||
      prior.length !== state.pin_count ||
      state.pins_digest !==
        hash([
          state.identity,
          state.unresolved_legacy,
          hash(prior.map((pin) => [pin.operation_digest, pin.checksum])),
        ])
    )
      throw new Error("replay transcript lifecycle is incomplete or corrupt");
    const rewritten: [string, string][] = [];
    for (const pin of prior) {
      const fields = [
        pin.identity,
        pin.operation_digest,
        pin.end_time,
        pin.retention_days,
        pin.network,
        pin.completed_slot,
        pin.classification_complete,
        pin.proof_started,
      ];
      if (pin.checksum !== hash(fields))
        throw new Error("replay transcript lifecycle is corrupt");
      const checksum = hash([...fields.slice(0, 3), null, ...fields.slice(3)]);
      rewritten.push([pin.operation_digest, checksum]);
      database
        .prepare(
          "UPDATE watcher_replay_transcript_operation SET checksum = ? WHERE identity = ? AND operation_digest = ?",
        )
        .run(checksum, pin.identity, pin.operation_digest);
    }
    database
      .prepare(
        "UPDATE watcher_replay_transcript_lifecycle SET pins_digest = ? WHERE identity = ?",
      )
      .run(
        hash([state.identity, state.unresolved_legacy, hash(rewritten)]),
        state.identity,
      );
  }
  const orphan = database
    .prepare(
      "SELECT 1 FROM watcher_replay_transcript_operation o LEFT JOIN watcher_replay_transcript_lifecycle l USING(identity) WHERE l.identity IS NULL LIMIT 1",
    )
    .get();
  if (orphan !== undefined)
    throw new Error("replay transcript lifecycle is incomplete or corrupt");
};
