import type { SchemaSet } from "../schema/apply.js";
import type { DialectName } from "../sql/backend.js";

/**
 * The intent journal: a role's own signed transactions (class B). The
 * exact signed bytes are recorded before the first submission and never
 * change; a resubmission sends them again. Status is never stored: it is
 * derived from the facts (`deriveIntentStatusesIn`), so a rollback that
 * un-lands an intent makes it live again with no write.
 *
 * The SQLite form mirrors the Postgres form as the fact schema does:
 * `bytea[]` becomes a JSON array of hex `TEXT`, `jsonb` becomes `TEXT`.
 * `l1_intent_events.at` is a log timestamp only; no decision reads it.
 *
 * Bounded per-block reads (S6 and the prune hook read what changed, never
 * the whole journal):
 *
 * - `l1_intent_spends`: one row per outref an intent spends, references or
 *   uses as collateral, so the outputs spent in a slot range find the
 *   intents they land or conflict, and a parent finds its dependants, by
 *   index. Written with the intent.
 * - `recorded_seq`: the order intents became visible in, so S6 finds the
 *   intents recorded since its last pass by index. Postgres: the recording
 *   transaction's id (`xid8`); every transaction a snapshot does not see has
 *   an id at or above that snapshot's `xmin`. SQLite: a counter
 *   (`l1_intent_record_seq`) read and raised in the recording transaction;
 *   SQLite writers are serial, so the order is the commit order.
 * - `valid_to_slot` and the abandon events' `tip_slot`: the intents expired
 *   or abandoned at or below a slot; `l1_intent_events_abandoned` finds
 *   abandoned intents without scanning every event (each resubmission
 *   appends one).
 *
 * `l1_intent_prune_mark` is the generation and boundary the prune hook last
 * ran under (`pruneIntentsIn`). A prune step that ran without the hook
 * (skipped while a store reset replays) deleted spent outputs the hook never
 * read; the next run finds the generation changed with no rollback to
 * explain it and derives every retained intent once. No row: the hook has
 * not run yet.
 */
export const INTENT_SCHEMA_NAMESPACE = "l1-intents";

/** The journal's event kinds. */
export const INTENT_EVENT_KINDS = [
  "signed",
  "stale_at_write",
  "submit_attempt",
  "submit_rejected",
  "abandoned",
  "superseded_by",
] as const;

export type IntentEventKind = (typeof INTENT_EVENT_KINDS)[number];

const kindCheck = INTENT_EVENT_KINDS.map((kind) => `'${kind}'`).join(", ");

const POSTGRES = `
-- class: B; retention: deleted only once the intent has been terminal for k blocks (landed k deep, or dead with an input spent k deep, its validity passed k deep or a dependency so pruned), in the follower's prune step
CREATE TABLE l1_intents (
  tx_hash          bytea   PRIMARY KEY,
  family           text    NOT NULL,
  workflow_key     text    NOT NULL,
  tx_cbor          bytea   NOT NULL,
  inputs           bytea[] NOT NULL,
  reference_inputs bytea[] NOT NULL,
  collaterals      bytea[] NOT NULL,
  own_outputs      jsonb   NOT NULL,
  valid_from_slot  bigint,
  valid_to_slot    bigint,
  depends_on       bytea[] NOT NULL,
  built_generation bigint  NOT NULL,
  built_slot       bigint  NOT NULL,
  built_hash       bytea   NOT NULL,
  content_ref      bytea,
  recorded_seq     xid8    NOT NULL DEFAULT pg_current_xact_id()
);
CREATE INDEX l1_intents_workflow ON l1_intents (workflow_key);
CREATE INDEX l1_intents_content_ref ON l1_intents (content_ref);
CREATE INDEX l1_intents_recorded_seq ON l1_intents (recorded_seq);
CREATE INDEX l1_intents_valid_to ON l1_intents (valid_to_slot) WHERE valid_to_slot IS NOT NULL;

-- class: B; retention: with its l1_intents row (cascade)
CREATE TABLE l1_intent_events (
  tx_hash  bytea   NOT NULL REFERENCES l1_intents(tx_hash) ON DELETE CASCADE,
  seq      integer NOT NULL,
  kind     text    NOT NULL CHECK (kind IN (${kindCheck})),
  detail   jsonb,
  tip_slot bigint,
  at       timestamptz NOT NULL DEFAULT now(),
  PRIMARY KEY (tx_hash, seq)
);
CREATE INDEX l1_intent_events_abandoned ON l1_intent_events (tx_hash, tip_slot) WHERE kind = 'abandoned';
CREATE INDEX l1_intent_events_abandoned_slot ON l1_intent_events (tip_slot) WHERE kind = 'abandoned';

-- class: B; retention: with its l1_intents row (cascade)
CREATE TABLE l1_intent_spends (
  out_tx    bytea   NOT NULL,
  out_index integer NOT NULL,
  tx_hash   bytea   NOT NULL REFERENCES l1_intents(tx_hash) ON DELETE CASCADE,
  PRIMARY KEY (out_tx, out_index, tx_hash)
);
CREATE INDEX l1_intent_spends_intent ON l1_intent_spends (tx_hash);

-- class: B; retention: one row, kept with the journal (the prune hook's last run)
CREATE TABLE l1_intent_prune_mark (
  id            integer PRIMARY KEY CHECK (id = 1),
  generation    bigint NOT NULL,
  boundary_slot bigint NOT NULL
);
`;

const SQLITE = `
-- class: B; retention: deleted only once the intent has been terminal for k blocks (landed k deep, or dead with an input spent k deep, its validity passed k deep or a dependency so pruned), in the follower's prune step
CREATE TABLE l1_intents (
  tx_hash          BLOB    PRIMARY KEY,
  family           TEXT    NOT NULL,
  workflow_key     TEXT    NOT NULL,
  tx_cbor          BLOB    NOT NULL,
  inputs           TEXT    NOT NULL,
  reference_inputs TEXT    NOT NULL,
  collaterals      TEXT    NOT NULL,
  own_outputs      TEXT    NOT NULL,
  valid_from_slot  INTEGER,
  valid_to_slot    INTEGER,
  depends_on       TEXT    NOT NULL,
  built_generation INTEGER NOT NULL,
  built_slot       INTEGER NOT NULL,
  built_hash       BLOB    NOT NULL,
  content_ref      BLOB,
  recorded_seq     INTEGER NOT NULL DEFAULT 0
);
CREATE INDEX l1_intents_workflow ON l1_intents (workflow_key);
CREATE INDEX l1_intents_content_ref ON l1_intents (content_ref);
CREATE INDEX l1_intents_recorded_seq ON l1_intents (recorded_seq);
CREATE INDEX l1_intents_valid_to ON l1_intents (valid_to_slot) WHERE valid_to_slot IS NOT NULL;

-- class: B; retention: with its l1_intents row (cascade)
CREATE TABLE l1_intent_events (
  tx_hash  BLOB    NOT NULL REFERENCES l1_intents(tx_hash) ON DELETE CASCADE,
  seq      INTEGER NOT NULL,
  kind     TEXT    NOT NULL CHECK (kind IN (${kindCheck})),
  detail   TEXT,
  tip_slot INTEGER,
  at       TEXT    NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (tx_hash, seq)
);
CREATE INDEX l1_intent_events_abandoned ON l1_intent_events (tx_hash, tip_slot) WHERE kind = 'abandoned';
CREATE INDEX l1_intent_events_abandoned_slot ON l1_intent_events (tip_slot) WHERE kind = 'abandoned';

-- class: B; retention: with its l1_intents row (cascade)
CREATE TABLE l1_intent_spends (
  out_tx    BLOB    NOT NULL,
  out_index INTEGER NOT NULL,
  tx_hash   BLOB    NOT NULL REFERENCES l1_intents(tx_hash) ON DELETE CASCADE,
  PRIMARY KEY (out_tx, out_index, tx_hash)
);
CREATE INDEX l1_intent_spends_intent ON l1_intent_spends (tx_hash);

-- class: B; retention: one row, kept with the journal (the next recording sequence number)
CREATE TABLE l1_intent_record_seq (
  id   INTEGER PRIMARY KEY CHECK (id = 1),
  next INTEGER NOT NULL
);
INSERT INTO l1_intent_record_seq (id, next) VALUES (1, 1);

-- class: B; retention: one row, kept with the journal (the prune hook's last run)
CREATE TABLE l1_intent_prune_mark (
  id            integer PRIMARY KEY CHECK (id = 1),
  generation    INTEGER NOT NULL,
  boundary_slot INTEGER NOT NULL
);
`;

export const intentSchema = (dialect: DialectName): SchemaSet => ({
  namespace: INTENT_SCHEMA_NAMESPACE,
  sql: dialect === "postgres" ? POSTGRES : SQLITE,
});
