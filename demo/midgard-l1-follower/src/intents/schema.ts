import type { MigrationSet } from "../schema/migrate.js";
import type { DialectName } from "../sql/backend.js";

/**
 * The §8.2 intent journal: a role's own signed transactions (class B). The
 * exact signed bytes are recorded before the first submission and never
 * change; a resubmission sends them again. Status is never stored: it is
 * derived from the facts (`deriveIntentStatusesIn`), so a rollback that
 * un-lands an intent makes it live again with no write.
 *
 * The SQLite form mirrors the Postgres form as the fact schema does:
 * `bytea[]` becomes a JSON array of hex `TEXT`, `jsonb` becomes `TEXT`.
 * `l1_intent_events.at` is a log timestamp only; no decision reads it.
 */
export const INTENT_MIGRATION_NAMESPACE = "l1-intents";

/** The journal's event kinds (§8.2). */
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

const POSTGRES_0001 = `
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
  content_ref      bytea
);
CREATE INDEX l1_intents_workflow ON l1_intents (workflow_key);
CREATE INDEX l1_intents_content_ref ON l1_intents (content_ref);

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
`;

const SQLITE_0001 = `
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
  content_ref      BLOB
);
CREATE INDEX l1_intents_workflow ON l1_intents (workflow_key);
CREATE INDEX l1_intents_content_ref ON l1_intents (content_ref);

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
`;

/**
 * The status read finds abandoned intents without scanning every event
 * (each resubmission appends one): a partial index, the same in both
 * dialects.
 */
const ANY_0002 = `
CREATE INDEX l1_intent_events_abandoned ON l1_intent_events (tx_hash, tip_slot) WHERE kind = 'abandoned';
`;

/**
 * Bounded per-block reads (S6 and the prune hook read what changed, never
 * the whole journal):
 *
 * - `l1_intent_spends`: one row per outref an intent spends, references or
 *   uses as collateral, so the outputs spent in a slot range find the
 *   intents they land or conflict, and a parent finds its dependants, by
 *   index. Written with the intent; backfilled here.
 * - `recorded_seq`: the order intents became visible in, so S6 finds the
 *   intents recorded since its last pass by index. Postgres: the recording
 *   transaction's id (`xid8`); every transaction a snapshot does not see has
 *   an id at or above that snapshot's `xmin`. SQLite: a counter
 *   (`l1_intent_record_seq`) read and raised in the recording transaction;
 *   SQLite writers are serial, so the order is the commit order.
 * - `valid_to_slot` and the abandon events' `tip_slot`: the intents expired
 *   or abandoned at or below a slot.
 */
const POSTGRES_0003 = `
-- class: B; retention: with its l1_intents row (cascade)
CREATE TABLE l1_intent_spends (
  out_tx    bytea   NOT NULL,
  out_index integer NOT NULL,
  tx_hash   bytea   NOT NULL REFERENCES l1_intents(tx_hash) ON DELETE CASCADE,
  PRIMARY KEY (out_tx, out_index, tx_hash)
);
CREATE INDEX l1_intent_spends_intent ON l1_intent_spends (tx_hash);
INSERT INTO l1_intent_spends (out_tx, out_index, tx_hash)
SELECT DISTINCT substring(o FROM 1 FOR 32), get_byte(o, 32) * 256 + get_byte(o, 33), i.tx_hash
  FROM l1_intents i, unnest(i.inputs || i.reference_inputs || i.collaterals) AS o;
ALTER TABLE l1_intents ADD COLUMN recorded_seq xid8 NOT NULL DEFAULT pg_current_xact_id();
CREATE INDEX l1_intents_recorded_seq ON l1_intents (recorded_seq);
CREATE INDEX l1_intents_valid_to ON l1_intents (valid_to_slot) WHERE valid_to_slot IS NOT NULL;
CREATE INDEX l1_intent_events_abandoned_slot ON l1_intent_events (tip_slot) WHERE kind = 'abandoned';
`;

const SQLITE_HEX_DIGIT = (at: number): string =>
  `(instr('0123456789abcdef', lower(substr(o.value, ${String(at)}, 1))) - 1)`;

const SQLITE_0003 = `
-- class: B; retention: with its l1_intents row (cascade)
CREATE TABLE l1_intent_spends (
  out_tx    BLOB    NOT NULL,
  out_index INTEGER NOT NULL,
  tx_hash   BLOB    NOT NULL REFERENCES l1_intents(tx_hash) ON DELETE CASCADE,
  PRIMARY KEY (out_tx, out_index, tx_hash)
);
CREATE INDEX l1_intent_spends_intent ON l1_intent_spends (tx_hash);
INSERT OR IGNORE INTO l1_intent_spends (out_tx, out_index, tx_hash)
SELECT unhex(substr(o.value, 1, 64)),
       ${SQLITE_HEX_DIGIT(65)} * 4096 + ${SQLITE_HEX_DIGIT(66)} * 256 + ${SQLITE_HEX_DIGIT(67)} * 16 + ${SQLITE_HEX_DIGIT(68)},
       i.tx_hash
  FROM l1_intents i, json_each(i.inputs) o
UNION SELECT unhex(substr(o.value, 1, 64)),
       ${SQLITE_HEX_DIGIT(65)} * 4096 + ${SQLITE_HEX_DIGIT(66)} * 256 + ${SQLITE_HEX_DIGIT(67)} * 16 + ${SQLITE_HEX_DIGIT(68)},
       i.tx_hash
  FROM l1_intents i, json_each(i.reference_inputs) o
UNION SELECT unhex(substr(o.value, 1, 64)),
       ${SQLITE_HEX_DIGIT(65)} * 4096 + ${SQLITE_HEX_DIGIT(66)} * 256 + ${SQLITE_HEX_DIGIT(67)} * 16 + ${SQLITE_HEX_DIGIT(68)},
       i.tx_hash
  FROM l1_intents i, json_each(i.collaterals) o;
-- class: B; retention: one row, kept with the journal (the next recording sequence number)
CREATE TABLE l1_intent_record_seq (
  id   INTEGER PRIMARY KEY CHECK (id = 1),
  next INTEGER NOT NULL
);
INSERT INTO l1_intent_record_seq (id, next) VALUES (1, 1);
ALTER TABLE l1_intents ADD COLUMN recorded_seq INTEGER NOT NULL DEFAULT 0;
CREATE INDEX l1_intents_recorded_seq ON l1_intents (recorded_seq);
CREATE INDEX l1_intents_valid_to ON l1_intents (valid_to_slot) WHERE valid_to_slot IS NOT NULL;
CREATE INDEX l1_intent_events_abandoned_slot ON l1_intent_events (tip_slot) WHERE kind = 'abandoned';
`;

/**
 * The generation and boundary the prune hook last ran under
 * (`pruneIntentsIn`). A prune step that ran without the hook (skipped while
 * a store reset replays) deleted spent outputs the hook never read; the
 * next run finds the generation changed with no rollback to explain it and
 * derives every retained intent once. No row: the hook has not run yet.
 */
const ANY_0004 = (bigint: string): string => `
-- class: B; retention: one row, kept with the journal (the prune hook's last run)
CREATE TABLE l1_intent_prune_mark (
  id            integer PRIMARY KEY CHECK (id = 1),
  generation    ${bigint} NOT NULL,
  boundary_slot ${bigint} NOT NULL
);
`;

export const intentMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: INTENT_MIGRATION_NAMESPACE,
  migrations: [
    {
      id: "0001_intent_journal",
      sql: dialect === "postgres" ? POSTGRES_0001 : SQLITE_0001,
    },
    { id: "0002_intent_abandoned_index", sql: ANY_0002 },
    {
      id: "0003_intent_bounded_reads",
      sql: dialect === "postgres" ? POSTGRES_0003 : SQLITE_0003,
    },
    {
      id: "0004_intent_prune_mark",
      sql: ANY_0004(dialect === "postgres" ? "bigint" : "INTEGER"),
    },
  ],
});
