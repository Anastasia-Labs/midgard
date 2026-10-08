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

export const intentMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: INTENT_MIGRATION_NAMESPACE,
  migrations: [
    {
      id: "0001_intent_journal",
      sql: dialect === "postgres" ? POSTGRES_0001 : SQLITE_0001,
    },
    { id: "0002_intent_abandoned_index", sql: ANY_0002 },
  ],
});
