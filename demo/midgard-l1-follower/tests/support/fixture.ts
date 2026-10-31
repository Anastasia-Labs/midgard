import {
  type DerivationHook,
  type DialectName,
  encodeOutRef,
  type MigrationSet,
  type TemporalTableSpec,
} from "../../src/index.js";

/**
 * Sample role D-t tables for the property test: one versioned table, and an
 * append-only parent with an append-only child that references it, so the
 * generated rewind must truncate children first (the foreign key refuses
 * any other order).
 */
export const FIXTURE_TABLES: readonly TemporalTableSpec[] = [
  {
    name: "fixture_address_live_count",
    shape: "versioned",
    startColumn: "from_slot",
    endColumn: "to_slot",
    retention: { kind: "closed_k_deep" },
  },
  // Registered child before parent on purpose: the registry orders them.
  {
    name: "fixture_spend_log",
    shape: "append_only",
    slotColumn: "spent_slot",
    parents: ["fixture_block_marks"],
    retention: { kind: "created_k_deep" },
  },
  {
    name: "fixture_block_marks",
    shape: "append_only",
    slotColumn: "slot",
    retention: { kind: "created_k_deep" },
  },
];

const fixtureSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  const int8 = dialect === "postgres" ? "bigint" : "INTEGER";
  return `
-- class: D-t; retention: current rows forever; closed rows once to_slot is k deep
CREATE TABLE fixture_address_live_count (
  address ${bytes} NOT NULL,
  live_count integer NOT NULL,
  from_slot ${int8} NOT NULL,
  to_slot ${int8}
);
CREATE UNIQUE INDEX fixture_address_live_count_current ON fixture_address_live_count (address) WHERE to_slot IS NULL;
CREATE INDEX fixture_address_live_count_from ON fixture_address_live_count (from_slot);
CREATE INDEX fixture_address_live_count_to ON fixture_address_live_count (to_slot);

-- class: D-t; retention: once slot is k deep (after its spend-log children)
CREATE TABLE fixture_block_marks (
  slot ${int8} PRIMARY KEY,
  hash ${bytes} NOT NULL,
  qualifying integer NOT NULL
);

-- class: D-t; retention: once spent_slot is k deep
CREATE TABLE fixture_spend_log (
  tx_hash ${bytes} NOT NULL,
  output_index integer NOT NULL,
  spent_slot ${int8} NOT NULL REFERENCES fixture_block_marks(slot),
  PRIMARY KEY (tx_hash, output_index)
);
CREATE INDEX fixture_spend_log_slot ON fixture_spend_log (spent_slot);
`;
};

export const fixtureMigrations = (dialect: DialectName): MigrationSet => ({
  namespace: "fixture",
  migrations: [{ id: "0001_fixture_tables", sql: fixtureSql(dialect) }],
});

const number = (value: unknown): number => Number(value as string | number);

/**
 * The sample S3 derivation: block marks, a spend log, a per-address live
 * count (versioned), and event keys for valid txs minting under a tracked
 * policy. A pure function of the block and the facts.
 */
export const FIXTURE_DERIVATION: DerivationHook = {
  name: "fixture",
  writes: [
    "fixture_block_marks",
    "fixture_spend_log",
    "fixture_address_live_count",
  ],
  apply: async ({ tx, block, qualified }) => {
    const slot = block.point.slot;
    await tx.query(
      "INSERT INTO fixture_block_marks (slot, hash, qualifying) VALUES (?, ?, ?)",
      [slot, block.point.hash, qualified.length],
    );
    const touched = new Map<string, Buffer>();
    for (const entry of qualified) {
      for (const outRef of entry.spent) {
        await tx.query(
          "INSERT INTO fixture_spend_log (tx_hash, output_index, spent_slot) VALUES (?, ?, ?)",
          [outRef.txHash, outRef.index, slot],
        );
        const row = (
          await tx.query(
            "SELECT address FROM l1_outputs WHERE tx_hash = ? AND output_index = ?",
            [outRef.txHash, outRef.index],
          )
        )[0];
        if (row !== undefined) {
          const address = Buffer.from(row.address as Uint8Array);
          touched.set(address.toString("hex"), address);
        }
      }
      for (const { output } of entry.created)
        touched.set(output.address.toString("hex"), output.address);
      const first = entry.tx.inputs[0];
      if (entry.trackedMint && entry.tx.isValid && first !== undefined)
        await tx.query(
          "INSERT INTO l1_event_keys (kind, key, origin_outref, first_canonical_slot) VALUES (?, ?, ?, ?) ON CONFLICT (kind, key) DO NOTHING",
          ["fixture_mint", entry.tx.hash, encodeOutRef(first), slot],
        );
    }
    for (const [, address] of [...touched.entries()].sort(([a], [b]) =>
      a < b ? -1 : 1,
    )) {
      const live = number(
        (
          await tx.query(
            "SELECT count(*) AS n FROM l1_outputs WHERE address = ? AND spent_slot IS NULL",
            [address],
          )
        )[0]?.n,
      );
      const current = (
        await tx.query(
          "SELECT live_count FROM fixture_address_live_count WHERE address = ? AND to_slot IS NULL",
          [address],
        )
      )[0];
      if (current !== undefined && number(current.live_count) === live)
        continue;
      if (current !== undefined)
        await tx.query(
          "UPDATE fixture_address_live_count SET to_slot = ? WHERE address = ? AND to_slot IS NULL",
          [slot, address],
        );
      await tx.query(
        "INSERT INTO fixture_address_live_count (address, live_count, from_slot, to_slot) VALUES (?, ?, ?, NULL)",
        [address, live, slot],
      );
    }
  },
};
