import { AUTHORITY_ENVELOPE_MAX_BYTES } from "./trusted-head-authority.envelope-codec.js";
import { MAX_RECORD_BYTES } from "./trusted-head-authority.exact-record.js";

export const TABLES = [
  "authority_current",
  "authority_checkpoint",
  "authority_initialization",
] as const;
export const SCHEMAS = new Map<string, string>([
  ...TABLES.map(
    (name) =>
      [
        name,
        `CREATE TABLE ${name} (id INTEGER PRIMARY KEY CHECK(id = 1), bytes BLOB NOT NULL CHECK(length(bytes) BETWEEN 1 AND ${AUTHORITY_ENVELOPE_MAX_BYTES}))`,
      ] as const,
  ),
  [
    "authority_records",
    `CREATE TABLE authority_records (revision TEXT PRIMARY KEY CHECK(length(revision) = 20), bytes BLOB NOT NULL CHECK(length(bytes) BETWEEN 1 AND ${MAX_RECORD_BYTES}))`,
  ],
]);
