import { DatabaseSync } from "node:sqlite";
import { pathToFileURL } from "node:url";

import type { WatcherRollbackDurableTrustedHead } from "../l1/rollback-engine.js";
import type { AuthorityEnvelopeCodec } from "./trusted-head-authority.envelope-codec.js";
import { sha256 } from "./trusted-head-authority.exact-record.js";
import { SCHEMAS } from "./trusted-head-authority.sqlite-schema.js";
const key = (revision: bigint) => revision.toString().padStart(20, "0");

/** Called only in an exclusively owned unselected generation. */
export const createSqliteAuthority = (
  input: Readonly<{
    databasePath: string;
    envelopes: AuthorityEnvelopeCodec;
    initialRecords: readonly Uint8Array[];
    boundaryRecord: unknown | null;
    initialization: Readonly<Record<string, unknown>>;
    head: WatcherRollbackDurableTrustedHead | null;
    recordSha256: string | null;
  }>,
): string => {
  const db = new DatabaseSync(
    `${pathToFileURL(input.databasePath).href}?mode=rwc`,
  );
  try {
    db.exec(
      "PRAGMA journal_mode=WAL; PRAGMA synchronous=FULL; PRAGMA busy_timeout=1000; BEGIN IMMEDIATE;",
    );
    for (const schema of SCHEMAS.values()) db.exec(schema);
    const initialBytes = input.envelopes.encode(
        "initialization",
        input.initialization,
      ),
      initializationSha256 = sha256(initialBytes);
    db.prepare(
      "INSERT INTO authority_initialization(id,bytes) VALUES(1,?)",
    ).run(initialBytes);
    const cp =
      input.boundaryRecord === null
        ? null
        : input.envelopes.encode("checkpoint", {
            boundaryRecord: input.boundaryRecord,
          });
    if (cp !== null)
      db.prepare("INSERT INTO authority_checkpoint(id,bytes) VALUES(1,?)").run(
        cp,
      );
    for (const raw of input.initialRecords) {
      const parsed = JSON.parse(new TextDecoder().decode(raw)) as {
        revision: string;
      };
      db.prepare(
        "INSERT INTO authority_records(revision,bytes) VALUES(?,?)",
      ).run(key(BigInt(parsed.revision)), raw);
    }
    db.prepare("INSERT INTO authority_current(id,bytes) VALUES(1,?)").run(
      input.envelopes.encode("current", {
        head: input.head,
        recordSha256: input.recordSha256,
        checkpointSha256: cp === null ? null : sha256(cp),
        initializationSha256,
      }),
    );
    db.exec("COMMIT; PRAGMA wal_checkpoint(TRUNCATE);");
    return initializationSha256;
  } catch (error) {
    try {
      db.exec("ROLLBACK");
    } catch {
      /* Preserve storage/commit error. */
    }
    throw error;
  } finally {
    db.close();
  }
};
