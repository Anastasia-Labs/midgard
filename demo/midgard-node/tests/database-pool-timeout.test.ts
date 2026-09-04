import { SqlError } from "@effect/sql";
import { Duration } from "effect";
import { describe, expect, it } from "vitest";

import {
  DatabaseError,
  formatDatabaseError,
} from "../src/database/utils/common.js";
import {
  databaseConnectTimeout,
  type DatabasePoolRole,
} from "../src/services/database.js";

describe("database pool connection establishment timeout", () => {
  it.each<DatabasePoolRole>(["admission", "batch", "worker"])(
    "gives the %s pool ten seconds to establish a connection",
    (role) => {
      expect(Duration.toMillis(databaseConnectTimeout(role))).toBe(10_000);
    },
  );

  it("preserves nested PostgreSQL connection error names and messages without statement parameters", () => {
    const postgresCause = Object.assign(
      new Error("write CONNECT_TIMEOUT postgres:5432"),
      { code: "CONNECT_TIMEOUT" },
    );
    const error = new DatabaseError({
      table: "tx_admissions",
      message: "Failed to durably admit reserved transaction batch",
      cause: new SqlError.SqlError({
        message: "Failed to execute statement",
        cause: postgresCause,
      }),
    });

    expect(formatDatabaseError(error)).toBe(
      "DatabaseError: Failed to durably admit reserved transaction batch; cause=SqlError: Failed to execute statement; cause=Error: write CONNECT_TIMEOUT postgres:5432; codes=CONNECT_TIMEOUT",
    );
    expect(formatDatabaseError(error)).not.toContain("tx_canonical_cbor");
  });
});
