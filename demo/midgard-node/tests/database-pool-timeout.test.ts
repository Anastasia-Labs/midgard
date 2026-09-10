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

/**
 * Static contract (not a runtime assertion): every member of the production
 * `DatabasePoolRole` union must be listed here. `everyRoleIsCovered` stops
 * compiling if a role is added to the union without being covered below, so
 * the role-invariance assertion can never silently stop seeing a pool.
 */
const DATABASE_POOL_ROLES = [
  "admission",
  "batch",
  "worker",
] as const satisfies readonly DatabasePoolRole[];
type UncoveredPoolRole = Exclude<
  DatabasePoolRole,
  (typeof DATABASE_POOL_ROLES)[number]
>;
export const everyRoleIsCovered: [UncoveredPoolRole] extends [never]
  ? true
  : false = true;

/**
 * Independent bounds, not a copy of the production constant.
 *
 * Lower: a connect attempt must survive at least one dropped SYN (Linux
 * retransmits the first one after ~1 s and the second after ~3 s), otherwise a
 * single lost packet takes a pool down.
 * Upper: a stalled connect must give up well inside the readiness
 * heartbeat-age budget (`READINESS_MAX_HEARTBEAT_AGE_MS`, default 120 s) so it
 * is reported as unready instead of hanging past the probe window.
 */
const MIN_REASONABLE_CONNECT_TIMEOUT_MS = 3_000;
const MAX_REASONABLE_CONNECT_TIMEOUT_MS = 30_000;

describe("database pool connection establishment timeout", () => {
  it("gives every pool role the same connect timeout", () => {
    const timeouts = DATABASE_POOL_ROLES.map((role) => [
      role,
      Duration.toMillis(databaseConnectTimeout(role)),
    ]);
    const [, firstTimeout] = timeouts[0]!;

    expect(timeouts).toEqual(
      DATABASE_POOL_ROLES.map((role) => [role, firstTimeout]),
    );
  });

  it.each(DATABASE_POOL_ROLES)(
    "keeps the %s pool connect timeout inside the operable band",
    (role) => {
      const timeoutMs = Duration.toMillis(databaseConnectTimeout(role));

      expect(timeoutMs).toBeGreaterThanOrEqual(
        MIN_REASONABLE_CONNECT_TIMEOUT_MS,
      );
      expect(timeoutMs).toBeLessThanOrEqual(MAX_REASONABLE_CONNECT_TIMEOUT_MS);
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
