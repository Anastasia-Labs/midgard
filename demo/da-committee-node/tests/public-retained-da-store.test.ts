import { randomBytes } from "node:crypto";

import { Client } from "pg";
import { afterAll, beforeAll, describe, expect, it, vi } from "vitest";

import {
  PostgresPublicRetainedDaStore,
  type PublicRetainedDaPool,
  type PublicRetainedDaPoolClient,
} from "../src/store/public-retained-da.js";

const EXPECTED_ROLE = "midgard_public_reader";
const HEADER_HASH = "ab".repeat(28);

type Access = {
  readonly current_user: string;
  readonly session_user: string;
  readonly rolsuper: boolean;
  readonly rolbypassrls: boolean;
  readonly rolcreaterole: boolean;
  readonly rolcreatedb: boolean;
  readonly rolreplication: boolean;
  readonly privileged_membership: boolean;
  readonly broad_role_membership: boolean;
  readonly payload_select: boolean;
  readonly payload_write: boolean;
  readonly header_select: boolean;
  readonly header_write: boolean;
};

const readOnlyAccess = (overrides: Partial<Access> = {}): Access => ({
  current_user: EXPECTED_ROLE,
  session_user: EXPECTED_ROLE,
  rolsuper: false,
  rolbypassrls: false,
  rolcreaterole: false,
  rolcreatedb: false,
  rolreplication: false,
  privileged_membership: false,
  broad_role_membership: false,
  payload_select: true,
  payload_write: false,
  header_select: true,
  header_write: false,
  ...overrides,
});

const payloadRecord = (headerHash = HEADER_HASH): Record<string, unknown> => ({
  deploymentFingerprint: "cd".repeat(32),
  headerHash,
  payloadSchemaVersion: 1,
  payloadCborHex: "80",
  payloadSha256: "ef".repeat(32),
  sourcePeerId: "public-peer",
  fetchedAt: "2026-08-03T00:00:00.000Z",
  validationStatus: "verified",
});

const fakePool = ({
  access = readOnlyAccess(),
  payload,
  header,
}: {
  readonly access?: Access;
  readonly payload?: unknown;
  readonly header?: unknown;
} = {}): {
  readonly pool: PublicRetainedDaPool;
  readonly queries: string[];
  readonly release: ReturnType<typeof vi.fn>;
  readonly end: ReturnType<typeof vi.fn>;
} => {
  const queries: string[] = [];
  const release = vi.fn();
  const end = vi.fn(async (): Promise<void> => undefined);
  const client: PublicRetainedDaPoolClient = {
    query: async <T extends Record<string, unknown>>(
      query: string,
    ): Promise<{ readonly rows: readonly T[] }> => {
      queries.push(query);
      if (query.includes("FROM pg_roles role")) {
        return { rows: [access as unknown as T] };
      }
      if (query.includes("FROM watcher_da_payloads")) {
        return {
          rows:
            payload === undefined
              ? []
              : ([{ record: payload }] as unknown as readonly T[]),
        };
      }
      if (query.includes("FROM watcher_state_queue_headers")) {
        return {
          rows:
            header === undefined
              ? []
              : ([{ record: header }] as unknown as readonly T[]),
        };
      }
      return { rows: [] };
    },
    release,
  };
  return {
    pool: {
      connect: async (): Promise<PublicRetainedDaPoolClient> => client,
      end,
    },
    queries,
    release,
    end,
  };
};

const openWith = async (pool: PublicRetainedDaPool) =>
  PostgresPublicRetainedDaStore.open({
    databaseUrl: "postgresql://midgard_public_reader@db.example/midgard",
    expectedRole: EXPECTED_ROLE,
    poolFactory: () => pool,
  });

describe("PostgresPublicRetainedDaStore", () => {
  it("accepts only the exact SELECT-only login and reads the two public tables", async () => {
    const fake = fakePool({
      payload: payloadRecord(),
      header: { headerHash: HEADER_HASH },
    });
    const store = await openWith(fake.pool);

    await expect(store.getDaPayload(HEADER_HASH)).resolves.toMatchObject({
      headerHash: HEADER_HASH,
      validationStatus: "verified",
    });
    await expect(store.getStateQueueHeader(HEADER_HASH)).resolves.toEqual({
      headerHash: HEADER_HASH,
    });
    expect(
      fake.queries.filter((query) => query === "BEGIN READ ONLY"),
    ).toHaveLength(3);
    expect(
      fake.queries.some((query) =>
        /^\s*(?:INSERT|UPDATE|DELETE|TRUNCATE)\b/imu.test(query),
      ),
    ).toBe(false);
    await store.close();
    expect(fake.release).toHaveBeenCalledTimes(3);
    expect(fake.end).toHaveBeenCalledOnce();
  });

  it("rejects SET ROLE-style masquerading even when current_user is the reader", async () => {
    const fake = fakePool({
      access: readOnlyAccess({ session_user: "privileged_login" }),
    });
    await expect(openWith(fake.pool)).rejects.toThrow(/SELECT-only role/u);
    expect(fake.end).toHaveBeenCalledOnce();
  });

  it("rejects a DML-capable login and inherited privileged memberships", async () => {
    await expect(
      openWith(
        fakePool({ access: readOnlyAccess({ payload_write: true }) }).pool,
      ),
    ).rejects.toThrow(/SELECT-only role/u);
    await expect(
      openWith(
        fakePool({ access: readOnlyAccess({ privileged_membership: true }) })
          .pool,
      ),
    ).rejects.toThrow(/SELECT-only role/u);
  });

  it("rejects a changed current role and privileged role attributes", async () => {
    for (const access of [
      readOnlyAccess({ current_user: "writer" }),
      readOnlyAccess({ rolsuper: true }),
      readOnlyAccess({ broad_role_membership: true }),
    ]) {
      await expect(openWith(fakePool({ access }).pool)).rejects.toThrow(
        /SELECT-only role/u,
      );
    }
  });

  it("rejects a role granted DELETE on watcher_da_payloads at open()", async () => {
    // Q54 adversarial: the public retained-DA plane must be structurally
    // incapable of pruning still-challengeable evidence. A login that merely
    // *holds* DELETE (even without exercising it) is refused at open().
    const fake = fakePool({
      access: readOnlyAccess({ payload_write: true }),
    });
    await expect(openWith(fake.pool)).rejects.toThrow(/SELECT-only role/u);
    expect(fake.end).toHaveBeenCalledOnce();
    // The privilege probe never issues DML of its own.
    expect(
      fake.queries.some((query) =>
        /^\s*(?:INSERT|UPDATE|DELETE|TRUNCATE)\b/imu.test(query),
      ),
    ).toBe(false);
    // A DELETE grant on the header table is refused the same way.
    await expect(
      openWith(
        fakePool({ access: readOnlyAccess({ header_write: true }) }).pool,
      ),
    ).rejects.toThrow(/SELECT-only role/u);
  });

  it("cannot delete inside BEGIN READ ONLY even with a compliant role", async () => {
    // Second, independent barrier: every statement runs in a read-only
    // transaction, so a DELETE would be rejected by the server. Assert both
    // that the store never emits one and that the read-only transaction frames
    // every single query it does emit.
    const fake = fakePool({
      payload: payloadRecord(),
      header: { headerHash: HEADER_HASH },
    });
    const store = await openWith(fake.pool);
    await store.getDaPayload(HEADER_HASH);
    await store.getStateQueueHeader(HEADER_HASH);

    const beginIndexes = fake.queries
      .map((query, index) => (query === "BEGIN READ ONLY" ? index : -1))
      .filter((index) => index >= 0);
    expect(beginIndexes).toHaveLength(3);
    expect(fake.queries.filter((query) => query === "COMMIT")).toHaveLength(3);
    // No statement escapes a BEGIN READ ONLY frame.
    expect(beginIndexes[0]).toBe(0);
    for (const query of fake.queries) {
      expect(
        /\b(?:DELETE|TRUNCATE|DROP)\s+(?:FROM\s+)?watcher_/iu.test(query),
      ).toBe(false);
    }
    await store.close();
  });

  it("rejects malformed payloads and row-key mismatches", async () => {
    const malformed = await openWith(
      fakePool({ payload: { headerHash: HEADER_HASH } }).pool,
    );
    await expect(malformed.getDaPayload(HEADER_HASH)).rejects.toThrow(
      /DA stored payload record V1/u,
    );
    await malformed.close();

    const mismatched = await openWith(
      fakePool({ payload: payloadRecord("cd".repeat(28)) }).pool,
    );
    await expect(mismatched.getDaPayload(HEADER_HASH)).rejects.toThrow(
      /row key does not match/u,
    );
    await mismatched.close();

    const mismatchedHeader = await openWith(
      fakePool({ header: { headerHash: "cd".repeat(28) } }).pool,
    );
    await expect(
      mismatchedHeader.getStateQueueHeader(HEADER_HASH),
    ).rejects.toThrow(/row key does not match/u);
    await mismatchedHeader.close();
  });
});

/**
 * The privilege probe in `assertReadOnlyRole` is a SQL claim about a real
 * PostgreSQL cluster: "this login cannot write the retained-evidence tables".
 * A fake client can only replay whatever booleans a test hands it, so the
 * claim is settled here against a real server — the probe SQL is run against
 * actual roles and grants, and PostgreSQL itself decides the answers.
 *
 * This suite fails closed: with no reachable cluster it errors, it never
 * skips. CI provides one (midgard-node-ci `postgres` service, POSTGRES_*).
 */
describe("PostgresPublicRetainedDaStore against a real PostgreSQL cluster", () => {
  const suffix = randomBytes(6).toString("hex");
  const databaseName = `midgard_public_reader_${suffix}`;
  const readerRole = `midgard_public_reader_${suffix}`;
  const readerPassword = `pw_${suffix}`;
  const admin = {
    host: process.env.POSTGRES_HOST ?? "127.0.0.1",
    port: Number(process.env.POSTGRES_PORT ?? "5432"),
    user: process.env.POSTGRES_USER ?? "postgres",
    password: process.env.POSTGRES_PASSWORD ?? "postgres",
  };
  const readerUrl = `postgresql://${readerRole}:${readerPassword}@${admin.host}:${admin.port.toString()}/${databaseName}`;
  let clusterClient: Client;
  let dbClient: Client;

  const adminClient = async (database: string): Promise<Client> => {
    const client = new Client({ ...admin, database });
    await client.connect();
    return client;
  };

  beforeAll(async () => {
    clusterClient = await adminClient(process.env.POSTGRES_DB ?? "postgres");
    await clusterClient.query(`CREATE DATABASE ${databaseName}`);
    await clusterClient.query(
      `CREATE ROLE ${readerRole} LOGIN PASSWORD '${readerPassword}'`,
    );
    dbClient = await adminClient(databaseName);
    await dbClient.query(
      "CREATE TABLE watcher_da_payloads (header_hash text PRIMARY KEY, record jsonb NOT NULL)",
    );
    await dbClient.query(
      "CREATE TABLE watcher_state_queue_headers (header_hash text PRIMARY KEY, record jsonb NOT NULL)",
    );
    await dbClient.query(
      "INSERT INTO watcher_da_payloads (header_hash, record) VALUES ($1, $2)",
      [HEADER_HASH, JSON.stringify(payloadRecord())],
    );
    await dbClient.query(
      "INSERT INTO watcher_state_queue_headers (header_hash, record) VALUES ($1, $2)",
      [HEADER_HASH, JSON.stringify({ headerHash: HEADER_HASH })],
    );
    await dbClient.query(
      `GRANT CONNECT ON DATABASE ${databaseName} TO ${readerRole}`,
    );
    await dbClient.query(`GRANT USAGE ON SCHEMA public TO ${readerRole}`);
    await dbClient.query(
      `GRANT SELECT ON watcher_da_payloads, watcher_state_queue_headers TO ${readerRole}`,
    );
  }, 60_000);

  afterAll(async () => {
    await dbClient?.end();
    await clusterClient?.query(`DROP DATABASE IF EXISTS ${databaseName}`);
    await clusterClient?.query(`DROP ROLE IF EXISTS ${readerRole}`);
    await clusterClient?.end();
  }, 60_000);

  const openReal = () =>
    PostgresPublicRetainedDaStore.open({
      databaseUrl: readerUrl,
      expectedRole: readerRole,
    });

  it("opens on a SELECT-only login and reads the retained evidence PostgreSQL actually stores", async () => {
    const store = await openReal();
    try {
      await expect(store.getDaPayload(HEADER_HASH)).resolves.toMatchObject({
        headerHash: HEADER_HASH,
        validationStatus: "verified",
        payloadSha256: "ef".repeat(32),
      });
      await expect(store.getStateQueueHeader(HEADER_HASH)).resolves.toEqual({
        headerHash: HEADER_HASH,
      });
      await expect(
        store.getDaPayload("ba".repeat(28)),
      ).resolves.toBeUndefined();
    } finally {
      await store.close();
    }
  }, 60_000);

  it("refuses the same login the moment PostgreSQL grants it DELETE, and admits it again when the grant is revoked", async () => {
    // The whole point of the probe: a login that merely HOLDS DELETE on the
    // retained-evidence table is refused. Only a real cluster can decide
    // whether the probe's has_table_privilege SQL sees that grant.
    await dbClient.query(
      `GRANT DELETE ON watcher_da_payloads TO ${readerRole}`,
    );
    try {
      await expect(openReal()).rejects.toThrow(/SELECT-only role/u);
    } finally {
      // Revoked in a finally so a failure here cannot leak the grant into
      // the following cases and manufacture cascading failures.
      await dbClient.query(
        `REVOKE DELETE ON watcher_da_payloads FROM ${readerRole}`,
      );
    }
    const store = await openReal();
    await store.close();
  }, 60_000);

  it("refuses a login that inherits pg_read_all_data", async () => {
    await dbClient.query(`GRANT pg_read_all_data TO ${readerRole}`);
    try {
      await expect(openReal()).rejects.toThrow(/SELECT-only role/u);
    } finally {
      await dbClient.query(`REVOKE pg_read_all_data FROM ${readerRole}`);
    }
    const store = await openReal();
    await store.close();
  }, 60_000);

  it("has no write path even for a superuser inside BEGIN READ ONLY", async () => {
    // The second barrier, decided by the server rather than by a double: a
    // DELETE inside the store's transaction mode is rejected with 25006 even
    // for the cluster owner, so no privilege escalation reopens the pruning
    // path the store deliberately lacks.
    await dbClient.query("BEGIN READ ONLY");
    await expect(
      dbClient.query("DELETE FROM watcher_da_payloads"),
    ).rejects.toMatchObject({ code: "25006" });
    await dbClient.query("ROLLBACK");
    // Nothing was pruned.
    const remaining = await dbClient.query<{ readonly count: string }>(
      "SELECT count(*)::text AS count FROM watcher_da_payloads",
    );
    expect(remaining.rows[0]?.count).toBe("1");
  }, 60_000);

  it("refuses the reader's own DELETE attempt with a privilege error", async () => {
    const reader = new Client({
      ...admin,
      user: readerRole,
      password: readerPassword,
      database: databaseName,
    });
    await reader.connect();
    try {
      await expect(
        reader.query("DELETE FROM watcher_da_payloads"),
      ).rejects.toMatchObject({ code: "42501" });
    } finally {
      await reader.end();
    }
  }, 60_000);
});
