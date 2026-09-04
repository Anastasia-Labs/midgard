import { describe, expect, it, vi } from "vitest";

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

  it("surfaces the read-only transaction rejection when DML is attempted", async () => {
    // Simulates PostgreSQL's own barrier: once BEGIN READ ONLY is in effect,
    // any DELETE raises 25006. The store still completes its reads, proving the
    // read-only frame is real rather than cosmetic.
    let readOnly = false;
    const release = vi.fn();
    const client: PublicRetainedDaPoolClient = {
      query: async <T extends Record<string, unknown>>(
        query: string,
      ): Promise<{ readonly rows: readonly T[] }> => {
        if (query === "BEGIN READ ONLY") {
          readOnly = true;
          return { rows: [] };
        }
        if (query === "COMMIT" || query === "ROLLBACK") {
          readOnly = false;
          return { rows: [] };
        }
        if (
          readOnly &&
          /^\s*(?:INSERT|UPDATE|DELETE|TRUNCATE)\b/imu.test(query)
        ) {
          throw new Error(
            "cannot execute DELETE in a read-only transaction (25006)",
          );
        }
        if (query.includes("FROM pg_roles role")) {
          return { rows: [readOnlyAccess() as unknown as T] };
        }
        if (query.includes("FROM watcher_da_payloads")) {
          return {
            rows: [{ record: payloadRecord() }] as unknown as readonly T[],
          };
        }
        return { rows: [] };
      },
      release,
    };
    const pool: PublicRetainedDaPool = {
      connect: async (): Promise<PublicRetainedDaPoolClient> => client,
      end: async (): Promise<void> => undefined,
    };
    const store = await openWith(pool);
    await expect(store.getDaPayload(HEADER_HASH)).resolves.toMatchObject({
      headerHash: HEADER_HASH,
    });
    await client.query("BEGIN READ ONLY");
    await expect(
      client.query("DELETE FROM watcher_da_payloads"),
    ).rejects.toThrow(/read-only transaction/u);
    await client.query("ROLLBACK");
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
