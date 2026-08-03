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
