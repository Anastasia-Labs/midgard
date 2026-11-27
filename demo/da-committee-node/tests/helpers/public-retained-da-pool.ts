import { vi } from "vitest";

import {
  PostgresPublicRetainedDaStore,
  type PublicRetainedDaPool,
  type PublicRetainedDaPoolClient,
} from "../../src/store/public-retained-da.js";

export const EXPECTED_ROLE = "midgard_public_reader";

export type Access = {
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

export const readOnlyAccess = (overrides: Partial<Access> = {}): Access => ({
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

export const fakePool = ({
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
      if (query.includes("FROM committee_da_payloads")) {
        return {
          rows:
            payload === undefined
              ? []
              : ([{ record: payload }] as unknown as readonly T[]),
        };
      }
      if (query.includes("FROM committee_state_queue_headers")) {
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

export const openWith = async (pool: PublicRetainedDaPool) =>
  PostgresPublicRetainedDaStore.open({
    databaseUrl: "postgresql://midgard_public_reader@db.example/midgard",
    expectedRole: EXPECTED_ROLE,
    poolFactory: () => pool,
  });
