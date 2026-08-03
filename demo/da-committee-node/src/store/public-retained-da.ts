import { Pool } from "pg";

import type {
  DaStoredPayloadRecordV1,
  StateQueueHeaderRecord,
} from "../domain.js";
import { parseDaStoredPayloadRecordV1 } from "../domain.js";
import type { WatcherStore } from "../store.js";

/** The complete storage authority of the standalone public listener. */
export type PublicRetainedDaStore = Pick<
  WatcherStore,
  "close" | "getDaPayload" | "getStateQueueHeader"
>;

type StoredRecordRow = { readonly record: unknown };

export type PublicRetainedDaPoolClient = {
  query<T extends Record<string, unknown> = Record<string, unknown>>(
    query: string,
    values?: readonly unknown[],
  ): Promise<{ readonly rows: readonly T[] }>;
  release(): void;
};

export type PublicRetainedDaPool = {
  connect(): Promise<PublicRetainedDaPoolClient>;
  end(): Promise<void>;
};

export type PublicRetainedDaPoolFactory = (options: {
  readonly databaseUrl: string;
}) => PublicRetainedDaPool;

/**
 * A database-only, read-only adapter. It never initializes schema or shares
 * the committee process's mutable store credentials. Every query runs in an
 * explicit PostgreSQL READ ONLY transaction, and startup verifies that the
 * configured role has SELECT but no DML privilege on either exposed table.
 */
export class PostgresPublicRetainedDaStore implements PublicRetainedDaStore {
  private constructor(private readonly pool: PublicRetainedDaPool) {}

  static async open({
    databaseUrl,
    expectedRole,
    poolFactory = defaultPublicRetainedDaPoolFactory,
  }: {
    readonly databaseUrl: string;
    readonly expectedRole: string;
    readonly poolFactory?: PublicRetainedDaPoolFactory;
  }): Promise<PostgresPublicRetainedDaStore> {
    const parsed = new URL(databaseUrl);
    if (parsed.protocol !== "postgres:" && parsed.protocol !== "postgresql:") {
      throw new Error(
        "DA_PUBLIC_RETAINED_DA_DATABASE_URL must be a postgres:// or postgresql:// URL",
      );
    }
    const store = new PostgresPublicRetainedDaStore(
      poolFactory({ databaseUrl }),
    );
    try {
      await store.assertReadOnlyRole(expectedRole);
      return store;
    } catch (error) {
      await store.close();
      throw error;
    }
  }

  async close(): Promise<void> {
    await this.pool.end();
  }

  async getDaPayload(
    headerHash: string,
  ): Promise<DaStoredPayloadRecordV1 | undefined> {
    return this.withReadOnlyTransaction(async (client) => {
      const result = await client.query<StoredRecordRow>(
        "SELECT record FROM watcher_da_payloads WHERE header_hash = $1",
        [headerHash],
      );
      const record = result.rows[0]?.record;
      if (record === undefined) return undefined;
      const parsed = parseDaStoredPayloadRecordV1(record);
      if (parsed.headerHash !== headerHash) {
        throw new Error(
          "public retained-DA payload row key does not match record identity",
        );
      }
      return parsed;
    });
  }

  async getStateQueueHeader(
    headerHash: string,
  ): Promise<StateQueueHeaderRecord | undefined> {
    return this.withReadOnlyTransaction(async (client) => {
      const result = await client.query<StoredRecordRow>(
        "SELECT record FROM watcher_state_queue_headers WHERE header_hash = $1",
        [headerHash],
      );
      const record = result.rows[0]?.record;
      if (!isStateQueueHeaderRecordFor(record, headerHash)) {
        return record === undefined
          ? undefined
          : invalidStateQueueHeaderRecord();
      }
      return record;
    });
  }

  private async assertReadOnlyRole(expectedRole: string): Promise<void> {
    await this.withReadOnlyTransaction(async (client) => {
      const result = await client.query<{
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
      }>(`
        SELECT current_user,
               session_user,
               role.rolsuper,
               role.rolbypassrls,
               role.rolcreaterole,
               role.rolcreatedb,
               role.rolreplication,
               EXISTS (
                 SELECT 1
                 FROM pg_roles granted
                 WHERE (granted.rolsuper
                        OR granted.rolbypassrls
                        OR granted.rolcreaterole
                        OR granted.rolcreatedb
                        OR granted.rolreplication)
                   AND pg_has_role(current_user, granted.oid, 'member')
               ) AS privileged_membership,
               pg_has_role(current_user, 'pg_read_all_data', 'member')
                 OR pg_has_role(current_user, 'pg_write_all_data', 'member')
                 OR pg_has_role(current_user, 'pg_monitor', 'member') AS broad_role_membership,
               has_table_privilege(current_user, 'watcher_da_payloads', 'SELECT') AS payload_select,
               has_table_privilege(current_user, 'watcher_da_payloads', 'INSERT')
                 OR has_table_privilege(current_user, 'watcher_da_payloads', 'UPDATE')
                 OR has_table_privilege(current_user, 'watcher_da_payloads', 'DELETE')
                 OR has_table_privilege(current_user, 'watcher_da_payloads', 'TRUNCATE') AS payload_write,
               has_table_privilege(current_user, 'watcher_state_queue_headers', 'SELECT') AS header_select,
               has_table_privilege(current_user, 'watcher_state_queue_headers', 'INSERT')
                 OR has_table_privilege(current_user, 'watcher_state_queue_headers', 'UPDATE')
                 OR has_table_privilege(current_user, 'watcher_state_queue_headers', 'DELETE')
                 OR has_table_privilege(current_user, 'watcher_state_queue_headers', 'TRUNCATE') AS header_write
        FROM pg_roles role
        WHERE role.rolname = current_user
      `);
      const access = result.rows[0];
      if (
        access === undefined ||
        access.current_user !== expectedRole ||
        access.session_user !== expectedRole ||
        access.rolsuper ||
        access.rolbypassrls ||
        access.rolcreaterole ||
        access.rolcreatedb ||
        access.rolreplication ||
        access.privileged_membership ||
        access.broad_role_membership ||
        !access.payload_select ||
        !access.header_select ||
        access.payload_write ||
        access.header_write
      ) {
        throw new Error(
          "DA public retained-DA database role must be the configured SELECT-only role for payload and state-header tables",
        );
      }
    });
  }

  private async withReadOnlyTransaction<T>(
    operation: (client: PublicRetainedDaPoolClient) => Promise<T>,
  ): Promise<T> {
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN READ ONLY");
      const result = await operation(client);
      await client.query("COMMIT");
      return result;
    } catch (error) {
      await client.query("ROLLBACK").catch(() => undefined);
      throw error;
    } finally {
      client.release();
    }
  }
}

const isStateQueueHeaderRecordFor = (
  value: unknown,
  headerHash: string,
): value is StateQueueHeaderRecord =>
  typeof value === "object" &&
  value !== null &&
  (value as { readonly headerHash?: unknown }).headerHash === headerHash;

const invalidStateQueueHeaderRecord = (): never => {
  throw new Error(
    "public retained-DA state header row key does not match record identity",
  );
};

const defaultPublicRetainedDaPoolFactory: PublicRetainedDaPoolFactory = ({
  databaseUrl,
}): PublicRetainedDaPool =>
  new Pool({
    connectionString: databaseUrl,
    max: 4,
  }) as unknown as PublicRetainedDaPool;
