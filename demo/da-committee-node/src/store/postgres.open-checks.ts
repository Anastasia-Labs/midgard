import type { Pool, PoolClient } from "pg";

import {
  decodeRecord,
  encodeRecord,
} from "./postgres.assert-postgres-decision-retry.js";
import type { PostgresStoreInstanceLock } from "./postgres.instance-lock.js";
import { STORED_POINTS_SQL } from "./postgres.stored-l1-points.js";
import {
  type CommitteeRetirementBinding,
  makeRetirementFloor,
  parseRetirementBinding,
  parseRetirementFloor,
  retirementDigest,
} from "./retirement-model.js";

/**
 * What the store open checks the stored records against: the member's
 * current retirement binding (when its promise profile is adopted), the
 * deployment's L1 origin (when configured), and k, the depth past which a
 * terminal header record is final (the manifest's
 * `automaticRecoveryMaxDepth`).
 */
export type CommitteeStoreOpenChecks = Readonly<{
  retirementBinding?: CommitteeRetirementBinding;
  l1Origin?: Readonly<{ slot: number }>;
  securityParameter?: number;
}>;

/**
 * A stored retirement floor bound to anything but this member's current
 * binding, beyond its L1 source-authority digest. The open throws it, so the
 * startup fails on it at once (`committee_startup_failed`) and the process
 * holds, unready, until it is restarted.
 */
export const COMMITTEE_RETIREMENT_BINDING_CHANGED =
  "committee_retirement_binding_changed";

/**
 * A stored record names an L1 point older than the configured `L1_ORIGIN`.
 * The follower's origin is the deployment's init point, so no committee
 * record can predate it: the configuration is wrong. The open throws it, so
 * the startup fails on it at once (`committee_startup_failed`) and the
 * process holds, unready, until it is restarted on the corrected `L1_ORIGIN`.
 */
export const COMMITTEE_STORE_POINT_BEFORE_L1_ORIGIN =
  "committee_store_point_before_l1_origin";

/**
 * Runs `run` in one transaction on a connection the server confirms still
 * holds the store's instance lock.
 */
export const fencedOpenTransaction = async <T>(
  pool: Pool,
  lock: Pick<PostgresStoreInstanceLock, "assertHeldAtServer">,
  run: (client: PoolClient) => Promise<T>,
): Promise<T> => {
  const client = await pool.connect();
  try {
    await client.query("BEGIN");
    await lock.assertHeldAtServer(client);
    const result = await run(client);
    await client.query("COMMIT");
    return result;
  } catch (error) {
    await client.query("ROLLBACK").catch(() => undefined);
    throw error;
  } finally {
    client.release();
  }
};

const BINDING_FIELDS = [
  "actorId",
  "committeeSignersHash",
  "contractManifestId",
  "deploymentFingerprint",
  "manifestSha256",
  "maximumEncodedBytes",
  "maximumRecords",
  "peerIds",
  "recoveryDepth",
  "retentionDays",
] as const;

/**
 * Re-binds a stored retirement floor to the current L1 source-authority
 * digest (whose formula no longer covers the L1 origin), under the instance
 * lock and idempotently: only `sourceAuthoritySha256` is rewritten, only on
 * a floor whose every other binding field equals the current binding, and
 * the floor keeps its generation, points and breach. A floor differing in
 * any other field is refused, naming the fields.
 */
export const rebindRetirementFloor = async (
  client: PoolClient,
  configured: CommitteeRetirementBinding,
  write: (line: string) => void,
): Promise<void> => {
  const binding = parseRetirementBinding(configured);
  // The retirement write lock every floor write takes.
  await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
  const stored = await client.query<{ readonly record: unknown }>(
    "SELECT record FROM committee_retirement_metadata WHERE id = 1 FOR UPDATE",
  );
  const row = stored.rows[0];
  if (row === undefined) return;
  const floor = parseRetirementFloor(decodeRecord(row.record));
  if (retirementDigest(floor.binding) === retirementDigest(binding)) return;
  const differing = BINDING_FIELDS.filter(
    (field) =>
      retirementDigest(floor.binding[field]) !==
      retirementDigest(binding[field]),
  );
  if (differing.length > 0)
    throw new Error(
      `${COMMITTEE_RETIREMENT_BINDING_CHANGED}: the stored retirement floor is bound to another ${differing.join(", ")}`,
    );
  const { digest, ...rest } = floor;
  void digest;
  const rebound = makeRetirementFloor({
    ...rest,
    binding: {
      ...floor.binding,
      sourceAuthoritySha256: binding.sourceAuthoritySha256,
    },
  });
  await client.query(
    "UPDATE committee_retirement_metadata SET record = $1::jsonb WHERE id = 1",
    [encodeRecord(rebound)],
  );
  write(
    `${JSON.stringify({
      event: "committee_retirement_floor_rebound",
      from: floor.binding.sourceAuthoritySha256,
      to: binding.sourceAuthoritySha256,
    })}\n`,
  );
};

/**
 * Refuses the open when a stored record names a point older than the L1
 * origin: `COMMITTEE_STORE_POINT_BEFORE_L1_ORIGIN`, naming the first one.
 */
export const assertNoPointBeforeL1Origin = async (
  client: Pick<PoolClient, "query">,
  origin: Readonly<{ slot: number }>,
): Promise<void> => {
  const found = await client.query<{
    readonly holder: string;
    readonly slot: string;
  }>(
    `SELECT holder, slot FROM (${STORED_POINTS_SQL}) p
      WHERE slot IS NOT NULL AND block_hash IS NOT NULL AND slot < $1
      ORDER BY slot, holder LIMIT 1`,
    [origin.slot],
  );
  const first = found.rows[0];
  if (first !== undefined)
    throw new Error(
      `${COMMITTEE_STORE_POINT_BEFORE_L1_ORIGIN}: the ${first.holder} names slot ${first.slot}, before L1_ORIGIN slot ${origin.slot.toString()}; L1_ORIGIN must be the deployment's init point`,
    );
};
