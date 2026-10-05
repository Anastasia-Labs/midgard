import type { Pool } from "pg";

import {
  applyVerifiedL1Recovery,
  consumeL1RecoveryCertificate,
  type L1RecoveryCertificate,
  type L1RecoverySnapshot,
  l1RecoverySnapshot,
  readL1RecoveryCertificate,
} from "../l1/recovery-incident.js";
import { encodeRecord } from "./postgres.assert-postgres-decision-retry.js";
import type { PostgresStoreInstanceLock } from "./postgres.instance-lock.js";
import type { CommitteeRetirementController } from "./retirement-model.js";
import { readPostgresRetirementData } from "./retirement-postgres.js";

/** Both reads and CAS use the same existing writer lock and instance ownership. */
export const postgresL1RecoverySnapshot = async (
  pool: Pool,
  instanceLock: PostgresStoreInstanceLock,
): Promise<L1RecoverySnapshot> => {
  const client = await pool.connect();
  try {
    await client.query("BEGIN");
    await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
    await instanceLock.assertHeldAtServer(client);
    const snapshot = l1RecoverySnapshot(
      await readPostgresRetirementData(client),
    );
    await client.query("COMMIT");
    return snapshot;
  } catch (error) {
    await client.query("ROLLBACK");
    throw error;
  } finally {
    client.release();
  }
};

export const postgresApplyL1Recovery = async (
  pool: Pool,
  instanceLock: PostgresStoreInstanceLock,
  retirement: CommitteeRetirementController,
  certificate: L1RecoveryCertificate,
): Promise<void> => {
  const guard = retirement.capture();
  const client = await pool.connect();
  try {
    await client.query("BEGIN");
    await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
    await instanceLock.assertHeldAtServer(client);
    retirement.assert(guard);
    const data = await readPostgresRetirementData(client);
    const verified = readL1RecoveryCertificate(certificate);
    applyVerifiedL1Recovery(data, certificate);
    await verified.assertCurrent();
    retirement.assert(guard);
    const next = applyVerifiedL1Recovery(data, certificate);
    await instanceLock.assertHeldAtServer(client);
    verified.assertScopeCurrent();
    await client.query(
      "UPDATE committee_l1_source_state SET record=$1::jsonb, updated_at=NOW() WHERE id=1",
      [encodeRecord(next.chainCursor)],
    );
    verified.assertScopeCurrent();
    await client.query("COMMIT");
  } catch (error) {
    await client.query("ROLLBACK");
    throw error;
  } finally {
    consumeL1RecoveryCertificate(certificate);
    client.release();
  }
};
