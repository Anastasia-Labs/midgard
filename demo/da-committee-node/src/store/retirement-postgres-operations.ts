import type { Pool, PoolClient } from "pg";

import type { InFlightDecisionAttempts } from "../store.committee-store.js";
import { encodeRecord } from "./postgres.assert-postgres-decision-retry.js";
import type { PostgresStoreInstanceLock } from "./postgres.instance-lock.js";
import {
  type CommitteeRetirementCertificate,
  consumeRetirementCertificate,
  readRetirementCertificate,
} from "./retirement-certificate.js";
import {
  type CommitteeRetirementBreachPoint,
  CommitteeRetirementController,
  makeRetirementFloor,
  parseRetirementBreachPoint,
} from "./retirement-model.js";
import {
  assertPostgresRetirementResources,
  readPostgresRetirementData,
  readPostgresRetirementFloor,
  writePostgresRetirementPlan,
} from "./retirement-postgres.js";
import { retirementStoreDigest } from "./retirement-transition.js";
export class PostgresRetirementOperations {
  constructor(
    private readonly pool: Pool,
    private readonly instanceLock: PostgresStoreInstanceLock,
    private readonly controller: CommitteeRetirementController,
    private readonly inFlight: Pick<InFlightDecisionAttempts, "has">,
  ) {}
  async applyRetirementCertificate(
    certificate: CommitteeRetirementCertificate,
  ): Promise<readonly string[]> {
    const v = readRetirementCertificate(certificate);
    await v.assertCurrent();
    this.controller.begin(v.snapshot.guard);
    let client: PoolClient | undefined;
    try {
      client = await this.pool.connect();
      await client.query("BEGIN");
      await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
      await this.instanceLock.assertHeldAtServer(client);
      const data = await readPostgresRetirementData(client);
      if (retirementStoreDigest(data) !== v.snapshot.digest)
        throw new Error("Retirement store snapshot changed");
      if (
        v.plan.headerHashes.some((h) => this.controller.pinned().has(h)) ||
        Object.values(data.decisionOutbox).some(
          (e) =>
            v.plan.headerHashes.includes(e.headerHash) &&
            this.inFlight.has(e.effectId),
        )
      )
        throw new Error("Retirement cohort acquired a live callback");
      await v.assertCurrent();
      v.assertScopeCurrent();
      const next = await writePostgresRetirementPlan(client, data, v.plan);
      await assertPostgresRetirementResources(client, next);
      v.assertScopeCurrent();
      await client.query("COMMIT");
      this.controller.load(next.retirementFloor);
      return v.plan.headerHashes;
    } catch (error) {
      if (client) await client.query("ROLLBACK");
      throw error;
    } finally {
      client?.release();
      consumeRetirementCertificate(certificate);
      this.controller.end();
    }
  }
  async recordRetirementBreach(
    reason: string,
    observedAt: CommitteeRetirementBreachPoint,
  ): Promise<void> {
    const point = parseRetirementBreachPoint(observedAt);
    this.controller.holdBreach();
    const client = await this.pool.connect();
    try {
      await client.query("BEGIN");
      await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
      await this.instanceLock.assertHeldAtServer(client);
      const persistedFloor = await readPostgresRetirementFloor(client);
      const floor =
        persistedFloor === undefined
          ? undefined
          : (await readPostgresRetirementData(client)).retirementFloor;
      if (floor && !floor.breach) {
        const { digest: _digest, ...prior } = floor;
        const next = makeRetirementFloor({
          ...prior,
          generation: floor.generation + 1,
          breach: { reason, observedAt: point },
        });
        await client.query(
          "UPDATE committee_retirement_metadata SET record=$1::jsonb WHERE id=1",
          [encodeRecord(next)],
        );
        await client.query("COMMIT");
        this.controller.load(next);
        this.controller.persistedBreach();
      } else {
        await client.query("COMMIT");
        this.controller.persistedBreach();
      }
    } catch (error) {
      await client.query("ROLLBACK");
      throw error;
    } finally {
      client.release();
    }
  }
}
