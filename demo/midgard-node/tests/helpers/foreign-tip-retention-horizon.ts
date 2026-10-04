import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, it } from "vitest";

import * as ForeignTips from "../../src/database/foreignTipReconciliations.js";
import type { historyOwnerCoverage } from "../../src/services/event-history-owner.coverage.js";

type Coverage = ReturnType<typeof historyOwnerCoverage>;

/** Unresolved and other-deployment evidence goes only once the window was
 * ingested past the challengeability horizon and no verdict or event pins it. */
export const registerForeignTipHorizonTests = (
  setup: (lateKind?: "deposit") => Promise<{
    put: (
      n: number,
      options?: {
        resolved?: boolean;
        manifest?: string;
        start?: number;
        end?: number;
        header?: Partial<SDK.Header>;
      },
    ) => Promise<string>;
    sweep: (options?: {
      coverage?: Coverage;
      txOrdersIngestedThrough?: number | "unset";
    }) => Promise<void>;
    exists: (key: string) => Promise<boolean>;
    coverage: Coverage;
  }>,
  run: <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) => Promise<A>,
  hash: (n: number) => string,
) => {
  const ingestedPastHorizon = (coverage: Coverage) => ({
    ...coverage,
    includedThroughMs: MIDGARD_RETENTION_WINDOW.requiredRetentionMs + 1_000_001,
  });

  it("prunes awaiting evidence a window can lift, and other deployments' evidence, once ingested past the horizon", async () => {
    const f = await setup();
    const liftable = await f.put(1, { resolved: false });
    const invalid = await f.put(2, { resolved: false });
    const present = await f.put(3, { resolved: false });
    const malformed = await f.put(4, {
      resolved: false,
      header: { depositCount: 1n, totalEventCount: 1n },
    });
    const foreign = await f.put(5, { manifest: hash(999) });
    await run(
      ForeignTips.markAwaiting({
        foreignHeaderHash: invalid,
        reason: "invalid:payload_failed_verification",
      }),
    );
    await run(
      ForeignTips.markAwaiting({
        foreignHeaderHash: present,
        reason: "foreign_event_present_requires_finalization:deposit",
      }),
    );
    const all = [liftable, invalid, present, malformed, foreign];
    await f.sweep({ coverage: f.coverage });
    for (const key of all)
      expect(
        await f.exists(key),
        "a window not yet ingested for the horizon keeps every unresolved or foreign row",
      ).toBe(true);
    await f.sweep({ coverage: ingestedPastHorizon(f.coverage) });
    expect(await f.exists(liftable)).toBe(false);
    expect(await f.exists(foreign)).toBe(false);
    for (const key of [invalid, present, malformed])
      expect(
        await f.exists(key),
        "a verdict no window lifts keeps its row",
      ).toBe(true);
  });

  it("retains awaiting evidence while an event no header carries yet occupies its window, even once consumed", async () => {
    const f = await setup("deposit");
    const key = await f.put(1, {
      resolved: false,
      start: 999_999,
      end: 1_000_000,
    });
    const coverage = ingestedPastHorizon(f.coverage);
    await f.sweep({ coverage });
    expect(await f.exists(key)).toBe(true);
    // An L2 transaction spent the deposit's mempool output, but no header
    // carries the deposit: the commit gate still counts it, so the peer block
    // may carry it and its row must keep gating.
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE deposits_utxos SET status = 'consumed'`;
      }),
    );
    await f.sweep({ coverage });
    expect(
      await f.exists(key),
      "a consumed deposit no header carries still occupies the window",
    ).toBe(true);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`UPDATE deposits_utxos SET projected_header_hash = ${Buffer.alloc(28, 0x77)}`;
      }),
    );
    await f.sweep({ coverage });
    expect(await f.exists(key)).toBe(false);
  });

  it("retains awaiting and other deployments' evidence until forced transactions are also ingested past the horizon", async () => {
    const f = await setup();
    const liftable = await f.put(1, { resolved: false });
    const foreign = await f.put(2, { manifest: hash(999) });
    const coverage = ingestedPastHorizon(f.coverage);
    await f.sweep({
      coverage,
      txOrdersIngestedThrough: MIDGARD_RETENTION_WINDOW.requiredRetentionMs + 1,
    });
    for (const key of [liftable, foreign])
      expect(
        await f.exists(key),
        "a window the tx-order barrier has not covered for the horizon keeps its row",
      ).toBe(true);
    await f.sweep({ coverage, txOrdersIngestedThrough: "unset" });
    for (const key of [liftable, foreign])
      expect(
        await f.exists(key),
        "a node that has not reconciled tx orders yet keeps every row",
      ).toBe(true);
    await f.sweep({ coverage });
    expect(await f.exists(liftable)).toBe(false);
    expect(await f.exists(foreign)).toBe(false);
  });

  it("retains resolved evidence of this deployment until forced transactions are also ingested past the horizon", async () => {
    const f = await setup();
    const resolved = await f.put(1);
    const coverage = ingestedPastHorizon(f.coverage);
    await f.sweep({
      coverage,
      txOrdersIngestedThrough: MIDGARD_RETENTION_WINDOW.requiredRetentionMs + 1,
    });
    expect(
      await f.exists(resolved),
      "a resolved row whose window the tx-order watermark has not covered for the horizon is kept",
    ).toBe(true);
    await f.sweep({ coverage });
    expect(await f.exists(resolved)).toBe(false);
  });

  it("prunes foreign-tip evidence even when the DA payload prune fails, and still fails the sweep", async () => {
    const f = await setup();
    const key = await f.put(1, { resolved: false });
    const coverage = ingestedPastHorizon(f.coverage);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`ALTER TABLE da_payloads RENAME TO da_payloads_retention_test`;
      }),
    );
    try {
      await expect(f.sweep({ coverage })).rejects.toThrow();
    } finally {
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`ALTER TABLE da_payloads_retention_test RENAME TO da_payloads`;
        }),
      );
    }
    expect(await f.exists(key)).toBe(false);
  });

  it("keeps sweeping when foreign-tip deletion fails, and prunes once it recovers", async () => {
    const f = await setup();
    const key = await f.put(1, { resolved: false });
    const coverage = ingestedPastHorizon(f.coverage);
    await run(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql.unsafe(`
          CREATE OR REPLACE FUNCTION foreign_tip_retention_refuse_delete()
          RETURNS trigger LANGUAGE plpgsql AS $$
          BEGIN RAISE EXCEPTION 'injected prune failure'; END $$;
          CREATE TRIGGER foreign_tip_retention_refuse_delete
          BEFORE DELETE ON foreign_tip_reconciliations
          FOR EACH ROW EXECUTE FUNCTION foreign_tip_retention_refuse_delete();
        `);
      }),
    );
    try {
      await f.sweep({ coverage });
      expect(await f.exists(key)).toBe(true);
    } finally {
      await run(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql.unsafe(`
            DROP TRIGGER IF EXISTS foreign_tip_retention_refuse_delete
              ON foreign_tip_reconciliations;
            DROP FUNCTION IF EXISTS foreign_tip_retention_refuse_delete();
          `);
        }),
      );
    }
    await f.sweep({ coverage });
    expect(await f.exists(key)).toBe(false);
  });
};
