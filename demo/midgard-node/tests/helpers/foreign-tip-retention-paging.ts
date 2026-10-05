import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import { makeDeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, it } from "vitest";

import * as ForeignTips from "../../src/database/foreignTipReconciliations.js";

export const registerForeignTipPagingTests = (
  setup: () => Promise<{
    putEffect: (
      n: number,
      options: { resolved: boolean },
    ) => Effect.Effect<string, unknown, SqlClient.SqlClient>;
    sweep: () => Promise<void>;
  }>,
  run: <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) => Promise<A>,
  manifestId: () => string,
) => {
  it("deletes foreign evidence in bounded batches until drained, and keyset pages across resolution changes", async () => {
    const f = await setup();
    const keys = await run(
      Effect.forEach(
        Array.from(
          { length: ForeignTips.FOREIGN_TIP_RETENTION_BATCH_SIZE + 2 },
          (_, n) => n + 1,
        ),
        (n) => f.putEffect(n, { resolved: false }),
        { concurrency: 1 },
      ),
    );
    const scope = {
      manifestId: manifestId(),
      consensusProfileId: MIDGARD_CONSENSUS_PROFILE_ID,
    };
    const page = (after?: ForeignTips.ForeignTipEvidencePageCursor) =>
      run(
        Effect.map(
          ForeignTips.retrieveActionableEvidencePage(scope, after),
          ({ entries, undecodable }) => {
            expect(undecodable).toEqual([]);
            return entries;
          },
        ),
      );
    const retained = () =>
      run(
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) =>
            sql`SELECT foreign_header_hash FROM foreign_tip_reconciliations`,
        ),
      );
    const first = await page();
    expect(first).toHaveLength(
      ForeignTips.FOREIGN_TIP_RECONCILIATION_PAGE_SIZE,
    );
    await run(
      Effect.forEach(
        first,
        (row) =>
          ForeignTips.markResolved({
            foreignHeaderHash: row.foreign_header_hash.toString("hex"),
            deploymentMarker: makeDeploymentMarker(manifestId()),
            evidence: { kind: ForeignTips.EvidenceKind.VerifiedEmpty },
          }),
        { concurrency: 1 },
      ),
    );
    const last = first[first.length - 1]!;
    const second = await page({
      startTime: last.block_start_time,
      endTime: last.block_end_time,
      headerHash: last.foreign_header_hash,
    });
    expect(second).toHaveLength(
      ForeignTips.FOREIGN_TIP_RECONCILIATION_PAGE_SIZE,
    );
    const secondLast = second[second.length - 1]!;
    const third = await page({
      startTime: secondLast.block_start_time,
      endTime: secondLast.block_end_time,
      headerHash: secondLast.foreign_header_hash,
    });
    expect(third).toHaveLength(2);
    expect(
      new Set(
        [...first, ...second, ...third].map((row) =>
          row.foreign_header_hash.toString("hex"),
        ),
      ).size,
    ).toBe(keys.length);
    await run(
      Effect.forEach(
        keys,
        (key) =>
          ForeignTips.markResolved({
            foreignHeaderHash: key,
            deploymentMarker: makeDeploymentMarker(manifestId()),
            evidence: { kind: ForeignTips.EvidenceKind.VerifiedEmpty },
          }),
        { concurrency: 1 },
      ),
    );
    expect(await page()).toEqual([]);
    // More rows than one batch: one sweep keeps deleting until a batch comes
    // back short.
    await f.sweep();
    expect(await retained()).toEqual([]);
  });
};
