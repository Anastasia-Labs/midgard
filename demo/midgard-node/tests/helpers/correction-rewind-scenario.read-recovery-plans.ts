import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import {
  fetchLatestCommittedBlock,
  runCommitWorkerUntilSubmitted,
} from "../deposit-flow-emulator-shared.js";
import {
  commitLocallyFinalizedBlock,
  type Handle,
  type Lifecycle,
  read,
} from "./correction-rewind-scenario.commit-locally-finalized-block.js";
import { alignMempoolToEmulatorClock } from "./correction-rewind-scenario.read-acceptance-traces.js";

export const readRecoveryPlans = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        state: string;
        intent: string;
      }>`SELECT state, intent FROM event_history_recovery_plans
        ORDER BY created_at`;
      return rows.map((row) => ({
        state: row.state,
        intent: JSON.parse(row.intent) as {
          domain: string;
          headerHash: string;
          members?: readonly { headerHash: string; transitionDigest: string }[];
          expectedRoot: string;
          targetRoot: string;
        },
      }));
    }),
  );

export const readSqlLedgerRoot = () =>
  read(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const rows = yield* sql<{
        root_hex: string;
        utxo_payload_entry_count: number | string | null;
      }>`SELECT root_hex, utxo_payload_entry_count FROM mpf_engine_state
        WHERE store_name = 'ledger'`;
      expect(rows).toHaveLength(1);
      return rows[0]!;
    }),
  );

/** Commit the next block with the production commit worker and wait for its
 * L1 acceptance. */
export const commitNextBlock = async (h: Handle) => {
  const { fixture, lucidService, globals, production } = h;
  const next = await runCommitWorkerUntilSubmitted({
    fixture,
    lucidService,
    latestBlock: await fetchLatestCommittedBlock(
      fixture.operatorLucid,
      fixture.contracts,
    ),
    nodeConfig: production.nodeConfig,
    production: { ...production, globals },
  });
  expect(await fixture.operatorLucid.awaitTx(next.submittedTxHash)).toBe(true);
  return next;
};

/** Commit, confirm and locally finalize the next block with the production
 * owner; returns its header hash. */
export const commitAndLocallyFinalizeNextBlock = async (
  h: Pick<Lifecycle, "deployment"> & Handle,
) => {
  await alignMempoolToEmulatorClock(h);
  return commitLocallyFinalizedBlock(h, h.fixture.emulator.now() - 1000);
};

export const closeLifecycle = async (h: Pick<Lifecycle, "close">) => {
  try {
    await h.close();
  } finally {
    vi.useRealTimers();
  }
};
