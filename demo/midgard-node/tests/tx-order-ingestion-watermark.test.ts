import "./utils.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect, Exit, Ref } from "effect";
import { describe, expect, it } from "vitest";

import { reconcileVisibleTxOrderUTxOs } from "../src/fibers/fetch-and-insert-tx-order-utxos.js";
import { type UserEventFetchBounds } from "../src/fibers/user-event-ingestion.js";
import {
  ContractDeploymentIdentity,
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";

/**
 * Runs the shared tx-order reconcile against an L1 with no visible order,
 * so it touches no database, and returns the watermark it left.
 */
const reconcile = (
  calls: readonly (UserEventFetchBounds | undefined)[],
  options: { readonly failing?: boolean; readonly initial?: number } = {},
) =>
  Effect.runPromise(
    Effect.gen(function* () {
      const globals = yield* Globals;
      yield* Ref.set(globals.TX_ORDERS_INGESTED_THROUGH_MS, options.initial);
      const exits = [];
      for (const bounds of calls)
        exits.push(yield* Effect.exit(reconcileVisibleTxOrderUTxOs(bounds)));
      return {
        exits,
        watermark: yield* Ref.get(globals.TX_ORDERS_INGESTED_THROUGH_MS),
      };
    }).pipe(
      Effect.provideService(Lucid, {
        api: {
          utxosAt: async () => {
            if (options.failing === true) throw new Error("kupo unavailable");
            return [];
          },
        },
      } as never),
      Effect.provideService(MidgardContracts, {
        txOrder: { spendingScriptAddress: "addr_test", policyId: "00" },
        cekProgramMaterial: { spendingScriptHash: "11" },
      } as never),
      Effect.provideService(ContractDeploymentIdentity, {
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      } as never),
      // No visible order, so the reconcile issues no statement.
      Effect.provideService(SqlClient.SqlClient, {} as never),
      Effect.provideService(NodeConfig, {} as never),
      Effect.provide(Globals.Default),
    ),
  );

describe("tx-order ingestion watermark", () => {
  it("records the fetch start for an unbounded reconcile", async () => {
    const before = Date.now();
    const { exits, watermark } = await reconcile([undefined]);
    expect(exits.every(Exit.isSuccess)).toBe(true);
    expect(watermark).toBeGreaterThanOrEqual(before);
    expect(watermark).toBeLessThanOrEqual(Date.now());
  });

  it("records the barrier's inclusive bound when it is behind the fetch start", async () => {
    const { watermark } = await reconcile([
      { inclusionTimeUpperBound: 1_001n },
    ]);
    expect(watermark).toBe(1_000);
  });

  it("never records past the fetch start, whatever the requested bound", async () => {
    const { watermark } = await reconcile([
      { inclusionTimeUpperBound: BigInt(Date.now() + 3_600_000) },
    ]);
    expect(watermark).toBeLessThanOrEqual(Date.now());
  });

  it("only advances, and records nothing for a failed or lower-bounded reconcile", async () => {
    expect(
      (
        await reconcile([{ inclusionTimeUpperBound: 1_001n }], {
          initial: 5_000,
        })
      ).watermark,
    ).toBe(5_000);
    expect(
      (
        await reconcile([{ inclusionTimeLowerBound: 1n }], {
          initial: 5_000,
        })
      ).watermark,
    ).toBe(5_000);
    const failed = await reconcile([undefined], { failing: true });
    expect(failed.exits.every(Exit.isFailure)).toBe(true);
    expect(failed.watermark).toBeUndefined();
  });
});
