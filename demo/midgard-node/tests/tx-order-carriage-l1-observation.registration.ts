import "./tx-order-carriage-l1-observation.v1-forced-order-8-carriage-read-off-l1.js";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  ForcedTransactionsDB,
  MigrationRunner,
} from "../src/database/index.js";
import { reconcileVisibleTxOrderUTxOs } from "../src/fibers/fetch-and-insert-tx-order-utxos.js";
import { txOrderMintRedeemer } from "../src/l1-tx-order-carriage.js";
import {
  ContractDeploymentIdentity,
  Database,
  Lucid as LucidService,
  MidgardContracts,
  NodeConfig,
} from "../src/services/index.js";
import { nativeTransactionCbor } from "./tx-order-carriage-l1-observation.native-transaction-cbor.js";
import {
  submitForcedOrder,
  withHarness,
} from "./tx-order-carriage-l1-observation.submit-forced-order.js";

/**
 * The same read, reached the way production reaches it.
 *
 * Everything above calls {@link observeVisibleTxOrderCarriage} directly, which
 * leaves the wiring between it and the fiber's own entry point proved only by the
 * type-checker: `reconcileVisibleTxOrderUTxOs` threads the tx-order policy id and
 * the transport overrides down through `txOrderUTxOToEntry`, and a wrong thread
 * there fails *closed* — a material-bearing order simply stops being ingested,
 * with no red test to say so. This drives the whole path, from the visible order
 * set to the row in `forced_transaction_utxos`, and asserts on the bytes that
 * landed.
 *
 * It needs a live Postgres on 5433 (`docker-compose.dev.yaml`), which the suites
 * above do not, so it takes the package's existing opt-out for database-backed
 * tests — the one `retention-enforcement.test.ts` uses — rather than making the
 * whole file undrivable without a database.
 */
const dbEnabled = process.env.MIDGARD_SKIP_DB_TESTS !== "1";

const runWithDatabase = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
): Promise<A> =>
  Effect.runPromise(
    effect.pipe(
      Effect.provide(Database.layer),
      Effect.provide(NodeConfig.layer),
    ),
  );

describe.skipIf(!dbEnabled)(
  "V1 forced-order ingestion, into the database",
  () => {
    beforeAll(async () => {
      await runWithDatabase(
        MigrationRunner.migrate({
          appVersion: "test",
          actor: "tx-order-carriage-l1-observation-v1.test",
        }),
      );
    }, 300_000);

    it("reconciles a material-bearing order through the fiber into forced_transaction_utxos", async () => {
      const harness = await withHarness();
      await runWithDatabase(ForcedTransactionsDB.clear);

      // Tier 2, so the row can only exist if the Kupo reference-input resolution
      // ran: the preimage is in a published UTxO, not in the redeemer.
      const submittedTxCbor = nativeTransactionCbor([0x11, 0x22]);
      const { plan, orderUtxo } = await submitForcedOrder({
        harness,
        submittedTxCbor,
        inlineReserveBytes: 0,
      });
      expect(plan.carriage.map((field) => field.plan.tier)).toEqual([
        "RawUtxo",
      ]);

      const nodeConfig = await Effect.runPromise(
        Effect.gen(function* () {
          const base = yield* NodeConfig;
          return {
            ...base,
            L1_OGMIOS_KEY: harness.l1.ogmiosUrl,
            L1_KUPO_KEY: harness.l1.kupoUrl,
          };
        }).pipe(Effect.provide(NodeConfig.layer)),
      );

      const result = await Effect.runPromise(
        reconcileVisibleTxOrderUTxOs(undefined, { timeoutMs: 10_000 }).pipe(
          Effect.provideService(LucidService, { api: harness.lucid } as never),
          Effect.provideService(MidgardContracts, {
            ...harness.contracts,
            // Every validator in the always-succeeds blueprint compiles to the
            // same trivial script, so here the order's own UTxO also sits under
            // the CEK program-material credential. That is the public-network
            // shape too: the material script is a plain always-fails validator
            // whose credential anyone can pay to. The pass must skip the
            // non-material output and still ingest the order.
            consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          } as never),
          Effect.provideService(
            ContractDeploymentIdentity,
            ContractDeploymentIdentity.make({
              kind: "derived",
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
            }),
          ),
          Effect.provideService(NodeConfig, nodeConfig as never),
          Effect.provide(Database.layer),
        ) as Effect.Effect<
          Awaited<ReturnType<typeof Effect.runPromise>>,
          unknown,
          never
        >,
      );
      expect(result).toMatchObject({ reconciledCount: 1 });

      const rows = await runWithDatabase(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          return yield* sql<{
            readonly native_tx_cbor: Buffer;
            readonly tx_order_l1_tx_hash: Buffer;
            readonly consensus_profile_id: string;
          }>`
          SELECT ${sql(ForcedTransactionsDB.Columns.NATIVE_TX_CBOR)},
                 ${sql(ForcedTransactionsDB.Columns.TX_ORDER_L1_TX_HASH)},
                 ${sql(ForcedTransactionsDB.Columns.CONSENSUS_PROFILE_ID)}
          FROM ${sql(ForcedTransactionsDB.tableName)}
        `;
        }),
      );

      // The stored transaction is the one the order committed to, byte for byte —
      // which is only reachable by opening the published carriage against the
      // payload's own §4 commitments. Nothing in this file hands the fiber those
      // bytes; it read them off the local L1 itself.
      expect(rows.length).toBe(1);
      expect(Buffer.from(rows[0]!.native_tx_cbor)).toEqual(submittedTxCbor);
      expect(Buffer.from(rows[0]!.tx_order_l1_tx_hash).toString("hex")).toBe(
        orderUtxo.utxo.txHash,
      );

      await runWithDatabase(ForcedTransactionsDB.clear);
    }, 300_000);
  },
);

/**
 * The one piece of ledger arithmetic the read has to get right on its own.
 *
 * An order transaction observed above mints a single policy, which puts its
 * redeemer at pointer index 0 and cannot tell "this policy's redeemer" apart from
 * "the first mint redeemer". The Lucid builder cannot mix a Plutus mint with a
 * second policy in one emulator transaction, so the multi-policy case is pinned
 * here against the observation shape the reader consumes rather than through a
 * transaction that cannot be built.
 */
describe("tx-order mint redeemer selection", () => {
  const transaction = (
    mintPolicyIds: readonly string[],
    redeemers: readonly { purpose: string; index: number; redeemer: string }[],
  ) => ({
    txHash: "aa".repeat(32),
    referenceInputs: [],
    mintPolicyIds,
    redeemers,
  });
  const below = "11".repeat(28);
  const target = "22".repeat(28);

  it("selects by the policy's position in the ascending mint list", () => {
    expect(
      txOrderMintRedeemer(
        transaction(
          [below, target],
          [
            { purpose: "mint", index: 0, redeemer: "d87980" },
            { purpose: "mint", index: 1, redeemer: "d87a80" },
            { purpose: "spend", index: 1, redeemer: "d87b80" },
          ],
        ),
        target,
      ),
    ).toBe("d87a80");
  });

  it("refuses a transaction that mints nothing under the policy", () => {
    expect(() =>
      txOrderMintRedeemer(
        transaction(
          [below],
          [{ purpose: "mint", index: 0, redeemer: "d87980" }],
        ),
        target,
      ),
    ).toThrow(/mints nothing under the tx-order policy/u);
  });

  it("refuses a mint whose pointer names another policy", () => {
    // The redeemer is present, it is a mint redeemer, and it is the only one —
    // and it belongs to the policy at index 0. Taking it would authenticate an
    // order against a vector the tx-order validator never saw.
    expect(() =>
      txOrderMintRedeemer(
        transaction(
          [below, target],
          [{ purpose: "mint", index: 0, redeemer: "d87980" }],
        ),
        target,
      ),
    ).toThrow(/carries no mint redeemer for the tx-order policy/u);
  });
});
