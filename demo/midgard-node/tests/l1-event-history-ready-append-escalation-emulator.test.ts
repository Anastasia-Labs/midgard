import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import { expect, it, vi } from "vitest";

import * as Authority from "../src/database/eventHistoryAuthority.js";
import { Database } from "../src/services/database.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  ensureSeparateCollateralUtxo,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

// Every production deposit reconciliation, including one whose transaction
// later rolls back: the authority state it ran under, and how many already
// spendable rows it restored to mempool_ledger.
const restorations = vi.hoisted(
  () => [] as { state: string | null; restored: number }[],
);
vi.mock(
  "../src/fibers/project-deposits-to-mempool-ledger.js",
  async (importOriginal) => {
    const { Effect, Option } = await import("effect");
    const { currentOwnedTransaction } = await import(
      "../src/database/eventHistoryAuthority.js"
    );
    const actual =
      await importOriginal<
        typeof import("../src/fibers/project-deposits-to-mempool-ledger.js")
      >();
    return {
      ...actual,
      reconcileDepositProjection: (upTo: Date) =>
        actual.reconcileDepositProjection(upTo).pipe(
          Effect.tap(({ reconciled }) =>
            currentOwnedTransaction.pipe(
              Effect.map((owned) => {
                restorations.push({
                  state: Option.isSome(owned) ? owned.value.state : null,
                  restored: reconciled.spendableUpserts.length,
                });
              }),
            ),
          ),
        ),
    };
  },
);

const read = <A, E>(program: Effect.Effect<A, E, SqlClient.SqlClient>) =>
  Effect.runPromise(program.pipe(Effect.provide(Database.layer)));

// Production owner and reconciliation over an actual deployment and an actual
// admitted, projected deposit. The one synthetic input is SQL: that deposit is
// assigned to a header and its mempool_ledger row is missing, so the next
// reconciliation restores an already spendable row.
it("escalates a Ready append that restores spendable deposit rows to one recovery that reloads the cache", async () => {
  const h = await openHistoryProductionOwnerLifecycle();
  try {
    const { fixture } = h;
    const wallet = fixture.depositorLucid;
    const address = await wallet.wallet().address();
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await h.synchronize();
    const built = await Effect.runPromise(
      SDK.buildUnsignedDepositTxWithMetadataProgram(wallet, fixture.contracts, {
        l2Address: address,
        l2Datum: null,
        lovelace: 12_000_000n,
        additionalAssets: {},
        referenceScripts: fixture.referenceScripts.deposit,
      }),
    );
    const signed = await built.tx.sign.withWallet().complete();
    expect(await wallet.awaitTx(await signed.submit())).toBe(true);
    wallet.overrideUTxOs(await wallet.utxosAt(address));
    await h.synchronize();
    const deposits = await Effect.runPromise(
      SDK.fetchDepositUTxOsProgram(
        fixture.operatorLucid,
        SDK.eventHistoryDeploymentFromContracts(
          SDK.requireEventHistoryContracts(fixture.contracts).deposit,
        ),
      ),
    );
    expect(deposits).toHaveLength(1);
    await h.deployment.chain.awaitLedgerTime(
      Number(deposits[0]!.facts.inclusion_time) + 1000,
    );
    vi.setSystemTime(fixture.emulator.now());
    await h.synchronize();
    const eventId = await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const projected = yield* sql<{
          event_id: Buffer;
        }>`SELECT event_id FROM deposits_utxos WHERE status = 'projected'`;
        expect(projected).toHaveLength(1);
        const eventId = projected[0]!.event_id;
        const deleted =
          yield* sql`DELETE FROM mempool_ledger WHERE source_event_id = ${eventId} RETURNING outref`;
        expect(deleted).toHaveLength(1);
        yield* sql`UPDATE deposits_utxos SET projected_header_hash = ${Buffer.alloc(32, 0x55)}
          WHERE event_id = ${eventId}`;
        return eventId;
      }),
    );
    const before = await read(Authority.retrieve).then(Option.getOrThrow);
    expect(before.state).toBe("ready");
    restorations.length = 0;
    // One forward block at the head of the open gate. synchronize also
    // requires the validation cache to equal the durable spendable ledger.
    fixture.emulator.awaitBlock(1);
    await h.synchronize();
    // The Ready append restored the row, escalated and rolled back: the
    // recovery that then journaled the block found the row still missing.
    expect(restorations.filter(({ restored }) => restored > 0)).toEqual([
      { state: "ready", restored: 1 },
      { state: "recovering", restored: 1 },
    ]);
    const after = await read(Authority.retrieve).then(Option.getOrThrow);
    expect(after.state).toBe("ready");
    expect(BigInt(after.generation)).toBeGreaterThan(BigInt(before.generation));
    const restored = await read(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          outref: Buffer;
        }>`SELECT outref FROM mempool_ledger WHERE source_event_id = ${eventId}`;
      }),
    );
    expect(restored).toHaveLength(1);
    // Published by the recovery's cache reload, not by the append.
    const cache = h.production.cache;
    const cached = await Effect.runPromise(
      cache.withPhaseBLock(cache.currentState),
    );
    expect(cached.has(restored[0]!.outref.toString("hex"))).toBe(true);
  } finally {
    await h.close();
    vi.useRealTimers();
  }
}, 300_000);
