import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { Database } from "../src/services/database.js";
import { HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS } from "../src/services/history-commit-window.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceHistoryAdmissionClock,
  alignCommitSchedulerBeforeTestWorker,
  ensureSeparateCollateralUtxo,
  SDK,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

const readHistoryRows = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const cursors = yield* sql<{
        binding_digest: Buffer;
      }>`SELECT binding_digest FROM event_history_cursor ORDER BY binding_digest`;
      const deposits = yield* sql<{
        event_id: Buffer;
        l1_event_key: Buffer | null;
        l1_origin_outref: Buffer | null;
      }>`SELECT event_id, l1_event_key, l1_origin_outref
        FROM deposits_utxos ORDER BY event_id`;
      return {
        cursors: cursors.map((row) => row.binding_digest.toString("hex")),
        deposits: deposits.map((row) => ({
          eventId: row.event_id.toString("hex"),
          eventKey: row.l1_event_key?.toString("hex"),
          originOutRef: row.l1_origin_outref?.toString("hex"),
        })),
      };
    }).pipe(Effect.provide(Database.layer)),
  );

/** The production composition derives its history binding from the chain and
 * the deployment only. A node restarted against the same recorded chain behind
 * a different Ogmios host and port keeps its binding, so it adopts the journal
 * it captured before and keeps the deposit rows the follower-change driver
 * ingested, rather than capturing a second history. Transport ancestry is
 * synthetic, and the follower stands in for the emulator chain
 * (`emulator-l1-follower.ts`); the chain, deployment, ingestion and restart
 * are the production path. */
it("adopts its captured history after a restart through another Ogmios endpoint", async () => {
  const initial = await openHistoryProductionOwnerLifecycle();
  const { fixture, lucidService, binding } = initial;
  const wallet = fixture.depositorLucid;
  const address = await wallet.wallet().address();
  let h: Awaited<ReturnType<typeof initial.restartRuntime>> = initial;
  try {
    await advanceEmulatorPastLatestBlockEndTime(fixture);
    await ensureSeparateCollateralUtxo(wallet);
    await advanceHistoryAdmissionClock(fixture, "deposit");
    await alignCommitSchedulerBeforeTestWorker({
      fixture,
      lucidService,
      targetEndTimeMs:
        fixture.emulator.now() + HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
    });
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
    const depositHash = await signed.submit();
    expect(await wallet.awaitTx(depositHash)).toBe(true);
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

    const before = await readHistoryRows();
    expect(before.cursors).toEqual([binding.digest]);
    expect(before.deposits).toHaveLength(1);
    expect(before.deposits[0]!.eventId).toBe(
      deposits[0]!.idCbor.toString("hex"),
    );
    // The follower admission identity: the event key and the deposit output.
    expect(before.deposits[0]!.eventKey).toMatch(/^[0-9a-f]{64}$/u);
    expect(before.deposits[0]!.originOutRef).toMatch(
      new RegExp(`^${depositHash}[0-9a-f]{4}$`, "u"),
    );

    // The restart synchronizes on start: it reaches Ready only by loading the
    // journal under the fixture's binding; the driver's ingestion at the same
    // follower view leaves the row unchanged. A binding that named the
    // endpoint would find no journal and capture a fresh history.
    h = await initial.restartRuntime({
      ogmiosUrl: "ws://ogmios-behind-proxy.invalid:2337",
    });
    await h.synchronize();

    expect(await readHistoryRows()).toEqual(before);
  } finally {
    await h.close();
    vi.useRealTimers();
  }
});
