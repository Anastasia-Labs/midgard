import { inspect } from "node:util";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import { HistoryProducer } from "../src/services/event-history-producer.js";
import { assertForeignVerificationSource } from "../src/services/foreign-verification-source.js";
import { foreignEventCensus } from "../src/workers/commit-block-header.foreign-event-census.js";
import { ensureSeparateCollateralUtxo } from "./deposit-flow-emulator-shared.js";
import { fixture as rejectedForcedFixture } from "./foreign-block-import.fixture.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";
import { makeRollbackHistoryTransport } from "./helpers/history-rollback-transport.js";
import { historyOutputObservation } from "./helpers/history-projection-observations.js";
import { provideDatabaseLayers } from "./utils.js";

const openCensusOwner = (
  options: Parameters<typeof openHistoryProductionOwnerLifecycle>[0],
) =>
  openHistoryProductionOwnerLifecycle(options).catch((cause: unknown) => {
    throw new Error(
      `Forced census owner startup failed: ${inspect(cause, { depth: 20, colors: false })}`,
      { cause },
    );
  });

const mintForcedOrder = async (
  lifecycle: Awaited<ReturnType<typeof openHistoryProductionOwnerLifecycle>>,
) => {
  const { fixture } = lifecycle;
  const wallet = fixture.operatorLucid;
  const addressData = await Effect.runPromise(
    SDK.addressDataFromBech32(await wallet.wallet().address()),
  );
  const refundAddress: SDK.TxOrderRefundAddress = {
    paymentCredential: addressData.paymentCredential,
    stakeCredential:
      addressData.stakeCredential !== null &&
      "Pointer" in addressData.stakeCredential
        ? { Pointer: addressData.stakeCredential.Pointer[0] }
        : addressData.stakeCredential,
  };
  const nonce = (await wallet.wallet().getUtxos()).find(
    (utxo) => utxo.assets.lovelace >= 10_000_000n,
  );
  if (nonce === undefined)
    throw new Error("Forced census fixture has no funded nonce");
  const payload = await rejectedForcedFixture();
  const nativeCbor = payload.block_body.forced_transaction_preimages[0]?.[1];
  if (nativeCbor === undefined)
    throw new Error("Rejected forced fixture has no canonical bytes");
  const built = await Effect.runPromise(
    SDK.buildUnsignedTxOrderTxWithMetadataProgram(wallet, fixture.contracts, {
      nonceInput: nonce,
      submittedTxCbor: nativeCbor,
      refundAddress,
    }),
  );
  const signed = await built.tx.sign.withWallet().complete();
  const mintTxHash = await signed.submit();
  expect(await wallet.awaitTx(mintTxHash)).toBe(true);
  await lifecycle.deployment.chain.awaitLedgerTime(
    built.metadata.inclusionTime + 1000,
  );
  vi.setSystemTime(fixture.emulator.now());
  await lifecycle.synchronize();
  return {
    key: Data.to(built.metadata.txOrderId, SDK.OutputReference),
    mintTxHash,
    census: () =>
      foreignEventCensus("ab".repeat(28), {
        ...payload.block_body.header,
        startTime: BigInt(built.metadata.inclusionTime - 1),
        endTime: BigInt(built.metadata.inclusionTime),
      }),
  };
};

it("retains an actual forced NFT admission independently of ingestion rows and reacquires it through the same source owner after restart", async () => {
  const initial = await openCensusOwner({
    rollbackHorizon: 1,
  });
  let current: Pick<typeof initial, "command" | "synchronize" | "close"> =
    initial;
  try {
    const { fixture } = initial;
    const wallet = fixture.operatorLucid;
    await ensureSeparateCollateralUtxo(wallet);
    await initial.synchronize();
    const { key, mintTxHash, census } = await mintForcedOrder(initial);
    const admitted = await initial.command(census());
    expect(admitted.forced.map((entry) => entry.key)).toEqual([key]);
    expect(admitted.forced[0]?.transactionHash).toBe(mintTxHash);
    const absentLocalRow = await initial.command(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql`SELECT tx_order_id FROM forced_transaction_utxos WHERE tx_order_id = ${Buffer.from(key, "hex")}`;
      }),
    );
    expect(absentLocalRow).toEqual([]);
    for (let index = 0; index < 3; index++) {
      fixture.emulator.awaitBlock(1);
      vi.setSystemTime(fixture.emulator.now());
      await initial.synchronize();
    }
    expect(
      (await initial.command(census())).forced.map((entry) => entry.key),
    ).toEqual([key]);
    // Isolated test-database upgrade model: the stopped owner has durable
    // journal state, but no complete census frontier. Its next generation must
    // replay activation through the existing follower before granting Ready.
    const restarted = await initial.restartRuntime({
      afterStop: async () => {
        await Effect.runPromise(
          provideDatabaseLayers(
            Effect.gen(function* () {
              const sql = yield* SqlClient.SqlClient;
              yield* sql`DELETE FROM event_history_census_frontier`;
            }),
          ),
        );
      },
    });
    current = restarted;
    const reacquired = await restarted.command(census());
    expect(reacquired.forced.map((entry) => entry.key)).toEqual([key]);
    expect(reacquired.forced[0]?.transactionHash).toBe(mintTxHash);
  } finally {
    await current.close();
    vi.useRealTimers();
  }
});

// Transactions and the fork's address snapshot are actual accepted emulator
// observations. Only the network branch ancestry is the existing controlled
// transport model; production journal undo and authority revocation run.
it("removes an orphaned forced admission from the complete census and revokes its actual source permit after a journal rollback and fork", async () => {
  let source: ReturnType<typeof makeRollbackHistoryTransport> | undefined;
  const lifecycle = await openCensusOwner({
    rollbackHorizon: 16,
    transportFactory: (recorded) => {
      source = makeRollbackHistoryTransport(recorded);
      return source;
    },
  });
  try {
    if (source === undefined) throw new Error("Missing rollback source");
    const { fixture, binding } = lifecycle;
    await ensureSeparateCollateralUtxo(fixture.operatorLucid);
    await lifecycle.synchronize();
    const ancestor = source.points.at(-1)!.point;
    const { key, mintTxHash, census } = await mintForcedOrder(lifecycle);
    const admissionPoint = source.points[source.indexOf(mintTxHash)]!.point;
    const oldPermit = await lifecycle.runWithoutSynchronizing(HistoryProducer);
    expect(
      (await lifecycle.runWithoutSynchronizing(census())).forced.map(
        (entry) => entry.key,
      ),
    ).toEqual([key]);

    fixture.emulator.awaitBlock(1);
    vi.setSystemTime(fixture.emulator.now());
    await lifecycle.observer.flush();
    const addresses = [
      ...new Set([
        binding.hubAddress,
        ...Object.values(binding.deployments).flatMap(
          ({ address, retentionAddress }) => [address, retentionAddress],
        ),
        fixture.contracts.stateQueue.spendingScriptAddress,
      ]),
    ];
    const outputs = (
      await Promise.all(
        addresses.map((address) => fixture.operatorLucid.utxosAt(address)),
      )
    )
      .flat()
      .map(historyOutputObservation);
    source.rollbackTo(ancestor.id);
    const fork = source.appendFork({
      observations: [],
      observedSlot: fixture.emulator.slot,
      observedHeight: fixture.emulator.blockHeight,
      outputs,
    });
    await Effect.runPromise(
      lifecycle.production.owner
        .awaitReadyAt(fork)
        .pipe(Effect.timeout("30 seconds")),
    );
    expect((await lifecycle.runWithoutSynchronizing(census())).forced).toEqual(
      [],
    );
    const stale = await lifecycle.runWithoutSynchronizing(
      Effect.either(
        assertForeignVerificationSource({
          kind: "ready",
          binding: oldPermit,
        }),
      ),
    );
    expect(stale._tag).toBe("Left");
    const orphan = await lifecycle.runWithoutSynchronizing(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        return yield* sql<{
          canonical: boolean;
          admissions_record: string;
        }>`SELECT canonical,admissions_record FROM event_history_census_blocks
          WHERE binding_digest = ${Buffer.from(binding.digest, "hex")} AND block_hash = ${Buffer.from(admissionPoint.id, "hex")}`;
      }),
    );
    expect(orphan).toEqual([
      {
        canonical: false,
        admissions_record: expect.stringContaining(key),
      },
    ]);
  } finally {
    await lifecycle.close();
    vi.useRealTimers();
  }
});
