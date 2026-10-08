import { randomUUID } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { generateSeedPhrase, Lucid as makeLucid } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as Journal from "../../src/database/settlement.js";
import * as L1View from "../../src/l1-provider-view.js";
import * as IntentJournal from "../../src/services/intent-journal.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  type SettlementHealth,
  settlementLevel,
  settlementTick,
} from "../../src/services/settlement.js";
import { settlementDepthParameters } from "../../src/services/settlement.status.js";
import { followerBlockHash } from "./follower-view.js";
import { withoutFollowerJournal } from "./intent-journal.js";
import type { ProductionLifecycle } from "./production-lifecycle.js";

/** Real builders, signatures, script execution, SQL queue and reconciliation.
 * Only the indexer point's hash is synthetic (the follower stand-in's block
 * hash at the emulator tip), and the S6 send of a pending journaled body is
 * played by `runPhase`. No intent reconciler runs, so the intent journal's
 * derived status of an attempt is the emulator's: landed in the block that
 * holds it, at its depth below the emulator tip, else live. */
export const openAutomaticSettlement = async (h: ProductionLifecycle) => {
  const operator = h.fixture.operatorLucid;
  const api = await makeLucid(h.fixture.emulator, operator.config().network!, {
    slotConfig: operator.config().slotConfig!,
  });
  api.selectWallet.fromSeed(generateSeedPhrase());
  const walletAddress = await api.wallet().address();
  operator.clearUTxOOverride();
  const funding = await operator
    .newTx()
    .pay.ToAddress(walletAddress, { lovelace: 500_000_000n })
    .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
    .complete();
  const signed = await funding.sign.withWallet().complete();
  await operator.awaitTx(await signed.submit());
  operator.clearUTxOOverride();
  await h.synchronize();
  const owner: Journal.SettlementOwner = {
    deploymentId: h.binding.manifestId,
    walletAddress,
    token: randomUUID(),
  };
  const service = new Lucid({
    ...h.lucidService,
    operatorMainAddress: await operator.wallet().address(),
    operatorMergeAddress: await operator.wallet().address(),
    referenceScriptsWalletAddress: await h.fixture.referenceScriptsLucid
      .wallet()
      .address(),
    api,
    switchToOperatorsMainWallet: Effect.void,
  });
  const actualStatus = api.transactionStatus.bind(api);
  const barrier = vi.spyOn(L1View, "providerViewPoint").mockImplementation(() =>
    Effect.sync(() => {
      const slot = h.fixture.emulator.slot;
      return { slot, id: followerBlockHash(slot).toString("hex") };
    }),
  );
  const journalStatus = vi
    .spyOn(IntentJournal, "readIntentStatus")
    .mockImplementation((hash) =>
      Effect.promise(async () => {
        const observed = await actualStatus(hash);
        if (observed.status !== "confirmed")
          return { kind: "live" as const, inputsAvailable: true };
        const { slot, blockHeight, confirmations } = observed.confirmation;
        if (
          slot === undefined ||
          blockHeight === undefined ||
          confirmations === undefined
        )
          throw new Error(`Emulator confirmation of ${hash} is incomplete`);
        return {
          kind: "landed" as const,
          slot,
          height: blockHeight,
          depth: confirmations,
        };
      }),
    );
  const depths = await h.command(settlementDepthParameters);
  const health: SettlementHealth[] = [];
  const tick = settlementTick(owner, api, (value) => health.push(value)).pipe(
    Effect.provideService(Lucid, service),
  );
  const runPhase = async (eventId: string, phase: Journal.SettlementPhase) => {
    for (let i = 0; i < 40; i++) {
      await h.command(Journal.renew(owner));
      await h.runWithoutSynchronizing(withoutFollowerJournal(tick));
      const pending = (
        await h.runWithoutSynchronizing(
          Journal.openAttempts(owner.deploymentId),
        )
      )
        // Settled attempts stay pending until k deep, so pick this phase's.
        .filter(
          (attempt) =>
            attempt.status === "pending" &&
            attempt.event_id === eventId &&
            attempt.phase === phase,
        )
        .at(-1);
      // No follower runs here, so this stands in for S6, the journaled
      // bytes' one sender: it sends the pending body, exactly, while the
      // chain does not know it.
      if (
        pending !== undefined &&
        (await actualStatus(pending.tx_hash)).status === "not_found"
      )
        await api.config().provider!.submitTx(pending.signed_cbor);
      if (
        pending !== undefined &&
        (await actualStatus(pending.tx_hash)).status === "pending"
      ) {
        await api.awaitTx(pending.tx_hash);
        await h.synchronize();
      }
      // A receipt: the phase's attempt the tick read cd deep (its job
      // took the next phase from it), or one stored final.
      const receipt = await h.command(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const rows = yield* sql<Journal.SettlementAttempt>`SELECT a.*
          FROM settlement_attempts a JOIN settlement_jobs j USING (deployment_id, kind, event_id)
          WHERE a.deployment_id = ${owner.deploymentId} AND a.event_id = ${eventId}
            AND a.phase = ${phase} AND a.status <> 'expired'
          ORDER BY a.created_at DESC LIMIT 1`;
          const attempt = rows[0];
          if (attempt === undefined) return undefined;
          const status = yield* IntentJournal.readIntentStatus(attempt.tx_hash);
          return attempt.status === "final" ||
            settlementLevel(status, depths) !== "open"
            ? attempt
            : undefined;
        }),
      );
      if (receipt !== undefined) {
        expect(health.some((value) => value.state === "error")).toBe(false);
        return receipt;
      }
      // One L1 block per round, so a landed attempt deepens every round.
      h.fixture.emulator.awaitBlock(1);
      vi.setSystemTime(h.fixture.emulator.now());
      await h.synchronize();
    }
    throw new Error(
      `Automatic ${phase} did not confirm: ${JSON.stringify(health.slice(-5))}`,
    );
  };
  return {
    runPhase,
    close: () => {
      barrier.mockRestore();
      journalStatus.mockRestore();
    },
  };
};
