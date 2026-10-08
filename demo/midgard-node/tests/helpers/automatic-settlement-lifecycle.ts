import { randomUUID } from "node:crypto";

import {
  CML,
  coreToTxOutput,
  datumToHash,
  generateSeedPhrase,
  Lucid as makeLucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, vi } from "vitest";

import * as Journal from "../../src/database/settlement.js";
import { Lucid } from "../../src/services/lucid.js";
import {
  type SettlementHealth,
  settlementTick,
} from "../../src/services/settlement.js";
import * as publicationProvider from "../../src/transactions/reference-publication-provider.js";
import type { openHistoryProductionOwnerLifecycle } from "./history-production-owner-lifecycle.js";
import { withoutFollowerJournal } from "./intent-journal.js";

/** Real builders, signatures, script execution, SQL queue and reconciliation.
 * As in the owner fixture, only network point labels/transport are synthetic. */
export const openAutomaticSettlement = async (
  h: Awaited<ReturnType<typeof openHistoryProductionOwnerLifecycle>>,
) => {
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
  const status = vi
    .spyOn(api, "transactionStatus")
    .mockImplementation(async (hash) => {
      const observed = await actualStatus(hash);
      if (observed.status !== "confirmed") return observed;
      const block = (await h.evidence()).points.find((p) =>
        p.transactions.some((tx) => tx.id === hash),
      );
      if (block === undefined)
        throw new Error(`Missing recorded settlement block ${hash}`);
      return {
        ...observed,
        confirmation: { ...observed.confirmation, blockHash: block.point.id },
      };
    });
  const barrier = vi
    .spyOn(publicationProvider, "synchronizePublicationIndexerPoint")
    .mockImplementation(async () => {
      const points = (await h.evidence()).points;
      return points[points.length - 1]!.point;
    });
  const fetchBefore = globalThis.fetch;
  const fetchSpy = vi
    .spyOn(globalThis, "fetch")
    .mockImplementation(async (input, init) => {
      const match = /\/matches\/\*@([a-f0-9]{64})$/u.exec(String(input));
      if (match === null) return fetchBefore(input, init);
      const receipt = h.receipts.find((r) => r.transaction.txHash === match[1]);
      const block = (await h.evidence()).points.find((p) =>
        p.transactions.some((tx) => tx.id === match[1]),
      );
      if (receipt === undefined || block === undefined)
        return Response.json([]);
      const outputs = CML.Transaction.from_cbor_hex(receipt.signedCbor)
        .body()
        .outputs();
      return Response.json(
        Array.from({ length: outputs.len() }, (_, index) => {
          const output = coreToTxOutput(outputs.get(index));
          return {
            transaction_id: match[1],
            output_index: index,
            address: output.address,
            value: {
              coins: String(output.assets.lovelace),
              assets: Object.fromEntries(
                Object.entries(output.assets)
                  .filter(([unit]) => unit !== "lovelace")
                  .map(([unit, amount]) => [unit, String(amount)]),
              ),
            },
            datum_hash:
              output.datumHash ??
              (output.datum == null ? null : datumToHash(output.datum)),
            script_hash: null,
            created_at: {
              slot_no: block.point.slot,
              header_hash: block.point.id,
            },
          };
        }),
      );
    });
  const health: SettlementHealth[] = [];
  const tick = settlementTick(owner, api, (value) => health.push(value)).pipe(
    Effect.provideService(Lucid, service),
  );
  const runPhase = async (eventId: string, phase: Journal.SettlementPhase) => {
    for (let i = 0; i < 40; i++) {
      await h.command(Journal.renew(owner));
      await h.runWithoutSynchronizing(withoutFollowerJournal(tick));
      const pending = await h.runWithoutSynchronizing(
        Journal.pending(owner.deploymentId),
      );
      if (
        pending !== undefined &&
        (await actualStatus(pending.tx_hash)).status === "pending"
      ) {
        await api.awaitTx(pending.tx_hash);
        await h.synchronize();
      }
      const receipts = await h.command(
        Journal.attempts({
          deployment_id: owner.deploymentId,
          kind: phase === "absorb" ? "deposit" : "withdrawal",
          event_id: eventId,
          phase,
          failures: 0,
          verified_generation: "0",
        }),
      );
      const receipt = receipts.find((r) => r.phase === phase);
      if (receipt !== undefined) {
        expect(health.some((value) => value.state === "error")).toBe(false);
        return receipt;
      }
      h.fixture.emulator.awaitSlot(1);
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
      status.mockRestore();
      barrier.mockRestore();
      fetchSpy.mockRestore();
    },
  };
};
