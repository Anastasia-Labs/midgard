import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  type LucidEvolution,
  paymentCredentialOf,
} from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it, vi } from "vitest";

import { availabilityResponderOperations } from "../src/availability/factory.js";
import {
  AvailabilityResponder,
  type AvailabilityResponderReport,
  availabilityResponderReportLine,
} from "../src/availability/responder.js";
import { createAvailabilityResponseLoop } from "../src/availability-response-loop.js";

/**
 * A held intent stops the responder signing, so it must never read as an
 * ordinary pending wait: the committee reports it with its reason and fails
 * readiness until a reconciliation without the hold clears it.
 */

const dirs: string[] = [];
afterEach(() =>
  dirs
    .splice(0)
    .forEach((dir) => rmSync(dir, { recursive: true, force: true })),
);

/** A confirmed Settle whose input the source says our own bytes spent. */
const confirmedSettle = async () => {
  const account = generateEmulatorAccount({ lovelace: 500_000_000n });
  const emulator = new Emulator([account]);
  emulator.awaitBlock(5);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const address = await lucid.wallet().address();
  const actor = paymentCredentialOf(address).hash;
  const split = await (
    await lucid
      .newTx()
      .pay.ToAddress(address, { lovelace: 20_000_000n })
      .pay.ToAddress(address, { lovelace: 20_000_000n })
      .complete()
  ).sign
    .withWallet()
    .complete();
  await split.submit();
  emulator.awaitBlock();
  const [normal, collateral] = (await lucid.wallet().getUtxos()).filter(
    (utxo) =>
      utxo.txHash === split.toHash() && utxo.assets.lovelace === 20_000_000n,
  );
  const built = (
    await lucid
      .newTx()
      .collectFrom([normal!])
      .pay.ToAddress(address, { lovelace: 10_000_000n })
      .validFrom(emulator.now() - 60_000)
      .validTo(emulator.now() + 60_000)
      .complete({ coinSelection: false })
  ).toTransaction();
  const body = built.body();
  const collateralInputs = CML.TransactionInputList.new();
  collateralInputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(collateral!.txHash),
      BigInt(collateral!.outputIndex),
    ),
  );
  body.set_collateral_inputs(collateralInputs);
  const signedCbor = (
    await lucid
      .fromTx(
        CML.Transaction.new(
          body,
          built.witness_set(),
          true,
          built.auxiliary_data(),
        ).to_cbor_hex(),
      )
      .sign.withWallet()
      .complete()
  ).toCBOR();
  const ours = SDK.inspectDaAvailabilitySignedIntent({
    deploymentIdentity: "aa".repeat(32),
    actor,
    headerHash: "bb".repeat(28),
    action: "settle",
    signedCbor,
  });
  const dir = mkdtempSync(join(tmpdir(), "committee-held-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(join(dir, "journal.sqlite"));
  const lease = journal.acquire(actor, "setup", Date.now(), 60_000);
  journal.persist(lease, ours, Date.now());
  journal.transition(lease, ours.id, "confirmed", "5:aa", null, Date.now());
  journal.release(lease);
  const point = {
    network: "Custom" as const,
    slot: ours.validUntilSlot - 1,
    blockHash: "ab".repeat(32),
    providerSource: "test",
    observedAt: "test",
  };
  const spendPoint = { slot: 50, blockHash: "cd".repeat(32) };
  const operations = availabilityResponderOperations({
    // Not found, its input gone: missing-input evidence.
    lucid: {
      transactionStatus: async (txHash: string) => ({
        txHash,
        status: "not_found",
      }),
      utxosByOutRef: async () => [],
    } as unknown as LucidEvolution,
    readers: {
      currentPoint: async () => point,
      currentCursor: async () => ({
        sequence: 1,
        rollbackGeneration: 0,
        point,
      }),
      tipBlockNo: async () => 1_000,
      resolveInclusion: async () => ({}),
      foreignSpend: {
        fetchSpend: async () => ({
          transactionId: ours.txHash,
          point: spendPoint,
        }),
        fetchAncestor: async (slot) => ({
          slot: slot - 1,
          blockHash: "00".repeat(32),
        }),
        readTransaction: async ({ txHash }) => ({
          txHash,
          point: { ...spendPoint, blockNo: 990 },
          cbor: signedCbor,
        }),
      },
    },
    assertSourceHealthy: async () => {},
    context: {
      deploymentIdentity: "aa".repeat(32),
      actor,
      journal,
      stateQueuePolicyId: "cc".repeat(28),
      minimumConfirmationDepth: 10,
      transactionLimits: {
        maxTxSize: 16_384,
        maxTxExMem: 16_500_000n,
        maxTxExSteps: 10_000_000_000n,
        coinsPerUtxoByte: 4_310n,
        feeCeilings: {},
      },
      submit: async () => {
        throw new Error("a held intent is never resubmitted");
      },
    },
  });
  return { ours, journal, operations };
};

const HELD_DETAIL =
  "Missing-input evidence for a confirmed availability transaction is not canonical";

describe("a held availability intent on the committee", () => {
  it("is reported as held with its reason, never as pending, and no new step is discovered", async () => {
    const { ours, journal, operations } = await confirmedSettle();
    try {
      await expect(operations.reconcile()).resolves.toEqual({
        held: `${ours.txHash}: ${HELD_DETAIL}`,
      });
      const discover = vi.fn(async () => []);
      const responder = new AvailabilityResponder({
        deploymentFingerprint: "ff".repeat(32),
        deploymentIdentity: "ee".repeat(28),
        store: { getDaPayload: async () => undefined } as never,
        discover,
        reconcile: operations.reconcile,
        execute: async () => "pending",
      });
      const report = await responder.drain();
      expect(report).toEqual({
        challenges: 0,
        status: "held",
        detail: `${ours.txHash}: ${HELD_DETAIL}`,
      });
      expect(discover).not.toHaveBeenCalled();
      expect(availabilityResponderReportLine(report)?.stream).toBe("stderr");
      // The hold changes nothing: the intent and its reservations stay.
      expect(journal.get(ours.id)?.state).toBe("confirmed");
    } finally {
      journal.close();
    }
  });

  it("reports a conflicting intent as held instead of throwing", async () => {
    const responder = new AvailabilityResponder({
      deploymentFingerprint: "ff".repeat(32),
      deploymentIdentity: "ee".repeat(28),
      store: { getDaPayload: async () => undefined } as never,
      discover: async () => [],
      reconcile: async () => ({ held: "conflict detail" }),
      execute: async () => "pending",
    });
    await expect(responder.tick()).resolves.toEqual({
      challenges: 0,
      status: "held",
      detail: "conflict detail",
    });
  });

  it("fails committee readiness with the reason until a reconciliation without the hold", async () => {
    const reports: AvailabilityResponderReport[] = [
      { challenges: 0, status: "held", detail: "tx: held reason" },
      // A drain that never reconciled proves nothing either way.
      {
        challenges: 0,
        status: "awaiting_scan",
        detail: "awaiting the next scan",
      },
      { challenges: 0, status: "pending" },
    ];
    const lines: [string, string][] = [];
    const loop = createAvailabilityResponseLoop({
      drain: async () => reports.shift()!,
      write: (stream, line) => lines.push([stream, line]),
      pollIntervalMs: 1_000,
    });
    expect(loop.reasons()).toEqual([]);
    await loop.run();
    expect(loop.reasons()).toEqual([
      "availability_operation_held:tx: held reason",
    ]);
    expect(lines[0]?.[0]).toBe("stderr");
    await loop.run();
    expect(loop.reasons()).toEqual([
      "availability_operation_held:tx: held reason",
    ]);
    await loop.run();
    expect(loop.reasons()).toEqual([]);
  });
});
