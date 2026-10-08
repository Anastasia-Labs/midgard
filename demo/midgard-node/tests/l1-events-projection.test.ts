import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  plutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import {
  dueByCutoff,
  dueByCutoffNow,
  eventListAt,
  eventProjection,
  eventsAt,
  eventTrackedSet,
  type ProjectedEvent,
  type SlotTime,
  slotToPosixMs,
  spendableAt,
} from "@al-ft/midgard-l1-follower/events";
import { createSlotClock } from "@al-ft/midgard-l1-follower/heads";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it, vi } from "vitest";

import {
  depositDataToEntry,
  withdrawalDataToEntry,
} from "../src/l1-event-history-entries.js";
import { userEventEntry } from "../src/l1-events/entries.js";
import {
  admissionTx,
  ELSEWHERE,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
  retirementTx,
  retirementWitness,
  rootOutput,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  vi.useRealTimers();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
});

const DEPOSITS = listOf("deposit");
const WITHDRAWALS = listOf("withdrawal");
const SLOT_TIME: SlotTime = { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 };

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd0);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

const ok = <T>(read: { kind: string; value?: T }): T => {
  if (read.kind !== "ok") throw new Error(`read: ${JSON.stringify(read)}`);
  return read.value as T;
};

const refusals = async (store: FactStore) =>
  store.transaction("read", (tx) =>
    tx.query(
      "SELECT kind, reason, event_key FROM node_l1_event_refusals ORDER BY slot, output_index",
    ),
  );

const keyRows = async (store: FactStore) =>
  store.transaction("read", (tx) =>
    tx.query(
      "SELECT kind, key, first_canonical_slot FROM l1_event_keys ORDER BY first_canonical_slot",
    ),
  );

describe.each(["sqlite", "postgres"] as const)(
  "node event projection (%s)",
  (dialect) => {
    const open = storeOpener(dialect, databases);
    const driver = async (): Promise<ChainDriver> => {
      const store = await open([eventProjection(EVENTS_CONFIG)], 4);
      opened.push(store);
      const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
      await d.init();
      return d;
    };

    it("admits inline and external events, follows continuations and records retirements with the observer's reason", async () => {
      const d = await driver();
      const deposit = eventOrder("deposit", nonceRef());
      const withdrawal = eventOrder("withdrawal", nonceRef(), {
        external: true,
      });
      expect(withdrawal.retained).not.toBeNull();
      const [retentionHash] = await d.forward([
        { inputs: [nonceRef()], outputs: [withdrawal.retained!], nonce: 1 },
      ]);
      const [depositTx, withdrawalTx] = await d.forward([
        admissionTx(deposit, 2),
        admissionTx(withdrawal, 3, { txHash: retentionHash!, index: 0 }),
      ]);
      const admittedAt = d.tip.point;
      const events = ok(await eventsAt(d.store, DEPOSITS, admittedAt));
      expect(events).toHaveLength(1);
      expect(events[0]).toMatchObject({
        kind: "deposit",
        key: deposit.key,
        retirement: null,
        admission: { txIndex: 0, slot: admittedAt.slot },
      });
      expect(events[0]!.location.txHash.equals(depositTx!)).toBe(true);
      expect(
        Data.from(events[0]!.idCbor, SDK.OutputReference).outputIndex,
      ).toBe(BigInt(deposit.nonce.index));
      const external = ok(await eventsAt(d.store, WITHDRAWALS, admittedAt));
      expect(external.map((e) => e.key)).toEqual([withdrawal.key]);
      expect(external[0]!.admission.txHash).toBe(withdrawalTx!.toString("hex"));
      expect(
        "WithdrawalPayload" in
          Data.from(external[0]!.payloadCbor, SDK.EventHistoryPayload),
      ).toBe(true);

      // A continuation moves the Order; it admits nothing new.
      const [moved] = await d.forward([
        {
          inputs: [{ txHash: depositTx!, index: 0 }],
          outputs: [deposit.order],
          nonce: 4,
        },
      ]);
      const afterMove = ok(await eventsAt(d.store, DEPOSITS, d.tip.point));
      expect(afterMove).toHaveLength(1);
      expect(afterMove[0]!.location.txHash.equals(moved!)).toBe(true);
      expect(afterMove[0]!.admission.txHash).toBe(depositTx!.toString("hex"));

      // Retirement: the burn under the observer's zero withdrawal.
      const [retiredBy] = await d.forward([
        retirementTx(deposit, { txHash: moved!, index: 0 }, "absorbed", 5),
      ]);
      const retired = ok(await eventsAt(d.store, DEPOSITS, d.tip.point))[0]!;
      expect(retired.retirement).toMatchObject({
        reason: "absorbed",
        // spend redeemer first, then the reward redeemer (ledger order).
        observerRedeemerIndex: 1,
        txHash: retiredBy!.toString("hex"),
        witnessCbor: Data.to(
          retirementWitness("absorbed"),
          SDK.EventHistoryRetirementWitness,
        ),
      });
      expect(retired.location.txHash.equals(moved!)).toBe(true);
      // As of the admission point it is still live.
      expect(
        ok(await eventsAt(d.store, DEPOSITS, admittedAt))[0]!.retirement,
      ).toBeNull();
      expect(await refusals(d.store)).toEqual([]);
    });

    it("refuses a retired key, admits the same key under the other kind, and refuses malformed list outputs", async () => {
      const d = await driver();
      const nonce = nonceRef();
      const first = eventOrder("deposit", nonce);
      const [admitted] = await d.forward([admissionTx(first, 10)]);
      await d.forward([
        retirementTx(first, { txHash: admitted!, index: 0 }, "absorbed", 11),
      ]);
      // The same id (so the same key) resubmitted: refused, never reused.
      await d.forward([admissionTx(eventOrder("deposit", nonce), 12)]);
      const deposits = ok(await eventsAt(d.store, DEPOSITS, d.tip.point));
      expect(deposits).toHaveLength(1);
      expect(deposits[0]!.retirement).not.toBeNull();
      // The same key on the withdrawal list is another key-set entry.
      await d.forward([admissionTx(eventOrder("withdrawal", nonce), 13)]);
      expect(
        ok(await eventsAt(d.store, WITHDRAWALS, d.tip.point)).map((e) => e.key),
      ).toEqual([first.key]);
      // A list token on an output with an unreadable datum, and on one without a datum.
      const junk = eventOrder("deposit", nonceRef());
      const bare = eventOrder("deposit", nonceRef());
      await d.forward([
        {
          ...admissionTx(junk, 14),
          outputs: [{ ...junk.order, datum: Buffer.from("d87980", "hex") }],
        },
        {
          ...admissionTx(bare, 15),
          outputs: [{ ...bare.order, datum: undefined }],
        },
      ]);
      const rows = await refusals(d.store);
      expect(
        rows.map((r) => [
          r.kind,
          r.reason,
          Buffer.from(r.event_key as Uint8Array).toString("hex"),
        ]),
      ).toEqual([
        ["deposit", "retired_key", first.key],
        ["deposit", "malformed", junk.key],
        ["deposit", "malformed", bare.key],
      ]);
      expect(
        ok(await eventsAt(d.store, DEPOSITS, d.tip.point)).map((e) => e.key),
      ).toEqual([first.key]);
      expect((await keyRows(d.store)).map((r) => r.kind)).toEqual([
        "deposit",
        "withdrawal",
      ]);
    });

    it("readmits an id whose origin a rollback orphaned, at its new placement", async () => {
      const d = await driver();
      const order = eventOrder("deposit", nonceRef());
      await d.forward([]);
      await d.forward([admissionTx(order, 20)]);
      const orphanedSlot = d.tip.point.slot;
      await d.backward(1);
      expect(ok(await eventsAt(d.store, DEPOSITS, d.tip.point))).toEqual([]);
      expect(await keyRows(d.store)).toEqual([]);
      await d.forward([]);
      await d.forward([
        {
          inputs: [nonceRef()],
          outputs: [{ address: ELSEWHERE, lovelace: 1n }],
          nonce: 21,
        },
        admissionTx(order, 22),
      ]);
      const [event] = ok(await eventsAt(d.store, DEPOSITS, d.tip.point));
      expect(event).toMatchObject({
        key: order.key,
        admission: { slot: d.tip.point.slot, txIndex: 1 },
      });
      expect(event!.admission.slot).not.toBe(orphanedSlot);
      const keys = await keyRows(d.store);
      expect(keys).toHaveLength(1);
      expect(Number(keys[0]!.first_canonical_slot)).toBe(d.tip.point.slot);
      expect(await refusals(d.store)).toEqual([]);
    });

    it("decides deposit due-ness at slotNow, so a fast wall clock makes nothing due early", async () => {
      const d = await driver();
      await d.forward([]);
      const tipMs = slotToPosixMs(SLOT_TIME, d.tip.point.slot + 1);
      const due = eventOrder("deposit", nonceRef(), {
        inclusionTime: BigInt(tipMs - 5_000),
      });
      const later = eventOrder("deposit", nonceRef(), {
        inclusionTime: BigInt(tipMs + 600_000),
      });
      await d.forward([admissionTx(due, 30), admissionTx(later, 31)]);
      const at = d.tip.point;
      expect(
        ok(
          await dueByCutoff(d.store, DEPOSITS, at, {
            slot: at.slot,
            slotTime: SLOT_TIME,
          }),
        ).map((s) => s.key),
      ).toEqual([due.key]);
      // A wall clock 10 minutes fast: slotNow follows the tip, not the wall clock.
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(Date.now() + 20 * 60_000);
      const clock = createSlotClock({
        slotLengthMs: SLOT_TIME.slotLength,
        monotonicNowMs: () => 0,
      });
      expect(
        await dueByCutoffNow(d.store, DEPOSITS, at, clock, SLOT_TIME),
      ).toEqual({ kind: "no_slot_now" });
      clock.observeTipSlot(at.slot);
      const now = await dueByCutoffNow(d.store, DEPOSITS, at, clock, SLOT_TIME);
      expect(now.kind === "ok" && now.value.map((s) => s.key)).toEqual([
        due.key,
      ]);
      // The later deposit becomes due once the chain itself reaches it.
      expect(
        ok(
          await dueByCutoff(d.store, DEPOSITS, at, {
            slot: at.slot + 700,
            slotTime: SLOT_TIME,
          }),
        ).map((s) => s.key),
      ).toEqual([due.key, later.key]);
    });

    it("makes a deposit spendable only by own-block inclusion, never by a clock ahead of the tip (P4)", async () => {
      const d = await driver();
      await d.forward([]);
      const tipMs = slotToPosixMs(SLOT_TIME, d.tip.point.slot + 1);
      const long = eventOrder("deposit", nonceRef(), {
        inclusionTime: BigInt(tipMs - 5_000),
      });
      const fresh = eventOrder("deposit", nonceRef(), {
        inclusionTime: BigInt(tipMs + 600_000),
      });
      await d.forward([admissionTx(long, 30), admissionTx(fresh, 31)]);
      const at = d.tip.point;
      const ids = ok(await eventsAt(d.store, DEPOSITS, at)).map((e) => ({
        key: e.key,
        idCbor: e.idCbor,
      }));
      const longId = ids.find((e) => e.key === long.key)!.idCbor;
      const freshId = ids.find((e) => e.key === fresh.key)!.idCbor;
      // A wall clock a day ahead: nothing is included, so nothing is spendable.
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(Date.now() + 24 * 3_600_000);
      expect(ok(await spendableAt(d.store, DEPOSITS, at, new Set()))).toEqual(
        [],
      );
      // Inclusion, not due-ness, decides: the not-yet-due deposit is spendable
      // once an own block includes it; the due one is not until included.
      expect(
        ok(await spendableAt(d.store, DEPOSITS, at, new Set([freshId]))).map(
          (s) => s.key,
        ),
      ).toEqual([fresh.key]);
      expect(
        ok(await spendableAt(d.store, DEPOSITS, at, new Set([longId, freshId])))
          .map((s) => s.key)
          .sort(),
      ).toEqual([long.key, fresh.key].sort());
    });

    it("walks the list from its root and reports a broken walk as unhealthy", async () => {
      const d = await driver();
      const a = eventOrder("deposit", nonceRef());
      const b = eventOrder("deposit", nonceRef());
      const [low, high] = a.key < b.key ? [a, b] : [b, a];
      const lowNext = eventOrder("deposit", low.nonce, { next: high.key });
      await d.forward([
        {
          inputs: [nonceRef()],
          outputs: [rootOutput("deposit", low.key)],
          nonce: 40,
        },
        admissionTx(lowNext, 41),
        admissionTx(high, 42),
      ]);
      expect(ok(await eventListAt(d.store, DEPOSITS, d.tip.point))).toEqual({
        keys: [low.key, high.key],
        orders: 2,
        fillers: 0,
      });
      expect(await eventListAt(d.store, WITHDRAWALS, d.tip.point)).toEqual({
        kind: "unhealthy",
        detail: "no root",
      });
      await d.forward([
        {
          inputs: [nonceRef()],
          outputs: [rootOutput("withdrawal", "77".repeat(32))],
          nonce: 43,
        },
      ]);
      expect(await eventListAt(d.store, WITHDRAWALS, d.tip.point)).toEqual({
        kind: "unhealthy",
        detail: `missing node ${"77".repeat(32)}`,
      });
    });

    it("produces the same ingestion rows as today's entry conversion", async () => {
      const d = await driver();
      const deposit = eventOrder("deposit", nonceRef());
      const withdrawal = eventOrder("withdrawal", nonceRef());
      await d.forward([admissionTx(deposit, 50), admissionTx(withdrawal, 51)]);
      const read = async (list: typeof DEPOSITS): Promise<ProjectedEvent> =>
        ok(await eventsAt(d.store, list, d.tip.point))[0]!;
      const projectedDeposit = await read(DEPOSITS);
      const projectedWithdrawal = await read(WITHDRAWALS);
      const base = (event: ProjectedEvent) => ({
        idCbor: Buffer.from(event.idCbor, "hex"),
        infoCbor: Buffer.from(
          aikenSerialisedPlutusDataCborPreservingMapOrder(
            plutusConstrFieldCbor(event.payloadCbor, [0, 1]),
          ),
          "hex",
        ),
        inclusionTime: new Date(Number(event.inclusionTime)),
        location: {
          txHash: event.location.txHash.toString("hex"),
          outputIndex: event.location.index,
        },
      });
      const oldDeposit = await Effect.runPromise(
        depositDataToEntry(
          {
            ...base(projectedDeposit),
            originalAssets: SDK.valueToAssets(
              Data.from(projectedDeposit.originalAssetsCbor, SDK.Value),
            ),
          },
          "Preprod",
        ),
      );
      const newDeposit = userEventEntry(projectedDeposit, "Preprod");
      if (newDeposit.kind !== "deposit") throw new Error("expected a deposit");
      expect(newDeposit.entry).toEqual({
        idCbor: oldDeposit.event_id.toString("hex"),
        infoCbor: oldDeposit.event_info.toString("hex"),
        inclusionTimeMs: oldDeposit.inclusion_time.getTime(),
        l1TxHash: oldDeposit.deposit_l1_tx_hash.toString("hex"),
        ledgerTxId: Buffer.from(oldDeposit.ledger_tx_id).toString("hex"),
        ledgerOutput: Buffer.from(oldDeposit.ledger_output).toString("hex"),
        ledgerAddress: oldDeposit.ledger_address,
      });
      const oldWithdrawal = await Effect.runPromise(
        withdrawalDataToEntry({
          ...base(projectedWithdrawal),
          assetName: projectedWithdrawal.key,
          payloadCbor: projectedWithdrawal.payloadCbor,
        }),
      );
      const newWithdrawal = userEventEntry(projectedWithdrawal, "Preprod");
      if (newWithdrawal.kind !== "withdrawal")
        throw new Error("expected a withdrawal");
      expect(newWithdrawal.entry).toEqual({
        idCbor: oldWithdrawal.event_id.toString("hex"),
        rawEventInfo: oldWithdrawal.raw_event_info.toString("hex"),
        inclusionTimeMs: oldWithdrawal.inclusion_time.getTime(),
        l1TxHash: oldWithdrawal.withdrawal_l1_tx_hash.toString("hex"),
        l1OutputIndex: oldWithdrawal.withdrawal_l1_output_index,
        assetName: oldWithdrawal.asset_name.toString("hex"),
        l2Outref: oldWithdrawal.l2_outref.toString("hex"),
        l2Owner: oldWithdrawal.l2_owner.toString("hex"),
        l2Value: oldWithdrawal.l2_value.toString("hex"),
        l1Address: oldWithdrawal.l1_address.toString("hex"),
        l1Datum: oldWithdrawal.l1_datum.toString("hex"),
        refundAddress: oldWithdrawal.refund_address.toString("hex"),
        refundDatum: oldWithdrawal.refund_datum.toString("hex"),
      });
    });
  },
);
