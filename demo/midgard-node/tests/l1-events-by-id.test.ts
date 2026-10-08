/**
 * NC13: settlement opens one event by its id from the follower's facts.
 * The lookup reads the event's projection row by key, its Order by the
 * token the key names and its retained payload by outref, so the rows it
 * reads do not grow with the list.
 */
import type { FactStore, OutRef, SqlTx } from "@al-ft/midgard-l1-follower";
import { addressText } from "@al-ft/midgard-l1-follower/provider";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  eventOrderByIdIn,
  type EventOrderRead,
  eventProjection,
  eventTrackedSet,
} from "../src/l1-events/index.js";
import {
  admissionTx,
  eventIdOf,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
  retirementTx,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
  DROP_ALL_TIMEOUT_MS,
  storeOpener,
  testDatabases,
} from "./helpers/l1-events-store.js";

const databases = testDatabases();
const opened: FactStore[] = [];
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd1);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

const idCborOf = (nonce: OutRef): Buffer =>
  Buffer.from(Data.to(eventIdOf(nonce), SDK.OutputReference), "hex");

const deploymentOf = (kind: "deposit" | "withdrawal") => {
  const list = listOf(kind);
  return {
    policyId: list.policyId,
    address: addressText(Buffer.from(list.listAddress, "hex")),
    retentionAddress: addressText(Buffer.from(list.retentionAddress, "hex")),
    inlineLimitBytes: 512n,
  };
};

/** The lookup, with every statement it runs and the rows they return. */
const lookup = async (
  store: FactStore,
  kind: "deposit" | "withdrawal",
  nonce: OutRef,
): Promise<{ read: EventOrderRead; rows: number; statements: string[] }> => {
  let rows = 0;
  const statements: string[] = [];
  const read = await store.transaction("read", (tx) => {
    const counted: SqlTx = {
      query: async (text, params) => {
        const result = await tx.query(text, params);
        rows += result.length;
        statements.push(text.replace(/\s+/gu, " "));
        return result;
      },
      exec: (text) => tx.exec(text),
    };
    return eventOrderByIdIn(
      counted,
      store.dialect,
      { kind, policyId: listOf(kind).policyId },
      idCborOf(nonce),
    );
  });
  return { read, rows, statements };
};

const ok = (read: EventOrderRead) => {
  if (read.kind !== "ok") throw new Error(`lookup: ${JSON.stringify(read)}`);
  return read;
};

describe.each(["sqlite", "postgres"] as const)(
  "event lookup by id (%s)",
  (dialect) => {
    const open = storeOpener(dialect, databases);
    const driver = async (): Promise<ChainDriver> => {
      const store = await open([eventProjection(EVENTS_CONFIG)], 4);
      opened.push(store);
      const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
      await d.init();
      return d;
    };

    it("opens a deposit and an external withdrawal by key, reading the same rows however long the list is", async () => {
      const d = await driver();
      const deposit = eventOrder("deposit", nonceRef());
      const withdrawal = eventOrder("withdrawal", nonceRef(), {
        external: true,
      });
      const [retentionHash] = await d.forward([
        { inputs: [nonceRef()], outputs: [withdrawal.retained!], nonce: 1 },
      ]);
      const [depositTx, withdrawalTx] = await d.forward([
        admissionTx(deposit, 2),
        admissionTx(withdrawal, 3, { txHash: retentionHash!, index: 0 }),
      ]);

      const first = await lookup(d.store, "deposit", deposit.nonce);
      const found = ok(first.read);
      expect(found.order.txHash).toBe(depositTx!.toString("hex"));
      expect(found.retained).toEqual([]);
      const utxo = await Effect.runPromise(
        SDK.orderToDepositUTxO(
          found.order,
          found.retained,
          deploymentOf("deposit"),
        ),
      );
      expect(Buffer.from(utxo.idCbor).equals(idCborOf(deposit.nonce))).toBe(
        true,
      );

      const external = ok(
        (await lookup(d.store, "withdrawal", withdrawal.nonce)).read,
      );
      expect(external.order.txHash).toBe(withdrawalTx!.toString("hex"));
      expect(external.retained.map((u) => u.txHash)).toEqual([
        retentionHash!.toString("hex"),
      ]);
      const withdrawalUtxo = await Effect.runPromise(
        SDK.orderToWithdrawalUTxO(
          external.order,
          external.retained,
          deploymentOf("withdrawal"),
        ),
      );
      expect(
        Buffer.from(withdrawalUtxo.idCbor).equals(idCborOf(withdrawal.nonce)),
      ).toBe(true);
      // Without its retained payload the SDK refuses the external Order.
      await expect(
        Effect.runPromise(
          SDK.orderToWithdrawalUTxO(
            external.order,
            [],
            deploymentOf("withdrawal"),
          ),
        ),
      ).rejects.toThrow();

      // Thirty more deposits: the lookup reads exactly the rows it read.
      await d.forward(
        Array.from({ length: 30 }, (_, i) =>
          admissionTx(eventOrder("deposit", nonceRef()), 10 + i),
        ),
      );
      const again = await lookup(d.store, "deposit", deposit.nonce);
      expect(ok(again.read).order.txHash).toBe(depositTx!.toString("hex"));
      expect(again.rows).toBe(first.rows);
      expect(again.statements).toEqual(first.statements);
      const events = again.statements.filter((s) =>
        s.includes("node_l1_events"),
      );
      expect(events).toHaveLength(1);
      expect(events[0]).toMatch(/event_key = \?/u);
      const outputs = again.statements.filter((s) => s.includes("l1_outputs"));
      expect(outputs).toHaveLength(1);
      expect(outputs[0]).toMatch(/a\.asset_name = \?/u);
    });

    it("answers absent for an unknown or retired id and unavailable when the retained payload is gone", async () => {
      const d = await driver();
      const deposit = eventOrder("deposit", nonceRef());
      const withdrawal = eventOrder("withdrawal", nonceRef(), {
        external: true,
      });
      const [retentionHash] = await d.forward([
        { inputs: [nonceRef()], outputs: [withdrawal.retained!], nonce: 1 },
      ]);
      const [depositTx] = await d.forward([
        admissionTx(deposit, 2),
        admissionTx(withdrawal, 3, { txHash: retentionHash!, index: 0 }),
      ]);

      expect((await lookup(d.store, "deposit", nonceRef())).read).toEqual({
        kind: "absent",
      });
      // The id under the other list is absent too: the key is per kind.
      expect((await lookup(d.store, "withdrawal", deposit.nonce)).read).toEqual(
        { kind: "absent" },
      );

      await d.forward([
        retirementTx(deposit, { txHash: depositTx!, index: 0 }, "absorbed", 4),
        {
          inputs: [{ txHash: retentionHash!, index: 0 }],
          outputs: [],
          nonce: 5,
        },
      ]);
      expect((await lookup(d.store, "deposit", deposit.nonce)).read).toEqual({
        kind: "absent",
      });
      expect(
        (await lookup(d.store, "withdrawal", withdrawal.nonce)).read,
      ).toEqual({
        kind: "unavailable",
        detail: "the retained payload output is spent",
      });
    });
  },
);
