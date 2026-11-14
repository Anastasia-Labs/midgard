/**
 * The follower opens a list Order only when it holds exactly one token of
 * its list policy, of quantity one, named by the hash of its event id, and
 * its datum is keyed by that hash: the off-chain twin of the list
 * validator's checks. Every other shape is refused by name in
 * `node_l1_event_refusals`, once per output, and the block still applies.
 */
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import {
  eventProjection,
  eventsAt,
  eventTrackedSet,
} from "@al-ft/midgard-l1-follower/events";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
} from "./helpers/l1-events-chain.js";
import {
  ChainDriver,
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
});

const DEPOSITS = listOf("deposit");

let nonces = 0;
const nonceRef = (): OutRef => {
  nonces += 1;
  const txHash = Buffer.alloc(32, 0xd4);
  txHash.writeUInt32BE(nonces, 28);
  return { txHash, index: nonces % 3 };
};

const ok = <T>(read: { kind: string; value?: T }): T => {
  if (read.kind !== "ok") throw new Error(`read: ${JSON.stringify(read)}`);
  return read.value as T;
};

const keyRows = async (store: FactStore) =>
  store.transaction("read", (tx) =>
    tx.query("SELECT kind FROM l1_event_keys ORDER BY first_canonical_slot"),
  );

describe.each(["sqlite", "postgres"] as const)(
  "the follower's list Order shape (%s)",
  (dialect) => {
    const open = storeOpener(dialect, databases);
    const driver = async (): Promise<ChainDriver> => {
      const store = await open([eventProjection(EVENTS_CONFIG)], 4);
      opened.push(store);
      const d = new ChainDriver(store, eventTrackedSet(EVENTS_CONFIG));
      await d.init();
      return d;
    };

    it("admits an Order holding exactly one key token keyed by its event id, and refuses each other shape by name", async () => {
      const d = await driver();
      const policy = DEPOSITS.policyId;
      const honest = eventOrder("deposit", nonceRef());
      const doubled = eventOrder("deposit", nonceRef());
      // A second list key token on the Order: refused once for the output,
      // and the block still applies.
      const extra = eventOrder("deposit", nonceRef());
      // An Order holding its id's key token, its datum keyed by another key.
      const rekeyed = eventOrder("deposit", nonceRef());
      const node = Data.from(
        rekeyed.order.datum!.toString("hex"),
        SDK.EventHistoryNode,
      );
      const otherKey = eventOrder("deposit", nonceRef()).key;
      await d.forward([
        admissionTx(honest, 30),
        {
          ...admissionTx(doubled, 31),
          outputs: [
            {
              ...doubled.order,
              assets: new Map([[policy, new Map([[doubled.key, 2n]])]]),
            },
          ],
        },
        {
          ...admissionTx(extra, 32),
          outputs: [
            {
              ...extra.order,
              assets: new Map([
                [
                  policy,
                  new Map([
                    [extra.key, 1n],
                    ["ff".repeat(32), 1n],
                  ]),
                ],
              ]),
            },
          ],
        },
        {
          ...admissionTx(rekeyed, 33),
          outputs: [
            {
              ...rekeyed.order,
              datum: Buffer.from(
                Data.to(
                  { ...node, position: { Key: [otherKey] } },
                  SDK.EventHistoryNode,
                ),
                "hex",
              ),
            },
          ],
        },
      ]);
      expect(
        ok(await eventsAt(d.store, DEPOSITS, d.tip.point)).map((e) => e.key),
      ).toEqual([honest.key]);
      const rows = await d.store.transaction("read", (tx) =>
        tx.query(
          "SELECT reason, event_key, detail FROM node_l1_event_refusals ORDER BY slot, tx_hash",
        ),
      );
      const named = (key: string) => {
        const found = rows.filter(
          (row) =>
            Buffer.from(row.event_key as Uint8Array).toString("hex") === key,
        );
        return found.map((row) => [row.reason, String(row.detail)]);
      };
      expect(named(doubled.key)).toEqual([
        [
          "malformed",
          "Error: History Order does not hold exactly one of its key token",
        ],
      ]);
      expect(named(extra.key)).toEqual([
        [
          "malformed",
          "Error: History Order holds another token of its list policy",
        ],
      ]);
      expect(named(rekeyed.key)).toEqual([
        [
          "malformed",
          "Error: History Order key is not the hash of its event id",
        ],
      ]);
      expect((await keyRows(d.store)).map((r) => r.kind)).toEqual(["deposit"]);
    });
  },
);
