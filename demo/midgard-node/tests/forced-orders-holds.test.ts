/**
 * The forced-order hook's named holds (N10b), on a simulated chain with the
 * follower store in the node database as in production. The three ruled
 * admission stops hold `forced_order_admission_stopped` by name; an order
 * whose transaction does not decode still holds
 * `forced_order_ingestion_failed`. Both hold the commit horizon.
 */
import {
  encodeCbor,
  encodeMidgardFieldPreimage,
  encodeMidgardForcedTxCanonical,
  encodeMidgardTxOutput,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardForcedTxFromCanonical,
} from "@al-ft/midgard-core/codec";
import { validateMidgardConsensusForcedTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { describe, expect, it } from "vitest";

import {
  FORCED_ORDER_ADMISSION_STOPPED,
  FORCED_ORDER_INGESTION_FAILED,
} from "../src/forced-orders/index.js";
import {
  canonicalTransaction,
  TEST_ADDRESS,
} from "./forced-transactions.make-signed-effectful-transaction.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import {
  honest,
  horizon,
  INCLUSION,
  inlineOrder,
  label,
  nodeFollowerLifecycle,
  rows,
} from "./helpers/forced-orders-node-chain.js";
import {
  ingestionHook,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";

const follow = nodeFollowerLifecycle();

const forced = (tx: ReturnType<typeof canonicalTransaction>) =>
  encodeMidgardForcedTxCanonical(materializeMidgardForcedTxFromCanonical(tx));

/** The three ruled stops, each as the order's transaction. */
const STOPS = [
  [
    "auxiliary hash",
    "E_AUX_DATA_FORBIDDEN auxiliary_data",
    () => {
      const tx = canonicalTransaction();
      return forced({
        ...tx,
        body: { ...tx.body, auxiliaryDataHash: Buffer.alloc(32, 1) },
      });
    },
  ],
  [
    "witness envelope",
    "E_SCRIPT_PROGRAM_ENCODING script_witnesses",
    () => {
      const tx = canonicalTransaction();
      return forced({
        ...tx,
        witnessSet: {
          ...tx.witnessSet,
          scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
            { language: "MidgardV1", scriptBytes: Buffer.from("80", "hex") },
          ]),
        },
      });
    },
  ],
  [
    "reference envelope",
    "E_SCRIPT_PROGRAM_ENCODING reference_scripts",
    () => {
      const tx = canonicalTransaction();
      return forced({
        ...tx,
        body: {
          ...tx.body,
          outputsPreimageCbor: encodeCbor([
            encodeMidgardTxOutput({
              address: TEST_ADDRESS,
              value: { lovelace: 100_000_000n, assets: new Map() },
              script_ref: {
                language: "MidgardV1",
                scriptBytes: Buffer.from("80", "hex"),
              },
            }),
          ]),
        },
      });
    },
  ],
  [
    "value size",
    "E_VALUE_SIZE output_value",
    () => {
      const tx = canonicalTransaction();
      const assets = new Map(
        Array.from(
          { length: 170 },
          (_, i) => [i.toString(16).padStart(64, "0"), 1n] as const,
        ),
      );
      return forced({
        ...tx,
        body: {
          ...tx.body,
          outputsPreimageCbor: encodeCbor([
            encodeMidgardTxOutput({
              address: TEST_ADDRESS,
              value: {
                lovelace: 100_000_000n,
                assets: new Map([["01".repeat(28), assets]]),
              },
            }),
          ]),
        },
      });
    },
  ],
] as const;

/**
 * Transactions the order policy accepts whose items do not decode (unruled),
 * with what the admission screen throws: the field-6 envelope decode
 * (`WitnessScriptHeaderMalformed`) and the field-2 output decode
 * (`OutputNonCanonical` via `ForcedTxInvalid`).
 */
const UNDECODABLE = [
  [
    "a field-6 script header that does not decode",
    /Indefinite-length CBOR is not valid/u,
    () => {
      const tx = canonicalTransaction();
      return forced({
        ...tx,
        witnessSet: {
          ...tx.witnessSet,
          scriptTxWitsPreimageCbor: encodeMidgardFieldPreimage([
            Buffer.from("ff", "hex"),
          ]),
        },
      });
    },
  ],
  [
    "a field-2 output that is not canonical",
    /Midgard output missing address key 0/u,
    () => {
      const tx = canonicalTransaction();
      return forced({
        ...tx,
        body: {
          ...tx.body,
          outputsPreimageCbor: encodeMidgardFieldPreimage([
            Buffer.from("a0", "hex"),
          ]),
        },
      });
    },
  ],
] as const;

describe("forced-order hook holds (N10b)", () => {
  it.each(STOPS)(
    "the %s stop holds forced_order_admission_stopped by name and caps the horizon",
    async (_name, stop, bytes) => {
      const { store, chain } = await follow();
      const order = inlineOrder(chain.chain, bytes());
      await chain.forward([order]);
      const { hook } = ingestionHook(store, FORCED_CONFIG);
      for (let attempt = 0; attempt < 2; attempt += 1) {
        const hold = await hook(UNCHANGED);
        expect(hold?.reason).toBe(FORCED_ORDER_ADMISSION_STOPPED);
        expect(hold?.detail).toContain(`${label(order)}: ${stop} (`);
        expect(await rows()).toEqual([]);
        expect(await horizon()).toBe(Number(INCLUSION) - 1);
      }
    },
  );

  it.each(UNDECODABLE)(
    "an order with %s still holds forced_order_ingestion_failed and caps the horizon",
    async (_name, thrown, bytes) => {
      // The screen throws on the item, never returning a ruled stop.
      expect(() => validateMidgardConsensusForcedTxCbor(bytes())).toThrow(
        thrown,
      );
      const { store, chain } = await follow();
      const order = inlineOrder(chain.chain, bytes());
      await chain.forward([order]);
      const { hook } = ingestionHook(store, FORCED_CONFIG);
      const hold = await hook(UNCHANGED);
      expect(hold?.reason).toBe(FORCED_ORDER_INGESTION_FAILED);
      expect(hold?.detail).toContain(
        `${label(order)}: DatabaseError: Failed to verify the exact canonical V1 forced transaction`,
      );
      expect(await rows()).toEqual([]);
      expect(await horizon()).toBe(Number(INCLUSION) - 1);
    },
  );

  it("a failure outranks a stop and names it as a count", async () => {
    const { store, chain } = await follow();
    const stopped = inlineOrder(chain.chain, STOPS[0][2](), 6_000n);
    const failed = inlineOrder(chain.chain, UNDECODABLE[0][2](), 7_000n);
    const good = inlineOrder(chain.chain, honest(), 8_000n);
    await chain.forward([stopped, failed, good]);
    const { hook } = ingestionHook(store, FORCED_CONFIG);
    const hold = await hook(UNCHANGED);
    expect(hold?.reason).toBe(FORCED_ORDER_INGESTION_FAILED);
    expect(hold?.detail).toContain(label(failed));
    expect(hold?.detail).toContain("1 more stopped at a ruled admission stop");
    // The honest order is still ingested; the other two cap the horizon.
    expect((await rows()).map((r) => Buffer.from(r.native_tx_cbor))).toEqual([
      honest(),
    ]);
    expect(await horizon()).toBe(6_000 - 1);
  });
});
