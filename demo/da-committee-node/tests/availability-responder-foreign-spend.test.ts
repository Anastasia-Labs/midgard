import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  expectAwaitingScan,
  expectRefused,
  expectStillPending,
  FINALITY,
  type Fixture,
  fixture,
  removeFixtureDirs,
  TIP_BLOCK_NO,
} from "./helpers/availability-foreign-spend.js";

/**
 * A committee Publish, Settle or Close spends only protocol UTxOs; the
 * responder wallet backs collateral alone. When another party's transaction
 * (another member, a watcher, a griefer copying the mempool bytes) consumes
 * those inputs first, every normal input of the committee's signed intent is
 * gone. The intent must expire once that rival spend is verified and final,
 * so the next tick discovers live challenges again; anything short of that
 * keeps it pending, and inconsistent evidence throws.
 *
 * The wallet's coins stand in for the protocol inputs: reconcile judges an
 * intent by its signed bytes and the spends of its inputs, not by who owns
 * them.
 */

afterEach(removeFixtureDirs);

const STEPS = [
  // A first-chunk Publish spends the tranche thread alone, so no other input
  // can stay unspent to prove it lost.
  ["a first-chunk Publish", "publish", 1],
  ["a Settle", "settle", 3],
  ["a Close", "close", 3],
] as const;

describe("committee responder after a rival spent its step's inputs", () => {
  it.each(STEPS)(
    "expires %s once the rival spend is final, and discovers again",
    async (_label, action, inputs) => {
      const f = await fixture(action, inputs);
      try {
        await expect(f.responder.tick()).resolves.toStrictEqual({
          challenges: 0,
          status: "idle",
        });
        expect(f.discover).toHaveBeenCalledTimes(1);
        expect(f.state()).toBe("expired");
        expect(f.journal.get(f.ours.id)?.detail).toBe(
          "Expired with a normal input finally spent by another transaction",
        );
        expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual([]);
      } finally {
        f.journal.close();
      }
    },
  );

  it.each([
    [
      "the rival spend is one block short of finality",
      (f: Fixture) => {
        f.chain.evidence!.blockNo = TIP_BLOCK_NO - FINALITY + 1;
      },
    ],
    [
      "the intent is still inside its validity",
      (f: Fixture) => {
        f.chain.slot = f.ours.validUntilSlot - 1;
      },
    ],
    [
      "the stored spender transaction does not list the input",
      (f: Fixture) => {
        f.chain.evidence = { ...f.split, blockNo: TIP_BLOCK_NO - FINALITY };
      },
    ],
    [
      "the rival transaction failed phase 2",
      (f: Fixture) => {
        const valid = CML.Transaction.from_cbor_hex(f.rival.cbor);
        f.chain.evidence!.cbor = CML.Transaction.new(
          valid.body(),
          valid.witness_set(),
          false,
          valid.auxiliary_data(),
        ).to_cbor_hex();
      },
    ],
    [
      "the follower holds no spend",
      (f: Fixture) => {
        f.chain.evidence = undefined;
      },
    ],
  ])("keeps the intent pending when %s", async (_label, arrange) => {
    const f = await fixture("settle", 3);
    try {
      arrange(f);
      await expectStillPending(f);
    } finally {
      f.journal.close();
    }
  });

  it.each([
    [
      "the named spender is the committee's own transaction",
      (f: Fixture) => {
        f.chain.evidence = {
          txHash: f.ours.txHash,
          cbor: f.ours.signedCbor,
          blockNo: TIP_BLOCK_NO - FINALITY,
        };
      },
      "Invalid canonical missing-input observation",
    ],
    [
      "the spend block lies above the boundary",
      (f: Fixture) => {
        f.chain.evidence!.blockNo = TIP_BLOCK_NO + 1;
      },
      "Availability input spend lies above the canonical boundary",
    ],
    [
      "the spend is served without its raw transaction",
      (f: Fixture) => {
        delete f.chain.evidence!.cbor;
      },
      "Ogmios must run with --include-transaction-cbor to verify a rival spend",
    ],
  ])(
    "refuses and releases nothing when %s",
    async (_label, arrange, message) => {
      const f = await fixture("settle", 3);
      try {
        arrange(f);
        await expectRefused(f, message);
      } finally {
        f.journal.close();
      }
    },
  );

  it.each([
    [
      "the follower holds the committee unready",
      (f: Fixture) => {
        f.chain.held = "rollback_beyond_k: rolled back 7 blocks";
      },
      "rollback_beyond_k: rolled back 7 blocks",
    ],
    [
      "a rollback undoes the reconciled view while the rival spend is read",
      (f: Fixture) => {
        f.chain.onReadTransaction = () => {
          f.chain.generation += 1;
        };
      },
      "its view rolled back since this pass reconciled; the next pass reconciles again",
    ],
  ])(
    "awaits the follower and releases nothing when %s",
    async (_label, arrange, detail) => {
      const f = await fixture("settle", 3);
      try {
        arrange(f);
        await expectAwaitingScan(f, detail);
      } finally {
        f.journal.close();
      }
    },
  );

  it("awaits the next pass and releases nothing when the follower's view advances during the spend read", async () => {
    const f = await fixture("settle", 3);
    try {
      f.chain.onReadTransaction = () => {
        f.chain.slot += 1;
      };
      await expectAwaitingScan(
        f,
        "its view advanced during a canonical spend read; the next pass reads again",
      );
    } finally {
      f.journal.close();
    }
  });
});
