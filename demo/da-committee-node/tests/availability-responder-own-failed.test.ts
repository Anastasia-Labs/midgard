import { CML } from "@lucid-evolution/lucid";
import { afterEach, describe, expect, it } from "vitest";

import {
  expectStillPending,
  FINALITY,
  type Fixture,
  fixture,
  removeFixtureDirs,
  TIP_BLOCK_NO,
} from "./helpers/availability-foreign-spend.js";

/**
 * A committee step that landed and failed consumed its collateral and left
 * its normal inputs unspent. The signed transaction can never land again,
 * so the intent must expire once that landing is final, and the next tick
 * discovers live challenges again; one block short, or a landing that did
 * not consume its collateral, keeps it pending.
 */

afterEach(removeFixtureDirs);

describe("committee responder after its own step landed and failed", () => {
  /** The follower holds the committee's own step as a failed landing. */
  const landFailed = (f: Fixture, blockNo: number, valid = false) => {
    const signed = CML.Transaction.from_cbor_hex(f.ours.signedCbor);
    f.chain.ownFailed = true;
    f.chain.evidence = {
      txHash: f.ours.txHash,
      cbor: CML.Transaction.new(
        signed.body(),
        signed.witness_set(),
        valid,
        signed.auxiliary_data(),
      ).to_cbor_hex(),
      blockNo,
    };
    // Still inside its validity: only the final landing expires it.
    f.chain.slot = f.ours.validUntilSlot - 1;
  };

  it("expires the intent once its failed landing is final, and discovers again", async () => {
    const f = await fixture("settle", 3);
    try {
      landFailed(f, TIP_BLOCK_NO - FINALITY);
      await expect(f.responder.tick()).resolves.toStrictEqual({
        challenges: 0,
        status: "idle",
      });
      expect(f.state()).toBe("expired");
      expect(f.journal.get(f.ours.id)?.detail).toBe(
        "Expired with its own landing failed and its collateral spend final",
      );
      expect(f.journal.reservedOutRefs(f.ours.actor)).toEqual([]);
      expect(f.discover).toHaveBeenCalledTimes(1);
    } finally {
      f.journal.close();
    }
  });

  it.each([
    [
      "the failed landing is one block short of finality",
      (f: Fixture) => landFailed(f, TIP_BLOCK_NO - FINALITY + 1),
    ],
    [
      "the stored landing did not consume its collateral",
      (f: Fixture) => landFailed(f, TIP_BLOCK_NO - FINALITY, true),
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
});
