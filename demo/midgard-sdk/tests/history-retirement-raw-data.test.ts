import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { Data, type TxBuilder, type TxOutput } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { CardanoDatum } from "../src/ledger-state.js";
import { __reservePayoutTest as retirement } from "../src/reserve-payout.js";
import { EventHistoryPayload } from "../src/user-events/history.js";

const owner = "aa".repeat(28);
const address = {
  paymentCredential: { PublicKeyCredential: [owner] as [string] },
  stakeCredential: null,
};
const payload: EventHistoryPayload = {
  WithdrawalPayload: {
    event: {
      id: { transactionId: "bb".repeat(32), outputIndex: 0n },
      info: {
        body: {
          l2_outref: { transactionId: "cc".repeat(32), outputIndex: 1n },
          l2_owner: owner,
          l2_value: new Map([["", new Map([["", 20_000_000n]])]]),
          l1_address: address,
          l1_datum: { InlineDatum: { data: 0n } },
        },
        signature: ["dd".repeat(32), "ee".repeat(64)],
        validity: "WithdrawalIsValid",
      },
    },
    refund_address: address,
    refund_datum: { InlineDatum: { data: 0n } },
  },
};

describe("retirement raw output assembly (no ledger evaluation)", () => {
  it.each(["a2020a010b", "a3020a010b020c"])(
    "retains payout and refund datum %s through output selection",
    (raw) => {
      const payloadCbor = replacePlutusConstrFieldCbor(
        replacePlutusConstrFieldCbor(
          Data.to(payload, EventHistoryPayload),
          [0, 1, 0, 4, 0],
          raw,
        ),
        [2, 0],
        raw,
      );
      const payout = retirement.withdrawalPayoutDatumCbor(payloadCbor);
      expect(plutusConstrFieldCbor(payout, [2, 0])).toBe(raw);
      for (let field = 0; field < 3; field++)
        expect(plutusConstrFieldCbor(payout, [field])).toBe(
          plutusConstrFieldCbor(payloadCbor, [0, 1, 0, field + 2]),
        );
      const payoutWrapper = replacePlutusConstrFieldCbor(
        Data.to({ InlineDatum: { data: 0n } }, CardanoDatum),
        [0],
        payout,
      );
      expect(retirement.cardanoDatumCborToOutputDatum(payoutWrapper)).toEqual({
        kind: "inline",
        value: payout,
      });
      for (const wrapper of [
        plutusConstrFieldCbor(payout, [2]),
        plutusConstrFieldCbor(payloadCbor, [2]),
      ]) {
        const pay = vi.fn();
        const tx = { pay: { ToAddressWithData: pay } } as unknown as TxBuilder;
        const assets = { lovelace: 20_000_000n };
        retirement.payToAddressWithCardanoDatum(
          tx,
          "destination",
          wrapper,
          assets,
        );
        expect(pay).toHaveBeenCalledWith(
          "destination",
          { kind: "inline", value: raw },
          assets,
        );
        const expected: TxOutput = {
          address: "destination",
          assets,
          datum: raw,
        };
        const normalized: TxOutput = { ...expected, datum: "a2010b020a" };
        expect(retirement.outputDatumMatches(normalized, wrapper)).toBe(false);
        expect(
          retirement.outputWithCardanoDatumIndex(
            [normalized, expected],
            "destination",
            wrapper,
            assets,
            "destination",
          ),
        ).toBe(1n);
        expect(() =>
          retirement.outputWithCardanoDatumIndex(
            [normalized],
            "destination",
            wrapper,
            assets,
            "destination",
          ),
        ).toThrow();
      }
    },
  );

  it("preserves absent/hash datum cases", () => {
    expect(
      retirement.cardanoDatumCborToOutputDatum(
        Data.to("NoDatum", CardanoDatum),
      ),
    ).toBeUndefined();
    expect(
      retirement.cardanoDatumCborToOutputDatum(
        Data.to({ DatumHash: { hash: "12".repeat(32) } }, CardanoDatum),
      ),
    ).toEqual({ kind: "hash", value: "12".repeat(32) });
  });
});
