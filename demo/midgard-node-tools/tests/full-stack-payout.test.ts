import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  includedPayout,
  payoutConclusion,
  payoutOutRef,
  type SettlementAttempt,
} from "../src/full-stack/payout-body.js";
import {
  exactPayout,
  fundedOnly,
  PAYOUT,
  payoutAssets,
  payoutBody,
  payoutToken,
  recipient,
  stranger,
} from "./full-stack-payout.fixtures.js";

const complete = [{ phase: "complete" }];
const localConclusion: SettlementAttempt = {
  phase: "conclude",
  status: "pending",
  txHash: "a".repeat(64),
  signedCbor: "",
};

describe("withdrawal payout decision", () => {
  it("accepts a payout another operator concluded on L1", () => {
    // The twin case: this node's job completed through the settled-elsewhere
    // path, so its journal holds no conclusion of its own.
    for (const attempts of [
      fundedOnly,
      null,
      // Its own conclusion that lost the race and has not expired yet.
      [...fundedOnly, localConclusion],
    ]) {
      const conclusion = payoutConclusion({
        jobs: complete,
        attempts,
        payoutToken: payoutToken(exactPayout),
      });
      expect(conclusion).toEqual(exactPayout);
      expect(payoutOutRef(conclusion!, recipient, payoutAssets)).toEqual({
        txHash: exactPayout.txHash,
        outputIndex: 0,
      });
    }
  });
  it("waits while no transaction burned the payout token", () => {
    for (const token of [payoutToken(null), [], null, undefined])
      expect(
        payoutConclusion({
          jobs: complete,
          attempts: fundedOnly,
          payoutToken: token,
        }),
      ).toBeUndefined();
    // The burn alone is not enough: the local job is the depth signal.
    for (const jobs of [[{ phase: "fund" }], [], null])
      expect(
        payoutConclusion({
          jobs,
          attempts: fundedOnly,
          payoutToken: payoutToken(exactPayout),
        }),
      ).toBeUndefined();
    // A follower that no longer holds the burning body cannot show the payout.
    expect(
      payoutConclusion({
        jobs: complete,
        attempts: fundedOnly,
        payoutToken: payoutToken(exactPayout).map((output) => ({
          ...output,
          spenderBody: null,
        })),
      }),
    ).toBeUndefined();
  });
  it("refuses a payout of another value or a second output at the recipient", () => {
    for (const outputs of [
      [{ address: recipient, lovelace: PAYOUT - 1n }],
      [{ address: stranger, lovelace: PAYOUT }],
      [
        { address: recipient, lovelace: PAYOUT },
        { address: recipient, lovelace: PAYOUT },
      ],
    ]) {
      const conclusion = payoutConclusion({
        jobs: complete,
        attempts: fundedOnly,
        payoutToken: payoutToken(payoutBody(outputs)),
      });
      expect(() => payoutOutRef(conclusion!, recipient, payoutAssets)).toThrow(
        "exactly one output with the exact destination and value",
      );
    }
  });
  it("refuses a body that is not the burning transaction's", () =>
    expect(() =>
      payoutOutRef(
        { ...exactPayout, txHash: "b".repeat(64) },
        recipient,
        payoutAssets,
      ),
    ).toThrow("does not hash to its recorded id"));
  it("refuses two unexpired local conclusions for one withdrawal", () =>
    expect(() =>
      payoutConclusion({
        jobs: complete,
        attempts: [
          localConclusion,
          { ...localConclusion, status: "final", txHash: "b".repeat(64) },
        ],
        payoutToken: payoutToken(exactPayout),
      }),
    ).toThrow("More than one unexpired payout"));
  it("refuses a payout token that two transactions burned", () =>
    expect(() =>
      payoutConclusion({
        jobs: complete,
        attempts: fundedOnly,
        payoutToken: [
          ...payoutToken(exactPayout),
          { txHash: "c".repeat(64), spentTx: "d".repeat(64), spenderBody: "" },
        ],
      }),
    ).toThrow("More than one transaction burned"));
});

describe("payout inclusion", () => {
  const outRef = { txHash: exactPayout.txHash, outputIndex: 0 };
  // A `utxo_by_txin` answer: a CBOR map from `[txHash, index]` to the
  // ledger's output (here the payout output's own bytes).
  const outputCbor = Buffer.from(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(recipient),
      CML.Value.from_coin(PAYOUT),
    ).to_cbor_bytes(),
  );
  const answer = (rows: readonly (readonly [string, number])[]) =>
    Buffer.concat([
      Buffer.from([0xa0 + rows.length]),
      ...rows.flatMap(([id, index]) => [
        Buffer.from([0x82, 0x58, 0x20]),
        Buffer.from(id, "hex"),
        Buffer.from([index]),
        outputCbor,
      ]),
    ]);
  it("waits until the node's ledger holds the payout output", () => {
    expect(includedPayout(outRef, answer([]))).toBeUndefined();
    expect(includedPayout(outRef, answer([[outRef.txHash, 0]]))).toEqual({
      outputIndex: 0,
    });
  });
  it("refuses an answer for another output", () => {
    for (const rows of [
      [["c".repeat(64), 0]],
      [[outRef.txHash, 1]],
      [
        [outRef.txHash, 0],
        [outRef.txHash, 1],
      ],
    ] as const)
      expect(() => includedPayout(outRef, answer(rows))).toThrow(
        "another output",
      );
    expect(() => includedPayout(outRef, Buffer.from([0x80]))).toThrow();
  });
});
