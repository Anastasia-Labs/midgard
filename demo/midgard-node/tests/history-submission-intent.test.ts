import { replacePlutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { depositSubmissionIntentHash } from "../src/transactions/submit-deposit.js";
import { withdrawalSubmissionIntentHash } from "../src/transactions/submit-withdrawal.js";

const owner = "11".repeat(28);
const address = {
  paymentCredential: { PublicKeyCredential: [owner] as [string] },
  stakeCredential: null,
};
const raw = "a302c2410101d86682008002c2420001";
const normalized = "a3020101d879800201";
const reordered = "a301d8798002010201";

describe("durable raw event intents", () => {
  it("keeps deposit duplicate pairs and order in the pre-nonce identity", () => {
    const config = {
      l2Address: credentialToAddress("Custom", { type: "Key", hash: owner }),
      l2Datum: raw,
      lovelace: 5_000_000n,
      additionalAssets: {},
    };
    const hash = depositSubmissionIntentHash(config);
    expect(
      depositSubmissionIntentHash({ ...config, l2Datum: normalized }),
    ).toBe(hash);
    for (const l2Datum of [null, reordered, Data.to(Data.from(raw))])
      expect(depositSubmissionIntentHash({ ...config, l2Datum })).not.toBe(
        hash,
      );
  });

  it("binds raw withdrawal body and refund independently", () => {
    const body: SDK.WithdrawalBody = {
      l2_outref: { transactionId: "22".repeat(32), outputIndex: 0n },
      l2_owner: owner,
      l2_value: new Map([["", new Map([["", 5_000_000n]])]]),
      l1_address: address,
      l1_datum: { InlineDatum: { data: 0n } },
    };
    const bodyCbor = replacePlutusConstrFieldCbor(
      Data.to(body, SDK.WithdrawalBody),
      [4, 0],
      raw,
    );
    const refundDatumCbor = `d87b9f${raw}ff`;
    const config: SDK.SubmitWithdrawalConfig = {
      bodyCbor,
      signature: ["33".repeat(32), "44".repeat(64)],
      refundAddress: address,
      refundDatumCbor,
    };
    const hash = withdrawalSubmissionIntentHash(config);
    expect(
      withdrawalSubmissionIntentHash({
        ...config,
        bodyCbor: replacePlutusConstrFieldCbor(bodyCbor, [4, 0], normalized),
        refundDatumCbor: `d87b9f${normalized}ff`,
      }),
    ).toBe(hash);
    expect(
      withdrawalSubmissionIntentHash({
        ...config,
        bodyCbor: replacePlutusConstrFieldCbor(bodyCbor, [4, 0], reordered),
      }),
    ).not.toBe(hash);
    expect(
      withdrawalSubmissionIntentHash({
        ...config,
        refundDatumCbor: `d87b9f${reordered}ff`,
      }),
    ).not.toBe(hash);
  });
});
