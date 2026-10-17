import {
  aikenSerialisedPlutusDataCborPreservingMapOrder,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { WithdrawalBody } from "../src/ledger-state.js";
import {
  signWithdrawalBody,
  signWithdrawalBodyCbor,
  verifyWithdrawalSignatureCbor,
  withdrawalSigningMessage,
  withdrawalSigningMessageCbor,
} from "../src/withdrawal-signature.js";

const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const owner = key.to_public().hash().to_hex();
const body: WithdrawalBody = {
  l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
  l2_owner: owner,
  l2_value: new Map([["", new Map([["", 20_000_000n]])]]),
  l1_address: {
    paymentCredential: { PublicKeyCredential: [owner] },
    stakeCredential: null,
  },
  l1_datum: { InlineDatum: { data: 0n } },
};

describe("raw Withdrawal signature preimage", () => {
  it.each(["a2020a010b", "a3020a010b020c"])(
    "signs exact ordered pairs %s",
    (raw) => {
      const cbor = replacePlutusConstrFieldCbor(
        Data.to(body, WithdrawalBody),
        [4, 0],
        raw,
      );
      const signature = signWithdrawalBodyCbor(key, cbor);
      expect(verifyWithdrawalSignatureCbor(cbor, signature, owner)).toEqual({
        valid: true,
        publicKeyHash: owner,
      });
      expect(
        verifyWithdrawalSignatureCbor(
          aikenSerialisedPlutusDataCborPreservingMapOrder(cbor),
          signature,
          owner,
        ).valid,
      ).toBe(true);
      const changed = replacePlutusConstrFieldCbor(cbor, [4, 0], "a2010b020a");
      expect(
        verifyWithdrawalSignatureCbor(changed, signature, owner),
      ).toMatchObject({ valid: false, reason: "invalid_signature" });
      expect(
        verifyWithdrawalSignatureCbor(cbor, signature, "bb".repeat(28)),
      ).toMatchObject({ valid: false, reason: "owner_hash_mismatch" });
    },
  );

  it("retains typed signing semantics and refuses a malformed raw body", () => {
    const cbor = Data.to(body, WithdrawalBody);
    expect(withdrawalSigningMessageCbor(cbor)).toEqual(
      withdrawalSigningMessage(body),
    );
    expect(signWithdrawalBodyCbor(key, cbor)).toEqual(
      signWithdrawalBody(key, body),
    );
    expect(() => signWithdrawalBodyCbor(key, "00")).toThrow();
    expect(
      verifyWithdrawalSignatureCbor("00", signWithdrawalBody(key, body), owner)
        .valid,
    ).toBe(false);
  });
});
