import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { WithdrawalBody } from "../src/ledger-state.js";
import {
  signWithdrawalBodyCbor,
  verifyWithdrawalSignatureCbor,
} from "../src/withdrawal-signature.js";

// A fault of the signature check itself, as a failed libsodium call would
// raise it.
vi.mock("@al-ft/midgard-core", async (original) => ({
  ...(await original<typeof import("@al-ft/midgard-core")>()),
  verifyLedgerEd25519Signature: () => {
    throw new Error("signature verifier fault");
  },
}));

const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const owner = key.to_public().hash().to_hex();

describe("withdrawal signature verifier fault", () => {
  it("raises the fault instead of reporting an invalid signature", () => {
    const body = Data.to(
      {
        l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
        l2_owner: owner,
        l2_value: new Map([["", new Map([["", 20_000_000n]])]]),
        l1_address: {
          paymentCredential: { PublicKeyCredential: [owner] },
          stakeCredential: null,
        },
        l1_datum: { InlineDatum: { data: 0n } },
      },
      WithdrawalBody,
    );
    const signature = signWithdrawalBodyCbor(key, body);
    expect(() => verifyWithdrawalSignatureCbor(body, signature, owner)).toThrow(
      "signature verifier fault",
    );
  });
});
