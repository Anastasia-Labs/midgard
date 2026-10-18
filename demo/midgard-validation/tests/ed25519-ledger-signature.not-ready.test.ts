import { verifyLedgerEd25519Signature } from "@al-ft/midgard-core";
import {
  findInvalidAddressWitnessIndex,
  nativeTxHasInvalidSignatureViolation,
  signWithdrawalBodyCbor,
  verifyAddressWitness,
  verifyWithdrawalSignatureCbor,
  WithdrawalBody,
} from "@al-ft/midgard-sdk";
import { CML, Data } from "@lucid-evolution/lucid";
import { describe, expect, it, vi } from "vitest";

import { phaseAVerdict } from "./ed25519-ledger-transactions.js";
import {
  honestSignature,
  signerPublicKey,
  signerSeed,
} from "./ed25519-ledger-vectors.js";
import { makeNativeTx } from "./validation-fixtures.js";

/**
 * Until libsodium has initialised there is no Ed25519 answer: every check
 * raises, and none reports a signature as not verifying. libsodium here never
 * initialises, and nothing in this file waits for it.
 */
vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "ready"
            ? new Promise<never>(() => undefined)
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

const message = Buffer.alloc(32, 0x42);
const seed = signerSeed(0);
const vkey = signerPublicKey(seed);
const signature = honestSignature(seed, message);
const witness = {
  verification_key: vkey.toString("hex"),
  signature: signature.toString("hex"),
};
const brokenWitness = {
  ...witness,
  signature: Buffer.alloc(64, 0x01).toString("hex"),
};

describe("Ed25519 verification before libsodium initialises", () => {
  it("raises from the shared verifier", () => {
    expect(() =>
      verifyLedgerEd25519Signature(vkey, message, signature),
    ).toThrow();
    expect(() =>
      verifyLedgerEd25519Signature(vkey, message, Buffer.alloc(64, 0x01)),
    ).toThrow();
  });

  it("raises from the SDK's address-witness checks", () => {
    const txId = message.toString("hex");
    expect(() => verifyAddressWitness({ txId, witness })).toThrow();
    expect(() =>
      verifyAddressWitness({ txId, witness: brokenWitness }),
    ).toThrow();
    expect(() =>
      findInvalidAddressWitnessIndex({
        txId,
        addrTxWits: [witness, brokenWitness],
      }),
    ).toThrow();
    expect(() =>
      nativeTxHasInvalidSignatureViolation({
        txId,
        addrTxWits: [brokenWitness],
      }),
    ).toThrow();
  });

  it("raises from Phase A", () => {
    for (const fixture of [
      makeNativeTx(),
      makeNativeTx({ invalidVkeyWitness: true }),
    ]) {
      expect(() => phaseAVerdict(fixture)).toThrow();
    }
  });

  it("raises from the withdrawal signature check", () => {
    const key = CML.PrivateKey.from_normal_bytes(seed);
    const owner = key.to_public().hash().to_hex();
    const bodyCbor = Data.to(
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
    const signed = signWithdrawalBodyCbor(key, bodyCbor);
    expect(() =>
      verifyWithdrawalSignatureCbor(bodyCbor, signed, owner),
    ).toThrow();
    expect(() =>
      verifyWithdrawalSignatureCbor(
        bodyCbor,
        [signed[0], "01".repeat(64)],
        owner,
      ),
    ).toThrow();
  });
});
