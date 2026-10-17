import {
  ledgerEd25519SignatureReady,
  verifyLedgerEd25519Signature,
} from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, Constr, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { beforeAll, describe, expect, it } from "vitest";

import { WithdrawalBody } from "../src/ledger-state.js";
import { verifyWithdrawalSignatureCbor } from "../src/withdrawal-signature.js";

/**
 * The withdrawal signature is checked over a withdrawal body only: Data of
 * another shape is refused as the body's, even under a signature the
 * ledger's verifier accepts over exactly those bytes.
 */
beforeAll(() => ledgerEd25519SignatureReady);

const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(9));
const publicKey = Buffer.from(key.to_public().to_raw_bytes());
const owner = key.to_public().hash().to_hex();

/** `blake2b_256("MidgardWithdrawalV1" ++ cbor.serialise(body))`. */
const chainMessage = (bodyCbor: string): Uint8Array =>
  blake2b(
    Buffer.concat([
      Buffer.from("MidgardWithdrawalV1", "utf8"),
      Buffer.from(
        aikenSerialisedPlutusDataCborPreservingMapOrder(bodyCbor),
        "hex",
      ),
    ]),
    { dkLen: 32 },
  );

const signed = (bodyCbor: string): [string, string] => [
  publicKey.toString("hex"),
  key.sign(chainMessage(bodyCbor)).to_hex(),
];

const fields = [
  new Constr(0, ["aa".repeat(32), 0n]),
  owner,
  new Map([["", new Map([["", 20_000_000n]])]]),
  new Constr(0, [new Constr(0, [owner]), new Constr(1, [])]),
];
const honestBody = Data.to(new Constr(0, [...fields, new Constr(0, [])]));

const OTHER_SHAPES = [
  ["the integer zero", "00"],
  ["a body missing its datum", Data.to(new Constr(0, fields))],
  [
    "a body whose datum is no datum alternative",
    Data.to(new Constr(0, [...fields, new Constr(3, [])])),
  ],
  [
    "a body with an extra field",
    Data.to(new Constr(0, [...fields, new Constr(0, []), 0n])),
  ],
] as const;

describe("withdrawal signature body shape", () => {
  it("accepts an honest signature over a withdrawal body", () => {
    expect(() => Data.from(honestBody, WithdrawalBody)).not.toThrow();
    expect(
      verifyWithdrawalSignatureCbor(honestBody, signed(honestBody), owner),
    ).toEqual({ valid: true, publicKeyHash: owner });
  });

  it.each(OTHER_SHAPES)(
    "refuses %s as the body's shape, however it is signed",
    (_label, bodyCbor) => {
      expect(() => Data.from(bodyCbor, WithdrawalBody)).toThrow();
      const signature = signed(bodyCbor);
      // The signature is the key's over exactly these bytes.
      expect(
        verifyLedgerEd25519Signature(
          publicKey,
          chainMessage(bodyCbor),
          Buffer.from(signature[1], "hex"),
        ),
      ).toBe(true);
      const verification = verifyWithdrawalSignatureCbor(
        bodyCbor,
        signature,
        owner,
      );
      expect(verification.valid).toBe(false);
      if (verification.valid) return;
      expect(verification.reason).not.toBe("invalid_signature");
      expect(verification.publicKeyHash).toBeUndefined();
    },
  );
});
