import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import { CML, Data } from "@lucid-evolution/lucid";
import { blake2b } from "@noble/hashes/blake2.js";
import { beforeAll, describe, expect, it } from "vitest";

import { WithdrawalBody } from "../src/ledger-state.js";
import {
  publicKeyHashFromWithdrawalSignature,
  verifyWithdrawalSignatureCbor,
} from "../src/withdrawal-signature.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady);

const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const publicKeyHex = Buffer.from(key.to_public().to_raw_bytes()).toString(
  "hex",
);
const owner = key.to_public().hash().to_hex();

/** The chain's signed message (`withdrawal_signature_is_valid_v1`):
 * `blake2b_256("MidgardWithdrawalV1" ++ cbor.serialise(body))`. */
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

const signChainMessage = (bodyCbor: string): [string, string] => [
  publicKeyHex,
  key.sign(chainMessage(bodyCbor)).to_hex(),
];

const bodyWithInlineDatum = (datumCbor: string): string => {
  const shallow = Data.to(
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
  // The inline datum is the body's last field: `d87b9f 00 ff` then the body's
  // closing break. Splice the datum bytes in place of the `00`.
  const suffix = "d87b9f00ffff";
  expect(shallow.endsWith(suffix)).toBe(true);
  return `${shallow.slice(0, -suffix.length)}d87b9f${datumCbor}ffff`;
};

// Data values nested as deep as one Cardano transaction's bytes allow: one
// byte per list level, four per constructor level.
const DEEP_DATUMS = [
  ["lists", (depth: number) => `${"81".repeat(depth)}00`, 15_000],
  [
    "constructors",
    (depth: number) => `${"d8799f".repeat(depth)}00${"ff".repeat(depth)}`,
    3_900,
  ],
] as const;

describe("withdrawal signature, as the chain checks it", () => {
  it.each(DEEP_DATUMS)(
    "accepts an honest signature over a body whose inline datum nests %s",
    (_label, nested, depth) => {
      const body = bodyWithInlineDatum(nested(depth));
      expect(
        verifyWithdrawalSignatureCbor(body, signChainMessage(body), owner),
      ).toEqual({ valid: true, publicKeyHash: owner });
    },
  );

  it.each(DEEP_DATUMS)(
    "refuses a signature over another body when the inline datum nests %s",
    (_label, nested, depth) => {
      const body = bodyWithInlineDatum(nested(depth));
      const other = bodyWithInlineDatum(nested(depth - 1));
      expect(
        verifyWithdrawalSignatureCbor(body, signChainMessage(other), owner),
      ).toEqual({
        valid: false,
        reason: "invalid_signature",
        publicKeyHash: owner,
      });
    },
  );

  it.each([
    ["y above the field prime", `${"ff".repeat(31)}7f`],
    ["y with no curve point", `02${"00".repeat(31)}`],
  ])(
    "hashes a key that is not a curve point (%s) as raw bytes and refuses its signature",
    (_label, nonPointKey) => {
      const nonPointOwner = Buffer.from(
        blake2b(Buffer.from(nonPointKey, "hex"), { dkLen: 28 }),
      ).toString("hex");
      const body = bodyWithInlineDatum("00");
      const signature: [string, string] = [nonPointKey, "00".repeat(64)];
      expect(publicKeyHashFromWithdrawalSignature(signature)).toBe(
        nonPointOwner,
      );
      expect(
        verifyWithdrawalSignatureCbor(body, signature, nonPointOwner),
      ).toEqual({
        valid: false,
        reason: "invalid_signature",
        publicKeyHash: nonPointOwner,
      });
    },
  );

  it.each([
    ["a truncated body", "d8799f"],
    ["trailing bytes after the body", `${bodyWithInlineDatum("00")}00`],
    ["a break with nothing open", "ff"],
  ])("refuses %s without raising", (_label, body) => {
    const signature = signChainMessage(bodyWithInlineDatum("00"));
    expect(verifyWithdrawalSignatureCbor(body, signature, owner).valid).toBe(
      false,
    );
  });
});

// The same key and bodies judged on chain by `withdrawal_signature_is_valid_v1`,
// in onchain/aiken/lib/midgard/fraud-proofs/withdrawal-mistag/
// step-03.signature-agreement.test.ak. The on-chain test runner's own stack
// bounds how deep a datum it can judge, so these nest 1,000 deep.
const TWIN_DEPTH = 1_000;

const CHAIN_TWIN = {
  publicKey: "ea4a6c63e29c520abef5507b132ec5f9954776aebebe7b92421eea691446d22c",
  owner: "8b218424ad74df25d35c2ea8e094a4c5c5aeb2cbb442419331569313",
  lists: {
    honest:
      "66bed881e2eb231e4c41e57c45e45489d08246250a2660a39d91cebb0e455631331fcfbe9194863326126a803566a0af02131cc6f2d6aeffb6de5d2370a52305",
    other:
      "5568dcfe380b2b085f0c21c4ba1e25ac80c28e0f4d360413cf2542f51b2baa0ad88bdf30ab4759373b57cfa7f890f102a7fd6037f97c06dbd035d4014f7ff909",
  },
  constructors: {
    honest:
      "afd0ab5bd0f13d82c6903130ae1e57e14f883585d145cdb62edc12c892aaecb5f89696b055a25961a65574ed9a9fb78095cdfd6c71f489e28170f40bcae62103",
    other:
      "3c19b1693e2bfe66c194197cfd41909da60fed3956d600a0a805b87ac1f00f90cc39cbf2013361a2ba35aff8d2a4da11c0722c77ea93f0e7597f622ed266ec09",
  },
} as const;

describe("withdrawal signature chain-twin vectors", () => {
  it("are this key's signatures over these bodies, with the chain's verdicts", () => {
    expect(publicKeyHex).toBe(CHAIN_TWIN.publicKey);
    expect(owner).toBe(CHAIN_TWIN.owner);
    for (const [label, nested] of DEEP_DATUMS) {
      const body = bodyWithInlineDatum(nested(TWIN_DEPTH));
      const honest = signChainMessage(body);
      const other = signChainMessage(
        bodyWithInlineDatum(nested(TWIN_DEPTH - 1)),
      );
      expect({ honest: honest[1], other: other[1] }).toEqual(CHAIN_TWIN[label]);
      expect(verifyWithdrawalSignatureCbor(body, honest, owner).valid).toBe(
        true,
      );
      expect(verifyWithdrawalSignatureCbor(body, other, owner).valid).toBe(
        false,
      );
    }
  });
});
