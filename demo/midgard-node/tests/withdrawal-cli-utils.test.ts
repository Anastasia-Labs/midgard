import {
  encodeMidgardAddressText,
  midgardAddressFromText,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  parseEventId,
  resolveWalletSeedPhrase,
} from "../src/commands/command-utils.js";
import {
  __submitWithdrawalTest,
  withdrawalEventIdFromBuildMetadata,
} from "../src/commands/submit-withdrawal.js";
import {
  parseCardanoDatum,
  parseWithdrawalTxOutRefLabel,
} from "../src/commands/withdrawal-utils.js";
import { assetsToValue } from "../src/transactions/reserve-payout.js";
import {
  publicKeyHashFromWithdrawalSignature,
  signWithdrawalBody,
  verifyWithdrawalSignature,
} from "../src/withdrawal-signature.js";

const seedPhrase =
  "test test test test test test test test test test test junk";
const wallet = walletFromSeed(seedPhrase, { network: "Preprod" });
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const keyHash = privateKey.to_public().hash().to_hex();

const makeWithdrawalBody = async (): Promise<SDK.WithdrawalBody> => {
  const l1Address = await Effect.runPromise(
    SDK.addressDataFromBech32(wallet.address),
  );
  return {
    l2_outref: {
      transactionId:
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      outputIndex: 0n,
    },
    l2_owner: keyHash,
    l2_value: assetsToValue({ lovelace: 7_000_000n }),
    l1_address: l1Address,
    l1_datum: "NoDatum",
  };
};

describe("withdrawal signature utilities", () => {
  it("signs and verifies a withdrawal body", async () => {
    const body = await makeWithdrawalBody();
    const signature = signWithdrawalBody(privateKey, body);
    expect(publicKeyHashFromWithdrawalSignature(signature)).toEqual(
      body.l2_owner,
    );
    expect(verifyWithdrawalSignature(body, signature, body.l2_owner)).toEqual({
      valid: true,
      publicKeyHash: body.l2_owner,
    });
  });

  it("rejects tampered withdrawal bodies and wrong owners", async () => {
    const body = await makeWithdrawalBody();
    const signature = signWithdrawalBody(privateKey, body);
    const tampered: SDK.WithdrawalBody = {
      ...body,
      l2_value: assetsToValue({ lovelace: 8_000_000n }),
    };
    expect(
      verifyWithdrawalSignature(tampered, signature, body.l2_owner),
    ).toMatchObject({
      valid: false,
      reason: "invalid_signature",
    });
    expect(
      verifyWithdrawalSignature(
        body,
        signature,
        "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb",
      ),
    ).toMatchObject({
      valid: false,
      reason: "owner_hash_mismatch",
    });
  });

  it("rejects malformed public keys and signatures", async () => {
    const body = await makeWithdrawalBody();
    expect(
      verifyWithdrawalSignature(body, ["aa", "bb"], body.l2_owner),
    ).toEqual({
      valid: false,
      reason: "malformed_public_key",
    });
    const [publicKey] = signWithdrawalBody(privateKey, body);
    expect(
      verifyWithdrawalSignature(body, [publicKey, "bb"], body.l2_owner),
    ).toEqual({
      valid: false,
      reason: "malformed_signature",
    });
  });
});

describe("withdrawal CLI parsers", () => {
  it("extracts selected protected L2 UTxO owners with the Midgard address codec", () => {
    const protectedAddress = encodeMidgardAddressText(
      protectMidgardAddress(midgardAddressFromText(wallet.address)),
    );

    expect(
      __submitWithdrawalTest.selectedUtxoPaymentKeyHash(protectedAddress),
    ).toEqual(keyHash);
  });

  it("rejects selected L2 UTxOs owned by script credentials", () => {
    const scriptAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_script(CML.ScriptHash.from_hex("22".repeat(28))),
    )
      .to_address()
      .to_bech32();

    expect(() =>
      __submitWithdrawalTest.selectedUtxoPaymentKeyHash(scriptAddress),
    ).toThrow("Selected L2 UTxO must be owned by a key credential.");
  });

  it("rejects malformed selected L2 UTxO addresses", () => {
    expect(() =>
      __submitWithdrawalTest.selectedUtxoPaymentKeyHash("not-an-address"),
    ).toThrow();
  });

  it("parses tx out refs and event ids in canonical OutputReference CBOR form", () => {
    const parsed = parseWithdrawalTxOutRefLabel(
      "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa#2",
    );
    expect(parsed.outputReference).toEqual({
      transactionId:
        "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      outputIndex: 2n,
    });
    const eventId = Data.to(parsed.outputReference, SDK.OutputReference);
    expect(parseEventId(eventId).toString("hex")).toEqual(eventId);
  });

  it("derives the withdrawal event id from the withdrawal nonce input", () => {
    const nonceInput = {
      txHash: "11".repeat(32),
      outputIndex: 0,
    };
    const selectedL2OutRef = {
      transactionId: "22".repeat(32),
      outputIndex: 3n,
    };
    const expectedEventId = Data.to(
      {
        transactionId: nonceInput.txHash,
        outputIndex: BigInt(nonceInput.outputIndex),
      },
      SDK.OutputReference,
    );
    expect(
      withdrawalEventIdFromBuildMetadata({
        withdrawalAddress:
          "addr_test1vqz4js6k2c6un3h8y8sh2nmkg7u9s8w7up0psd4w6zv6r9usknpxf",
        withdrawalEventIdCbor: expectedEventId,
        withdrawalAuthUnit: "aa".repeat(28) + "bb".repeat(32),
        nonceInput,
        validTo: 123,
        inclusionTime: 456,
      }),
    ).toEqual(expectedEventId);
    expect(expectedEventId).not.toEqual(
      Data.to(selectedL2OutRef, SDK.OutputReference),
    );
  });

  it("parses absent and inline datum arguments", () => {
    expect(parseCardanoDatum(undefined)).toEqual("NoDatum");
    expect(parseCardanoDatum("   ")).toEqual("NoDatum");
    const datum = parseCardanoDatum("d87980");
    expect(datum).toHaveProperty("InlineDatum");
    expect(() => parseCardanoDatum("d8798")).toThrow(
      "datum must be an even-length hex string",
    );
  });

  it("resolves seed phrases from direct input before env vars", () => {
    expect(
      resolveWalletSeedPhrase({
        walletSeedPhrase: "direct seed",
        walletSeedPhraseEnv: "USER_WALLET",
        env: { USER_WALLET: "env seed" },
      }),
    ).toEqual({
      seedPhrase: "direct seed",
      resolvedFrom: "direct-argument",
    });
  });
});
