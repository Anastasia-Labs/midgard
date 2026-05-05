import { describe, expect, it } from "vitest";
import { Effect } from "effect";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { assetsToValue } from "@/transactions/reserve-payout.js";
import {
  publicKeyHashFromWithdrawalSignature,
  signWithdrawalBody,
  verifyWithdrawalSignature,
} from "@/withdrawal-signature.js";
import {
  parseCardanoDatum,
  parseEventId,
  parseTxOutRefLabel,
  resolveWalletSeedPhrase,
} from "@/commands/withdrawal-utils.js";
import { withdrawalEventIdFromBuildMetadata } from "@/commands/submit-withdrawal.js";

const seedPhrase =
  "test test test test test test test test test test test junk";

const makeWithdrawalBody = async (): Promise<SDK.WithdrawalBody> => {
  const wallet = walletFromSeed(seedPhrase, { network: "Preprod" });
  const keyHash = CML.PrivateKey.from_bech32(wallet.paymentKey)
    .to_public()
    .hash()
    .to_hex();
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
    const privateKey = CML.PrivateKey.from_bech32(
      walletFromSeed(seedPhrase, { network: "Preprod" }).paymentKey,
    );
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
    const privateKey = CML.PrivateKey.from_bech32(
      walletFromSeed(seedPhrase, { network: "Preprod" }).paymentKey,
    );
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
    const privateKey = CML.PrivateKey.from_bech32(
      walletFromSeed(seedPhrase, { network: "Preprod" }).paymentKey,
    );
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
  it("parses tx out refs and event ids in canonical OutputReference CBOR form", () => {
    const parsed = parseTxOutRefLabel(
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
    const datum = parseCardanoDatum("d87980");
    expect(datum).toHaveProperty("InlineDatum");
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
