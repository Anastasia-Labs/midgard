import { chmodSync, mkdtempSync, rmSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import {
  parseEventId,
  resolveWalletSeedPhrase,
} from "../src/commands/command-utils.js";
import {
  parseCardanoDatum,
  parseCardanoDatumCbor,
} from "../src/commands/withdrawal-utils.js";
import { assetsToValue } from "../src/transactions/reserve-payout.js";

// The ledger's Ed25519 check is libsodium's, which initialises
// asynchronously; production entry points await it, and so does this file.
beforeAll(() => ledgerEd25519SignatureReady());

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
    const signature = SDK.signWithdrawalBody(privateKey, body);
    expect(SDK.publicKeyHashFromWithdrawalSignature(signature)).toEqual(
      body.l2_owner,
    );
    expect(
      SDK.verifyWithdrawalSignature(body, signature, body.l2_owner),
    ).toEqual({
      valid: true,
      publicKeyHash: body.l2_owner,
    });
  });

  it("rejects tampered withdrawal bodies and wrong owners", async () => {
    const body = await makeWithdrawalBody();
    const signature = SDK.signWithdrawalBody(privateKey, body);
    const tampered: SDK.WithdrawalBody = {
      ...body,
      l2_value: assetsToValue({ lovelace: 8_000_000n }),
    };
    expect(
      SDK.verifyWithdrawalSignature(tampered, signature, body.l2_owner),
    ).toMatchObject({
      valid: false,
      reason: "invalid_signature",
    });
    expect(
      SDK.verifyWithdrawalSignature(
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
      SDK.verifyWithdrawalSignature(body, ["aa", "bb"], body.l2_owner),
    ).toEqual({
      valid: false,
      reason: "malformed_public_key",
    });
    const [publicKey] = SDK.signWithdrawalBody(privateKey, body);
    expect(
      SDK.verifyWithdrawalSignature(body, [publicKey, "bb"], body.l2_owner),
    ).toEqual({
      valid: false,
      reason: "malformed_signature",
    });
  });
});

describe("withdrawal CLI parsers", () => {
  it("parses event ids in canonical OutputReference CBOR form", () => {
    const eventId = Data.to(
      { transactionId: "aa".repeat(32), outputIndex: 2n },
      SDK.OutputReference,
    );
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
      SDK.withdrawalEventIdFromBuildMetadata({
        withdrawalAddress:
          "addr_test1vqz4js6k2c6un3h8y8sh2nmkg7u9s8w7up0psd4w6zv6r9usknpxf",
        withdrawalEventIdCbor: expectedEventId,
        withdrawalAuthUnit: "aa".repeat(28) + "bb".repeat(32),
        nonceInput,
        validTo: 123,
        inclusionTime: 456,
        lockedLovelace: 3_000_000n,
        orderOutputIndex: 0,
      }),
    ).toEqual(expectedEventId);
    expect(expectedEventId).not.toEqual(
      Data.to(selectedL2OutRef, SDK.OutputReference),
    );
  });

  it("parses absent and inline datum arguments", () => {
    const duplicatePairs = "a3020a010b020c";
    expect(
      plutusConstrFieldCbor(parseCardanoDatumCbor(duplicatePairs), [0]),
    ).toBe(duplicatePairs);
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
  it("reads a private mounted wallet seed without literal arguments and refuses conflicting or readable secrets", () => {
    const directory = mkdtempSync(join(tmpdir(), "midgard-r7-wallet-"));
    const path = join(directory, "wallet");
    try {
      writeFileSync(path, seedPhrase, { mode: 0o600 });
      expect(
        resolveWalletSeedPhrase({
          walletSeedPhraseFile: path,
          walletSeedPhraseEnv: "USER_WALLET",
          env: {},
        }),
      ).toEqual({ seedPhrase, resolvedFrom: "mounted-file" });
      expect(() =>
        resolveWalletSeedPhrase({
          walletSeedPhraseFile: path,
          walletSeedPhrase: seedPhrase,
          walletSeedPhraseEnv: "USER_WALLET",
        }),
      ).toThrow("one wallet secret source");
      chmodSync(path, 0o644);
      expect(() =>
        resolveWalletSeedPhrase({
          walletSeedPhraseFile: path,
          walletSeedPhraseEnv: "USER_WALLET",
        }),
      ).toThrow("E_PUBLIC_HTTP_CONFIG");
    } finally {
      rmSync(directory, { recursive: true, force: true });
    }
  });
});
