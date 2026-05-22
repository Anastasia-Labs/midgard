import { STATE_QUEUE_NODE_ASSET_NAME_PREFIX } from "@al-ft/midgard-sdk";
import { CML, type UTxO, walletFromSeed } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  resolveFraudulentHeaderHash,
  resolveProverSigner,
} from "../src/index.js";

const seedPhrase =
  "test test test test test test test test test test test junk";

const expectedSeedPaymentKeyHash = (): string =>
  CML.PrivateKey.from_bech32(
    walletFromSeed(seedPhrase, { network: "Preprod" }).paymentKey,
  )
    .to_public()
    .hash()
    .to_hex();

const makeUtxo = (assets: Record<string, bigint>): UTxO =>
  ({
    txHash: "aa".repeat(32),
    outputIndex: 0,
    address: "addr_test1vqz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3d7t0s7uqthvq7n",
    assets,
  }) as UTxO;

describe("submit-init signer resolution", () => {
  it("uses USER_WALLET as the default seed phrase source like submit-withdrawal", () => {
    const signer = resolveProverSigner(
      { network: "Preprod" },
      { USER_WALLET: seedPhrase },
    );

    expect(signer.source).toBe("USER_WALLET");
    expect(signer.address).toBe(
      walletFromSeed(seedPhrase, { network: "Preprod" }).address,
    );
    expect(signer.paymentKeyHash).toBe(expectedSeedPaymentKeyHash());
  });

  it("uses a direct seed phrase before the configured seed env var", () => {
    const signer = resolveProverSigner(
      {
        network: "Preprod",
        walletSeedPhrase: seedPhrase,
        walletSeedPhraseEnv: "OTHER_WALLET",
      },
      { OTHER_WALLET: "env env env env env env env env env env env env" },
    );

    expect(signer.source).toBe("direct-seed-phrase");
    expect(signer.paymentKeyHash).toBe(expectedSeedPaymentKeyHash());
  });

  it("accepts a direct private key even when USER_WALLET is present", () => {
    const privateKey = CML.PrivateKey.generate_ed25519();
    const signer = resolveProverSigner(
      {
        network: "Preprod",
        walletPrivateKey: privateKey.to_bech32(),
      },
      { USER_WALLET: seedPhrase },
    );

    expect(signer.source).toBe("direct-private-key");
    expect(signer.paymentKeyHash).toBe(privateKey.to_public().hash().to_hex());
    expect(signer.address).toContain("addr_test");
  });

  it("rejects ambiguous direct seed and private-key signer inputs", () => {
    expect(() =>
      resolveProverSigner(
        {
          network: "Preprod",
          walletSeedPhrase: seedPhrase,
          walletPrivateKey: CML.PrivateKey.generate_ed25519().to_bech32(),
        },
        {},
      ),
    ).toThrow("wallet seed phrase or a wallet private key");
  });
});

describe("submit-init state queue header resolution", () => {
  it("derives the fraudulent header hash from the selected state-queue block UTxO", () => {
    const stateQueuePolicyId = "11".repeat(28);
    const headerHash = "22".repeat(28);
    const unit = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

    expect(
      resolveFraudulentHeaderHash({
        stateQueuePolicyId,
        fraudulentBlockUtxo: makeUtxo({ lovelace: 5_000_000n, [unit]: 1n }),
      }),
    ).toBe(headerHash);
  });

  it("rejects a configured header hash that does not match the UTxO block token", () => {
    const stateQueuePolicyId = "33".repeat(28);
    const headerHash = "44".repeat(28);
    const unit = `${stateQueuePolicyId}${STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${headerHash}`;

    expect(() =>
      resolveFraudulentHeaderHash({
        stateQueuePolicyId,
        fraudulentBlockUtxo: makeUtxo({ [unit]: 1n }),
        configuredHeaderHash: "55".repeat(28),
      }),
    ).toThrow("--fraudulent-header-hash mismatch");
  });
});
