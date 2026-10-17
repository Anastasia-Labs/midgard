import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  encodeMidgardAddressText,
  midgardAddressFromText,
  protectMidgardAddress,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { type QueuedTx } from "@al-ft/midgard-validation";
import { assetsToValue, CML, walletFromSeed } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DEFAULT_WALLET_SEED_ENV,
  type NodeUtxo,
  resolveWalletSeedPhrase,
} from "../src/commands/command-utils.js";
import {
  makeStaticMidgardProvider,
  parseSubmitL2TransferConfig,
} from "../src/commands/submit-l2-transfer.js";
import { ContractDeploymentIdentity } from "../src/services/midgard-contracts.js";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "./midgard-output-helpers.js";

export const TEST_SEED =
  "cupboard digital guitar diesel critic will afford salon game dolphin phrase baby dad urban machine barely rack acoustic blood vote misery enemy salute depart";

export const OTHER_TEST_SEED =
  "panther fly crawl express smile lend company blue slogan dawn wall tip angle tomorrow battle myth category vanish misery ocean include salon wood rail";

export const launchDeploymentIdentity = ContractDeploymentIdentity.make({
  kind: "derived" as const,
  consensusProfile: MIDGARD_CONSENSUS_PROFILE,
});

export const mkQueued = (txId: Buffer, txCbor: Buffer): QueuedTx => ({
  txId,
  txCbor,
  programMaterialSidecarCbor: encodeMidgardCekProgramMaterialSidecar([]),
  arrivalSeq: 0n,
  createdAt: new Date(0),
});

export const mkNodeUtxo = ({
  txHash,
  outputIndex,
  address,
  assets,
}: {
  readonly txHash: string;
  readonly outputIndex: number;
  readonly address: string;
  readonly assets: { readonly [unit: string]: bigint };
}): NodeUtxo => {
  const outrefCbor = makeOutRefCbor(txHash, outputIndex);
  const outputCbor = Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      assetsToValue(assets),
    ).to_cbor_bytes(),
  );
  return {
    txHash,
    outputIndex,
    outrefCbor,
    outputCbor,
    address,
    assets,
  };
};

describe("submit-l2-transfer config helpers", () => {
  it("preserves a bounded lower submit cap in the static provider", async () => {
    const maxSubmitTxCborBytes =
      MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes - 1;
    const provider = makeStaticMidgardProvider({
      address: "addr_test1static",
      utxos: [],
      network: "Preprod",
      networkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      maxSubmitTxCborBytes,
    });

    await expect(provider.getProtocolInfo()).resolves.toMatchObject({
      submissionLimits: { maxSubmitTxCborBytes },
    });
  });

  it("parses a valid config and derives a normalized endpoint", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const config = parseSubmitL2TransferConfig({
      l2Address: ` ${wallet.address} `,
      lovelace: "5000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000/",
    });

    expect(config.l2Address).toBe(wallet.address);
    expect(config.lovelace).toBe(5_000_000n);
    expect(config.nodeEndpoint).toBe("http://127.0.0.1:3000");
    expect(config.networkId).toBe(0n);
  });

  it("parses protected Midgard destination addresses with the Midgard codec", () => {
    const wallet = walletFromSeed(TEST_SEED, { network: "Preprod" });
    const protectedAddress = encodeMidgardAddressText(
      protectMidgardAddress(midgardAddressFromText(wallet.address)),
    );

    const config = parseSubmitL2TransferConfig({
      l2Address: ` ${protectedAddress} `,
      lovelace: "5000000",
      assetSpecs: [],
      nodeEndpoint: "http://127.0.0.1:3000/",
    });

    expect(config.l2Address).toBe(protectedAddress);
    expect(config.networkId).toBe(0n);
  });

  it("resolves direct input or USER_WALLET without legacy fallback", () => {
    const direct = resolveWalletSeedPhrase({
      walletSeedPhrase: TEST_SEED,
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {},
    });
    expect(direct.resolvedFrom).toBe("direct-argument");

    const envSeed = resolveWalletSeedPhrase({
      walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
      env: {
        [DEFAULT_WALLET_SEED_ENV]: TEST_SEED,
      },
    });
    expect(envSeed.resolvedFrom).toBe(DEFAULT_WALLET_SEED_ENV);
    expect(envSeed.seedPhrase).toBe(TEST_SEED);

    expect(() =>
      resolveWalletSeedPhrase({
        walletSeedPhraseEnv: DEFAULT_WALLET_SEED_ENV,
        env: {
          USER_SEED_PHRASE: TEST_SEED,
        },
      }),
    ).toThrow(
      `Environment variable "${DEFAULT_WALLET_SEED_ENV}" does not contain a wallet seed phrase.`,
    );
  });
});
