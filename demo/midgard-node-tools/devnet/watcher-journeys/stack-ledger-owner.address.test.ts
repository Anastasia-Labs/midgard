import { mkdtemp, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  assetsToValue,
  CML,
  credentialToAddress,
  generateSeedPhrase,
  paymentCredentialOf,
  walletFromSeed,
} from "@lucid-evolution/lucid";
import {
  makeMidgardTxOutput,
  makeOutRefCbor,
} from "midgard-node/tests/midgard-output-helpers";
import { afterAll, describe, expect, it } from "vitest";

import type { JourneyLedger } from "../../src/full-stack/journey.js";
import { ownedHistoryLedgerEntries } from "./history-cases.js";
import { headerOn, honestOperator } from "./honest-successor.test-support.js";
import {
  type ReplayedPredecessor,
  successorTransfer,
} from "./honest-successor.transfer.js";
import type { JourneyLedgerPredecessor } from "./journey-ledger.js";
import {
  ourLedgerHoldsLedgerOwnerOutput,
  requireStackLedgerOwnerFunded,
  stackLedgerOwnerDepositAddress,
  StackLedgerOwnerUnfundedRefusal,
} from "./stack-ledger-owner.js";

const directories: string[] = [];
afterAll(async () => {
  await Promise.all(
    directories.map((directory) =>
      rm(directory, { recursive: true, force: true }),
    ),
  );
});

/** The drill's ledger owner, its wallet address as the stack context reads it. */
const ledgerOwner = () => {
  const seedPhrase = generateSeedPhrase();
  const { address } = walletFromSeed(seedPhrase, { network: "Preprod" });
  return { seedPhrase, address, keyHash: paymentCredentialOf(address).hash };
};

/** One L2 output of `lovelace` at `address`, as our node's ledger holds it. */
const nodeOutput = (address: string, txHash: string, lovelace: bigint) => ({
  txHash,
  outputIndex: 0,
  outrefCbor: makeOutRefCbor(txHash, 0),
  outputCbor: Buffer.from(
    makeMidgardTxOutput(
      CML.Address.from_bech32(address),
      assetsToValue({ lovelace }),
    ).to_cbor_bytes(),
  ),
  address,
  assets: { lovelace },
});

/**
 * Our node's ledger holding `outputs`: like its `/utxos`, it answers only
 * the outputs whose address text is exactly the one asked for.
 */
const exactAddressLedger = (
  outputs: readonly ReturnType<typeof nodeOutput>[],
): JourneyLedger => ({
  utxos: (address) =>
    Promise.resolve(outputs.filter((output) => output.address === address)),
  submitTransfer: () => Promise.reject(new Error("not sent in this test")),
});

/** An attested tail whose ledger holds `outputs`. */
const tailHolding = async (
  outputs: readonly ReturnType<typeof nodeOutput>[],
) => ({
  ...(await headerOn("00".repeat(28), honestOperator, 1n)),
  ledger: {
    entries: outputs.map(({ outrefCbor, outputCbor }) => ({
      outRef: outrefCbor,
      output: outputCbor,
    })),
  },
});

describe("the ledger owner's funding deposit on an attached stack", () => {
  it("pays the address the honest successor's transfer spends from", async () => {
    const owner = ledgerOwner();
    const deposited = nodeOutput(
      stackLedgerOwnerDepositAddress(owner),
      "44".repeat(32),
      50_000_000n,
    );
    const ledger = exactAddressLedger([deposited]);
    const predecessor = (await tailHolding([
      deposited,
    ])) satisfies ReplayedPredecessor;
    // Every family still finds it by the owner's payment key.
    expect(
      ownedHistoryLedgerEntries(
        predecessor as unknown as JourneyLedgerPredecessor,
        owner.keyHash,
      ),
    ).toHaveLength(1);
    const directory = await mkdtemp(join(tmpdir(), "ledger-owner-address-"));
    directories.push(directory);
    // The honest successor's transfer, built on our node's ledger from the
    // inputs it reads at the owner's address; never sent here.
    const intent = await successorTransfer({
      directory,
      nodeEndpoint: "http://127.0.0.1:1",
      ledger,
      ledgerOwner: owner,
      network: "Preprod",
      transferLovelace: 5_000_000n,
    }).build(predecessor);
    expect(intent.predecessorHeaderHash).toBe(predecessor.headerHash);
  });

  it("does not count an output at the owner's key-only address as funded", async () => {
    const owner = ledgerOwner();
    const stale = nodeOutput(
      credentialToAddress("Preprod", { type: "Key", hash: owner.keyHash }),
      "46".repeat(32),
      50_000_000n,
    );
    expect(stale.address).not.toBe(owner.address);
    await expect(
      ourLedgerHoldsLedgerOwnerOutput(exactAddressLedger([stale]), owner),
    ).resolves.toBe(false);
    const tail = (await tailHolding([
      stale,
    ])) as unknown as JourneyLedgerPredecessor;
    expect(() => requireStackLedgerOwnerFunded(tail, owner)).toThrow(
      StackLedgerOwnerUnfundedRefusal,
    );
  });

  it("counts an output at the owner's wallet address as funded", async () => {
    const owner = ledgerOwner();
    const held = nodeOutput(owner.address, "47".repeat(32), 50_000_000n);
    await expect(
      ourLedgerHoldsLedgerOwnerOutput(exactAddressLedger([held]), owner),
    ).resolves.toBe(true);
    const tail = (await tailHolding([
      held,
    ])) as unknown as JourneyLedgerPredecessor;
    expect(() => requireStackLedgerOwnerFunded(tail, owner)).not.toThrow();
  });
});
