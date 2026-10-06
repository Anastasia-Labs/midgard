import { mkdtempSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { openAvailabilityOperationJournal } from "@al-ft/midgard-core/availability-operation-journal";
import {
  buildDaAvailabilityFundingPreparationTx,
  type DaAvailabilityOperationContext,
  type DaAvailabilityOperationObservation,
  reconcileDaAvailabilityOperations,
  runDaAvailabilityOperation,
} from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
  paymentCredentialOf,
  type UTxO,
} from "@lucid-evolution/lucid";
import { expect, vi } from "vitest";

/**
 * A real journal and real signed preparation intents over an emulator wallet;
 * the canonical observation is injected at the provider seam because the
 * emulator cannot roll back.
 */
export const MIN_DEPTH = 30;
/** Each fixture's journal directory, for the test file to remove. */
export const dirs: string[] = [];
const hashOf = (cbor: string) =>
  CML.hash_transaction(CML.Transaction.from_cbor_hex(cbor).body()).to_hex();

export const fixture = async () => {
  const account = generateEmulatorAccount({ lovelace: 100_000_000n });
  // Two genesis coins of one wallet, so two intents share no input.
  const emulator = new Emulator([account, account]);
  emulator.awaitBlock(5);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const dir = mkdtempSync(join(tmpdir(), "availability-rollback-"));
  dirs.push(dir);
  const journal = openAvailabilityOperationJournal(join(dir, "journal.sqlite"));
  const coins = await lucid.wallet().getUtxos();
  expect(coins).toHaveLength(2);
  let observation: (
    txHash: string,
  ) => DaAvailabilityOperationObservation = () => ({
    status: "unknown",
    reason: "unobserved",
  });
  const submitted: string[] = [];
  let blockNo = 100;
  const context: DaAvailabilityOperationContext = {
    deploymentIdentity: "aa".repeat(32),
    actor: paymentCredentialOf(account.address).hash,
    journal,
    stateQueuePolicyId: "cc".repeat(28),
    minimumConfirmationDepth: MIN_DEPTH,
    transactionLimits: {
      maxTxSize: 16384,
      maxTxExMem: 16500000n,
      maxTxExSteps: 10000000000n,
      coinsPerUtxoByte: 4310n,
      feeCeilings: { prepare: 1000000n },
    },
    assertActuationCurrent: () => {},
    readBoundary: async () => ({ blockNo }),
    observe: async (intent) => observation(intent.txHash),
    submit: async (bytes) => {
      submitted.push(bytes);
      return hashOf(bytes);
    },
  };
  const build = vi.fn<(headerHash: string) => void>();
  const prepare = async (
    headerHash: string,
    fundingInput: UTxO,
    outputLovelace = 50_000_000n,
  ) => {
    const result = await runDaAvailabilityOperation(context, {
      action: "prepare",
      headerHash,
      build: async () => {
        build(headerHash);
        return buildDaAvailabilityFundingPreparationTx(lucid, {
          fundingInput,
          outputLovelace,
          feeLovelace: 1_000_000n,
          validFrom: BigInt(emulator.now() - 60_000),
          validTo: BigInt(emulator.now() + 60_000),
        });
      },
    });
    return journal.findTransaction(result.txHash)!;
  };
  return {
    lucid,
    emulator,
    context,
    journal,
    coins,
    build,
    submitted,
    prepare,
    boundary: (height: number) => {
      blockNo = height;
    },
    observe: (next: (txHash: string) => DaAvailabilityOperationObservation) => {
      observation = next;
    },
  };
};
export type Fixture = Awaited<ReturnType<typeof fixture>>;
export const included = (
  txHash: string,
  confirmationDepth: number,
  currentSlot?: number,
): DaAvailabilityOperationObservation => ({
  status: "included",
  txHash,
  inclusionPoint: `${txHash.slice(0, 8)}-block`,
  confirmationDepth,
  currentBlockNo: 100,
  ...(currentSlot === undefined ? {} : { currentSlot }),
});
export const confirm = async (f: Fixture) => {
  f.observe((txHash) => included(txHash, MIN_DEPTH, 0));
  await reconcileDaAvailabilityOperations(f.context);
};
