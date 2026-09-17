import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it, vi } from "vitest";

import {
  createPublishedWatcherBlockActor,
  PublishedTransactionSubmissionError,
  type PublishedWatcherBlock,
  type PublishedWatcherDeployment,
} from "./support/published-block-actor.js";

vi.mock("@al-ft/midgard-sdk", async (importOriginal) => {
  const actual = await importOriginal<typeof SDK>();
  return {
    ...actual,
    incompleteEmulatorCommitBlockHeaderTxProgram: vi.fn(),
    utxoToStateQueueUTxO: vi.fn(() => Effect.succeed({})),
  };
});

const fixture = async () => {
  const operator = "ab".repeat(28);
  const address = credentialToAddress("Preprod", {
    type: "Key",
    hash: operator,
  });
  const policy = "bc".repeat(28);
  const anchor: UTxO = {
    txHash: "cd".repeat(32),
    outputIndex: 0,
    address,
    assets: { lovelace: 5_000_000n },
  };
  const transaction = CML.Transaction.new(
    CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      200_000n,
    ),
    CML.TransactionWitnessSet.new(),
    true,
  );
  const signedCbor = transaction.to_cbor_hex();
  const txHash = CML.hash_transaction(transaction.body()).to_hex();
  const submit = vi.fn(async () => txHash);
  const builder = {
    complete: vi.fn(async () => ({
      sign: {
        withWallet: () => ({
          complete: async () => ({ toCBOR: () => signedCbor, submit }),
        }),
      },
    })),
  };
  vi.mocked(SDK.incompleteEmulatorCommitBlockHeaderTxProgram).mockReturnValue(
    Effect.succeed(builder) as unknown as ReturnType<
      typeof SDK.incompleteEmulatorCommitBlockHeaderTxProgram
    >,
  );
  const utxosAtWithUnit = vi.fn(async (_address: string, unit: string) => [
    unit === toUnit(policy, SDK.CORRECTION_LOCK_ASSET_NAME)
      ? { ...anchor, datum: Data.to("Idle", SDK.CorrectionLockDatum) }
      : anchor,
  ]);
  const lucid = {
    wallet: () => ({
      address: async () => address,
      getUtxos: async () => [anchor],
    }),
    utxosAtWithUnit,
    utxosAt: async () => [anchor],
    overrideUTxOs: vi.fn(),
  } as unknown as LucidEvolution;
  const contract = { policyId: policy, spendingScriptAddress: address };
  const deployment = {
    publisherLucid: lucid,
    references: new Map(
      [
        "stateQueueSpend",
        "stateQueueMint",
        "activeOperatorsSpend",
        "stateQueueCommitWithdraw",
      ].map((name) => [name, anchor]),
    ),
    chain: { now: () => 100_000, delaySlots: vi.fn() },
    contracts: {
      stateQueue: { ...contract, yields: { commit: {} } },
      activeOperators: contract,
      scheduler: contract,
      hubOracle: contract,
      correctionLock: contract,
    },
  } as unknown as PublishedWatcherDeployment;
  const actor = await createPublishedWatcherBlockActor({
    deployment,
    lucid,
    daSignerConfig: {} as never,
  });
  const block = {
    header: { endTime: 120_000n },
    headerHash: "de".repeat(28),
  } as PublishedWatcherBlock;
  return { actor, anchor, block, submit, txHash, signedCbor };
};

it("awaits persistence of the exact signed attempt before submission", async () => {
  const f = await fixture();
  let persisted = false;
  const onSigned = vi.fn(async (attempt) => {
    expect(attempt).toEqual({ txHash: f.txHash, signedCbor: f.signedCbor });
    expect(f.submit).not.toHaveBeenCalled();
    await Promise.resolve();
    persisted = true;
  });
  f.submit.mockImplementation(async () => {
    expect(persisted).toBe(true);
    return f.txHash;
  });

  await expect(
    f.actor.commit(f.block, f.anchor, undefined, onSigned),
  ).resolves.toBe(f.txHash);
  expect(onSigned).toHaveBeenCalledOnce();
  expect(f.submit).toHaveBeenCalledOnce();
});

it("preserves the signed transaction identity and cause when submission rejects", async () => {
  const f = await fixture();
  const cause = new Error("All inputs are spent");
  f.submit.mockRejectedValue(cause);
  const onSigned = vi.fn(async () => {});

  const error = await f.actor
    .commit(f.block, f.anchor, undefined, onSigned)
    .catch((error: unknown) => error);
  expect(error).toBeInstanceOf(PublishedTransactionSubmissionError);
  expect(error).toMatchObject({ txHash: f.txHash, cause });
  expect(onSigned).toHaveBeenCalledExactlyOnceWith({
    txHash: f.txHash,
    signedCbor: f.signedCbor,
  });
  expect(f.submit).toHaveBeenCalledOnce();
});

it("does not submit or classify checkpoint persistence failure as a submission error", async () => {
  const f = await fixture();
  const cause = new Error("Checkpoint write failed");
  await expect(
    f.actor.commit(f.block, f.anchor, undefined, async () => {
      throw cause;
    }),
  ).rejects.toBe(cause);
  expect(f.submit).not.toHaveBeenCalled();
});

it("keeps a returned transaction hash mismatch as a hard error", async () => {
  const f = await fixture();
  f.submit.mockResolvedValue("ef".repeat(32));
  const error = await f.actor
    .commit(f.block, f.anchor)
    .catch((error: unknown) => error);
  expect(error).toBeInstanceOf(Error);
  expect(error).not.toBeInstanceOf(PublishedTransactionSubmissionError);
  expect(error).toMatchObject({
    message: "Submitted header hash differs from its signed transaction",
  });
});
