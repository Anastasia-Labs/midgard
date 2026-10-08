import http from "node:http";
import type { AddressInfo } from "node:net";

import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  Kupmios,
  type LucidEvolution,
  toUnit,
  TxSubmitError,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Cause, Effect, Runtime } from "effect";
import { expect, it, vi } from "vitest";

import {
  createPublishedWatcherBlockActor,
  isUnansweredSubmission,
  PublishedTransactionExpiredError,
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

vi.mock("midgard-node/transactions/register-active-operator", () => ({
  registerOperatorProgram: vi.fn(() => Effect.void),
  activateOperatorProgram: vi.fn(() => Effect.void),
}));

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
      : unit === toUnit(policy, SDK.SCHEDULER_ASSET_NAME)
        ? { ...anchor, datum: Data.to("NoActiveOperators", SDK.SchedulerDatum) }
        : anchor,
  ]);
  // The chain's clock, in step with the actor's polling.
  let now = 100_000;
  const order: string[] = [];
  const stop = new Error("stop before building");
  const lucid = {
    wallet: () => ({
      address: async () => address,
      getUtxos: async () => [anchor],
    }),
    utxosAtWithUnit,
    newTx: () => {
      order.push("newTx");
      throw stop;
    },
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
    chain: {
      now: () => now,
      delaySlots: vi.fn(async () => {
        now += 20_000;
      }),
      awaitLedgerTime: vi.fn(async (target: number) => {
        order.push(`awaitLedgerTime ${target}`);
      }),
    },
    contracts: {
      stateQueue: { ...contract, yields: { commit: {} } },
      activeOperators: contract,
      registeredOperators: contract,
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
  const headerUnit = toUnit(
    policy,
    SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + block.headerHash,
  );
  return {
    actor,
    anchor,
    block,
    submit,
    txHash,
    signedCbor,
    utxosAtWithUnit,
    headerUnit,
    order,
    stop,
    now: () => now,
  };
};

/**
 * What Lucid's `submit()` rejects with, from the real provider: an Ogmios
 * that never answers (the provider times out) or one that refuses the
 * transaction with a script failure.
 */
const lucidSubmitFailure = async (reply: "hang" | "refuse") => {
  const server = http.createServer((_request, response) => {
    if (reply === "hang") return;
    response.writeHead(200, { "content-type": "application/json" });
    response.end(
      JSON.stringify({
        jsonrpc: "2.0",
        method: "submitTransaction",
        error: { code: 3005, message: "script failed", data: [] },
        id: null,
      }),
    );
  });
  await new Promise<void>((resolve) => server.listen(0, "127.0.0.1", resolve));
  const { port } = server.address() as AddressInfo;
  const kupmios = new Kupmios(
    "http://127.0.0.1:1",
    `http://127.0.0.1:${port}`,
    { requestTimeoutMs: 100 },
  );
  try {
    return await Effect.runPromise(
      Effect.tryPromise({
        try: () => kupmios.submitTx("00"),
        catch: (cause) => new TxSubmitError({ cause }),
      }),
    ).then(
      () => {
        throw new Error("expected the submission to fail");
      },
      (error: unknown) => error,
    );
  } finally {
    server.closeAllConnections();
    server.close();
  }
};

/** The provider errors in a Lucid submit rejection, as `tag kind code`. */
const providerErrors = (error: unknown): string[] => {
  const found: string[] = [];
  const visit = (value: unknown): void => {
    if (typeof value !== "object" || value === null) return;
    const link = value as {
      _tag?: unknown;
      kind?: unknown;
      code?: unknown;
      cause?: unknown;
    };
    if (link._tag === "KupmiosError" || link._tag === "OgmiosJsonRpcError")
      found.push(
        `${String(link._tag)} ${String(link.kind)} ${String(link.code)}`,
      );
    if (Runtime.isFiberFailure(value))
      for (const failure of Cause.failures(value[Runtime.FiberFailureCauseId]))
        visit(failure);
    visit(link.cause);
  };
  visit(error);
  return found;
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

it("awaits the header when submission times out unanswered and it lands", async () => {
  const f = await fixture();
  const cause = await lucidSubmitFailure("hang");
  expect(providerErrors(cause)).toEqual(["KupmiosError timeout undefined"]);
  f.submit.mockRejectedValue(cause);
  await expect(f.actor.commit(f.block, f.anchor)).resolves.toBe(f.txHash);
});

it("fails closed as expired when an unanswered submission never lands", async () => {
  const f = await fixture();
  f.submit.mockRejectedValue(await lucidSubmitFailure("hang"));
  const reads = f.utxosAtWithUnit.getMockImplementation()!;
  f.utxosAtWithUnit.mockImplementation(async (address: string, unit: string) =>
    unit === f.headerUnit ? [] : reads(address, unit),
  );
  await expect(f.actor.commit(f.block, f.anchor)).rejects.toBeInstanceOf(
    PublishedTransactionExpiredError,
  );
});

it("keeps an Ogmios script refusal a submission error", async () => {
  const f = await fixture();
  const cause = await lucidSubmitFailure("refuse");
  expect(providerErrors(cause)).toEqual(["OgmiosJsonRpcError json_rpc 3005"]);
  expect(isUnansweredSubmission(cause)).toBe(false);
  f.submit.mockRejectedValue(cause);
  await expect(f.actor.commit(f.block, f.anchor)).rejects.toMatchObject({
    name: "PublishedTransactionSubmissionError",
    txHash: f.txHash,
    cause,
  });
});

it("waits for a fresh ledger tip before building a scheduler appointment", async () => {
  const f = await fixture();
  const at = f.now();
  await expect(f.actor.onboardOperator()).rejects.toBe(f.stop);
  const built = f.order.indexOf("newTx");
  expect(built).toBeGreaterThan(0);
  const wait = /^awaitLedgerTime (\d+)$/u.exec(f.order[built - 1]!);
  expect(wait).not.toBeNull();
  // A tip no more than thirty seconds old.
  expect(Number(wait![1])).toBeGreaterThanOrEqual(at - 30_000);
  expect(Number(wait![1])).toBeLessThanOrEqual(at);
});
