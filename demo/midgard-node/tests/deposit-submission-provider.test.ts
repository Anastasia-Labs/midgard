import { CML, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  depositDependenciesFromSignedTx,
  type DepositObservationRuntime,
  observePreparedDeposit,
  queryHistoricalDepositOutput,
} from "@/transactions/deposit-submission-provider.js";
import { submitPreparedDepositProgram } from "@/transactions/submit-deposit.js";

const SPEND_TX_HASH = "11".repeat(32);
const CHECKPOINT_HASH = "cc".repeat(32);

const signedDepositFixture = ({
  invalidBeforeSlot,
  invalidHereafterSlot,
}: {
  readonly invalidBeforeSlot?: number;
  readonly invalidHereafterSlot?: number;
} = {}) => {
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(SPEND_TX_HASH), 0n),
  );
  const outputs = CML.TransactionOutputList.new();
  const body = CML.TransactionBody.new(inputs, outputs, 0n);
  if (invalidBeforeSlot !== undefined) {
    body.set_validity_interval_start(BigInt(invalidBeforeSlot));
  }
  if (invalidHereafterSlot !== undefined) {
    body.set_ttl(BigInt(invalidHereafterSlot));
  }
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const txHash = CML.hash_transaction(body).to_hex();
  return {
    transaction,
    txHash,
    signedTxCbor: transaction.to_cbor_hex(),
    expectedDepositOutRef: `${txHash}#0`,
    dependencies: depositDependenciesFromSignedTx(transaction),
  };
};

const visibleDependency = (): UTxO =>
  ({ txHash: SPEND_TX_HASH, outputIndex: 0 }) as UTxO;

const runtime = (
  overrides: Partial<DepositObservationRuntime> = {},
): DepositObservationRuntime => ({
  queryHistoricalOutput: vi.fn(async () => ({
    kind: "absent" as const,
    kupoCheckpoint: 100,
    kupoCheckpointHash: CHECKPOINT_HASH,
  })),
  queryMempool: vi.fn(async () => ({ slot: 100, contains: false })),
  queryCanonicalPoint: vi.fn(async () => true),
  queryCurrentSlot: vi.fn(async () => ({
    source: "test" as const,
    currentSlot: 50,
    chainTipSlot: 100,
    observedAtMs: 1_800_000_000_000,
    slotLengthMs: 1_000,
  })),
  queryDependencies: vi.fn(async () => [visibleDependency()]),
  ...overrides,
});

describe("durable deposit provider evidence", () => {
  it("queries the exact historical Kupo outref without an unspent-only filter", async () => {
    const txHash = "33".repeat(32);
    const fetchImpl = vi.fn<
      (input: string | URL, init?: RequestInit) => Promise<Response>
    >(
      async () =>
        new Response(
          JSON.stringify([
            {
              transaction_id: txHash,
              output_index: 0,
              created_at: {
                slot_no: 90,
                header_hash: "44".repeat(32),
              },
              spent_at: {
                slot_no: 100,
                header_hash: "55".repeat(32),
              },
            },
          ]),
          {
            status: 200,
            headers: {
              "content-type": "application/json",
              "x-most-recent-checkpoint": "120",
              etag: `"${CHECKPOINT_HASH}"`,
            },
          },
        ),
    );

    await expect(
      queryHistoricalDepositOutput({
        kupoUrl: "http://127.0.0.1:1442",
        txHash,
        outputIndex: 0,
        fetchImpl: fetchImpl as never,
      }),
    ).resolves.toEqual({
      kind: "committed",
      slot: 90,
      blockHash: "44".repeat(32),
      kupoCheckpoint: 120,
      kupoCheckpointHash: CHECKPOINT_HASH,
    });
    expect(fetchImpl).toHaveBeenCalledTimes(1);
    const requestedUrl = fetchImpl.mock.calls[0]?.[0];
    expect(requestedUrl).toBe(`http://127.0.0.1:1442/matches/0%40${txHash}`);
    expect(requestedUrl).not.toContain("unspent");
  });

  it("recognizes a historical deposit as committed even after its output and dependencies are spent", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });
    const queryMempool = vi.fn<DepositObservationRuntime["queryMempool"]>();
    const queryDependencies =
      vi.fn<DepositObservationRuntime["queryDependencies"]>();
    const observationRuntime = runtime({
      queryHistoricalOutput: vi.fn(async () => ({
        kind: "committed" as const,
        slot: 80,
        blockHash: "aa".repeat(32),
        kupoCheckpoint: 100,
        kupoCheckpointHash: CHECKPOINT_HASH,
      })),
      queryMempool,
      queryDependencies,
    });

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: observationRuntime,
      }),
    ).resolves.toEqual({
      kind: "committed",
      slot: 80,
      blockHash: "aa".repeat(32),
      kupoCheckpoint: 100,
      kupoCheckpointHash: CHECKPOINT_HASH,
    });
    expect(queryMempool).not.toHaveBeenCalled();
    expect(queryDependencies).not.toHaveBeenCalled();
  });

  it("rejects a Kupo match whose creation point is not canonical in Ogmios", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });
    const queryCanonicalPoint = vi
      .fn<DepositObservationRuntime["queryCanonicalPoint"]>()
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(false);

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: runtime({
          queryHistoricalOutput: vi.fn(async () => ({
            kind: "committed" as const,
            slot: 80,
            blockHash: "aa".repeat(32),
            kupoCheckpoint: 100,
            kupoCheckpointHash: CHECKPOINT_HASH,
          })),
          queryCanonicalPoint,
        }),
      }),
    ).resolves.toEqual({
      kind: "ambiguous",
      reason: "Kupo deposit match is not on the canonical Ogmios chain",
    });
    expect(queryCanonicalPoint).toHaveBeenCalledTimes(2);
  });

  it("recognizes the exact transaction in a frozen mempool snapshot as accepted", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });
    const queryDependencies =
      vi.fn<DepositObservationRuntime["queryDependencies"]>();
    const observationRuntime = runtime({
      queryMempool: vi.fn(async () => ({ slot: 90, contains: true })),
      queryDependencies,
    });

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: observationRuntime,
      }),
    ).resolves.toEqual({ kind: "accepted", mempoolSlot: 90 });
    expect(queryDependencies).not.toHaveBeenCalled();
  });

  it("fails closed when Kupo has not synchronized through the mempool snapshot", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: runtime({
          queryHistoricalOutput: vi.fn(async () => ({
            kind: "absent" as const,
            kupoCheckpoint: 89,
            kupoCheckpointHash: CHECKPOINT_HASH,
          })),
          queryMempool: vi.fn(async () => ({ slot: 90, contains: false })),
        }),
      }),
    ).resolves.toEqual({
      kind: "ambiguous",
      reason: "Kupo checkpoint 89 is behind Ogmios mempool slot 90",
    });
  });

  it("fails closed when Kupo absence does not cover the final authoritative chain tip", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: runtime({
          queryCurrentSlot: vi.fn(async () => ({
            source: "test" as const,
            currentSlot: 102,
            chainTipSlot: 101,
            observedAtMs: 1_800_000_000_000,
            slotLengthMs: 1_000,
          })),
        }),
      }),
    ).resolves.toEqual({
      kind: "ambiguous",
      reason:
        "Kupo checkpoint 100 is behind authoritative Ogmios chain tip 101",
    });
  });

  it("fails closed before provider queries when stored dependencies differ from the signed body", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });
    const queryHistoricalOutput =
      vi.fn<DepositObservationRuntime["queryHistoricalOutput"]>();

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: {
          spend: [`${"22".repeat(32)}#0`],
          collateral: [],
          reference: [],
        },
        runtime: runtime({ queryHistoricalOutput }),
      }),
    ).resolves.toEqual({
      kind: "ambiguous",
      reason: "stored dependency set does not match the exact signed body",
    });
    expect(queryHistoricalOutput).not.toHaveBeenCalled();
  });

  it("fails closed when any signed transaction dependency is no longer unspent", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: runtime({ queryDependencies: vi.fn(async () => []) }),
      }),
    ).resolves.toEqual({
      kind: "ambiguous",
      reason: `signed transaction dependencies are no longer all unspent: ${SPEND_TX_HASH}#0`,
    });
  });

  it("allows one initial claim only after synchronized absence and exact dependency visibility", async () => {
    const fixture = signedDepositFixture({
      invalidBeforeSlot: 40,
      invalidHereafterSlot: 100,
    });
    const jsonbOrderedDependencies = JSON.parse(
      JSON.stringify({
        reference: fixture.dependencies.reference,
        collateral: fixture.dependencies.collateral,
        spend: fixture.dependencies.spend,
      }),
    ) as typeof fixture.dependencies;

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: jsonbOrderedDependencies,
        runtime: runtime(),
      }),
    ).resolves.toEqual({
      kind: "absent_safe",
      mempoolSlot: 100,
      kupoCheckpoint: 100,
      currentSlot: 50,
    });
  });

  it("marks an absent exact transaction expired at the signed validity margin", async () => {
    const fixture = signedDepositFixture({ invalidHereafterSlot: 100 });

    await expect(
      observePreparedDeposit({
        ...fixture,
        storedDependencies: fixture.dependencies,
        runtime: runtime({
          queryCurrentSlot: vi.fn(async () => ({
            source: "test" as const,
            currentSlot: 98,
            chainTipSlot: 100,
            observedAtMs: 1_800_000_000_000,
            slotLengthMs: 1_000,
          })),
        }),
      }),
    ).resolves.toEqual({
      kind: "expired",
      mempoolSlot: 100,
      kupoCheckpoint: 100,
      currentSlot: 98,
      invalidHereafterSlot: 100,
    });
  });
});

describe("exact prepared deposit submission", () => {
  const fakeLucid = (submitTx: (cbor: string) => Promise<string>) =>
    ({
      config: () => ({ provider: { submitTx } }),
      wallet: () => ({ address: async () => "addr_test1prepared" }),
    }) as never;

  it("submits the exact persisted bytes once", async () => {
    const fixture = signedDepositFixture();
    const submitTx = vi.fn(async () => fixture.txHash);

    await expect(
      Effect.runPromise(
        submitPreparedDepositProgram(fakeLucid(submitTx), fixture),
      ),
    ).resolves.toMatchObject({
      txHash: fixture.txHash,
      signedTxCbor: fixture.signedTxCbor,
      providerTxHash: fixture.txHash,
    });
    expect(submitTx).toHaveBeenCalledTimes(1);
    expect(submitTx).toHaveBeenCalledWith(fixture.signedTxCbor);
  });

  it("rejects a provider hash mismatch without resubmitting", async () => {
    const fixture = signedDepositFixture();
    const submitTx = vi.fn(async () => "ff".repeat(32));

    const result = await Effect.runPromise(
      Effect.either(
        submitPreparedDepositProgram(fakeLucid(submitTx), fixture, {
          sleep: () => Effect.void,
        }),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitTx).toHaveBeenCalledTimes(1);
    expect(submitTx).toHaveBeenCalledWith(fixture.signedTxCbor);
  });

  it("does not retry an OutsideValidityInterval response after the one durable claim", async () => {
    const fixture = signedDepositFixture();
    const submitTx = vi.fn(async () => {
      throw new Error(
        "OutsideValidityIntervalUTxO (ValidityInterval {invalidBefore = SJust (SlotNo 10), invalidHereafter = SJust (SlotNo 20)}) (SlotNo 7)",
      );
    });

    const result = await Effect.runPromise(
      Effect.either(
        submitPreparedDepositProgram(fakeLucid(submitTx), fixture, {
          sleep: () => Effect.void,
        }),
      ),
    );

    expect(result._tag).toBe("Left");
    expect(submitTx).toHaveBeenCalledTimes(1);
    expect(submitTx).toHaveBeenCalledWith(fixture.signedTxCbor);
  });
});
