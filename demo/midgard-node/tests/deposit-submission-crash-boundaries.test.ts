import { randomBytes } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect, Fiber, Option } from "effect";
import { describe, expect, it, vi } from "vitest";

import { DepositSubmissionAttemptsDB } from "@/database/index.js";
import { MidgardContracts } from "@/services/midgard-contracts.js";
import { Lucid as LucidService } from "@/services/lucid.js";
import {
  depositDependenciesFromSignedTx,
  observePreparedDeposit,
} from "@/transactions/deposit-submission-provider.js";
import {
  reconcileOpenDepositSubmissionAttemptsProgram,
  reconcileDepositSubmissionAttemptProgram,
  resumeDepositSubmissionAttemptProgram,
  type DepositSubmissionObservationReader,
} from "@/transactions/submit-deposit.js";

import { provideDatabaseLayers } from "./utils.js";

type PromiseLatch = {
  readonly promise: Promise<void>;
  readonly resolve: () => void;
};

const promiseLatch = (): PromiseLatch => {
  let resolve!: () => void;
  const promise = new Promise<void>((complete) => {
    resolve = complete;
  });
  return { promise, resolve };
};

const withTimeout = async <A>(promise: Promise<A>): Promise<A> =>
  Promise.race([
    promise,
    new Promise<never>((_, reject) => {
      setTimeout(
        () => reject(new Error("timed out at deposit crash boundary")),
        5_000,
      );
    }),
  ]);

const signedTransactionFixture = () => {
  const inputTxHash = randomBytes(32).toString("hex");
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(CML.TransactionHash.from_hex(inputTxHash), 0n),
  );
  const body = CML.TransactionBody.new(
    inputs,
    CML.TransactionOutputList.new(),
    0n,
  );
  const transaction = CML.Transaction.new(
    body,
    CML.TransactionWitnessSet.new(),
    true,
    undefined,
  );
  const txHash = CML.hash_transaction(body).to_hex();
  const signedTxCbor = transaction.to_cbor_hex();
  const eventId = randomBytes(39);
  const attempt: DepositSubmissionAttemptsDB.InsertPreparedInput = {
    [DepositSubmissionAttemptsDB.Columns.TX_HASH]: Buffer.from(txHash, "hex"),
    [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: eventId,
    [DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR]: Buffer.from(
      signedTxCbor,
      "hex",
    ),
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]:
      `${txHash}#0`,
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]:
      "addr_test1_crash_boundary",
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]: "1000000",
    [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]: {
      lovelace: "1000000",
    },
    [DepositSubmissionAttemptsDB.Columns.METADATA]: {
      depositAddress: "addr_test1_crash_boundary_deposit",
      depositEventId: eventId.toString("hex"),
      depositAssetName: "00".repeat(32),
      depositAuthUnit: `${"11".repeat(28)}${"00".repeat(32)}`,
      nonceInput: { txHash: inputTxHash, outputIndex: 0 },
      validTo: 1_800_000_000_000,
      inclusionTime: 1_800_000_060_000,
    },
    [DepositSubmissionAttemptsDB.Columns.DEPENDENCY_OUT_REFS]: {
      spend: [`${inputTxHash}#0`],
      collateral: [],
      reference: [],
    },
  };
  return { attempt, txHash, signedTxCbor };
};

const absentObservation: DepositSubmissionObservationReader = async () => ({
  kind: "absent_safe",
  mempoolSlot: 100,
  kupoCheckpoint: 100,
  currentSlot: 100,
});

const fakeLucidService = (
  submitTx: (signedTxCbor: string) => Promise<string>,
) => {
  const api = {
    config: () => ({ provider: { submitTx } }),
    wallet: () => ({ address: async () => "addr_test1_crash_boundary" }),
    awaitTx: vi.fn(async () => true),
  };
  return {
    api,
    referenceScriptsApi: api,
    operatorMainAddress: "addr_test1_crash_boundary",
    operatorMergeAddress: "addr_test1_crash_boundary_merge",
    referenceScriptsWalletAddress: "addr_test1_crash_boundary_reference",
    referenceScriptsAddress: "addr_test1_crash_boundary_reference",
    submitSlotSnapshot: () =>
      Effect.succeed({
        source: "test" as const,
        currentSlot: 100,
        observedAtMs: 1_800_000_000_000,
        slotLengthMs: 1_000,
      }),
    switchToOperatorsMainWallet: Effect.void,
    switchToOperatorsMergingWallet: Effect.void,
    switchToReferenceScriptWallet: Effect.void,
  };
};

const withRequiredServices = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
  submitTx: (signedTxCbor: string) => Promise<string>,
) =>
  provideDatabaseLayers(
    effect.pipe(
      Effect.provideService(LucidService, fakeLucidService(submitTx) as never),
      Effect.provideService(MidgardContracts, {} as never),
    ),
  );

const deleteAttempts = (txHashes: readonly Buffer[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const txHash of txHashes) {
      yield* sql`DELETE FROM deposit_submission_attempts
        WHERE tx_hash = ${txHash}`;
    }
  }).pipe(Effect.orDie);

describe.sequential("durable deposit crash boundaries", () => {
  it("does not classify expiry when dependency evidence is absent", async () => {
    const fixture = signedTransactionFixture();
    const transaction = CML.Transaction.from_cbor_hex(fixture.signedTxCbor);
    const expiringBody = transaction.body();
    expiringBody.set_ttl(100n);
    const expiringTransaction = CML.Transaction.new(
      expiringBody,
      transaction.witness_set(),
      true,
      undefined,
    );
    const signedTxCbor = expiringTransaction.to_cbor_hex();
    const txHash = CML.hash_transaction(expiringBody).to_hex();

    await expect(
      observePreparedDeposit({
        txHash,
        signedTxCbor,
        expectedDepositOutRef: `${txHash}#0`,
        storedDependencies:
          depositDependenciesFromSignedTx(expiringTransaction),
        runtime: {
          queryHistoricalOutput: async () => ({
            kind: "absent",
            kupoCheckpoint: 100,
            kupoCheckpointHash: "aa".repeat(32),
          }),
          queryMempool: async () => ({ slot: 100, contains: false }),
          queryCanonicalPoint: async () => true,
          queryCurrentSlot: async () => ({
            source: "test",
            currentSlot: 98,
            chainTipSlot: 100,
            observedAtMs: 1_800_000_000_000,
            slotLengthMs: 1_000,
          }),
          queryDependencies: async () => [],
        },
      }),
    ).resolves.toEqual({
      kind: "ambiguous",
      reason: expect.stringContaining(
        "signed transaction dependencies are no longer all unspent",
      ),
    });
  });

  it("durably prepares exact bytes before one concurrent claimant can own submission", async () => {
    const fixture = signedTransactionFixture();
    const txHashBuffer =
      fixture.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH];

    await Effect.runPromise(
      withRequiredServices(
        Effect.gen(function* () {
          const prepared = yield* DepositSubmissionAttemptsDB.insertPrepared(
            fixture.attempt,
          );
          expect(prepared[DepositSubmissionAttemptsDB.Columns.STATUS]).toBe(
            DepositSubmissionAttemptsDB.Status.Prepared,
          );
          expect(
            prepared[
              DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR
            ].toString("hex"),
          ).toBe(fixture.signedTxCbor);
          expect(
            prepared[DepositSubmissionAttemptsDB.Columns.ATTEMPT_COUNT],
          ).toBe(0);

          const claims = yield* Effect.all(
            [
              DepositSubmissionAttemptsDB.beginSubmission(txHashBuffer).pipe(
                Effect.either,
              ),
              DepositSubmissionAttemptsDB.beginSubmission(txHashBuffer).pipe(
                Effect.either,
              ),
            ],
            { concurrency: "unbounded" },
          );
          expect(claims.filter((claim) => claim._tag === "Right")).toHaveLength(
            1,
          );
          expect(claims.filter((claim) => claim._tag === "Left")).toHaveLength(
            1,
          );

          const claimed = Option.getOrThrow(
            yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHashBuffer),
          );
          expect(claimed[DepositSubmissionAttemptsDB.Columns.STATUS]).toBe(
            DepositSubmissionAttemptsDB.Status.SubmissionUnknown,
          );
          expect(
            claimed[DepositSubmissionAttemptsDB.Columns.ATTEMPT_COUNT],
          ).toBe(1);
        }).pipe(Effect.ensuring(deleteAttempts([txHashBuffer]))),
        async () => fixture.txHash,
      ),
    );
  });

  it("claims before the provider call, round-trips exact DB bytes, and never resubmits after response loss", async () => {
    const fixture = signedTransactionFixture();
    const txHashBuffer =
      fixture.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH];
    const providerEntered = promiseLatch();
    const releaseProvider = promiseLatch();
    const submittedBytes: string[] = [];
    const submitTx = vi.fn(async (signedTxCbor: string) => {
      submittedBytes.push(signedTxCbor);
      providerEntered.resolve();
      await releaseProvider.promise;
      throw new Error("provider response lost after accepting exact bytes");
    });

    await Effect.runPromise(
      withRequiredServices(
        Effect.gen(function* () {
          yield* DepositSubmissionAttemptsDB.insertPrepared(fixture.attempt);
          expect(submitTx).not.toHaveBeenCalled();

          const recoveryFiber = yield* Effect.fork(
            resumeDepositSubmissionAttemptProgram(fixture.txHash, {
              observe: absentObservation,
              submitRecovery: { sleep: () => Effect.void },
            }),
          );
          yield* Effect.promise(() => withTimeout(providerEntered.promise));

          const duringProviderCall = Option.getOrThrow(
            yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHashBuffer),
          );
          expect(
            duringProviderCall[DepositSubmissionAttemptsDB.Columns.STATUS],
          ).toBe(DepositSubmissionAttemptsDB.Status.SubmissionUnknown);
          expect(
            duringProviderCall[
              DepositSubmissionAttemptsDB.Columns.ATTEMPT_COUNT
            ],
          ).toBe(1);
          expect(
            duringProviderCall[
              DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR
            ].toString("hex"),
          ).toBe(fixture.signedTxCbor);
          expect(submittedBytes).toEqual([fixture.signedTxCbor]);

          releaseProvider.resolve();
          expect(
            (yield* Fiber.join(recoveryFiber).pipe(Effect.either))._tag,
          ).toBe("Left");

          const afterResponseLoss = Option.getOrThrow(
            yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHashBuffer),
          );
          expect(
            afterResponseLoss[DepositSubmissionAttemptsDB.Columns.STATUS],
          ).toBe(DepositSubmissionAttemptsDB.Status.Ambiguous);
          expect(
            afterResponseLoss[DepositSubmissionAttemptsDB.Columns.LAST_ERROR],
          ).toContain("provider submission outcome is unknown");

          const resumed = yield* resumeDepositSubmissionAttemptProgram(
            fixture.txHash,
            { observe: absentObservation },
          ).pipe(Effect.either);
          expect(resumed._tag).toBe("Left");
          expect(submitTx).toHaveBeenCalledTimes(1);
          expect(submittedBytes).toEqual([fixture.signedTxCbor]);
        }).pipe(
          Effect.ensuring(
            Effect.sync(releaseProvider.resolve).pipe(
              Effect.andThen(deleteAttempts([txHashBuffer])),
            ),
          ),
        ),
        submitTx,
      ),
    );
  });

  it("never reopens claimed states when synchronized evidence reports the transaction unseen", async () => {
    const unknown = signedTransactionFixture();
    const submitted = signedTransactionFixture();
    const ambiguous = signedTransactionFixture();
    const fixtures = [unknown, submitted, ambiguous] as const;
    const txHashes = fixtures.map(
      (fixture) => fixture.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
    );
    const submitTx = vi.fn(async () => {
      throw new Error("provider must not be called for a claimed state");
    });

    await Effect.runPromise(
      withRequiredServices(
        Effect.gen(function* () {
          for (const fixture of fixtures) {
            yield* DepositSubmissionAttemptsDB.insertPrepared(fixture.attempt);
            yield* DepositSubmissionAttemptsDB.beginSubmission(
              fixture.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            );
          }
          yield* DepositSubmissionAttemptsDB.markSubmitted(
            submitted.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            submitted.txHash,
          );
          yield* DepositSubmissionAttemptsDB.markAmbiguous(
            ambiguous.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            "provider response was lost",
          );

          for (const fixture of fixtures) {
            const reconciliation =
              yield* reconcileDepositSubmissionAttemptProgram(fixture.txHash, {
                observe: absentObservation,
              });
            expect(reconciliation.status).toBe("ambiguous");

            const resumed = yield* resumeDepositSubmissionAttemptProgram(
              fixture.txHash,
              {
                observe: absentObservation,
              },
            ).pipe(Effect.either);
            expect(resumed._tag).toBe("Left");
            expect(
              yield* DepositSubmissionAttemptsDB.beginSubmission(
                fixture.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
              ).pipe(Effect.either),
            ).toMatchObject({ _tag: "Left" });
          }
          expect(submitTx).not.toHaveBeenCalled();
        }).pipe(Effect.ensuring(deleteAttempts(txHashes))),
        submitTx,
      ),
    );
  });

  it("bounds background startup observation, continues on ambiguity, and never submits", async () => {
    const fixtures = [
      signedTransactionFixture(),
      signedTransactionFixture(),
      signedTransactionFixture(),
    ] as const;
    const txHashes = fixtures.map(
      (fixture) => fixture.attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
    );
    const observe = vi.fn(async () => ({
      kind: "ambiguous" as const,
      reason: "provider evidence unavailable during startup",
    }));
    const submitTx = vi.fn(async () => {
      throw new Error("startup reconciliation must never submit");
    });

    await Effect.runPromise(
      withRequiredServices(
        Effect.gen(function* () {
          const openBefore =
            (yield* DepositSubmissionAttemptsDB.retrieveOpenAttempts()).length;
          yield* Effect.forEach(
            fixtures,
            (fixture) =>
              DepositSubmissionAttemptsDB.insertPrepared(fixture.attempt),
            { discard: true },
          );

          const result = yield* reconcileOpenDepositSubmissionAttemptsProgram({
            limit: 2,
            concurrency: 2,
            attemptTimeoutMs: 1_000,
            observe,
          });
          expect(result.open).toBe(openBefore + 3);
          expect(result.inspected).toBe(2);
          expect(result.deferred).toBe(openBefore + 1);
          expect(result.results.map((entry) => entry.status)).toEqual([
            "ambiguous",
            "ambiguous",
          ]);
          expect(observe).toHaveBeenCalledTimes(2);
          expect(submitTx).not.toHaveBeenCalled();

          for (const txHash of txHashes) {
            const row = Option.getOrThrow(
              yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHash),
            );
            expect(row[DepositSubmissionAttemptsDB.Columns.STATUS]).toBe(
              DepositSubmissionAttemptsDB.Status.Prepared,
            );
          }
        }).pipe(Effect.ensuring(deleteAttempts(txHashes))),
        submitTx,
      ),
    );
  });
});
