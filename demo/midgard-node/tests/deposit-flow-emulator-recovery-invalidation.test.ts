import { inspect } from "node:util";

import { describe, expect, it, vi } from "vitest";

import { NativeMpfWorkerPortClient } from "../src/services/mpf-native-owner/client.js";
import {
  alignIndependentDepositCommit,
  assertRetainedForeignDepositEvidence,
  submitIndependentDepositBlock,
  submitOwnedDeposit,
  verifyCurrentForeignParent,
} from "./deposit-flow-emulator-independent-deposit-block.js";
import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  assertSpeculativeDepositSnapshotIsMemoryOnly,
  canonicalSlotConfigForLucid,
  countDaPayloadRows,
  Data,
  decideSpeculativeInstructionForLiveTip,
  DepositsDB,
  Effect,
  ensureSeparateCollateralUtxo,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  fetchStateQueueSnapshotProgram,
  ForeignTipReconciliationsDB,
  initializeNodeRuntime,
  initializeProtocol,
  makeFixture,
  makeGlobalsService,
  makeLucid,
  makeLucidRuntimeService,
  MIDGARD_CONSENSUS_PROFILE,
  MidgardMpf,
  Option,
  PendingBlockFinalizationsDB,
  readKeyHash,
  Ref,
  refreshWalletUtxosFromProvider,
  resetActiveRuntimePaths,
  resolveCurrentOperatorSchedulerWindow,
  retainAndAttestSubmittedHeader,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runNodeDatabaseEffect,
  runSpeculativeWorkerWithInstruction,
  SDK,
  type SpeculativeCommitWorkerInstruction,
  StateQueueMutationLeasesDB,
  submitDepositAndRefreshBarriers,
} from "./deposit-flow-emulator-shared.js";
import { openHistoryProductionOwnerLifecycle } from "./helpers/history-production-owner-lifecycle.js";

describe.sequential("deposit flow emulator", () => {
  it("keeps T7 restart invalidation memory-only with the submitted base journal intact", async () => {
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();
      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
      });
      await advanceEmulatorPastUnixTime(fixture, blockN.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const { watermarks } = await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 13_000_000n,
        projectToLedger: false,
      });

      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          assertSpeculativeDepositSnapshotIsMemoryOnly({
            baseBlockEndTimeMs: blockN.blockEndTimeMs,
            candidateEndTimeMs: candidate.endTimeMs,
          }).pipe(
            Effect.as({
              type: "InvalidateSpeculativeCandidate",
              reason: "T7",
            } satisfies SpeculativeCommitWorkerInstruction),
          ),
      });
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T7",
      });
      const activeJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveActive(),
      );
      expect(Option.isSome(activeJournal)).toBe(true);
      if (Option.isSome(activeJournal)) {
        expect(
          activeJournal.value[
            PendingBlockFinalizationsDB.Columns.HEADER_HASH
          ].toString("hex"),
        ).toBe(blockN.submittedHeaderHash);
      }
    } finally {
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);

  it("invalidates T2 when an independently submitted header advances the confirmed tail", async () => {
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    let t2Phase = "fixture initialization";
    let owner:
      | Awaited<ReturnType<typeof openHistoryProductionOwnerLifecycle>>
      | undefined;
    try {
      owner = await openHistoryProductionOwnerLifecycle();
      const { fixture, lucidService, globals } = owner;
      const production = { ...owner.production, globals };
      const testNodeConfig = production.nodeConfig;
      await ensureSeparateCollateralUtxo(fixture.operatorLucid);
      await ensureSeparateCollateralUtxo(fixture.depositorLucid);
      await owner.synchronize();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitOwnedDeposit(owner, 12_000_000n);
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      t2Phase = "base block submission";
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
        production,
        nodeConfig: testNodeConfig,
      });
      await retainAndAttestSubmittedHeader({
        fixture,
        lucidService,
        globals,
        headerHash: blockN.submittedHeaderHash,
        submittedTxHash: blockN.submittedTxHash,
      });
      await advanceEmulatorPastUnixTime(fixture, blockN.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const { watermarks, coverage } = await submitOwnedDeposit(
        owner,
        13_000_000n,
      );
      const admittedDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(new Date(Date.now())),
      );
      expect(admittedDeposits).toMatchObject([
        { status: DepositsDB.Status.Projected, projected_header_hash: null },
      ]);

      // The scheduler authorizes one active credential for this window. A
      // genuinely different key cannot produce a valid competing commit until
      // it is registered, activated, and appointed. Use a distinct Lucid
      // instance (the independent submitter identity) with the currently
      // authorized credential so T2 still exercises a real competing tx.
      const independentOperatorLucid = await makeLucid(
        fixture.emulator,
        "Custom",
        {
          slotConfig: canonicalSlotConfigForLucid(fixture.operatorLucid),
        },
      );
      expect(canonicalSlotConfigForLucid(independentOperatorLucid)).toEqual(
        canonicalSlotConfigForLucid(fixture.operatorLucid),
      );
      independentOperatorLucid.selectWallet.fromSeed(
        fixture.operatorAccount.seedPhrase,
      );
      expect(independentOperatorLucid).not.toBe(fixture.operatorLucid);
      expect(await readKeyHash(independentOperatorLucid)).toBe(
        fixture.operatorKeyHash,
      );
      expect(await readKeyHash(fixture.depositorLucid)).not.toBe(
        fixture.operatorKeyHash,
      );
      expect(
        await Effect.runPromise(
          resolveCurrentOperatorSchedulerWindow(
            fixture.depositorLucid,
            fixture.contracts,
          ),
        ),
      ).toBeUndefined();
      const schedulerBeforeIndependentSubmit =
        await fetchSchedulerDatum(fixture);
      expect(
        typeof schedulerBeforeIndependentSubmit === "object" &&
          schedulerBeforeIndependentSubmit !== null &&
          "ActiveOperator" in schedulerBeforeIndependentSubmit
          ? schedulerBeforeIndependentSubmit.ActiveOperator.operator
          : undefined,
      ).toBe(fixture.operatorKeyHash);
      const independentLucidService = await makeLucidRuntimeService({
        ...fixture,
        operatorLucid: independentOperatorLucid,
      });

      const daPayloadCountBeforeCandidate = await countDaPayloadRows();
      const discardSpy = vi.spyOn(
        NativeMpfWorkerPortClient.prototype,
        "discard",
      );
      const retainSpy = vi.spyOn(
        NativeMpfWorkerPortClient.prototype,
        "retainForJournal",
      );
      const closeSpy = vi.spyOn(MidgardMpf.prototype, "close");
      let independentlySubmittedHeaderHash = "";
      let independentlySubmittedBlockEndTimeMs = 0;
      let daPayloadCountBeforeT2Decision = -1;
      let discardCalls = 0;
      let retainCalls = 0;
      let closeInstances: readonly MidgardMpf[] = [];
      const speculative = await (async () => {
        try {
          t2Phase = "speculative candidate construction";
          return await runSpeculativeWorkerWithInstruction({
            fixture,
            lucidService,
            watermarks,
            production,
            nodeConfig: testNodeConfig,
            onReady: (candidate) =>
              Effect.gen(function* () {
                // Canonical admission already projects this deposit. The
                // speculative candidate must preserve its exact source row.
                expect(
                  yield* DepositsDB.retrievePendingHeaderEntriesUpTo(
                    new Date(candidate.endTimeMs),
                  ),
                ).toEqual(admittedDeposits);
                expect(yield* Effect.promise(countDaPayloadRows)).toBe(
                  daPayloadCountBeforeCandidate,
                );
                const confirmedNHeader = yield* Effect.promise(async () => {
                  await fixture.operatorLucid.awaitTx(blockN.submittedTxHash);
                  await runBlockConfirmation(
                    globals,
                    fixture.contracts,
                    lucidService,
                    testNodeConfig,
                    production,
                  );
                  await runLocalFinalizationRecoveryWorker(
                    globals,
                    fixture.contracts,
                    lucidService,
                    fixture.runtimeOverrides!.deploymentIdentity,
                    testNodeConfig,
                    production,
                  );
                  const confirmedN = await fetchLatestCommittedBlock(
                    fixture.operatorLucid,
                    fixture.contracts,
                  );
                  return Effect.runPromise(
                    SDK.getHeaderFromStateQueueDatum(confirmedN.datum),
                  );
                });
                const independentEndTimeMs =
                  yield* alignIndependentDepositCommit(
                    fixture,
                    independentLucidService,
                    blockN.blockEndTimeMs,
                    coverage,
                  );
                t2Phase = "independent foreign header submission";
                const independent = yield* submitIndependentDepositBlock(
                  fixture,
                  independentLucidService,
                  blockN.submittedHeaderHash,
                  independentEndTimeMs,
                  testNodeConfig,
                );
                expect(independent.payload.block_body.deposits).toHaveLength(1);
                expect(
                  independent.payload.block_body.header.utxosRoot,
                ).not.toBe(confirmedNHeader.utxosRoot);
                // Independently published DA is durable; the discarded candidate
                // must not add another DA row during the T2 decision.
                daPayloadCountBeforeT2Decision =
                  yield* Effect.promise(countDaPayloadRows);
                t2Phase = "foreign-tail reconciliation";
                independentlySubmittedHeaderHash = independent.headerHash;
                independentlySubmittedBlockEndTimeMs =
                  independent.blockEndTimeMs;
                const snapshot = yield* fetchStateQueueSnapshotProgram(
                  lucidService.api,
                  fixture.contracts.stateQueue,
                  "commit_preflight",
                );
                const leaseResult =
                  yield* StateQueueMutationLeasesDB.tryWithLease(
                    "block_commitment",
                    (leaseToken) =>
                      decideSpeculativeInstructionForLiveTip({
                        expectedHeaderHash: candidate.baseHeaderHash,
                        liveTail: snapshot.tailCommitBase.utxo,
                        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
                        submitInstruction: {
                          type: "SubmitSpeculativeCandidate",
                          confirmedBlock: snapshot.tailCommitBase.utxo,
                          stateQueueLeaseToken: leaseToken,
                          baseSnapshotId: snapshot.snapshotId,
                          stateQueueHasUnmergedTail:
                            snapshot.root.outRef !==
                            snapshot.tailCommitBase.outRef,
                        },
                      }),
                  );
                if (leaseResult._tag === "Busy") {
                  return yield* Effect.fail(
                    new Error("T2 production decision could not acquire lease"),
                  );
                }
                expect(leaseResult.value).toEqual({
                  type: "InvalidateSpeculativeCandidate",
                  reason: "T2",
                });
                return leaseResult.value;
              }),
          });
        } finally {
          discardCalls = discardSpy.mock.calls.length;
          retainCalls = retainSpy.mock.calls.length;
          closeInstances = [
            ...(closeSpy.mock.contexts as readonly MidgardMpf[]),
          ];
          discardSpy.mockRestore();
          retainSpy.mockRestore();
          closeSpy.mockRestore();
        }
      })();
      // T2 invalidation must not acquire another provider after CandidateReady.
      expect(speculative.lucidAcquisitions).toBe(1);
      // The invalidated candidate's native generation is discarded, never
      // retained for a journal, and its scratch transactions trie is closed.
      expect(discardCalls).toBe(1);
      expect(retainCalls).toBe(0);
      expect(new Set(closeInstances).size).toBe(closeInstances.length);
      expect(
        closeInstances.some(
          (mpf) => mpf.trieName === "architecture-g-transactions",
        ),
      ).toBe(true);
      expect(independentlySubmittedHeaderHash).not.toBe(
        speculative.candidate.baseHeaderHash,
      );
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T2",
      });
      const pendingDeposits = await runNodeDatabaseEffect(
        DepositsDB.retrievePendingHeaderEntriesUpTo(new Date(Date.now())),
      );
      expect(pendingDeposits).toEqual(admittedDeposits);
      expect(
        Option.isNone(
          await runNodeDatabaseEffect(
            PendingBlockFinalizationsDB.retrieveActive(),
          ),
        ),
      ).toBe(true);
      expect(daPayloadCountBeforeT2Decision).toBeGreaterThanOrEqual(0);
      expect(await countDaPayloadRows()).toBe(daPayloadCountBeforeT2Decision);

      const foreignTip = await fetchLatestCommittedBlock(
        independentOperatorLucid,
        fixture.contracts,
      );
      const foreignTipHeader = await Effect.runPromise(
        SDK.getHeaderFromStateQueueDatum(foreignTip.datum),
      );
      expect(
        await Effect.runPromise(SDK.hashBlockHeader(foreignTipHeader)),
      ).toBe(independentlySubmittedHeaderHash);
      await advanceEmulatorPastUnixTime(
        fixture,
        independentlySubmittedBlockEndTimeMs,
      );
      vi.setSystemTime(new Date(fixture.emulator.now()));
      t2Phase = "source-owned rebuild deposit admission";
      await submitOwnedDeposit(owner, 14_000_000n);
      t2Phase = "rebuilt block submission";
      await refreshWalletUtxosFromProvider(fixture.operatorLucid);
      const rebuilt = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: foreignTip,
        production,
        nodeConfig: testNodeConfig,
      });
      t2Phase = "rebuilt block canonical observation";
      await fixture.operatorLucid.awaitTx(rebuilt.submittedTxHash);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      await owner.synchronize();
      const rebuiltJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(rebuilt.submittedHeaderHash, "hex"),
        ),
      );
      expect(
        Option.isNone(
          await runNodeDatabaseEffect(
            ForeignTipReconciliationsDB.retrieveAwaitingByForeignHeaderHash(
              independentlySubmittedHeaderHash,
            ),
          ),
        ),
      ).toBe(true);
      const authenticatedBase = await owner.command(
        verifyCurrentForeignParent(
          lucidService,
          fixture.contracts,
          independentlySubmittedHeaderHash,
        ),
      );
      expect(authenticatedBase.verification).toMatchObject({
        status: "verified",
        foreignHeaderHash: independentlySubmittedHeaderHash,
        verifiedHeaderHashes: expect.arrayContaining([
          independentlySubmittedHeaderHash,
        ]),
      });
      expect(authenticatedBase.root).toBe(foreignTipHeader.utxosRoot);
      await owner.command(
        assertRetainedForeignDepositEvidence(
          independentlySubmittedHeaderHash,
          foreignTipHeader.utxosRoot,
        ),
      );
      expect(Option.isSome(rebuiltJournal)).toBe(true);
      if (Option.isSome(rebuiltJournal)) {
        expect(
          rebuiltJournal.value[
            PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH
          ].toString("hex"),
        ).toBe(independentlySubmittedHeaderHash);
        expect(
          rebuiltJournal.value[
            PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT
          ],
        ).toBe(foreignTipHeader.utxosRoot);
      }
    } catch (cause) {
      throw new Error(
        `T2 emulator regression failed during ${t2Phase}: ${inspect(cause, { depth: 12 })}`,
        {
          cause,
        },
      );
    } finally {
      await owner?.close();
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 300_000);

  it("invalidates T3 for a late-visible deposit and includes it on rebuild", async () => {
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    try {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();
      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      await advanceEmulatorPastLatestBlockEndTime(fixture);
      vi.useFakeTimers({ toFake: ["Date"] });
      vi.setSystemTime(new Date(fixture.emulator.now()));

      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 12_000_000n,
      });
      const blockNBase = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const blockN = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: blockNBase,
      });
      await retainAndAttestSubmittedHeader({
        fixture,
        lucidService,
        globals,
        headerHash: blockN.submittedHeaderHash,
        submittedTxHash: blockN.submittedTxHash,
      });
      await advanceEmulatorPastUnixTime(fixture, blockN.blockEndTimeMs);
      vi.setSystemTime(new Date(fixture.emulator.now()));
      const { watermarks } = await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 13_000_000n,
        projectToLedger: false,
      });

      const lateEventId = Buffer.from(
        Data.to(
          {
            transactionId: "f3".repeat(32),
            outputIndex: 0n,
          },
          SDK.OutputReference,
        ),
        "hex",
      );
      const speculative = await runSpeculativeWorkerWithInstruction({
        fixture,
        lucidService,
        watermarks,
        onReady: (candidate) =>
          Effect.gen(function* () {
            yield* assertSpeculativeDepositSnapshotIsMemoryOnly({
              baseBlockEndTimeMs: blockN.blockEndTimeMs,
              candidateEndTimeMs: candidate.endTimeMs,
            });
            const existingDeposits = yield* DepositsDB.retrieveAllEntries();
            const template = existingDeposits.find(
              (entry) =>
                entry[DepositsDB.Columns.INCLUSION_TIME].getTime() >
                blockN.blockEndTimeMs,
            );
            if (template === undefined) {
              return yield* Effect.fail(
                new Error("Missing N+1 deposit template for T3 injection"),
              );
            }
            yield* DepositsDB.insertEntries([
              {
                ...template,
                [DepositsDB.Columns.ID]: lateEventId,
                [DepositsDB.Columns.INCLUSION_TIME]: new Date(
                  candidate.endTimeMs - 1,
                ),
                [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: Buffer.alloc(32, 0xf3),
                [DepositsDB.Columns.LEDGER_TX_ID]: Buffer.alloc(32, 0xf3),
                [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
                [DepositsDB.Columns.STATUS]: DepositsDB.Status.Awaiting,
              },
            ]);
            yield* Effect.promise(() =>
              fixture.operatorLucid.awaitTx(blockN.submittedTxHash),
            );
            yield* Effect.promise(() =>
              runBlockConfirmation(globals, fixture.contracts, lucidService),
            );
            const leaseToken = yield* StateQueueMutationLeasesDB.acquire({
              holder: "speculative-emulator-t3",
            });
            const snapshot = yield* fetchStateQueueSnapshotProgram(
              lucidService.api,
              fixture.contracts.stateQueue,
              "commit_preflight",
            );
            const localFinalizationBlock = yield* Ref.get(
              globals.AVAILABLE_LOCAL_FINALIZATION_BLOCK,
            );
            return {
              type: "SubmitSpeculativeCandidate",
              confirmedBlock: snapshot.tailCommitBase.utxo,
              stateQueueLeaseToken: leaseToken,
              baseSnapshotId: snapshot.snapshotId,
              stateQueueHasUnmergedTail:
                snapshot.root.outRef !== snapshot.tailCommitBase.outRef,
              localFinalizationBlock:
                localFinalizationBlock === ""
                  ? undefined
                  : localFinalizationBlock,
            } satisfies SpeculativeCommitWorkerInstruction;
          }),
      });
      expect(speculative.candidate.expectedUserEventCounts.deposits).toBe(1);
      expect(speculative.output).toEqual({
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: speculative.candidate.candidateId,
        reason: "T3",
      });

      const confirmedN = await fetchLatestCommittedBlock(
        fixture.operatorLucid,
        fixture.contracts,
      );
      const rebuilt = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: confirmedN,
      });
      const rebuiltJournal = await runNodeDatabaseEffect(
        PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(rebuilt.submittedHeaderHash, "hex"),
        ),
      );
      expect(Option.isSome(rebuiltJournal)).toBe(true);
      if (Option.isSome(rebuiltJournal)) {
        expect(rebuiltJournal.value.depositEventIds).toHaveLength(2);
        expect(
          rebuiltJournal.value.depositEventIds.some((eventId) =>
            eventId.equals(lateEventId),
          ),
        ).toBe(true);
      }
    } finally {
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 300_000);
});
