import { describe, expect, it, vi } from "vitest";

import {
  advanceEmulatorPastLatestBlockEndTime,
  advanceEmulatorPastUnixTime,
  alignCommitSchedulerBeforeTestWorker,
  assertSpeculativeDepositSnapshotIsMemoryOnly,
  canonicalSlotConfigForLucid,
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  commitExplicitBlockHeaderProgram,
  commitTimingBudget,
  countDaPayloadRows,
  Data,
  decideSpeculativeInstructionForLiveTip,
  DepositsDB,
  Effect,
  fetchLatestCommittedBlock,
  fetchSchedulerDatum,
  fetchStateQueueSnapshotProgram,
  ForeignTipReconciliationsDB,
  initializeNodeRuntime,
  initializeProtocol,
  LucidService,
  makeFixture,
  makeGlobalsService,
  makeLucid,
  makeLucidRuntimeService,
  makeNodeConfigForFixture,
  MIDGARD_CONSENSUS_PROFILE,
  MidgardContracts,
  MidgardMpf,
  NodeConfig,
  Option,
  PendingBlockFinalizationsDB,
  readKeyHash,
  Ref,
  resetActiveRuntimePaths,
  resolveCurrentOperatorSchedulerWindow,
  runBlockConfirmation,
  runCommitWorkerUntilSubmitted,
  runLocalFinalizationRecoveryWorker,
  runNodeDatabaseEffect,
  runSpeculativeWorkerWithInstruction,
  runT1RecoveryScenario,
  SDK,
  type SpeculativeCommitWorkerInstruction,
  StateQueueMutationLeasesDB,
  submitDepositAndRefreshBarriers,
} from "./deposit-flow-emulator-shared.js";

describe.sequential("deposit flow emulator", () => {
  it("recovers T1 and matches flag-off database and global state", async () => {
    const flagOn = await runT1RecoveryScenario(true);
    const flagOff = await runT1RecoveryScenario(false);

    expect(flagOn.normalizedState).toEqual(flagOff.normalizedState);
    expect(flagOn.normalizedGlobals).toEqual(flagOff.normalizedGlobals);
    expect(flagOn.normalizedState.deposits).toHaveLength(2);
    expect(
      flagOn.normalizedState.deposits.every(
        (deposit) => !deposit.hasProjectedHeader,
      ),
    ).toBe(true);
  }, 480_000);

  it("keeps T7 restart invalidation memory-only with the submitted base journal intact", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
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
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 240_000);

  it("invalidates T2 when an independently submitted header advances the confirmed tail", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
    process.env.SPECULATIVE_COMMIT_BUILD = "true";
    let t2Phase = "fixture initialization";
    try {
      await resetActiveRuntimePaths();
      await initializeNodeRuntime();
      const fixture = await makeFixture();
      await initializeProtocol(fixture);
      const lucidService = await makeLucidRuntimeService(fixture);
      const globals = await makeGlobalsService();
      const testNodeConfig = await makeNodeConfigForFixture(fixture);
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
      t2Phase = "base block submission";
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
      const resumeSpy = vi.spyOn(MidgardMpf, "resumeParkedOverlay");
      const resumeEventFlatSpy = vi.spyOn(
        MidgardMpf,
        "resumeParkedEventFlatOverlayV1",
      );
      const discardSpy = vi.spyOn(
        MidgardMpf.prototype,
        "discardBlockOverlayIfActive",
      );
      const closeSpy = vi.spyOn(MidgardMpf.prototype, "close");
      let independentlySubmittedHeaderHash = "";
      let independentlySubmittedBlockEndTimeMs = 0;
      let daPayloadCountBeforeT2Decision = -1;
      let resumeCalls = 0;
      let resumeEventFlatCalls = 0;
      let discardInstances: readonly MidgardMpf[] = [];
      let closeInstances: readonly MidgardMpf[] = [];
      const speculative = await (async () => {
        try {
          t2Phase = "speculative candidate construction";
          return await runSpeculativeWorkerWithInstruction({
            fixture,
            lucidService,
            watermarks,
            onReady: (candidate) =>
              Effect.gen(function* () {
                yield* assertSpeculativeDepositSnapshotIsMemoryOnly({
                  baseBlockEndTimeMs: blockN.blockEndTimeMs,
                  candidateEndTimeMs: candidate.endTimeMs,
                });
                expect(yield* Effect.promise(countDaPayloadRows)).toBe(
                  daPayloadCountBeforeCandidate,
                );
                const confirmedNHeader = yield* Effect.promise(async () => {
                  await fixture.operatorLucid.awaitTx(blockN.submittedTxHash);
                  await runBlockConfirmation(
                    globals,
                    fixture.contracts,
                    lucidService,
                  );
                  await runLocalFinalizationRecoveryWorker(
                    globals,
                    fixture.contracts,
                    lucidService,
                  );
                  const confirmedN = await fetchLatestCommittedBlock(
                    fixture.operatorLucid,
                    fixture.contracts,
                  );
                  return Effect.runPromise(
                    SDK.getHeaderFromStateQueueDatum(confirmedN.datum),
                  );
                });
                daPayloadCountBeforeT2Decision =
                  yield* Effect.promise(countDaPayloadRows);
                // Confirmation and local finalization advance the emulator beyond
                // the candidate's original scheduler evidence. T2 only requires a
                // real foreign tail, so align the independent submitter and let the
                // explicit drill use that same fresh valid end time.
                let independentEndTimeMs = 0;
                yield* Effect.promise(async () => {
                  for (let attempt = 1; attempt <= 3; attempt += 1) {
                    const beforeAlignment = await Effect.runPromise(
                      independentLucidService.submitSlotSnapshot(),
                    );
                    independentEndTimeMs =
                      beforeAlignment.observedAtMs +
                      COMMIT_MINIMUM_FUTURE_BUFFER_MS +
                      30_000;
                    await alignCommitSchedulerBeforeTestWorker({
                      fixture,
                      lucidService: independentLucidService,
                      targetEndTimeMs: independentEndTimeMs,
                    });
                    const afterAlignment = await Effect.runPromise(
                      independentLucidService.submitSlotSnapshot(),
                    );
                    vi.setSystemTime(new Date(afterAlignment.observedAtMs));
                    if (
                      commitTimingBudget({
                        checkpoint: "pre_witness",
                        resolvedEndTimeMs: independentEndTimeMs,
                        nowMs: afterAlignment.observedAtMs,
                      }).satisfied
                    ) {
                      return;
                    }
                  }
                  throw new Error(
                    "T2 independent scheduler alignment repeatedly eroded the pre-witness budget",
                  );
                });
                expect(
                  commitTimingBudget({
                    checkpoint: "pre_witness",
                    resolvedEndTimeMs: independentEndTimeMs,
                    nowMs: (yield* independentLucidService.submitSlotSnapshot())
                      .observedAtMs,
                  }).satisfied,
                ).toBe(true);
                t2Phase = "independent foreign header submission";
                const independent = yield* commitExplicitBlockHeaderProgram({
                  utxosRoot: confirmedNHeader.utxosRoot,
                  transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  endTimeMs: independentEndTimeMs,
                  l2TransactionCount: 0n,
                  awaitConfirmation: true,
                }).pipe(
                  Effect.provideService(
                    LucidService,
                    independentLucidService as any,
                  ),
                  Effect.provideService(
                    MidgardContracts,
                    fixture.contracts as any,
                  ),
                  Effect.provideService(NodeConfig, testNodeConfig),
                );
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
          resumeCalls = resumeSpy.mock.calls.length;
          resumeEventFlatCalls = resumeEventFlatSpy.mock.calls.length;
          discardInstances = [
            ...(discardSpy.mock.contexts as readonly MidgardMpf[]),
          ];
          closeInstances = [
            ...(closeSpy.mock.contexts as readonly MidgardMpf[]),
          ];
          resumeSpy.mockRestore();
          resumeEventFlatSpy.mockRestore();
          discardSpy.mockRestore();
          closeSpy.mockRestore();
        }
      })();
      expect(resumeCalls).toBe(0);
      expect(resumeEventFlatCalls).toBe(0);
      expect(discardInstances).toHaveLength(1);
      expect(discardInstances[0]?.trieName).toBe("ledger");
      expect(new Set(discardInstances).size).toBe(discardInstances.length);
      expect(new Set(closeInstances).size).toBe(closeInstances.length);
      expect(closeInstances.some((mpf) => mpf.trieName === "ledger")).toBe(
        true,
      );
      expect(
        closeInstances.some((mpf) => mpf.trieName === "transactions"),
      ).toBe(true);
      expect(
        closeInstances.some(
          (mpf) => mpf.trieName === "speculative-transactions",
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
      expect(pendingDeposits).toHaveLength(1);
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
      await submitDepositAndRefreshBarriers({
        fixture,
        lucidService,
        globals,
        lovelace: 14_000_000n,
        projectToLedger: false,
      });
      t2Phase = "rebuilt block submission";
      const rebuilt = await runCommitWorkerUntilSubmitted({
        fixture,
        lucidService,
        latestBlock: foreignTip,
      });
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
      const retainedForeignEvidence = await runNodeDatabaseEffect(
        ForeignTipReconciliationsDB.retrieveByForeignHeaderHash(
          independentlySubmittedHeaderHash,
        ),
      );
      expect(Option.isSome(retainedForeignEvidence)).toBe(true);
      if (Option.isSome(retainedForeignEvidence)) {
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.STATUS
          ],
        ).toBe(ForeignTipReconciliationsDB.Status.Resolved);
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME
          ].getTime(),
        ).toBe(Number(foreignTipHeader.startTime));
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME
          ].getTime(),
        ).toBe(Number(foreignTipHeader.endTime));
        expect(
          retainedForeignEvidence.value[
            ForeignTipReconciliationsDB.Columns.VERIFIED_DA_PAYLOAD_CBOR
          ],
        ).toBeNull();
      }
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
      throw new Error(`T2 emulator regression failed during ${t2Phase}`, {
        cause,
      });
    } finally {
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 300_000);

  it("invalidates T3 for a late-visible deposit and includes it on rebuild", async () => {
    const previousMpfEngine = process.env.MPF_ENGINE;
    const previousSpeculativeCommitBuild = process.env.SPECULATIVE_COMMIT_BUILD;
    process.env.MPF_ENGINE = "overlay";
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
      if (previousMpfEngine === undefined) delete process.env.MPF_ENGINE;
      else process.env.MPF_ENGINE = previousMpfEngine;
      if (previousSpeculativeCommitBuild === undefined) {
        delete process.env.SPECULATIVE_COMMIT_BUILD;
      } else {
        process.env.SPECULATIVE_COMMIT_BUILD = previousSpeculativeCommitBuild;
      }
    }
  }, 300_000);
});
