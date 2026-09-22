import { readFile, rename, rmdir } from "node:fs/promises";
import { join } from "node:path";

import {
  admitFraudProofRawL1Snapshot,
  assertWorkflowActuationPermitIdentity,
  bindWorkflowActuationJournal,
  bindWorkflowFundingReservationJournal,
  createWorkflowActuationPermitController,
  deriveFraudProofRawL1CompletedTerminal,
  deriveFraudProofRawL1FamilyStage,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  fraudProofRawL1SnapshotRequestForFamily,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  LocalKupmiosTransportUnavailableError,
  verifyCompletedFraudProofWorkflow,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import {
  fixture as terminalFixture,
  rollBackTerminalFixture,
} from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  watcherFaultProofDeadline,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import { admitWatcherNativeRollForwardBlock } from "../../src/l1/native-block-admission.js";
import { WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION } from "../../src/l1/native-chain-sync.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  finality,
  key,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";

const finishControl = vi.hoisted(() => ({
  beforeFinish: async (): Promise<void> => undefined,
  beforeDecisionRead: async (): Promise<void> => undefined,
}));
vi.mock("../../src/fault-proofs/fault-proof-queue-journal.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-queue-journal.js")
    >();
  return {
    ...actual,
    openWatcherFaultProofQueueJournal: async (
      input: Parameters<typeof actual.openWatcherFaultProofQueueJournal>[0],
    ) => {
      const journal = await actual.openWatcherFaultProofQueueJournal(input);
      return {
        ...journal,
        markFinished: async (
          ...args: Parameters<typeof journal.markFinished>
        ) => {
          await finishControl.beforeFinish();
          await journal.markFinished(...args);
        },
      };
    },
  };
});
vi.mock("../../src/fault-proofs/fault-decision-journal.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-decision-journal.js")
    >();
  return {
    ...actual,
    openWatcherFaultDecisionJournal: async (
      input: Parameters<typeof actual.openWatcherFaultDecisionJournal>[0],
    ) => {
      const journal = await actual.openWatcherFaultDecisionJournal(input);
      return {
        ...journal,
        readAll: async () => {
          await finishControl.beforeDecisionRead();
          return await journal.readAll();
        },
      };
    },
  };
});
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-application.js")
  >()),
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
}));

const releases: (() => void)[] = [];
const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  releases.push(resolve);
  return { promise, resolve };
};
const supervisors: ReturnType<typeof createWatcherFaultProofSupervisor>[] = [];
beforeEach(() => {
  finishControl.beforeFinish = async () => undefined;
  finishControl.beforeDecisionRead = async () => undefined;
});
afterEach(async () => {
  for (const release of releases.splice(0)) release();
  await Promise.all(
    supervisors.splice(0).map((supervisor) => supervisor.close()),
  );
  await cleanupFundingRecoveryFixtures();
});

const setup = async (headerEndTime = BigInt(Date.now())) => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    false,
    false,
    headerEndTime,
  );
  const getUtxos = vi.fn(async () => fixture.walletUtxos);
  const getUtxosByOutRef = vi.fn(async () => []);
  const journal = new DirectoryFraudProofWorkflowJournalStore(
    fixture.journalDirectory,
  );
  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entries = await journal.load(fixture.initial.workflowId);
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: fixture.initial.workflowId,
        identity: fixture.initial.identity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event,
      },
      entries.length,
    );
  };
  const verifiedFinality = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const raw = await terminalFixture({
    proofCreation: true,
    header: fixture.fixture.header,
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
  });
  const terminal = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (terminal === null)
    throw new Error("raw fixture omitted completed correction");
  let capturedSnapshot = raw.snapshot;
  let beforeCapture = async (): Promise<void> => undefined;
  const verifyCompleted = vi.fn(
    async (input: Parameters<typeof verifyCompletedFraudProofWorkflow>[0]) =>
      verifyCompletedFraudProofWorkflow(input),
  );
  let beforeRun = async (_input: WorkflowAdapterRunnerInput): Promise<void> =>
    undefined;
  let afterRun = async (result: unknown): Promise<unknown> => result;
  const runOrResume = vi.fn(async (invocation: WorkflowAdapterRunnerInput) => {
    await beforeRun(invocation);
    const bound = bindWorkflowFundingReservationJournal({
      journal: bindWorkflowActuationJournal({
        journal: new DirectoryFraudProofWorkflowJournalStore(
          fixture.journalDirectory,
        ),
        permit: invocation.actuationPermit,
        category: "doubleSpend",
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: fixture.old.headerHash,
        decisionDigest: invocation.decisionDigest,
      }),
      permit: invocation.fundingReservationPermit,
    });
    return afterRun(await fixture.run(bound));
  });
  const createExecution = () =>
    createWatcherFaultProofExecution({
      application: {
        runners: { doubleSpend: fixture.runner },
        runOrResume,
        verifyCompleted: async ({ entries, terminal, decisionDigest }) =>
          verifyCompleted({
            binding: raw.binding,
            entries,
            terminal,
            decisionDigest,
            authority: {
              authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
              capture: async () => {
                await beforeCapture();
                return capturedSnapshot;
              },
            },
          }),
      },
      fundingFactory: fixture.fundingFactory(),
      walletAddress,
      provider: { getUtxos, getUtxosByOutRef },
      journalRoot: fixture.journalRoot,
      runtimeConfigPath: "/unused-objective-test.json",
      deploymentFingerprint: deploymentIdentity.manifestId,
      operationsSink: () => ({ recordProofStep: vi.fn(), setAlert: vi.fn() }),
    });
  const createSupervisor = () => {
    const supervisor = createWatcherFaultProofSupervisor({
      journalRoot: fixture.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs: 3_600_000,
      queueAuthenticationKey: new Uint8Array(32).fill(0xa5),
      execution: createExecution(),
    });
    supervisors.push(supervisor);
    return supervisor;
  };
  const request = (
    supervisor: ReturnType<typeof createSupervisor>,
    generation: number,
    decision = fixture.fresh,
    targetPresent = true,
  ) => {
    const controller = createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: generation.toString(),
    });
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
      header: targetPresent ? fixture.fixture : undefined,
      revision: generation,
    });
    return {
      controller,
      accepted: supervisor.requestProgress({
        observation,
        rollbackGeneration: generation.toString(),
        ...(targetPresent
          ? {
              fault: {
                decision,
                actuationPermit: controller.permit,
                deadline: watcherFaultProofDeadline(
                  observation.finalizedHeaders[0]!,
                ),
              },
            }
          : {}),
      }),
    };
  };
  const writeTerminal = async (provisional = false) => {
    for (const txHash of [
      terminal.proofToken.createdByTxHash,
      terminal.correction.removalTxHash,
    ]) {
      await append({
        kind: "preflight_passed",
        actionId: txHash,
        txHash,
        localEvaluator: "fixture-local-uplc",
        referenceScripts: [],
      });
      await append({
        kind: "submission_intent",
        actionId: txHash,
        txHash,
        attempt: 1,
        actionInput: {},
      });
      await append({ kind: "submitted", actionId: txHash, txHash, attempt: 1 });
      await append({
        kind: "reconciled",
        actionId: txHash,
        txHash,
        outcome: "confirmed",
      });
      await append({ kind: "confirmed", actionId: txHash, txHash });
    }
    await append({
      kind: provisional ? "terminal_included" : "completed",
      terminal,
      terminalDigest: journalJsonDigest(terminal),
    });
  };
  const idle = async (supervisor: ReturnType<typeof createSupervisor>) =>
    vi.waitFor(async () => {
      if (supervisor.status().phase === "blocked") await supervisor.done;
      expect(supervisor.status().phase).toBe("accepting");
      expect(supervisor.status().activeJob).toBeNull();
      expect(supervisor.status().queuedJobCount).toBe(0);
    });
  return {
    fixture,
    journal,
    append,
    terminal,
    raw,
    createSupervisor,
    request,
    idle,
    writeTerminal,
    runOrResume,
    verifyCompleted,
    getUtxos,
    getUtxosByOutRef,
    setBeforeRun: (callback: typeof beforeRun) => {
      beforeRun = callback;
    },
    setAfterRun: (callback: typeof afterRun) => {
      afterRun = callback;
    },
    setBeforeCapture: (callback: typeof beforeCapture) => {
      beforeCapture = callback;
    },
    setSnapshot: (snapshot: typeof capturedSnapshot) => {
      capturedSnapshot = snapshot;
    },
  };
};

describe("proof objective progress with durable funding and journals", () => {
  it("coalesces two generations and authenticates completion before another funding admission", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      await test.writeTerminal();
      return { kind: "completed" };
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([entered.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await vi.waitFor(async () => {
      if (supervisor.status().phase === "blocked") await supervisor.done;
      expect(test.verifyCompleted).toHaveBeenCalledTimes(1);
    });
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.verifyCompleted).toHaveBeenCalledTimes(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("retains completed authority for decisions published by another handle after supervisor startup", async () => {
    const test = await setup();
    // Stage the real signed fixture outside discovery, then publish its
    // evidence only after the supervisor has opened an empty decision reader.
    const proofs = join(test.fixture.journalRoot, "fault-proofs");
    const decisions = join(test.fixture.journalRoot, "fault-decisions");
    const stagedProofs = join(test.fixture.journalRoot, "staged-proof-fixture");
    await rename(proofs, stagedProofs);
    await rename(
      decisions,
      join(test.fixture.journalRoot, "staged-decision-fixture"),
    );
    const initializedReader = vi.fn(async () => undefined);
    finishControl.beforeDecisionRead = initializedReader;
    const supervisor = test.createSupervisor();
    await supervisor.requestProgress({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
      }),
      rollbackGeneration: "0",
    });
    await test.idle(supervisor);
    expect(initializedReader).toHaveBeenCalledOnce();
    const writer = await openWatcherFaultDecisionJournal({
      directory: test.fixture.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      launchScope: test.fixture.old.launchScope,
    });
    await writer.appendLiveDecision(test.fixture.old);
    await writer.appendLiveDecision(test.fixture.fresh);
    await rmdir(proofs);
    await rename(stagedProofs, proofs);
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      await test.writeTerminal();
      return { kind: "completed" };
    });
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([entered.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await vi.waitFor(async () => {
      if (supervisor.status().phase === "blocked") await supervisor.done;
      expect(test.verifyCompleted).toHaveBeenCalledTimes(1);
    });
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it("hands unfinished work to pending valid authority after the active permit is revoked", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      if (test.runOrResume.mock.calls.length === 1) {
        entered.resolve();
        await release.promise;
      }
    });
    const supervisor = test.createSupervisor();
    const first = test.request(supervisor, 1, test.fixture.old);
    await first.accepted;
    await Promise.race([entered.promise, supervisor.done]);
    first.controller.revoke("native_chain_rollback");
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(test.runOrResume.mock.calls[1]![0].decisionDigest).toBe(
      test.fixture.fresh.decisionDigest,
    );
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it("retains a new observation arriving while the finished queue record is being persisted", async () => {
    const test = await setup();
    const finishing = deferred(),
      release = deferred();
    let held = false;
    finishControl.beforeFinish = async () => {
      if (!held) {
        held = true;
        finishing.resolve();
        await release.promise;
      }
    };
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([finishing.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("drains a public request accepted before close while admission is still reading its journal", async () => {
    const test = await setup();
    const admitting = deferred(),
      release = deferred();
    finishControl.beforeDecisionRead = async () => {
      admitting.resolve();
      await release.promise;
    };
    const supervisor = test.createSupervisor();
    const request = test.request(supervisor, 2);
    await Promise.race([admitting.promise, supervisor.done]);
    const closing = supervisor.close();
    expect(supervisor.status().phase).toBe("closing");
    release.resolve();
    await request.accepted;
    await closing;
    expect(supervisor.status().phase).toBe("closed");
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
  });

  it("reconciles a retained signed attempt after the new-start deadline with read-only authority", async () => {
    const test = await setup(20n);
    test.setBeforeRun(async (invocation) => {
      const authority = assertWorkflowActuationPermitIdentity({
        permit: invocation.actuationPermit,
        category: "doubleSpend",
        rollbackGeneration: "2",
      });
      expect(authority.authority).toBe("reconciliation");
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledWith(
      expect.objectContaining({ txHash: test.fixture.transactionHash }),
    );
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it("does not spin or repeat funding admission for identical pending observations", async () => {
    const test = await setup();
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    for (let repeat = 0; repeat < 10; repeat += 1)
      await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("reconciles once for a quiet admitted native block while retaining identical queue evidence", async () => {
    const test = await setup();
    vi.mocked(test.fixture.adapter.reconcile).mockResolvedValueOnce({
      kind: "pending",
      txHash: test.fixture.transactionHash,
    });
    test.setBeforeRun(async (invocation) => {
      expect(
        assertWorkflowActuationPermitIdentity({
          permit: invocation.actuationPermit,
          category: "doubleSpend",
          rollbackGeneration: "2",
        }).authority,
      ).toBe("reconciliation");
    });
    const supervisor = test.createSupervisor();
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const request = { observation, rollbackGeneration: "2" };
    await supervisor.requestProgress(request);
    await test.idle(supervisor);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(1);
    const metadata = {
      blockHash:
        "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
      blockNo: "12069665",
      blockType: "7",
      prevHash:
        "ff51732269af51a2efaa2a7ad4a2ff5647af5629013a446511249e837be617a0",
      slot: "159835207",
    };
    const nativeProgress = admitWatcherNativeRollForwardBlock({
      ...metadata,
      schemaVersion: WATCHER_NATIVE_CHAIN_SYNC_SCHEMA_VERSION,
      kind: "roll_forward",
      rawBlockCbor: (
        await readFile(
          new URL("../support/conway-block.hex", import.meta.url),
          "utf8",
        )
      ).trim(),
      tip: {
        kind: "point",
        blockHash: metadata.blockHash,
        blockNo: metadata.blockNo,
        slot: metadata.slot,
      },
    });
    const quietRequest = { ...request, nativeProgress };
    await supervisor.requestProgress(quietRequest);
    await test.idle(supervisor);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(2);
    const entries = await test.journal.load(test.fixture.initial.workflowId);
    for (let repeat = 0; repeat < 5; repeat += 1)
      await supervisor.requestProgress(quietRequest);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(2);
    expect(await test.journal.load(test.fixture.initial.workflowId)).toEqual(
      entries,
    );
    expect(await test.fixture.records()).toHaveLength(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("reconstructs a submitted attempt after restart without submitting or allocating again", async () => {
    const test = await setup();
    const signed = test.fixture.signedTransactionCborHex;
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await test.idle(supervisor);
    await supervisor.close();
    await test.fixture.restartStore();
    const restarted = test.createSupervisor();
    await test.request(restarted, 2).accepted;
    await test.idle(restarted);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(1);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledWith(
      expect.objectContaining({ txHash: test.fixture.transactionHash }),
    );
    expect(test.fixture.signedTransactionCborHex).toBe(signed);
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it("authenticates completed execution after restart without calling the runner or funding provider", async () => {
    const test = await setup();
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal();
    const before = await test.fixture.records();
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.verifyCompleted).toHaveBeenCalledTimes(1);
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.getUtxosByOutRef).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
  });

  it("retries a completed objective's unavailable raw source without funding or another execution", async () => {
    const test = await setup();
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal();
    const records = await test.fixture.records();
    let captures = 0;
    test.setBeforeCapture(async () => {
      if (++captures === 1)
        throw new LocalKupmiosTransportUnavailableError(
          "canonical provider HTTP 503",
        );
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await vi.waitFor(
      async () => {
        if (supervisor.status().phase === "blocked") await supervisor.done;
        expect(test.verifyCompleted).toHaveBeenCalledTimes(2);
      },
      { timeout: 3_000 },
    );
    await test.idle(supervisor);
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(records);
  });

  it("resumes a provisionally completed objective under fresh rollback authority", async () => {
    const test = await setup();
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal(true);
    const rollback = rollBackTerminalFixture(test.raw);
    const observed = vi.fn(async () => {
      const snapshot = admitFraudProofRawL1Snapshot({
        value: rollback,
        request: fraudProofRawL1SnapshotRequestForFamily({
          definition: test.raw.definition,
          releaseFinality: test.raw.binding.releaseFinality,
        }),
        releaseFinality: test.raw.binding.releaseFinality,
        observationDepth: "inclusion",
      });
      const stage = await deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition: test.raw.definition,
        releaseEconomics: test.raw.binding.releaseEconomics,
      });
      expect(stage.kind).toBe("proof_token");
      return {
        kind: "pending" as const,
        reason: "authenticated rollback restored correction target",
      };
    });
    vi.mocked(test.fixture.adapter.observe).mockImplementation(observed);
    const supervisor = test.createSupervisor();
    supervisor.revokeAuthority("native_chain_rollback");
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.runOrResume.mock.calls[0]![0].mode).toBe("resume");
    expect(observed).toHaveBeenCalledOnce();
    expect(
      (await test.journal.load(test.fixture.initial.workflowId)).at(-1)?.event
        .kind,
    ).not.toBe("completed");
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
  });
});
