import { mkdtemp, rm } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowActuationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import * as DecisionJournal from "../../src/fault-proofs/fault-decision-journal.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { isWatcherProofDecisionMissingError } from "../../src/fault-proofs/watcher-decision-hold.js";
import { WATCHER_JOURNAL_DATABASE_FILE } from "../../src/fault-proofs/watcher-journal-database.js";
import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});
const setup = async (unsigned = false) => {
  const fixture = await setupFundingRecoveryFixture(false, false, unsigned);
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot: fixture.journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    categories: fixture.old.launchScope,
    authenticationKey: TEST_JOURNAL_KEY,
  });
  const observation = progressObservation({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  return {
    fixture,
    authority,
    request: { observation, rollbackGeneration: "2" },
  };
};

describe("supervisor historical progress authority", () => {
  it("indexes once, wakes historical work only for changed observations, and evicts acknowledged completion", async () => {
    const test = await setup();
    const load = vi.spyOn(
      DirectoryFraudProofWorkflowJournalStore.prototype,
      "load",
    );
    const first = await test.authority.admit(test.request);
    expect(first).toHaveLength(1);
    expect(first[0]!.decision.decisionDigest).toBe(
      test.fixture.old.decisionDigest,
    );
    expect(
      assertWorkflowActuationPermitIdentity({
        permit: first[0]!.actuationPermit,
        category: "doubleSpend",
        rollbackGeneration: "2",
      }).authority,
    ).toBe("reconciliation");
    load.mockClear();
    for (let i = 0; i < 50; i++)
      expect(await test.authority.admit(test.request)).toEqual([]);
    const changed = {
      ...test.request,
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        revision: 2,
      }),
    };
    expect((await test.authority.admit(changed))[0]!.actuationPermit).toBe(
      first[0]!.actuationPermit,
    );
    expect(load).not.toHaveBeenCalled();
    await test.authority.markCompleted(test.fixture.old);
    expect(
      await test.authority.admit({
        ...changed,
        observation: progressObservation({
          deploymentFingerprint: deploymentIdentity.manifestId,
          revision: 3,
        }),
      }),
    ).toEqual([]);
    expect(load).not.toHaveBeenCalled();
  });

  it("wakes retained signed work once when the queue observation advances to a quiet native block", async () => {
    const test = await setup();
    expect(await test.authority.admit(test.request)).toHaveLength(1);
    expect(await test.authority.admit(test.request)).toEqual([]);
    const { observationDigest: _digest, ...prior } = test.request.observation;
    const advanced = {
      ...prior,
      nativePoint: {
        ...prior.nativePoint,
        blockNo: (BigInt(prior.nativePoint.blockNo) + 1n).toString(),
        slot: (BigInt(prior.nativePoint.slot) + 20n).toString(),
        blockHash:
          "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
      },
    };
    const quietObservation =
      unsafeAdmitWatcherStateQueueObservationForReplayTest({
        ...advanced,
        observationDigest: watcherSha256CanonicalJson(advanced),
      });
    const request = { ...test.request, observation: quietObservation };
    const contexts = await test.authority.admit(request);
    expect(contexts).toHaveLength(1);
    expect(contexts[0]!.decision.decisionDigest).toBe(
      test.fixture.old.decisionDigest,
    );
    expect(contexts[0]!.observationRevision).toBe(
      `${quietObservation.observationDigest}:${advanced.nativePoint.blockHash}`,
    );
    for (let i = 0; i < 5; i++)
      expect(await test.authority.admit(request)).toEqual([]);
    expect(
      await test.authority.admit({ ...request, rollbackGeneration: "1" }),
    ).toEqual([]);
  });

  it("re-admits a completed execution decision appended by the live writer after startup cache creation", async () => {
    const fixture = await setupFundingRecoveryFixture();
    const journalRoot = await mkdtemp(
      join(process.cwd(), ".watcher-progress-decision-"),
    );
    try {
      const journalInput = {
        directory: journalRoot,
        deploymentFingerprint: deploymentIdentity.manifestId,
        launchScope: fixture.old.launchScope,
        authenticationKey: TEST_JOURNAL_KEY,
      };
      const writer =
        await DecisionJournal.openWatcherFaultDecisionJournal(journalInput);
      const authority = createWatcherFaultProofProgressAuthority({
        journalRoot,
        deploymentFingerprint: deploymentIdentity.manifestId,
        categories: fixture.old.launchScope,
        authenticationKey: TEST_JOURNAL_KEY,
      });
      const observation = progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        header: {
          header: fixture.fixture.header,
          headerHash: fixture.old.headerHash,
        },
      });
      expect(
        await authority.admit({ observation, rollbackGeneration: "1" }),
      ).toEqual([]);
      await writer.appendLiveDecision(fixture.old);
      await writer.appendLiveDecision(fixture.fresh);
      const admit = async (decision: typeof fixture.old) =>
        authority.admit({
          observation,
          rollbackGeneration: "1",
          fault: {
            decision,
            deadline: watcherFaultProofDeadline(
              observation.finalizedHeaders[0]!,
            ),
            actuationPermit: createWorkflowActuationPermitController({
              decision,
              rollbackGeneration: "1",
            }).permit,
          },
        });
      await admit(fixture.old);
      const execution = {
        workflowId: fixture.initial.workflowId,
        entries: fixture.originalEntries,
      };
      await authority.updateExecution({ objective: fixture.old, execution });
      // Completion retires the in-memory objective while an already-coalesced
      // invocation can still restore the same exact execution identity.
      const refresh = vi.spyOn(
        DecisionJournal,
        "openWatcherFaultDecisionJournal",
      );
      await authority.markCompleted(fixture.old);
      await admit(fixture.fresh);
      await expect(
        authority.updateExecution({ objective: fixture.old, execution }),
      ).resolves.toBeUndefined();
      for (let index = 0; index < 20; index++) {
        await authority.markCompleted(fixture.old);
        await authority.updateExecution({ objective: fixture.old, execution });
      }
      expect(refresh).not.toHaveBeenCalled();
      expect(await writer.readAll()).toHaveLength(2);
    } finally {
      await rm(journalRoot, { recursive: true, force: true });
    }
  });

  it.each(["recorded", "missing", "corrupt"] as const)(
    "authenticates the live writer's current decision chain on a genuine cache miss (%s)",
    async (availability) => {
      const fixture = await setupFundingRecoveryFixture();
      const journalRoot = await mkdtemp(
        join(process.cwd(), ".watcher-progress-decision-"),
      );
      try {
        const writer = await DecisionJournal.openWatcherFaultDecisionJournal({
          directory: journalRoot,
          deploymentFingerprint: deploymentIdentity.manifestId,
          launchScope: fixture.old.launchScope,
          authenticationKey: TEST_JOURNAL_KEY,
        });
        const authority = createWatcherFaultProofProgressAuthority({
          journalRoot,
          deploymentFingerprint: deploymentIdentity.manifestId,
          categories: fixture.old.launchScope,
          authenticationKey: TEST_JOURNAL_KEY,
        });
        await authority.admit({
          observation: progressObservation({
            deploymentFingerprint: deploymentIdentity.manifestId,
          }),
          rollbackGeneration: "1",
        });
        if (availability !== "missing")
          await writer.appendLiveDecision(fixture.old);
        await writer.appendLiveDecision(fixture.fresh);
        if (availability === "corrupt") {
          // Another writer alters the committed row behind the journal.
          const raw = new DatabaseSync(
            join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
          );
          raw
            .prepare(
              "UPDATE watcher_fault_decisions SET body = '{}' WHERE row_key = ?",
            )
            .run(fixture.old.decisionDigest);
          raw.close();
        }
        const refresh = vi.spyOn(
          DecisionJournal,
          "openWatcherFaultDecisionJournal",
        );
        const update = {
          objective: fixture.old,
          execution: {
            workflowId: fixture.initial.workflowId,
            entries: fixture.originalEntries,
          },
        };
        if (availability === "recorded") {
          await expect(
            authority.updateExecution(update),
          ).resolves.toBeUndefined();
          for (let index = 0; index < 20; index++) {
            await authority.markCompleted(fixture.old);
            await authority.updateExecution(update);
          }
        } else if (availability === "missing") {
          // A missing decision holds the objective by name.
          const missing = authority.updateExecution(update);
          await expect(missing).rejects.toThrow("omitted its exact recorded");
          await expect(missing).rejects.toSatisfy(
            isWatcherProofDecisionMissingError,
          );
          const { category, headerHash, decisionDigest } = fixture.old;
          expect(authority.decisionHolds()).toMatchObject([
            { kind: "objective", category, headerHash, decisionDigest },
          ]);
          expect(authority.unfinishedCount()).toBe(1);
        } else {
          await expect(authority.updateExecution(update)).rejects.toThrow(
            `row ${fixture.old.decisionDigest} MAC differs`,
          );
          expect(authority.decisionHolds()).toEqual([]);
        }
        // The miss reads the writer's commits through the open journal; it
        // never reopens it.
        expect(refresh).not.toHaveBeenCalled();
      } finally {
        await rm(journalRoot, { recursive: true, force: true });
      }
    },
  );

  it("refreshes an initially unsigned indexed execution after its real journal records an intent", async () => {
    const test = await setup(true);
    expect(await test.authority.admit(test.request)).toEqual([]);
    await test.fixture.append(test.fixture.handoff.preflight);
    await test.fixture.append(test.fixture.handoff.submissionIntent);
    const entries = await test.fixture.journal.load(
      test.fixture.initial.workflowId,
    );
    await test.authority.updateExecution({
      objective: test.fixture.old,
      execution: { workflowId: test.fixture.initial.workflowId, entries },
    });
    const contexts = await test.authority.admit({
      ...test.request,
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        revision: 2,
      }),
    });
    expect(contexts).toHaveLength(1);
  });

  it("admits a new fault even on the same observation without mutating an active older permit", async () => {
    const test = await setup();
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
      header: {
        header: test.fixture.fixture.header,
        headerHash: test.fixture.fresh.headerHash,
      },
    });
    const makePermit = () =>
      createWorkflowActuationPermitController({
        decision: test.fixture.fresh,
        rollbackGeneration: "2",
      }).permit;
    const firstPermit = makePermit();
    const request = {
      ...test.request,
      observation,
      fault: {
        decision: test.fixture.fresh,
        actuationPermit: firstPermit,
        deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
      },
    };
    expect(await test.authority.admit(request)).toHaveLength(1);
    const secondPermit = makePermit();
    expect(
      await test.authority.admit({
        ...request,
        fault: { ...request.fault, actuationPermit: secondPermit },
      }),
    ).toHaveLength(1);
    expect(
      assertWorkflowActuationPermitIdentity({
        permit: firstPermit,
        category: "doubleSpend",
        rollbackGeneration: "2",
      }).authority,
    ).toBe("submission");
    test.authority.revokeAuthority("native rollback");
    expect(() =>
      assertWorkflowActuationPermitIdentity({
        permit: secondPermit,
        category: "doubleSpend",
        rollbackGeneration: "2",
      }),
    ).toThrow("native rollback");
  });

  it("mints deadline recovery separately from the active submission permit", async () => {
    const test = await setup();
    await test.authority.admit(test.request);
    const permit = await test.authority.reconcileExecution({
      objective: test.fixture.old,
      execution: {
        workflowId: test.fixture.initial.workflowId,
        entries: test.fixture.originalEntries,
      },
      rollbackGeneration: "2",
    });
    expect(
      assertWorkflowActuationPermitIdentity({
        permit,
        category: "doubleSpend",
        rollbackGeneration: "2",
      }),
    ).toMatchObject({
      authority: "reconciliation",
      decisionDigest: test.fixture.old.decisionDigest,
    });
  });
});

describe("supervisor progress authority: proof retention", () => {
  it("pins a newly admitted fault's header before indexing it, once", async () => {
    const fixture = await setupFundingRecoveryFixture();
    const journalRoot = await mkdtemp(
      join(process.cwd(), ".watcher-progress-decision-"),
    );
    try {
      const writer = await DecisionJournal.openWatcherFaultDecisionJournal({
        directory: journalRoot,
        deploymentFingerprint: deploymentIdentity.manifestId,
        launchScope: fixture.old.launchScope,
        authenticationKey: TEST_JOURNAL_KEY,
      });
      const pinned: string[] = [];
      const authority = createWatcherFaultProofProgressAuthority({
        journalRoot,
        deploymentFingerprint: deploymentIdentity.manifestId,
        categories: fixture.old.launchScope,
        authenticationKey: TEST_JOURNAL_KEY,
        retention: {
          ...storelessProofRetention,
          pin: async ({ category, headerHash }) => {
            pinned.push(`${category}/${headerHash}`);
            return { kind: "pinned" };
          },
        },
      });
      const observation = progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        header: {
          header: fixture.fixture.header,
          headerHash: fixture.old.headerHash,
        },
      });
      await authority.admit({ observation, rollbackGeneration: "1" });
      expect(pinned).toEqual([]);
      await writer.appendLiveDecision(fixture.old);
      const admit = () =>
        authority.admit({
          observation,
          rollbackGeneration: "1",
          fault: {
            decision: fixture.old,
            deadline: watcherFaultProofDeadline(
              observation.finalizedHeaders[0]!,
            ),
            actuationPermit: createWorkflowActuationPermitController({
              decision: fixture.old,
              rollbackGeneration: "1",
            }).permit,
          },
        });
      await admit();
      await admit();
      expect(pinned).toEqual([
        `${fixture.old.category}/${fixture.old.headerHash}`,
      ]);
    } finally {
      await rm(journalRoot, { recursive: true, force: true });
    }
  });
});
