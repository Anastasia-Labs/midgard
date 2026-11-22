import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowActuationPermitController,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { afterEach, describe, expect, it, vi } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherFaultProofProgressAuthority } from "../../src/fault-proofs/fault-proof-progress-authority.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  setupClassifiedFaultFixture,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import {
  recordObjectives,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupClassifiedFaultFixtures();
});
const setup = async () => {
  const fixture = await setupClassifiedFaultFixture();
  const authority = createWatcherFaultProofProgressAuthority({
    journalRoot: fixture.journalRoot,
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    categories: installedLaunchScope(fixture.old.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
  });
  const observation = progressObservation({
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    header: {
      header: fixture.fixture.header,
      headerHash: fixture.old.headerHash,
    },
  });
  return {
    fixture,
    authority,
    request: { observation, rollbackGeneration: "2" },
  };
};

/** The live fault `decision` as the bridge hands it over, with a fresh
 * submission permit. */
const faultOf = (
  observation: ReturnType<typeof progressObservation>,
  decision: Awaited<ReturnType<typeof setupClassifiedFaultFixture>>["old"],
  rollbackGeneration = "2",
) => ({
  decision,
  deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
  actuationPermit: createWorkflowActuationPermitController({
    decision,
    rollbackGeneration,
  }).permit,
});

describe("supervisor progress authority", () => {
  it("starts the proof deadline at an authenticated catch-up node's commit time", async () => {
    const test = await setup();
    const header = test.fixture.fixture.header;
    const blockTimeMs = header.endTime + 6n * 60n * 60_000n;
    const observation = progressObservation({
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      header: { header, headerHash: test.fixture.fresh.headerHash },
      blockTimeMs,
    });
    expect(
      watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
    ).toMatchObject({
      headerBlockTimeMs: blockTimeMs.toString(),
      maturityAtMs: (
        blockTimeMs + BigInt(MIDGARD_RETENTION_WINDOW.maturityMs)
      ).toString(),
      latestSafeStartAtMs: (
        blockTimeMs +
        BigInt(
          MIDGARD_RETENTION_WINDOW.maturityMs -
            MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
        )
      ).toString(),
    });
  });

  it("drives nothing an earlier process left: work starts only from a live fault", async () => {
    const test = await setup();
    // An earlier process's open objective.
    recordObjectives(test.fixture.journalRoot, [
      { category: "doubleSpend", headerHash: test.fixture.old.headerHash },
    ]);
    for (const revision of [1, 2, 3])
      expect(
        await test.authority.admit({
          ...test.request,
          observation: progressObservation({
            hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
            header: {
              header: test.fixture.fixture.header,
              headerHash: test.fixture.old.headerHash,
            },
            revision,
          }),
        }),
      ).toEqual([]);
    expect(test.authority.unfinishedCount()).toBe(0);
    // The fault, observed live, admits its objective afresh.
    const contexts = await test.authority.admit({
      ...test.request,
      fault: faultOf(test.request.observation, test.fixture.old),
    });
    expect(contexts).toHaveLength(1);
    expect(contexts[0]!.decision.decisionDigest).toBe(
      test.fixture.old.decisionDigest,
    );
    expect(test.authority.unfinishedCount()).toBe(1);
  });

  it("admits a new fault even on the same observation without mutating an active older permit", async () => {
    const test = await setup();
    const observation = progressObservation({
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
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
});

describe("supervisor progress authority: proof retention", () => {
  it("pins a newly admitted fault's header before indexing it, once", async () => {
    const fixture = await setupClassifiedFaultFixture();
    const pinned: string[] = [];
    const authority = createWatcherFaultProofProgressAuthority({
      journalRoot: fixture.journalRoot,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      categories: installedLaunchScope(fixture.old.launchScope),
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
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      header: {
        header: fixture.fixture.header,
        headerHash: fixture.old.headerHash,
      },
    });
    await authority.admit({ observation, rollbackGeneration: "1" });
    expect(pinned).toEqual([]);
    const admit = () =>
      authority.admit({
        observation,
        rollbackGeneration: "1",
        fault: faultOf(observation, fixture.old, "1"),
      });
    await admit();
    await admit();
    expect(pinned).toEqual([
      `${fixture.old.category}/${fixture.old.headerHash}`,
    ]);
  });
});
