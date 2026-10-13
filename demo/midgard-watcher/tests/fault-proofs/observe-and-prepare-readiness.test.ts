import { mkdtempSync, readdirSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  watcherFaultProofDeadline,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  createWatcherFaultProofPreparation,
  openWatcherPreparedProofStore,
} from "../../src/runtime/observe-and-prepare.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";
import { storelessProofRetention } from "../support/proof-retention.js";

vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-application.js")
  >()),
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
}));

const supervisors: ReturnType<typeof createWatcherFaultProofSupervisor>[] = [];
const directories: string[] = [];
afterEach(async () => {
  await Promise.all(supervisors.splice(0).map((s) => s.close()));
  for (const directory of directories.splice(0))
    rmSync(directory, { recursive: true, force: true });
  await cleanupFundingRecoveryFixtures();
});

describe("an observe-and-prepare watcher with an unfunded prover wallet", () => {
  it("prepares the fault, pages, and stays ready without reading the wallet", async () => {
    // A fresh objective (no signed attempt) inside its safe-start window.
    const fixture = await setupFundingRecoveryFixture(
      false,
      false,
      true,
      false,
      false,
      BigInt(Date.now()),
    );
    // The prover wallet holds nothing.
    const getUtxos = vi.fn(async () => []);
    const getUtxosByOutRef = vi.fn(async () => []);
    const runOrResume = vi.fn(async (): Promise<unknown> => {
      throw new Error("an observer must not run a proof");
    });
    const directory = mkdtempSync(join(tmpdir(), "watcher-observer-ready-"));
    directories.push(directory);
    const store = openWatcherPreparedProofStore(join(directory, "bundles"));
    // The sink is read lazily: observability is built after the supervisor.
    const late: {
      operations?: ReturnType<typeof createWatcherOperationsObservability>;
    } = {};
    const operationsSink = () => late.operations!.sink;
    const execution = createWatcherFaultProofExecution({
      application: {
        runners: { doubleSpend: fixture.runner },
        runOrResume,
        verifyCompleted: vi.fn(),
      },
      fundingFactory: fixture.fundingFactory(),
      walletAddress,
      provider: { getUtxos, getUtxosByOutRef },
      journalRoot: fixture.journalRoot,
      runtimeConfigPath: "/unused-observer-test.json",
      deploymentFingerprint: deploymentIdentity.manifestId,
      operationsSink,
      preparation: createWatcherFaultProofPreparation({
        store,
        observerKeyHash: "ab".repeat(28),
        deploymentFingerprint: deploymentIdentity.manifestId,
        operationsSink,
      }),
    });
    const supervisor = createWatcherFaultProofSupervisor({
      reservationDecisionHolds: () => [],
      proofRetention: storelessProofRetention,
      journalRoot: fixture.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      queueAuthenticationKey: new Uint8Array(32).fill(0xa5),
      execution,
    });
    supervisors.push(supervisor);
    const operations = (late.operations = createWatcherOperationsObservability({
      deploymentFingerprint: deploymentIdentity.manifestId,
      supervisor,
      launchScopeStatus: () => ({
        installedCategoryCount: 1,
        requiredCategoryCount: 1,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      durableProofQueueStatus: () => supervisor.durableQueueStatus(),
      l1Readiness: () => execution.readiness(),
      submissionModes: () => ({
        faultProofs: "observe_and_prepare",
        availability: "submit",
      }),
    }));

    const controller = createWorkflowActuationPermitController({
      decision: fixture.fresh,
      rollbackGeneration: "2",
    });
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
      header: fixture.fixture,
      revision: 2,
    });
    await supervisor.requestProgress({
      observation,
      rollbackGeneration: "2",
      fault: {
        decision: fixture.fresh,
        actuationPermit: controller.permit,
        deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
      },
    });
    await waitForFaultProofSupervisorIdle(supervisor);

    expect(runOrResume).not.toHaveBeenCalled();
    expect(getUtxos).not.toHaveBeenCalled();
    expect(readdirSync(store.directory)).toEqual([
      `fault_proof-doubleSpend-${fixture.fresh.headerHash}.json`,
    ]);
    expect(operations.api.status().activeAlerts).toMatchObject([
      { code: "prepared_proof_awaiting_submission" },
    ]);
    operations.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: Date.now().toString(),
    });
    const readyz = await handleWatcherOperationsHttpRequest(
      new Request("http://127.0.0.1/readyz"),
      operations.api,
    );
    const body = (await readyz.json()) as { reasons: readonly string[] };
    expect(body.reasons).toEqual([]);
    expect(operations.api.status().readiness).toBe("ready");
  });
});
