import {
  existsSync,
  mkdtempSync,
  readdirSync,
  readFileSync,
  rmSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofPreparation,
  openWatcherPreparedProofStore,
} from "../../src/runtime/observe-and-prepare.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";

const directories: string[] = [];
afterEach(async () => {
  for (const directory of directories.splice(0))
    rmSync(directory, { recursive: true, force: true });
  await cleanupFundingRecoveryFixtures();
});

const OBSERVER = "ab".repeat(28);

const setup = async (prepare: boolean) => {
  const fixture = await setupFundingRecoveryFixture();
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const fundingFactory = fixture.fundingFactory();
  const getUtxos = vi.fn(async () => fixture.walletUtxos);
  const getUtxosByOutRef = vi.fn(async () => []);
  const runOrResume = vi.fn(async (): Promise<unknown> => {
    throw new Error("a prepared objective must not run");
  });
  const recordProofStep = vi.fn();
  const setAlert = vi.fn();
  const directory = mkdtempSync(join(tmpdir(), "watcher-prepared-"));
  directories.push(directory);
  const store = openWatcherPreparedProofStore(join(directory, "bundles"));
  const operationsSink = () => ({ recordProofStep, setAlert });
  const execution = createWatcherFaultProofExecution({
    application: {
      runners: { doubleSpend: fixture.runner },
      runOrResume,
      verifyCompleted: vi.fn(),
    },
    fundingFactory,
    walletAddress,
    provider: { getUtxos, getUtxosByOutRef },
    journalRoot: fixture.journalRoot,
    runtimeConfigPath: "/unused-test-runtime.json",
    deploymentFingerprint: deploymentIdentity.manifestId,
    operationsSink,
    ...(prepare
      ? {
          preparation: createWatcherFaultProofPreparation({
            store,
            observerKeyHash: OBSERVER,
            deploymentFingerprint: deploymentIdentity.manifestId,
            operationsSink,
            nowMs: () => 1_000,
          }),
        }
      : {}),
  });
  const input = {
    job: {
      mode: "resume",
      category: "doubleSpend",
      headerHash: fixture.fresh.headerHash,
      decisionDigest: fixture.fresh.decisionDigest,
      rollbackGeneration: "2",
      deadline: null,
    },
    actuationPermit: controller.permit,
    admission: { mode: "resume", funding: "resume_only" },
  } as const;
  return {
    fixture,
    controller,
    execution,
    input,
    store,
    getUtxos,
    runOrResume,
    setAlert,
  };
};

describe("a prover key the fraud-prover allow-list omits (observe and prepare)", () => {
  it("prepares the admitted objective for a listed key instead of running it, and pages until it is settled", async () => {
    const test = await setup(true);
    const { headerHash, decisionDigest } = test.input.job;
    const outcome = await test.execution.execute(test.input);
    expect(outcome).toMatchObject({
      kind: "pending",
      resume: "await_observation",
      reason: expect.stringMatching(/^prepared_proof_awaiting_submission: /u),
    });
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    const [bundle] = readdirSync(test.store.directory);
    expect(bundle).toBe(`fault_proof-doubleSpend-${headerHash}.json`);
    expect(
      JSON.parse(readFileSync(join(test.store.directory, bundle!), "utf8")),
    ).toEqual({
      format: "midgard-watcher-prepared-proof-v1",
      kind: "fault_proof",
      deploymentFingerprint: deploymentIdentity.manifestId,
      category: "doubleSpend",
      headerHash,
      decisionDigest,
      rollbackGeneration: "2",
      observerKeyHash: OBSERVER,
      preparedAtMs: "1000",
    });
    // Each re-drive keeps the one page.
    await test.execution.execute(test.input);
    expect(test.setAlert).toHaveBeenCalledExactlyOnceWith({
      code: "prepared_proof_awaiting_submission",
      subjectDigest: decisionDigest,
      active: true,
      observedAtMs: "1000",
    });

    test.execution.objectiveSettled({ category: "doubleSpend", headerHash });
    expect(readdirSync(test.store.directory)).toEqual([]);
    expect(test.setAlert).toHaveBeenLastCalledWith({
      code: "prepared_proof_awaiting_submission",
      subjectDigest: decisionDigest,
      active: false,
      observedAtMs: "1000",
    });
  });

  it("still refuses an objective whose admission changed before preparing anything", async () => {
    const test = await setup(true);
    await expect(
      test.execution.execute({
        ...test.input,
        job: { ...test.input.job, decisionDigest: "00".repeat(32) },
      }),
    ).rejects.toThrow(/admission changed its objective or authority/u);
    expect(existsSync(test.store.directory)).toBe(false);
  });

  it("runs the objective itself when its key is listed", async () => {
    const test = await setup(false);
    await expect(test.execution.execute(test.input)).rejects.toThrow(
      /must not run/u,
    );
    expect(test.runOrResume).toHaveBeenCalledOnce();
  });
});
