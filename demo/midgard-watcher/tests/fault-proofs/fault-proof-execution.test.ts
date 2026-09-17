import { join } from "node:path";

import {
  createWorkflowActuationPermitController,
  LocalKupmiosCheckpointChangedError,
  LocalKupmiosTransportUnavailableError,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import { fundingTerminal } from "../funding/funding-handoff-fixture.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

const setup = async (newReservation = false) => {
  const fixture = await setupFundingRecoveryFixture();
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const journalRoot = newReservation
    ? join(fixture.journalRoot, "fresh")
    : fixture.journalRoot;
  const fundingFactory = newReservation
    ? createWatcherProverFundingAuthorityFactory({
        journalRoot,
        launchScope: fixture.fresh.launchScope,
        deploymentIdentity,
        protocolParameters: fixture.protocolParameters,
        store: fixture.store,
      })
    : fixture.fundingFactory();
  const getUtxos = vi.fn(async () => fixture.walletUtxos);
  const getUtxosByOutRef = vi.fn(async () => []);
  const runOrResume = vi.fn(
    async (invocation: WorkflowAdapterRunnerInput): Promise<unknown> =>
      fixture.run(
        fixture.bind(fixture.fresh, {
          controller,
          permit: invocation.fundingReservationPermit,
          releaseUnused: () =>
            fundingFactory.releaseUnused({
              actuationPermit: controller.permit,
            }),
        }),
      ),
  );
  const recordProofStep = vi.fn();
  const setAlert = vi.fn();
  const verifyCompleted = vi.fn(async () => {
    throw new Error("unexpected completed verification");
  });
  const execution = createWatcherFaultProofExecution({
    application: {
      runners: { doubleSpend: fixture.runner },
      runOrResume,
      verifyCompleted,
    },
    fundingFactory,
    walletAddress,
    provider: { getUtxos, getUtxosByOutRef },
    journalRoot,
    runtimeConfigPath: "/unused-test-runtime.json",
    deploymentFingerprint: deploymentIdentity.manifestId,
    operationsSink: () => ({ recordProofStep, setAlert }),
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
    getUtxos,
    getUtxosByOutRef,
    runOrResume,
    verifyCompleted,
    recordProofStep,
    setAlert,
  };
};

describe("supervisor execution adapter with durable funding", () => {
  it("restores the signed objective without querying fresh wallet inputs", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "pending",
      resume: "await_observation",
    });
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.getUtxosByOutRef).not.toHaveBeenCalled();
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect((await test.fixture.records())[0]!.reservationId).toBe(
      before[0]!.reservationId,
    );
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("returns revoked authority before funding or runner work", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    test.controller.revoke("new rollback generation");
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "authority_revoked",
    });
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("waits for a fresh observation when a typed canonical capture changes", async () => {
    const test = await setup();
    test.runOrResume.mockRejectedValueOnce(
      new LocalKupmiosCheckpointChangedError("checkpoint changed"),
    );
    expect(await test.execution.execute(test.input)).toEqual({
      kind: "pending",
      resume: "await_observation",
      reason: "checkpoint changed",
    });
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it.each([
    { kind: "completed" },
    { kind: "terminal_included" },
    { kind: "pending", reason: "signed transaction awaiting inclusion" },
    { kind: "stalled", phase: "preflight", reason: "observed action changed" },
  ])("returns $kind as an explicit supervisor outcome", async (result) => {
    const test = await setup();
    test.runOrResume.mockResolvedValueOnce(result);
    const outcome = await test.execution.execute(test.input);
    expect(outcome.kind).toBe(
      result.kind === "stalled" ? "pending" : result.kind,
    );
    expect(test.setAlert).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
  });

  it("refuses existing-only recovery without a durable reservation before querying the wallet", async () => {
    const test = await setup(true);
    const before = await test.fixture.records();
    await expect(test.execution.execute(test.input)).rejects.toThrow(
      "existing-only funding requires its durable reservation",
    );
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
  });

  it("returns structured read-only provider transport failures for bounded backoff", async () => {
    const test = await setup(true);
    const before = await test.fixture.records();
    const cause = Object.assign(new Error("socket reset"), {
      code: "ECONNRESET",
    });
    test.getUtxos.mockRejectedValueOnce(
      new TypeError("fetch failed", { cause }),
    );
    const outcome = await test.execution.execute({
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: { mode: "run", funding: "create_or_resume" },
    });
    expect(outcome).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.getUtxos).toHaveBeenCalledOnce();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("keeps initial funding scarcity pending without a failed-submission alert", async () => {
    const test = await setup(true);
    const before = await test.fixture.records();
    test.getUtxos.mockResolvedValueOnce([]);
    const outcome = await test.execution.execute({
      ...test.input,
      job: { ...test.input.job, mode: "run" },
      admission: { mode: "run", funding: "create_or_resume" },
    });
    expect(outcome).toMatchObject({
      kind: "pending",
      resume: "await_observation",
    });
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).not.toHaveBeenCalled();
  });

  it("does not classify provider message strings as transport authority", async () => {
    const test = await setup(true);
    test.getUtxos.mockRejectedValueOnce(new Error("provider says ECONNRESET"));
    await expect(
      test.execution.execute({
        ...test.input,
        job: { ...test.input.job, mode: "run" },
        admission: { mode: "run", funding: "create_or_resume" },
      }),
    ).rejects.toThrow("provider says ECONNRESET");
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.setAlert).toHaveBeenCalledWith(
      expect.objectContaining({ active: true }),
    );
  });

  it("backs off raw-source reconciliation outages without changing signed attempts or submitting", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    const entries = await test.fixture.journal.load(
      test.fixture.initial.workflowId,
    );
    vi.mocked(test.fixture.adapter.reconcile).mockRejectedValueOnce(
      new LocalKupmiosTransportUnavailableError(
        "Ogmios connection unavailable",
      ),
    );
    expect(await test.execution.execute(test.input)).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.setAlert).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(
      await test.fixture.journal.load(test.fixture.initial.workflowId),
    ).toEqual(entries);
  });

  it("backs off completed verification transport outages without funding or journal changes", async () => {
    const test = await setup();
    const entries = await test.fixture.journal.load(
      test.fixture.initial.workflowId,
    );
    const before = await test.fixture.records();
    test.verifyCompleted.mockRejectedValueOnce(
      new LocalKupmiosTransportUnavailableError("HTTP 503"),
    );
    const request = {
      job: test.input.job,
      actuationPermit: test.controller.permit,
      entries,
      terminal: fundingTerminal(
        test.fixture.old.headerHash,
        test.fixture.transactionHash,
        "bb".repeat(32),
      ),
    };
    expect(await test.execution.verifyCompleted(request)).toMatchObject({
      kind: "retryable",
      resume: "backoff",
      retryAfterMs: 1000,
    });
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
    expect(
      await test.fixture.journal.load(test.fixture.initial.workflowId),
    ).toEqual(entries);
    test.verifyCompleted.mockRejectedValueOnce(
      new Error("authenticated terminal mismatch"),
    );
    await expect(test.execution.verifyCompleted(request)).rejects.toThrow(
      "authenticated terminal mismatch",
    );
  });

  it("keeps an authentication failure hard and preserves the signed reservation", async () => {
    const test = await setup();
    const before = await test.fixture.records();
    test.runOrResume.mockRejectedValueOnce(
      new Error("authenticated proof datum changed"),
    );
    await expect(test.execution.execute(test.input)).rejects.toThrow(
      "authenticated proof datum changed",
    );
    expect(await test.fixture.records()).toEqual(before);
    expect(test.setAlert).toHaveBeenCalledWith(
      expect.objectContaining({
        code: "proof_submission_failure",
        active: true,
      }),
    );
  });
});
