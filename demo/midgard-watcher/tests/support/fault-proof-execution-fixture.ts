/**
 * The watcher's fault-proof execution adapter over a durable funding fixture:
 * a doubleSpend objective resumed under its admitted decision, with the
 * workflow runner, wallet reads and operations sink as mocks.
 */
import { join } from "node:path";

import {
  createWorkflowActuationPermitController,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import type { UTxO } from "@lucid-evolution/lucid";
import { type Mock, vi } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import {
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "./fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "./watcher-journal-fixture.js";

export const setupFaultProofExecution = async (
  newReservation = false,
  { installedRunner = true }: { readonly installedRunner?: boolean } = {},
) => {
  // A new reservation's journal root holds no decision. The store shares
  // only an unsigned reservation with it: signed funding for the same target
  // without its decision would hold the objective.
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    newReservation,
  );
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
        journalAuthenticationKey: TEST_JOURNAL_KEY,
        launchScope: installedLaunchScope(fixture.fresh.launchScope),
        deploymentIdentity,
        protocolParameters: fixture.protocolParameters,
        store: fixture.store,
      })
    : fixture.fundingFactory();
  const getUtxos: Mock<() => Promise<UTxO[]>> = vi.fn(
    async () => fixture.walletUtxos,
  );
  const getUtxosByOutRef: Mock<() => Promise<UTxO[]>> = vi.fn(async () => []);
  const runOrResume: Mock<
    (invocation: WorkflowAdapterRunnerInput) => Promise<unknown>
  > = vi.fn(
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
  const recordProofStep: Mock = vi.fn();
  const setAlert: Mock = vi.fn();
  const verifyCompleted: Mock<() => Promise<never>> = vi.fn(async () => {
    throw new Error("unexpected completed verification");
  });
  const execution = createWatcherFaultProofExecution({
    application: {
      runners: installedRunner ? { doubleSpend: fixture.runner } : {},
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
