/**
 * The watcher's fault-proof execution adapter over a durable funding fixture:
 * a doubleSpend objective resumed under its admitted decision, with the
 * workflow runner, wallet reads and operations sink as mocks.
 */
import { join } from "node:path";

import { WALLET_ONLY_SEND_ALWAYS_OPEN } from "@al-ft/midgard-fault-proofs/test-support/wallet-only-send-always-open";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { WorkflowAdapterRunnerInput } from "@al-ft/midgard-fault-proofs/workflow/adapters";
import type { UTxO } from "@lucid-evolution/lucid";
import { type Mock, vi } from "vitest";

import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.create-watcher-prover-funding-authority-factory.js";
import { setupFundingRecoveryFixture } from "./fault-proof-funding-fixture.setup-funding-recovery-fixture.js";
import {
  deploymentIdentity,
  walletAddress,
} from "./fault-proof-funding-fixture.sources-for.js";
import {
  emptyWorkflowJournal,
  TEST_JOURNAL_KEY,
} from "./watcher-journal-fixture.js";

export const setupFaultProofExecution = async (
  newReservation = false,
  {
    installedRunner = true,
    collateralReturn = false,
  }: {
    readonly installedRunner?: boolean;
    /** The pending attempt posts collateral with a collateral return. */
    readonly collateralReturn?: boolean;
  } = {},
) => {
  // A new reservation's journal root holds no decision. The store shares
  // only an unsigned reservation with it: signed funding for the same target
  // without its decision would hold the objective.
  const fixture = await setupFundingRecoveryFixture(
    newReservation,
    collateralReturn,
    false,
    20n,
    44,
    3,
    100n,
    collateralReturn,
  );
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: "2",
  });
  const journalRoot = newReservation
    ? join(fixture.journalRoot, "fresh")
    : fixture.journalRoot;
  // A fresh root's workflow journal is empty too.
  const workflowJournal = newReservation
    ? emptyWorkflowJournal()
    : fixture.workflowJournal;
  const fundingFactory = newReservation
    ? createWatcherProverFundingAuthorityFactory({
        journalRoot,
        journalAuthenticationKey: TEST_JOURNAL_KEY,
        workflowJournal,
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
    workflowJournal,
    runtimeConfigPath: "/unused-test-runtime.json",
    deploymentFingerprint: deploymentIdentity.manifestId,
    operationsSink: { recordProofStep, setAlert },
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
    admission: {
      mode: "resume",
      funding: "resume_only",
      walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
    },
  } as const;
  return {
    fixture,
    controller,
    execution,
    input,
    /** The admission of a fresh run, with the restart wait over. */
    runAdmission: {
      mode: "run",
      funding: "create_or_resume",
      walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN,
    } as const,
    getUtxos,
    getUtxosByOutRef,
    runOrResume,
    verifyCompleted,
    recordProofStep,
    setAlert,
  };
};
