/** The mint-item run of the own-thread reclaim rows: the process it runs
 * in, the chain its thread stands on, and how L1 shows the header's removal.
 * The run is the shared runner over mint's assembled family adapter, which
 * reclaims through the deployment's cancel port. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, vi } from "vitest";

import { createMintItemNonCanonicalRecoveryAdapter } from "../../src/mint-item-non-canonical/workflow.execute-manifest-bound-mint-item-non-canonical-workflow.js";
import { FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT } from "../../src/workflow/family-l1-observation.js";
import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "../../src/workflow/journal.fraud-proof-workflow-terminal.js";
import { createFraudProofWorkflowRegistry } from "../../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "../../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import * as cancel from "../../src/workflow/own-thread-reclaim.linear-cancel-port.js";
import type { OwnThreadReclaimPort } from "../../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import { FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER } from "../../src/workflow/raw-l1-publication-observation.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import { FraudProofResolvedByAnotherRecordError } from "../../src/workflow/resolved-by-another-record.js";
import { bindWorkflowPreflightTransaction } from "../../src/workflow/transaction-boundary.js";
import { createFixtureChain } from "../workflow.make-adapter.js";
import { bindFamilyRecoveryFixture } from "./bound-family-recovery-fixture.js";
import { signedWorkflowTransactionFixture } from "./signed-workflow-transaction.js";
import { createTestWorkflowProcess } from "./workflow-process.js";

const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
export const FINAL_DEPTH = policy.automaticRecoveryMaxDepth + 2;
export const OUR_THREAD = `${"42".repeat(32)}#0`;
const OUR_PROOF = "44".repeat(32);
const REMOVAL = "55".repeat(32);
const THREAD_LOVELACE = "9000000";
const category = "mintItemNonCanonical" as const;

/** Our run on a mint-item header: our thread stands at step two, unless
 * `stepsTaken` is zero. */
export const fixture = async ({
  stepsTaken = 2,
}: { stepsTaken?: 0 | 2 } = {}) => {
  const headerHash = "22".repeat(28);
  const fingerprint = "11".repeat(32);
  const releaseFinality = {
    schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
    deploymentIdentityDigest: fingerprint,
    blueprintHash: "cc".repeat(32),
    policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
    policy,
  };
  const terminal: FraudProofWorkflowTerminal = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
    category,
    headerHash,
    proofToken: {
      unit: "66".repeat(28),
      outRef: `${OUR_PROOF}#0`,
      createdByTxHash: OUR_PROOF,
      retainedAtFinalState: true,
    },
    correction: {
      removalTxHash: REMOVAL,
      removedStateQueueOutRef: `${"77".repeat(32)}#0`,
      fraudulentHeaderAbsent: true,
      referencedProofTokenOutRef: `${OUR_PROOF}#0`,
    },
    economics: {
      operatorCredential: "88".repeat(28),
      proverCredential: "99".repeat(28),
      operatorBondInputOutRef: `${"aa".repeat(32)}#0`,
      operatorBondInputLovelace: "900000000",
      slashedLovelace: "500000000",
      proverRewardOutputOutRef: `${REMOVAL}#0`,
      proverRewardLovelace: "400000000",
      removalFeeLovelace: "500000000",
      duplicateRewardAbsent: true,
    },
    observedAt: {
      slot: "1234",
      blockHash: "bb".repeat(32),
      confirmationDepth: FINAL_DEPTH,
    },
  };
  const reclaim = await signedWorkflowTransactionFixture();
  const reclaimTx = reclaim.input.transactionHash;
  /** The chain: our thread standing (when we took steps) until its cancel
   * lands, spent by the cancel while the node's mempool holds it, then that
   * cancel at its depth. */
  const chain = {
    standing: stepsTaken !== 0,
    pending: false,
    reclaimDepth: undefined as number | undefined,
  };
  const l1Chain = createFixtureChain();
  const land = (depth: number) => {
    chain.standing = false;
    chain.pending = false;
    chain.reclaimDepth = depth;
    l1Chain.land(reclaimTx);
  };
  const threads = () => ({
    ...(chain.standing
      ? {
          standing: {
            threadOutRef: OUR_THREAD,
            step: 2 as const,
            lovelace: THREAD_LOVELACE,
            ...(chain.pending ? { pendingSpend: reclaimTx } : {}),
          },
        }
      : {}),
    spends:
      chain.reclaimDepth === undefined
        ? []
        : [
            {
              txHash: reclaimTx,
              threadOutRef: OUR_THREAD,
              confirmationDepth: chain.reclaimDepth,
            },
          ],
  });
  const resolution = {
    removalTxHash: REMOVAL,
    referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
    confirmationDepth: FINAL_DEPTH,
  };
  /** How L1 shows the removal: against our own proof, a terminal, or
   * against another prover's record, which the stage derivation throws as
   * resolved. */
  const removedBy = { kind: "own" as "own" | "other" };
  /** The removal's depth, as either record shows it. */
  const removalAt = { depth: FINAL_DEPTH };
  const provenance = {
    trustClass: "authenticated_cardano_l1",
    sourceId: "local-transport-test",
    grade: "security",
  } as const;
  const observe = vi.fn(async () => {
    if (removedBy.kind === "own")
      return {
        provenance,
        stage: {
          kind: "removed" as const,
          terminal: {
            ...terminal,
            observedAt: {
              ...terminal.observedAt,
              confirmationDepth: removalAt.depth,
            },
          },
        },
      };
    const standing = threads().standing;
    const removal = { ...resolution, confirmationDepth: removalAt.depth };
    throw new FraudProofResolvedByAnotherRecordError(
      standing === undefined
        ? removal
        : { ...removal, standingThread: standing },
    );
  });
  const transactionConfirmed = vi.fn(
    async ({ txHash }: { txHash: string }) =>
      txHash !== reclaimTx || chain.reclaimDepth !== undefined,
  );
  const port = (): OwnThreadReclaimPort & {
    preflight: ReturnType<typeof vi.fn>;
    submit: ReturnType<typeof vi.fn>;
  } => ({
    observe: vi.fn(async () => threads()),
    preflight: vi.fn(async ({ actionId }: { actionId: string }) =>
      bindWorkflowPreflightTransaction(
        {
          actionId,
          txHash: reclaimTx,
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: "uplc-v1",
          },
          referenceScripts: [
            {
              role: "family-step-cancel",
              outRef: `${"b2".repeat(32)}#0`,
              scriptHash: "c3".repeat(28),
            },
          ],
        },
        reclaim.signed,
      ),
    ),
    // A submitted cancel waits in the node's mempool until `land`.
    submit: vi.fn(async ({ preflight }: { preflight: { txHash: string } }) => {
      if (chain.standing) chain.pending = true;
      l1Chain.accept(reclaimTx);
      return preflight.txHash;
    }),
  });
  const forbidden = async (): Promise<never> => {
    throw new Error("must not refetch the removed header or its DA");
  };
  /** Mint's shared assembly over this deployment: its cancel port is
   * `reclaim`, or, with none, an L1 view that cannot list our threads. */
  const assemble = async (reclaim: OwnThreadReclaimPort | undefined) => {
    const binding = {
      deploymentFingerprint: fingerprint,
      definition: { category, headerHash },
      releaseFinality,
    };
    const l1 = {
      portVersion: FRAUD_PROOF_FAMILY_L1_OBSERVATION_PORT,
      category,
      observe,
      observeHeader: forbidden,
      transactionConfirmed,
      publications: {
        observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
        observeExact: forbidden,
      },
      ...(reclaim === undefined
        ? {}
        : {
            observeOwnThreads: async () =>
              await reclaim.observe({ headerHash }),
          }),
    };
    const workflow = {
      binding,
      l1,
      stateQueueMutationLeaseCoordinator: { acquire: forbidden },
    };
    const linearCancel = vi
      .spyOn(cancel, "createLinearCancelReclaimPort")
      .mockReturnValue(reclaim!);
    try {
      return createMintItemNonCanonicalRecoveryAdapter({
        ...workflow,
        deployment: await bindFamilyRecoveryFixture(workflow as never),
        config: { binding },
      } as never);
    } finally {
      linearCancel.mockRestore();
    }
  };
  let process = createTestWorkflowProcess();
  /** A restart: a fresh process; the node's mempool keeps what it holds. */
  const restart = () => {
    process = createTestWorkflowProcess();
  };
  /** One run of the shared runner in the current process. */
  const execute = async (reclaim: OwnThreadReclaimPort | undefined) => {
    const assembled = await assemble(reclaim);
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: fingerprint,
      category,
      headerHash,
      prepareFamilyArtifact: async () => ({ retained: "evidence" }),
      registry: createFraudProofWorkflowRegistry({
        adapters: [process.recording(assembled.adapter)],
        launchScope: [category],
      }),
      run: process.run({ category, headerHash }),
      terminalVerifier: assembled.terminalVerifier,
      releaseFinality,
    });
  };
  return {
    reclaimTx,
    threads,
    sent: () => process.sent(),
    restart,
    land,
    removedBy,
    removalAt,
    resolution,
    port,
    execute,
  };
};

/** Resolved against `removal` once our thread is reclaimed by exactly one
 * landed cancel, and never as our conviction. */
export const expectReclaimedThenResolved = (
  outcome: Awaited<ReturnType<Awaited<ReturnType<typeof fixture>>["execute"]>>,
  removal: Readonly<{
    removalTxHash: string;
    referencedProofTokenOutRef: string;
    confirmationDepth: number;
  }>,
  f: Awaited<ReturnType<typeof fixture>>,
) => {
  expect(outcome).toMatchObject({
    kind: "resolved_by_another_record",
    resolution: removal,
  });
  const { standing, spends } = f.threads();
  expect(standing).toBeUndefined();
  expect(spends).toEqual([
    expect.objectContaining({ txHash: f.reclaimTx, threadOutRef: OUR_THREAD }),
  ]);
  // A reclaim is never our conviction: no terminal.
  expect("terminal" in outcome).toBe(false);
};
