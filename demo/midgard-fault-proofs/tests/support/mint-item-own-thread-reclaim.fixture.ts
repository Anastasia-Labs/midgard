/** The mint-item run of the own-thread reclaim rows: its journal, the
 * chain its thread stands on, and how L1 shows the header's removal. */
import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { expect, vi } from "vitest";

import {
  executeManifestBoundMintItemNonCanonicalWorkflow,
  type ManifestBoundMintItemNonCanonicalWorkflow,
} from "../../src/mint-item-non-canonical/workflow.js";
import * as funding from "../../src/workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  validateFraudProofWorkflowJournal,
} from "../../src/workflow/journal.js";
import type { OwnThreadReclaimPort } from "../../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import { FraudProofResolvedByAnotherRecordError } from "../../src/workflow/resolved-by-another-record.js";

const policy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
export const FINAL_DEPTH = policy.automaticRecoveryMaxDepth + 2;
const STEP_ONE = "41".repeat(32);
const STEP_TWO = "42".repeat(32);
export const OUR_THREAD = `${STEP_TWO}#0`;
export const RECLAIM_TX = "43".repeat(32);
export const SIBLING_PROOF = "44".repeat(32);
export const REMOVAL = "55".repeat(32);
const THREAD_LOVELACE = "9000000";

/** Our run on a mint-item header: two steps confirmed, so our thread
 * stands at step two, unless `stepsTaken` is zero. */
export const fixture = ({ stepsTaken = 2 }: { stepsTaken?: 0 | 2 } = {}) => {
  const headerHash = "22".repeat(28);
  const fingerprint = "11".repeat(32);
  const identity = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    deploymentFingerprint: fingerprint,
    category: "mintItemNonCanonical" as const,
    target: { kind: "state_queue_header" as const, headerHash },
    decisionDigest: "33".repeat(32),
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const entries: FraudProofWorkflowJournalEntry[] = [];
  const push = (event: FraudProofWorkflowJournalEntry["event"]) =>
    entries.push({
      schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
      workflowId,
      identity,
      sequence: entries.length,
      recordedAt: "2026-09-16T12:00:00.000Z",
      event,
    });
  push({ kind: "started" });
  const artifact = {
    category: "mintItemNonCanonical",
    familyIdentity: "retained-evidence",
  };
  push({
    kind: "prepared",
    artifact,
    artifactDigest: journalJsonDigest(artifact),
  });
  const steps = [
    ["mintItemNonCanonical:submitStep01", STEP_ONE, "none", "step01"],
    ["mintItemNonCanonical:submitStep02", STEP_TWO, "step01", "step02"],
  ] as const;
  for (const [actionId, txHash, sourceStage, targetStage] of steps.slice(
    0,
    stepsTaken,
  )) {
    push({
      kind: "preflight_passed",
      actionId,
      txHash,
      localEvaluator: "lucid",
      referenceScripts: [],
    });
    push({
      kind: "submission_intent",
      actionId,
      txHash,
      attempt: 1,
      actionInput: {},
      durableRecovery: {
        familyIdentity: "retained-evidence",
        sourceStage,
        targetStage,
      },
    });
    push({ kind: "submitted", actionId, txHash, attempt: 1 });
    push({ kind: "reconciled", actionId, txHash, outcome: "confirmed" });
    push({ kind: "confirmed", actionId, txHash });
  }
  const terminal: FraudProofWorkflowTerminal = {
    schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
    category: identity.category,
    headerHash,
    proofToken: {
      unit: "66".repeat(28),
      outRef: `${SIBLING_PROOF}#0`,
      createdByTxHash: SIBLING_PROOF,
      retainedAtFinalState: true,
    },
    correction: {
      removalTxHash: REMOVAL,
      removedStateQueueOutRef: `${"77".repeat(32)}#0`,
      fraudulentHeaderAbsent: true,
      referencedProofTokenOutRef: `${SIBLING_PROOF}#0`,
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
  /** The chain: our thread standing (when we took steps) until its cancel
   * lands, then that cancel at its depth. */
  const chain = {
    standing: stepsTaken !== 0,
    reclaimDepth: undefined as number | undefined,
  };
  const land = (depth: number) => {
    chain.standing = false;
    chain.reclaimDepth = depth;
  };
  const threads = () => ({
    ...(chain.standing
      ? {
          standing: {
            threadOutRef: OUR_THREAD,
            step: 2 as const,
            lovelace: THREAD_LOVELACE,
          },
        }
      : {}),
    spends:
      chain.reclaimDepth === undefined
        ? []
        : [
            {
              txHash: RECLAIM_TX,
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
  /** How L1 shows the removal: a sibling's terminal, or another prover's
   * record, which the stage derivation throws as resolved. */
  const removedBy = { kind: "sibling" as "sibling" | "other" };
  /** The removal's depth, as either record shows it. */
  const removalAt = { depth: FINAL_DEPTH };
  const observe = vi.fn(async () => {
    if (removedBy.kind === "sibling")
      return {
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
      txHash !== RECLAIM_TX || chain.reclaimDepth !== undefined,
  );
  const port = (): OwnThreadReclaimPort & {
    preflight: ReturnType<typeof vi.fn>;
    submit: ReturnType<typeof vi.fn>;
  } => ({
    observe: vi.fn(async () => threads()),
    preflight: vi.fn(async ({ actionId }: { actionId: string }) => ({
      actionId,
      txHash: RECLAIM_TX,
      scriptExecution: "reference_scripts" as const,
      localUplcEvaluation: { status: "passed" as const, evaluator: "uplc-v1" },
      referenceScripts: [],
    })),
    submit: vi.fn(
      async ({ preflight }: { preflight: { txHash: string } }) =>
        preflight.txHash,
    ),
  });
  const workflow = (reclaim: OwnThreadReclaimPort | undefined) =>
    ({
      binding: {
        deploymentFingerprint: fingerprint,
        definition: { category: identity.category, headerHash },
        releaseFinality: {
          schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
          deploymentIdentityDigest: fingerprint,
          blueprintHash: "cc".repeat(32),
          policyDigest: computeFraudProofReleaseFinalityPolicyDigest(policy),
          policy,
        },
      },
      decisionDigest: identity.decisionDigest,
      l1: {
        category: identity.category,
        observe,
        observeHeader: async () => {
          throw new Error("must not refetch the removed header or its DA");
        },
        transactionConfirmed,
      },
      ...(reclaim === undefined ? {} : { reclaim }),
    }) as unknown as ManifestBoundMintItemNonCanonicalWorkflow;
  const journal: FraudProofWorkflowJournalStore = {
    load: async (id) => {
      expect(id).toBe(workflowId);
      return [...entries];
    },
    append: async (entry, sequence) => {
      expect(sequence).toBe(entries.length);
      validateFraudProofWorkflowJournal({
        workflowId,
        entries: [...entries, entry],
        expectedIdentity: identity,
      });
      entries.push(structuredClone(entry));
    },
  };
  const release = vi.spyOn(funding, "releaseWorkflowFundingReservation");
  const execute = (reclaim: OwnThreadReclaimPort | undefined) =>
    executeManifestBoundMintItemNonCanonicalWorkflow({
      workflow: workflow(reclaim),
      sources: [],
      journal,
    });
  return {
    entries,
    land,
    removedBy,
    removalAt,
    resolution,
    port,
    release,
    execute,
  };
};

const kinds = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.map(({ event }) => event.kind);

export const expectReclaimedThenResolved = (
  outcome: Awaited<ReturnType<ReturnType<typeof fixture>["execute"]>>,
  removal: Readonly<{
    removalTxHash: string;
    referencedProofTokenOutRef: string;
    confirmationDepth: number;
  }>,
) => {
  expect(outcome).toMatchObject({
    kind: "resolved_by_another_record",
    resolution: removal,
    reclaimed: [
      {
        actionId: `reclaim:${OUR_THREAD}`,
        txHash: RECLAIM_TX,
        threadOutRef: OUR_THREAD,
        lovelace: THREAD_LOVELACE,
      },
    ],
  });
  // A reclaim is never our conviction: no terminal, no completion.
  const settled = outcome as {
    readonly terminal?: unknown;
    readonly entries: readonly FraudProofWorkflowJournalEntry[];
  };
  expect(settled.terminal).toBeUndefined();
  const recorded = kinds(settled.entries);
  expect(recorded).not.toContain("completed");
  expect(recorded).not.toContain("terminal_included");
};
