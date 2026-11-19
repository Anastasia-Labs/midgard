/** A permit revoked while a run is past its start hands the revocation back
 * to the caller, whichever stage it lands in, so the caller reads the run as
 * revoked, not as a stall of a proof that failed: nothing is journaled as
 * stalled, and nothing more is sent. */
import { describe, expect, it } from "vitest";

import {
  bindWorkflowActuationJournal,
  isWorkflowActuationRevokedError,
} from "../src/workflow/actuation-permit.js";
import { unsafeWorkflowFundingReservationSelectedOutRefsForTest } from "../src/workflow/funding-reservation-permit.js";
import {
  type FraudProofWorkflowJournalEntry,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
} from "../src/workflow/orchestrator.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.js";
import { terminalVerifierOf } from "./support/terminal-verifier.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN_ON_PREVIEW } from "./support/wallet-only-send-always-open.js";
import {
  makeAdapter,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  terminal,
  terminalVerifier,
} from "./workflow.make-adapter.js";
import { DEPLOYMENT } from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

const FUNDING_LOVELACE = 15_000_000n;
const FEE = 200_000n;
const ACTION = { actionId: "step-one", input: { actionKind: "step-one" } };

type Stage = "preflight" | "reconcile" | "terminal_verify";

/** The kinds of the events the run journals. */
class RecordingJournal extends MemoryFraudProofWorkflowJournalStore {
  readonly kinds: string[] = [];

  override async append(
    entry: FraudProofWorkflowJournalEntry,
    expectedSequence: number,
  ): Promise<void> {
    await super.append(entry, expectedSequence);
    this.kinds.push(entry.event.kind);
  }
}

/** A funded run whose permit is revoked while it is in `stage`. */
const scene = async (stage: Stage) => {
  const journal = new RecordingJournal();
  const runtime = await runtimeFunding("step-one", { begin: false, journal });
  // The run's own permit, as the watcher's runner binds it.
  bindWorkflowActuationJournal({
    journal,
    permit: runtime.actuation.actuationPermit,
    decisionDigest: runtime.actuation.decisionDigest,
    deploymentFingerprint: DEPLOYMENT,
    category: "doubleSpend",
    headerHash: runtime.actuation.headerHash,
  });
  const revoke = () => runtime.actuation.revoke("fault_target_changed");
  const sent: string[] = [];
  const base = makeAdapter();
  const adapter: FraudProofFamilyWorkflowAdapter = {
    ...base,
    observe: async () => {
      if (stage === "terminal_verify")
        return {
          kind: "completed" as const,
          terminal: terminal(runtime.actuation.headerHash),
        };
      // The target moves while the family reads L1, before preflight.
      if (stage === "preflight") revoke();
      return { kind: "action_required" as const, action: ACTION };
    },
    preflight: async ({ action }) => {
      const signed = signedFundingTransaction({
        inputOutRefs: unsafeWorkflowFundingReservationSelectedOutRefsForTest(
          runtime.permit,
        ).fundingOutRefs,
        outputLovelace: FUNDING_LOVELACE - FEE,
        fee: FEE,
        // A wallet-only transaction carries its validity window.
        ttl: 1_000n,
      });
      return bindWorkflowPreflightTransaction(
        Object.freeze({
          actionId: action.actionId,
          txHash: signed.toHash(),
          scriptExecution: "reference_scripts" as const,
          localUplcEvaluation: {
            status: "passed" as const,
            evaluator: "uplc-v1",
          },
          referenceScripts: [
            {
              role: "family-step",
              outRef: REFERENCE_OUT_REF,
              scriptHash: REFERENCE_SCRIPT_HASH,
            },
          ] as const,
        }),
        signed,
      );
    },
    submit: async ({ preflight }) => {
      sent.push(preflight.txHash);
      return { kind: "submitted" as const, txHash: preflight.txHash };
    },
    // The target moves while the sent attempt is reconciled.
    reconcile: async ({ txHash }) => {
      revoke();
      return { kind: "pending" as const, ...(txHash ? { txHash } : {}) };
    },
  };
  const { headerHash } = runtime.actuation;
  const run = () =>
    runAdmittedFraudProofWorkflow({
      walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN_ON_PREVIEW,
      deploymentFingerprint: DEPLOYMENT,
      category: "doubleSpend",
      headerHash,
      evidenceBinding: {
        route: "canonical_block",
        headerHash,
        payloadEnvelopeSha256: "77".repeat(32),
        payloadSha256: "78".repeat(32),
        l1BlockHash: "79".repeat(32),
        l1Slot: "1",
      },
      prepareFamilyArtifact: async () => ({}),
      registry: createFraudProofWorkflowRegistry({
        adapters: [adapter],
        launchScope: ["doubleSpend"],
      }),
      journal,
      // The target moves while the landed proof is verified.
      terminalVerifier:
        stage === "terminal_verify"
          ? terminalVerifierOf(async ({ candidate }) => {
              revoke();
              return candidate;
            })
          : terminalVerifier,
      releaseFinality: {
        ...releaseFinality,
        deploymentIdentityDigest: DEPLOYMENT,
      },
    });
  return { journal, sent, run };
};

describe("workflow: a permit revoked mid-run", () => {
  it.each(["preflight", "reconcile", "terminal_verify"] as const)(
    "hands the revocation back from %s and journals no stall",
    async (stage) => {
      const { journal, sent, run } = await scene(stage);
      const outcome = await run().then(
        (result) => result,
        (error: unknown) => error,
      );
      expect(isWorkflowActuationRevokedError(outcome)).toBe(true);
      expect(journal.kinds).toContain("prepared");
      expect(journal.kinds).not.toContain("stalled");
      // Only the reconcile stage sent anything, before its permit was revoked.
      expect(sent).toHaveLength(stage === "reconcile" ? 1 : 0);
    },
  );
});
