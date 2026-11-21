/** A permit revoked while a run reads L1 on paths past its main reconcile
 * hands the revocation back to the caller, as the main stages do: the
 * reconciliation of a parent a descendant built on, and the funding
 * reservation of an authority-bearing transaction. Nothing is journaled as
 * stalled, and nothing more is sent. */
import { describe, expect, it } from "vitest";

import { bindWorkflowActuationJournal } from "../src/workflow/actuation-permit.assert-workflow-actuation-permit-identity.js";
import { isWorkflowActuationRevokedError } from "../src/workflow/actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { grantValidationDisputeFundingAuthority } from "../src/workflow/funding-reservation-permit.assert-validation-dispute-funding.js";
import { unsafeWorkflowFundingReservationSelectedOutRefsForTest } from "../src/workflow/funding-reservation-permit.restrict-workflow-funding-signer.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.directory-fraud-proof-workflow-journal-store.js";
import type { FraudProofWorkflowJournalEntry } from "../src/workflow/journal.fraud-proof-workflow-journal-event.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import { createFraudProofWorkflowRegistry } from "../src/workflow/orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "../src/workflow/orchestrator.run-admitted-fraud-proof-workflow.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { releaseFinality } from "./support/raw-l1-terminal-fixture.output.js";
import { WALLET_ONLY_SEND_ALWAYS_OPEN_ON_PREVIEW } from "./support/wallet-only-send-always-open.js";
import {
  makeAdapter,
  PROOF_TX_HASH,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  REMOVAL_TX_HASH,
  terminalVerifier,
} from "./workflow.make-adapter.js";
import {
  admittedActuation,
  DEPLOYMENT,
  fundingAddress,
} from "./workflow-runtime.admitted-actuation.js";
import {
  runtimeFunding,
  signedFundingTransaction,
} from "./workflow-runtime.runtime-funding.js";

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

type Actuation = Readonly<{
  actuationPermit: Parameters<typeof bindWorkflowActuationJournal>[0]["permit"];
  decisionDigest: string;
  headerHash: string;
}>;

/** `journal` bound to the run's own permit, as the watcher's runner binds it. */
const bound = (journal: RecordingJournal, actuation: Actuation) =>
  bindWorkflowActuationJournal({
    journal,
    permit: actuation.actuationPermit,
    decisionDigest: actuation.decisionDigest,
    deploymentFingerprint: DEPLOYMENT,
    category: "doubleSpend",
    headerHash: actuation.headerHash,
  });

/** The admitted run of `adapter` on `actuation`'s header over `journal`. */
const admittedRun = (
  journal: RecordingJournal,
  actuation: Actuation,
  adapter: FraudProofFamilyWorkflowAdapter,
) =>
  runAdmittedFraudProofWorkflow({
    walletOnlySend: WALLET_ONLY_SEND_ALWAYS_OPEN_ON_PREVIEW,
    deploymentFingerprint: DEPLOYMENT,
    category: "doubleSpend",
    headerHash: actuation.headerHash,
    evidenceBinding: {
      route: "canonical_block",
      headerHash: actuation.headerHash,
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
    terminalVerifier,
    releaseFinality: {
      ...releaseFinality,
      deploymentIdentityDigest: DEPLOYMENT,
    },
  }).then(
    (result) => result,
    (error: unknown) => error,
  );

describe("workflow: a permit revoked mid-run, past the main stages", () => {
  it("hands the revocation back from the reconciliation of a parent a pending descendant built on, and journals no stall", async () => {
    const journal = new RecordingJournal();
    const actuation = await admittedActuation();
    bound(journal, actuation);
    let parentIncluded = false;
    let childSubmitted = false;
    const sent: string[] = [];
    const adapter = makeAdapter({
      submit: async ({ preflight }) => {
        sent.push(preflight.txHash);
        if (preflight.txHash === REMOVAL_TX_HASH) childSubmitted = true;
        return { kind: "submitted", txHash: preflight.txHash };
      },
      reconcile: async ({ txHash, reconciliationOnly }) => {
        // The target moves while the parent's inclusion is read again.
        if (reconciliationOnly) actuation.revoke("fault_target_changed");
        if (txHash === PROOF_TX_HASH) parentIncluded = true;
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    // Once the descendant is sent, the family asks for its parent again.
    adapter.observe = async () => ({
      kind: "action_required",
      action:
        parentIncluded && !childSubmitted
          ? { actionId: "remove", input: { step: 1 } }
          : { actionId: "prove", input: { step: 0 } },
    });
    const outcome = await admittedRun(journal, actuation, adapter);
    expect(isWorkflowActuationRevokedError(outcome)).toBe(true);
    expect(journal.kinds).toContain("confirmed");
    expect(journal.kinds).not.toContain("stalled");
    expect(sent).toEqual([PROOF_TX_HASH, REMOVAL_TX_HASH]);
  });

  it("hands the revocation back from the funding reservation of a transaction whose funding authority it checks, and journals no stall", async () => {
    const journal = new RecordingJournal();
    const runtime = await runtimeFunding("step-one", { begin: false, journal });
    bound(journal, runtime.actuation);
    const sent: string[] = [];
    const base = makeAdapter();
    const action = { actionId: "step-one", input: { actionKind: "step-one" } };
    const adapter: FraudProofFamilyWorkflowAdapter = {
      ...base,
      observe: async () => ({ kind: "action_required" as const, action }),
      preflight: async () => {
        const signed = signedFundingTransaction({
          inputOutRefs: unsafeWorkflowFundingReservationSelectedOutRefsForTest(
            runtime.permit,
          ).fundingOutRefs,
          outputLovelace: 14_800_000n,
          fee: 200_000n,
          // A wallet-only transaction carries its validity window.
          ttl: 1_000n,
        });
        // A transaction carrying a dispute's funding authority, which the
        // reservation binds to the run's permit before it reserves.
        grantValidationDisputeFundingAuthority({
          signed,
          deploymentFingerprint: DEPLOYMENT,
          headerHash: runtime.actuation.headerHash,
          opener: {
            txHash: "75".repeat(32),
            outputIndex: 0,
            address: fundingAddress,
            assets: { lovelace: 5_000_000n },
          },
          bond: {
            address: fundingAddress,
            unit: "d5".repeat(28),
            lovelace: 10_000_000_000n,
            datumCborHex: "00",
          },
          caseOutput: {
            address: fundingAddress,
            unit: "d5".repeat(28),
            floorLovelace: 3_200_000n,
            lovelace: 5_000_000n,
          },
        });
        // The target moves while the family builds it.
        runtime.actuation.revoke("fault_target_changed");
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
    };
    const outcome = await admittedRun(journal, runtime.actuation, adapter);
    expect(isWorkflowActuationRevokedError(outcome)).toBe(true);
    expect(journal.kinds).toContain("prepared");
    expect(journal.kinds).not.toContain("stalled");
    expect(sent).toEqual([]);
  });
});
