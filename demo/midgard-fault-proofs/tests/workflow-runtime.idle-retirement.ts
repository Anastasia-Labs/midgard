import { expect, it, vi } from "vitest";

import {
  releaseIdleWorkflowFundingReservation,
  type WorkflowFundingReservationSnapshot,
} from "../src/workflow/funding-reservation-permit.js";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import { readAdmittedLocalKupmiosSignedTransactionRecovery } from "../src/workflow/local-kupmios-http-ogmios-source.js";
import { reconcileSignedWorkflowTransaction } from "../src/workflow/signed-transaction-reconciliation.js";
import { signedRecoveryFixture } from "./workflow-kupmios-source.signed-recovery-fixture.js";
import { DEPLOYMENT } from "./workflow-runtime.admitted-actuation.js";
import { runtimeFunding } from "./workflow-runtime.runtime-funding.js";

export const registerIdleFundingRetirementTests = () => {
  it.each([false, true])(
    "authorizes stale idle refresh once no signed attempt is in flight, without waiting for retirement, and reobservation revokes it (read-only: %s)",
    async (readOnly) => {
      const journal = new MemoryFraudProofWorkflowJournalStore();
      const refreshIdle = vi.fn(
        async (): Promise<WorkflowFundingReservationSnapshot> =>
          funding.snapshot,
      );
      // It holds collateral, so only stale inputs trigger the refresh.
      const funding = await runtimeFunding("step-one", {
        journal,
        refreshIdle,
        collateral: true,
      });
      const checkStaleRefresh = async (allowed: boolean) => {
        if (readOnly) return;
        funding.resolveInputs.mockResolvedValueOnce([]);
        await funding.begin();
        expect(refreshIdle).toHaveBeenLastCalledWith({
          expectedRevision: funding.snapshot.revision,
          releaseStaleInputs: allowed,
        });
      };
      const identity: FraudProofWorkflowIdentity = {
        schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
        deploymentFingerprint: DEPLOYMENT,
        category: "doubleSpend",
        decisionDigest: funding.actuation.decisionDigest,
        target: {
          kind: "state_queue_header",
          headerHash: funding.actuation.headerHash,
        },
      };
      const workflowId = computeFraudProofWorkflowId(identity);
      const append = async (event: FraudProofWorkflowJournalEvent) => {
        const sequence = (await journal.load(workflowId)).length;
        await journal.append(
          {
            schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
            workflowId,
            identity,
            sequence,
            recordedAt: new Date().toISOString(),
            event,
          },
          sequence,
        );
      };
      await append({ kind: "started" });
      await append({
        kind: "prepared",
        artifact: {},
        artifactDigest: journalJsonDigest({}),
      });
      const parent = await signedRecoveryFixture({ ttl: 100 });
      const child = await signedRecoveryFixture({ included: true });
      const parentHash = parent.input.transactionHash,
        childHash = child.input.transactionHash;
      const parentProof = await reconcileSignedWorkflowTransaction({
        ...parent.input,
        observe: (input) =>
          readAdmittedLocalKupmiosSignedTransactionRecovery({
            source: parent.source,
            ...input,
          }),
      });
      const childProof = await reconcileSignedWorkflowTransaction({
        ...child.input,
        observe: (input) =>
          readAdmittedLocalKupmiosSignedTransactionRecovery({
            source: child.source,
            ...input,
          }),
      });
      if (
        parentProof.kind !== "not_found" ||
        parentProof.retirement === undefined ||
        childProof.kind !== "pending" ||
        childProof.retirement === undefined
      )
        throw new Error(
          "Expected authenticated exact signed expiry and deep inclusion receipts",
        );
      expect(parentProof.retirement).toMatchObject({
        transactionHash: parentHash,
        reason: "expired",
      });
      expect(childProof.retirement).toMatchObject({
        transactionHash: childHash,
        reason: "included",
      });
      for (const [actionId, txHash] of [
        ["parent", parentHash],
        ["child", childHash],
      ] as const) {
        await append({
          kind: "preflight_passed",
          actionId,
          txHash,
          localEvaluator: "test",
          referenceScripts: [],
        });
        await append({
          kind: "submission_intent",
          actionId,
          txHash,
          attempt: 1,
          actionInput: { actionKind: "step-one" },
          durableRecovery: {
            signedTransactionCborHex:
              txHash === parentHash
                ? parent.input.signedTransactionCborHex
                : child.input.signedTransactionCborHex,
          },
        });
        await append({
          kind: "reconciled",
          actionId,
          txHash,
          outcome: "confirmed",
        });
        await append({ kind: "confirmed", actionId, txHash });
      }
      await append({
        kind: "reobserved",
        actionId: "child",
        txHash: childHash,
      });
      await append({
        kind: "reobserved",
        actionId: "parent",
        txHash: parentHash,
      });
      await append({
        kind: "reconciled",
        actionId: "parent",
        txHash: parentHash,
        outcome: "not_found",
      });
      if (readOnly)
        funding.actuation.restrictToReconciliation(
          "parent recovery owns execution slot",
        );
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).not.toHaveBeenCalled();
      await checkStaleRefresh(false);
      // Legacy status alone never retires the parent. Fresh canonical absence
      // certifies its exact old bytes while the child remains provisional.
      await append({
        kind: "signed_attempt_retired",
        txHash: parentHash,
        retirement: parentProof.retirement,
      });
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).not.toHaveBeenCalled();
      await checkStaleRefresh(false);
      await append({
        kind: "reobserved",
        actionId: "child",
        txHash: childHash,
      });
      await append({
        kind: "reconciled",
        actionId: "child",
        txHash: childHash,
        outcome: "confirmed",
      });
      await append({ kind: "confirmed", actionId: "child", txHash: childHash });
      // Confirmation inside the recovery horizon already releases idle inputs
      // and reprices collateral; retirement only prunes the record later.
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).toHaveBeenCalledTimes(readOnly ? 1 : 0);
      await checkStaleRefresh(true);
      await append({
        kind: "signed_attempt_retired",
        txHash: childHash,
        retirement: childProof.retirement,
      });
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).toHaveBeenCalledTimes(readOnly ? 2 : 0);
      await checkStaleRefresh(true);
      // A rollback reopens the confirmed attempt; it is in flight again.
      await append({
        kind: "reobserved",
        actionId: "child",
        txHash: childHash,
      });
      await releaseIdleWorkflowFundingReservation({ journal, workflowId });
      expect(funding.releaseIdle).toHaveBeenCalledTimes(readOnly ? 2 : 0);
      await checkStaleRefresh(false);
    },
  );
};
