import "./workflow.compiled-production-workflow-boundary.js";

import { describe, expect, it, vi } from "vitest";

import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import * as actuationAuthority from "../src/workflow/actuation-permit.js";
import {
  FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
  type FraudProofFamilyWorkflowAdapter,
} from "../src/workflow/orchestrator.js";
import { bindWorkflowPreflightTransaction } from "../src/workflow/transaction-boundary.js";
import { WorkflowFundingUnavailableError } from "../src/workflow/workflow-funding-shortfall.js";
import { signedWorkflowTransactionFixture } from "./support/signed-workflow-transaction.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  chainOf,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";

describe("canonical reobservation with pending descendants", () => {
  it.each(["action changed", "funding unavailable"])(
    "resumes an unsigned %s without sending a transaction",
    async (cause) => {
      const evidence = await canonicalEvidence();
      const process = createTestWorkflowProcess();
      const adapter = makeAdapter();
      const original = adapter.preflight;
      adapter.preflight = async () => {
        throw cause === "action changed"
          ? new WorkflowActionChangedError("authenticated action changed")
          : new WorkflowFundingUnavailableError();
      };
      const pending = await run({ evidence, adapter, process });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      expect(process.sent()).toEqual([]);
      expect(adapter.submit).not.toHaveBeenCalled();
      adapter.preflight = original;
      expect((await run({ evidence, adapter, process })).kind).toBe(
        "completed",
      );
    },
  );

  it("keeps deterministic preflight failures explicit", async () => {
    const evidence = await canonicalEvidence();
    const adapter = makeAdapter();
    adapter.preflight = async () => {
      throw new Error("local validator rejected the proof witness");
    };
    expect(
      await run({ evidence, adapter, process: createTestWorkflowProcess() }),
    ).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("validator rejected"),
    });
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("waits for fresh authority before building again a transaction that left the mempool, under reconciliation-only authority", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter({ lands: false });
    const chain = chainOf(adapter);
    // The proof waits in the mempool; the removal over it does too.
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    // Both leave the mempool unlanded: the proof is required again.
    chain.drop(PROOF_TX_HASH);
    chain.drop(REMOVAL_TX_HASH);
    const readonly = vi
      .spyOn(actuationAuthority, "workflowRunIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, process })).toMatchObject({
        kind: "pending",
        reason: FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
      });
      expect(adapter.preflight).toHaveBeenCalledTimes(2);
      expect(adapter.submit).toHaveBeenCalledTimes(2);
    } finally {
      readonly.mockRestore();
    }
  });

  it("builds neither a parent nor its descendant again while the mempool holds both", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter();
    const chain = chainOf(adapter);
    vi.mocked(adapter.submit).mockImplementation(async ({ preflight }) => {
      if (preflight.txHash === PROOF_TX_HASH) chain.land(preflight.txHash);
      else chain.accept(preflight.txHash);
      return { kind: "submitted", txHash: preflight.txHash };
    });
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    // A rollback returns the parent to the mempool, its descendant still
    // after it: the next state shows both.
    chain.drop(PROOF_TX_HASH);
    chain.accept(PROOF_TX_HASH);
    expect(await run({ evidence, adapter, process })).toMatchObject({
      kind: "pending",
      reason: `${REMOVAL_TX_HASH} awaits inclusion`,
    });
    chain.land(PROOF_TX_HASH);
    chain.land(REMOVAL_TX_HASH);
    expect((await run({ evidence, adapter, process })).kind).toBe("completed");
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("rebuilds a parent that left the chain and the mempool, and derives a fresh child identity", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const [parent, child] = await Promise.all([
      signedWorkflowTransactionFixture(),
      signedWorkflowTransactionFixture(),
    ]);
    const replacementParent = parent.signed.toHash();
    const replacementChild = child.signed.toHash();
    const adapter = makeAdapter();
    const chain = chainOf(adapter);
    let expiredParent = false;
    // The first removal waits in the mempool; everything else lands on
    // submit.
    vi.mocked(adapter.submit).mockImplementation(async ({ preflight }) => {
      if (preflight.txHash === REMOVAL_TX_HASH) chain.accept(preflight.txHash);
      else chain.land(preflight.txHash);
      return { kind: "submitted", txHash: preflight.txHash };
    });
    const preflight = adapter.preflight;
    adapter.preflight = vi.fn(async (context) => {
      const prepared = await preflight(context);
      const replacement =
        context.action.actionId === "prove"
          ? expiredParent
            ? parent.signed
            : undefined
          : context.action.actionId === `remove:${replacementParent}`
            ? child.signed
            : undefined;
      return replacement === undefined
        ? prepared
        : bindWorkflowPreflightTransaction(
            { ...prepared, txHash: replacement.toHash() },
            replacement,
          );
    });
    const parentOnChain = () =>
      chain.seen(replacementParent)
        ? replacementParent
        : chain.seen(PROOF_TX_HASH)
          ? PROOF_TX_HASH
          : undefined;
    adapter.observe = async (): Promise<
      Awaited<ReturnType<FraudProofFamilyWorkflowAdapter["observe"]>>
    > => {
      if (chain.landed(replacementChild)) {
        const value = terminal(evidence.headerHash);
        return {
          kind: "completed",
          terminal: {
            ...value,
            proofToken: {
              ...value.proofToken,
              outRef: `${replacementParent}#0`,
              createdByTxHash: replacementParent,
            },
            correction: {
              ...value.correction,
              removalTxHash: replacementChild,
              referencedProofTokenOutRef: `${replacementParent}#0`,
            },
            economics: {
              ...value.economics,
              proverRewardOutputOutRef: `${replacementChild}#0`,
            },
          },
        };
      }
      if (chain.seen(REMOVAL_TX_HASH))
        return { kind: "pending", reason: "the removal awaits inclusion" };
      const landed = parentOnChain();
      return {
        kind: "action_required",
        action:
          landed === undefined
            ? { actionId: "prove", input: { step: 0 } }
            : {
                actionId: `remove:${landed}`,
                input: { step: 1, parent: landed },
              },
      };
    };
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    // A rollback, then the parent leaves the mempool unlanded, and the
    // child that spends it with it.
    chain.drop(PROOF_TX_HASH);
    chain.drop(REMOVAL_TX_HASH);
    expiredParent = true;
    expect((await run({ evidence, adapter, process })).kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(4);
    expect(
      vi
        .mocked(adapter.submit)
        .mock.calls.map(([input]) => input.action.actionId),
    ).toEqual([
      "prove",
      `remove:${PROOF_TX_HASH}`,
      "prove",
      `remove:${replacementParent}`,
    ]);
    expect(process.sent().map(({ txHash }) => txHash)).toEqual([
      PROOF_TX_HASH,
      REMOVAL_TX_HASH,
      replacementParent,
      replacementChild,
    ]);
  });
});
