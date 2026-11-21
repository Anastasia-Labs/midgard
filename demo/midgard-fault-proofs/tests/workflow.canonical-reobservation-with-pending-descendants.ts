import "./workflow.included-parent-with-pending-descendant.js";
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

/** The adapter's submissions land from now on. */
const landingFromNow = (adapter: FraudProofFamilyWorkflowAdapter) => {
  const chain = chainOf(adapter);
  vi.mocked(adapter.submit).mockImplementation(async ({ preflight }) => {
    chain.land(preflight.txHash);
    return { kind: "submitted", txHash: preflight.txHash };
  });
};

describe("canonical reobservation with pending descendants", () => {
  it("backs off exact rebroadcasts without exhausting the proof objective", async () => {
    const evidence = await canonicalEvidence();
    let time = Date.parse("2026-08-29T00:00:00.000Z");
    const adapter = makeAdapter({ lands: false });
    const chain = chainOf(adapter);
    const process = createTestWorkflowProcess();
    const sends = process.sends({
      category: "doubleSpend",
      headerHash: evidence.headerHash,
    });
    const broadcastAt = () => sends.inFlight()[0]!.broadcastAtMs;
    const invoke = () => run({ evidence, adapter, process, nowMs: () => time });
    expect(await invoke()).toMatchObject({ kind: "pending" });
    // The proof is absent but may still land: its exact bytes go again at
    // most once per interval.
    chain.status.set(PROOF_TX_HASH, "rebroadcast");
    let last = time;
    for (let index = 1; index <= 4; index += 1) {
      time += 29_999;
      expect(await invoke()).toMatchObject({ kind: "pending" });
      expect(broadcastAt()).toBe(last);
      time += 1;
      expect(await invoke()).toMatchObject({ kind: "pending" });
      expect(broadcastAt()).toBe(time);
      last = time;
      expect(await invoke()).toMatchObject({ kind: "pending" });
      expect(broadcastAt()).toBe(last);
    }
    expect(adapter.submit).toHaveBeenCalledTimes(1);
    chain.land(PROOF_TX_HASH);
    landingFromNow(adapter);
    const result = await invoke();
    expect(result.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(process.sent().map(({ actionId }) => actionId)).toEqual([
      "prove",
      "remove",
    ]);
  });

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

  it("yields an uncertain chain answer and later continues from the same transaction", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter({ lands: false });
    const chain = chainOf(adapter);
    chain.status.set(PROOF_TX_HASH, "unknown");
    expect(await run({ evidence, adapter, process })).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
    });
    expect(adapter.submit).toHaveBeenCalledTimes(1);
    chain.land(PROOF_TX_HASH);
    landingFromNow(adapter);
    const result = await run({ evidence, adapter, process });
    expect(result.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(process.sent().map(({ actionId }) => actionId)).toEqual([
      "prove",
      "remove",
    ]);
  });

  it("keeps an unknown answer pending under reconciliation-only authority instead of blocking execution", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter({ lands: false });
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    chainOf(adapter).status.set(PROOF_TX_HASH, "unknown");
    const readonly = vi
      .spyOn(actuationAuthority, "workflowRunIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, process })).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(`${PROOF_TX_HASH} of prove: unknown`),
      });
      expect(adapter.submit).toHaveBeenCalledTimes(1);
    } finally {
      readonly.mockRestore();
    }
  });

  it("waits for fresh authority before replacing an expired transaction under reconciliation-only authority", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter({ lands: false });
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    chainOf(adapter).status.set(PROOF_TX_HASH, "expired");
    const readonly = vi
      .spyOn(actuationAuthority, "workflowRunIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, process })).toMatchObject({
        kind: "pending",
        reason: FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
      });
      expect(adapter.preflight).toHaveBeenCalledTimes(1);
      expect(adapter.submit).toHaveBeenCalledTimes(1);
    } finally {
      readonly.mockRestore();
    }
  });

  it("holds a rolled-back parent and its sent descendant on the chain rather than building either again", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const adapter = makeAdapter();
    const chain = chainOf(adapter);
    vi.mocked(adapter.submit).mockImplementation(async ({ preflight }) => {
      if (preflight.txHash === PROOF_TX_HASH) chain.land(preflight.txHash);
      return { kind: "submitted", txHash: preflight.txHash };
    });
    expect((await run({ evidence, adapter, process })).kind).toBe("pending");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    // A rollback drops the parent: absent, but it may land again.
    chain.status.set(PROOF_TX_HASH, "rebroadcast");
    expect(await run({ evidence, adapter, process })).toMatchObject({
      kind: "pending",
      awaitingActionId: "prove",
    });
    chain.land(PROOF_TX_HASH);
    chain.land(REMOVAL_TX_HASH);
    expect((await run({ evidence, adapter, process })).kind).toBe("completed");
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("rebuilds a stably expired parent and derives a fresh child identity", async () => {
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
    // The first removal stays pending; everything else lands on submit.
    vi.mocked(adapter.submit).mockImplementation(async ({ preflight }) => {
      if (preflight.txHash !== REMOVAL_TX_HASH) chain.land(preflight.txHash);
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
      chain.landed(replacementParent)
        ? replacementParent
        : chain.landed(PROOF_TX_HASH)
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
    // A rollback, then the chain says the parent can never land, nor the
    // child that spends it.
    chain.status.set(PROOF_TX_HASH, "expired");
    chain.status.set(REMOVAL_TX_HASH, "invalidated");
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
