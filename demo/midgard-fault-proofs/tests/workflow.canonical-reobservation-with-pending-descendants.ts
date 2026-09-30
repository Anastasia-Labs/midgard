import "./workflow.compiled-production-workflow-boundary.js";

import { describe, expect, it, vi } from "vitest";

import { WorkflowActionChangedError } from "../src/workflow/action-changed.js";
import * as actuationAuthority from "../src/workflow/actuation-permit.js";
import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import { type FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";

describe("canonical reobservation with pending descendants", () => {
  it("backs off exact rebroadcasts without exhausting the proof objective", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let time = Date.parse("2026-08-29T00:00:00.000Z");
    let finish = false;
    let rebroadcasts = 0;
    let transition: fundingAuthority.WorkflowFundingPreparedTransition | null =
      null;
    const recovery = vi
      .spyOn(fundingAuthority, "readWorkflowFundingRecovery")
      .mockImplementation(async () => ({
        transition,
        submissionHandoff: null,
        completionHandoff: null,
        abandonmentHandoff: null,
      }));
    const adapter = makeAdapter({
      submit: async ({ preflight }) => {
        transition = {
          actionKind: "prove",
          transactionHash: preflight.txHash,
          signedTransactionCborHex: "80",
          transactionBodySha256: "00".repeat(32),
          consumedOutRefs: [],
          producedInputs: [],
        };
        return { kind: "submitted", txHash: preflight.txHash };
      },
      reconcile: async ({
        txHash,
        authorizeResubmission,
        signedTransactionCborHex,
      }) => {
        if (finish) {
          transition = null;
          return { kind: "confirmed", txHash: txHash! };
        }
        if (
          authorizeResubmission === undefined ||
          signedTransactionCborHex === undefined
        )
          throw new Error("expected exact signed recovery authority");
        try {
          await authorizeResubmission({
            transactionHash: txHash!,
            signedTransactionCborHex,
          });
        } catch (cause) {
          return { kind: "unknown", reason: String(cause) };
        }
        rebroadcasts += 1;
        return { kind: "pending", txHash };
      },
    });
    const invoke = () =>
      run({ evidence, adapter, journal, now: () => new Date(time) });
    try {
      expect(await invoke()).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      for (let index = 1; index <= 4; index += 1) {
        time += 29_999;
        expect(await invoke()).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
        });
        expect(rebroadcasts).toBe(index - 1);
        time += 1;
        expect(await invoke()).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
        });
        expect(rebroadcasts).toBe(index);
        expect(await invoke()).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
        });
        expect(rebroadcasts).toBe(index);
      }
      expect(adapter.submit).toHaveBeenCalledTimes(1);
      finish = true;
      const result = await invoke();
      expect(result.kind).toBe("completed");
      if (result.kind !== "completed")
        throw new Error("expected objective completion");
      expect(
        result.entries.flatMap(({ event }) =>
          event.kind === "rebroadcast_intent" ? [event.attempt] : [],
        ),
      ).toEqual([2, 3, 4, 5]);
      expect(adapter.prepare).toHaveBeenCalledTimes(1);
      expect(adapter.submit).toHaveBeenCalledTimes(2);
    } finally {
      recovery.mockRestore();
    }
  });

  it.each(["action changed", "funding unavailable"])(
    "resumes an unsigned %s without recording a transaction attempt",
    async (cause) => {
      const evidence = await canonicalEvidence();
      const journal = new MemoryFraudProofWorkflowJournalStore();
      const adapter = makeAdapter();
      const original = adapter.preflight;
      adapter.preflight = async () => {
        throw cause === "action changed"
          ? new WorkflowActionChangedError("authenticated action changed")
          : new fundingAuthority.WorkflowFundingReservationUnavailableError();
      };
      const pending = await run({ evidence, adapter, journal });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      if (pending.kind !== "pending") throw new Error("expected safe yield");
      expect(
        pending.entries.some(({ event }) => event.kind === "submission_intent"),
      ).toBe(false);
      expect(adapter.submit).not.toHaveBeenCalled();
      adapter.preflight = original;
      expect((await run({ evidence, adapter, journal })).kind).toBe(
        "completed",
      );
      expect(adapter.prepare).toHaveBeenCalledTimes(1);
    },
  );

  it("keeps deterministic preflight failures explicit", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter();
    adapter.preflight = async () => {
      throw new Error("local validator rejected the proof witness");
    };
    expect(await run({ evidence, adapter, journal })).toMatchObject({
      kind: "stalled",
      reason: expect.stringContaining("validator rejected"),
    });
    expect(adapter.submit).not.toHaveBeenCalled();
  });

  it("preserves a signed attempt when its funding disappears before submit", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter();
    const readiness = vi
      .spyOn(fundingAuthority, "assertWorkflowFundingReservationReadyToSubmit")
      .mockRejectedValueOnce(
        new fundingAuthority.WorkflowFundingReservationUnavailableError(),
      );
    try {
      const pending = await run({ evidence, adapter, journal });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        reason: expect.stringContaining(PROOF_TX_HASH),
      });
      if (pending.kind !== "pending")
        throw new Error("expected signed recovery");
      expect(pending.entries.at(-1)?.event).toMatchObject({
        kind: "submission_intent",
        txHash: PROOF_TX_HASH,
      });
      expect(adapter.submit).not.toHaveBeenCalled();
      // The exact intent must be reconciled before another body can be built.
      adapter.reconcile = async ({ txHash }) => ({ kind: "pending", txHash });
      expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
      expect(adapter.preflight).toHaveBeenCalledTimes(1);
      expect(adapter.submit).not.toHaveBeenCalled();
    } finally {
      readiness.mockRestore();
    }
  });

  it("keeps the objective resumable after a bounded batch of retired attempts", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let proofAttempts = 0;
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) =>
        txHash === PROOF_TX_HASH || txHash === REMOVAL_TX_HASH
          ? { kind: "confirmed", txHash }
          : { kind: "not_found" },
    });
    const preflight = adapter.preflight;
    adapter.preflight = async (context) => {
      const prepared = await preflight(context);
      if (context.action.actionId !== "prove") return prepared;
      proofAttempts += 1;
      return {
        ...prepared,
        txHash:
          proofAttempts === 5
            ? PROOF_TX_HASH
            : proofAttempts.toString(16).padStart(64, "0"),
      };
    };

    const first = await run({ evidence, adapter, journal });
    expect(first).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
    });
    expect(proofAttempts).toBe(3);
    const resumed = await run({ evidence, adapter, journal });
    expect(resumed.kind).toBe("completed");
    if (first.kind !== "pending" || resumed.kind !== "completed")
      throw new Error("expected bounded continuation of the same objective");
    expect(resumed.workflowId).toBe(first.workflowId);
    expect(adapter.prepare).toHaveBeenCalledTimes(1);
    expect(
      resumed.entries.flatMap(({ event }) =>
        event.kind === "submission_intent" && event.actionId === "prove"
          ? [event.attempt]
          : [],
      ),
    ).toEqual([1, 2, 3, 4, 5]);
    expect(resumed.entries.some(({ event }) => event.kind === "stalled")).toBe(
      false,
    );
  });

  it("yields uncertain signed recovery and later confirms the same attempt", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    let available = false;
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) =>
        available
          ? { kind: "confirmed", txHash: txHash! }
          : { kind: "unknown", reason: "canonical history is catching up" },
    });
    expect(await run({ evidence, adapter, journal })).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
    });
    expect(adapter.submit).toHaveBeenCalledTimes(1);
    available = true;
    const result = await run({ evidence, adapter, journal });
    expect(result.kind).toBe("completed");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    expect(adapter.prepare).toHaveBeenCalledTimes(1);
  });

  it("yields an admitted ownership collision without changing the submitted journal", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const onChain = new Set<string>();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        if (txHash === REMOVAL_TX_HASH) return { kind: "pending", txHash };
        onChain.add(txHash!);
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    adapter.observe = async () => ({
      kind: "action_required",
      action: onChain.has(PROOF_TX_HASH)
        ? { actionId: "remove", input: { step: 1 } }
        : { actionId: "prove", input: { step: 0 } },
    });
    const first = await run({ evidence, adapter, journal });
    expect(first.kind).toBe("pending");
    onChain.clear();
    const unavailable = vi
      .spyOn(fundingAuthority, "reobserveWorkflowFundingReservationTransaction")
      .mockResolvedValue(false);
    try {
      const pending = await run({ evidence, adapter, journal });
      expect(pending).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
      });
      if (first.kind !== "pending" || pending.kind !== "pending")
        throw new Error("expected pending workflow");
      expect(pending.entries).toEqual(first.entries);
      expect(adapter.submit).toHaveBeenCalledTimes(2);
    } finally {
      unavailable.mockRestore();
    }
  });

  it("keeps unknown read-only reconciliation pending instead of blocking execution", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
    });
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    adapter.reconcile = async () => ({
      kind: "unknown",
      reason: "Exact recorded transaction requires live rebroadcast authority",
    });
    const readonly = vi
      .spyOn(actuationAuthority, "workflowJournalIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, journal })).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining("live rebroadcast authority"),
      });
      expect(adapter.submit).toHaveBeenCalledTimes(1);
    } finally {
      readonly.mockRestore();
    }
  });

  it("waits for fresh authority before replacing an expired read-only attempt", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => ({ kind: "pending", txHash }),
    });
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    adapter.reconcile = async () => ({ kind: "not_found" });
    const readonly = vi
      .spyOn(actuationAuthority, "workflowJournalIsReconciliationOnly")
      .mockReturnValue(true);
    try {
      expect(await run({ evidence, adapter, journal })).toMatchObject({
        kind: "pending",
        reason: "Canonical workflow requires fresh submission authority",
      });
      expect(adapter.preflight).toHaveBeenCalledTimes(1);
      expect(adapter.submit).toHaveBeenCalledTimes(1);
    } finally {
      readonly.mockRestore();
    }
  });

  it("reconciles a rolled-back parent before its already submitted descendant", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const onChain = new Set<string>();
    let finishChild = false;
    const reconciled: string[] = [];
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        reconciled.push(txHash!);
        if (txHash === REMOVAL_TX_HASH && !finishChild)
          return { kind: "pending", txHash };
        onChain.add(txHash!);
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    adapter.observe = async () =>
      onChain.has(REMOVAL_TX_HASH)
        ? { kind: "completed", terminal: terminal(evidence.headerHash) }
        : {
            kind: "action_required",
            action: onChain.has(PROOF_TX_HASH)
              ? { actionId: "remove", input: { step: 1 } }
              : { actionId: "prove", input: { step: 0 } },
          };
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(adapter.submit).toHaveBeenCalledTimes(2);
    onChain.clear();
    finishChild = true;
    reconciled.length = 0;
    expect((await run({ evidence, adapter, journal })).kind).toBe("completed");
    expect(reconciled).toEqual([PROOF_TX_HASH, REMOVAL_TX_HASH]);
    expect(adapter.preflight).toHaveBeenCalledTimes(2);
    expect(adapter.submit).toHaveBeenCalledTimes(2);
  });

  it("rebuilds a stably expired parent and derives a fresh child identity", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const replacementParent = "b1".repeat(32);
    const replacementChild = "b2".repeat(32);
    let parentOnChain: string | undefined;
    let childOnChain = false;
    let expiredParent = false;
    const reconciled: string[] = [];
    const adapter = makeAdapter({
      reconcile: async ({ txHash }) => {
        reconciled.push(txHash!);
        // Signed recovery supplies authenticated stable expiry, not simple absence.
        if (txHash === PROOF_TX_HASH && expiredParent)
          return { kind: "not_found" };
        if (txHash === PROOF_TX_HASH || txHash === replacementParent) {
          parentOnChain = txHash;
          return { kind: "confirmed", txHash };
        }
        if (txHash === REMOVAL_TX_HASH) return { kind: "pending", txHash };
        childOnChain = true;
        return { kind: "confirmed", txHash: txHash! };
      },
    });
    const preflight = adapter.preflight;
    adapter.preflight = vi.fn(async (context) => ({
      ...(await preflight(context)),
      txHash:
        context.action.actionId === "prove"
          ? expiredParent
            ? replacementParent
            : PROOF_TX_HASH
          : parentOnChain === replacementParent
            ? replacementChild
            : REMOVAL_TX_HASH,
    }));
    adapter.observe = async (): Promise<
      Awaited<ReturnType<FraudProofFamilyWorkflowAdapter["observe"]>>
    > => {
      if (childOnChain) {
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
      return {
        kind: "action_required",
        action:
          parentOnChain === undefined
            ? { actionId: "prove", input: { step: 0 } }
            : {
                actionId: `remove:${parentOnChain}`,
                input: { step: 1, parent: parentOnChain },
              },
      };
    };
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    parentOnChain = undefined;
    expiredParent = true;
    reconciled.length = 0;
    expect((await run({ evidence, adapter, journal })).kind).toBe("completed");
    expect(reconciled).toEqual([
      PROOF_TX_HASH,
      replacementParent,
      replacementChild,
    ]);
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
  });
});
