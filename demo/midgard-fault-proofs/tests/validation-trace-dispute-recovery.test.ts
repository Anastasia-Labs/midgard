import { afterEach, expect, it, vi } from "vitest";

import { validationTraceFieldCarriageAction } from "../src/validation-dispute/workflow-field-carriage.js";
import {
  action,
  durableRecovery,
  mechanics,
  observation,
  policy,
  recordedRecovery,
  signed,
  terminal,
} from "./support/validation-trace-dispute-recovery.js";

const hash = (byte: string) => byte.repeat(32);
const target = `${hash("34")}#0`;
const child = `${hash("35")}#0`;
const proof = `${hash("36")}#0`;

afterEach(() => vi.restoreAllMocks());

it("distinguishes re-included init inputs and successive descendant removal inputs", () => {
  const first = validationTraceFieldCarriageAction({
    stage: "init",
    stateQueueBlockOutRef: target,
  });
  const reIncluded = validationTraceFieldCarriageAction({
    stage: "init",
    stateQueueBlockOutRef: `${hash("37")}#0`,
  });
  expect(first.actionId).not.toBe(reIncluded.actionId);
  const finalRemoval = validationTraceFieldCarriageAction({
    stage: "remove",
    stateQueueBlockOutRef: target,
    nextRemovalOutRef: target,
    fraudProofOutRef: proof,
  });
  expect(action.actionId).not.toBe(finalRemoval.actionId);
  expect(action.input.requiresMutationLease).toBe(true);
  expect(finalRemoval.input.requiresMutationLease).toBe(false);
});

it("rebroadcasts only the exact durable signed body after a crash between intent and network submission", async () => {
  const f = mechanics();
  f.l1.observeSignedTransaction.mockResolvedValue(observation("rebroadcast"));
  const authorizeResubmission = vi.fn(async () => {});
  const result = await f.adapter.reconcile({
    ...f.context,
    action,
    txHash: signed.transactionHash,
    durableRecovery,
    signedTransactionCborHex: signed.signedTransactionCborHex,
    authorizeResubmission,
  });
  expect(result).toEqual({ kind: "pending", txHash: signed.transactionHash });
  expect(f.resume).toHaveBeenCalledExactlyOnceWith(
    durableRecovery.stateQueueMutationLease,
  );
  expect(authorizeResubmission).toHaveBeenCalledExactlyOnceWith(signed);
  expect(f.l1.rebroadcastSignedTransaction.mock.calls[0]?.[0]).toMatchObject(
    signed,
  );
  expect(f.lease.renew).toHaveBeenCalled();
  expect(f.lease.release).not.toHaveBeenCalled();
  expect(f.forbidden).not.toHaveBeenCalled();
});

it.each(["pending", "unknown"] as const)(
  "keeps %s outcomes unresolved without rebuilding or releasing the lease",
  async (status) => {
    const f = mechanics();
    f.l1.observeSignedTransaction.mockResolvedValue(observation(status));
    const result = await f.adapter.reconcile({
      ...f.context,
      action,
      txHash: signed.transactionHash,
      durableRecovery,
      signedTransactionCborHex: signed.signedTransactionCborHex,
    });
    expect(result.kind).toBe(status);
    expect(f.lease.renew).toHaveBeenCalledOnce();
    expect(f.lease.release).not.toHaveBeenCalled();
    expect(f.l1.rebroadcastSignedTransaction).not.toHaveBeenCalled();
  },
);

it.each(["expired", "invalidated"] as const)(
  "releases an %s exact intent for replanning",
  async (status) => {
    const f = mechanics();
    f.l1.observeSignedTransaction.mockResolvedValue(observation(status));
    expect(
      await f.adapter.reconcile({
        ...f.context,
        action,
        txHash: signed.transactionHash,
        durableRecovery,
        signedTransactionCborHex: signed.signedTransactionCborHex,
      }),
    ).toEqual({ kind: "not_found" });
    expect(f.lease.release).toHaveBeenCalledOnce();
    expect(f.l1.rebroadcastSignedTransaction).not.toHaveBeenCalled();
  },
);

it("refuses replay after the canonical action changes and never grants replay under reconciliation-only authority", async () => {
  const f = mechanics();
  f.l1.observeSignedTransaction.mockResolvedValue(observation("rebroadcast"));
  f.setStage({
    kind: "proof_token",
    stateQueueBlockOutRef: `${hash("37")}#0`,
    nextRemovalOutRef: `${hash("37")}#0`,
    fraudProofOutRef: proof,
  });
  const authorizeResubmission = vi.fn(async () => {});
  const context = {
    ...f.context,
    action,
    txHash: signed.transactionHash,
    durableRecovery,
    signedTransactionCborHex: signed.signedTransactionCborHex,
  };
  expect(
    (await f.adapter.reconcile({ ...context, authorizeResubmission })).kind,
  ).toBe("unknown");
  expect(authorizeResubmission).not.toHaveBeenCalled();
  expect(
    (await f.adapter.reconcile({ ...context, reconciliationOnly: true })).kind,
  ).toBe("unknown");
  expect(f.lease.release).not.toHaveBeenCalled();
});

it("authenticates reversible inclusion, then closes the journal and funding at release finality after a cold recovery", async () => {
  const f = await recordedRecovery();
  f.confirmed.mockResolvedValue(true);
  f.setStage({ kind: "removed" });
  f.setTerminal(terminal(1));
  expect((await f.execute()).kind).toBe("terminal_included");
  expect(f.lease.release).toHaveBeenCalledOnce();
  expect(f.releaseFunding.mock.calls.at(-1)?.[0].handoff.completion.kind).toBe(
    "terminal_included",
  );
  expect(
    (await f.journal.load(f.workflowId)).some(
      ({ event }) => event.kind === "completed",
    ),
  ).toBe(false);
  f.setTerminal(terminal(policy.confirmationDepth));
  expect((await f.execute()).kind).toBe("completed");
  expect(f.releaseFunding.mock.calls.at(-1)?.[0].handoff.completion.kind).toBe(
    "completed",
  );
  expect(
    (await f.journal.load(f.workflowId)).filter(
      ({ event }) => event.kind === "completed",
    ),
  ).toHaveLength(1);
  expect(f.forbidden).not.toHaveBeenCalled();
});

it("requires canonical authority before replaying a rolled-back removal and keeps its funding reserved", async () => {
  const f = await recordedRecovery();
  f.confirmed.mockResolvedValue(true);
  f.setStage({ kind: "removed" });
  f.setTerminal(terminal(1));
  expect((await f.execute()).kind).toBe("terminal_included");
  f.confirmed.mockResolvedValue(false);
  f.setTerminal(undefined);
  f.setStage({
    kind: "proof_token",
    stateQueueBlockOutRef: target,
    nextRemovalOutRef: child,
    fraudProofOutRef: proof,
  });
  const result = await f.execute();
  expect(result.kind).toBe("pending");
  expect(f.l1.rebroadcastSignedTransaction).not.toHaveBeenCalled();
  expect(
    f.releaseFunding.mock.calls.filter(
      ([input]) => input.handoff.completion.kind === "completed",
    ),
  ).toHaveLength(0);
  f.controller.revoke("native_chain_rollback");
  await expect(f.execute()).rejects.toThrow("revoked");
  expect(f.forbidden).not.toHaveBeenCalled();
});

it("refuses a removed cursor when independently authenticated terminal effects disagree", async () => {
  const f = await recordedRecovery();
  f.confirmed.mockResolvedValue(true);
  f.setStage({ kind: "removed" });
  f.setTerminal(terminal(policy.confirmationDepth));
  const candidate = {
    provenance: {
      trustClass: "authenticated_cardano_l1" as const,
      sourceId: "controlled-L1",
      grade: "security" as const,
    },
    stage: {
      kind: "removed" as const,
      terminal: terminal(policy.confirmationDepth),
    },
  };
  // The runner observes once before reconciliation, then again after recording
  // confirmation. Its independent terminal verifier performs the third read.
  f.l1.observe
    .mockResolvedValueOnce(candidate)
    .mockResolvedValueOnce(candidate);
  f.l1.observe.mockResolvedValue({
    provenance: {
      trustClass: "authenticated_cardano_l1",
      sourceId: "controlled-L1",
      grade: "security",
    },
    stage: {
      kind: "proof_token",
      stateQueueBlockOutRef: target,
      nextRemovalOutRef: child,
      fraudProofOutRef: proof,
    },
  });
  expect((await f.execute()).kind).toBe("stalled");
  expect(f.releaseFunding).not.toHaveBeenCalled();
});
