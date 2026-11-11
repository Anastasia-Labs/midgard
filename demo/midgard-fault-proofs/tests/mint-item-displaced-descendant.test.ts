import { afterEach, expect, it, vi } from "vitest";

import * as fundingAuthority from "../src/workflow/funding-reservation-permit.js";
import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
} from "./workflow.make-adapter.js";

afterEach(() => {
  vi.restoreAllMocks();
});

// Mint's step_02 spends step_01's output. The shared runner meets the same
// shape in any parent and descendant: here `prove` is step_01 and `remove`
// is step_02.
it("supersedes a pending step02 before reobserving the step01 a rollback dropped", async () => {
  const evidence = await canonicalEvidence();
  const journal = new MemoryFraudProofWorkflowJournalStore();
  const onChain = new Set<string>();
  let rolledBack = false;
  const adapter = makeAdapter({
    reconcile: async ({ txHash, reconciliationOnly }) => {
      // step01 lands until the rollback; step02 never does.
      if (txHash === PROOF_TX_HASH && !rolledBack) onChain.add(txHash);
      if (onChain.has(txHash!)) return { kind: "confirmed", txHash: txHash! };
      if (reconciliationOnly)
        return { kind: "unknown", reason: "transaction is not on chain" };
      // step01 may still be in the mempool.
      return { kind: "pending", txHash: txHash! };
    },
  });
  adapter.observe = async () => ({
    kind: "action_required",
    action: onChain.has(PROOF_TX_HASH)
      ? { actionId: "remove", input: { step: 1 } }
      : { actionId: "prove", input: { step: 0 } },
  });
  // step01 confirms; step02, which spends step01's output, is in flight.
  expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
  expect(adapter.submit).toHaveBeenCalledTimes(2);

  // The funding authority is spied: it holds step02 as the pending attempt
  // and refuses to abandon any other, exactly as the bound permit does.
  let pending = REMOVAL_TX_HASH;
  const calls: string[] = [];
  vi.spyOn(fundingAuthority, "readWorkflowFundingRecovery").mockImplementation(
    async () => ({
      transition: {
        actionKind: "proof.init",
        transactionHash: pending,
        signedTransactionCborHex: "00",
        transactionBodySha256: "0".repeat(64),
        consumedOutRefs: [],
        producedInputs: [],
      },
      submissionHandoff: null,
      completionHandoff: null,
      abandonmentHandoff: null,
    }),
  );
  vi.spyOn(
    fundingAuthority,
    "abandonWorkflowFundingReservationTransaction",
  ).mockImplementation(
    async ({ transactionHash, retirement, writer, events }) => {
      if (transactionHash !== pending)
        throw new Error(
          "funding abandonment changed its exact transaction identity",
        );
      expect(retirement).toBeUndefined();
      calls.push(`abandon ${transactionHash}`);
      await fundingAuthority.appendWorkflowJournalEvents(writer, events);
    },
  );
  vi.spyOn(
    fundingAuthority,
    "reobserveWorkflowFundingReservationTransaction",
  ).mockImplementation(async ({ transactionHash, writer, events }) => {
    calls.push(`reobserve ${transactionHash}`);
    pending = transactionHash;
    await fundingAuthority.appendWorkflowJournalEvents(writer, events);
    return true;
  });

  // A rollback drops step01, and the chain is back at its source stage.
  rolledBack = true;
  onChain.clear();
  const rerun = await run({ evidence, adapter, journal });
  if (rerun.kind !== "pending") throw new Error(`rerun ended ${rerun.kind}`);
  // step02 is superseded (its inputs become an exclusion set in the bound
  // store) before step01 is reobserved; no identity wedge follows.
  expect(calls).toEqual([
    `abandon ${REMOVAL_TX_HASH}`,
    `reobserve ${PROOF_TX_HASH}`,
  ]);
  const events = (await journal.load(rerun.workflowId)).map(
    ({ event }) => event,
  );
  const closed = events.findIndex(
    (event) =>
      event.kind === "reconciled" &&
      event.outcome === "not_found" &&
      event.txHash === REMOVAL_TX_HASH,
  );
  const reobserved = events.findIndex(
    (event) => event.kind === "reobserved" && event.txHash === PROOF_TX_HASH,
  );
  expect(closed).toBeGreaterThanOrEqual(0);
  expect(reobserved).toBeGreaterThan(closed);
  // A second pass reconciles only step01 and supersedes nothing again.
  expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
  expect(calls).toHaveLength(2);
  expect(adapter.submit).toHaveBeenCalledTimes(2);
});
