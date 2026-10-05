import { describe, expect, it, vi } from "vitest";

import { setupFundingRecoveryFixture as setup } from "../support/fault-proof-funding-fixture.js";
import { expiredNotFound } from "./prover-funding-recovery.retirement-fixture.js";

describe("interrupted funding retirement", () => {
  it.each(["before_not_found", "after_not_found"] as const)(
    "recovers abandonment interrupted %s with exact signed bytes",
    async (boundary) => {
      const test = await setup(false, true);
      test.useUnspentPendingInputs();
      vi.mocked(test.adapter.reconcile).mockImplementation(
        async ({ txHash, signedTransactionCborHex }) => {
          expect(txHash).toBe(test.transactionHash);
          expect(signedTransactionCborHex).toBe(test.signedTransactionCborHex);
          return expiredNotFound(test.transactionHash);
        },
      );
      const journal = await test.recover();
      const append = journal.append.bind(journal);
      const crash = new Error(`crash ${boundary}`);
      vi.spyOn(journal, "append").mockImplementation(
        async (entry, sequence) => {
          if (
            entry.event.kind !== "reconciled" ||
            entry.event.outcome !== "not_found"
          )
            return append(entry, sequence);
          expect((await test.records())[0]).toMatchObject({
            revision: "2",
            pendingTransition: null,
            activeInputs: test.pending.activeInputs,
          });
          expect(
            await test.store.readAbandonmentHandoff({
              reservationId: test.plan.reservationId,
            }),
          ).toMatchObject({
            transition: {
              signedTransactionCborHex: test.signedTransactionCborHex,
              transactionHash: test.transactionHash,
            },
            handoff: { reconciliation: entry.event },
          });
          if (boundary === "after_not_found") await append(entry, sequence);
          throw crash;
        },
      );
      await expect(test.run(journal)).rejects.toBe(crash);
      expect(test.adapter.observe).not.toHaveBeenCalled();
      await test.restartStore();
      vi.mocked(test.adapter.reconcile).mockResolvedValueOnce({
        kind: "pending",
        txHash: test.transactionHash,
      });
      expect(await test.run(await test.recover())).toMatchObject({
        kind: "stalled",
      });
      expect((await test.records())[0]).toMatchObject({
        revision: "2",
        pendingTransition: null,
      });
      expect(
        await test.store.readAbandonmentHandoff({
          reservationId: test.plan.reservationId,
        }),
      ).not.toBeNull();
      expect(test.adapter.observe).not.toHaveBeenCalled();
      expect(await test.run(await test.recover())).toMatchObject({
        kind: "pending",
        workflowId: test.initial.workflowId,
      });
      const [acknowledged] = await test.records();
      expect(acknowledged).toMatchObject({
        revision: "4",
        pendingTransition: null,
        activeInputs: [],
      });
      expect(
        await test.store.readAbandonmentHandoff({
          reservationId: test.plan.reservationId,
        }),
      ).toBeNull();
      const entries = await test.journal.load(test.initial.workflowId);
      expect(
        entries.filter(
          ({ event }) =>
            event.kind === "reconciled" && event.outcome === "not_found",
        ),
      ).toHaveLength(1);
      expect(
        entries
          .filter(({ event }) => event.kind === "submission_intent")
          .map(({ event }) => event),
      ).toEqual([test.handoff.submissionIntent]);
      expect(test.adapter.reconcile).toHaveBeenCalledTimes(3);
      expect(test.adapter.preflight).not.toHaveBeenCalled();
      expect(test.adapter.submit).not.toHaveBeenCalled();
      await test.restartStore();
      await test.run(await test.recover());
      expect(await test.records()).toEqual([acknowledged]);
      expect(test.adapter.reconcile).toHaveBeenCalledTimes(3);
    },
  );
});
