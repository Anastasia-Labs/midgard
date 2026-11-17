/**
 * Bytes the journal records as landed failing phase 2 are never sent again.
 * The process stops after the forfeiture commits and before the attempt is
 * superseded; a rollback then removes the landing, so the L1 source reports
 * the attempt absent with every input live, as one that could be sent again.
 */
import {
  type FraudProofWorkflowJournalEvent,
  reconcileSignedWorkflowTransaction,
  type SignedWorkflowTransaction,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  FixtureProcessStop,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

it("never sends again an attempt whose phase-2-invalid landing a rollback removed after its forfeiture, and waits for its validity window without holding the objective", async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    true,
    false,
    20n,
    44,
    3,
    100n,
  );
  const sent: string[] = [];
  // The family reads its recorded attempt as production does: through the
  // signed-transaction reconciliation over the L1 source's signed read.
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async ({ txHash, signedTransactionCborHex, authorizeResubmission }) =>
      await reconcileSignedWorkflowTransaction({
        transactionHash: txHash!,
        ...(signedTransactionCborHex === undefined
          ? {}
          : { signedTransactionCborHex }),
        observe: fixture.observeSignedTransaction,
        rebroadcast: async (
          signed: SignedWorkflowTransaction & {
            readonly authorizeResubmission: (
              input: SignedWorkflowTransaction,
            ) => Promise<void>;
          },
        ) => {
          await signed.authorizeResubmission(signed);
          sent.push(signed.transactionHash);
          return signed.transactionHash;
        },
        ...(authorizeResubmission === undefined
          ? {}
          : { authorizeResubmission }),
      }),
  );
  const events = async (): Promise<FraudProofWorkflowJournalEvent[]> =>
    (await fixture.journal.load(fixture.initial.workflowId)).map(
      ({ event }) => event,
    );

  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  fixture.stopAt("abandonPendingTransition", "before");
  await expect(fixture.run(await fixture.recover())).rejects.toBeInstanceOf(
    FixtureProcessStop,
  );
  const forfeited = await events();
  expect(forfeited.at(-1)).toMatchObject({
    kind: "collateral_forfeited",
    txHash: fixture.transactionHash,
  });
  await fixture.restartStore();
  const journal = await fixture.recover();

  // The rollback: absent, every input live, inside its validity window. The
  // last broadcast is past its retry interval.
  fixture.useUnspentPendingInputs();
  fixture.chain.mockResolvedValue({
    kind: "unknown",
    reason: "absent",
    absent: true,
  });
  const later = () => new Date(Date.now() + 60_000);
  await expect(fixture.run(journal, later)).resolves.toMatchObject({
    kind: "pending",
    resumeOnObservation: true,
  });
  expect(sent).toEqual([]);
  expect((await events()).slice(forfeited.length)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
  ]);

  // Its validity window ends: the attempt is superseded.
  fixture.chain.mockResolvedValue({ kind: "not_found" });
  await fixture.run(journal, later);
  expect(sent).toEqual([]);
  const settled = await events();
  expect(settled.slice(forfeited.length + 1)).toEqual([
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(settled.some(({ kind }) => kind === "rebroadcast_intent")).toBe(false);
});
