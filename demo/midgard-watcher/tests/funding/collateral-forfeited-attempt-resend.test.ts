/**
 * Bytes the journal records as landed failing phase 2 are never sent again,
 * and they never hold the objective. The process stops after the forfeiture
 * commits and before the attempt is superseded; a rollback then removes the
 * landing, so the L1 source reports the attempt absent with every input
 * live, as one that could be sent again. An attempt with no upper validity
 * bound reads that way for as long as its inputs stay live.
 */
import {
  createWorkflowActuationPermitController,
  FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
  FraudProofL1UnavailableError,
  type FraudProofWorkflowJournalEvent,
  reconcileSignedWorkflowTransaction,
  type SignedWorkflowTransaction,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { FixtureProcessStop } from "../support/fault-proof-funding-fixture.process-stop.js";
import { setupFundingRecoveryFixture } from "../support/fault-proof-funding-fixture.setup-funding-recovery-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.sources-for.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

const setup = async (): Promise<{
  readonly fixture: Fixture;
  readonly sent: string[];
}> => {
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
  return { fixture, sent };
};

const eventsOf =
  (fixture: Fixture) => async (): Promise<FraudProofWorkflowJournalEvent[]> =>
    (await fixture.journal.load(fixture.initial.workflowId)).map(
      ({ event }) => event,
    );

it("never sends again an attempt whose phase-2-invalid landing a rollback removed, and supersedes it at once although it has no upper validity bound", async () => {
  const { fixture, sent } = await setup();
  const events = eventsOf(fixture);

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

  // The rollback: absent, every input live, and no upper bound ever ends the
  // attempt. The last broadcast is past its retry interval.
  fixture.useUnspentPendingInputs();
  fixture.chain.mockResolvedValue({
    kind: "unknown",
    reason: "absent",
    absent: true,
  });
  const replacement = { actionId: "init", input: { actionKind: "proof.init" } };
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "action_required",
    action: replacement,
  });
  // The test ends at the replacement's preflight, once it is funded.
  const ended = new FraudProofL1UnavailableError("test ends at preflight");
  vi.mocked(fixture.adapter.preflight).mockRejectedValue(ended);
  const later = () => new Date(Date.now() + 60_000);

  await expect(fixture.run(journal, later)).rejects.toBe(ended);
  expect(sent).toEqual([]);
  const settled = await events();
  expect(settled.slice(forfeited.length)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(settled.some(({ kind }) => kind === "rebroadcast_intent")).toBe(false);
  expect(fixture.adapter.preflight).toHaveBeenCalledTimes(1);
  expect(vi.mocked(fixture.adapter.preflight).mock.calls[0]![0].action).toEqual(
    replacement,
  );
  // Whichever lands wins: the replacement spends one of the attempt's inputs.
  const funded = (await fixture.records())[0]!.activeInputs.map(
    ({ outRef }) => outRef,
  );
  expect(
    funded.filter((outRef) =>
      [fixture.fundingInput.outRef, fixture.collateralInput.outRef].includes(
        outRef,
      ),
    ),
  ).not.toEqual([]);
});

it("supersedes such an attempt of a historical objective and releases its funding, sending nothing", async () => {
  const { fixture, sent } = await setup();
  const events = eventsOf(fixture);
  // The objective is historical: its header no longer holds the execution
  // slot, so the run holds reconciliation-only authority throughout.
  const historical = async () => {
    const controller = createWorkflowActuationPermitController({
      decision: fixture.fresh,
      rollbackGeneration: "2",
    });
    controller.restrictToReconciliation(
      "another header owns the execution slot",
    );
    return fixture.bind(
      fixture.fresh,
      await fixture.createPermit(fixture.fresh, "2", controller),
    );
  };

  fixture.chain.mockResolvedValue({ kind: "not_found", landedInvalid: true });
  fixture.stopAt("abandonPendingTransition", "before");
  await expect(fixture.run(await historical())).rejects.toBeInstanceOf(
    FixtureProcessStop,
  );
  const forfeited = await events();
  expect(forfeited.at(-1)).toMatchObject({
    kind: "collateral_forfeited",
    txHash: fixture.transactionHash,
  });
  await fixture.restartStore();

  // The rollback: absent, every input live, no upper bound ever ends it.
  fixture.useUnspentPendingInputs();
  fixture.chain.mockResolvedValue({
    kind: "unknown",
    reason: "absent",
    absent: true,
  });
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "action_required",
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const later = () => new Date(Date.now() + 60_000);

  // Repeated observations reach the same end.
  for (let observation = 0; observation < 2; observation++) {
    await fixture.restartStore();
    await expect(fixture.run(await historical(), later)).resolves.toMatchObject(
      {
        kind: "pending",
        reason: FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
      },
    );
  }
  expect(sent).toEqual([]);
  const settled = await events();
  expect(settled.slice(forfeited.length)).toEqual([
    {
      kind: "collateral_forfeiture_reversed",
      actionId: "init",
      txHash: fixture.transactionHash,
    },
    {
      kind: "reconciled",
      actionId: "init",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    },
  ]);
  expect(settled.some(({ kind }) => kind === "rebroadcast_intent")).toBe(false);
  // The attempt's coins are no longer held: the reservation is idle.
  expect((await fixture.records())[0]).toMatchObject({
    state: "active",
    activeInputs: [],
    pendingTransition: null,
  });
  expect(fixture.adapter.preflight).not.toHaveBeenCalled();
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});
