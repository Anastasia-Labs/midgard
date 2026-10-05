import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import {
  fundingOf,
  recordReplacement,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

it("supersedes a pending step that a reobserved earlier action displaces, so its replacement must spend its lineage", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const funding = fundingOf(fixture);
  const change = `${fixture.transactionHash}#0`;
  let journal = await fixture.recover();
  await fixture.run(journal);

  // The next step spends the proof thread the confirmed proof created and is
  // still in flight when the process restarts.
  const thread = `${fixture.transactionHash}#1`;
  const next = { actionId: "next", actionKind: "step-one" };
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: next.actionId, input: { actionKind: next.actionKind } },
  });
  const [begun] = await fixture.records();
  const collateral = begun!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  const stepFunding = begun!.activeInputs.find(
    ({ role, outRef }) => role === "funding" && outRef !== funding.outRef,
  )!;
  const step = signReplacement(
    stepFunding.outRef,
    BigInt(stepFunding.lovelace),
    300n,
    collateral,
    [thread],
  );
  await recordReplacement(fixture, journal, step, 1, next);
  journal = await fixture.recover();

  // A rollback within k drops the proof: its funding input is unspent again.
  fixture.walletUtxos.splice(
    0,
    fixture.walletUtxos.length,
    ...fixture.walletUtxos.filter(
      ({ txHash, outputIndex }) => `${txHash}#${outputIndex}` !== change,
    ),
    (() => {
      const [txHash, outputIndex] = funding.outRef.split("#");
      return {
        txHash: txHash!,
        outputIndex: Number(outputIndex),
        address: walletAddress,
        assets: { lovelace: BigInt(funding.lovelace) },
      };
    })(),
  );
  // The chain asks for the proof while the step is still pending; the
  // reobserved proof then lands again, and the chain asks for the step.
  let proofLanded = false;
  vi.mocked(fixture.adapter.observe).mockImplementation(async () =>
    proofLanded
      ? {
          kind: "action_required",
          action: {
            actionId: next.actionId,
            input: { actionKind: next.actionKind },
          },
        }
      : {
          kind: "action_required",
          action: { actionId: "init", input: { actionKind: "proof.init" } },
        },
  );
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async ({ txHash }) => {
      if (txHash === step.transactionHash) return { kind: "pending" };
      proofLanded = true;
      return { kind: "confirmed", txHash: txHash! };
    },
  );
  // The fixture's builder refuses to build, so the step's new attempt stops
  // at its preflight; a real builder signs here.
  await expect(
    fixture.run(journal, () => new Date(Date.now() + 60_000)),
  ).resolves.toMatchObject({ kind: "stalled", phase: "preflight" });
  expect(proofLanded).toBe(true);

  // The displaced step is closed as superseded, not silently dropped.
  const events = (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
  const closed = events.findIndex(
    (event) =>
      event.kind === "reconciled" &&
      event.outcome === "not_found" &&
      event.txHash === step.transactionHash,
  );
  const reobserved = events.findIndex(
    (event) =>
      event.kind === "reobserved" && event.txHash === fixture.transactionHash,
  );
  expect(closed).toBeGreaterThanOrEqual(0);
  expect(reobserved).toBeGreaterThan(closed);
  const sets = await fixture.store.readSupersededAttemptFundingOutRefs!({
    reservationId: fixture.plan.reservationId,
  });
  expect(sets).toHaveLength(1);
  expect(sets[0]).toEqual(
    expect.arrayContaining([stepFunding.outRef, thread, funding.outRef]),
  );

  // Negative: a step that avoids every input of the displaced step's lineage
  // is refused, so the displaced bytes and the replacement cannot both land.
  const [record] = await fixture.records();
  const other = record!.activeInputs.find(
    ({ role, outRef }) =>
      role === "funding" &&
      outRef !== funding.outRef &&
      outRef !== stepFunding.outRef,
  );
  expect(other).toBeDefined();
  await expect(
    fixture.store.prepareTransition({
      handoff: fixture.handoff,
      plan: fixture.plan,
      expectedRevision: record!.revision,
      actionKind: next.actionKind,
      ...signReplacement(
        other!.outRef,
        BigInt(other!.lovelace),
        400n,
        collateral,
      ),
    }),
  ).rejects.toThrow("must share an input with each superseded attempt");
  // Positive: a step that re-spends the displaced step's funding is admitted.
  const resigned = signReplacement(
    stepFunding.outRef,
    BigInt(stepFunding.lovelace),
    400n,
    collateral,
    [thread],
  );
  await recordReplacement(fixture, journal, resigned, 2, next);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    resigned.transactionHash,
  );
});
