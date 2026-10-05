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

it("re-signs a rolled-back proof at once after a rollback also drops the next step that spent its output", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const funding = fundingOf(fixture);
  const change = `${fixture.transactionHash}#0`;
  let journal = await fixture.recover();
  await fixture.run(journal);
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event.kind,
  ).toBe("confirmed");

  // The next step spends the proof thread the confirmed proof created.
  const thread = `${fixture.transactionHash}#1`;
  const next = { actionId: "next", actionKind: "step-one" };
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: next.actionId, input: { actionKind: next.actionKind } },
  });
  const [begun] = await fixture.records();
  const stepFunding = begun!.activeInputs.find(
    ({ role, outRef }) => role === "funding" && outRef !== funding.outRef,
  )!;
  const step = signReplacement(
    stepFunding.outRef,
    BigInt(stepFunding.lovelace),
    300n,
    begun!.activeInputs
      .filter(({ role }) => role === "collateral")
      .map(({ outRef }) => outRef),
    [thread],
  );
  await recordReplacement(fixture, journal, step, 1, next);
  // The process restarts with the step in flight.
  journal = await fixture.recover();

  // A rollback within k drops both: the proof's funding input is unspent
  // again, and its change and the thread the step spends no longer exist.
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
  // Neither the step nor the proof can land at the tip. The step is
  // reconciled first; then the chain asks for the proof again.
  vi.mocked(fixture.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
  await expect(fixture.run(journal)).resolves.toMatchObject({
    kind: "pending",
  });
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "action_required",
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  // The fixture's builder refuses to build, so the new attempt stops at its
  // preflight; a real builder signs here.
  await expect(
    fixture.run(journal, () => new Date(Date.now() + 60_000)),
  ).resolves.toMatchObject({
    kind: "stalled",
    phase: "preflight",
  });
  const events = (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
  for (const txHash of [step.transactionHash, fixture.transactionHash])
    expect(events).toContainEqual(
      expect.objectContaining({
        kind: "reconciled",
        outcome: "not_found",
        txHash,
      }),
    );
  // Both are superseded. The step's exclusion set reaches the proof's funding
  // input through its lineage, so one shared input excludes both, and the
  // orchestrator went on to a new attempt without waiting for k.
  expect(fixture.adapter.preflight).toHaveBeenCalled();
  const sets = await fixture.store.readSupersededAttemptFundingOutRefs!({
    reservationId: fixture.plan.reservationId,
  });
  expect(sets).toHaveLength(2);
  for (const set of sets) expect(set).toContain(funding.outRef);
  expect(sets.some((set) => set.includes(thread))).toBe(true);
  const [record] = await fixture.records();
  const collateral = record!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  const other = record!.activeInputs.find(
    ({ role, outRef }) =>
      role === "funding" &&
      outRef !== funding.outRef &&
      outRef !== stepFunding.outRef,
  );
  expect(other).toBeDefined();
  // Negative: a proof that spends neither attempt's lineage is refused.
  await expect(
    fixture.store.prepareTransition({
      handoff: fixture.handoff,
      plan: fixture.plan,
      expectedRevision: record!.revision,
      actionKind: "proof.init",
      ...signReplacement(
        other!.outRef,
        BigInt(other!.lovelace),
        400n,
        collateral,
      ),
    }),
  ).rejects.toThrow("must share an input with each superseded attempt");
  const resigned = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    400n,
    collateral,
  );
  await recordReplacement(fixture, journal, resigned, 2);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    resigned.transactionHash,
  );
});
