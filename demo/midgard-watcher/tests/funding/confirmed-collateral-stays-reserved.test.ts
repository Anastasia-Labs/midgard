import { join } from "node:path";

import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs/workflow/funding-reservation-permit";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.unsafe-open-watcher-sqlite-prover-funding-reservation-store-for-test.js";
import { setupFundingRecoveryFixture } from "../support/fault-proof-funding-fixture.setup-funding-recovery-fixture.js";
import {
  cleanupFundingRecoveryFixtures,
  walletAddress,
} from "../support/fault-proof-funding-fixture.sources-for.js";
import { plan as storePlan } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const setup = async () => {
  const fixture = await setupFundingRecoveryFixture(false, true);
  // The pending attempt posts one reserved coin as collateral and spends
  // the other.
  const body = CML.Transaction.from_cbor_hex(
    fixture.signedTransactionCborHex,
  ).body();
  const outRefOf = (input: CML.TransactionInput) =>
    `${input.transaction_id().to_hex()}#${input.index().toString()}`;
  const collateral = outRefOf(body.collateral_inputs()!.get(0));
  const spent = Array.from({ length: body.inputs().len() }, (_, index) =>
    outRefOf(body.inputs().get(index)),
  );
  const funding = fixture.plan.inputs.find(({ outRef }) =>
    spent.includes(outRef),
  )!.outRef;
  expect(spent).not.toContain(collateral);
  const change = `${fixture.transactionHash}#0`;
  // Only fixture creation uses the unsafe seam: a second reservation that
  // wants the confirmed proof's collateral.
  const peer =
    await unsafeOpenWatcherSqliteProverFundingReservationStoreForTest(
      { path: join(fixture.journalRoot, "watcher.sqlite") },
      () => undefined,
    );
  const competitorFor = (collateralOutRef: string, fundingOutRef: string) => {
    const base = storePlan("ee", "ef", fundingOutRef);
    return {
      ...base,
      inputs: base.inputs
        .map((input) =>
          input.outRef === fundingOutRef
            ? input
            : { ...input, outRef: collateralOutRef },
        )
        .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
    };
  };
  const other = competitorFor(collateral, `${"ab".repeat(32)}#0`);
  // The store lists every reservation, the peer's among them, by id.
  const own = async () =>
    (await fixture.records()).find(
      ({ reservationId }) => reservationId === fixture.plan.reservationId,
    )!;
  return {
    fixture,
    collateral,
    funding,
    change,
    peer,
    competitorFor,
    other,
    own,
  };
};

it("keeps a confirmed step's collateral in its reservation, where the next action uses it again", async () => {
  const { fixture, collateral, funding, change, peer, other, own } =
    await setup();
  try {
    // Pending: the signed attempt's collateral is reserved.
    expect((await own()).activeInputs.map(({ outRef }) => outRef)).toContain(
      collateral,
    );
    await expect(peer.store.reserve(other)).rejects.toThrow("already reserved");

    const journal = await fixture.recover();
    await fixture.run(journal);
    const entries = await fixture.journal.load(fixture.initial.workflowId);
    expect(entries.at(-1)!.event).toEqual({
      kind: "confirmed",
      actionId: "init",
      txHash: fixture.transactionHash,
    });
    // A phase-2-valid transaction leaves its collateral unspent: it stays
    // reserved beside the change, and the spent input is gone.
    const confirmed = await own();
    expect(confirmed.pendingTransition).toBeNull();
    const held = confirmed.activeInputs.map(({ outRef }) => outRef);
    expect(held).toEqual(expect.arrayContaining([collateral, change]));
    expect(held).not.toContain(funding);
    await expect(peer.store.reserve(other)).rejects.toThrow("already reserved");

    // The next action begins on the reservation as it is: nothing is
    // reserved again, and the coin is still there to post.
    await beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "next", input: { actionKind: "step-one" } },
    });
    const next = await own();
    expect(next.revision).toBe(confirmed.revision);
    expect(next.activeInputs.map(({ outRef }) => outRef)).toContain(collateral);
  } finally {
    peer.close();
  }
});

it("rebroadcasts the identical bytes after a rollback while another reservation holds the collateral its inclusion released", async () => {
  const { fixture, collateral, funding, change, peer, other, own } =
    await setup();
  try {
    const journal = await fixture.recover();
    await fixture.run(journal);
    const confirmed = await journal.load(fixture.initial.workflowId);
    expect(confirmed.at(-1)!.event.kind).toBe("confirmed");
    // The correction's inclusion frees what the reservation holds; another
    // reservation takes the collateral.
    await fixture.store.releaseProducedOutputs!({
      journal: [],
      plan: fixture.plan,
      expectedRevision: (await own()).revision,
    });
    await peer.store.reserve(other);

    // A rollback within k drops the proof: its funding input is unspent again
    // and its change is gone. The collateral is still unspent.
    const [fundingHash, fundingIndex] = funding.split("#");
    fixture.walletUtxos.splice(
      fixture.walletUtxos.findIndex(
        ({ txHash, outputIndex }) => `${txHash}#${outputIndex}` === change,
      ),
      1,
      {
        txHash: fundingHash!,
        outputIndex: Number(fundingIndex),
        address: walletAddress,
        assets: {
          lovelace: BigInt(
            fixture.plan.inputs.find(({ outRef }) => outRef === funding)!
              .lovelace,
          ),
        },
      },
    );

    // Reclaiming collateral another reservation holds is refused at the store.
    const before = await own();
    await expect(
      fixture.store.reobserveTransition!({
        journal: [],
        plan: fixture.plan,
        expectedRevision: before.revision,
        transactionHash: fixture.transactionHash,
        inputs: fixture.plan.inputs,
      }),
    ).rejects.toThrow("already reserved");

    vi.mocked(fixture.adapter.observe).mockResolvedValue({
      kind: "action_required",
      action: { actionId: "init", input: { actionKind: "proof.init" } },
    });
    const rebroadcast: string[] = [];
    vi.mocked(fixture.adapter.reconcile).mockImplementation(
      async ({ txHash, signedTransactionCborHex, authorizeResubmission }) => {
        if (
          signedTransactionCborHex === undefined ||
          authorizeResubmission === undefined
        )
          throw new Error("reobserved attempt lost its exact signed bytes");
        await authorizeResubmission({
          transactionHash: txHash!,
          signedTransactionCborHex,
        });
        rebroadcast.push(signedTransactionCborHex);
        return { kind: "pending", txHash: txHash! };
      },
    );
    // The watcher resumes on the reservation as the inclusion left it.
    const resumed = await fixture.recover();
    const result = await fixture.run(
      resumed,
      () => new Date(Date.now() + 60_000),
    );
    expect(result).toMatchObject({ kind: "pending" });
    expect(rebroadcast).toEqual([fixture.signedTransactionCborHex]);
    const record = await own();
    expect(record.pendingTransition?.transactionHash).toBe(
      fixture.transactionHash,
    );
    const active = record.activeInputs.map(({ outRef }) => outRef);
    expect(active).toContain(funding);
    expect(active).not.toContain(collateral);
    expect(
      await peer.store.readReservedOutRefs({
        excludingReservationId: fixture.plan.reservationId,
      }),
    ).toContain(collateral);
    expect(fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(fixture.adapter.submit).not.toHaveBeenCalled();
  } finally {
    peer.close();
  }
});
