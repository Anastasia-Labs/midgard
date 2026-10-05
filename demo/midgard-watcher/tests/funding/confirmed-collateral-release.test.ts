import { join } from "node:path";

import { beginWorkflowFundingReservationAction } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { unsafeOpenWatcherSqliteProverFundingReservationStoreForTest } from "../../src/funding/sqlite-prover-funding-reservation-store.js";
import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { plan as storePlan } from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const setup = async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const collateral = fixture.plan.inputs.find(
    ({ role }) => role === "collateral",
  )!.outRef;
  const funding = fixture.plan.inputs.find(
    ({ role }) => role === "funding",
  )!.outRef;
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
          input.role === "collateral"
            ? { ...input, outRef: collateralOutRef }
            : input,
        )
        .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
    };
  };
  const other = competitorFor(collateral, `${"ab".repeat(32)}#0`);
  return { fixture, collateral, funding, change, peer, competitorFor, other };
};

it("releases a confirmed proof's collateral lease at confirmation so another reservation can use it", async () => {
  const { fixture, collateral, funding, change, peer, competitorFor, other } =
    await setup();
  try {
    // Pending: the signed attempt's collateral is leased.
    expect((await fixture.records())[0]!.activeInputs).toContainEqual(
      expect.objectContaining({ outRef: collateral, role: "collateral" }),
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
    const [record] = await fixture.records();
    expect(record!.pendingTransition).toBeNull();
    expect(record!.activeInputs.map(({ outRef }) => outRef)).not.toContain(
      collateral,
    );
    const reserved = await peer.store.readReservedOutRefs({});
    expect(reserved).not.toContain(collateral);
    // The consumed input and the change stay leased for a re-land.
    expect(reserved).toEqual(expect.arrayContaining([funding, change]));

    await expect(peer.store.reserve(other)).resolves.toBe("reserved");
    expect(
      await peer.store.readReservedOutRefs({
        excludingReservationId: fixture.plan.reservationId,
      }),
    ).toContain(collateral);
    // This reservation's next action selects fresh collateral at once.
    await beginWorkflowFundingReservationAction({
      journal,
      action: { actionId: "next", input: { actionKind: "step-one" } },
    });
    const nextCollateral = (await fixture.records())[0]!.activeInputs.filter(
      ({ role }) => role === "collateral",
    );
    expect(nextCollateral).toHaveLength(1);
    expect(nextCollateral[0]!.outRef).not.toBe(collateral);
    await expect(
      peer.store.reserve({
        ...competitorFor(`${"ac".repeat(32)}#0`, change),
        reservationId: "ed".repeat(32),
        decisionDigest: "ec".repeat(32),
      }),
    ).rejects.toThrow("already reserved");
  } finally {
    peer.close();
  }
});

it("rebroadcasts the identical bytes after a rollback while another reservation holds the collateral lease", async () => {
  const { fixture, collateral, funding, change, peer, other } = await setup();
  try {
    const journal = await fixture.recover();
    await fixture.run(journal);
    const confirmed = await journal.load(fixture.initial.workflowId);
    expect(confirmed.at(-1)!.event.kind).toBe("confirmed");
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
    const [before] = await fixture.records();
    await expect(
      fixture.store.reobserveTransition!({
        plan: fixture.plan,
        expectedRevision: before!.revision,
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
    const result = await fixture.run(
      journal,
      () => new Date(Date.now() + 60_000),
    );
    expect(result).toMatchObject({ kind: "pending" });
    expect(rebroadcast).toEqual([fixture.signedTransactionCborHex]);
    const [record] = await fixture.records();
    expect(record!.pendingTransition?.transactionHash).toBe(
      fixture.transactionHash,
    );
    const active = record!.activeInputs.map(({ outRef }) => outRef);
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
