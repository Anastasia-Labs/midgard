/**
 * What the supervisor does at a completed pass at tip for objectives no run
 * drives any more: the in-flight part of one whose header left the queue is
 * followed until nothing in it may land, and a proof-materials sweep that
 * failed is run again while the views report materials to sweep.
 */
import { syntheticEmulatorPoint } from "@al-ft/midgard-fault-proofs/test-support/cross-block-raw-emulator";
import type { SignedTransactionRecoveryObservation } from "@al-ft/midgard-fault-proofs/workflow/signed-transaction-reconciliation";
import { expect, it, vi } from "vitest";

import type { WatcherProofMaterialsSweepable } from "../../src/fault-proofs/fault-proof-supervisor.proof-materials-sweep.js";
import { recordingProofRetention } from "../support/proof-retention.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

const POINT = syntheticEmulatorPoint(10, 200);

export const registerReleasedInFlightTests = (
  setupScenario: typeof setup,
): void => {
  it("follows the send of an objective whose header left the queue with no removal of ours, keeps it while it may land, and forgets it at the next pass at tip once another removal spent its input", async () => {
    const test = await setupScenario();
    const { retention } = recordingProofRetention({ atTip: true });
    let status: SignedTransactionRecoveryObservation["status"] = "pending";
    const observeSignedTransaction = vi.fn(
      async (input: {
        transactionHash: string;
        signedTransactionCborHex: string;
      }): Promise<SignedTransactionRecoveryObservation> => ({
        ...input,
        status,
        canonicalPoint: POINT,
        releaseFinalPoint: POINT,
        inputs: [],
        reason: status,
      }),
    );
    test.sendOnRun();
    const supervisor = test.createSupervisor(retention, {
      observeSignedTransaction,
    });
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.sending()).toBe(true);
    // Its run follows it while the objective is driven.
    expect(observeSignedTransaction).not.toHaveBeenCalled();

    // The header leaves the queue and the chain shows no removal of ours:
    // the objective is released, its send still waiting in the mempool.
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    await vi.waitFor(() => expect(observeSignedTransaction).toHaveBeenCalled());
    expect(supervisor.status()).toMatchObject({ unfinishedObjectiveCount: 0 });
    expect(test.sending()).toBe(true);

    // Another prover's removal spent its input: the next pass forgets it.
    status = "invalidated_at_tip";
    await test.request(supervisor, 4, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    await vi.waitFor(() => expect(test.sending()).toBe(false));
    expect(test.runOrResume).toHaveBeenCalledOnce();
  });

  it("runs a failed proof-materials sweep again at the next pass at tip while the views report materials, and no more once they report none", async () => {
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      const test = await setupScenario();
      const { retention } = recordingProofRetention({ atTip: true });
      let found: Awaited<ReturnType<WatcherProofMaterialsSweepable>> = "some";
      const proofMaterialsSweepable = vi.fn(async () => found);
      test.sweepProofMaterials.mockRejectedValueOnce(new Error("sweep lost"));
      const supervisor = test.createSupervisor(retention, {
        proofMaterialsSweepable,
      });
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
      test.landRemoval();
      await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
      await test.idle(supervisor);
      await vi.waitFor(() =>
        expect(test.sweepProofMaterials).toHaveBeenCalledOnce(),
      );
      await vi.waitFor(() =>
        expect(String(stderr.mock.calls.at(-1)?.[0])).toContain("sweep lost"),
      );

      // The next completed pass at tip sweeps again.
      await test.request(supervisor, 4, test.fixture.fresh, false).accepted;
      await test.idle(supervisor);
      await vi.waitFor(() =>
        expect(test.sweepProofMaterials).toHaveBeenCalledTimes(2),
      );

      // Once the views report nothing to sweep, no pass sweeps again.
      found = "none";
      await test.request(supervisor, 5, test.fixture.fresh, false).accepted;
      await test.idle(supervisor);
      await vi.waitFor(() =>
        expect(proofMaterialsSweepable.mock.calls.length).toBeGreaterThan(1),
      );
      found = "some";
      await test.request(supervisor, 6, test.fixture.fresh, false).accepted;
      await test.idle(supervisor);
      await supervisor.close();
      expect(test.sweepProofMaterials).toHaveBeenCalledTimes(2);
      expect(test.runOrResume).toHaveBeenCalledOnce();
    } finally {
      stderr.mockRestore();
    }
  });
};
