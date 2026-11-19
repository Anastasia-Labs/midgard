/**
 * A proof whose correction is included frees the outputs its transactions
 * produced, and another fault's reservation may take one. A rollback that
 * drops the producing transaction and lands it again must leave that output
 * with the reservation that took it.
 */
import { rm } from "node:fs/promises";

import { afterEach, expect, it } from "vitest";

import { parseWatcherProverFundingReservationRecord } from "../../src/funding/prover-funding-reservation.parse-watcher-prover-funding-reservation-record.js";
import {
  openStore,
  plan,
  prepareTransition,
  signedTransition,
  temporaryDirectories,
} from "./sqlite-prover-funding-reservation-store.signed-transition.js";

afterEach(async () => {
  await Promise.all(
    temporaryDirectories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

it("leaves a freed output with the reservation that took it when a rollback drops the transaction that produced it and lands it again", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  try {
    // Proof A: its step lands, and its correction is included.
    const first = plan("aa", "66");
    const step = signedTransition();
    const change = step.producedInputs[0]!.outRef;
    await store.reserve(first);
    const prepared = await prepareTransition(store, {
      plan: first,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...step,
      consumedOutRefs: [first.inputs[0]!.outRef],
    });
    const confirmed = await store.confirmTransition({
      journal: [],
      plan: first,
      expectedRevision: prepared.revision,
      transactionHash: step.transactionHash,
      transitionDigest: prepared.pendingTransition!.transitionDigest,
    });
    const freed = await store.releaseProducedOutputs!({
      journal: [],
      plan: first,
      expectedRevision: confirmed.revision,
    });

    // Proof B takes A's change, with a small coin of its own, and spends it.
    const base = plan("bb", "77", change);
    const second = {
      ...base,
      inputs: base.inputs
        .map((input) =>
          input.outRef === `${"12".repeat(32)}#0`
            ? { ...input, outRef: `${"13".repeat(32)}#0` }
            : input,
        )
        .sort((left, right) => (left.outRef < right.outRef ? -1 : 1)),
    };
    await expect(store.reserve(second)).resolves.toBe("reserved");
    const spend = signedTransition({ inputHash: step.transactionHash });
    const spending = await prepareTransition(store, {
      plan: second,
      expectedRevision: "0",
      actionKind: "proof.init",
      ...spend,
      consumedOutRefs: [change],
    });
    await store.confirmTransition({
      journal: [],
      plan: second,
      expectedRevision: spending.revision,
      transactionHash: spend.transactionHash,
      transitionDigest: spending.pendingTransition!.transitionDigest,
    });

    // A rollback drops A's step, which lands again.
    const restored = await store.reobserveTransition!({
      journal: [],
      plan: first,
      expectedRevision: freed.revision,
      transactionHash: step.transactionHash,
      inputs: first.inputs.filter(
        ({ outRef }) => outRef !== `${"12".repeat(32)}#0`,
      ),
    });
    expect(restored.pendingTransition?.transactionHash).toBe(
      step.transactionHash,
    );
    const relanded = await store.confirmTransition({
      journal: [],
      plan: first,
      expectedRevision: restored.revision,
      transactionHash: step.transactionHash,
      transitionDigest: restored.pendingTransition!.transitionDigest,
    });

    // The change stays B's: A does not take it back, and B's lease on what
    // it spent holds.
    expect(relanded.activeInputs.map(({ outRef }) => outRef)).not.toContain(
      change,
    );
    expect(
      await store.readReservedOutRefs({
        excludingReservationId: first.reservationId,
      }),
    ).toContain(change);
    const taken = (await store.readAll())
      .map(parseWatcherProverFundingReservationRecord)
      .find(({ reservationId }) => reservationId === second.reservationId);
    expect(taken?.lastConfirmedTransitionDigest).toBe(
      spending.pendingTransition!.transitionDigest,
    );
  } finally {
    opened.runtime.close();
  }
});
