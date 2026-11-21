/**
 * A proof whose correction is included frees the outputs its transactions
 * produce, even those of an attempt still pending at that moment: once that
 * attempt confirms, its change funds other proofs rather than staying held
 * by the finished one.
 */
import { rm } from "node:fs/promises";

import { afterEach, expect, it } from "vitest";

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

it("frees the change of an attempt still pending when the proof's outputs are released, once that attempt confirms", async () => {
  const opened = await openStore();
  const { store } = opened.runtime;
  try {
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
    // The correction's inclusion is recorded while the attempt is pending.
    const freed = await store.releaseProducedOutputs!({
      journal: [],
      plan: first,
      expectedRevision: prepared.revision,
    });
    const confirmed = await store.confirmTransition({
      journal: [],
      plan: first,
      expectedRevision: freed.revision,
      transactionHash: step.transactionHash,
      transitionDigest: prepared.pendingTransition!.transitionDigest,
    });

    expect(confirmed.activeInputs.map(({ outRef }) => outRef)).not.toContain(
      change,
    );
    // Another fault's reservation can take it, with a small coin of its own.
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
  } finally {
    opened.runtime.close();
  }
});
