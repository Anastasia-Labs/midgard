/**
 * Operator registration when its submitted transaction never reaches a
 * block in time (the devnet stalled past the transaction's upper bound):
 * the emulator stands in for a node whose mempool dropped the bytes. Only
 * the emulator's own `submitTx` and `awaitSlot` are wrapped, to hold the
 * first submission back and hand it over later or never.
 */
import { CML, type Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { registerOperatorProgram } from "../src/transactions/register-active-operator.activate-registered-operator-program.js";
import { runWithoutFollower } from "./helpers/intent-journal.js";
import {
  EMULATOR_REQUIRED_BOND_LOVELACE,
  initOperatorLifecycleFixture,
} from "./operator-lifecycle-emulator.build-operator-lifecycle-snapshot.js";
import { fetchRegisteredOperatorNodes } from "./operator-lifecycle-emulator.register-second-operator-behind-active-first.js";

type Submission = {
  readonly txHash: string;
  readonly slot: number;
  /** The transaction's `invalid_hereafter` slot. */
  readonly ttl: number;
};

const submissionOf = (cbor: string, slot: number): Submission => {
  const transaction = CML.Transaction.from_cbor_hex(cbor);
  try {
    const body = transaction.body();
    const ttl = body.ttl();
    if (ttl === undefined) throw new Error("expected a bounded transaction");
    return {
      txHash: CML.hash_transaction(body).to_hex(),
      slot,
      ttl: Number(ttl),
    };
  } finally {
    transaction.free();
  }
};

/**
 * Records every transaction sent to `emulator` and holds the first one back
 * from its ledger. `deliverAt` says, from that first submission, the slot
 * the held bytes reach the emulator, or `undefined` for never. Time moves
 * one slot at a time so the hand-over lands on that exact slot.
 */
const holdFirstSubmission = (
  emulator: Emulator,
  deliverAt: (first: Submission) => number | undefined,
) => {
  const submissions: Submission[] = [];
  let held: { cbor: string; slot: number } | undefined;
  const submitTx = emulator.submitTx.bind(emulator);
  emulator.submitTx = (cbor: string) => {
    const submission = submissionOf(cbor, emulator.slot);
    submissions.push(submission);
    if (submissions.length > 1) return submitTx(cbor);
    const slot = deliverAt(submission);
    if (slot !== undefined) held = { cbor, slot };
    return Promise.resolve(submission.txHash);
  };
  const awaitSlot = emulator.awaitSlot.bind(emulator);
  emulator.awaitSlot = (length = 1) => {
    for (let step = 0; step < length; step++) {
      awaitSlot(1);
      if (held !== undefined && emulator.slot >= held.slot) {
        const { cbor } = held;
        held = undefined;
        void submitTx(cbor);
      }
    }
  };
  return submissions;
};

const registerOperator = async (
  deliverAt: (first: Submission) => number | undefined,
) => {
  const fixture = await initOperatorLifecycleFixture();
  const submissions = holdFirstSubmission(fixture.emulator, deliverAt);
  const result = await runWithoutFollower(
    registerOperatorProgram(
      fixture.lucid,
      fixture.contracts,
      EMULATOR_REQUIRED_BOND_LOVELACE,
      fixture.referenceScriptsLucid,
    ),
  );
  const registered = await fetchRegisteredOperatorNodes(
    fixture.lucid,
    fixture.contracts,
  );
  return {
    submissions,
    result,
    landed: registered.some((utxo) => utxo.txHash === result.registerTxHash),
  };
};

describe("operator registration whose transaction expires unconfirmed", () => {
  it("rebuilds once the tip passed the dropped transaction's bound, and the registration lands", async () => {
    const { submissions, result, landed } = await registerOperator(
      () => undefined,
    );

    expect(submissions).toHaveLength(2);
    const [dropped, rebuilt] = submissions as [Submission, Submission];
    expect(rebuilt.txHash).not.toBe(dropped.txHash);
    expect(rebuilt.slot).toBeGreaterThanOrEqual(dropped.ttl);
    expect(result.registerTxHash).toBe(rebuilt.txHash);
    expect(landed).toBe(true);
  });

  it("does not rebuild a transaction that lands well before its bound", async () => {
    const { submissions, result, landed } = await registerOperator(
      (first) => first.slot + 10,
    );

    expect(submissions).toHaveLength(1);
    expect(result.registerTxHash).toBe(submissions[0]?.txHash);
    expect(landed).toBe(true);
  });

  it("keeps waiting, without a rebuild, while the tip is still before the bound", async () => {
    const { submissions, result, landed } = await registerOperator(
      (first) => first.ttl - 1,
    );

    expect(submissions).toHaveLength(1);
    expect(result.registerTxHash).toBe(submissions[0]?.txHash);
    expect(landed).toBe(true);
  });
});
