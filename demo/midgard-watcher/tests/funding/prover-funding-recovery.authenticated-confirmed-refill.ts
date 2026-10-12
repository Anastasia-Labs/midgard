import { createHash } from "node:crypto";

import {
  beginWorkflowFundingReservationAction,
  type FraudProofWorkflowJournalStore,
  releaseIdleWorkflowFundingReservation,
  type SignedTransactionRecoveryObservation,
  type SignedWorkflowTransaction,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { expect, it } from "vitest";

import {
  deploymentIdentity,
  finality,
  setupFundingRecoveryFixture as setup,
} from "../support/fault-proof-funding-fixture.js";

type FundingFixture = Awaited<ReturnType<typeof setup>>;
// An included observation of exact signed bytes, `depth` blocks below the
// canonical point. The journal admits the retirement it carries as untrusted
// input; the follower source that produces it is pinned in tests/l1-follower.
const includedObservation = (
  input: SignedWorkflowTransaction,
  depth: number,
): SignedTransactionRecoveryObservation => {
  const at = (blockNo: number) => {
    const slot = (1_000 + blockNo).toString();
    const blockHash = createHash("sha256")
      .update(`${input.transactionHash}:${blockNo.toString()}`)
      .digest("hex");
    const block = { slot, blockHash, blockNo: blockNo.toString() };
    return {
      ...block,
      pointId: createHash("sha256")
        .update(`${slot}:${blockHash}:${block.blockNo}`)
        .digest("hex"),
    };
  };
  const inclusionPoint = at(1);
  return {
    ...input,
    status: "included",
    inclusionPoint,
    canonicalPoint: at(1 + depth),
    releaseFinalPoint: inclusionPoint,
    inputs: [],
    reason: "Exact recorded transaction body is on the canonical chain",
  };
};

const retirementEvent = (observed: SignedTransactionRecoveryObservation) => {
  expect(observed.status).toBe("included");
  if (observed.inclusionPoint === undefined)
    throw new Error("exact included observation is missing its admitted point");
  return {
    kind: "signed_attempt_retired" as const,
    txHash: observed.transactionHash,
    retirement: {
      transactionHash: observed.transactionHash,
      canonicalPoint: observed.canonicalPoint,
      releaseFinalPoint: observed.inclusionPoint,
      reason: "included" as const,
    },
  };
};

export const assertConfirmedFundingHistoryRetained = async (
  test: FundingFixture,
) => {
  const transition = test.pending.pendingTransition!;
  const protectedRefs = [
    ...transition.consumedOutRefs,
    ...transition.producedInputs.map(({ outRef }) => outRef),
  ];
  const collateral = CML.Transaction.from_cbor_hex(
    transition.signedTransactionCborHex,
  )
    .body()
    .collateral_inputs();
  if (collateral !== undefined)
    for (let index = 0; index < collateral.len(); index += 1) {
      const input = collateral.get(index);
      protectedRefs.push(
        `${input.transaction_id().to_hex()}#${input.index().toString()}`,
      );
    }
  const leases = await test.store.readReservedOutRefs({});
  for (const outRef of protectedRefs) expect(leases).toContain(outRef);
  expect(test.store.hasSignedHistory).toBeTypeOf("function");
  expect(
    await test.store.hasSignedHistory!({
      reservationId: test.plan.reservationId,
    }),
  ).toBe(true);
};

/** Retirement beyond the recovery horizon is bookkeeping only: a confirmed
 * attempt has already released its stale inputs, so its receipts change no
 * reservation, lease or wallet read, and its signed history stays retained. */
export const retireConfirmedFundingAttempts = async (
  test: FundingFixture,
  journal: FraudProofWorkflowJournalStore,
  checkShallowRefusal = false,
) => {
  const records = await test.records();
  const walletReads = test.readWalletUtxos.mock.calls.length;
  const leases = await test.store.readReservedOutRefs({});
  const entries = await journal.load(test.initial.workflowId);
  const signed = new Map<string, SignedWorkflowTransaction>();
  for (const { event } of entries) {
    if (event.kind !== "submission_intent") continue;
    const transition = test.pending.pendingTransition!;
    const signedTransactionCborHex =
      event.durableRecovery?.signedTransactionCborHex ??
      (event.txHash === transition.transactionHash
        ? transition.signedTransactionCborHex
        : undefined);
    if (typeof signedTransactionCborHex !== "string")
      throw new Error(
        "fixture has an intent without exact persisted signed bytes",
      );
    const input = { transactionHash: event.txHash, signedTransactionCborHex };
    const prior = signed.get(event.txHash);
    if (prior !== undefined) expect(input).toEqual(prior);
    signed.set(event.txHash, input);
  }
  expect(signed.size).toBeGreaterThan(0);
  const assertHeld = async () => {
    expect(await test.records()).toEqual(records);
    expect([...(await test.store.readReservedOutRefs({}))].sort()).toEqual(
      [...leases].sort(),
    );
    expect(await journal.load(test.initial.workflowId)).toEqual(entries);
    expect(test.readWalletUtxos).toHaveBeenCalledTimes(walletReads);
    await assertConfirmedFundingHistoryRetained(test);
  };
  const policy = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const inputs = [...signed.values()];
  const deep = policy.policy.automaticRecoveryMaxDepth + 1;
  if (checkShallowRefusal) {
    const shallow = includedObservation(
      inputs[0]!,
      policy.policy.automaticRecoveryMaxDepth,
    );
    await expect(test.append(retirementEvent(shallow))).rejects.toThrow(
      "canonical recovery horizon",
    );
    await assertHeld();
  }
  // Observe every exact intent before admitting any retirement to the journal.
  const observations: SignedTransactionRecoveryObservation[] = [];
  for (const input of inputs) {
    const observed = includedObservation(input, deep);
    expect(observed.transactionHash).toBe(input.transactionHash);
    expect(observed.signedTransactionCborHex).toBe(
      input.signedTransactionCborHex,
    );
    observations.push(observed);
  }
  for (const observed of observations)
    await test.append(retirementEvent(observed));
  await releaseIdleWorkflowFundingReservation({
    journal,
    workflowId: test.initial.workflowId,
  });
  expect(await test.records()).toEqual(records);
  expect([...(await test.store.readReservedOutRefs({}))].sort()).toEqual(
    [...leases].sort(),
  );
  expect(test.readWalletUtxos).toHaveBeenCalledTimes(walletReads);
  await assertConfirmedFundingHistoryRetained(test);
};

export const registerConfirmedFundingRefillTest = () => {
  it("refreshes externally spent unsigned change after confirmation without rotating healthy inputs", async () => {
    const test = await setup();
    const admitted = await test.createPermit(test.fresh, "2");
    const journal = test.bind(test.fresh, admitted);
    await test.run(journal);
    const before = await test.records();
    const action = { actionId: "next", input: { actionKind: "proof.init" } };
    await beginWorkflowFundingReservationAction({ journal, action });
    expect(await test.records()).toEqual(before);
    expect(test.readWalletUtxos).not.toHaveBeenCalled();
    const spent = `${test.transactionHash}#0`;
    expect(before[0]!.activeInputs).toContainEqual(
      expect.objectContaining({ role: "funding", outRef: spent }),
    );
    test.walletUtxos.splice(
      0,
      test.walletUtxos.length,
      ...test.walletUtxos.filter(
        (utxo) => `${utxo.txHash}#${utxo.outputIndex}` !== spent,
      ),
    );
    // The confirmed attempt is not retired; its stale input refreshes at once.
    await beginWorkflowFundingReservationAction({ journal, action });
    expect(test.readWalletUtxos).toHaveBeenCalledOnce();
    expect(
      (await test.records())[0]!.activeInputs.map(({ outRef }) => outRef),
    ).not.toContain(spent);
    expect((await test.records())[0]!.reservationId).toBe(
      before[0]!.reservationId,
    );
    await assertConfirmedFundingHistoryRetained(test);
    await retireConfirmedFundingAttempts(test, journal, true);
  });
};
