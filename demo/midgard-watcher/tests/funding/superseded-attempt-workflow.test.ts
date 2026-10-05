import { createHash } from "node:crypto";

import {
  beginWorkflowFundingReservationAction,
  type FraudProofWorkflowJournalStore,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";
import { afterEach, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { signFundingRecoveryFixtureBody } from "../support/fault-proof-funding-fixture.sign-body.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

type Fixture = Awaited<ReturnType<typeof setupFundingRecoveryFixture>>;

const fundingOf = (fixture: Fixture) =>
  fixture.plan.inputs.find(({ role }) => role === "funding")!;

/** A signed replacement spending `outRef` with `collateral`, as a builder
 * would produce it for the current action. */
const signReplacement = (
  outRef: string,
  lovelace: bigint,
  ttl: bigint,
  collateral: readonly string[] = [],
) => {
  const [hash, index] = outRef.split("#");
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(hash!),
      BigInt(index!),
    ),
  );
  const outputs = CML.TransactionOutputList.new();
  const remaining = lovelace - 1_000_000n;
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(walletAddress),
      CML.Value.from_coin(remaining),
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 1_000_000n);
  body.set_ttl(ttl);
  if (collateral.length !== 0) {
    const list = CML.TransactionInputList.new();
    for (const value of collateral) {
      const [txHash, outputIndex] = value.split("#");
      list.add(
        CML.TransactionInput.new(
          CML.TransactionHash.from_hex(txHash!),
          BigInt(outputIndex!),
        ),
      );
    }
    body.set_collateral_inputs(list);
  }
  const signed = signFundingRecoveryFixtureBody(body);
  return {
    ...signed,
    transactionBodySha256: createHash("sha256")
      .update(Buffer.from(body.to_cbor_hex(), "hex"))
      .digest("hex"),
    consumedOutRefs: [outRef],
    producedInputs: [
      {
        outRef: `${signed.transactionHash}#0`,
        role: "funding" as const,
        lovelace: remaining.toString(),
        assets: [],
      },
    ],
  };
};

/** Records a replacement for the init action exactly as a submission does:
 * the store handoff, then preflight, intent, submitted and pending. */
const recordReplacement = async (
  fixture: Fixture,
  journal: FraudProofWorkflowJournalStore,
  replacement: ReturnType<typeof signReplacement>,
  attempt: number,
) => {
  const entries = await journal.load(fixture.initial.workflowId);
  const [record] = await fixture.records();
  const handoff = {
    ...fixture.handoff,
    expectedJournalSequence: entries.length,
    preflight: {
      ...fixture.handoff.preflight,
      txHash: replacement.transactionHash,
    },
    submissionIntent: {
      ...fixture.handoff.submissionIntent,
      attempt,
      txHash: replacement.transactionHash,
    },
  };
  await fixture.store.prepareTransition({
    handoff,
    plan: fixture.plan,
    expectedRevision: record!.revision,
    actionKind: "proof.init",
    ...replacement,
  });
  await fixture.append(handoff.preflight);
  await fixture.append(handoff.submissionIntent);
  await fixture.append({
    kind: "submitted",
    actionId: "init",
    attempt,
    txHash: replacement.transactionHash,
  });
  await fixture.append({
    kind: "reconciled",
    actionId: "init",
    outcome: "pending",
    txHash: replacement.transactionHash,
  });
};

/** The fixture's attempt expires at the tip: its funding input is unspent and
 * the reader reports absence without retirement. */
const supersedeAtTip = async (fixture: Fixture) => {
  fixture.useUnspentPendingInputs();
  vi.mocked(fixture.adapter.reconcile).mockResolvedValue({ kind: "not_found" });
  const journal = await fixture.recover();
  await fixture.run(journal);
  return journal;
};

it("admits a replacement for an attempt that expired at the tip at once, drawing on that attempt's funding input", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const funding = fundingOf(fixture);
  const journal = await supersedeAtTip(fixture);
  const [record] = await fixture.records();
  expect(record!.pendingTransition).toBeNull();
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event,
  ).toMatchObject({ kind: "reconciled", outcome: "not_found" });
  expect(
    await fixture.store.readSupersededAttemptFundingOutRefs!({
      reservationId: fixture.plan.reservationId,
    }),
  ).toEqual([[funding.outRef]]);

  // The workflow is free at once: no retirement past k is awaited.
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const [refreshed] = await fixture.records();
  expect(refreshed!.activeInputs).toContainEqual(
    expect.objectContaining({ outRef: funding.outRef, role: "funding" }),
  );
  const collateral = refreshed!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  expect(collateral).not.toContain(funding.outRef);
  const other = refreshed!.activeInputs.find(
    ({ role, outRef }) => role === "funding" && outRef !== funding.outRef,
  );
  // Negative: a replacement that avoids the expired attempt's input could
  // land beside it, so the store refuses it.
  expect(other).toBeDefined();
  await expect(
    fixture.store.prepareTransition({
      handoff: fixture.handoff,
      plan: fixture.plan,
      expectedRevision: refreshed!.revision,
      actionKind: "proof.init",
      ...signReplacement(other!.outRef, BigInt(other!.lovelace), 300n),
    }),
  ).rejects.toThrow("must share an input with each superseded attempt");
  const replacement = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    collateral,
  );
  await recordReplacement(fixture, journal, replacement, 2);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    replacement.transactionHash,
  );
  expect(replacement.transactionHash).not.toBe(fixture.transactionHash);
});

const replaceAfterExpiry = async (fixture: Fixture) => {
  const funding = fundingOf(fixture);
  const journal = await supersedeAtTip(fixture);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const collateral = (await fixture.records())[0]!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  const replacement = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    collateral,
  );
  await recordReplacement(fixture, journal, replacement, 2);
  // The process restarts with the replacement in flight.
  return { journal: await fixture.recover(), replacement };
};

it("adopts the expired attempt as the result when it lands late, and retires the replacement's reservation", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await replaceAfterExpiry(fixture);
  // A rollback lands the original attempt; the replacement, which spends one
  // of its inputs, can no longer land.
  vi.mocked(fixture.adapter.reconcile).mockImplementation(async ({ txHash }) =>
    txHash === fixture.transactionHash
      ? { kind: "confirmed", txHash }
      : { kind: "not_found" },
  );
  // A landing that cannot be adopted yet backs off like any unresolved read,
  // so each run is a minute apart.
  for (let run = 0; run < 3; run += 1)
    await fixture.run(journal, () => new Date(Date.now() + run * 60_000));
  const events = (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
  expect(events.at(-1)).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: fixture.transactionHash,
  });
  // The adoption is journaled as a fresh intent for the landed bytes.
  expect(
    events.filter(
      (event) =>
        event.kind === "submission_intent" &&
        event.txHash === fixture.transactionHash,
    ),
  ).toHaveLength(2);
  expect(events).toContainEqual(
    expect.objectContaining({
      kind: "reconciled",
      outcome: "not_found",
      txHash: replacement.transactionHash,
    }),
  );
  expect(events.some(({ kind }) => kind === "stalled")).toBe(false);
  const [record] = await fixture.records();
  expect(record!.pendingTransition).toBeNull();
  expect(record!.lastConfirmedTransitionDigest).not.toBeNull();
  // The landed attempt's output is this reservation's funding now. The
  // replacement holds no workflow and asks for no exclusion; only its own
  // would-be output stays leased to this reservation until final completion,
  // in case a further rollback lands it instead.
  const reserved = await fixture.store.readReservedOutRefs({});
  expect(reserved).toContain(`${fixture.transactionHash}#0`);
  expect(reserved).toContain(`${replacement.transactionHash}#0`);
  expect(
    await fixture.store.readSupersededAttemptFundingOutRefs!({
      reservationId: fixture.plan.reservationId,
    }),
  ).toEqual([]);
  expect(fixture.adapter.preflight).not.toHaveBeenCalled();
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});

it("does not adopt a late landing while the replacement is unresolved or once the action has a result", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await replaceAfterExpiry(fixture);
  const before = await journal.load(fixture.initial.workflowId);
  // Unresolved replacement: the late landing is reported, the replacement is
  // still pending, so nothing is adopted yet.
  vi.mocked(fixture.adapter.reconcile).mockImplementation(async ({ txHash }) =>
    txHash === fixture.transactionHash
      ? { kind: "confirmed", txHash }
      : { kind: "pending", txHash: txHash! },
  );
  await fixture.run(journal);
  const pending = await journal.load(fixture.initial.workflowId);
  expect(
    pending
      .slice(before.length)
      .some(
        ({ event }) =>
          event.kind === "submission_intent" &&
          event.txHash === fixture.transactionHash,
      ),
  ).toBe(false);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    replacement.transactionHash,
  );
  // Recorded result: the replacement confirms. A conflicting report for the
  // superseded attempt does not reopen the action.
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async ({ txHash }) => ({ kind: "confirmed", txHash: txHash! }),
  );
  for (let run = 0; run < 2; run += 1) await fixture.run(journal);
  const events = (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
  expect(events.at(-1)).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: replacement.transactionHash,
  });
  expect(
    events
      .slice(before.length)
      .some(
        (event) =>
          event.kind === "submission_intent" &&
          event.txHash === fixture.transactionHash,
      ),
  ).toBe(false);
  expect((await fixture.records())[0]!.pendingTransition).toBeNull();
});

it("re-signs a mutually exclusive proof with fresh collateral when a rollback drops a confirmed proof whose collateral was spent meanwhile", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const funding = fundingOf(fixture);
  const collateral = fixture.plan.inputs.find(
    ({ role }) => role === "collateral",
  )!.outRef;
  const change = `${fixture.transactionHash}#0`;
  const journal = await fixture.recover();
  await fixture.run(journal);
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event.kind,
  ).toBe("confirmed");
  expect(await fixture.store.readReservedOutRefs({})).not.toContain(collateral);

  // Another action spends the released collateral; then a rollback within k
  // drops the proof. Its funding input is unspent again, its change is gone,
  // and its collateral no longer exists, so the identical bytes cannot land.
  fixture.walletUtxos.splice(
    0,
    fixture.walletUtxos.length,
    ...fixture.walletUtxos.filter(({ txHash, outputIndex }) => {
      const outRef = `${txHash}#${outputIndex}`;
      return outRef !== change && outRef !== collateral;
    }),
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
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "action_required",
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const offered: (string | undefined)[] = [];
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async ({ signedTransactionCborHex }) => {
      offered.push(signedTransactionCborHex);
      // The reader finds the identical bytes cannot land (their collateral is
      // spent) and reports the dropped proof as invalidated at the tip.
      return { kind: "not_found" };
    },
  );
  // The fixture's builder refuses to build, so the new attempt stops at its
  // preflight; a real builder signs here.
  await expect(fixture.run(journal)).resolves.toMatchObject({
    kind: "stalled",
    phase: "preflight",
  });
  // The exact signed bytes were reobserved and offered first.
  expect(offered).toContain(fixture.signedTransactionCborHex);
  const events = (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
  expect(events).toContainEqual(
    expect.objectContaining({
      kind: "reconciled",
      outcome: "not_found",
      txHash: fixture.transactionHash,
    }),
  );
  // The orchestrator went straight on to a new attempt (preflight), with no
  // wait for retirement past k.
  expect(fixture.adapter.preflight).toHaveBeenCalled();
  expect(
    await fixture.store.readSupersededAttemptFundingOutRefs!({
      reservationId: fixture.plan.reservationId,
    }),
  ).toEqual([[funding.outRef]]);
  const [record] = await fixture.records();
  const fresh = record!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  expect(fresh).toHaveLength(1);
  expect(fresh).not.toContain(collateral);
  expect(record!.activeInputs).toContainEqual(
    expect.objectContaining({ outRef: funding.outRef, role: "funding" }),
  );
  const other = record!.activeInputs.find(
    ({ role, outRef }) => role === "funding" && outRef !== funding.outRef,
  );
  expect(other).toBeDefined();
  await expect(
    fixture.store.prepareTransition({
      handoff: fixture.handoff,
      plan: fixture.plan,
      expectedRevision: record!.revision,
      actionKind: "proof.init",
      ...signReplacement(other!.outRef, BigInt(other!.lovelace), 300n, fresh),
    }),
  ).rejects.toThrow("must share an input with each superseded attempt");
  const resigned = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    300n,
    fresh,
  );
  await recordReplacement(fixture, journal, resigned, 2);
  expect(resigned.transactionHash).not.toBe(fixture.transactionHash);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    resigned.transactionHash,
  );
});
