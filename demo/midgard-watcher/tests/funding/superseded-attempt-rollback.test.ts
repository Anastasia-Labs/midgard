import {
  beginWorkflowFundingReservationAction,
  deriveFraudProofRawL1CompletedTerminal,
  type FraudProofWorkflowJournalStore,
  supersededAttemptReadSchedule,
} from "@al-ft/midgard-fault-proofs";
import { fixture as terminalFixture } from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";
import { afterEach, expect, it, vi } from "vitest";

import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  finality,
  key,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import {
  type Fixture,
  fundingOf,
  recordReplacement,
  replaceAfterExpiry,
  signReplacement,
  supersedeAtTip,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

/** The wallet as the chain shows it once `landed` spent the fixture's
 * funding input: its change is the wallet's, and no other attempt's is. */
const walletAfter = (fixture: Fixture, landed: string) => {
  const funding = fundingOf(fixture);
  const coins = fixture.walletUtxos.filter(
    ({ txHash, outputIndex }) =>
      txHash === "99".repeat(32) &&
      `${txHash}#${outputIndex}` !== funding.outRef,
  );
  fixture.walletUtxos.splice(0, fixture.walletUtxos.length, ...coins, {
    txHash: landed,
    outputIndex: 0,
    address: walletAddress,
    assets: { lovelace: BigInt(funding.lovelace) - 1_000_000n },
  });
};

/** The chain reports exactly `onChain` as included. */
const chainHolds = (fixture: Fixture, onChain: readonly string[]) =>
  vi
    .mocked(fixture.adapter.reconcile)
    .mockImplementation(async ({ txHash }) =>
      onChain.includes(txHash!)
        ? { kind: "confirmed", txHash: txHash! }
        : { kind: "not_found" },
    );

/** The replacement of the expired attempt lands and is the init result. */
const confirmReplacement = async (fixture: Fixture) => {
  const { journal, replacement } = await replaceAfterExpiry(fixture);
  // The fixture signs the same attempts in every row: each row starts with
  // no reads of them behind it.
  for (const hash of [fixture.transactionHash, replacement.transactionHash])
    supersededAttemptReadSchedule.forget(hash);
  walletAfter(fixture, replacement.transactionHash);
  chainHolds(fixture, [replacement.transactionHash]);
  await fixture.run(journal);
  expect(
    (await journal.load(fixture.initial.workflowId)).at(-1)!.event,
  ).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: replacement.transactionHash,
  });
  return { journal, replacement };
};

const runMinutesApart = async (
  fixture: Fixture,
  journal: FraudProofWorkflowJournalStore,
  runs: number,
) => {
  for (let run = 1; run <= runs; run += 1)
    await fixture.run(journal, () => new Date(Date.now() + run * 60_000));
  return (await journal.load(fixture.initial.workflowId)).map(
    ({ event }) => event,
  );
};

const intentsOf = (
  events: readonly { kind: string; txHash?: string }[],
  txHash: string,
) =>
  events.filter(
    (event) => event.kind === "submission_intent" && event.txHash === txHash,
  ).length;

it("takes the expired attempt as the result when a rollback drops its confirmed replacement and lands it instead, and reopens the replacement", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await confirmReplacement(fixture);
  const before = (await journal.load(fixture.initial.workflowId)).length;
  walletAfter(fixture, fixture.transactionHash);
  chainHolds(fixture, [fixture.transactionHash]);
  const events = await runMinutesApart(fixture, journal, 4);
  expect(events.at(-1)).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: fixture.transactionHash,
  });
  // The replacement is reobserved as rolled back, then closed as absent.
  expect(
    events.slice(before).map((event) => ({
      kind: event.kind,
      txHash: "txHash" in event ? event.txHash : undefined,
    })),
  ).toEqual([
    { kind: "reobserved", txHash: replacement.transactionHash },
    { kind: "reconciled", txHash: replacement.transactionHash },
    { kind: "preflight_passed", txHash: fixture.transactionHash },
    { kind: "submission_intent", txHash: fixture.transactionHash },
    { kind: "reconciled", txHash: fixture.transactionHash },
    { kind: "confirmed", txHash: fixture.transactionHash },
  ]);
  expect(events.slice(before)[1]).toMatchObject({ outcome: "not_found" });
  const [record] = await fixture.records();
  expect(record!.pendingTransition).toBeNull();
  expect(await fixture.store.readReservedOutRefs({})).toContain(
    `${fixture.transactionHash}#0`,
  );
  expect(fixture.adapter.preflight).not.toHaveBeenCalled();
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
});

it("keeps the confirmed replacement as the result when the expired attempt is on chain beside it, and records the pair once", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await confirmReplacement(fixture);
  const before = (await journal.load(fixture.initial.workflowId)).length;
  chainHolds(fixture, [fixture.transactionHash, replacement.transactionHash]);
  const events = await runMinutesApart(fixture, journal, 4);
  expect(events.slice(before)).toEqual([
    {
      kind: "stalled",
      reason: `attempts ${replacement.transactionHash} and ${fixture.transactionHash} of init both landed; ${replacement.transactionHash} is its result`,
    },
  ]);
  expect(intentsOf(events, fixture.transactionHash)).toBe(1);
  expect((await fixture.records())[0]!.pendingTransition).toBeNull();
});

it("takes the expired attempt as the result when a rollback past the correction's inclusion drops the confirmed replacement and lands it instead", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await confirmReplacement(fixture);
  // The correction is included on the replacement's proof, within the
  // recovery window.
  const verifiedFinality = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const raw = await terminalFixture({
    proofCreation: true,
    header: fixture.fixture.header,
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
  });
  const terminal = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (terminal === null) throw new Error("raw fixture omitted its correction");
  expect(terminal.observedAt.confirmationDepth).toBeLessThanOrEqual(
    verifiedFinality.policy.automaticRecoveryMaxDepth + 1,
  );
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "completed",
    terminal: {
      ...terminal,
      proofToken: {
        ...terminal.proofToken,
        createdByTxHash: replacement.transactionHash,
      },
    },
  });
  await fixture.run(journal, () => new Date(Date.now() + 60_000));
  const included = await journal.load(fixture.initial.workflowId);
  expect(included.at(-1)!.event.kind).toBe("terminal_included");

  // A rollback drops the correction and the replacement, and lands the
  // expired attempt in its place.
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "pending",
    reason: "next canonical proof step",
  });
  walletAfter(fixture, fixture.transactionHash);
  chainHolds(fixture, [fixture.transactionHash]);
  const events = (await runMinutesApart(fixture, journal, 5)).slice(
    included.length,
  );
  expect(events.at(-1)).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: fixture.transactionHash,
  });
  expect(events).toContainEqual({
    kind: "reobserved",
    actionId: "init",
    txHash: replacement.transactionHash,
  });
  expect(events.some(({ kind }) => kind === "stalled")).toBe(false);
  const [record] = await fixture.records();
  expect(record!.pendingTransition).toBeNull();
  expect(
    await fixture.store.readCompletionHandoff({
      reservationId: fixture.plan.reservationId,
    }),
  ).toBeNull();
});

it("refuses a fresh attempt that spends an output of its action's landed superseded attempt, though the reservation holds none of that attempt's outputs", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const funding = fundingOf(fixture);
  const journal = await supersedeAtTip(fixture);
  await beginWorkflowFundingReservationAction({
    journal,
    action: { actionId: "init", input: { actionKind: "proof.init" } },
  });
  const [record] = await fixture.records();
  expect(
    record!.activeInputs.some(({ outRef }) =>
      outRef.startsWith(fixture.transactionHash),
    ),
  ).toBe(false);
  const collateral = record!.activeInputs
    .filter(({ role }) => role === "collateral")
    .map(({ outRef }) => outRef);
  const sign = (others: readonly string[]) =>
    signReplacement(
      funding.outRef,
      BigInt(funding.lovelace),
      300n,
      collateral,
      others,
    );
  // It also spends another output of the superseded attempt, as a later
  // step would: that attempt landed and is the action's result.
  const fresh = sign([`${fixture.transactionHash}#1`]);
  expect(
    await fixture.store.readLandedSupersededAttempt({
      reservationId: fixture.plan.reservationId,
      actionId: "init",
      signedTransactionCborHex: fresh.signedTransactionCborHex,
    }),
  ).toBe(fixture.transactionHash);
  await expect(
    fixture.store.prepareTransition({
      handoff: fixture.handoff,
      plan: fixture.plan,
      expectedRevision: record!.revision,
      actionKind: "proof.init",
      ...fresh,
    }),
  ).rejects.toThrow("repeats an action whose superseded attempt landed");
  // Without that output nothing shows the attempt landed.
  const plain = sign([]);
  expect(
    await fixture.store.readLandedSupersededAttempt({
      reservationId: fixture.plan.reservationId,
      actionId: "init",
      signedTransactionCborHex: plain.signedTransactionCborHex,
    }),
  ).toBeNull();
  await recordReplacement(fixture, journal, plain, 2);
  expect((await fixture.records())[0]!.pendingTransition?.transactionHash).toBe(
    plain.transactionHash,
  );
});
