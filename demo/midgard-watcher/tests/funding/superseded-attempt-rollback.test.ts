import {
  beginWorkflowFundingReservationAction,
  computeFraudProofRawL1PointId,
  createDoubleSpendConstrainedWorkflowAdapter,
  deriveFraudProofRawL1CompletedTerminal,
  type DoubleSpendConstrainedWorkflowAdapterConfig,
  type FraudProofWorkflowJournalStore,
  type SignedTransactionRecoveryObservation,
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

/** The chain as a double-spend computation thread sees it: its init landed
 * as whichever attempt is on chain, and the thread moved on with it. The
 * family's own reconcile answers every read; signed bytes are read against
 * `onChain`, except while `unreadable` holds them. */
const threadFamilyChain = (
  fixture: Fixture,
  chain: { onChain: readonly string[]; unreadable: readonly string[] },
) => {
  const point = (blockNo: number) => {
    const value = {
      blockNo: String(blockNo),
      slot: String(blockNo * 20),
      blockHash: blockNo.toString(16).padStart(64, "0"),
    };
    return { ...value, pointId: computeFraudProofRawL1PointId(value) };
  };
  const family = createDoubleSpendConstrainedWorkflowAdapter({
    l1: {
      observe: async () => ({
        provenance: {
          trustClass: "authenticated_cardano_l1",
          sourceId: "local-node-test",
          grade: "security",
        },
        stage: {
          kind: "step_01",
          threadOutRef: `${chain.onChain[0]}#0`,
          stateQueueBlockOutRef: `${"73".repeat(32)}#0`,
        },
      }),
      transactionConfirmed: async ({ txHash }: { txHash: string }) =>
        chain.onChain.includes(txHash),
      observeSignedTransaction: async (input: {
        transactionHash: string;
        signedTransactionCborHex: string;
      }): Promise<SignedTransactionRecoveryObservation> => {
        if (chain.unreadable.includes(input.transactionHash))
          throw new Error("chain provider unavailable");
        const included = chain.onChain.includes(input.transactionHash);
        return {
          ...input,
          status: included ? "included" : "invalidated_at_tip",
          canonicalPoint: point(100),
          releaseFinalPoint: point(10),
          ...(included ? { inclusionPoint: point(99) } : {}),
          inputs: [],
          reason: "test chain",
        };
      },
    },
  } as unknown as DoubleSpendConstrainedWorkflowAdapterConfig);
  // The fixture's init action names no stage; the family reads it as init.
  vi.mocked(fixture.adapter.reconcile).mockImplementation(
    async (input) =>
      await family.reconcile({
        ...input,
        action: {
          ...input.action,
          input: { ...input.action.input, stage: "init" },
        },
      }),
  );
};

/** The correction's terminal, created by `createdBy`, at `confirmationDepth`
 * (by default within the recovery window). */
const correctionOn = async (
  fixture: Fixture,
  createdBy: string,
  confirmationDepth?: number,
) => {
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
    ...(confirmationDepth === undefined ? {} : { confirmationDepth }),
  });
  const terminal = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (terminal === null) throw new Error("raw fixture omitted its correction");
  return {
    policy: verifiedFinality.policy,
    terminal: {
      ...terminal,
      proofToken: { ...terminal.proofToken, createdByTxHash: createdBy },
    },
  };
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
  const { policy, terminal } = await correctionOn(
    fixture,
    replacement.transactionHash,
  );
  expect(terminal.observedAt.confirmationDepth).toBeLessThanOrEqual(
    policy.automaticRecoveryMaxDepth + 1,
  );
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "completed",
    terminal,
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

it("takes the expired attempt as the result in a family that reads its computation thread, when a rollback drops the confirmed replacement and lands it instead", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await confirmReplacement(fixture);
  const before = (await journal.load(fixture.initial.workflowId)).length;
  walletAfter(fixture, fixture.transactionHash);
  threadFamilyChain(fixture, {
    onChain: [fixture.transactionHash],
    unreadable: [],
  });
  const events = (await runMinutesApart(fixture, journal, 4)).slice(before);
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
  expect(record!.state).toBe("active");
  expect(record!.pendingTransition).toBeNull();
  expect(await fixture.store.readReservedOutRefs({})).toContain(
    `${fixture.transactionHash}#0`,
  );
});

it("holds the confirmed replacement as the result while it cannot be read, then takes the expired attempt once the replacement reads as gone", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await confirmReplacement(fixture);
  const before = await journal.load(fixture.initial.workflowId);
  const [held] = await fixture.records();
  walletAfter(fixture, fixture.transactionHash);
  const chain = {
    onChain: [fixture.transactionHash],
    unreadable: [replacement.transactionHash],
  };
  threadFamilyChain(fixture, chain);
  const results = [];
  for (const minutes of [1, 12, 24])
    results.push(
      await fixture.run(journal, () => new Date(Date.now() + minutes * 60_000)),
    );
  expect(results.map(({ kind }) => kind)).toEqual([
    "pending",
    "pending",
    "pending",
  ]);
  expect(await journal.load(fixture.initial.workflowId)).toEqual(before);
  expect((await fixture.records())[0]).toEqual(held);

  chain.unreadable = [];
  for (const minutes of [36, 48, 60, 72])
    await fixture.run(journal, () => new Date(Date.now() + minutes * 60_000));
  const events = (await journal.load(fixture.initial.workflowId))
    .slice(before.length)
    .map(({ event }) => event);
  expect(events.at(-1)).toEqual({
    kind: "confirmed",
    actionId: "init",
    txHash: fixture.transactionHash,
  });
  expect((await fixture.records())[0]!.state).toBe("active");
});

it("leaves a workflow past its final completion as it is when a superseded attempt of it lands", async () => {
  const fixture = await setupFundingRecoveryFixture(false, false, false, true);
  const { journal, replacement } = await confirmReplacement(fixture);
  const { policy, terminal } = await correctionOn(
    fixture,
    replacement.transactionHash,
    (
      await finality.verifyForWorkflow({
        deploymentFingerprint: deploymentIdentity.manifestId,
      })
    ).policy.automaticRecoveryMaxDepth + 2,
  );
  expect(terminal.observedAt.confirmationDepth).toBeGreaterThan(
    policy.automaticRecoveryMaxDepth + 1,
  );
  vi.mocked(fixture.adapter.observe).mockResolvedValue({
    kind: "completed",
    terminal,
  });
  await runMinutesApart(fixture, journal, 2);
  const completed = await journal.load(fixture.initial.workflowId);
  expect(completed.at(-1)!.event.kind).toBe("completed");
  const handoff = await fixture.store.readCompletionHandoff({
    reservationId: fixture.plan.reservationId,
  });
  expect(handoff).not.toBeNull();
  const [record] = await fixture.records();

  // The expired attempt lands after a rollback the completion is past.
  walletAfter(fixture, fixture.transactionHash);
  chainHolds(fixture, [fixture.transactionHash]);
  for (const minutes of [10, 20, 30])
    await fixture.run(journal, () => new Date(Date.now() + minutes * 60_000));
  expect(await journal.load(fixture.initial.workflowId)).toEqual(completed);
  expect(
    await fixture.store.readCompletionHandoff({
      reservationId: fixture.plan.reservationId,
    }),
  ).toEqual(handoff);
  expect((await fixture.records())[0]).toEqual(record);
});
