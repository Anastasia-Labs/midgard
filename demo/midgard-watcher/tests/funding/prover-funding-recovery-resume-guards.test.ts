import { mkdir, rename, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  assertWorkflowActuationPermitIdentity,
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  computeFraudProofWorkflowId,
  createHeaderClassifier,
  createWorkflowActuationPermitController,
  DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
  type HeaderFaultDecision,
  journalJsonDigest,
  normalizeJournalJson,
} from "@al-ft/midgard-fault-proofs";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeOpenWatcherFaultDecisionJournalForTest } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import { authorizeWatcherProverFundingRecovery } from "../../src/funding/prover-funding-recovery.js";
import type {
  WatcherProverFundingReservationPlan,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  finality,
  setupFundingRecoveryFixture,
  sourcesFor,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { directories } from "../support/fault-proof-funding-fixture.sources-for.js";
import { createFundingRecoveryDirectory } from "../support/test-storage-root.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import {
  type Fixture,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const MULTIPLE =
  "workflow recovery has multiple signed funding reservations for one target";
const CONFLICTED =
  "workflow recovery has no unique non-conflicted original funding reservation";
const EMPTY_BESIDE = "empty workflow directory has durable funding history";

/** A restarted process is refused the objective before any funding is
 * minted: no wallet query, no second reservation, no action, and the store
 * unchanged. */
const refused = async (fixture: Fixture, message: string) => {
  const records = await fixture.records();
  for (const mock of [
    fixture.readWalletUtxos,
    fixture.adapter.preflight,
    fixture.adapter.submit,
  ])
    vi.mocked(mock).mockClear();
  await expect(fixture.recover()).rejects.toThrow(message);
  expect(await fixture.records()).toEqual(records);
  expect(fixture.readWalletUtxos).not.toHaveBeenCalled();
  expect(fixture.adapter.preflight).not.toHaveBeenCalled();
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
};

/** Admits `decision` through a funding factory over the fixture's store and
 * returns the plan it reserved under. */
const admit = async (fixture: Fixture, decision: HeaderFaultDecision) => {
  const plans: WatcherProverFundingReservationPlan[] = [];
  const store = fixture.store;
  const factory = createWatcherProverFundingAuthorityFactory({
    launchScope: installedLaunchScope(fixture.old.launchScope),
    journalRoot: fixture.journalRoot,
    journalAuthenticationKey: TEST_JOURNAL_KEY,
    deploymentIdentity,
    protocolParameters: fixture.protocolParameters,
    protocolParameterHistory: fixture.protocolParameterHistory,
    store: {
      ...store,
      releaseUnused: (record) => store.releaseUnused!(record),
      reserve: async (plan, expectedIdleRevision) => {
        plans.push(plan);
        return await store.reserve(plan, expectedIdleRevision);
      },
    },
  });
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "3",
  });
  await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: "3",
    decisionDigest: decision.decisionDigest,
    walletAddress,
    walletUtxos: fixture.walletUtxos,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol input lookup");
    },
  });
  return plans.at(-1)!;
};

/** Signs an init attempt over `plan`'s funding for the workflow of
 * `decision` and records it in the store. Returns that workflow's id. */
const sign = async (
  fixture: Fixture,
  plan: WatcherProverFundingReservationPlan,
  decision: HeaderFaultDecision,
) => {
  const funding = plan.inputs.find(({ role }) => role === "funding")!;
  const attempt = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    // Unlike the fixture's own attempt, so no output is reserved twice.
    200n,
  );
  const identity = {
    ...fixture.handoff.identity,
    decisionDigest: decision.decisionDigest,
  };
  const workflowId = computeFraudProofWorkflowId(identity);
  const record = (await fixture.records()).find(
    ({ reservationId }) => reservationId === plan.reservationId,
  )!;
  await fixture.store.prepareTransition({
    handoff: {
      ...fixture.handoff,
      workflowId,
      identity,
      preflight: {
        ...fixture.handoff.preflight,
        txHash: attempt.transactionHash,
      },
      submissionIntent: {
        ...fixture.handoff.submissionIntent,
        txHash: attempt.transactionHash,
      },
    },
    plan,
    expectedRevision: record.revision,
    actionKind: "proof.init",
    ...attempt,
  });
  return workflowId;
};

/** The wallet holds enough for a second reservation beside the first. */
const fundWallet = (fixture: Fixture) =>
  fixture.walletUtxos.push(
    ...[0, 1, 2, 3].map((outputIndex) => ({
      txHash: "98".repeat(32),
      outputIndex,
      address: walletAddress,
      assets: { lovelace: 2_000_000_000n },
    })),
  );

/** A reservation that has not signed yet, under its objective's journal. */
const unsigned = () => setupFundingRecoveryFixture(false, false, true);

/** A second reservation for the same target, under the fresh decision,
 * admitted while the original's workflow journal was set aside. Returns its
 * plan. */
const secondReservation = async (fixture: Fixture) => {
  const original = join(fixture.journalDirectory, fixture.initial.workflowId);
  const aside = `${fixture.journalDirectory}.aside`;
  await rename(original, aside);
  fundWallet(fixture);
  const plan = await admit(fixture, fixture.fresh);
  await rename(aside, original);
  expect(await fixture.records()).toHaveLength(2);
  return plan;
};

// A lost workflow journal is resumed only from the one signed original
// execution for its target.
describe("a lost workflow journal resumes only one non-conflicted original", () => {
  it("refuses a conflicted signed original", async () => {
    const fixture = await setupFundingRecoveryFixture();
    const [record] = await fixture.records();
    await fixture.store.markConflict({
      plan: fixture.plan,
      expectedRevision: record!.revision,
      code: "unexpected_spend",
    });
    await rm(fixture.journalDirectory, { recursive: true, force: true });
    await refused(fixture, CONFLICTED);
  });

  it("refuses two signed reservations for the target", async () => {
    const fixture = await unsigned();
    await sign(fixture, await secondReservation(fixture), fixture.fresh);
    await sign(fixture, fixture.plan, fixture.old);
    await rm(fixture.journalDirectory, { recursive: true, force: true });
    await refused(fixture, MULTIPLE);
  });

  it("refuses an empty directory with signed history beside a non-empty journal", async () => {
    const fixture = await unsigned();
    const workflowId = await sign(
      fixture,
      await secondReservation(fixture),
      fixture.fresh,
    );
    await mkdir(join(fixture.journalDirectory, workflowId));
    await refused(fixture, EMPTY_BESIDE);
  });
});

/** A double spend classified by a watcher that also proves wrong network
 * ids, at the original and at a deeper confirmation depth. */
const classifiedDoubleSpend = async () => {
  const sharedInput = outRefCbor(91, 0n);
  const block = await buildCanonicalBlockFixture({
    endTime: 20n,
    transactions: [
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 1n }),
      buildFixtureTransaction({ spendInputs: [sharedInput], fee: 2n }),
    ],
  });
  const classifier = await createHeaderClassifier({
    deploymentFingerprint: deploymentIdentity.manifestId,
    replayer: DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY,
    releaseFinalityAuthority: finality,
  });
  const classify = async (confirmationDepth: number) => {
    const observation = {
      ...authenticatedHeaderObservation(block),
      confirmationDepth,
    };
    const decision = await classifyHeader({
      classifier,
      observation,
      authenticatedObservationDigest:
        await authenticatedStateQueueObservationDigest({
          observation,
          minimumConfirmationDepth: 30,
        }),
      sources: sourcesFor(block.payloadEnvelopeCbor),
    });
    if (decision.decision !== "fault_detected")
      throw new Error("fixture must classify double spend");
    return decision;
  };
  return { old: await classify(30), fresh: await classify(31) };
};

/** A reservation record for `decision` with a confirmed signed attempt. */
const signedRecord = (reservationId: string, decisionDigest: string) => {
  const record = {
    reservationId,
    deploymentFingerprint: deploymentIdentity.manifestId,
    decisionDigest,
    policyDigest: "cc".repeat(32),
    reservationBasisDigest: "dd".repeat(32),
    revision: "1",
    state: "active",
    activeInputs: [],
    pendingTransition: null,
    lastConfirmedTransitionDigest: "ee".repeat(32),
    conflictCode: null,
  };
  return {
    ...record,
    recordDigest: journalJsonDigest(normalizeJournalJson(record)),
  };
};

// Objectives are per category and header: another category's funding on the
// same header is another objective's, never this one's original.
describe("a lost workflow journal resumes beside another objective's funding", () => {
  it("resumes its original beside signed funding of another category on the same header", async () => {
    const { old, fresh } = await classifiedDoubleSpend();
    const { decisionDigest: _, ...unsealed } = old;
    const otherUnsealed = { ...unsealed, category: "networkId" as const };
    const other = {
      ...otherUnsealed,
      decisionDigest: journalJsonDigest(normalizeJournalJson(otherUnsealed)),
    };
    const journalRoot = await createFundingRecoveryDirectory();
    directories.push(journalRoot);
    const decisions = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      launchScope: installedLaunchScope(old.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
    });
    for (const decision of [old, fresh, other])
      await decisions.unsafeAppendDecisionEnvelopeForTest(decision);
    const forbidden = vi.fn(async (): Promise<never> => {
      throw new Error("recovery must not mutate funding");
    });
    const store: WatcherProverFundingReservationStore = {
      readAll: async () => [
        signedRecord("aa".repeat(32), other.decisionDigest),
        signedRecord("bb".repeat(32), old.decisionDigest),
      ],
      readReservedOutRefs: async () => [],
      readAbandonmentHandoff: async () => null,
      readPendingHandoff: async () => null,
      readPendingTransition: async () => null,
      readCompletionHandoff: async () => null,
      readConfirmedInput: async () => null,
      reserve: forbidden,
      prepareTransition: forbidden,
      confirmTransition: forbidden,
      abandonPendingTransition: forbidden,
      acknowledgeAbandonment: forbidden,
      markConflict: forbidden,
      release: forbidden,
    };
    const { permit } = createWorkflowActuationPermitController({
      decision: fresh,
      rollbackGeneration: "1",
    });
    await authorizeWatcherProverFundingRecovery({
      journalRoot,
      journalAuthenticationKey: TEST_JOURNAL_KEY,
      deploymentIdentity,
      actuationPermit: permit,
      category: "doubleSpend",
      rollbackGeneration: "1",
      store,
    });
    expect(
      assertWorkflowActuationPermitIdentity({
        permit,
        category: "doubleSpend",
        rollbackGeneration: "1",
      }).executionDecisionDigest,
    ).toBe(old.decisionDigest);
    expect(forbidden).not.toHaveBeenCalled();
  });
});
