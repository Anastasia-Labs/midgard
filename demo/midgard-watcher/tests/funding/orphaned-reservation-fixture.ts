import { rm } from "node:fs/promises";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  createWorkflowActuationPermitController,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  type FraudProofWorkflowJournalEvent,
  type FraudProofWorkflowJournalStore,
  type HeaderFaultDecision,
} from "@al-ft/midgard-fault-proofs";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.js";
import {
  closeWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { createWatcherProverFundingAuthorityFactory } from "../../src/funding/prover-funding-authority.js";
import type {
  WatcherProverFundingReservationPlan,
  WatcherProverFundingReservationStore,
} from "../../src/funding/prover-funding-reservation.js";
import {
  deploymentIdentity,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import {
  recordObjectives,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";
import type {
  Fixture,
  signReplacement,
} from "./superseded-attempt-replacement.js";

/** Loses every journal under the journal root, the decision journal and each
 * workflow journal, as a lost data directory beside a surviving funding
 * store would. The queue records the objective again, and the decision
 * journal holds only `decisions`. */
export const loseJournals = async (
  fixture: Fixture,
  decisions: readonly HeaderFaultDecision[],
) => {
  closeWatcherJournalDatabase(fixture.journalRoot);
  const database = join(fixture.journalRoot, WATCHER_JOURNAL_DATABASE_FILE);
  for (const suffix of ["", "-wal", "-shm"])
    await rm(database + suffix, { force: true });
  await rm(join(fixture.journalRoot, "fault-proofs"), {
    recursive: true,
    force: true,
  });
  recordObjectives(fixture.journalRoot, [
    { category: "doubleSpend", headerHash: fixture.old.headerHash },
  ]);
  const journal = await openWatcherFaultDecisionJournal({
    directory: fixture.journalRoot,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope: installedLaunchScope(fixture.old.launchScope),
    authenticationKey: TEST_JOURNAL_KEY,
  });
  for (const decision of decisions) await journal.appendLiveDecision(decision);
};

/** The wallet holds enough for a second reservation beside the first. */
export const fundWallet = (fixture: Fixture) =>
  fixture.walletUtxos.push(
    ...[0, 1, 2, 3].map((outputIndex) => ({
      txHash: "98".repeat(32),
      outputIndex,
      address: walletAddress,
      assets: { lovelace: 2_000_000_000n },
    })),
  );

type FactoryInput = Parameters<
  typeof createWatcherProverFundingAuthorityFactory
>[0];

/** The production funding factory over the fixture's store, reading signed
 * attempts through `reads`, with `overrides` in place of store operations.
 * Each reservation it makes is kept in `plans`. */
export const orphanReadingFactory = (
  fixture: Fixture,
  reads: Partial<
    Pick<FactoryInput, "observeSignedTransaction" | "now" | "alertSink">
  > = { observeSignedTransaction: fixture.observeSignedTransaction },
  overrides: Partial<WatcherProverFundingReservationStore> = {},
) => {
  const plans: WatcherProverFundingReservationPlan[] = [];
  const store = fixture.store;
  const factory = createWatcherProverFundingAuthorityFactory({
    launchScope: installedLaunchScope(fixture.old.launchScope),
    journalRoot: fixture.journalRoot,
    journalAuthenticationKey: TEST_JOURNAL_KEY,
    deploymentIdentity,
    protocolParameters: fixture.protocolParameters,
    protocolParameterHistory: fixture.protocolParameterHistory,
    ...reads,
    store: {
      ...store,
      releaseUnused: (record) => store.releaseUnused!(record),
      reserve: async (plan, expectedIdleRevision) => {
        plans.push(plan);
        return await store.reserve(plan, expectedIdleRevision);
      },
      ...overrides,
    },
  });
  return { factory, plans };
};

/** Admits the fresh decision through `factory`, as a restarted process does,
 * and returns its bound workflow journal. */
export const admitFresh = async (
  fixture: Fixture,
  factory: ReturnType<typeof orphanReadingFactory>["factory"],
  generation = "2",
) => {
  const controller = createWorkflowActuationPermitController({
    decision: fixture.fresh,
    rollbackGeneration: generation,
  });
  const permit = await factory.create({
    category: "doubleSpend",
    runner: fixture.runner,
    actuationPermit: controller.permit,
    rollbackGeneration: generation,
    decisionDigest: fixture.fresh.decisionDigest,
    walletAddress,
    walletUtxos: fixture.walletUtxos,
    readWalletUtxos: fixture.readWalletUtxos,
    resolveInputs: fixture.resolveInputs,
    resolveProtocolInputAuthority: async () => {
      throw new Error("unexpected protocol input lookup");
    },
  });
  return fixture.bind(fixture.fresh, {
    permit,
    controller,
    releaseUnused: () =>
      factory.releaseUnused({ actuationPermit: controller.permit }),
  });
};

/** The fresh decision's workflow, the one a hand-over moves attempts into. */
export const freshWorkflow = (fixture: Fixture) => {
  const identity = {
    ...fixture.handoff.identity,
    decisionDigest: fixture.fresh.decisionDigest,
  };
  return { identity, workflowId: computeFraudProofWorkflowId(identity) };
};

/** Records `replacement` as the fresh workflow's next init attempt, exactly
 * as a submission does: the store handoff, then preflight, intent,
 * submission and a pending read. */
export const recordFreshReplacement = async (
  fixture: Fixture,
  journal: FraudProofWorkflowJournalStore,
  plan: WatcherProverFundingReservationPlan,
  replacement: ReturnType<typeof signReplacement>,
  attempt: number,
) => {
  const { identity, workflowId } = freshWorkflow(fixture);
  const entries = await journal.load(workflowId);
  const prepared = entries[1]!.event;
  if (prepared.kind !== "prepared") throw new Error("missing prepared record");
  const record = (await fixture.records()).find(
    ({ reservationId }) => reservationId === plan.reservationId,
  )!;
  const handoff = {
    ...fixture.handoff,
    workflowId,
    identity,
    preparedArtifactDigest: prepared.artifactDigest,
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
    plan,
    expectedRevision: record.revision,
    actionKind: "proof.init",
    ...replacement,
  });
  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const current = await journal.load(workflowId);
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId,
        identity,
        sequence: current.length,
        recordedAt: new Date().toISOString(),
        event,
      },
      current.length,
    );
  };
  await append(handoff.preflight);
  await append(handoff.submissionIntent);
  await append({
    kind: "submitted",
    actionId: "init",
    attempt,
    txHash: replacement.transactionHash,
  });
  await append({
    kind: "reconciled",
    actionId: "init",
    outcome: "pending",
    txHash: replacement.transactionHash,
  });
};

/** Every out-ref leased by more than one active reservation. */
export const doublyLeased = async (fixture: Fixture) => {
  const seen = new Map<string, number>();
  for (const record of await fixture.records())
    if (record.state === "active")
      for (const { outRef } of record.activeInputs)
        seen.set(outRef, (seen.get(outRef) ?? 0) + 1);
  return [...seen].filter(([, count]) => count > 1).map(([outRef]) => outRef);
};
