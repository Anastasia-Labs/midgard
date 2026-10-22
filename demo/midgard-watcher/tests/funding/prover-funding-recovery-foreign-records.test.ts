import { readdir, rm } from "node:fs/promises";
import { join } from "node:path";

import {
  computeFraudProofWorkflowId,
  DirectoryFraudProofWorkflowJournalStore,
  journalJsonDigest,
  lostWorkflowFundingJournalRecords,
  normalizeJournalJson,
  recoverWorkflowFundingSubmissionHandoff,
  type WorkflowFundingAbandonmentHandoff,
  type WorkflowFundingCompletionHandoff,
  type WorkflowFundingSubmissionHandoff,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  cleanupFundingRecoveryFixtures,
  setupFundingRecoveryFixture,
} from "../support/fault-proof-funding-fixture.js";
import { fundingTerminal } from "./funding-handoff-fixture.js";
import { substituteAbandonmentTransactionHash } from "./sqlite-prover-funding-reservation-store.signed-transition.js";
import {
  type Fixture,
  fundingOf,
  signReplacement,
} from "./superseded-attempt-replacement.js";

afterEach(async () => {
  vi.restoreAllMocks();
  await cleanupFundingRecoveryFixtures();
});

const OWN_ATTEMPT =
  "lost workflow journal has no exact signed attempt of its own to re-record";
const UNSIGNED_CREATION =
  "lost workflow journal cannot re-record a proof creation it did not sign";
const PRIOR_WORKFLOW =
  "funding handoff differs from its existing prepared workflow";

/** The handoff of the same objective's execution for another target under
 * the same reservation: only its target header differs. */
const foreign = <T extends WorkflowFundingSubmissionHandoff>(
  fixture: Fixture,
  handoff: T,
): T => {
  const identity = {
    ...handoff.identity,
    target: {
      kind: "state_queue_header" as const,
      headerHash: "ab".repeat(fixture.old.headerHash.length / 2),
    },
  };
  return {
    ...handoff,
    identity,
    workflowId: computeFraudProofWorkflowId(identity),
  };
};

/** Signs a new attempt over the reservation's funding and records it in the
 * store under `handoff` (the objective's own one by default). */
const prepare = async (
  fixture: Fixture,
  shape: (
    handoff: WorkflowFundingSubmissionHandoff,
  ) => WorkflowFundingSubmissionHandoff = (handoff) => handoff,
) => {
  const funding = fundingOf(fixture);
  const attempt = signReplacement(
    funding.outRef,
    BigInt(funding.lovelace),
    100n,
  );
  const handoff = shape({
    ...fixture.handoff,
    preflight: {
      ...fixture.handoff.preflight,
      txHash: attempt.transactionHash,
    },
    submissionIntent: {
      ...fixture.handoff.submissionIntent,
      txHash: attempt.transactionHash,
    },
  });
  const [record] = await fixture.records();
  await fixture.store.prepareTransition({
    handoff,
    plan: fixture.plan,
    expectedRevision: record!.revision,
    actionKind: "proof.init",
    ...attempt,
  });
  return handoff;
};

const abandonmentOf = (
  handoff: WorkflowFundingSubmissionHandoff,
): WorkflowFundingAbandonmentHandoff => ({
  workflowId: handoff.workflowId,
  identity: handoff.identity,
  preparedArtifactDigest: handoff.preparedArtifactDigest,
  expectedJournalSequence: handoff.expectedJournalSequence,
  submissionIntent: handoff.submissionIntent,
  reconciliation: {
    kind: "reconciled",
    actionId: handoff.submissionIntent.actionId,
    outcome: "not_found",
    txHash: handoff.submissionIntent.txHash,
  },
});

/** The store abandons the pending attempt under `handoff`. */
const abandon = async (
  fixture: Fixture,
  handoff: WorkflowFundingAbandonmentHandoff,
  acknowledge = false,
) => {
  const [record] = await fixture.records();
  const next = await fixture.store.abandonPendingTransition({
    handoff,
    plan: fixture.plan,
    expectedRevision: record!.revision,
    transitionDigest: record!.pendingTransition!.transitionDigest,
  });
  if (acknowledge)
    await fixture.store.acknowledgeAbandonment({
      handoff,
      plan: fixture.plan,
      expectedRevision: next.revision,
    });
};

/** The store releases the reservation under a completion whose proof token
 * `createdByTxHash` created, anchored as `anchor` says. */
const complete = async (
  fixture: Fixture,
  createdByTxHash: string,
  anchor: Partial<WorkflowFundingCompletionHandoff> = {},
) => {
  const terminal = fundingTerminal(
    fixture.old.headerHash,
    createdByTxHash,
    "ee".repeat(32),
  );
  const [record] = await fixture.records();
  await fixture.store.release({
    handoff: {
      workflowId: fixture.handoff.workflowId,
      identity: fixture.handoff.identity,
      preparedArtifactDigest: fixture.handoff.preparedArtifactDigest,
      expectedJournalSequence: fixture.handoff.expectedJournalSequence,
      completion: {
        kind: "completed",
        terminal,
        terminalDigest: journalJsonDigest(normalizeJournalJson(terminal)),
      },
      ...anchor,
    },
    plan: fixture.plan,
    expectedRevision: record!.revision,
  });
};

/** Loses the workflow journal: what remains is the started and prepared
 * records a recreated journal begins with. */
const loseJournal = async (fixture: Fixture) => {
  const directory = join(fixture.journalDirectory, fixture.initial.workflowId);
  for (const name of await readdir(directory))
    if (name !== "00000000.json" && name !== "00000001.json")
      await rm(join(directory, name));
  expect(
    (
      await new DirectoryFraudProofWorkflowJournalStore(
        fixture.journalDirectory,
      ).load(fixture.initial.workflowId)
    ).map(({ event }) => event.kind),
  ).toEqual(["started", "prepared"]);
};

/** A restarted process is refused the objective before any funding is
 * minted: no wallet query, no action, and the store unchanged. */
const refusedBeforeMint = async (fixture: Fixture, message: string) => {
  const records = await fixture.records();
  const abandonment = await fixture.store.readAbandonmentHandoff(fixture.plan);
  const completion = await fixture.store.readCompletionHandoff(fixture.plan);
  for (const mock of [
    fixture.readWalletUtxos,
    fixture.adapter.preflight,
    fixture.adapter.submit,
  ])
    vi.mocked(mock).mockClear();
  await expect(fixture.recover()).rejects.toThrow(message);
  expect(await fixture.records()).toEqual(records);
  expect(await fixture.store.readAbandonmentHandoff(fixture.plan)).toEqual(
    abandonment,
  );
  expect(await fixture.store.readCompletionHandoff(fixture.plan)).toEqual(
    completion,
  );
  expect(fixture.readWalletUtxos).not.toHaveBeenCalled();
  expect(fixture.adapter.preflight).not.toHaveBeenCalled();
  expect(fixture.adapter.submit).not.toHaveBeenCalled();
};

/** A reservation that has not signed yet, under its objective's journal. */
const unsigned = () => setupFundingRecoveryFixture(false, false, true);

// A lost workflow journal is rebuilt only from the reservation store's own
// signed records of the objective. Any other record there refuses it.
describe("a lost workflow journal is not rebuilt from records of another execution", () => {
  it("refuses an abandoned attempt of another objective", async () => {
    const fixture = await unsigned();
    const own = await prepare(fixture);
    await abandon(fixture, abandonmentOf(foreign(fixture, own)));
    await loseJournal(fixture);
    await refusedBeforeMint(fixture, OWN_ATTEMPT);
  });

  it("refuses an abandoned attempt signed for another objective", async () => {
    const fixture = await unsigned();
    const signed = await prepare(fixture, (handoff) =>
      foreign(fixture, handoff),
    );
    await abandon(fixture, {
      ...abandonmentOf(signed),
      workflowId: fixture.handoff.workflowId,
      identity: fixture.handoff.identity,
    });
    await loseJournal(fixture);
    await refusedBeforeMint(fixture, OWN_ATTEMPT);
  });

  it("refuses an abandoned attempt for an action other than the one signed", async () => {
    const fixture = await unsigned();
    const own = abandonmentOf(await prepare(fixture));
    await abandon(fixture, {
      ...own,
      submissionIntent: { ...own.submissionIntent, actionId: "other" },
      reconciliation: { ...own.reconciliation, actionId: "other" },
    });
    await loseJournal(fixture);
    await refusedBeforeMint(fixture, OWN_ATTEMPT);
  });

  it("refuses an abandoned attempt the store holds no signed submission for", async () => {
    // A store written before submissions were recorded beside their attempt.
    const fixture = await setupFundingRecoveryFixture(false, true);
    await abandon(fixture, abandonmentOf(fixture.handoff));
    await loseJournal(fixture);
    await refusedBeforeMint(fixture, OWN_ATTEMPT);
  });

  it("refuses a completion whose proof creation it never signed", async () => {
    const fixture = await unsigned();
    await complete(fixture, "ab".repeat(32));
    await loseJournal(fixture);
    await refusedBeforeMint(fixture, UNSIGNED_CREATION);
  });

  it("refuses a completion whose proof creation it abandoned", async () => {
    const fixture = await unsigned();
    const own = await prepare(fixture);
    await abandon(fixture, abandonmentOf(own), true);
    await complete(fixture, own.submissionIntent.txHash);
    await loseJournal(fixture);
    await refusedBeforeMint(fixture, UNSIGNED_CREATION);
  });

  it("checks a journal that differs from the store's records as the one that recorded them", async () => {
    const fixture = await unsigned();
    const own = await prepare(fixture);
    await abandon(fixture, abandonmentOf(own));
    await loseJournal(fixture);
    await fixture.append({ ...own.preflight, localEvaluator: "other" });
    await refusedBeforeMint(
      fixture,
      "funding abandonment differs from its journaled signed intent",
    );
  });

  // The store checks each attempt's identity as it reads it; the rebuild
  // checks it again for any other port.
  it("refuses an attempt whose identity differs from its signed transaction", async () => {
    const fixture = await unsigned();
    await abandon(fixture, abandonmentOf(await prepare(fixture)));
    const [saved] = (await fixture.store.readLegacyAbandonedTransactions!(
      fixture.plan,
    )) as { transition: unknown; handoff: WorkflowFundingAbandonmentHandoff }[];
    expect(() =>
      lostWorkflowFundingJournalRecords({
        workflowId: fixture.initial.workflowId,
        abandoned: [
          {
            transition: saved!.transition,
            handoff: substituteAbandonmentTransactionHash(
              saved!.handoff,
              "ab".repeat(32),
            ),
          },
        ],
        submissions: [],
        completion: null,
      }),
    ).toThrow("funding store changed a signed attempt's identity");
  });

  it("refuses to re-anchor a pending attempt ahead of the earlier records", async () => {
    const fixture = await unsigned();
    const own = await prepare(fixture);
    await abandon(fixture, abandonmentOf(own), true);
    const records = lostWorkflowFundingJournalRecords({
      workflowId: fixture.initial.workflowId,
      abandoned: await fixture.store.readLegacyAbandonedTransactions!(
        fixture.plan,
      ),
      submissions: await fixture.store.readRecordedSubmissions!(fixture.plan),
      completion: null,
    });
    expect(records).toHaveLength(3);
    await loseJournal(fixture);
    const entries = await new DirectoryFraudProofWorkflowJournalStore(
      fixture.journalDirectory,
    ).load(fixture.initial.workflowId);
    const append = vi.fn();
    await expect(
      recoverWorkflowFundingSubmissionHandoff({
        journal: {},
        workflowId: fixture.initial.workflowId,
        identity: fixture.initial.identity,
        entries: () => entries,
        append,
        handoff: { ...own, expectedJournalSequence: 5 },
        records,
      }),
    ).rejects.toThrow(
      "funding reservation re-records its pending attempt after its earlier records",
    );
    expect(append).not.toHaveBeenCalled();
  });
});

// A journal that was not lost recorded its handoffs: each is checked against
// it exactly.
describe("a workflow journal that recorded its handoffs", () => {
  it("refuses a stored abandonment anchored in another journal", async () => {
    const fixture = await setupFundingRecoveryFixture();
    await abandon(fixture, {
      ...abandonmentOf(fixture.handoff),
      preparedArtifactDigest: "ab".repeat(32),
    });
    await refusedBeforeMint(fixture, PRIOR_WORKFLOW);
  });

  it("refuses a stored completion anchored in another journal", async () => {
    const fixture = await setupFundingRecoveryFixture();
    await fixture.run(await fixture.recover());
    await complete(fixture, fixture.transactionHash, {
      preparedArtifactDigest: "ab".repeat(32),
    });
    await refusedBeforeMint(fixture, PRIOR_WORKFLOW);
  });
});
