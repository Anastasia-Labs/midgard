import {
  DirectoryFraudProofWorkflowJournalStore,
  type FraudProofWorkflowJournalEvent,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_QUARANTINED } from "../../src/fault-proofs/fault-proof-quarantine.js";
import { setupFaultProofExecution } from "../support/fault-proof-execution-fixture.js";
import { cleanupFundingRecoveryFixtures } from "../support/fault-proof-funding-fixture.js";

afterEach(cleanupFundingRecoveryFixtures);

const setup = setupFaultProofExecution;

// A refusal while the objective's own funding is minted holds that objective
// alone, as a failure of its run does: nothing is signed before the mint, and
// each re-check runs the same recovery authorization again.
describe("an objective whose own funding is refused is quarantined alone", () => {
  it("holds existing-only recovery without a durable reservation before querying the wallet", async () => {
    const test = await setup(true);
    const before = await test.fixture.records();
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      expect(await test.execution.execute(test.input)).toMatchObject({
        kind: "retryable",
        reason: expect.stringContaining(
          "existing-only funding requires its durable reservation",
        ),
      });
    } finally {
      stderr.mockRestore();
    }
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
  });

  it("holds only its objective when its journal no longer anchors the reservation's stored handoff, before querying the wallet", async () => {
    const test = await setup();
    // The journal moved on to an attempt the reservation never signed.
    const journal = new DirectoryFraudProofWorkflowJournalStore(
      test.fixture.journalDirectory,
    );
    const entries = await journal.load(test.fixture.initial.workflowId);
    const signed = (kind: string) =>
      entries.find(({ event }) => event.kind === kind)!.event;
    const unsigned = "ab".repeat(32);
    for (const event of [
      { kind: "reconciled", actionId: "init", outcome: "not_found" },
      { ...signed("preflight_passed"), txHash: unsigned },
      { ...signed("submission_intent"), txHash: unsigned, attempt: 2 },
    ] as FraudProofWorkflowJournalEvent[]) {
      const sequence = (await journal.load(test.fixture.initial.workflowId))
        .length;
      await journal.append(
        {
          ...entries[0]!,
          sequence,
          recordedAt: new Date().toISOString(),
          event,
        },
        sequence,
      );
    }
    const before = await test.fixture.records();
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      // Every re-check meets the same refusal and stays held, never failed.
      for (let run = 0; run < 2; run += 1)
        expect(await test.execution.execute(test.input)).toMatchObject({
          kind: "retryable",
          resume: "backoff",
        });
    } finally {
      stderr.mockRestore();
    }
    expect(test.execution.readiness()).toEqual([
      {
        reason: WATCHER_FAULT_PROOF_QUARANTINED,
        detail: expect.stringContaining(
          ": funding handoff was superseded by a later submission intent",
        ),
      },
    ]);
    expect(
      test.setAlert.mock.calls.filter(
        ([alert]) =>
          (alert as { code: string }).code === "proof_submission_failure",
      ),
    ).toHaveLength(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(before);
  });

  it("does not classify provider message strings as transport authority", async () => {
    const test = await setup(true);
    test.getUtxos.mockRejectedValueOnce(new Error("provider says ECONNRESET"));
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      // Not a transport backoff: the objective is quarantined under it.
      expect(
        await test.execution.execute({
          ...test.input,
          job: { ...test.input.job, mode: "run" },
          admission: { mode: "run", funding: "create_or_resume" },
        }),
      ).toMatchObject({
        reason: `${WATCHER_FAULT_PROOF_QUARANTINED}: provider says ECONNRESET`,
      });
    } finally {
      stderr.mockRestore();
    }
    expect(test.execution.readiness()).toEqual([
      expect.objectContaining({ reason: WATCHER_FAULT_PROOF_QUARANTINED }),
    ]);
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.setAlert).toHaveBeenCalledWith(
      expect.objectContaining({ active: true }),
    );
  });
});
