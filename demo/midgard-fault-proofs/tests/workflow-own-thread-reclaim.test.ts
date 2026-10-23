/** A header removed against a record this run did not create may leave a
 * computation thread of ours standing on it, still holding our lovelace.
 * The run never forgets the objective while it does: it cancels that thread
 * back to our wallet first, as a journaled move, and resolves only once the
 * cancel is past rollback recovery. The reclaim is reported as a reclaim,
 * never as a conviction. A family with no cancel holds under a named alarm,
 * and so does a reclaim our wallet cannot fund, until a top-up. */
import { afterEach, describe, expect, it, vi } from "vitest";

import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REMOVAL_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";
import {
  chain,
  decorate,
  expectReclaimedThenResolved,
  FINAL_DEPTH,
  OUR_THREAD,
  reclaimPort,
  removedByAnotherProver,
  resolution,
} from "./workflow-own-thread-reclaim.fixtures.js";

afterEach(() => {
  vi.restoreAllMocks();
});

describe("workflow: our thread on a header another record removed", () => {
  it("cancels our thread back to our wallet, waits out rollback recovery, then resolves", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const port = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), port);

    const submitted = await run({ evidence, adapter, journal });
    expect(submitted.kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    expect(port.preflight).toHaveBeenCalledWith(
      expect.objectContaining({ threadOutRef: OUR_THREAD, step: 2 }),
    );
    // Not landed: the run waits on its own cancel and builds nothing new.
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    // Landed, still within rollback recovery: not yet resolved.
    ledger.land(1);
    const shallow = await run({ evidence, adapter, journal });
    expect(shallow).toMatchObject({
      kind: "pending",
      reason: expect.stringMatching(/awaits release finality/u),
    });
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, journal }),
      resolution,
    );
    expect(port.submit).toHaveBeenCalledTimes(1);
  });

  it("resumes after a restart between the cancel's submission and its confirmation, submitting it once", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const before = reclaimPort(ledger);
    expect(
      (
        await run({
          evidence,
          adapter: decorate(removedByAnotherProver(ledger), before),
          journal,
        })
      ).kind,
    ).toBe("pending");
    expect(before.submit).toHaveBeenCalledTimes(1);
    // A fresh process: new adapter and port over the same journal and chain.
    const after = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), after);
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, journal }),
      resolution,
    );
    expect(after.submit).not.toHaveBeenCalled();
  });

  it("reads our threads once in an observation pass, for its own check and the orchestrator's guard alike", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    // No thread of ours stands: the pass resolves, and the guard confirms it
    // from the read the resolution's check made.
    ledger.land(FINAL_DEPTH);
    const port = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), port);
    expect(await run({ evidence, adapter, journal })).toMatchObject({
      kind: "resolved_by_another_record",
      resolution,
    });
    expect(port.observe).toHaveBeenCalledTimes(1);
  });

  it("holds a family with no cancel under a named alarm, and never resolves while our thread stands", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const base = removedByAnotherProver(ledger);
    for (const adapter of [
      base,
      decorate(base, {
        ...reclaimPort(ledger),
        unavailable: () => "no cancel script is deployed",
      }),
    ])
      for (let pass = 0; pass < 2; pass += 1) {
        const held = await run({ evidence, adapter, journal });
        expect(held).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
          namedState: {
            name: "fraud-proof-thread-reclaim-unavailable",
            alarm: true,
          },
          reason: expect.stringContaining(OUR_THREAD),
        });
      }
    expect(base.submit).not.toHaveBeenCalled();
  });

  it("holds a reclaim our wallet cannot fund under its own alarm, retries it, and submits it after a top-up", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    let funded = false;
    const funds = reclaimPort(ledger);
    const port = reclaimPort(ledger, async (input) => {
      if (!funded)
        throw new Error(
          "wallet must contain a pure-ADA UTxO with at least 2000000 lovelace for fees",
        );
      return await funds.preflight(input);
    });
    const adapter = decorate(removedByAnotherProver(ledger), port);
    for (let pass = 0; pass < 2; pass += 1) {
      const held = await run({ evidence, adapter, journal });
      expect(held).toMatchObject({
        kind: "pending",
        resumeOnObservation: true,
        namedState: {
          name: "fraud-proof-thread-reclaim-unfunded",
          alarm: true,
        },
        reason: expect.stringMatching(/no deadline applies/u),
      });
    }
    expect(port.preflight).toHaveBeenCalledTimes(2);
    expect(port.submit).not.toHaveBeenCalled();
    funded = true;
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, journal }),
      resolution,
    );
  });

  it("reclaims our thread before resolving when a sibling run's record removed the header", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const siblingProof = "c6".repeat(32);
    const ours = makeAdapter();
    // Our proof lands; the header is then removed against the sibling's
    // record while a second thread of ours still stands.
    const base: FraudProofFamilyWorkflowAdapter = {
      ...ours,
      observe: async (context) => {
        const observed = await ours.observe(context);
        if (
          !context.entries.some(
            ({ event }) =>
              event.kind === "confirmed" && event.txHash === PROOF_TX_HASH,
          )
        )
          return observed;
        const sibling = terminal(String(context.artifact.headerHash));
        return {
          kind: "completed",
          terminal: {
            ...sibling,
            proofToken: {
              ...sibling.proofToken,
              outRef: `${siblingProof}#0`,
              createdByTxHash: siblingProof,
            },
            correction: {
              ...sibling.correction,
              referencedProofTokenOutRef: `${siblingProof}#0`,
            },
          },
        };
      },
    };
    const port = reclaimPort(ledger);
    const adapter = decorate(base, port);
    let outcome = await run({ evidence, adapter, journal });
    for (let step = 0; step < 4 && outcome.kind === "pending"; step += 1)
      outcome = await run({ evidence, adapter, journal });
    expect(outcome.kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH);
    outcome = await run({ evidence, adapter, journal });
    expectReclaimedThenResolved(outcome, {
      removalTxHash: REMOVAL_TX_HASH,
      referencedProofTokenOutRef: `${siblingProof}#0`,
      confirmationDepth: FINAL_DEPTH,
    });
  });
});
