/** A header removed against a record this run did not create may leave a
 * computation thread of ours standing on it, still holding our lovelace.
 * The run never forgets the objective while it does: it cancels that thread
 * back to our wallet first, and resolves only once the cancel is past
 * rollback recovery. The reclaim is reported as a reclaim,
 * never as a conviction. A family with no cancel holds under a named alarm,
 * and so does a reclaim our wallet cannot fund, until a top-up. */
import { afterEach, describe, expect, it, vi } from "vitest";

import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { createTestWorkflowProcess } from "./support/workflow-process.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
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
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    const port = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), port);

    const submitted = await run({
      evidence,
      adapter,
      process,
      chain: ledger.l1,
    });
    expect(submitted.kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    expect(port.preflight).toHaveBeenCalledWith(
      expect.objectContaining({ threadOutRef: OUR_THREAD, step: 2 }),
    );
    // Not landed: the run waits on its own cancel and builds nothing new.
    expect(
      (await run({ evidence, adapter, process, chain: ledger.l1 })).kind,
    ).toBe("pending");
    // Landed, still within rollback recovery: not yet resolved.
    ledger.land(1);
    const shallow = await run({ evidence, adapter, process, chain: ledger.l1 });
    expect(shallow).toMatchObject({
      kind: "pending",
      reason: expect.stringMatching(/awaits release finality/u),
    });
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, process, chain: ledger.l1 }),
      resolution,
      ledger,
    );
    expect(port.submit).toHaveBeenCalledTimes(1);
  });

  it("after a restart between the cancel's submission and its landing, sends the same cancel again and only one lands", async () => {
    const evidence = await canonicalEvidence();
    const ledger = await chain();
    const before = reclaimPort(ledger);
    expect(
      (
        await run({
          evidence,
          adapter: decorate(removedByAnotherProver(ledger), before),
          process: createTestWorkflowProcess(),
          chain: ledger.l1,
        })
      ).kind,
    ).toBe("pending");
    expect(before.submit).toHaveBeenCalledTimes(1);
    // A fresh process: new adapter, port and in-flight set over the same
    // chain, on which our thread still stands.
    const process = createTestWorkflowProcess();
    const after = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), after);
    expect(
      (await run({ evidence, adapter, process, chain: ledger.l1 })).kind,
    ).toBe("pending");
    expect(after.submit).toHaveBeenCalledTimes(1);
    expect(process.sent().map(({ txHash }) => txHash)).toEqual([
      ledger.reclaimTx,
    ]);
    // Both spend the same thread: exactly one lands.
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, process, chain: ledger.l1 }),
      resolution,
      ledger,
    );
    expect(after.submit).toHaveBeenCalledTimes(1);
  });

  it("reads our threads once in an observation pass, for its own check and the orchestrator's guard alike", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    // No thread of ours stands: the pass resolves, and the guard confirms it
    // from the read the resolution's check made.
    ledger.land(FINAL_DEPTH);
    const port = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), port);
    expect(
      await run({ evidence, adapter, process, chain: ledger.l1 }),
    ).toMatchObject({
      kind: "resolved_by_another_record",
      resolution,
    });
    expect(port.observe).toHaveBeenCalledTimes(1);
  });

  it("holds a family with no cancel under a named alarm, and never resolves while our thread stands", async () => {
    const evidence = await canonicalEvidence();
    const process = createTestWorkflowProcess();
    const ledger = await chain();
    const base = removedByAnotherProver(ledger);
    for (const adapter of [
      base,
      decorate(base, {
        ...reclaimPort(ledger),
        unavailable: () => "no cancel script is deployed",
      }),
    ])
      for (let pass = 0; pass < 2; pass += 1) {
        const held = await run({
          evidence,
          adapter,
          process,
          chain: ledger.l1,
        });
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
    const process = createTestWorkflowProcess();
    const ledger = await chain();
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
      const held = await run({
        evidence,
        adapter,
        process,
        chain: ledger.l1,
      });
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
    expect(
      (await run({ evidence, adapter, process, chain: ledger.l1 })).kind,
    ).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, process, chain: ledger.l1 }),
      resolution,
      ledger,
    );
  });

  it("reclaims our thread before completing on our own record, across a restart between the cancel and its landing", async () => {
    const evidence = await canonicalEvidence();
    const ledger = await chain();
    const ours = makeAdapter({ chain: ledger.l1 });
    // Our proof lands, and the header is then removed against it while a
    // second thread of ours still stands.
    const base: FraudProofFamilyWorkflowAdapter = {
      ...ours,
      observe: async (context) =>
        ledger.l1.landed(PROOF_TX_HASH)
          ? {
              kind: "completed",
              terminal: terminal(String(context.artifact.headerHash)),
            }
          : await ours.observe(context),
    };
    const first = reclaimPort(ledger);
    expect(
      (
        await run({
          evidence,
          adapter: decorate(base, first),
          process: createTestWorkflowProcess(),
          chain: ledger.l1,
        })
      ).kind,
    ).toBe("pending");
    expect(first.submit).toHaveBeenCalledTimes(1);
    const process = createTestWorkflowProcess();
    const port = reclaimPort(ledger);
    const adapter = decorate(base, port);
    expect(
      (await run({ evidence, adapter, process, chain: ledger.l1 })).kind,
    ).toBe("pending");
    ledger.land(FINAL_DEPTH);
    const outcome = await run({ evidence, adapter, process, chain: ledger.l1 });
    expect(outcome.kind).toBe("completed");
    expect(ledger.threads()).toEqual({
      spends: [
        expect.objectContaining({
          txHash: ledger.reclaimTx,
          threadOutRef: ledger.thread,
        }),
      ],
    });
    expect(port.submit).toHaveBeenCalledTimes(1);
  });
});
