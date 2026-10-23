/** The edges of reclaiming our thread on a header another record removed.
 * The cancel waits until the removal itself is past rollback recovery, so a
 * rollback that drops the removal leaves the run as it was. The run resolves
 * only once its cancel is past rollback recovery, not a block earlier. And
 * the orchestrator itself refuses to resolve while the adapter still sees a
 * thread of ours, wrapped or not: two runs on our key that both opened a
 * thread on one header end with the second one's thread back in our
 * wallet, never stranded. */
import { afterEach, describe, expect, it, vi } from "vitest";

import { MemoryFraudProofWorkflowJournalStore } from "../src/workflow/journal.js";
import type { FraudProofFamilyWorkflowAdapter } from "../src/workflow/orchestrator.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
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
  kinds,
  type Ledger,
  OUR_THREAD,
  reclaimPort,
  removedByAnotherProver,
  resolution,
} from "./workflow-own-thread-reclaim.fixtures.js";

afterEach(() => {
  vi.restoreAllMocks();
});

/** `makeAdapter`'s family on a header a sibling run on our key convicted:
 * its own proof never lands, and once `convicted()` it observes the
 * sibling's completed record. */
const siblingConvicted = (
  convicted: () => boolean,
): FraudProofFamilyWorkflowAdapter => {
  const base = makeAdapter();
  return {
    ...base,
    observe: async (context) =>
      convicted()
        ? {
            kind: "completed",
            terminal: terminal(String(context.artifact.headerHash)),
          }
        : { kind: "pending", reason: "our thread awaits its next step" },
  };
};

/** An adapter with no cancel in front of it that still reports our threads,
 * as a family's own L1 view does. */
const unwrappedSeeing = (
  base: FraudProofFamilyWorkflowAdapter,
  ledger: Ledger,
): FraudProofFamilyWorkflowAdapter => ({
  ...base,
  observeOwnThreads: async () => ledger.threads(),
});

describe("workflow: reclaim edges on a header another record removed", () => {
  it("still waits on our cancel one block short of release finality", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const port = reclaimPort(ledger);
    const adapter = decorate(removedByAnotherProver(ledger), port);
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH - 1);
    expect(await run({ evidence, adapter, journal })).toMatchObject({
      kind: "pending",
      reason: expect.stringContaining(
        `reclaim ${ledger.reclaimTx} of our thread ${OUR_THREAD} awaits release finality`,
      ),
    });
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter, journal }),
      resolution,
    );
  });

  it("sends no cancel until the removal itself is past rollback recovery", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const port = reclaimPort(ledger);
    let depth = 1;
    const adapter = decorate(
      removedByAnotherProver(
        ledger,
        () => true,
        () => depth,
      ),
      port,
    );
    for (depth of [1, FINAL_DEPTH - 1])
      expect(await run({ evidence, adapter, journal })).toMatchObject({
        kind: "pending",
        reason: expect.stringContaining(
          `header removal awaits release finality before our thread ${OUR_THREAD} is reclaimed`,
        ),
      });
    expect(port.preflight).not.toHaveBeenCalled();
    expect(port.submit).not.toHaveBeenCalled();
    depth = FINAL_DEPTH;
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    expect(port.submit).toHaveBeenCalledTimes(1);
    ledger.land(FINAL_DEPTH);
    expectReclaimedThenResolved(await run({ evidence, adapter, journal }), {
      ...resolution,
      confirmationDepth: FINAL_DEPTH,
    });
  });

  it("carries on with the run when a rollback drops the removal before release finality", async () => {
    const evidence = await canonicalEvidence();
    const journal = new MemoryFraudProofWorkflowJournalStore();
    const ledger = chain();
    const port = reclaimPort(ledger);
    let removed = true;
    const base = removedByAnotherProver(
      ledger,
      () => removed,
      () => 1,
    );
    const adapter = decorate(base, port);
    expect((await run({ evidence, adapter, journal })).kind).toBe("pending");
    // The removal is rolled back and never re-lands: the header is live
    // again, our thread still stands, and the run takes its own next step.
    removed = false;
    const resumed = await run({ evidence, adapter, journal });
    if (!("entries" in resumed)) throw new Error("the run kept no journal");
    expect(port.preflight).not.toHaveBeenCalled();
    expect(port.submit).not.toHaveBeenCalled();
    expect(ledger.threads().standing?.threadOutRef).toBe(OUR_THREAD);
    expect(base.submit).toHaveBeenCalled();
    expect(
      resumed.entries.some(
        ({ event }) =>
          event.kind === "submission_intent" && event.txHash === PROOF_TX_HASH,
      ),
    ).toBe(true);
    expect(kinds(resumed.entries)).not.toContain("stalled");
    expect(resumed.kind).not.toBe("resolved_by_another_record");
  });

  it.each([
    {
      path: "another prover's record",
      // The family's stage derivation does not see this thread.
      base: (): FraudProofFamilyWorkflowAdapter => ({
        ...makeAdapter(),
        observe: async () => {
          throw new FraudProofResolvedByAnotherRecordError(resolution);
        },
      }),
    },
    {
      path: "a sibling run's record",
      base: () => siblingConvicted(() => true),
    },
  ])(
    "holds under the operator's alarm, never resolving, while an unwrapped adapter still sees our thread ($path)",
    async ({ base }) => {
      const evidence = await canonicalEvidence();
      const journal = new MemoryFraudProofWorkflowJournalStore();
      const ledger = chain();
      const adapter = unwrappedSeeing(base(), ledger);
      for (let pass = 0; pass < 2; pass += 1)
        expect(await run({ evidence, adapter, journal })).toMatchObject({
          kind: "pending",
          resumeOnObservation: true,
          namedState: {
            name: "fraud-proof-thread-reclaim-unavailable",
            alarm: true,
          },
          reason: expect.stringContaining(
            `our thread ${OUR_THREAD} still holds funds on the removed header and cannot be reclaimed: no reclaim is wired in front of this family's workflow`,
          ),
        });
      // Once our thread is gone, the resolution goes through.
      ledger.land(FINAL_DEPTH);
      expect((await run({ evidence, adapter, journal })).kind).toBe(
        "resolved_by_another_record",
      );
    },
  );

  it("two runs on our key open threads on one header: one convicts, the other cancels its thread before resolving", async () => {
    const evidence = await canonicalEvidence();
    const first = new MemoryFraudProofWorkflowJournalStore();
    const second = new MemoryFraudProofWorkflowJournalStore();
    const firstThread = chain({
      thread: `${"e1".repeat(32)}#0`,
      reclaimTx: "e2".repeat(32),
    });
    const secondThread = chain({
      thread: `${"e3".repeat(32)}#0`,
      reclaimTx: "e4".repeat(32),
    });
    // The first run's own proof consumes its thread.
    firstThread.state.standing = false;
    const firstPort = reclaimPort(firstThread);
    const convicting = decorate(makeAdapter(), firstPort);
    let convicted = false;
    const secondPort = reclaimPort(secondThread);
    const following = decorate(
      siblingConvicted(() => convicted),
      secondPort,
    );

    expect(
      (await run({ evidence, adapter: following, journal: second })).kind,
    ).toBe("pending");
    let won = await run({ evidence, adapter: convicting, journal: first });
    for (let pass = 0; pass < 4 && won.kind === "pending"; pass += 1)
      won = await run({ evidence, adapter: convicting, journal: first });
    expect(won.kind).toBe("completed");
    expect(firstPort.submit).not.toHaveBeenCalled();
    convicted = true;

    // The second run sees the first run's record, never its own: its thread
    // comes back to our wallet before it may resolve.
    expect(
      (await run({ evidence, adapter: following, journal: second })).kind,
    ).toBe("pending");
    expect(secondPort.submit).toHaveBeenCalledTimes(1);
    expect(secondPort.preflight).toHaveBeenCalledWith(
      expect.objectContaining({ threadOutRef: secondThread.thread }),
    );
    secondThread.land(FINAL_DEPTH);
    expectReclaimedThenResolved(
      await run({ evidence, adapter: following, journal: second }),
      {
        removalTxHash: REMOVAL_TX_HASH,
        referencedProofTokenOutRef: `${PROOF_TX_HASH}#0`,
        confirmationDepth: FINAL_DEPTH,
      },
      secondThread,
    );
  });
});
