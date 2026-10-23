/** A header removed against a record this run did not create may leave a
 * computation thread of ours standing on it, still holding our lovelace.
 * The run never forgets the objective while it does: it cancels that thread
 * back to our wallet first, as a journaled move, and resolves only once the
 * cancel is past rollback recovery. The reclaim is reported as a reclaim,
 * never as a conviction. A family with no cancel holds under a named alarm,
 * and so does a reclaim our wallet cannot fund, until a top-up. */
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  type FraudProofWorkflowJournalEntry,
  MemoryFraudProofWorkflowJournalStore,
} from "../src/workflow/journal.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowPreflight,
} from "../src/workflow/orchestrator.js";
import {
  type OwnThreadReclaimPort,
  withOwnThreadReclaim,
} from "../src/workflow/own-thread-reclaim.with-own-thread-reclaim.js";
import { FraudProofResolvedByAnotherRecordError } from "../src/workflow/resolved-by-another-record.js";
import {
  canonicalEvidence,
  makeAdapter,
  PROOF_TX_HASH,
  REFERENCE_OUT_REF,
  REFERENCE_SCRIPT_HASH,
  RELEASE_FINALITY_POLICY,
  REMOVAL_TX_HASH,
  run,
  terminal,
} from "./workflow.make-adapter.js";

const FINAL_DEPTH = RELEASE_FINALITY_POLICY.automaticRecoveryMaxDepth + 2;
const OUR_THREAD = `${"d7".repeat(32)}#1`;
const RECLAIM_TX = "d8".repeat(32);
const THREAD_LOVELACE = "9000000";

const resolution = {
  removalTxHash: "c5".repeat(32),
  referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
  confirmationDepth: FINAL_DEPTH,
};

afterEach(() => {
  vi.restoreAllMocks();
});

const kinds = (entries: readonly FraudProofWorkflowJournalEntry[]) =>
  entries.map(({ event }) => event.kind);

/** The chain as our L1 port reads it: our thread standing until its cancel
 * lands, then that cancel at its depth. */
const chain = () => {
  const state: {
    standing: boolean;
    reclaimDepth: number | undefined;
  } = { standing: true, reclaimDepth: undefined };
  return {
    state,
    land: (depth: number) => {
      state.standing = false;
      state.reclaimDepth = depth;
    },
    threads: () => ({
      ...(state.standing
        ? {
            standing: {
              threadOutRef: OUR_THREAD,
              step: 2 as const,
              lovelace: THREAD_LOVELACE,
            },
          }
        : {}),
      spends:
        state.reclaimDepth === undefined
          ? []
          : [
              {
                txHash: RECLAIM_TX,
                threadOutRef: OUR_THREAD,
                confirmationDepth: state.reclaimDepth,
              },
            ],
    }),
  };
};

/** A cancel port over `ledger`, as one process instance holds it. */
const reclaimPort = (
  ledger: ReturnType<typeof chain>,
  preflight: OwnThreadReclaimPort["preflight"] = async ({ actionId }) =>
    ({
      actionId,
      txHash: RECLAIM_TX,
      scriptExecution: "reference_scripts",
      localUplcEvaluation: { status: "passed", evaluator: "uplc-v1" },
      referenceScripts: [
        {
          role: "family-step-cancel",
          outRef: REFERENCE_OUT_REF,
          scriptHash: REFERENCE_SCRIPT_HASH,
        },
      ],
    }) satisfies FraudProofWorkflowPreflight,
) => ({
  observe: vi.fn(async () => ledger.threads()),
  preflight: vi.fn(preflight),
  submit: vi.fn(
    async ({ preflight: built }: { readonly preflight: { txHash: string } }) =>
      built.txHash,
  ),
});

/** `makeAdapter`'s family, its header removed against another prover's
 * record while `removed()` says so, with our thread named as the stage
 * derivation names it. */
const removedByAnotherProver = (
  ledger: ReturnType<typeof chain>,
  removed: () => boolean = () => true,
): FraudProofFamilyWorkflowAdapter => {
  const adapter = makeAdapter();
  return {
    ...adapter,
    observe: async (context) => {
      if (!removed()) return await adapter.observe(context);
      const standing = ledger.threads().standing;
      throw new FraudProofResolvedByAnotherRecordError(
        standing === undefined
          ? resolution
          : { ...resolution, standingThread: standing },
      );
    },
  };
};

const decorate = (
  base: FraudProofFamilyWorkflowAdapter,
  reclaim: OwnThreadReclaimPort,
) =>
  withOwnThreadReclaim({
    category: "doubleSpend",
    base,
    reclaim,
    releaseFinality: { policy: RELEASE_FINALITY_POLICY },
  });

const expectReclaimedThenResolved = (
  outcome: Awaited<ReturnType<typeof run>>,
  expected: typeof resolution,
) => {
  expect(outcome.kind).toBe("resolved_by_another_record");
  if (outcome.kind !== "resolved_by_another_record")
    throw new Error("not resolved");
  expect(outcome.resolution).toEqual(expected);
  expect(outcome.reclaimed).toEqual([
    {
      actionId: `reclaim:${OUR_THREAD}`,
      txHash: RECLAIM_TX,
      threadOutRef: OUR_THREAD,
      lovelace: THREAD_LOVELACE,
    },
  ]);
  // A reclaim is never our conviction: no terminal, no completion.
  expect("terminal" in outcome).toBe(false);
  expect(kinds(outcome.entries)).not.toContain("completed");
  expect(kinds(outcome.entries)).not.toContain("terminal_included");
  return outcome;
};

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
