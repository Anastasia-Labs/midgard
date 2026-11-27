/**
 * What one block's family pass reads from its predecessor ledger, and what
 * one artifact family's witness holds, as the predecessor ledger grows. Run
 * by `pnpm bench`, not by `test`:
 *
 *   pnpm --dir demo --filter @al-ft/midgard-fault-proofs run bench \
 *     bench/ledger-store-family-pass.bench.test.ts
 *
 * At each predecessor size of `FAMILY_PASS_BENCH_ENTRIES` (default
 * `1000,50000,200000`, comma-separated, ascending) it reports, as one JSON
 * line each:
 *
 * - `pass`: the installed catalogue's replay, but for the transition-trace
 *   family, of an honest block of `FAMILY_PASS_BENCH_EVENTS` (default 64)
 *   transactions (`eventLedgerBlock`), its predecessor held in a memory store
 *   that counts what the replay reads from it: trie nodes and outputs read
 *   per step, the pass's time, and the process's RSS and heap before it and
 *   at their peak during it (the fixture's own memory is in both);
 * - `witness`: the withdrawalMistag artifact of one mistagged withdrawal
 *   over a predecessor holding that many outputs: the node and output counts
 *   of its predecessor witness, the artifact's bytes as stored JSON (and the
 *   witness's share), and the time its admission takes.
 *
 * Every trie operation reads the nodes on the paths to its keys and the
 * branch children it needs there, so its reads grow with the trie's depth,
 * one level for each 16-fold growth of the ledger, and with nothing else of
 * the ledger's size. Nodes read per step and the witness's nodes are asserted
 * to grow no faster: at each larger size, at most the smallest size's count
 * again for each 16-fold growth. Outputs read per step are asserted equal at
 * every size. Time and RSS depend on the machine and its load and are not
 * asserted.
 */
import { describe, expect, it } from "vitest";

import type { LedgerEntry } from "../src/ledger-at.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type LedgerStorage,
} from "../src/ledger-store/index.js";
import {
  admitWithdrawalMistagWorkflowArtifact,
  prepareWithdrawalMistagWorkflowArtifact,
} from "../src/withdrawal-mistag/artifact.js";
import { classifyCanonicalBlockViolations } from "../src/workflow/classification.classify-canonical-block-violations.js";
import { admitCompleteCanonicalReplayPredecessor } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  createCompleteCanonicalReplayUnion,
  WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY,
} from "../src/workflow/complete-replay.create-complete-canonical-replay-union.js";
import type { WorkflowJsonObject } from "../src/workflow/identity.js";
import { testLedgerEntries } from "../tests/support/ledger-store-conformance.js";
import { withdrawalMistagRetainedFixture } from "../tests/support/withdrawal-mistag-retained.js";
import { eventLedgerBlock } from "./event-ledger-block.js";
import { installedFamiliesWithoutTransitionTrace } from "./installed-families.js";
import { sampled } from "./sampled.js";

const positive = (name: string, raw: string): number => {
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 1)
    throw new Error(`${name} must hold positive integers, got ${raw}`);
  return parsed;
};

const SIZES = (process.env.FAMILY_PASS_BENCH_ENTRIES ?? "1000,50000,200000")
  .split(",")
  .map((raw) => positive("FAMILY_PASS_BENCH_ENTRIES", raw));
const EVENTS = positive(
  "FAMILY_PASS_BENCH_EVENTS",
  process.env.FAMILY_PASS_BENCH_EVENTS ?? "64",
);

if (SIZES.some((size, index) => index > 0 && size <= SIZES[index - 1]!))
  throw new Error("FAMILY_PASS_BENCH_ENTRIES must be ascending");

const report = (scenario: string, values: Record<string, unknown>) =>
  console.log(JSON.stringify({ scenario, ...values }));

/** `entries` admitted against `root` to a memory store that counts the trie
 * nodes and outputs read from it once admitted. */
const countedLedger = async (entries: readonly LedgerEntry[], root: string) => {
  const storage = createMemoryLedgerStorage();
  const reads = { nodes: 0, outputs: 0 };
  const counted: LedgerStorage = {
    ...storage,
    readNodes: async (hashes) => {
      reads.nodes += hashes.length;
      return await storage.readNodes(hashes);
    },
    readOutputs: async (outRefs) => {
      reads.outputs += outRefs.length;
      return await storage.readOutputs(outRefs);
    },
  };
  const ledger = await createLedgerStore(counted).admit(entries, root);
  reads.nodes = 0;
  reads.outputs = 0;
  return { ledger, reads };
};

/** The 16-fold growths of the ledger from `smallest` entries to `size`: the
 * levels its trie gains. */
const levelsAdded = (smallest: number, size: number): number =>
  Math.log(size / smallest) / Math.log(16);

const passes = new Map<
  number,
  { nodesPerStep: number; outputsPerStep: number }
>();
const witnesses = new Map<number, { nodes: number; outputs: number }>();

describe(`ledger store family pass over ${SIZES.join(", ")} predecessor entries`, () => {
  it.each(SIZES)("one block's family pass over %i entries", async (size) => {
    const block = await eventLedgerBlock({ events: EVENTS, entries: size });
    const { evidence } = block;
    const { ledger, reads } = await countedLedger(
      block.predecessorEntries,
      evidence.header.prevUtxosRoot,
    );
    const context = {
      predecessor: await admitCompleteCanonicalReplayPredecessor({
        currentEvidence: evidence,
        ledgerAt: async (headerHash) =>
          headerHash === evidence.header.prevHeaderHash ? ledger : undefined,
      }),
    };
    const replay = createCompleteCanonicalReplayUnion(
      installedFamiliesWithoutTransitionTrace().map(({ member }) => member),
    );
    const pass = await sampled(() => replay.replay(evidence, context));
    const steps = Number(evidence.header.transitionStepCount);
    expect(steps).toBe(EVENTS);
    const nodesPerStep = reads.nodes / steps;
    passes.set(size, { nodesPerStep, outputsPerStep: reads.outputs / steps });
    report("pass", {
      entries: size,
      steps,
      detections: pass.result.detections.length,
      nodesRead: reads.nodes,
      nodesReadPerStep: Math.round(nodesPerStep * 100) / 100,
      outputsRead: reads.outputs,
      outputsReadPerStep: Math.round((reads.outputs / steps) * 100) / 100,
      milliseconds: Math.round(pass.milliseconds),
      baselineRssMiB: pass.baselineRssMiB,
      peakRssMiB: pass.peakRssMiB,
      baselineHeapMiB: pass.baselineHeapMiB,
      peakHeapMiB: pass.peakHeapMiB,
    });
  });

  it.each(SIZES)(
    "one withdrawalMistag artifact's witness over %i outputs",
    async (size) => {
      const fixture = await withdrawalMistagRetainedFixture({
        direction: "valid-marked-invalid",
        predecessorExtra: testLedgerEntries(2000, size - 1).map(
          ({ outRef, output }) => ({ key: outRef, value: output }),
        ),
      });
      const { detections } =
        await WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY.replay(
          fixture.evidence,
          fixture.context,
        );
      const classification = await classifyCanonicalBlockViolations({
        evidence: fixture.evidence,
        detections,
        minimumConfirmationDepth: 1,
      });
      if (classification.decision !== "fault_detected")
        throw new Error("the withdrawal is not mistagged");
      // What the workflow store holds: JSON written and read back.
      const stored = JSON.stringify(
        await prepareWithdrawalMistagWorkflowArtifact({
          evidence: fixture.evidence,
          classification,
          replayContext: fixture.context,
        }),
      );
      const artifact = JSON.parse(stored) as WorkflowJsonObject;
      const witness = artifact.predecessorWitness as {
        nodes: unknown[];
        outputs: unknown[];
      };
      const admission = await sampled(() =>
        admitWithdrawalMistagWorkflowArtifact(artifact),
      );
      witnesses.set(size, {
        nodes: witness.nodes.length,
        outputs: witness.outputs.length,
      });
      report("witness", {
        entries: size,
        witnessNodes: witness.nodes.length,
        witnessOutputs: witness.outputs.length,
        artifactBytes: Buffer.byteLength(stored),
        witnessBytes: Buffer.byteLength(JSON.stringify(witness)),
        admissionMilliseconds: Math.round(admission.milliseconds),
      });
    },
  );

  it("reads and witnesses no more than the trie's depth adds", () => {
    const smallest = SIZES[0]!;
    const pass = passes.get(smallest)!;
    const witness = witnesses.get(smallest)!;
    for (const size of SIZES.slice(1)) {
      const growth = 1 + levelsAdded(smallest, size);
      expect(passes.get(size)!.nodesPerStep).toBeLessThanOrEqual(
        pass.nodesPerStep * growth,
      );
      expect(passes.get(size)!.outputsPerStep).toBe(pass.outputsPerStep);
      expect(witnesses.get(size)!.nodes).toBeLessThanOrEqual(
        witness.nodes * growth,
      );
      expect(witnesses.get(size)!.outputs).toBe(witness.outputs);
    }
  });
});
