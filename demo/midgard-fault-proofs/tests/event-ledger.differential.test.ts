/**
 * The ledger store walk (`ledgerBeforeStep`, `ledgerBeforeTraceStep`)
 * against a from-scratch replay of the same block (`referenceLedgerBefore`),
 * which rebuilds every intermediate ledger and root-checks it with a fresh
 * trie, and proves from the MPF library's own trie. For every transaction
 * event of a block and every step, the ledger the walk gives before it, or
 * its refusal, must be the reference's: the same root, outputs and proofs,
 * whatever was asked of the walk before, and however its requests
 * interleave. Seeded blocks carry one fault each at a seeded step; the
 * input-existence chronologies add forced transactions, withdrawals and
 * deposits.
 */
import { describe, expect, it } from "vitest";

import {
  ledgerBeforeStep,
  ledgerBeforeTraceStep,
} from "../src/event-ledger.ledger.js";
import { traceRelative } from "../src/event-ledger.steps.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import type { Ledger } from "../src/ledger-store/index.js";
import { completeCanonicalReplayPredecessorStoreLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import {
  ledgerSnapshot,
  type Outcome,
  outcome,
  referenceSnapshot,
} from "./helpers/event-ledger-ledger-snapshots.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import {
  afresh,
  FAULTS,
  hex,
  type Request,
  requests,
  seeded,
  seededBlock,
  shuffled,
  withdrawalOf,
} from "./helpers/event-ledger-seeded-blocks.js";
import {
  inputChronologies,
  type InputChronology,
  inputExistenceChronologyFixture,
  type RefusedInputChronology,
} from "./helpers/input-existence-chronology-fixture.js";

/**
 * Compares, for every request: the reference; one fresh walk asked every
 * request in drawn order, some of them concurrently; the block's own walk,
 * whatever was asked of it before, asked them in that order; and a fresh
 * walk per request. Then, for every step, the trace-relative form. Returns
 * the refusal reasons met.
 */
const compareWithReference = async ({
  block,
  predecessor,
  probes,
  seed,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: Ledger | undefined;
  readonly probes: readonly Buffer[];
  readonly seed: number;
}) => {
  const draw = seeded(seed);
  const asked = shuffled(draw, requests(block, draw));
  const walk = (walked: CanonicalBlockEvidence, request: Request) =>
    outcome(
      () => ledgerBeforeStep({ block: walked, ...request, predecessor }),
      (found) => ledgerSnapshot(found, probes),
    );
  const concurrent = afresh(block);
  const shared = new Map<Request, Promise<Outcome>>();
  for (let start = 0; start < asked.length; ) {
    const batch = asked.slice(start, (start += 1 + draw(4)));
    const settled = batch.map((request) => walk(concurrent, request));
    for (const [index, request] of batch.entries())
      shared.set(request, settled[index]!);
    await Promise.all(settled);
  }
  const own = new Map<Request, Outcome>();
  for (const request of asked) own.set(request, await walk(block, request));
  const refusals = new Set<string>();
  for (const request of asked) {
    const expected = await outcome(
      () => referenceLedgerBefore({ block, predecessor, ...request }),
      (reference) => referenceSnapshot(reference, probes),
    );
    expect(await shared.get(request)).toStrictEqual(expected);
    expect(own.get(request)).toStrictEqual(expected);
    expect(await walk(afresh(block), request)).toStrictEqual(expected);
    refusals.add(
      "refused" in expected
        ? String(expected.refused.reason ?? expected.refused.message)
        : "ledger",
    );
  }
  const { reconstruction } = block;
  for (
    let step = 0n;
    step <= reconstruction.header.transitionStepCount;
    step += 1n
  )
    expect(
      await outcome(
        () =>
          ledgerBeforeTraceStep(
            afresh(block).reconstruction,
            step,
            predecessor,
          ),
        (found) => ledgerSnapshot(found, probes),
      ),
    ).toStrictEqual(
      await outcome(
        () =>
          traceRelative(
            reconstruction,
            step,
            async (eventKey) =>
              await referenceLedgerBefore({
                block,
                predecessor,
                eventKey,
                stepIndex: step,
              }),
          ),
        (reference) => referenceSnapshot(reference, probes),
      ),
    );
  return refusals;
};

describe("ledger store walk against a fresh replay", () => {
  it.each(FAULTS)("seeded blocks with %s", async (fault) => {
    const seen = new Set<string>();
    for (let seed = 1; seed <= 6; seed += 1) {
      const blockSeed = FAULTS.indexOf(fault) * 100 + seed;
      const block = await seededBlock(blockSeed, fault);
      for (const reason of await compareWithReference({
        block: block.evidence,
        predecessor: completeCanonicalReplayPredecessorStoreLedger(block),
        probes: block.probes,
        seed: blockSeed,
      }))
        seen.add(reason);
    }
    // Each fault's own refusals are met, so its comparisons are not vacuous.
    const met = {
      none: ["ledger", "selected_pre_root_changed", "step_out_of_range"],
      absent_input: ["transaction_input_absent"],
      repeated_input: ["transaction_input_absent"],
      rejected: ["ledger"],
      output_present: ["transaction_output_already_present"],
      foreign_post_root: ["post_root_changed"],
      foreign_pre_root: [
        "transition_link_changed",
        "selected_pre_root_changed",
      ],
      non_canonical_output: ["not a canonical ledger entry"],
    }[fault];
    for (const reason of met)
      expect([...seen].some((found) => found.includes(reason))).toBe(true);
  });

  /** Each chronology, the scenario it builds, its committed withdrawal, and
   * the refusal some request of it meets: `ledger` when none refuses. */
  const chronologies: readonly (readonly [
    string,
    InputChronology | RefusedInputChronology,
    ReturnType<typeof withdrawalOf> | undefined,
    string,
  ])[] = [
    ...inputChronologies
      .filter((scenario) => !scenario.startsWith("withdrawal_"))
      .map((scenario) => [scenario, scenario, undefined, "ledger"] as const),
    [
      "forced_after_failed_normal",
      "forced_after_failed_normal",
      undefined,
      "transaction_input_absent",
    ],
    [
      "withdrawal_consumed",
      "withdrawal_consumed",
      withdrawalOf(0x51, true),
      "ledger",
    ],
    [
      "withdrawal_untouched",
      "withdrawal_untouched",
      withdrawalOf(0x51, true),
      "ledger",
    ],
    [
      "withdrawal of an absent output",
      "withdrawal_untouched",
      withdrawalOf(0x5f, true),
      "withdrawal_input_absent",
    ],
    [
      "rejected withdrawal",
      "withdrawal_consumed",
      withdrawalOf(0x51, false),
      "ledger",
    ],
  ];

  it.each(chronologies)(
    "the %s chronology",
    async (_, scenario, withdrawal, met) => {
      const fixture = await inputExistenceChronologyFixture(
        scenario,
        0,
        undefined,
        withdrawal?.committed,
      );
      const entries = [
        ...fixture.predecessorEntries.map(({ outRef: key }) => key),
        fixture.selected,
        outRefCbor(0x5f, 0n),
      ];
      const seen = await compareWithReference({
        block: fixture.evidence,
        predecessor: fixture.predecessor,
        probes: [...new Map(entries.map((key) => [hex(key), key])).values()],
        seed: scenario.length * 7 + (withdrawal?.seed ?? 0),
      });
      expect(seen).toContain(met);
    },
  );
});
