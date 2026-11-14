/**
 * The event-ledger walk against a from-scratch replay of the same block
 * (`referenceLedgerBefore`), which rebuilds every intermediate ledger and
 * root-checks it with a fresh trie. For every transaction event of a block
 * and every step, the ledger the walk gives before it, or its refusal, must
 * be the reference's: whatever was asked of the walk before, and however its
 * requests interleave. Seeded blocks carry one fault each at a seeded step;
 * the input-existence chronologies add forced transactions, withdrawals and
 * deposits.
 */
import type * as SDK from "@al-ft/midgard-sdk";
import { describe, expect, it } from "vitest";

import { eventLedgerBefore } from "../src/event-ledger.js";
import type { CanonicalBlockEvidence } from "../src/evidence/canonical-block-evidence.js";
import { type CheckedLedger, checkedLedgerPreState } from "../src/ledger-at.js";
import {
  keyValuePhasRoot,
  mutableKeyValuePhasTrie,
} from "../src/transition-trace/phas.js";
import { completeCanonicalReplayPredecessorLedger } from "../src/workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import { outRefCbor } from "./helpers/canonical-block-evidence-fixture.js";
import { referenceLedgerBefore } from "./helpers/event-ledger-reference.js";
import {
  bytes,
  type Draw,
  FAULTS,
  hex,
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

type Outcome =
  | Readonly<{ ledger: unknown }>
  | Readonly<{
      refused: Readonly<{
        name: string;
        message: string;
        reason: unknown;
        step: unknown;
      }>;
    }>;

const ledgerSnapshot = async (
  ledger: CheckedLedger,
  probes: readonly Buffer[],
) => {
  const each: [string, string][] = [];
  ledger.outputs.forEach((output, key) => each.push([key, hex(output)]));
  return {
    utxosRoot: ledger.utxosRoot,
    entries: ledger.entries.map(({ outRef: key, output, descriptor }) => [
      hex(key),
      hex(output),
      descriptor === undefined ? undefined : hex(descriptor),
    ]),
    trie: {
      root: ledger.trie.root,
      count: ledger.trie.count,
      entries: ledger.trie.entries.map(({ key, value }) => [
        hex(key),
        hex(value),
      ]),
    },
    outputs: [...ledger.outputs].map(([key, output]) => [key, hex(output)]),
    keys: [...ledger.outputs.keys()],
    values: [...ledger.outputs.values()].map(hex),
    each,
    size: ledger.outputs.size,
    probes: probes.map((key) => [
      ledger.outputs.has(hex(key)),
      ledger.outputs.get(hex(key)) === undefined
        ? undefined
        : hex(ledger.outputs.get(hex(key))!),
    ]),
    proofs: (await checkedLedgerPreState(ledger).proveResolution(probes)).map(
      hex,
    ),
  };
};

const outcome = async (
  run: () => Promise<CheckedLedger>,
  probes: readonly Buffer[],
): Promise<Outcome> => {
  let ledger: CheckedLedger;
  try {
    ledger = await run();
  } catch (error) {
    if (!(error instanceof Error)) throw error;
    const { reason, step } = error as Error & {
      reason?: unknown;
      step?: unknown;
    };
    return {
      refused: {
        name: error.constructor.name,
        message: error.message,
        reason,
        step,
      },
    };
  }
  return { ledger: await ledgerSnapshot(ledger, probes) };
};

type Request = Readonly<{ eventKey: SDK.EventKey; stepIndex: bigint }>;

/** Every withdrawal and transaction event of `block` at every step, and
 * requests outside them: a forced event and a withdrawal the block does not
 * hold, steps out of range, and a deposit, which has no event ledger. */
const requests = (block: CanonicalBlockEvidence, draw: Draw): Request[] => {
  const steps = block.header.transitionStepCount;
  const keys: SDK.EventKey[] = [];
  for (const { value } of block.reconstruction.traceByStepIndex.values())
    if (!("DepositEventKey" in value.event_key)) keys.push(value.event_key);
  const absent = () => ({
    transactionId: hex(bytes(draw, 32)),
    outputIndex: 0n,
  });
  keys.push(
    { ForcedTransactionEventKey: { tx_order_id: absent() } },
    { WithdrawalEventKey: { withdrawal_id: absent() } },
  );
  const all: Request[] = [];
  for (const eventKey of keys)
    for (let stepIndex = -1n; stepIndex <= steps; stepIndex += 1n)
      all.push({ eventKey, stepIndex });
  all.push({
    eventKey: { DepositEventKey: { deposit_id: absent() } },
    stepIndex: 0n,
  });
  return all;
};

/** `block` with a reconstruction object of its own, so that its walk
 * starts afresh: a block's one walk is shared by its reconstruction. */
const afresh = (block: CanonicalBlockEvidence): CanonicalBlockEvidence => ({
  ...block,
  reconstruction: { ...block.reconstruction },
});

/**
 * Compares, for every request: the reference; one fresh walk asked every
 * request in drawn order, some of them concurrently; the block's own walk,
 * whatever was asked of it before, asked them in that order; and a fresh
 * walk per request.
 */
const compareWithReference = async ({
  block,
  predecessor,
  probes,
  seed,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly predecessor: CheckedLedger | undefined;
  readonly probes: readonly Buffer[];
  readonly seed: number;
}) => {
  const draw = seeded(seed);
  const asked = shuffled(draw, requests(block, draw));
  const ask = (walked: CanonicalBlockEvidence, request: Request) => () =>
    eventLedgerBefore({ block: walked, ...request, predecessor });
  const concurrent = afresh(block);
  const shared = new Map<Request, Promise<Outcome>>();
  for (let start = 0; start < asked.length; ) {
    const batch = asked.slice(start, (start += 1 + draw(4)));
    const settled = batch.map((request) =>
      outcome(ask(concurrent, request), probes),
    );
    for (const [index, request] of batch.entries())
      shared.set(request, settled[index]!);
    await Promise.all(settled);
  }
  const own = new Map<Request, Outcome>();
  for (const request of asked)
    own.set(request, await outcome(ask(block, request), probes));
  const refusals = new Set<string>();
  for (const request of asked) {
    const expected = await outcome(
      () => referenceLedgerBefore({ block, predecessor, ...request }),
      probes,
    );
    expect(await shared.get(request)).toStrictEqual(expected);
    expect(own.get(request)).toStrictEqual(expected);
    expect(await outcome(ask(afresh(block), request), probes)).toStrictEqual(
      expected,
    );
    refusals.add(
      "refused" in expected
        ? String(expected.refused.reason ?? expected.refused.message)
        : "ledger",
    );
  }
  return refusals;
};

describe("mutableKeyValuePhasTrie", () => {
  it("opens the root of the entries it holds after every edit", async () => {
    const draw = seeded(894);
    const keys = Array.from({ length: 48 }, () => bytes(draw, 1 + draw(40)));
    const held = new Map<string, Buffer>();
    const trie = await mutableKeyValuePhasTrie([]);
    let emptied = 0;
    for (let edit = 0; edit < 600; edit += 1) {
      // Drain to empty every so often, so the trie grows from empty again.
      const draining = Math.floor(edit / 100) % 2 === 1 && held.size > 0;
      const key = draining
        ? Buffer.from([...held.keys()][0]!, "hex")
        : keys[draw(keys.length)]!;
      if (held.has(hex(key))) {
        await trie.delete(key);
        held.delete(hex(key));
      } else {
        const value = bytes(draw, 1 + draw(64));
        await trie.insert(key, value);
        held.set(hex(key), value);
      }
      if (held.size === 0) emptied += 1;
      expect(trie.root()).toBe(
        await keyValuePhasRoot(
          [...held].map(([held, value]) => ({
            key: Buffer.from(held, "hex"),
            value,
          })),
        ),
      );
    }
    expect(emptied).toBeGreaterThan(1);
  });

  it("starts from the entries it is given", async () => {
    const draw = seeded(895);
    const entries = Array.from({ length: 20 }, () => ({
      key: bytes(draw, 32),
      value: bytes(draw, 8),
    }));
    const trie = await mutableKeyValuePhasTrie(entries);
    expect(trie.root()).toBe(await keyValuePhasRoot(entries));
    for (const { key } of entries) await trie.delete(key);
    expect(trie.root()).toBe(await keyValuePhasRoot([]));
  });
});

describe("event-ledger walk against a fresh replay", () => {
  it.each(FAULTS)("seeded blocks with %s", async (fault) => {
    const seen = new Set<string>();
    for (let seed = 1; seed <= 6; seed += 1) {
      const blockSeed = FAULTS.indexOf(fault) * 100 + seed;
      const block = await seededBlock(blockSeed, fault);
      for (const reason of await compareWithReference({
        block: block.evidence,
        predecessor: completeCanonicalReplayPredecessorLedger(block),
        probes: block.probes,
        seed: blockSeed,
      }))
        seen.add(reason);
    }
    // Each fault's own refusal is met, so its comparisons are not vacuous.
    const met = {
      none: "ledger",
      absent_input: "transaction_input_absent",
      repeated_input: "transaction_input_absent",
      rejected: "ledger",
      output_present: "transaction_output_already_present",
      foreign_post_root: "post_root_changed",
      foreign_pre_root: "transition_link_changed",
      non_canonical_output: "not a canonical ledger entry",
    }[fault];
    expect([...seen].some((reason) => reason.includes(met))).toBe(true);
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
