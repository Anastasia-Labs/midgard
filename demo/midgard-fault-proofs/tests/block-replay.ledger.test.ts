/**
 * The block replay over a parent `Ledger`: it reads only the out-refs the
 * block's events name and never walks the whole ledger, and it gives every
 * event the roots, verdicts and fault text it gave when it took the whole
 * parent ledger as an array. The pinned roots were captured from a run of
 * that earlier replay over these blocks.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import { outRefFromByte } from "@al-ft/midgard-validation/tests/validation-fixtures";
import { Cause, Effect, Exit } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { isReplayLocalFault, replayBlockEvents } from "../src/block-replay.js";
import {
  createLedgerStore,
  createMemoryLedgerStorage,
  type Ledger,
} from "../src/ledger-store/index.js";
import {
  blockFaultOf as faultOf,
  FIXTURE_REPLAY_SETTINGS,
  fixtureBlock,
  type FixtureEvent,
  fixtureOutput,
  fundedEntry,
  type LedgerEntry,
  producedOutRef,
  recordingLedger,
} from "./support/block-replay-block.js";

beforeAll(() => ledgerEd25519SignatureReady());

const PARENT_ROOT =
  "ff8114793a2292839d3d542525481f5596b01955fc28888a14585bbe0fe7195e";

const parent = Array.from({ length: 8 }, (_, index) =>
  fundedEntry(0x21 + index),
);
const p = (index: number) => parent[index]!.outRef;

const admitted = (entries: readonly LedgerEntry[], root: string) =>
  createLedgerStore(createMemoryLedgerStorage()).admit(entries, root);

/** Orders `[key, value]` pairs by key, in code-unit order. */
const byKey = (
  [a]: readonly [string, unknown],
  [b]: readonly [string, unknown],
) => (a < b ? -1 : a > b ? 1 : 0);

const entriesOf = async (ledger: Ledger) => {
  const entries: [string, string][] = [];
  for await (const { outRef, output } of ledger.entries())
    entries.push([outRef.toString("hex"), output.toString("hex")]);
  return entries.sort(byKey);
};

const sortedEntries = (entries: readonly LedgerEntry[]) =>
  entries
    .map(({ outRef, output }): [string, string] => [
      outRef.toString("hex"),
      output.toString("hex"),
    ])
    .sort(byKey);

/** Replays `events` on the parent ledger, recording what each event gave. */
const replay = async (events: readonly FixtureEvent[], reads?: string[]) => {
  const block = await fixtureBlock(parent, events);
  const parentLedger = await admitted(parent, PARENT_ROOT);
  const roots: string[] = [];
  const mutations: [string, string | null][][] = [];
  const forcedCodes: string[] = [];
  const exit = await Effect.runPromiseExit(
    replayBlockEvents({
      header: block.payload.block_body.header,
      payload: block.payload,
      parent:
        reads === undefined
          ? parentLedger
          : recordingLedger(parentLedger, reads),
      ...FIXTURE_REPLAY_SETTINGS,
      ...block.material,
      verifyForcedSource: ({ rejection }) =>
        Effect.sync(() => void forcedCodes.push(rejection?.code ?? "accepted")),
      onReplayedEvent: (event) => {
        roots.push(event.root);
        mutations.push(
          event.mutations
            .map(({ key, output }): [string, string | null] => [
              key,
              output?.toString("hex") ?? null,
            ])
            .sort(byKey),
        );
      },
    }),
  );
  return { block, exit, roots, mutations, forcedCodes };
};

const t1: FixtureEvent = {
  phase: "L2Transaction",
  spend: [p(5)],
  references: [p(6)],
};
const f1: FixtureEvent = {
  phase: "ForcedTransaction",
  spend: [p(2)],
  references: [p(3)],
};
const f3: FixtureEvent = { phase: "ForcedTransaction", spend: [p(4)] };
const t2: FixtureEvent = {
  phase: "L2Transaction",
  spend: [producedOutRef(t1), p(7)],
};
/** Two of each event kind; one forced transaction names an absent input. */
const everyKind: readonly FixtureEvent[] = [
  { phase: "Withdrawal", outRef: p(0) },
  { phase: "Withdrawal", outRef: p(1) },
  f1,
  { phase: "ForcedTransaction", spend: [outRefFromByte(0xee)] },
  f3,
  t1,
  t2,
  {
    phase: "Deposit",
    outRef: outRefFromByte(0xd1),
    output: fixtureOutput(5_000_000n),
  },
  {
    phase: "Deposit",
    outRef: outRefFromByte(0xd2),
    output: fixtureOutput(6_000_000n),
  },
];
const EVERY_KIND_ROOTS = [
  "fb5b4b6e0de3d6e08bf19e324cc7bde91e3f05f632442c19289bd8fa8a4d4dbc",
  "eade646880ee8bf557b5728876ae5989054252be793f1ea6e9c68aea7cd9dc57",
  "37ebe03ed72455402618d45f7261d54a62dfffc583c0fe1e928fd5382bcd2cc7",
  "37ebe03ed72455402618d45f7261d54a62dfffc583c0fe1e928fd5382bcd2cc7",
  "4a52f2eb5449768ed9a9e161f2a474f348befe7305dfe4061b5ed21855b42f40",
  "25dafa368b40769e105b060e6682aeae433c8c930e18ddc3e22bae6d765fa7bf",
  "318e30ecd846e2120873d4480f7afb19806fcbf66ecce81f6336c882d55064bf",
  "1ff159366947b464603c3768566a4f6300b0d4555511d495ef0e598cb4bf2b8c",
  "c17885cb48d332eea48443dddfa3bbfa0c1eaf1cfbc369c160601d6552b725f0",
];

const hex = (outRef: Buffer) => outRef.toString("hex");
const funded = fixtureOutput().toString("hex");

describe("the block replay over a parent ledger", () => {
  it("reads only the out-refs the block's events name and never lists the ledger", async () => {
    const reads: string[] = [];
    const { block, exit } = await replay(everyKind, reads);
    expect(faultOf(exit)).toBeUndefined();
    if (Exit.isFailure(exit)) return;
    expect(exit.value.root).toBe(EVERY_KIND_ROOTS.at(-1));
    expect(reads.length).toBeGreaterThan(0);
    expect(reads.filter((outRef) => !block.named.has(outRef))).toEqual([]);
  }, 120_000);

  it("gives every event of every kind the roots, verdicts, changes and ledger it gave before", async () => {
    const { block, exit, roots, mutations, forcedCodes } =
      await replay(everyKind);
    expect(faultOf(exit)).toBeUndefined();
    if (Exit.isFailure(exit)) return;
    expect(block.roots).toEqual([PARENT_ROOT, ...EVERY_KIND_ROOTS]);
    expect(roots).toEqual(EVERY_KIND_ROOTS);
    expect(exit.value.root).toBe(EVERY_KIND_ROOTS.at(-1));
    expect(block.verdicts).toEqual([
      undefined,
      undefined,
      "accepted",
      "rejected",
      "accepted",
      "accepted",
      "accepted",
      undefined,
      undefined,
    ]);
    expect(forcedCodes).toEqual(["accepted", "E_INPUT_NOT_FOUND", "accepted"]);
    const sorted = (changes: [string, string | null][]) => changes.sort(byKey);
    expect(mutations).toEqual([
      [[hex(p(0)), null]],
      [[hex(p(1)), null]],
      sorted([
        [hex(p(2)), null],
        [hex(producedOutRef(f1)), funded],
      ]),
      [],
      sorted([
        [hex(p(4)), null],
        [hex(producedOutRef(f3)), funded],
      ]),
      sorted([
        [hex(p(5)), null],
        [hex(producedOutRef(t1)), funded],
      ]),
      sorted([
        [hex(p(7)), null],
        [hex(producedOutRef(t1)), null],
        [hex(producedOutRef(t2)), fixtureOutput(20_000_000n).toString("hex")],
      ]),
      [[hex(outRefFromByte(0xd1)), fixtureOutput(5_000_000n).toString("hex")]],
      [[hex(outRefFromByte(0xd2)), fixtureOutput(6_000_000n).toString("hex")]],
    ]);
    expect(await entriesOf(exit.value)).toEqual(sortedEntries(block.ledger));
  }, 120_000);

  it("gives a transaction its spend and reference inputs and a forced transaction naming an absent input its rejection", async () => {
    const { block, exit, roots, forcedCodes } = await replay([
      // The same spend refused for its absent reference input.
      {
        phase: "ForcedTransaction",
        spend: [p(0)],
        references: [outRefFromByte(0xef)],
      },
      { phase: "ForcedTransaction", spend: [outRefFromByte(0xee)] },
      { phase: "L2Transaction", spend: [p(1)], references: [p(2)] },
    ]);
    expect(faultOf(exit)).toBeUndefined();
    expect(block.verdicts).toEqual(["rejected", "rejected", "accepted"]);
    expect(forcedCodes).toEqual(["E_INPUT_NOT_FOUND", "E_INPUT_NOT_FOUND"]);
    expect(roots).toEqual([
      PARENT_ROOT,
      PARENT_ROOT,
      "f0765a72ae97c5606ccfb30d89670936a2472bfb1afbd38d295b36cab981dba3",
    ]);
  }, 120_000);

  it.each([
    {
      label: "a withdrawal of an absent out-ref",
      event: { phase: "Withdrawal", outRef: outRefFromByte(0x99) },
      fault: "foreign user event spends missing ledger output",
    },
    {
      label: "a deposit over a present out-ref",
      event: { phase: "Deposit", outRef: p(0), output: fixtureOutput() },
      fault: "foreign user event substitutes ledger output",
    },
  ] as const)(
    "refuses $label as the block's fault",
    async ({ event, fault }) => {
      const { exit } = await replay([event]);
      expect(faultOf(exit)).toBe(fault);
    },
  );
});

/** Every failure and defect of `exit`, which must have failed. */
const causesOf = (exit: Exit.Exit<unknown, unknown>) => {
  if (Exit.isSuccess(exit)) throw new Error("the replay succeeded");
  return [...Cause.defects(exit.cause), ...Cause.failures(exit.cause)];
};

/** Replays one deposit at `outRef` whose L1 material inserts `output`,
 * over the parent ledger as `wrap` presents it. */
const replayDeposit = async (
  outRef: Buffer,
  output: Buffer,
  wrap: (ledger: Ledger) => Ledger = (ledger) => ledger,
) => {
  const block = await fixtureBlock(parent, [
    { phase: "Deposit", outRef, output: fixtureOutput() },
  ]);
  return await Effect.runPromiseExit(
    replayBlockEvents({
      header: block.payload.block_body.header,
      payload: block.payload,
      parent: wrap(await admitted(parent, PARENT_ROOT)),
      ...FIXTURE_REPLAY_SETTINGS,
      ...block.material,
      replayUserEvent: () => Effect.succeed([{ key: hex(outRef), output }]),
    }),
  );
};

describe("the block replay's ledger failures", () => {
  it("refuses an inserted output with no ledger leaf as it did before, never as a local fault", async () => {
    const exit = await replayDeposit(
      outRefFromByte(0xd3),
      Buffer.from("00", "hex"),
    );
    const causes = causesOf(exit);
    expect(causes.length).toBeGreaterThan(0);
    expect(causes.filter(isReplayLocalFault)).toEqual([]);
    if (Exit.isFailure(exit))
      expect(Cause.pretty(exit.cause)).toContain(
        "not a canonical ledger entry",
      );
  });

  it("names a failing ledger read a local fault", async () => {
    const exit = await replayDeposit(
      outRefFromByte(0xd3),
      fixtureOutput(),
      (ledger) => ({
        root: ledger.root,
        read: () => Promise.reject(new Error("storage unavailable")),
        apply: (mutations) => ledger.apply(mutations),
        entries: () => ledger.entries(),
      }),
    );
    expect(causesOf(exit).filter(isReplayLocalFault)).toHaveLength(1);
  });
});
