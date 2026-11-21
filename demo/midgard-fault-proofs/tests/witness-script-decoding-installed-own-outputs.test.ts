/**
 * The script-decode record through the watcher's installed application,
 * with other parties' outputs at the addresses its own outputs go to.
 *
 * The application finds its staged chunks by content: an exact copy (the
 * same inline datum, ada only, no datum hash, no reference script) serves
 * as well as its own, whoever placed it, and a copy in any other shape is
 * never taken. Outputs holding the same datums in other shapes and a mass of
 * outputs leave the record's transactions and their fees as they are with
 * nothing planted; an exact copy confirmed before the record is read in
 * place of a stage of its own; another party's honest run of the same
 * script, its run token minted by the record's own policy, is walked in
 * place of a Bind of its own. A stage transaction a stopped process signed
 * but never sent is built again after the restart, one copy landing, and one
 * it sent is found on chain and never built again; a read that lags the
 * chunk, or a rollback that includes the stage transaction again, is waited
 * out on the same out-ref; a chunk that is gone for good is served by an
 * exact copy on chain.
 */
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
}));
vi.mock("../src/workflow/deployment-manifest-binding.js", async (load) => ({
  ...(await load<
    typeof import("../src/workflow/deployment-manifest-binding.js")
  >()),
  bindFraudProofWorkflowDeployment: async () => hooks.binding,
}));
vi.mock("../src/workflow/family-l1-observation.js", async (load) => {
  const actual =
    await load<typeof import("../src/workflow/family-l1-observation.js")>();
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      actual.createFraudProofFamilyRawL1ObservationPort({
        ...input,
        authority: hooks.authority as Parameters<
          typeof actual.createFraudProofFamilyRawL1ObservationPort
        >[0]["authority"],
      }),
  };
});

import {
  emulatorWallClock,
  lagReadsOf,
  type LedgerVerdict,
  type LedgerWatch,
  plantCopies,
  plantCopyBefore,
  plantForeignRun,
  plantMass,
  plantShapes,
  plantTokenCoin,
  removeOutput,
  stagedChunkOf,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  costOf,
  expectOwnOutputsOnly,
  type Harness,
  ITEM,
  readersOf,
  rolesOf,
  stagesOf,
  withoutFirst,
} from "./support/own-outputs-installed.script-decode-record.js";
import {
  expectBaselineCost,
  INSTALLED_WITNESS,
  type InstalledTurnContext,
  runInstalled,
  stopsOnceAndRestarts,
} from "./support/script-decode-installed-run.js";
import { witnessSubject } from "./support/script-decode-plutus-max-shape.js";

const SCENARIO = {
  ...INSTALLED_WITNESS,
  nativeTx: () => witnessSubject([ITEM]).nativeTx,
};
/** Each run convicts well inside these, so one that spins fails fast. */
const POLL_CAP = 200;
const TURN_CAP = 20;
/**
 * Blocks every run takes before the record begins, planted outputs or
 * none, so each run's transactions are built at the same chain time.
 */
const SETUP_BLOCKS = 24;
/** How long reads lag the chain in the lagging row: three blocks. */
const LAG_MS = 60_000;

/** Whether the transaction that creates `outRef` is in our node's mempool. */
const inMempool = (harness: Harness, outRef: string): boolean =>
  Object.keys(harness.emulator.mempool).some((key) =>
    key.startsWith(outRef.slice(0, 64)),
  );
/** Blocks past the provider grace (10 minutes of 20-second blocks). */
const PAST_GRACE_BLOCKS = 31;
/** Datum-bearing outputs of the mass plant. */
const MASS = 300;

/**
 * Runs each planting step, each in blocks of its own, then waits out the
 * rest of the setup blocks.
 */
const setUp = async (
  harness: Harness,
  steps: readonly (() => Promise<unknown>)[] = [],
) => {
  const from = harness.emulator.blockHeight;
  for (const step of steps) {
    await step();
    harness.emulator.awaitBlock();
  }
  const used = harness.emulator.blockHeight - from;
  if (used > SETUP_BLOCKS) throw new Error("planting took too many blocks");
  harness.emulator.awaitBlock(SETUP_BLOCKS - used);
};

/** A run of the scenario with `plant` before the record begins, its ledger watched. */
const runWith = async (
  plant: (
    harness: Harness,
    watch: LedgerWatch,
  ) => readonly (() => Promise<unknown>)[],
  options: Omit<Parameters<typeof runInstalled>[2] & object, "beforeTurns"> & {
    readonly decide?: Parameters<typeof watchLedger>[1];
    readonly during?: (
      context: Omit<InstalledTurnContext, "result" | "turn">,
      watch: LedgerWatch,
    ) => (() => void) | void;
  } = {},
) => {
  let watch: LedgerWatch | undefined;
  let harness: Harness | undefined;
  const { decide, during, ...runOptions } = options;
  const run = await runInstalled(SCENARIO, hooks, {
    maxPolls: POLL_CAP,
    maxTurns: TURN_CAP,
    ...runOptions,
    beforeTurns: async (context) => {
      harness = context.harness;
      const clock = emulatorWallClock();
      watch = watchLedger(context.harness.emulator, decide);
      await setUp(context.harness, plant(context.harness, watch));
      const stop = during?.(context, watch);
      const ledger = watch;
      return () => {
        stop?.();
        ledger.restore();
        clock.restore();
      };
    },
  });
  return {
    run,
    watch: watch!,
    harness: harness!,
    cost: costOf(watch!, harness!),
  };
};

describe("script-decode staging found by content, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];

  it("records with nothing else at its addresses: the cost every other row matches", async () => {
    const { run, watch, harness, cost } = await runWith(() => []);
    expect(run.convicted).toBe(true);
    expect(stagesOf(watch, harness)).toHaveLength(1);
    baseline = cost;
  }, 900_000);

  it("finishes the record past other-shaped copies and a mass of outputs, walking another party's run of the script in place of a Bind of its own", async () => {
    let foreign: string | undefined;
    const { run, watch, harness, cost } = await runWith((harness, watch) => {
      const { staging, script } = rolesOf(harness);
      const other = harness.funderLucid;
      return [
        () => plantShapes(watch, other, staging),
        () =>
          plantMass(watch, other, staging, {
            emulator: harness.emulator,
            count: MASS,
          }),
        async () => {
          const bound = await plantForeignRun(watch, other, {
            emulator: harness.emulator,
            script,
          });
          foreign = `${bound.txHash}#${bound.outputIndex.toString()}`;
        },
      ];
    });
    expect(run.convicted).toBe(true);
    const adopted = expectOwnOutputsOnly(watch, harness);
    expect(adopted.map(({ outRef, as }) => ({ outRef, as }))).toEqual([
      { outRef: foreign, as: "input" },
    ]);
    expect(stagesOf(watch, harness)).toHaveLength(1);
    expectBaselineCost(cost, withoutFirst(baseline, "bind-or-finish"));
  }, 900_000);

  it("funds the record from plain coins, leaving a larger token-bearing coin at the prover address unspent", async () => {
    const { run, watch, harness, cost } = await runWith((harness, watch) => [
      () =>
        plantTokenCoin(
          watch,
          harness.funderLucid,
          harness.proverSigner.address,
        ),
    ]);
    expect(run.convicted).toBe(true);
    expect(expectOwnOutputsOnly(watch, harness)).toEqual([]);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("reads an exact copy of its chunk confirmed before the record began, staging none of its own", async () => {
    const { run, watch, harness, cost } = await runWith((harness, watch) => [
      () => plantCopies(watch, harness.funderLucid, rolesOf(harness).staging),
    ]);
    expect(run.convicted).toBe(true);
    const adopted = expectOwnOutputsOnly(watch, harness);
    expect(stagesOf(watch, harness)).toHaveLength(0);
    const copies = new Set(adopted.map(({ outRef }) => outRef));
    expect(adopted.every(({ as }) => as === "reference")).toBe(true);
    const readers = readersOf(watch, harness);
    expect(readers.length).toBeGreaterThan(0);
    for (const reader of readers)
      expect(reader.referenceInputs.some((outRef) => copies.has(outRef))).toBe(
        true,
      );
    expectBaselineCost(cost, withoutFirst(baseline, "stage"));
  }, 900_000);

  it("stages its chunk and reads it, or an exact copy that lands in the same block and sorts before it", async () => {
    let copied = false;
    const context: { watch?: LedgerWatch; harness?: Harness } = {};
    const { run, watch, harness, cost } = await runWith(
      (harness, watch) => {
        context.watch = watch;
        context.harness = harness;
        return [];
      },
      {
        decide: async (transaction): Promise<LedgerVerdict> => {
          const { staging } = rolesOf(context.harness!);
          const chunk = stagedChunkOf(transaction, staging);
          if (chunk === undefined || copied) return "accept";
          copied = true;
          await plantCopyBefore(context.watch!, context.harness!.funderLucid, {
            address: staging.address,
            datum: staging.datums[0]!,
            below: transaction.txHash,
          });
          return "accept";
        },
      },
    );
    expect(copied).toBe(true);
    expect(run.convicted).toBe(true);
    const copies = new Set(
      expectOwnOutputsOnly(watch, harness).map(({ outRef }) => outRef),
    );
    const stages = stagesOf(watch, harness);
    expect(stages).toHaveLength(1);
    for (const reader of readersOf(watch, harness))
      expect(
        reader.referenceInputs.some(
          (outRef) => outRef === stages[0]!.chunk || copies.has(outRef),
        ),
      ).toBe(true);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("stages the chunk again after a restart loses the stage transaction its stopped process signed but never sent, landing one copy", async () => {
    let withheld: string | undefined;
    const stopping = stopsOnceAndRestarts(POLL_CAP);
    const { run, watch, harness, cost } = await runWith(() => [], {
      decide: (transaction) => {
        const { staging } = rolesOf(stopping.harness());
        if (
          withheld !== undefined ||
          stagedChunkOf(transaction, staging) === undefined
        )
          return "accept";
        // Signed, and lost before the ledger saw it; the process stops at
        // its next poll, and the next one holds nothing of it.
        withheld = transaction.txHash;
        stopping.stopAtNextPoll();
        return "withhold";
      },
      during: stopping.during,
      onStopped: stopping.onStopped,
    });
    expect(stopping.stops()).toBe(1);
    expect(run.convicted).toBe(true);
    expect(withheld).toBeDefined();
    const stages = stagesOf(watch, harness);
    expect(stages).toHaveLength(1);
    expect(readersOf(watch, harness)[0]!.referenceInputs).toContain(
      stages[0]!.chunk,
    );
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("finds on chain the chunk its stopped process's sent stage transaction staged, staging it once", async () => {
    let sent: string | undefined;
    const stopping = stopsOnceAndRestarts(POLL_CAP);
    const { run, watch, harness, cost } = await runWith(() => [], {
      decide: (transaction) => {
        const { staging } = rolesOf(stopping.harness());
        if (
          sent === undefined &&
          stagedChunkOf(transaction, staging) !== undefined
        ) {
          // The ledger takes it; the process stops at its next poll.
          sent = transaction.txHash;
          stopping.stopAtNextPoll();
        }
        return "accept";
      },
      during: stopping.during,
      onStopped: stopping.onStopped,
    });
    expect(stopping.stops()).toBe(1);
    expect(run.convicted).toBe(true);
    const stages = stagesOf(watch, harness);
    expect(stages.map(({ transaction }) => transaction.txHash)).toEqual([sent]);
    for (const reader of readersOf(watch, harness))
      expect(reader.referenceInputs).toContain(stages[0]!.chunk);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("waits out reads that do not yet show its confirmed chunk, then continues from the same out-ref", async () => {
    let hidden: string | undefined;
    let shownFrom = 0;
    const context: { harness?: Harness; reads?: () => number } = {};
    const { run, watch, harness, cost } = await runWith(() => [], {
      decide: (transaction) => {
        const chunk = stagedChunkOf(
          transaction,
          rolesOf(context.harness!).staging,
        );
        if (chunk !== undefined && hidden === undefined) {
          // Once confirmed, reads show the chunk only a minute after it was
          // sent: well inside the provider grace. In the mempool, our node
          // shows it.
          hidden = chunk;
          shownFrom = context.harness!.emulator.now() + LAG_MS;
        }
        return "accept";
      },
      during: ({ harness }) => {
        context.harness = harness;
        const lag = lagReadsOf(harness.emulator, () =>
          hidden !== undefined &&
          harness.emulator.now() < shownFrom &&
          !inMempool(harness, hidden)
            ? hidden
            : undefined,
        );
        context.reads = lag.hiddenReads;
        return lag.restore;
      },
    });
    expect(context.reads!()).toBeGreaterThan(0);
    expect(run.convicted).toBe(true);
    const stages = stagesOf(watch, harness);
    expect(stages.map(({ chunk }) => chunk)).toEqual([hidden]);
    for (const reader of readersOf(watch, harness))
      expect(reader.referenceInputs).toContain(hidden);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("continues from the same out-ref when its chunk drops out of reads after the Bind and is included again", async () => {
    let hidden: string | undefined;
    let shownFrom = 0;
    const context: { reads?: () => number } = {};
    const { run, watch, harness, cost } = await runWith(() => [], {
      during: ({ harness, clock }, watch) => {
        clock.state.onPoll = async () => {
          if (hidden !== undefined) return;
          const [first] = stagesOf(watch, harness);
          const bind = readersOf(watch, harness)[0];
          if (first === undefined || bind === undefined) return;
          const status = await harness.emulator.getTransactionStatus(
            bind.txHash,
          );
          if (status.status !== "confirmed") return;
          // Rolled back once the run is bound, and the same stage
          // transaction included again a minute later: well inside the
          // provider grace.
          hidden = first.chunk;
          shownFrom = harness.emulator.now() + LAG_MS;
        };
        const lag = lagReadsOf(harness.emulator, () =>
          harness.emulator.now() < shownFrom ? hidden : undefined,
        );
        context.reads = lag.hiddenReads;
        return lag.restore;
      },
    });
    expect(hidden).toBeDefined();
    expect(context.reads!()).toBeGreaterThan(0);
    expect(run.convicted).toBe(true);
    const stages = stagesOf(watch, harness);
    expect(stages.map(({ chunk }) => chunk)).toEqual([hidden]);
    for (const reader of readersOf(watch, harness))
      expect(reader.referenceInputs).toContain(hidden);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("reads an exact copy of its chunk once its own is gone for good, staging none again", async () => {
    let removed: string | undefined;
    const { run, watch, harness } = await runWith(() => [], {
      during: ({ harness, clock }, watch) => {
        clock.state.onPoll = async () => {
          if (removed !== undefined) return;
          const [first] = stagesOf(watch, harness);
          const bind = readersOf(watch, harness)[0];
          if (first === undefined || bind === undefined) return;
          // A block takes the Bind if it is still in the mempool.
          if (inMempool(harness, `${bind.txHash}#0`))
            harness.emulator.awaitBlock();
          const status = await harness.emulator.getTransactionStatus(
            bind.txHash,
          );
          if (status.status !== "confirmed") return;
          const [txHash, index] = first.chunk.split("#");
          const utxo = harness.emulator.ledger[`${txHash!}${index!}`]?.utxo;
          if (utxo === undefined) return;
          removed = first.chunk;
          // The stage transaction is rolled back for good once the run is
          // bound, and an exact copy of its chunk is on chain instead.
          await removeOutput(
            watch,
            harness.proverLucid,
            utxo,
            await harness.funderLucid.wallet().address(),
          );
          harness.emulator.awaitBlock();
          await plantCopies(
            watch,
            harness.funderLucid,
            rolesOf(harness).staging,
          );
          harness.emulator.awaitBlock(PAST_GRACE_BLOCKS);
        };
      },
    });
    expect(removed).toBeDefined();
    expect(run.convicted).toBe(true);
    const copies = new Set(
      expectOwnOutputsOnly(watch, harness).map(({ outRef }) => outRef),
    );
    expect(copies.size).toBeGreaterThan(0);
    expect(stagesOf(watch, harness).map(({ chunk }) => chunk)).toEqual([
      removed,
    ]);
    // The Bind read its own chunk; every Walk reads it or the copy.
    const readers = readersOf(watch, harness);
    expect(readers[0]!.referenceInputs).toContain(removed);
    for (const reader of readers)
      expect(
        reader.referenceInputs.some(
          (outRef) => outRef === removed || copies.has(outRef),
        ),
      ).toBe(true);
    expect(
      readers.some((reader) =>
        reader.referenceInputs.some((outRef) => copies.has(outRef)),
      ),
    ).toBe(true);
  }, 900_000);
});
