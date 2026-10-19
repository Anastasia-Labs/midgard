/**
 * The script-decode record through the watcher's installed application,
 * with other parties' outputs at the addresses its own outputs go to.
 *
 * The application finds its staged chunks by the out-refs its own signed
 * stage transactions gave them, never by what an output at the address
 * holds. Outputs holding the same datums in other shapes, exact copies
 * confirmed before the record or landing beside the application's own, a
 * mass of outputs, and another party's run of the same script leave the
 * record's transactions and their fees as they are with nothing planted.
 * A stopped process sends the stage transaction it signed; a read that lags
 * the chunk is waited out on the same out-ref; a chunk that is gone for good
 * is staged again, replacing it, and never served by a copy.
 */
import { describe, expect, it, vi } from "vitest";

const hooks = vi.hoisted(() => ({
  binding: undefined as unknown,
  authority: undefined as unknown,
  recovery: undefined as unknown,
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
  const { withFraudProofL1Recovery } = await import(
    "../src/workflow/l1-source.js"
  );
  return {
    ...actual,
    createFraudProofFamilyL1ObservationPort: (
      input: Parameters<
        typeof actual.createFraudProofFamilyL1ObservationPort
      >[0],
    ) =>
      withFraudProofL1Recovery(
        actual.createFraudProofFamilyRawL1ObservationPort({
          ...input,
          authority: hooks.authority as Parameters<
            typeof actual.createFraudProofFamilyRawL1ObservationPort
          >[0]["authority"],
        }),
        hooks.recovery as Parameters<typeof withFraudProofL1Recovery>[1],
      ),
  };
});

import {
  emulatorWallClock,
  kindOf,
  lagReadsOf,
  type LedgerVerdict,
  type LedgerWatch,
  plantCopies,
  plantCopyBefore,
  plantForeignRun,
  plantMass,
  plantShapes,
  removeOutput,
  scriptDecodeRoles,
  stagedChunkOf,
  watchLedger,
} from "./support/own-outputs-installed.js";
import {
  INSTALLED_WITNESS,
  type InstalledTurnContext,
  runInstalled,
} from "./support/script-decode-installed-run.js";
import {
  delayChainScript,
  plutusItem,
  witnessSubject,
} from "./support/script-decode-plutus-max-shape.js";

/** An undecodable witness whose record takes one staged chunk, a Bind, a few Walks, a Finish. */
const ITEM = plutusItem(delayChainScript(600));
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
/** Blocks past the provider grace (10 minutes of 20-second blocks). */
const PAST_GRACE_BLOCKS = 31;
/** Datum-bearing outputs of the mass plant. */
const MASS = 300;

type Harness = InstalledTurnContext["harness"];

const rolesOf = (harness: Harness) =>
  scriptDecodeRoles({
    emulator: harness.emulator,
    proverAddress: harness.proverSigner.address,
    item: ITEM,
  });

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

/**
 * How far one transaction's fee may move between two runs that build the
 * same transactions. Each run's chain has its own keys, so the out-ref order
 * of a transaction's reference inputs, which its redeemers index, differs
 * from run to run, and the fee by a few hundred lovelace with it.
 */
const FEE_SPREAD = 2_000n;

/** What a run's own transactions were and what they cost. */
const costOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging, run } = rolesOf(harness);
  return watch.ours().map((transaction) => ({
    kind: kindOf(transaction, staging, run),
    fee: transaction.fee,
  }));
};

/** The same transactions as the baseline, in the same order, at its fees. */
const expectBaselineCost = (
  cost: ReturnType<typeof costOf>,
  baseline: ReturnType<typeof costOf>,
) => {
  expect(cost.map(({ kind }) => kind)).toEqual(
    baseline.map(({ kind }) => kind),
  );
  cost.forEach(({ fee }, index) => {
    const spread = fee - baseline[index]!.fee;
    expect(spread < 0n ? -spread : spread).toBeLessThanOrEqual(FEE_SPREAD);
  });
};

/** Every input and reference input of the application's own transactions is not another party's. */
const expectOwnOutputsOnly = (watch: LedgerWatch) => {
  const planted = watch.plantedOutRefs();
  expect(planted.size).toBeGreaterThan(0);
  const taken = watch
    .ours()
    .flatMap(({ inputs, referenceInputs }) => [...inputs, ...referenceInputs])
    .filter((outRef) => planted.has(outRef));
  expect(taken).toEqual([]);
};

/** The application's stage transactions, and the chunk each one staged. */
const stagesOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging } = rolesOf(harness);
  return watch.ours().flatMap((transaction) => {
    const chunk = stagedChunkOf(transaction, staging);
    return chunk === undefined ? [] : [{ transaction, chunk }];
  });
};

/** The transactions that read the run's staged bytes: the Bind, then each Walk. */
const readersOf = (watch: LedgerWatch, harness: Harness) => {
  const { staging, run } = rolesOf(harness);
  const kinds = watch.ours().map((transaction) => ({
    transaction,
    kind: kindOf(transaction, staging, run),
  }));
  const bind = kinds.find(({ kind }) => kind === "bind-or-finish");
  return [
    ...(bind === undefined ? [] : [bind.transaction]),
    ...kinds
      .filter(({ kind }) => kind === "walk")
      .map(({ transaction }) => transaction),
  ];
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

describe("script-decode staging found by the out-refs it published, in the installed watcher application", () => {
  let baseline: Awaited<ReturnType<typeof runWith>>["cost"];

  it("records with nothing else at its addresses: the cost every other row matches", async () => {
    const { run, watch, harness, cost } = await runWith(() => []);
    expect(run.convicted).toBe(true);
    expect(stagesOf(watch, harness)).toHaveLength(1);
    baseline = cost;
  }, 900_000);

  it("finishes the record from its own outputs past other-shaped copies, a mass of outputs and another party's run", async () => {
    const { run, watch, cost } = await runWith((harness, watch) => {
      const { staging, script } = rolesOf(harness);
      const other = harness.funderLucid;
      return [
        () => plantShapes(watch, other, staging),
        () =>
          plantMass(watch, other, staging, {
            emulator: harness.emulator,
            count: MASS,
          }),
        () =>
          plantForeignRun(watch, other, { emulator: harness.emulator, script }),
      ];
    });
    expect(run.convicted).toBe(true);
    expectOwnOutputsOnly(watch);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("stages and reads its own chunk when exact copies of it were confirmed before the record began", async () => {
    const { run, watch, harness, cost } = await runWith((harness, watch) => [
      () => plantCopies(watch, harness.funderLucid, rolesOf(harness).staging),
    ]);
    expect(run.convicted).toBe(true);
    expectOwnOutputsOnly(watch);
    expect(stagesOf(watch, harness)).toHaveLength(1);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("reads its own chunk, not an exact copy that lands in the same block and sorts before it", async () => {
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
    expectOwnOutputsOnly(watch);
    expect(stagesOf(watch, harness)).toHaveLength(1);
    expectBaselineCost(cost, baseline);
  }, 900_000);

  it("sends the stage transaction its stopped process signed but never sent, and reads the chunk it staged", async () => {
    let withheld: string | undefined;
    let stops = 0;
    const context: {
      harness?: Harness;
      clock?: InstalledTurnContext["clock"];
    } = {};
    const { run, watch, harness, cost } = await runWith(() => [], {
      decide: (transaction) => {
        const { staging } = rolesOf(context.harness!);
        if (
          withheld !== undefined ||
          stagedChunkOf(transaction, staging) === undefined
        )
          return "accept";
        // Signed and journaled, lost before the ledger saw it; the process
        // stops at its next poll.
        withheld = transaction.txHash;
        context.clock!.state.crashAfterPolls = context.clock!.state.polls;
        return "withhold";
      },
      during: ({ harness, clock }) => {
        context.harness = harness;
        context.clock = clock;
      },
      onStopped: async (error, { application, clock }) => {
        expect(String(error)).toContain(
          "installed application process stopped",
        );
        stops += 1;
        await application.restart();
        clock.state.crashAfterPolls = clock.state.polls + POLL_CAP;
      },
    });
    expect(stops).toBe(1);
    expect(run.convicted).toBe(true);
    const stages = stagesOf(watch, harness);
    expect(stages.map(({ transaction }) => transaction.txHash)).toEqual([
      withheld,
    ]);
    expect(readersOf(watch, harness)[0]!.referenceInputs).toContain(
      stages[0]!.chunk,
    );
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
          // Reads show the chunk only a minute after it was sent: well
          // inside the provider grace.
          hidden = chunk;
          shownFrom = context.harness!.emulator.now() + LAG_MS;
        }
        return "accept";
      },
      during: ({ harness }) => {
        context.harness = harness;
        const lag = lagReadsOf(harness.emulator, () =>
          harness.emulator.now() < shownFrom ? hidden : undefined,
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

  it("stages the chunk again, replacing its own, once that one is gone for good, and never reads an exact copy for it", async () => {
    let removed: string | undefined;
    const { run, watch, harness } = await runWith(() => [], {
      during: ({ harness, clock }, watch) => {
        clock.state.onPoll = async () => {
          if (removed !== undefined) return;
          const [first] = stagesOf(watch, harness);
          const bind = readersOf(watch, harness)[0];
          if (first === undefined || bind === undefined) return;
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
    expectOwnOutputsOnly(watch);
    const stages = stagesOf(watch, harness);
    expect(stages.map(({ chunk }) => chunk)[0]).toBe(removed);
    expect(stages).toHaveLength(2);
    // Every Walk after the second stage reads the chunk it staged.
    const second = watch.ours().indexOf(stages[1]!.transaction);
    const later = readersOf(watch, harness)
      .slice(1)
      .filter((walk) => watch.ours().indexOf(walk) > second);
    expect(later.length).toBeGreaterThan(0);
    for (const reader of later)
      expect(reader.referenceInputs).toContain(stages[1]!.chunk);
  }, 900_000);
});
