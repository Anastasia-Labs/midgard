/**
 * The ledger store's exit measurements over one durable adapter, run as three
 * processes so each reports its own peak memory:
 *
 * - `build`: admits `entries` ledger entries, then applies and persists
 *   `events` events (each deletes two held entries and inserts two new ones),
 *   each on a ledger read back from the store by its root. It reports the
 *   edit (`apply`) and `persist` times, the bytes each event writes, and the
 *   database's growth on disk.
 * - `window`: `WINDOW_EVENTS` further events, each applied and persisted,
 *   then a `retain` of the newest `events` roots, which drops the oldest
 *   retained root (the first also drops the snapshot's). It reports the
 *   `persist` and `retain` times.
 * - `drop`: one `retain` of the newest root alone, dropping every other. It
 *   reports its time and peak memory.
 *
 * Each phase prints one JSON line and writes it to `<directory>/<phase>.json`.
 *
 * `exitBenchInputFromEnv` reads the run from `LEDGER_STORE_EXIT_PHASE`
 * (required), `LEDGER_STORE_EXIT_DIR` (required, the same directory for all
 * three phases), `LEDGER_STORE_EXIT_ENTRIES` (default 200000) and
 * `LEDGER_STORE_EXIT_EVENTS` (default 36720).
 */
import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { ledgerEntriesRoot, type LedgerEntry } from "../../src/ledger-at.js";
import {
  createLedgerStore,
  type LedgerMutation,
  type LedgerStorage,
} from "../../src/ledger-store/index.js";
import {
  testLedgerEntries,
  testLedgerEntry,
} from "./ledger-store-conformance.js";

export type ExitBenchStorage = {
  readonly storage: LedgerStorage;
  /** The database's size on disk, after any pending write reached it. */
  readonly bytesOnDisk: () => Promise<number>;
  readonly close: () => Promise<void>;
};

export type ExitBenchPhase = "build" | "window" | "drop";

const PHASES: readonly ExitBenchPhase[] = ["build", "window", "drop"];

/** Events the `window` phase adds, at most. */
const WINDOW_EVENTS = 1_000;

const sizeFromEnv = (name: string, fallback: number): number => {
  const raw = process.env[name];
  if (raw === undefined) return fallback;
  const parsed = Number(raw);
  if (!Number.isSafeInteger(parsed) || parsed < 1)
    throw new Error(`${name} must be a positive integer, got ${raw}`);
  return parsed;
};

/** The phase, directory and sizes of this run, from the environment. */
export const exitBenchInputFromEnv = () => {
  const phase = process.env.LEDGER_STORE_EXIT_PHASE;
  const directory = process.env.LEDGER_STORE_EXIT_DIR;
  if (!PHASES.includes(phase as ExitBenchPhase))
    throw new Error(
      `LEDGER_STORE_EXIT_PHASE must be one of ${PHASES.join(", ")}, got ${phase}`,
    );
  if (directory === undefined || directory === "")
    throw new Error("LEDGER_STORE_EXIT_DIR must name the run's directory");
  return {
    phase: phase as ExitBenchPhase,
    directory,
    entries: sizeFromEnv("LEDGER_STORE_EXIT_ENTRIES", 200_000),
    events: sizeFromEnv("LEDGER_STORE_EXIT_EVENTS", 36_720),
  };
};

type State = {
  /** The retained roots, oldest first. */
  roots: string[];
  live: number[];
  next: number;
};

const percentile = (sorted: readonly number[], fraction: number) =>
  sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * fraction))]!;

const summary = (samples: readonly number[]) => {
  const sorted = [...samples].sort((a, b) => a - b);
  const total = sorted.reduce((sum, value) => sum + value, 0);
  return {
    median: percentile(sorted, 0.5),
    p95: percentile(sorted, 0.95),
    mean: total / Math.max(1, sorted.length),
    max: sorted.at(-1) ?? 0,
  };
};

const mib = (bytes: number) => Math.round((bytes / 2 ** 20) * 10) / 10;

/** Peak RSS and heap, sampled while `run` runs. */
export const sampled = async <T>(run: () => Promise<T>) => {
  const baseline = process.memoryUsage();
  let rss = baseline.rss;
  let heap = baseline.heapUsed;
  const sample = () => {
    const usage = process.memoryUsage();
    rss = Math.max(rss, usage.rss);
    heap = Math.max(heap, usage.heapUsed);
  };
  const timer = setInterval(sample, 20);
  const started = performance.now();
  try {
    const result = await run();
    sample();
    return {
      result,
      milliseconds: performance.now() - started,
      baselineRssMiB: mib(baseline.rss),
      baselineHeapMiB: mib(baseline.heapUsed),
      peakRssMiB: mib(rss),
      peakHeapMiB: mib(heap),
    };
  } finally {
    clearInterval(timer);
  }
};

/** Deterministic picks: a linear congruential sequence. */
const picker = (seed: number) => {
  let state = seed >>> 0;
  return (bound: number) => {
    state = (Math.imul(state, 1_664_525) + 1_013_904_223) >>> 0;
    return state % bound;
  };
};

/** The next event over `live`: two held entries out, two new ones in. */
const nextEvent = (
  live: number[],
  next: number,
  pick: (bound: number) => number,
): { mutations: LedgerMutation[]; next: number } => {
  const deleted: number[] = [];
  for (let taken = 0; taken < 2; taken += 1) {
    const index = pick(live.length);
    deleted.push(live[index]!);
    live[index] = live[live.length - 1]!;
    live.pop();
  }
  const inserted = [next, next + 1];
  live.push(...inserted);
  return {
    mutations: [
      ...deleted.map((n) => ({
        type: "delete" as const,
        outRef: testLedgerEntry(n).outRef,
      })),
      ...inserted.map((n) => ({
        type: "insert" as const,
        ...testLedgerEntry(n),
      })),
    ],
    next: next + 2,
  };
};

/**
 * Counts what each `write` hands the adapter: new node bytes (the hash key
 * and the node JSON) and new output bytes (out-ref and output), apart, since
 * an adapter that keeps no outputs writes only the first; and the reference
 * counts it updates on held records.
 */
const counting = (storage: LedgerStorage) => {
  const nodeBytes: number[] = [];
  const outputBytes: number[] = [];
  const refUpdates: number[] = [];
  return {
    nodeBytes,
    outputBytes,
    refUpdates,
    storage: {
      ...storage,
      write: async (change) => {
        let nodes = 0;
        for (const { node } of change.putNodes.values())
          nodes += 32 + Buffer.byteLength(JSON.stringify(node));
        let outputs = 0;
        for (const [outRef, { output }] of change.putOutputs)
          outputs += outRef.length / 2 + output.length;
        nodeBytes.push(nodes);
        outputBytes.push(outputs);
        refUpdates.push(change.nodeRefs.size + change.outputRefs.size);
        await storage.write(change);
      },
    } satisfies LedgerStorage,
  };
};

/** Applies the next event to the newest retained root and persists it;
 * answers the edit and persist times. */
const buildEvent = async (
  store: ReturnType<typeof createLedgerStore>,
  state: { roots: string[]; live: number[]; next: number },
  pick: (bound: number) => number,
) => {
  const ledger = await store.at(state.roots.at(-1)!);
  const step = nextEvent(state.live, state.next, pick);
  state.next = step.next;
  const editStarted = performance.now();
  const edited = await ledger.apply(step.mutations);
  const edit = performance.now() - editStarted;
  const persistStarted = performance.now();
  await store.persist(edited);
  const persist = performance.now() - persistStarted;
  state.roots.push(edited.root);
  return { edit, persist };
};

export const runLedgerStoreExitBench = async (input: {
  readonly adapter: string;
  readonly phase: ExitBenchPhase;
  readonly directory: string;
  readonly entries: number;
  readonly events: number;
  readonly open: () => Promise<ExitBenchStorage>;
}): Promise<Record<string, unknown>> => {
  const statePath = join(input.directory, "state.json");
  await mkdir(input.directory, { recursive: true });
  const opened = await input.open();
  let report: Record<string, unknown>;
  try {
    if (input.phase === "build") {
      const diskBefore = await opened.bytesOnDisk();
      const counted = counting(opened.storage);
      const store = createLedgerStore(counted.storage);
      const base: LedgerEntry[] = testLedgerEntries(0, input.entries);
      const admitted = await sampled(async () => {
        const ledger = await store.admit(base, await ledgerEntriesRoot(base));
        return ledger.root;
      });
      // `admit` writes in batches: every write so far is the snapshot's.
      const sum = (values: number[]) =>
        values.splice(0).reduce((total, value) => total + value, 0);
      const snapshotNodeBytes = sum(counted.nodeBytes);
      const snapshotOutputBytes = sum(counted.outputBytes);
      counted.refUpdates.splice(0);
      const diskAfterSnapshot = await opened.bytesOnDisk();
      const pick = picker(0x1ed9e7);
      const state = {
        roots: [admitted.result],
        live: base.map((_, index) => index),
        next: input.entries,
      };
      const edits: number[] = [];
      const persists: number[] = [];
      for (let event = 0; event < input.events; event += 1) {
        const { edit, persist } = await buildEvent(store, state, pick);
        edits.push(edit);
        persists.push(persist);
      }
      const diskAfter = await opened.bytesOnDisk();
      await writeFile(statePath, JSON.stringify(state));
      report = {
        snapshot: {
          entries: input.entries,
          milliseconds: Math.round(admitted.milliseconds),
          peakRssMiB: admitted.peakRssMiB,
          nodeBytesWritten: snapshotNodeBytes,
          outputBytesWritten: snapshotOutputBytes,
          diskBytes: diskAfterSnapshot - diskBefore,
        },
        events: input.events,
        editMilliseconds: summary(edits),
        persistMilliseconds: summary(persists),
        nodeBytesWrittenPerEvent: summary(counted.nodeBytes),
        outputBytesWrittenPerEvent: summary(counted.outputBytes),
        refUpdatesPerEvent: summary(counted.refUpdates),
        diskBytesPerEvent: (diskAfter - diskAfterSnapshot) / input.events,
        counts: await opened.storage.counts(),
      };
    } else if (input.phase === "window") {
      const state = JSON.parse(await readFile(statePath, "utf8")) as State;
      const store = createLedgerStore(opened.storage);
      const diskBefore = await opened.bytesOnDisk();
      const width = state.roots.length - 1;
      const pick = picker(0x57ead);
      const edits: number[] = [];
      const persists: number[] = [];
      const retains: number[] = [];
      const run = await sampled(async () => {
        for (
          let event = 0;
          event < Math.min(WINDOW_EVENTS, input.events);
          event += 1
        ) {
          const { edit, persist } = await buildEvent(store, state, pick);
          edits.push(edit);
          persists.push(persist);
          state.roots.splice(0, state.roots.length - width);
          const window = new Set(state.roots);
          const retainStarted = performance.now();
          const { notRetained } = await store.retain(window);
          retains.push(performance.now() - retainStarted);
          if (notRetained.size > 0)
            throw new Error(`window lost ${notRetained.size} roots`);
        }
      });
      await writeFile(statePath, JSON.stringify(state));
      report = {
        events: edits.length,
        retainedRoots: width,
        editMilliseconds: summary(edits),
        persistMilliseconds: summary(persists),
        retainMilliseconds: summary(retains),
        peakRssMiB: run.peakRssMiB,
        diskBytesPerEvent:
          ((await opened.bytesOnDisk()) - diskBefore) / edits.length,
        counts: await opened.storage.counts(),
      };
    } else {
      const state = JSON.parse(await readFile(statePath, "utf8")) as State;
      const store = createLedgerStore(opened.storage);
      const before = await opened.storage.counts();
      const newest = state.roots.at(-1)!;
      const dropped = await sampled(() => store.retain(new Set([newest])));
      if (dropped.result.notRetained.size > 0)
        throw new Error("the drop lost the newest root");
      await writeFile(statePath, JSON.stringify({ ...state, roots: [newest] }));
      report = {
        droppedRoots: state.roots.length - 1,
        milliseconds: Math.round(dropped.milliseconds),
        baselineRssMiB: dropped.baselineRssMiB,
        baselineHeapMiB: dropped.baselineHeapMiB,
        peakRssMiB: dropped.peakRssMiB,
        peakHeapMiB: dropped.peakHeapMiB,
        countsBefore: before,
        countsAfter: await opened.storage.counts(),
        diskBytes: await opened.bytesOnDisk(),
      };
    }
  } finally {
    await opened.close();
  }
  report = { adapter: input.adapter, phase: input.phase, ...report };
  const line = JSON.stringify(report);
  console.log(line);
  await writeFile(join(input.directory, `${input.phase}.json`), line);
  return report;
};
