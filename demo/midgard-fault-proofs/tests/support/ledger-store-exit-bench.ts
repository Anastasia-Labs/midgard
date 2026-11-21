/**
 * The ledger store's exit measurements over one durable adapter, run as three
 * processes so each reports its own peak memory:
 *
 * - `build`: admits `entries` ledger entries, then applies and persists
 *   `events` events (each deletes two held entries and inserts two new ones),
 *   each on a ledger read back from the store by its root. It reports the
 *   edit (`apply`) and `persist` times, the bytes each event writes, and the
 *   database's growth on disk.
 * - `sweep`: the first full sweep, retaining every event root (all but the
 *   snapshot's).
 * - `steady`: one more block persisted, the oldest retained root dropped, and
 *   the sweep that follows.
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

export type ExitBenchPhase = "build" | "sweep" | "steady";

const PHASES: readonly ExitBenchPhase[] = ["build", "sweep", "steady"];

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
  readonly roots: string[];
  readonly live: number[];
  readonly next: number;
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
const sampled = async <T>(run: () => Promise<T>) => {
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
 * Counts what each `writeBlock` hands the adapter: node bytes (the hash key and
 * the node JSON) and output bytes (out-ref and output), apart, since an
 * adapter that keeps no outputs writes only the first.
 */
const counting = (storage: LedgerStorage) => {
  const nodeBytes: number[] = [];
  const outputBytes: number[] = [];
  return {
    nodeBytes,
    outputBytes,
    storage: {
      ...storage,
      writeBlock: async (block) => {
        let nodes = 0;
        for (const node of block.nodes.values())
          nodes += 32 + Buffer.byteLength(JSON.stringify(node));
        let outputs = 0;
        for (const [outRef, output] of block.outputs)
          outputs += outRef.length / 2 + output.length;
        nodeBytes.push(nodes);
        outputBytes.push(outputs);
        await storage.writeBlock(block);
      },
    } satisfies LedgerStorage,
  };
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
        await store.persist(ledger);
        return ledger.root;
      });
      const snapshotNodeBytes = counted.nodeBytes.pop()!;
      const snapshotOutputBytes = counted.outputBytes.pop()!;
      const diskAfterSnapshot = await opened.bytesOnDisk();
      const pick = picker(0x1ed9e7);
      const live = base.map((_, index) => index);
      let next = input.entries;
      const roots = [admitted.result];
      const edits: number[] = [];
      const persists: number[] = [];
      for (let event = 0; event < input.events; event += 1) {
        const ledger = await store.at(roots.at(-1)!);
        const step = nextEvent(live, next, pick);
        next = step.next;
        const editStarted = performance.now();
        const edited = await ledger.apply(step.mutations);
        edits.push(performance.now() - editStarted);
        const persistStarted = performance.now();
        await store.persist(edited);
        persists.push(performance.now() - persistStarted);
        roots.push(edited.root);
      }
      const diskAfter = await opened.bytesOnDisk();
      await writeFile(statePath, JSON.stringify({ roots, live, next }));
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
        diskBytesPerEvent: (diskAfter - diskAfterSnapshot) / input.events,
        counts: await opened.storage.counts(),
      };
    } else {
      const state = JSON.parse(await readFile(statePath, "utf8")) as State;
      const store = createLedgerStore(opened.storage);
      const before = await opened.storage.counts();
      let roots = state.roots.slice(1);
      let { next } = state;
      if (input.phase === "steady") {
        const ledger = await store.at(roots.at(-1)!);
        const step = nextEvent(state.live, next, picker(0x57ead));
        next = step.next;
        const edited = await ledger.apply(step.mutations);
        await store.persist(edited);
        roots = [...roots.slice(1), edited.root];
      }
      const swept = await sampled(() => store.retain(new Set(roots)));
      await writeFile(
        statePath,
        JSON.stringify({ roots: ["", ...roots], live: state.live, next }),
      );
      report = {
        retainedRoots: roots.length,
        milliseconds: Math.round(swept.milliseconds),
        baselineRssMiB: swept.baselineRssMiB,
        baselineHeapMiB: swept.baselineHeapMiB,
        peakRssMiB: swept.peakRssMiB,
        peakHeapMiB: swept.peakHeapMiB,
        countsBefore: before,
        countsAfter: await opened.storage.counts(),
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
