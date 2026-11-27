/**
 * A defending operator's material for a case on one of its landed blocks:
 * the block's evidence from the node's retained DA payload, checked
 * against the header the landed queue names, and its predecessor ledger,
 * the node's store ledger at the parent's root (retained, or rebuilt by
 * the processed deltas from the newest retained root before it,
 * `landed-blocks/parent-ledger.ts`); then the defense material the
 * fault-proofs package builds from them (our trace of the case's event at
 * the case's step count). The payload is looked up first, so a block
 * without one costs no ledger work.
 *
 * The build runs in a worker thread: its replay runs the event's scripts to
 * the end, which at the largest admitted script holds a thread for seconds,
 * and the node's main thread keeps committing meanwhile. The ledger stays
 * on the main thread; the worker reads the trie nodes and outputs the
 * replay touches over a read channel of its own (`ledger-port.ts`).
 *
 * Every transient lack (no retained payload, a parent ledger the store
 * cannot give, a store that cannot be read, a build that throws, a read
 * that fails, a worker that fails or outruns its bound) is `unavailable`
 * with its detail; the tick retries it. A deterministic outcome (our own
 * fault, or a trace that disagrees) is final for the case.
 */
import { MessageChannel, Worker } from "node:worker_threads";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type Ledger,
  ledgerReads,
} from "@al-ft/midgard-fault-proofs/ledger-store";
import {
  validationTraceDefenseFromPortable,
  type ValidationTraceDefenseMaterial,
} from "@al-ft/midgard-fault-proofs/validation-trace-dispute/defender";
import type * as SDK from "@al-ft/midgard-sdk/da";
import { Effect, Option } from "effect";

import { DaPayloadsDB } from "../database/index.js";
import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import { ledgerAt } from "../landed-blocks/parent-ledger.js";
import type { LandedBlockRow } from "../landed-blocks/store.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "../services/validation-pool.bundled-worker-exec-argv.js";
import { serveLedgerReads } from "./ledger-port.js";
import type {
  DefenseMaterialJob,
  PortableDefenseOutcome,
} from "./material-build.js";

export type ValidationTraceDefenseOutcome =
  | Readonly<{
      kind: "defend";
      material: Extract<ValidationTraceDefenseMaterial, { kind: "defend" }>;
    }>
  | Readonly<{ kind: "operator_fault"; detail: string }>
  | Readonly<{ kind: "trace_disagrees"; detail: string }>
  | Readonly<{ kind: "unavailable"; detail: string }>;

/** Where the node observed the landed block: the follower's cursor. */
export type DefenseObservationPoint = Readonly<{ slot: number; hash: string }>;

/**
 * The ledgers the node's store retains: the ledger with `root`, failing
 * with `LedgerNotRetained` when the store does not retain it.
 */
export type DefenseLedgers = Readonly<{
  retainedLedger: (root: string) => Effect.Effect<Ledger, unknown>;
}>;

const unavailable = (
  detail: string,
): Extract<ValidationTraceDefenseOutcome, { kind: "unavailable" }> => ({
  kind: "unavailable",
  detail,
});

/**
 * The worker a build runs in: its entry module, and how long a build may
 * run before its worker is ended and the build is `unavailable`.
 */
export type DefenseMaterialWorker = Readonly<{ entry: URL; timeoutMs: number }>;

/**
 * A build's bound: the consensus-maximum build measured about 8.4 s, and
 * this leaves fourteen times that for a slower or busier host.
 */
export const DEFENSE_MATERIAL_BUILD_TIMEOUT_MS = 120_000;

/** The node's own material worker, bundled beside it. */
export const nodeDefenseMaterialWorker = (): DefenseMaterialWorker => ({
  entry: resolveWorkerEntry(
    import.meta.url,
    "validation-trace-defense-material.js",
  ),
  timeoutMs: DEFENSE_MATERIAL_BUILD_TIMEOUT_MS,
});

/** `job`'s outcome, built in its own worker thread (`using`, the node's own
 * by default) that reads `predecessor` over a read channel; the thread is
 * ended and the channel closed when the outcome arrives, when the thread
 * fails, when it outruns its bound, and when the build is interrupted. */
const buildInWorker = (
  job: Omit<DefenseMaterialJob, "ledgerPort">,
  predecessor: Ledger,
  using: DefenseMaterialWorker | undefined,
) =>
  Effect.async<PortableDefenseOutcome>((resume) => {
    const channel = new MessageChannel();
    let worker: Worker;
    let timeoutMs: number;
    try {
      serveLedgerReads(channel.port1, ledgerReads(predecessor));
      const { entry, timeoutMs: bound } = using ?? nodeDefenseMaterialWorker();
      timeoutMs = bound;
      worker = new Worker(entry, {
        execArgv: [...BUNDLED_WORKER_EXEC_ARGV],
        workerData: {
          ...job,
          ledgerPort: channel.port2,
        } satisfies DefenseMaterialJob,
        transferList: [channel.port2],
      });
    } catch (cause) {
      channel.port1.close();
      channel.port2.close();
      resume(Effect.succeed(unavailable(formatUnknownError(cause))));
      return;
    }
    let settled = false;
    const end = () => {
      settled = true;
      clearTimeout(timer);
      channel.port1.close();
      void worker.terminate();
    };
    const settle = (outcome: PortableDefenseOutcome) => {
      if (settled) return;
      end();
      resume(Effect.succeed(outcome));
    };
    const timer = setTimeout(
      () =>
        settle(
          unavailable(
            `the defense material worker did not answer within ${timeoutMs.toString()} ms`,
          ),
        ),
      timeoutMs,
    );
    worker.once("message", settle);
    worker.once("error", (cause) =>
      settle(
        unavailable(
          `the defense material worker failed: ${formatUnknownError(cause)}`,
        ),
      ),
    );
    worker.once("exit", (code) =>
      settle(
        unavailable(
          `the defense material worker exited with code ${code.toString()}`,
        ),
      ),
    );
    return Effect.sync(end);
  });

/**
 * The defense material for a case on landed block `target`, on the case's
 * event at its step count. `lineage` is the processed chain from the
 * confirmed frontier through `target`'s parent (empty when the parent is
 * the frontier), which a rebuild of the parent's ledger replays when the
 * store does not retain its root.
 */
export const validationTraceDefenseOutcome = (
  input: Readonly<{
    target: LandedBlockRow;
    lineage: readonly LandedBlockRow[];
    ledgers: DefenseLedgers;
    caseDatum: SDK.Case;
    point: DefenseObservationPoint;
    l1SlotConfig: L1SlotConfig;
    /** The worker the build runs in; the node's own by default. */
    worker?: DefenseMaterialWorker | undefined;
  }>,
) =>
  Effect.gen(function* () {
    const { target, caseDatum } = input;
    const stored = yield* DaPayloadsDB.retrieveByHeaderHash(
      Buffer.from(target.headerHash, "hex"),
    );
    if (Option.isNone(stored))
      return unavailable(
        `no retained DA payload of block ${target.headerHash}`,
      );
    const parent = yield* Effect.either(
      ledgerAt(
        input.ledgers,
        target.parentUtxosRoot,
        Effect.succeed(input.lineage),
      ),
    );
    if (parent._tag === "Left")
      return unavailable(
        `the ledger store cannot give the parent ledger of block ${target.headerHash}: ${formatUnknownError(parent.left)}`,
      );
    if (parent.right.kind === "unavailable")
      return unavailable(
        `the parent ledger of block ${target.headerHash}: ${parent.right.detail}`,
      );
    const outcome = yield* buildInWorker(
      {
        payloadEnvelopeCbor: stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR],
        schemaVersion: stored.value[DaPayloadsDB.Columns.VERSION],
        point: input.point,
        headerHash: target.headerHash,
        parentHeaderHash: target.parentHeaderHash,
        parentUtxosRoot: target.parentUtxosRoot,
        eventKey: caseDatum.event_key,
        stepCount: Number(caseDatum.step_count),
        l1SlotConfig: input.l1SlotConfig,
      },
      parent.right.ledger,
      input.worker,
    );
    if (outcome.kind !== "defend") return outcome;
    try {
      return {
        kind: "defend",
        material: validationTraceDefenseFromPortable(outcome.defense),
      } as const;
    } catch (cause) {
      return unavailable(formatUnknownError(cause));
    }
  });
