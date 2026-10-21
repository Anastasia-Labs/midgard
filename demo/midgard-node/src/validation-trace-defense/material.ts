/**
 * A defending operator's material for a case on one of its landed blocks:
 * the block's evidence from the node's retained DA payload, checked
 * against the header the landed queue names, and its predecessor ledger
 * from the landed ledger at the block's parent, checked against the
 * parent's root; then the defense material the fault-proofs package builds
 * from them (our trace of the case's event at the case's step count).
 * The build runs in a worker thread: its replay runs the event's scripts to
 * the end, which at the largest admitted script holds a thread for seconds,
 * and the node's main thread keeps committing meanwhile.
 *
 * Every transient lack (no retained payload, a parent off the landed
 * ledger, a ledger that does not open its root, a build that throws) is
 * `unavailable` with its detail; the tick retries it. A deterministic
 * outcome (our own fault, or a trace that disagrees) is final for the case.
 */
import { Worker } from "node:worker_threads";

import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  validationTraceDefenseFromPortable,
  type ValidationTraceDefenseMaterial,
} from "@al-ft/midgard-fault-proofs";
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { DaPayloadsDB } from "../database/index.js";
import { resolveWorkerEntry } from "../fibers/resolve-worker-entry.js";
import { type LandedLedger, ledgerAt } from "../landed-blocks/ledger.js";
import type { LandedBlockRow } from "../landed-blocks/store.js";
import { BUNDLED_WORKER_EXEC_ARGV } from "../services/validation-pool.bundled-worker-exec-argv.js";
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

const unavailable = (
  detail: string,
): Extract<ValidationTraceDefenseOutcome, { kind: "unavailable" }> => ({
  kind: "unavailable",
  detail,
});

/** `job`'s outcome, built in its own worker thread; the thread is ended
 * when the outcome arrives, when it fails, and when the build is
 * interrupted. */
const buildInWorker = (job: DefenseMaterialJob) =>
  Effect.async<PortableDefenseOutcome>((resume) => {
    let worker: Worker;
    try {
      worker = new Worker(
        resolveWorkerEntry(
          import.meta.url,
          "validation-trace-defense-material.js",
        ),
        { execArgv: [...BUNDLED_WORKER_EXEC_ARGV], workerData: job },
      );
    } catch (cause) {
      resume(Effect.succeed(unavailable(formatUnknownError(cause))));
      return;
    }
    let settled = false;
    const settle = (outcome: PortableDefenseOutcome) => {
      if (settled) return;
      settled = true;
      void worker.terminate();
      resume(Effect.succeed(outcome));
    };
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
    return Effect.sync(() => {
      settled = true;
      void worker.terminate();
    });
  });

/**
 * The defense material for a case on landed block `target`, on the case's
 * event at its step count, given `landed` (the landed ledger, which holds
 * `target`'s parent while `target` is unmerged).
 */
export const validationTraceDefenseOutcome = (
  input: Readonly<{
    target: LandedBlockRow;
    landed: LandedLedger;
    caseDatum: SDK.Case;
    point: DefenseObservationPoint;
    l1SlotConfig: L1SlotConfig;
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
    const parent = ledgerAt(input.landed, target.parentHeaderHash);
    if (parent === undefined)
      return unavailable(
        `the parent ${target.parentHeaderHash} of block ${target.headerHash} is not on the landed ledger`,
      );
    const job: DefenseMaterialJob = {
      payloadEnvelopeCbor: stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR],
      schemaVersion: stored.value[DaPayloadsDB.Columns.VERSION],
      point: input.point,
      headerHash: target.headerHash,
      parentHeaderHash: target.parentHeaderHash,
      parentUtxosRoot: target.parentUtxosRoot,
      parentEntries: [...parent.ledger].map(([outRef, output]) => ({
        outRef: Buffer.from(outRef, "hex"),
        output,
      })),
      eventKey: caseDatum.event_key,
      stepCount: Number(caseDatum.step_count),
      l1SlotConfig: input.l1SlotConfig,
    };
    const outcome = yield* buildInWorker(job);
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
