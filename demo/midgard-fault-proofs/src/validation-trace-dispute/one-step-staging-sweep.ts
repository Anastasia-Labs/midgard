/** The spend back of our staging outputs (one-step-staging.ts), in each run
 * of the workflow that staged them. Each
 * observation, every staging output our own view (own-view.ts) shows staged
 * that the chain shows live is spent back to our wallet by our key, as is
 * every live copy, found by its content, of a run the plan of a thread of
 * ours at the PrepareSelected move's holder stages or a move of ours read
 * (none while no plan of ours can be recomputed: the proof-materials sweep
 * takes those), unless a thread of ours still needs it:
 *
 * - a thread at the PrepareSelected move's holder needs the output of each
 *   run its move would read: the runs its plan stages
 *   (one-step-staging-holds.ts), and those staged for it;
 * - a thread past that holder and short of the settle needs the outputs our
 *   moves of it read or, when no move of ours read any, every output staged
 *   for it;
 * - a thread our move took to the settle, which read them, needs them until
 *   that move is final at release depth; a thread another party took to the
 *   settle needs them until it moves on.
 *
 * A staging output two threads read is held while either needs it. Where
 * each thread stands, and whether an output is live, is read every time from
 * the ledger with our node's mempool applied: a move of ours in the mempool
 * has taken its thread on, and a spend back of ours in the mempool has spent
 * its output. A thread move of ours goes first, save once the header has rolled back: the workflow, then
 * reconciliation-only, takes no move, and its spend backs go on, save for
 * the outputs a thread of ours at the PrepareSelected move's holder or past
 * it holds, as above. That hold lasts until the dispute ends for the
 * thread: it leaves its holders (a cancel, a timeout or an award) or the
 * case closes, whoever sends it. Once our workflow is done (completed, in conflict, or resolved by another
 * record) no thread needs any output: each is spent back before its ending
 * is taken. The ending does not wait for a spend back to be final: one that
 * is rolled back leaves its output live, an unneeded plain output at our
 * proof-materials address once the workflow is released, which a later
 * proof-materials sweep takes. A spend back that does not build is the
 * run's alarm, and is tried again every run. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { MidgardCekStagingPins } from "@al-ft/midgard-sdk/fraud-proof";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowObservation } from "../workflow/orchestrator.js";
import {
  validationTraceOneStepContracts,
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolderAt,
} from "./one-step-holders.js";
import type { ValidationTraceStagingPlan } from "./one-step-staging-holds.js";
import {
  finalAtReleaseDepth,
  VALIDATION_TRACE_STAGING_RECLAIM,
  type ValidationTraceResolveReader,
  type ValidationTraceStagingJournal,
  type ValidationTraceStagingOutput,
} from "./one-step-staging-journal.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** Why our workflow's ending waits on its staging outputs. */
export const VALIDATION_TRACE_STAGING_OUTSTANDING =
  "validation-trace-staging-outstanding" as const;

/** The spend back of the staging output `outRef`. */
export const validationTraceReclaimAction = (outRef: string) =>
  Object.freeze({
    actionId: `validation-trace-reclaim-staging:${outRef}`,
    input: Object.freeze({
      schemaVersion: VALIDATION_TRACE_STAGING_RECLAIM,
      category: "validationTraceDispute",
      stage: "reclaim_staging",
      consumes: outRef,
    }),
  });

/** The out-ref `outRef` as the staging pins name it. */
export const stagedOutRef = (outRef: string) => {
  const { txHash, outputIndex } = parseOutRefLabel(outRef);
  return { transactionId: Buffer.from(txHash, "hex"), outputIndex };
};

/** Our workflow's resolution by another record, which the adapter throws
 * rather than observes: an ending, as a completion is. */
export const VALIDATION_TRACE_RESOLVED_ELSEWHERE = "resolved_by_another_record";

/** The output of ours to spend back, and the pins its build checks. */
export type ValidationTraceStagingReclaim = Readonly<{
  utxo: UTxO;
  pins: MidgardCekStagingPins;
}>;

/** What our own view shows of one thread of ours. */
type Thread = {
  /** The staging outputs our moves of it read. */
  readonly reads: Set<string>;
  /** Our moves of it whose thread output is at the settle. */
  readonly settles: ValidationTraceResolveReader[];
  /** Where our latest move of it left it. */
  latest:
    | Readonly<{ outRef: string; holder: ValidationTraceOneStepHolder }>
    | undefined;
  /** The staging outputs staged for its PrepareSelected move. */
  readonly stagedFor: ValidationTraceStagingOutput[];
};

const unitOf = (assets: Readonly<Record<string, bigint>>) =>
  Object.entries(assets).find(
    ([asset, quantity]) => asset !== "lovelace" && quantity === 1n,
  )?.[0];

export const validationTraceStagingSweep = ({
  lucid,
  deployment,
  releaseDepth,
  live,
  usable,
  stagings,
  plans,
  traced,
  select,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  /** How deep the ledger holds a transaction of ours once it is final. */
  readonly releaseDepth: number;
  /** The output `staging` left, while it is live as staged. */
  readonly live: (
    staging: ValidationTraceStagingOutput,
  ) => Promise<UTxO | undefined>;
  /** The staging output a PrepareSelected move would read for the run
   * `datumCbor`. */
  readonly usable: (
    known: ValidationTraceStagingJournal,
    datumCbor: string,
  ) => Promise<Readonly<{ staging: ValidationTraceStagingOutput }> | undefined>;
  /** Our staging as the own view of the workflow `context` shows it. */
  readonly stagings: (
    context: FraudProofWorkflowAdapterContext,
  ) => Promise<ValidationTraceStagingJournal>;
  /** The staging plans of our threads at the PrepareSelected move's
   * holder. */
  readonly plans: (
    context: FraudProofWorkflowAdapterContext,
  ) => Promise<readonly ValidationTraceStagingPlan[]>;
  /** Whether our trace of the workflow `context`'s claim is rebuilt, so
   * the plans of our threads can be recomputed. */
  readonly traced: (context: FraudProofWorkflowAdapterContext) => boolean;
  /** Keeps the output the spend back `actionId` spends, for its build. */
  readonly select: (
    actionId: string,
    reclaim: ValidationTraceStagingReclaim,
  ) => void;
}) => {
  const holderAt = (address: string) =>
    validationTraceOneStepHolderAt(deployment, address)?.holder;

  /** Each thread of ours our own view shows, by its token. */
  const threadsOf = (known: ValidationTraceStagingJournal) => {
    const staged = new Set(known.stagings.map(({ outRef }) => outRef));
    const threads = new Map<string, Thread>();
    const of = (unit: string) => {
      const found = threads.get(unit) ?? {
        reads: new Set<string>(),
        settles: [],
        latest: undefined,
        stagedFor: [],
      };
      threads.set(unit, found);
      return found;
    };
    for (const staging of known.stagings)
      if (staging.threadUnit !== undefined)
        of(staging.threadUnit).stagedFor.push(staging);
    for (const reader of known.readers) {
      const index = reader.outputs.findIndex(
        ({ address }) => holderAt(address) !== undefined,
      );
      const output = reader.outputs[index];
      const unit = output === undefined ? undefined : unitOf(output.assets);
      if (output === undefined || unit === undefined) continue;
      const thread = of(unit);
      for (const outRef of reader.reads)
        if (staged.has(outRef)) thread.reads.add(outRef);
      const holder = holderAt(output.address)!;
      if (holder === "settle") thread.settles.push(reader);
      thread.latest = {
        outRef: `${reader.txHash}#${index.toString()}`,
        holder,
      };
    }
    return threads;
  };

  /** Where the thread `unit` stands: its holder; none once it has left
   * every holder. */
  const placeOf = async (
    unit: string,
    thread: Thread,
  ): Promise<ValidationTraceOneStepHolder | undefined> => {
    const known =
      thread.latest === undefined
        ? [
            ...new Set(
              thread.stagedFor.flatMap(({ thread: at }) =>
                at === undefined ? [] : [at],
              ),
            ),
          ]
        : [thread.latest.outRef];
    for (const outRef of known) {
      const [utxo] = await lucid.utxosByOutRef([parseOutRefLabel(outRef)]);
      if (utxo?.assets[unit] === 1n) return holderAt(utxo.address);
    }
    for (const { holder, address } of validationTraceOneStepContracts(
      deployment,
    )) {
      if (holder === "boundary") continue;
      const utxos = await lucid.utxosAtWithUnit(address, unit);
      if (utxos.some(({ assets }) => assets[unit] === 1n)) return holder;
    }
    return undefined;
  };

  /** `known` with the live copies no move of ours read of each run `planned`
   * stages or a move of ours read, so each is spent back once no thread
   * holds it. */
  const withCopies = async (
    context: FraudProofWorkflowAdapterContext,
    known: ValidationTraceStagingJournal,
    planned: readonly ValidationTraceStagingPlan[],
  ): Promise<ValidationTraceStagingJournal> => {
    if (known.byContent === undefined || !traced(context)) return known;
    const listed = new Set(known.stagings.map(({ outRef }) => outRef));
    const copies: ValidationTraceStagingOutput[] = [];
    for (const datumCbor of new Set([
      ...planned.flatMap(({ datums }) => datums),
      ...known.stagings.map((staging) => staging.datumCbor),
    ]))
      for (const copy of await known.byContent(datumCbor))
        if (!listed.has(copy.outRef)) {
          listed.add(copy.outRef);
          copies.push(copy);
        }
    return copies.length === 0
      ? known
      : { ...known, stagings: [...known.stagings, ...copies] };
  };

  /** The staging outputs a thread of ours still needs, and their pins. */
  const heldBy = async (
    known: ValidationTraceStagingJournal,
    planned: readonly ValidationTraceStagingPlan[],
  ) => {
    const held = new Set<string>();
    const pins = new MidgardCekStagingPins();
    const hold = (unit: string, outRefs: Iterable<string>) => {
      const refs = [...outRefs];
      if (refs.length === 0) return;
      for (const outRef of refs) held.add(outRef);
      pins.pin(unit, refs.map(stagedOutRef));
    };
    // Each thread its plan shows at the PrepareSelected move's holder holds
    // the output of each run its move reads.
    for (const { unit, datums } of planned)
      for (const datumCbor of datums) {
        const found = await usable(known, datumCbor);
        if (found !== undefined) hold(unit, [found.staging.outRef]);
      }
    for (const [unit, thread] of threadsOf(known)) {
      if (thread.reads.size === 0 && thread.stagedFor.length === 0) continue;
      if (thread.settles.length > 0) {
        if (
          !thread.settles.some((reader) =>
            finalAtReleaseDepth(reader, releaseDepth),
          )
        )
          hold(unit, thread.reads);
        continue;
      }
      const at = await placeOf(unit, thread);
      if (at === undefined || at === "award") continue;
      const { reads } = thread;
      if (at === "prepare") {
        hold(unit, reads);
        for (const datumCbor of new Set(
          thread.stagedFor.map((staging) => staging.datumCbor),
        )) {
          const found = await usable(known, datumCbor);
          if (found !== undefined) hold(unit, [found.staging.outRef]);
        }
      } else
        hold(
          unit,
          reads.size > 0 ? reads : thread.stagedFor.map(({ outRef }) => outRef),
        );
    }
    return { held, pins };
  };

  /** The spend back to take instead of `current`, our workflow's next step
   * as observed or its resolution by another record; the wait its ending
   * takes; none to keep `current`. */
  return async (
    context: FraudProofWorkflowAdapterContext,
    current:
      | FraudProofWorkflowObservation
      | typeof VALIDATION_TRACE_RESOLVED_ELSEWHERE,
  ): Promise<FraudProofWorkflowObservation | undefined> => {
    const unbuilt = (actionId: string) =>
      context.unbuiltActionIds?.has(actionId) === true;
    const ending =
      current === VALIDATION_TRACE_RESOLVED_ELSEWHERE ? undefined : current;
    // A thread move of ours goes first, save under reconciliation-only
    // authority, which takes no move.
    if (
      ending?.kind === "action_required" &&
      context.reconciliationOnly !== true &&
      !unbuilt(ending.action.actionId)
    )
      return undefined;
    const planned = await plans(context);
    const known = await withCopies(context, await stagings(context), planned);
    if (known.stagings.length === 0) return undefined;
    const done =
      ending === undefined ||
      ending.kind === "completed" ||
      ending.kind === "conflict";
    // Under reconciliation-only authority a standing thread of ours still
    // holds its stagings. Past the PrepareSelected move its evidence names
    // those very outputs, so none can be staged again should the header come
    // back; at the prepare holder it keeps the usable ones so the thread can
    // go on without a fresh staging. The hold ends once the thread leaves its
    // holders (a cancel, a timeout or an award) or the case closes, whoever
    // sends it, and the spend back follows.
    const { held, pins } = done
      ? { held: new Set<string>(), pins: new MidgardCekStagingPins() }
      : await heldBy(known, planned);
    const outstanding: string[] = [];
    for (const staging of known.stagings) {
      if (held.has(staging.outRef)) continue;
      const utxo = await live(staging);
      if (utxo === undefined) continue;
      const action = validationTraceReclaimAction(staging.outRef);
      if (unbuilt(action.actionId)) {
        outstanding.push(
          `${staging.outRef} holding ${(utxo.assets.lovelace ?? 0n).toString()} lovelace does not spend back`,
        );
        continue;
      }
      select(action.actionId, { utxo, pins });
      return { kind: "action_required", action };
    }
    return done && outstanding.length > 0
      ? {
          kind: "pending",
          reason: `${VALIDATION_TRACE_STAGING_OUTSTANDING}: ${outstanding.join("; ")}`,
        }
      : undefined;
  };
};
