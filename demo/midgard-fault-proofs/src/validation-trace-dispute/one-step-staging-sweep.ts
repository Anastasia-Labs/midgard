/** The spend back of our staging outputs (one-step-staging.ts), in each run
 * of the workflow that staged them while it holds its reservation. Each
 * observation, every staging output our journal shows staged that the chain
 * shows live is spent back to our wallet by our key, unless a thread of ours
 * still needs it:
 *
 * - a thread at the PrepareSelected move's holder needs, of the outputs
 *   staged for it, the one of each run its move would read;
 * - a thread past that holder and short of the settle needs the outputs our
 *   moves of it read or, when no move of ours read any, every output staged
 *   for it;
 * - a thread our move took to the settle, which read them, needs them until
 *   that move is final at release depth; a thread another party took to the
 *   settle needs them until it moves on.
 *
 * Where each thread stands is read from the chain every time, as is whether
 * a spend back of ours stands: one the ledger took and then dropped is
 * required again, and its exact signed bytes are sent again. A thread move
 * of ours goes first, save once the header has rolled back: the workflow,
 * then reconciliation-only, takes no move, and its spend backs go on, save
 * for the outputs a thread of ours at the PrepareSelected move's holder or
 * past it holds, as above. That hold lasts until the dispute ends for the
 * thread: it leaves its holders (a cancel, a timeout or an award) or the
 * case closes, whoever sends it. Once the case's expiry has passed with
 * such a thread standing, a run that would only wait reports the outputs it
 * holds under a named alarm instead. Once our workflow is
 * done (completed, in conflict, or resolved by another record) no thread
 * needs any output: each is spent back before its ending is taken, and the
 * ending waits until each spend back is final at release depth, and while
 * an output the provider does not show may still be live. A spend back that
 * does not build is the run's alarm, and is tried again every run. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import { MidgardCekStagingPins } from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import type { FraudProofSignedTransactionRecovery } from "../workflow/l1-source.js";
import type { FraudProofWorkflowAdapterContext } from "../workflow/orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowObservation } from "../workflow/orchestrator.js";
import {
  validationTraceOneStepContracts,
  type ValidationTraceOneStepHolder,
  validationTraceOneStepHolderAt,
} from "./one-step-holders.js";
import {
  type JournaledStaging,
  VALIDATION_TRACE_STAGING_RECLAIM,
  type ValidationTraceResolveReader,
  type ValidationTraceStagingJournal,
  validationTraceStagingJournal,
} from "./one-step-staging-journal.js";
import {
  finalAtReleaseDepth,
  isFinalAtReleaseDepth,
} from "./own-key-actions.js";
import type { ValidationTraceDisputeDeployment } from "./transactions.js";

/** Why our workflow's ending waits on its staging outputs. */
export const VALIDATION_TRACE_STAGING_OUTSTANDING =
  "validation-trace-staging-outstanding" as const;

/** The alarm of staging outputs a thread of ours holds under
 * reconciliation-only authority once its case's expiry has passed. */
export const VALIDATION_TRACE_STAGING_HELD_PAST_EXPIRY =
  "validation-trace-staging-held-past-expiry" as const;

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

/** What our journal shows of one thread of ours. */
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
  readonly stagedFor: JournaledStaging[];
};

/** A thread of ours holding staging outputs, and where it stands. */
type Holding = Readonly<{
  unit: string;
  holder: ValidationTraceOneStepHolder | "unknown";
  outRefs: readonly string[];
}>;

const unitOf = (assets: Readonly<Record<string, bigint>>) =>
  Object.entries(assets).find(
    ([asset, quantity]) => asset !== "lovelace" && quantity === 1n,
  )?.[0];

export const validationTraceStagingSweep = ({
  lucid,
  deployment,
  signedRecovery,
  recent,
  now,
  caseExpiresAtMs,
  live,
  usable,
  select,
}: {
  readonly lucid: LucidEvolution;
  readonly deployment: ValidationTraceDisputeDeployment;
  readonly signedRecovery: FraudProofSignedTransactionRecovery;
  /** Whether a confirmation recorded at `confirmedAt` is still inside the
   * provider's grace. */
  readonly recent: (confirmedAt: string) => boolean;
  /** The runner's clock, which the case's expiry is read against. */
  readonly now: () => number;
  /** The expiry of our workflow's case, while the case is live. */
  readonly caseExpiresAtMs: (
    context: FraudProofWorkflowAdapterContext,
  ) => Promise<number | undefined>;
  /** The output `staging` left, while it is live as staged. */
  readonly live: (staging: JournaledStaging) => Promise<UTxO | undefined>;
  /** The staging output a PrepareSelected move would read for the run
   * `datumCbor`. */
  readonly usable: (
    known: ValidationTraceStagingJournal,
    datumCbor: string,
  ) => Promise<Readonly<{ staging: JournaledStaging }> | undefined>;
  /** Keeps the output the spend back `actionId` spends, for its build. */
  readonly select: (
    actionId: string,
    reclaim: ValidationTraceStagingReclaim,
  ) => void;
}) => {
  const holderAt = (address: string) =>
    validationTraceOneStepHolderAt(deployment, address)?.holder;

  /** Each thread of ours our journal shows, by its token. */
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

  /** Where the thread `unit` stands: its holder; "unknown" while the
   * provider may not show it yet; none once it has left every holder. */
  const placeOf = async (
    unit: string,
    thread: Thread,
    lagging: boolean,
  ): Promise<ValidationTraceOneStepHolder | "unknown" | undefined> => {
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
    return lagging ? "unknown" : undefined;
  };

  /** The alarm of the outputs `holdings` keep, those of `kept` that are
   * live, once the case's expiry `end` has passed; none before it. */
  const heldPastExpiry = (
    holdings: readonly Holding[],
    kept: ReadonlyMap<string, UTxO>,
    end: number,
  ): string | undefined => {
    if (now() <= end) return undefined;
    const held = holdings.flatMap(({ unit, holder, outRefs }) => {
      const outputs = outRefs.flatMap((outRef) => {
        const utxo = kept.get(outRef);
        return utxo === undefined ? [] : [{ outRef, utxo }];
      });
      if (outputs.length === 0) return [];
      const lovelace = outputs.reduce(
        (sum, { utxo }) => sum + (utxo.assets.lovelace ?? 0n),
        0n,
      );
      return [
        `thread ${unit} at the ${holder} holder holds staging outputs ${outputs.map(({ outRef }) => outRef).join(", ")} of ours, ${lovelace.toString()} lovelace at our staging address`,
      ];
    });
    return held.length === 0
      ? undefined
      : `${VALIDATION_TRACE_STAGING_HELD_PAST_EXPIRY}: ${held.join("; ")}, past its case's expiry at ${end.toString()}; under reconciliation-only authority each is spent back once the thread leaves its holder or the case closes, whoever sends it`;
  };

  /** The staging outputs a thread of ours still needs, and their pins. */
  const heldBy = async (
    known: ValidationTraceStagingJournal,
    lagging: boolean,
  ) => {
    const held = new Set<string>();
    const pins = new MidgardCekStagingPins();
    const holdings: Holding[] = [];
    const hold = (unit: string, outRefs: Iterable<string>) => {
      const refs = [...outRefs];
      if (refs.length === 0) return refs;
      for (const outRef of refs) held.add(outRef);
      pins.pin(unit, refs.map(stagedOutRef));
      return refs;
    };
    for (const [unit, thread] of threadsOf(known)) {
      if (thread.reads.size === 0 && thread.stagedFor.length === 0) continue;
      if (thread.settles.length > 0) {
        let final = false;
        for (const { txHash, signed } of thread.settles)
          if (await finalAtReleaseDepth(signedRecovery, txHash, signed)) {
            final = true;
            break;
          }
        if (!final) hold(unit, thread.reads);
        continue;
      }
      const at = await placeOf(unit, thread, lagging);
      if (at === undefined || at === "award") continue;
      const outRefs: string[] = [];
      if (at === "prepare") {
        outRefs.push(...hold(unit, thread.reads));
        for (const datumCbor of new Set(
          thread.stagedFor.map((staging) => staging.datumCbor),
        )) {
          const found = await usable(known, datumCbor);
          if (found !== undefined)
            outRefs.push(...hold(unit, [found.staging.outRef]));
        }
      } else
        outRefs.push(
          ...hold(
            unit,
            thread.reads.size > 0
              ? thread.reads
              : thread.stagedFor.map(({ outRef }) => outRef),
          ),
        );
      if (outRefs.length > 0) holdings.push({ unit, holder: at, outRefs });
    }
    return { held, pins, holdings };
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
    const known = validationTraceStagingJournal(context.entries);
    if (known.stagings.length === 0) return undefined;
    const done =
      ending === undefined ||
      ending.kind === "completed" ||
      ending.kind === "conflict";
    const lagging =
      known.latestConfirmedAt !== undefined && recent(known.latestConfirmedAt);
    // Under reconciliation-only authority a standing thread of ours still
    // holds its stagings. Past the PrepareSelected move its evidence names
    // those very outputs, so none can be staged again should the header come
    // back; at the prepare holder it keeps the usable ones so the thread can
    // go on without a fresh staging. The hold ends once the thread leaves its
    // holders (a cancel, a timeout or an award) or the case closes, whoever
    // sends it, and the spend back follows. Once the case's expiry has
    // passed, a run that would only wait reports what is held under a named
    // alarm instead; it sends nothing it would not have.
    const { held, pins, holdings } = done
      ? {
          held: new Set<string>(),
          pins: new MidgardCekStagingPins(),
          holdings: [],
        }
      : await heldBy(known, lagging);
    const kept = new Map<string, UTxO>();
    const outstanding: string[] = [];
    for (const staging of known.stagings) {
      const reclaim = known.reclaims.get(staging.outRef);
      const spentBack =
        reclaim === undefined
          ? undefined
          : await signedRecovery.observeSignedTransaction({
              transactionHash: reclaim.txHash,
              signedTransactionCborHex: reclaim.signed,
            });
      if (spentBack?.status === "included") {
        if (!isFinalAtReleaseDepth(spentBack))
          outstanding.push(
            `the spend back ${reclaim!.txHash} of ${staging.outRef} is not yet final at release depth`,
          );
        continue;
      }
      const utxo = await live(staging);
      if (utxo === undefined) {
        // While the L1 source shows our spend back's input unspent, or a
        // confirmation of ours is recent, the provider may be behind.
        if (
          spentBack === undefined
            ? lagging || recent(staging.confirmedAt)
            : spentBack.status === "pending" ||
              spentBack.status === "rebroadcast"
        )
          outstanding.push(
            `${staging.outRef} is not yet visible to the provider`,
          );
        continue;
      }
      if (held.has(staging.outRef)) {
        kept.set(staging.outRef, utxo);
        continue;
      }
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
    if (done)
      return outstanding.length > 0
        ? {
            kind: "pending",
            reason: `${VALIDATION_TRACE_STAGING_OUTSTANDING}: ${outstanding.join("; ")}`,
          }
        : undefined;
    if (
      context.reconciliationOnly !== true ||
      ending?.kind !== "pending" ||
      kept.size === 0
    )
      return undefined;
    const end = await caseExpiresAtMs(context);
    const alarm =
      end === undefined ? undefined : heldPastExpiry(holdings, kept, end);
    return alarm === undefined ? undefined : { kind: "pending", reason: alarm };
  };
};
