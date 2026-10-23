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
 * of ours goes first. Once our workflow is done (completed, or in conflict)
 * no thread needs any output: each is spent back before its ending is taken,
 * and the ending waits until each spend back is final at release depth, and
 * while an output the provider does not show may still be live. A spend back
 * that does not build is the run's alarm, and is tried again every run. */
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

const unitOf = (assets: Readonly<Record<string, bigint>>) =>
  Object.entries(assets).find(
    ([asset, quantity]) => asset !== "lovelace" && quantity === 1n,
  )?.[0];

export const validationTraceStagingSweep = ({
  lucid,
  deployment,
  signedRecovery,
  recent,
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

  /** The staging outputs a thread of ours still needs, and their pins. */
  const heldBy = async (
    known: ValidationTraceStagingJournal,
    lagging: boolean,
  ) => {
    const held = new Set<string>();
    const pins = new MidgardCekStagingPins();
    const hold = (unit: string, outRefs: Iterable<string>) => {
      const refs = [...outRefs];
      if (refs.length === 0) return;
      for (const outRef of refs) held.add(outRef);
      pins.pin(unit, refs.map(stagedOutRef));
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
      if (at === "prepare") {
        hold(unit, thread.reads);
        for (const datumCbor of new Set(
          thread.stagedFor.map((staging) => staging.datumCbor),
        )) {
          const found = await usable(known, datumCbor);
          if (found !== undefined) hold(unit, [found.staging.outRef]);
        }
        continue;
      }
      hold(
        unit,
        thread.reads.size > 0
          ? thread.reads
          : thread.stagedFor.map(({ outRef }) => outRef),
      );
    }
    return { held, pins };
  };

  /** The spend back to take instead of `current`, our workflow's next step
   * as observed; the wait its ending takes; none to keep `current`. */
  return async (
    context: FraudProofWorkflowAdapterContext,
    current: FraudProofWorkflowObservation,
  ): Promise<FraudProofWorkflowObservation | undefined> => {
    const unbuilt = (actionId: string) =>
      context.unbuiltActionIds?.has(actionId) === true;
    if (current.kind === "action_required" && !unbuilt(current.action.actionId))
      return undefined;
    const known = validationTraceStagingJournal(context.entries);
    if (known.stagings.length === 0) return undefined;
    const done = current.kind === "completed" || current.kind === "conflict";
    const lagging =
      known.latestConfirmedAt !== undefined && recent(known.latestConfirmedAt);
    const { held, pins } = done
      ? { held: new Set<string>(), pins: new MidgardCekStagingPins() }
      : await heldBy(known, lagging);
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
      if (held.has(staging.outRef)) continue;
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
