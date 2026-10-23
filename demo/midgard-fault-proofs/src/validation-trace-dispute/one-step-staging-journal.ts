/** What our workflow journal shows of our staging (one-step-staging.ts) and
 * of the resolve moves that read outputs of ours as reference inputs: the
 * staging outputs our confirmed staging transactions left, the latest spend
 * back of each we signed, and each confirmed resolve move with the out-refs
 * it read and the thread output it paid. The journal alone holds it, so a
 * runner started again over the journal reads the same. Whether a spend
 * back stands is read from the chain each time, never from the journal. */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import type {
  FraudProofWorkflowJournalEntry,
  FraudProofWorkflowJournalEvent,
} from "../workflow/journal.js";
import { latestSubmissionIntent } from "../workflow/orchestrator.fraud-proof-workflow-run-result.js";
import {
  recordedValidationTraceMoveBytes,
  VALIDATION_TRACE_DISPUTE_ACTION,
} from "./actions.js";
import { ownKeySignedBytes } from "./own-key-actions.js";

/** The action that stages one run of a staged step's operands. */
export const VALIDATION_TRACE_STAGING =
  "midgard-validation-trace-staging-v1" as const;

/** The action that spends a staging output of ours back by our key. */
export const VALIDATION_TRACE_STAGING_RECLAIM =
  "midgard-validation-trace-staging-reclaim-v1" as const;

type Intent = Extract<
  FraudProofWorkflowJournalEvent,
  { readonly kind: "submission_intent" }
>;

/** Each action of `schemaVersion` the journal shows confirmed, by the
 * intent that landed and when its confirmation was recorded, in the order of
 * their confirmations. */
const confirmedWithTimes = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  schemaVersion: string,
): readonly Readonly<{ intent: Intent; confirmedAt: string }>[] => {
  const confirmed = new Map<string, { intent: Intent; confirmedAt: string }>();
  for (const { event, recordedAt } of entries) {
    if (event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, event.actionId);
    if (
      intent === undefined ||
      intent.txHash !== event.txHash ||
      intent.actionInput.schemaVersion !== schemaVersion
    )
      continue;
    confirmed.delete(intent.actionId);
    confirmed.set(intent.actionId, { intent, confirmedAt: recordedAt });
  }
  return [...confirmed.values()];
};

/** Each action of `schemaVersion` the journal shows confirmed, by the
 * intent that landed, in the order of their confirmations. */
export const confirmedIntents = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  schemaVersion: string,
): readonly Intent[] =>
  confirmedWithTimes(entries, schemaVersion).map(({ intent }) => intent);

const outRefLabel = (input: CML.TransactionInput): string =>
  `${input.transaction_id().to_hex()}#${input.index().toString()}`;

/** A confirmed resolve move: its signed bytes, the out-refs it read, and
 * the outputs it paid. */
export type ValidationTraceResolveReader = Readonly<{
  txHash: string;
  signed: string;
  reads: ReadonlySet<string>;
  outputs: readonly Readonly<{
    address: string;
    assets: Readonly<Record<string, bigint>>;
  }>[];
}>;

/** Each confirmed resolve move of the journal. */
export const validationTraceResolveReaders = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): readonly ValidationTraceResolveReader[] =>
  confirmedIntents(entries, VALIDATION_TRACE_DISPUTE_ACTION)
    .filter(({ actionInput }) => actionInput.stage === "resolve")
    .map((intent) => {
      const signed = recordedValidationTraceMoveBytes({
        value: intent.durableRecovery,
        txHash: intent.txHash,
      });
      const body = CML.Transaction.from_cbor_hex(signed).body();
      const references = body.reference_inputs();
      const reads = new Set<string>();
      for (let index = 0; index < (references?.len() ?? 0); index += 1)
        reads.add(outRefLabel(references!.get(index)));
      const outputs = body.outputs();
      return Object.freeze({
        txHash: intent.txHash,
        signed,
        reads,
        outputs: Array.from({ length: outputs.len() }, (_, index) => {
          const { address, assets } = coreToTxOutput(outputs.get(index));
          return { address, assets };
        }),
      });
    });

/** A staging output our confirmed staging transaction left. */
export type JournaledStaging = Readonly<{
  actionId: string;
  /** The digest of the run it stages (its datum). */
  run: string;
  txHash: string;
  signed: string;
  outRef: string;
  datumCbor: string;
  /** When its confirmation was recorded. */
  confirmedAt: string;
  /** The thread out-ref whose PrepareSelected move it was staged for. */
  thread: string | undefined;
  /** That thread's token. */
  threadUnit: string | undefined;
}>;

/** Our latest signed spend back of a staging output. */
export type JournaledReclaim = Readonly<{ txHash: string; signed: string }>;

/** The staging outputs our journal shows staged, in the order they were,
 * and our latest signed spend back of each, by its out-ref. */
export const journaledStagings = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): Readonly<{
  stagings: readonly JournaledStaging[];
  reclaims: ReadonlyMap<string, JournaledReclaim>;
}> => {
  const stagings = confirmedWithTimes(
    entries,
    VALIDATION_TRACE_STAGING,
  ).flatMap(({ intent, confirmedAt }): JournaledStaging[] => {
    const signed = ownKeySignedBytes(intent.durableRecovery);
    const { outRef, threadOutRef, threadUnit } = intent.durableRecovery ?? {};
    const { run } = intent.actionInput;
    if (
      signed === undefined ||
      typeof outRef !== "string" ||
      typeof run !== "string"
    )
      return [];
    const index = Number(outRef.split("#")[1]);
    const output = CML.Transaction.from_cbor_hex(signed)
      .body()
      .outputs()
      .get(index);
    const datum = output.datum()?.as_datum();
    if (
      datum === undefined ||
      outRef !== `${intent.txHash}#${index.toString()}`
    )
      return [];
    return [
      Object.freeze({
        actionId: intent.actionId,
        run,
        txHash: intent.txHash,
        signed,
        outRef,
        datumCbor: datum.to_cbor_hex(),
        confirmedAt,
        thread: typeof threadOutRef === "string" ? threadOutRef : undefined,
        threadUnit: typeof threadUnit === "string" ? threadUnit : undefined,
      }),
    ];
  });
  const reclaims = new Map<string, JournaledReclaim>();
  for (const { event } of entries) {
    if (
      event.kind !== "submission_intent" ||
      event.actionInput.schemaVersion !== VALIDATION_TRACE_STAGING_RECLAIM
    )
      continue;
    const { consumes } = event.actionInput;
    const signed = ownKeySignedBytes(event.durableRecovery);
    if (typeof consumes === "string" && signed !== undefined)
      reclaims.set(consumes, { txHash: event.txHash, signed });
  }
  return { stagings, reclaims };
};

/** What our journal shows of our staging, and when it last recorded a
 * confirmation of ours. */
export type ValidationTraceStagingJournal = ReturnType<
  typeof journaledStagings
> &
  Readonly<{
    readers: readonly ValidationTraceResolveReader[];
    latestConfirmedAt: string | undefined;
  }>;

export const validationTraceStagingJournal = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): ValidationTraceStagingJournal => ({
  ...journaledStagings(entries),
  readers: validationTraceResolveReaders(entries),
  latestConfirmedAt: [...entries]
    .reverse()
    .find(({ event }) => event.kind === "confirmed")?.recordedAt,
});
