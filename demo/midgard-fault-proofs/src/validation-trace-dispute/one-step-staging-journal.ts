/** What our workflow journal shows of our staging (one-step-staging.ts) and
 * of the resolve moves that read outputs of ours as reference inputs: the
 * staging outputs our confirmed staging transactions left, those our
 * confirmed reclaims spent back, and each confirmed resolve move with the
 * out-refs it read and the thread output it paid. The journal alone holds
 * it, so a runner started again over the journal reads the same. */
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
 * intent that landed, in the order of their confirmations. */
export const confirmedIntents = (
  entries: readonly FraudProofWorkflowJournalEntry[],
  schemaVersion: string,
): readonly Intent[] => {
  const confirmed = new Map<string, Intent>();
  for (const { event } of entries) {
    if (event.kind !== "confirmed") continue;
    const intent = latestSubmissionIntent(entries, event.actionId);
    if (
      intent === undefined ||
      intent.txHash !== event.txHash ||
      intent.actionInput.schemaVersion !== schemaVersion
    )
      continue;
    confirmed.delete(intent.actionId);
    confirmed.set(intent.actionId, intent);
  }
  return [...confirmed.values()];
};

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
}>;

/** The staging outputs our journal shows staged, in the order they were,
 * and the out-refs of those it shows spent back. */
export const journaledStagings = (
  entries: readonly FraudProofWorkflowJournalEntry[],
): Readonly<{
  stagings: readonly JournaledStaging[];
  reclaimed: ReadonlySet<string>;
}> => {
  const stagings = confirmedIntents(entries, VALIDATION_TRACE_STAGING).flatMap(
    (intent): JournaledStaging[] => {
      const signed = ownKeySignedBytes(intent.durableRecovery);
      const { outRef } = intent.durableRecovery ?? {};
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
        }),
      ];
    },
  );
  const reclaimed = new Set(
    confirmedIntents(entries, VALIDATION_TRACE_STAGING_RECLAIM).flatMap(
      ({ actionInput: { consumes } }) =>
        typeof consumes === "string" ? [consumes] : [],
    ),
  );
  return { stagings, reclaimed };
};
