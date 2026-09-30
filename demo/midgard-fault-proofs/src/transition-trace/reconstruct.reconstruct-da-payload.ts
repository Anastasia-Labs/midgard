import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import { aikenSerialisedPlutusDataCborPreservingMapOrder } from "@al-ft/midgard-core/plutus-data-cbor";
import * as SDK from "@al-ft/midgard-sdk";
import { createCanonicalMidgardLedgerDescriptorResolver } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  retainedLedgerDescriptorCandidates,
  retainedUndecodableOutputDescriptor,
} from "../evidence/retained-ledger-output.js";
import { transitionTraceError } from "./errors.js";
import { buildCountedRoot, keyValuePhasRootWithCount } from "./phas.js";
import {
  authenticateForcedTransactionPreimages,
  buildSourceEvents,
  ensureUniqueSourceFingerprints,
  headerCounts,
  headerRoots,
  rootMismatches,
} from "./reconstruct.authenticate-forced-transaction-preimages.js";
import {
  decodePayloadStrict,
  decodeTransactions,
  decodeTypedEntries,
  rawEntries,
  validateDenseTransitionTrace,
} from "./reconstruct.decode-transactions.js";
import {
  countMismatches,
  eventKeyFingerprint,
  normalizeHeaderHash,
  type PayloadRootSet,
  type ReconstructDaPayloadOptions,
  type TransitionTraceReconstruction,
} from "./reconstruct.transition-trace-reconstruction.js";

export const reconstructDaPayload = async ({
  payloadEnvelopeCbor,
  expectedHeaderHash,
  committedHeader,
}: ReconstructDaPayloadOptions): Promise<TransitionTraceReconstruction> => {
  let payloadCbor: Buffer;
  try {
    payloadCbor = (
      await unwrapDaPayload(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      })
    ).innerBytes;
  } catch (cause) {
    throw transitionTraceError(
      "malformedPayload",
      "Failed to decode the mandatory DaPayloadEnvelopeV1.",
      cause,
    );
  }
  const payload = decodePayloadStrict(payloadCbor);
  const body = payload.block_body;
  const payloadBuffer = Buffer.from(payloadCbor);
  const payloadEnvelopeBuffer = Buffer.from(payloadEnvelopeCbor);
  const header = body.header;
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  if (headerHash !== body.header_hash) {
    throw transitionTraceError(
      "headerMismatch",
      `Embedded header hashes to ${headerHash}, but payload header_hash is ${body.header_hash}.`,
    );
  }
  if (
    expectedHeaderHash !== undefined &&
    normalizeHeaderHash(expectedHeaderHash, "expected header hash") !==
      body.header_hash
  ) {
    throw transitionTraceError(
      "headerMismatch",
      `Payload header_hash ${body.header_hash} does not match expected header_hash ${expectedHeaderHash}.`,
    );
  }
  if (
    committedHeader !== undefined &&
    Data.to(committedHeader as never, SDK.Header as never) !==
      Data.to(header as never, SDK.Header as never)
  ) {
    throw transitionTraceError(
      "headerMismatch",
      "Payload embedded header does not match the committed L1 header.",
    );
  }

  const rawTransactions = rawEntries("transactions", body.transactions);
  const rawUtxos = rawEntries("utxos", body.utxos);
  const retainedDescriptors = retainedLedgerDescriptorCandidates(
    body.validation_trace_witnesses,
  );
  const resolveDescriptor = createCanonicalMidgardLedgerDescriptorResolver();
  const descriptorUtxos = rawUtxos.map((entry) => {
    try {
      return {
        key: Buffer.from(entry.key),
        value: resolveDescriptor({
          outRef: entry.key,
          outputCbor: entry.value,
        }),
      };
    } catch (cause) {
      try {
        return {
          key: Buffer.from(entry.key),
          value: retainedUndecodableOutputDescriptor({
            key: entry.key,
            output: entry.value,
            candidates: retainedDescriptors,
          }),
        };
      } catch {
        /* The exact root check below remains the only authority. */
      }
      throw transitionTraceError(
        "invalidPayloadEntries",
        `UTxO ${entry.key.toString("hex")} cannot produce an exact canonical V1 descriptor.`,
        cause,
      );
    }
  });
  const rootData = {
    utxos: await keyValuePhasRootWithCount(descriptorUtxos),
    withdrawals: await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      rawEntries("withdrawals", body.withdrawals),
    ),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      rawEntries("forced_transactions", body.forced_transactions),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      rawTransactions,
    ),
    deposits: await buildCountedRoot(
      SDK.ROOT_DOMAINS.deposits,
      rawEntries("deposits", body.deposits),
    ),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      rawEntries("transition_trace", body.transition_trace),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      rawEntries("event_to_step", body.event_to_step),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      rawEntries("validation_traces", body.validation_traces),
    ),
  };
  const roots: PayloadRootSet = {
    utxosRoot: rootData.utxos.root,
    withdrawalsRoot: rootData.withdrawals.root,
    forcedTransactionsRoot: rootData.forcedTransactions.root,
    transactionsRoot: rootData.transactions.root,
    depositsRoot: rootData.deposits.root,
    transitionTraceRoot: rootData.transitionTrace.root,
    eventToStepRoot: rootData.eventToStep.root,
    validationTracesRoot: rootData.validationTraces.root,
  };
  const counts = body.counts;
  const mismatchedRoots = rootMismatches(headerRoots(header), roots);
  if (mismatchedRoots.length > 0) {
    throw transitionTraceError(
      "rootMismatch",
      `Payload roots do not match committed header: ${mismatchedRoots.join(
        ",",
      )}.`,
    );
  }
  const mismatchedCounts = countMismatches(headerCounts(header), counts);
  if (mismatchedCounts.length > 0) {
    throw transitionTraceError(
      "countMismatch",
      `Payload counts do not match committed header: ${mismatchedCounts.join(
        ",",
      )}.`,
    );
  }

  // Q44 is the one family for which canonical reconstruction is expected to
  // reject the payload. Detect it only after the raw transactions MPF/count
  // and embedded header have been authenticated against L1. This typed error
  // is therefore not a generic decode/fetch fallback: it proves an exact
  // committed source leaf is itself a total Q44 violation.
  const authenticatedSourceLeafDefect = rawTransactions.find(
    ({ key, value }) =>
      SDK.daHashPreimageEvidenceFromCommittedLeaf({
        committedTxId: key.toString("hex"),
        committedLeafValue: value,
      }).isViolation,
  );
  if (authenticatedSourceLeafDefect !== undefined) {
    throw transitionTraceError(
      "authenticatedSourceLeafDefect",
      `L1-authenticated transactions_root contains a Q44 source-leaf defect at ${authenticatedSourceLeafDefect.key.toString("hex")}.`,
    );
  }

  const transactions = await decodeTransactions({
    entries: body.transactions,
    preimages: body.transaction_preimages,
  });

  const withdrawals = decodeTypedEntries<
    SDK.OutputReference,
    SDK.WithdrawalInfo
  >({
    fieldName: "withdrawals",
    entries: body.withdrawals,
    keySchema: SDK.OutputReference as never,
    valueSchema: SDK.WithdrawalInfoSchema,
    // The authenticated leaf is already serialiseData, including opaque map
    // pair order/multiplicity. Typed decoding validates the schema only.
    valueEncoder: (_value, raw) =>
      aikenSerialisedPlutusDataCborPreservingMapOrder(raw),
  });
  const decodedForcedTransactions = decodeTypedEntries<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >({
    fieldName: "forced_transactions",
    entries: body.forced_transactions,
    keySchema: SDK.OutputReference as never,
    valueSchema: SDK.ForcedInclusionTxV1Schema,
  });
  const forcedTransactions = authenticateForcedTransactionPreimages(
    decodedForcedTransactions,
    body.forced_transaction_preimages,
  );
  const deposits = decodeTypedEntries<SDK.OutputReference, SDK.DepositInfo>({
    fieldName: "deposits",
    entries: body.deposits,
    keySchema: SDK.OutputReference as never,
    valueSchema: SDK.DepositInfoSchema,
    valueEncoder: (_value, raw) =>
      aikenSerialisedPlutusDataCborPreservingMapOrder(raw),
  });
  const transitionTrace = decodeTypedEntries<bigint, SDK.TransitionStep>({
    fieldName: "transition_trace",
    entries: body.transition_trace,
    keySchema: Data.Integer() as never,
    valueSchema: SDK.TransitionStepSchema,
  });
  validateDenseTransitionTrace(transitionTrace, counts.transitionStepCount);
  const eventToStep = decodeTypedEntries<SDK.EventKey, SDK.EventToStepValue>({
    fieldName: "event_to_step",
    entries: body.event_to_step,
    keySchema: SDK.EventKeySchema,
    valueSchema: SDK.EventToStepValueSchema,
  });
  const sourceEvents = buildSourceEvents({
    withdrawals,
    forcedTransactions,
    transactions,
    deposits,
  });
  const traceByStepIndex = new Map(
    transitionTrace.map((entry) => [entry.key, entry] as const),
  );
  const eventToStepByFingerprint = new Map(
    eventToStep.map(
      (entry) => [eventKeyFingerprint(entry.key), entry] as const,
    ),
  );

  return {
    payload,
    payloadEnvelopeCbor: payloadEnvelopeBuffer,
    payloadCbor: payloadBuffer,
    header,
    headerHash,
    roots,
    counts,
    utxos: rawUtxos,
    withdrawals,
    forcedTransactions,
    transactions,
    deposits,
    transitionTrace,
    eventToStep,
    sourceEvents,
    sourceEventsByFingerprint: ensureUniqueSourceFingerprints(sourceEvents),
    traceByStepIndex,
    eventToStepByFingerprint,
    rootData,
  };
};
