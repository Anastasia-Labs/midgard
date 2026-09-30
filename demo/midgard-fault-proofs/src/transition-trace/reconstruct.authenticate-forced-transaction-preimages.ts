import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { transitionTraceError } from "./errors.js";
import {
  type DecodedForcedTransactionEntry,
  type DecodedRootEntry,
  entryBuffer,
  eventKeyFingerprint,
  normalizeEntryHex,
  type PayloadCountSet,
  type PayloadRootSet,
  type SourceEventRecord,
  type TransitionTraceReconstruction,
} from "./reconstruct.transition-trace-reconstruction.js";

export const authenticateForcedTransactionPreimages = (
  entries: readonly DecodedRootEntry<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >[],
  preimages: readonly SDK.DaPayloadEntry[],
): readonly DecodedForcedTransactionEntry[] => {
  const preimagesByKey = new Map(
    preimages.map(([key, value], index) => [
      normalizeEntryHex(
        key,
        `forced_transaction_preimages[${index.toString()}].key`,
      ),
      entryBuffer(
        value,
        `forced_transaction_preimages[${index.toString()}].value`,
      ),
    ]),
  );
  if (preimagesByKey.size !== preimages.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "forced_transaction_preimages contains duplicate transaction-order IDs.",
    );
  }
  const authenticated: DecodedForcedTransactionEntry[] = [];
  for (const [index, entry] of entries.entries()) {
    const key = entry.keyBytes.toString("hex");
    const canonicalTransactionCbor = preimagesByKey.get(key);
    if (canonicalTransactionCbor === undefined) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `forced_transaction_preimages is missing forced_transactions[${index.toString()}].`,
      );
    }
    try {
      // Authenticate the immutable envelope and field commitments without
      // discarding malformed inner material needed for a fault proof.
      const raw = deriveMidgardForcedTxFaultEvidenceMaterial(
        canonicalTransactionCbor,
      );
      const source = raw.proofSource;
      const expected: SDK.ForcedInclusionTxV1 = {
        tx_id: raw.transactionId.toString("hex"),
        submitted_source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
        verdict: entry.value.verdict,
      };
      if (
        Data.to(entry.value, SDK.ForcedInclusionTxV1) !==
        Data.to(expected, SDK.ForcedInclusionTxV1)
      ) {
        throw new Error(
          "source or commitment does not match canonical preimage",
        );
      }
    } catch (cause) {
      throw transitionTraceError(
        "malformedPayload",
        `Failed to authenticate forced_transactions[${index.toString()}] against its canonical preimage.`,
        cause,
      );
    }
    authenticated.push({
      ...entry,
      fullTransactionCbor: canonicalTransactionCbor,
    });
  }
  if (preimagesByKey.size !== entries.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "forced_transaction_preimages contains entries without a matching forced transaction source.",
    );
  }
  return authenticated;
};

export const buildSourceEvents = ({
  withdrawals,
  forcedTransactions,
  transactions,
  deposits,
}: {
  readonly withdrawals: TransitionTraceReconstruction["withdrawals"];
  readonly forcedTransactions: TransitionTraceReconstruction["forcedTransactions"];
  readonly transactions: TransitionTraceReconstruction["transactions"];
  readonly deposits: TransitionTraceReconstruction["deposits"];
}): readonly SourceEventRecord[] => {
  const records: SourceEventRecord[] = [];
  for (const entry of withdrawals) {
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: entry.key },
    };
    records.push({
      phase: "Withdrawal",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  for (const entry of forcedTransactions) {
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: { tx_order_id: entry.key },
    };
    records.push({
      phase: "ForcedTransaction",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  for (const entry of transactions) {
    const eventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: entry.txId },
    };
    records.push({
      phase: "L2Transaction",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  for (const entry of deposits) {
    const eventKey: SDK.EventKey = {
      DepositEventKey: { deposit_id: entry.key },
    };
    records.push({
      phase: "Deposit",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  return records;
};

export const headerCounts = (header: SDK.Header): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

export const headerRoots = (header: SDK.Header): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

export const rootMismatches = (
  expected: PayloadRootSet,
  actual: PayloadRootSet,
): readonly string[] =>
  [
    expected.utxosRoot === actual.utxosRoot ? null : "utxos_root",
    expected.withdrawalsRoot === actual.withdrawalsRoot
      ? null
      : "withdrawals_root",
    expected.forcedTransactionsRoot === actual.forcedTransactionsRoot
      ? null
      : "forced_transactions_root",
    expected.transactionsRoot === actual.transactionsRoot
      ? null
      : "transactions_root",
    expected.depositsRoot === actual.depositsRoot ? null : "deposits_root",
    expected.transitionTraceRoot === actual.transitionTraceRoot
      ? null
      : "transition_trace_root",
    expected.eventToStepRoot === actual.eventToStepRoot
      ? null
      : "event_to_step_root",
    expected.validationTracesRoot === actual.validationTracesRoot
      ? null
      : "validation_traces_root",
  ].filter((field): field is string => field !== null);

export const ensureUniqueSourceFingerprints = (
  sourceEvents: readonly SourceEventRecord[],
): ReadonlyMap<string, SourceEventRecord> => {
  const map = new Map<string, SourceEventRecord>();
  for (const sourceEvent of sourceEvents) {
    const existing = map.get(sourceEvent.fingerprint);
    if (existing !== undefined) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `Payload contains duplicate source event key ${sourceEvent.fingerprint}.`,
      );
    }
    map.set(sourceEvent.fingerprint, sourceEvent);
  }
  return map;
};
