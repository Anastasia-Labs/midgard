import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { reconstructMidgardTransaction } from "@al-ft/midgard-core/consensus-validation";
import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { forcedVerdictForRejection } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  type MempoolLedgerDB,
  WithdrawalsDB,
} from "../database/index.js";
import * as Ledger from "../database/utils/ledger.js";
import { ForeignBlockVerificationError } from "../mpf/verified-block-import.js";
import type { ImportedBlockReplayContext } from "../mpf/verified-block-import.replay-events.js";
import { historyIncarnationEntry } from "../l1-event-history-entries.js";
import { NodeConfig } from "../services/index.js";
import {
  assertForeignEventCensusMatchesPayload,
  foreignEventCensus,
} from "./commit-block-header.foreign-event-census.js";

export type ForeignEventMemberships = Readonly<{
  deposits: readonly {
    readonly id: Buffer;
    readonly entry: MempoolLedgerDB.DepositEntry;
  }[];
  forcedTransactions: readonly Buffer[];
  withdrawals: readonly {
    readonly id: Buffer;
    readonly classification: Effect.Effect.Success<
      ReturnType<typeof SDK.classifyWithdrawalFromLedger>
    >;
  }[];
}>;

/** Resolve immutable canonical source incarnations and forced admissions.
 * DA-provided event bytes never authorize an L1 projection. */
export const foreignEventMaterial = (
  headerHash: string,
  header: SDK.Header,
  payload: SDK.DaPayload,
) =>
  Effect.gen(function* () {
    const missing = (detail: string) =>
      new ForeignBlockVerificationError({
        foreignHeaderHash: headerHash,
        reason: "missing",
        detail,
      });
    const invalid = (detail: string) =>
      new ForeignBlockVerificationError({
        foreignHeaderHash: headerHash,
        reason: "invalid",
        detail,
      });
    const config = yield* NodeConfig;
    const census = yield* foreignEventCensus(headerHash, header);
    yield* Effect.try({
      try: () => assertForeignEventCensusMatchesPayload(census, payload),
      catch: (cause) => invalid(String(cause)),
    });
    const depositMembers: {
      id: Buffer;
      entry: MempoolLedgerDB.DepositEntry;
    }[] = [];
    const withdrawalMembers: {
      id: Buffer;
      classification: Effect.Effect.Success<
        ReturnType<typeof SDK.classifyWithdrawalFromLedger>
      >;
    }[] = [];
    const memberships: ForeignEventMemberships = {
      deposits: depositMembers,
      forcedTransactions: census.forced.map((value) =>
        Buffer.from(value.key, "hex"),
      ),
      withdrawals: withdrawalMembers,
    };
    const deposits = new Map<
      string,
      { key: string; output: Buffer; value: string }
    >();
    const withdrawals = new Map<string, WithdrawalsDB.Entry>();
    const forced = new Map(census.forced.map((value) => [value.key, value]));
    for (const incarnation of census.deposits) {
      const projected = yield* historyIncarnationEntry(
        incarnation,
        config.NETWORK,
      );
      if (projected.kind !== "deposit")
        return yield* Effect.fail(invalid("Deposit source kind differs"));
      const entry = yield* DepositsDB.toLedgerEntry(projected.entry);
      depositMembers.push({
        id: Buffer.from(incarnation.event.idCbor, "hex"),
        entry: yield* DepositsDB.toMempoolLedgerEntry(projected.entry),
      });
      deposits.set(incarnation.event.idCbor, {
        key: entry[Ledger.Columns.OUTREF].toString("hex"),
        output: entry[Ledger.Columns.OUTPUT],
        value: projected.entry[DepositsDB.Columns.INFO].toString("hex"),
      });
    }
    for (const incarnation of census.withdrawals) {
      const projected = yield* historyIncarnationEntry(
        incarnation,
        config.NETWORK,
      );
      if (projected.kind !== "withdrawal")
        return yield* Effect.fail(invalid("Withdrawal source kind differs"));
      withdrawals.set(incarnation.event.idCbor, projected.entry);
    }
    const replayUserEvent: ImportedBlockReplayContext["replayUserEvent"] = ({
      step,
      source,
      ledger,
    }) =>
      Effect.gen(function* () {
        if (step.phase === "Deposit") {
          const entry = deposits.get(source[0]);
          if (entry === undefined)
            return yield* Effect.fail(
              missing("foreign deposit projection unavailable"),
            );
          if (entry.value !== source[1])
            return yield* Effect.fail(
              invalid("foreign deposit differs from authenticated event"),
            );
          return [{ key: entry.key, output: entry.output }];
        }
        const entry = withdrawals.get(source[0]);
        if (entry === undefined)
          return yield* Effect.fail(
            missing("foreign withdrawal classification unavailable"),
          );
        const outRef = yield* WithdrawalsDB.toLedgerOutRef(entry);
        const classification = yield* SDK.classifyWithdrawalFromLedger({
          l2Owner: entry[WithdrawalsDB.Columns.L2_OWNER].toString("hex"),
          l2ValueCbor: entry[WithdrawalsDB.Columns.L2_VALUE].toString("hex"),
          eventInfoCbor:
            entry[WithdrawalsDB.Columns.RAW_EVENT_INFO].toString("hex"),
          ledgerOutRef: outRef,
          ledgerOutput: ledger.get(outRef.toString("hex")) ?? null,
        });
        if (classification.settlementEventInfo.toString("hex") !== source[1])
          return yield* Effect.fail(
            invalid("foreign withdrawal verdict differs from replay"),
          );
        withdrawalMembers.push({
          id: Buffer.from(source[0], "hex"),
          classification,
        });
        return classification.shouldDeleteLedgerUtxo
          ? [{ key: outRef.toString("hex"), output: null }]
          : [];
      });
    const verifyForcedSource: ImportedBlockReplayContext["verifyForcedSource"] =
      ({ source, canonicalTransactionCbor, rejection }) =>
        Effect.gen(function* () {
          const entry = forced.get(source[0]);
          if (entry === undefined)
            return yield* Effect.fail(
              missing("foreign forced source unavailable"),
            );
          // The source-owned NFT/datum census authenticates the compact
          // commitments. DA supplies only preimages opened against those exact
          // commitments, independently of any mutable ingestion/cache row.
          yield* Effect.try({
            try: () => {
              const payload = SDK.decodeTxOrderDatumCbor(
                Buffer.from(entry.datumCbor, "hex"),
              ).event.tx;
              const material = deriveMidgardForcedTxFaultEvidenceMaterial(
                canonicalTransactionCbor,
              );
              const reconstructed = reconstructMidgardTransaction({
                sourceKind: "forced",
                transactionId: Buffer.from(payload.tx_id, "hex"),
                transactionCommitment: Buffer.from(
                  payload.transaction_commitment,
                  "hex",
                ),
                source: {
                  compactCbor: Buffer.from(
                    payload.submitted_source.compact_cbor,
                    "hex",
                  ),
                  witnessSetCompactCbor: Buffer.from(
                    payload.submitted_source.witness_set_compact_cbor,
                    "hex",
                  ),
                  fieldPreimageLengthsCbor: Buffer.from(
                    payload.submitted_source.field_preimage_lengths_cbor,
                    "hex",
                  ),
                },
                fieldPreimages: material.fieldPreimages,
              });
              if (!reconstructed.equals(canonicalTransactionCbor))
                throw new Error(
                  "Forced transaction differs from canonical source commitments",
                );
            },
            catch: (cause) =>
              invalid(
                `Foreign forced source differs from authenticated order: ${String(cause)}`,
              ),
          });
          const encoded =
            yield* ForcedTransactionsDB.encodeForcedInclusionValueV1({
              nativeTxCbor: canonicalTransactionCbor,
              consensusProfile: MIDGARD_CONSENSUS_PROFILE,
              verdict:
                rejection === undefined
                  ? "ForcedTxValid"
                  : forcedVerdictForRejection(rejection),
            });
          if (encoded.value.toString("hex") !== source[1])
            return yield* Effect.fail(
              invalid("foreign forced verdict/source differs from replay"),
            );
        });
    return { replayUserEvent, verifyForcedSource, memberships };
  });
