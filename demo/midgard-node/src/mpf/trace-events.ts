/**
 * Transition-trace source events and their event keys.
 */

import * as SDK from "@al-ft/midgard-sdk";
import {
  type RejectCode,
  type ValidationMachineLedgerEntry,
  type ValidationMachineLedgerMutationStep,
} from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as DepositsDB from "../database/deposits.js";
import * as ForcedTransactionsDB from "../database/forcedTransactions.js";
import * as WithdrawalsDB from "../database/withdrawals.js";
import { MpfError } from "./errors.js";
import { encodeTransitionEventKeyCbor } from "./transition-cbor.js";
import { type MpfBatchOp } from "./types.js";

export type TransitionTraceSourceEvent = {
  readonly eventKey: SDK.EventKey;
  readonly phase: SDK.TransitionPhase;
  readonly ledgerOps: readonly MpfBatchOp[];
};

export type RetainedTransitionTraceMember = {
  readonly stepIndex: bigint;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.TransitionStep;
};

export type RetainedEventToStepMember = {
  readonly eventKey: SDK.EventKey;
  readonly keyCbor: Buffer;
  readonly valueCbor: Buffer;
  readonly value: SDK.EventToStepValue;
};

export type ValidationTraceTransactionInput = {
  readonly eventKey: SDK.EventKey;
  readonly transactionId: Buffer;
  readonly canonicalTransactionCbor: Buffer;
  readonly programMaterialSidecarCbor: Buffer;
  readonly sourceKind: "normal" | "forced";
  readonly priorUtxosRoot: string;
  readonly postUtxosRoot: string;
  readonly ledgerOps: readonly MpfBatchOp[];
  readonly ledgerWitnessEntries: readonly ValidationMachineLedgerEntry[];
  readonly ledgerMutationSteps: readonly ValidationMachineLedgerMutationStep[];
  readonly verdict: "accepted" | "rejected";
  readonly rejectionCode: RejectCode | null;
};

const outputReferenceFromCbor = (
  cbor: Buffer,
  label: string,
): Effect.Effect<SDK.OutputReference, MpfError> =>
  Effect.try({
    try: () =>
      LucidData.from(
        cbor.toString("hex"),
        SDK.OutputReference,
      ) as SDK.OutputReference,
    catch: (cause) =>
      MpfError.rootBuild(
        "transition trace event key",
        new Error(`Failed to decode ${label} as OutputReference CBOR`, {
          cause,
        }),
      ),
  });

export const withdrawalTraceEventKey = (
  entry: WithdrawalsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[WithdrawalsDB.Columns.ID],
    "withdrawal event id",
  ).pipe(
    Effect.map((withdrawalId) => ({
      WithdrawalEventKey: { withdrawal_id: withdrawalId },
    })),
  );

export const forcedTransactionTraceEventKey = (
  entry: ForcedTransactionsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[ForcedTransactionsDB.Columns.TX_ORDER_ID],
    "forced transaction tx_order_id",
  ).pipe(
    Effect.map((txOrderId) => ({
      ForcedTransactionEventKey: { tx_order_id: txOrderId },
    })),
  );

export const depositTraceEventKey = (
  entry: DepositsDB.Entry,
): Effect.Effect<SDK.EventKey, MpfError> =>
  outputReferenceFromCbor(
    entry[DepositsDB.Columns.ID],
    "deposit event id",
  ).pipe(
    Effect.map((depositId) => ({
      DepositEventKey: { deposit_id: depositId },
    })),
  );

export const l2TransactionTraceEventKey = (txHash: Buffer): SDK.EventKey => ({
  L2TransactionEventKey: { tx_id: txHash.toString("hex") },
});

export const eventKeyCbor = (
  eventKey: SDK.EventKey,
): Effect.Effect<Buffer, MpfError> =>
  Effect.try({
    try: () => encodeTransitionEventKeyCbor(eventKey),
    catch: (cause) =>
      MpfError.rootBuild(
        "transition trace",
        new Error("Failed to encode transition event key", { cause }),
      ),
  });

export const eventKeyFingerprint = (
  eventKey: SDK.EventKey,
): Effect.Effect<string, MpfError> =>
  eventKeyCbor(eventKey).pipe(Effect.map((encoded) => encoded.toString("hex")));
