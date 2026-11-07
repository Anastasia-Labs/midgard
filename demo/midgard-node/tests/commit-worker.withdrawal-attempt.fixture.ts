/**
 * Commit attempts over a due withdrawal row, as the block build classifies
 * it, taken by the parent as the worker's output, and the serving node's
 * exit once the local-fault bound is reached.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Deferred, Effect, Exit, Logger } from "effect";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { findTransientBudgetExhausted } from "../src/services/transient-exhaustion.js";
import { captureCommitWorkerFailure } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import { takeCommitWorkerOutput } from "../src/workers/commit-worker-message.js";
import type { WorkerOutput } from "../src/workers/utils/commit-block-header.js";
import {
  classifyWithdrawalsInOrder,
  indexSelectedLedgerOutputs,
} from "../src/workers/utils/mpf/withdrawal-classification.js";

/** The reason the node exits under, from source `commit_worker`. */
export const EXIT_REASON = "commit_worker_withdrawal_unclassifiable";
/** `FailureOutput.localFault` of a row this node cannot classify. */
export const ROW_FAULT = "withdrawal_unclassifiable";

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const LOVELACE = 7_000_000n;
const OUTPUT = encodeMidgardTxOutput({
  address: midgardAddressFromText(wallet.address),
  value: { lovelace: LOVELACE, assets: new Map() },
});
export const ledgerAt = (output: Buffer): LedgerUtils.Entry => ({
  [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, 0xaa),
  [LedgerUtils.Columns.OUTREF]: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0xaa),
    outputIndex: 0,
  }),
  [LedgerUtils.Columns.OUTPUT]: output,
  [LedgerUtils.Columns.ADDRESS]: wallet.address,
});
export const U = ledgerAt(OUTPUT);
export const NOT_AN_OUTPUT = Buffer.from("ff", "hex");

type Unreadable = Partial<
  Pick<
    WithdrawalsDB.Entry,
    | WithdrawalsDB.Columns.RAW_EVENT_INFO
    | WithdrawalsDB.Columns.L2_VALUE
    | WithdrawalsDB.Columns.L2_OUTREF
  >
>;

/** A due withdrawal of U, honestly signed, with event id byte `id`;
 * `unreadable` replaces the named columns with bytes that do not decode. */
export const order = async (
  id = 0x01,
  unreadable: Unreadable = {},
): Promise<WithdrawalsDB.Entry> => {
  const value: SDK.Value = new Map([["", new Map([["", LOVELACE]])]]);
  const body: SDK.WithdrawalBody = {
    l2_outref: { transactionId: "aa".repeat(32), outputIndex: 0n },
    l2_owner: privateKey.to_public().hash().to_hex(),
    l2_value: value,
    l1_address: await Effect.runPromise(
      SDK.addressDataFromBech32(wallet.address),
    ),
    l1_datum: "NoDatum",
  };
  const signed: SDK.WithdrawalInfo = {
    body,
    signature: SDK.signWithdrawalBody(privateKey, body),
    validity: "WithdrawalIsValid",
  };
  return {
    [WithdrawalsDB.Columns.ID]: Buffer.from(
      Data.to(
        {
          transactionId: id.toString(16).padStart(2, "0").repeat(32),
          outputIndex: 0n,
        },
        SDK.OutputReference,
      ),
      "hex",
    ),
    raw_event_info: Buffer.from(Data.to(signed, SDK.WithdrawalInfo), "hex"),
    settlement_event_info: null,
    inclusion_time: new Date(0),
    withdrawal_l1_tx_hash: Buffer.alloc(32, id),
    withdrawal_l1_output_index: 0,
    asset_name: Buffer.alloc(32, id),
    l2_outref: Buffer.from(Data.to(body.l2_outref, SDK.OutputReference), "hex"),
    l2_owner: Buffer.from(body.l2_owner, "hex"),
    l2_value: Buffer.from(Data.to(value, SDK.Value), "hex"),
    l1_address: Buffer.from(Data.to(body.l1_address, SDK.AddressData), "hex"),
    l1_datum: Buffer.from(Data.to(body.l1_datum, SDK.CardanoDatum), "hex"),
    refund_address: Buffer.alloc(0),
    refund_datum: Buffer.alloc(0),
    validity: null,
    validity_detail: {},
    classification_revision: 0,
    reopened_from_header_hash: null,
    projected_header_hash: null,
    status: WithdrawalsDB.Status.Awaiting,
    ...unreadable,
  };
};

export const idOf = (entry: WithdrawalsDB.Entry) =>
  entry[WithdrawalsDB.Columns.ID].toString("hex");

/** A due withdrawal row whose event info does not decode. */
export const unreadableRow = (id = 0x01) =>
  order(id, { raw_event_info: Buffer.from("ff", "hex") });

/** A fresh process's globals. */
export const freshProcess = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));

/**
 * One commit attempt over `entry` against the ledger `base`: the worker
 * classifies it as the block build does and reports its outcome, which the
 * parent takes; `passed` is what the worker reports once classification
 * passes. Returns the parent's output and the validities the attempt derived.
 */
export const attempt = async (
  globals: Globals,
  entry: WithdrawalsDB.Entry,
  base: LedgerUtils.Entry = U,
  passed: WorkerOutput = { type: "NothingToCommitOutput" },
) => {
  const validities: string[] = [];
  const output = await Effect.runPromise(
    captureCommitWorkerFailure(
      indexSelectedLedgerOutputs([base]).pipe(
        Effect.flatMap((baseOutputs) =>
          classifyWithdrawalsInOrder({ entries: [entry], baseOutputs }),
        ),
        Effect.map((classified): WorkerOutput => {
          validities.push(...classified.map(({ validity }) => validity));
          return passed;
        }),
      ),
    ),
  );
  return { output: takeCommitWorkerOutput(globals, output, 16), validities };
};

export const attempts = async (
  globals: Globals,
  entry: WithdrawalsDB.Entry,
  count: number,
) => {
  const outputs: (WorkerOutput | undefined)[] = [];
  for (let index = 0; index < count; index += 1)
    outputs.push((await attempt(globals, entry)).output);
  return outputs;
};

/** The parent takes `output` from the worker. */
export const take = (globals: Globals, output: WorkerOutput) =>
  takeCommitWorkerOutput(globals, output, 16);

/** A failed attempt that is not a local fault (an L1 submission failure). */
export const NOT_LOCAL: WorkerOutput = {
  type: "FailureOutput",
  error: "Block commitment worker failure: L1 submission refused",
};

const submitted = {
  submittedTxHash: "11".repeat(32),
  txSize: 1,
  mempoolTxsCount: 0,
  sizeOfBlocksTxs: 0,
  blockEndTimeMs: 0,
};

/** Outputs that prove a commit went through. */
export const SUCCESSES: readonly WorkerOutput[] = [
  {
    type: "SuccessfulSubmissionOutput",
    ...submitted,
    mempoolLedgerDeletedOutRefHexes: [],
  },
  {
    type: "SubmittedAwaitingConfirmationOutput",
    ...submitted,
    submittedHeaderHash: "22".repeat(28),
    submittedUtxosRoot: "33".repeat(32),
  },
  {
    type: "SuccessfulLocalFinalizationRecoveryOutput",
    finalizedHeaderHash: "22".repeat(28),
    mempoolTxsCount: 0,
    sizeOfBlocksTxs: 0,
    mempoolLedgerDeletedOutRefHexes: [],
  },
];

/** Outputs that wait or do nothing, and a failure that is not a local
 * fault: none proves a commit went through. */
export const NOT_SUCCESSES: readonly WorkerOutput[] = [
  {
    type: "AwaitingCommitBaseOutput",
    baseHeaderHash: "44".repeat(28),
    detail: "foreign tail not applied yet",
  },
  { type: "NothingToCommitOutput" },
  {
    type: "AwaitingNextCommitWindowOutput",
    heldHeaderHash: "55".repeat(28),
    detail: "abandoned signed journal holds the header",
  },
  {
    type: "SkippedSubmissionOutput",
    mempoolTxsCount: 0,
    sizeOfProcessedTxs: 0,
  },
  NOT_LOCAL,
];

export const exhausted = (globals: Globals) =>
  Effect.runPromise(Deferred.isDone(globals.TRANSIENT_EXHAUSTION));

export const reasons = (globals: Globals) =>
  Effect.runPromise(activeLivenessReasons(globals)).then((active) =>
    active.map(({ source, reason }) => ({ source, reason })),
  );

/** The serving node's exit once `globals` signalled exhaustion, as
 * `runListen` races it, and the lines it logged. */
export const servingExit = async (globals: Globals) => {
  const messages: unknown[] = [];
  const exit = await Effect.runPromiseExit(
    withStartupHttpServer(0, (startup) =>
      Effect.gen(function* () {
        yield* startup.publish(
          Effect.succeed(HttpServerResponse.unsafeJson({ ready: true })),
        );
        return yield* Effect.raceFirst(
          Effect.never,
          Deferred.await(globals.TRANSIENT_EXHAUSTION),
        );
      }),
    ).pipe(
      Effect.provide(
        Logger.replace(
          Logger.defaultLogger,
          Logger.make(({ message }) => {
            messages.push(message);
          }),
        ),
      ),
    ),
  );
  const spent = Exit.isFailure(exit)
    ? findTransientBudgetExhausted(exit.cause)
    : undefined;
  return { spent, messages: messages.flat() };
};
