/**
 * This node's commit attempts over a due withdrawal row it cannot classify
 * from the row's own bytes: its event info, l2_value or l2_outref, or the
 * ledger output it names, does not decode. Each attempt classifies the row as
 * the block build does, fails at the worker's boundary as a local fault on
 * that row, and reaches the parent as its output. The parent counts it in the
 * one local-fault bound: `LOCAL_FAULT_RUN_LIMIT` attempts failing on a local
 * fault without a successful commit between them exit the node non-zero under
 * `commit_worker_withdrawal_unclassifiable`, naming the row, and a restarted
 * node meets the same bound again. A successful commit or a failure on a
 * different row starts the count afresh. An honest row is classified on every
 * attempt and never counted.
 */
import { ledgerEd25519SignatureReady } from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Deferred, Effect, Exit, Logger } from "effect";
import { beforeAll, describe, expect, it } from "vitest";

import { withStartupHttpServer } from "../src/commands/listen.startup-http.js";
import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import {
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_SOURCE,
} from "../src/fibers/block-commitment.worker-readiness.js";
import { Globals } from "../src/services/globals.js";
import { LOCAL_FAULT_RUN_LIMIT } from "../src/services/l1-follower.coalesced-runner.js";
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
const EXIT_REASON = "commit_worker_withdrawal_unclassifiable";
/** `FailureOutput.localFault` of a row this node cannot classify. */
const ROW_FAULT = "withdrawal_unclassifiable";

beforeAll(() => ledgerEd25519SignatureReady());

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
const ledgerAt = (output: Buffer): LedgerUtils.Entry => ({
  [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, 0xaa),
  [LedgerUtils.Columns.OUTREF]: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0xaa),
    outputIndex: 0,
  }),
  [LedgerUtils.Columns.OUTPUT]: output,
  [LedgerUtils.Columns.ADDRESS]: wallet.address,
});
const U = ledgerAt(OUTPUT);
const NOT_AN_OUTPUT = Buffer.from("ff", "hex");

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
const order = async (
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

const idOf = (entry: WithdrawalsDB.Entry) =>
  entry[WithdrawalsDB.Columns.ID].toString("hex");

/** A due withdrawal row whose event info does not decode. */
const unreadableRow = (id = 0x01) =>
  order(id, { raw_event_info: Buffer.from("ff", "hex") });

/** A fresh process's globals. */
const freshProcess = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));

/**
 * One commit attempt over `entry` against the ledger `base`: the worker
 * classifies it as the block build does and reports its outcome, which the
 * parent takes; `passed` is what the worker reports once classification
 * passes. Returns the parent's output and the validities the attempt derived.
 */
const attempt = async (
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

const attempts = async (
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
const take = (globals: Globals, output: WorkerOutput) =>
  takeCommitWorkerOutput(globals, output, 16);

/** A failed attempt that is not a local fault (an L1 submission failure). */
const NOT_LOCAL: WorkerOutput = {
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
const SUCCESSES: readonly WorkerOutput[] = [
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
const NOT_SUCCESSES: readonly WorkerOutput[] = [
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

const exhausted = (globals: Globals) =>
  Effect.runPromise(Deferred.isDone(globals.TRANSIENT_EXHAUSTION));

const reasons = (globals: Globals) =>
  Effect.runPromise(activeLivenessReasons(globals)).then((active) =>
    active.map(({ source, reason }) => ({ source, reason })),
  );

/** The serving node's exit once `globals` signalled exhaustion, as
 * `runListen` races it, and the lines it logged. */
const servingExit = async (globals: Globals) => {
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

describe("commit attempts over a withdrawal row this node cannot classify", () => {
  it.each([
    {
      part: "event info",
      unreadable: { raw_event_info: Buffer.from("ff", "hex") },
      base: U,
    },
    {
      part: "l2_value",
      unreadable: { l2_value: Buffer.from("ff", "hex") },
      base: U,
    },
    {
      part: "l2_outref",
      unreadable: { l2_outref: Buffer.from("ff", "hex") },
      base: U,
    },
    { part: "ledger output", unreadable: {}, base: ledgerAt(NOT_AN_OUTPUT) },
  ])(
    "fails the attempt as a local fault on the row whose $part does not decode",
    async ({ unreadable, base }) => {
      const globals = await freshProcess();
      const entry = await order(0x01, unreadable);
      const { output, validities } = await attempt(globals, entry, base);
      expect(output).toMatchObject({
        type: "FailureOutput",
        localFault: ROW_FAULT,
        localFaultSubject: idOf(entry),
      });
      expect(validities).toEqual([]);
      expect(await reasons(globals)).toEqual([
        { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
      ]);
    },
  );

  it("exits non-zero naming the row once its attempts reach the bound, and a fresh process meets it again", async () => {
    const entry = await unreadableRow();
    for (const label of ["first process", "restarted process"]) {
      const globals = await freshProcess();
      const outputs = await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(
        outputs.every((output) => output?.type === "FailureOutput"),
        label,
      ).toBe(true);
      expect(await exhausted(globals), label).toBe(false);
      expect(await reasons(globals), label).toEqual([
        { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
      ]);
      const last = await attempt(globals, entry);
      expect(last.validities, label).toEqual([]);
      expect(await exhausted(globals), label).toBe(true);

      const { spent, messages } = await servingExit(globals);
      expect(spent, label).toMatchObject({
        source: COMMIT_WORKER_SOURCE,
        reason: EXIT_REASON,
      });
      expect(spent?.message, label).toContain(`withdrawal ${idOf(entry)}`);
      expect(messages, label).toEqual([
        `node_transient_budget_exhausted stage=serving source=${COMMIT_WORKER_SOURCE} reason=${EXIT_REASON}; a transient failure outlived its bound; the node exits non-zero`,
      ]);
    }
  }, 60_000);

  it("counts afresh from a successful commit", async () => {
    const entry = await unreadableRow();
    for (const success of SUCCESSES) {
      const label = success.type;
      const globals = await freshProcess();
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(take(globals, success), label).toEqual(success);
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(await exhausted(globals), label).toBe(false);
      await attempt(globals, entry);
      expect(await exhausted(globals), label).toBe(true);
    }
  }, 60_000);

  it("counts afresh from a failure on a different row, and exits naming that row", async () => {
    const first = await unreadableRow(0x01);
    const second = await unreadableRow(0x02);
    const globals = await freshProcess();
    await attempts(globals, first, LOCAL_FAULT_RUN_LIMIT - 1);
    await attempts(globals, second, LOCAL_FAULT_RUN_LIMIT - 1);
    expect(await exhausted(globals)).toBe(false);
    await attempt(globals, second);
    expect(await exhausted(globals)).toBe(true);
    const { spent } = await servingExit(globals);
    expect(spent?.reason).toBe(EXIT_REASON);
    expect(spent?.message).toContain(`withdrawal ${idOf(second)}`);
    expect(spent?.message).not.toContain(idOf(first));
  }, 60_000);

  it("exits at the bound when its attempts are interleaved with waits and failures that are not local faults", async () => {
    const entry = await unreadableRow();
    const globals = await freshProcess();
    for (let index = 0; index < LOCAL_FAULT_RUN_LIMIT - 1; index += 1) {
      take(globals, NOT_SUCCESSES[index % NOT_SUCCESSES.length]!);
      await attempt(globals, entry);
    }
    // Every output was taken at least once, and none started the count afresh.
    for (const output of NOT_SUCCESSES) take(globals, output);
    expect(await exhausted(globals)).toBe(false);
    await attempt(globals, entry);
    expect(await exhausted(globals)).toBe(true);
  }, 60_000);
});

describe("commit attempts over an honest withdrawal row", () => {
  it("classifies it on every attempt and never counts it", async () => {
    const entry = await order();
    const globals = await freshProcess();
    for (let index = 0; index < 2 * LOCAL_FAULT_RUN_LIMIT; index += 1) {
      const { output, validities } = await attempt(globals, entry);
      expect(output).toEqual({ type: "NothingToCommitOutput" });
      expect(validities).toEqual(["WithdrawalIsValid"]);
    }
    expect(await exhausted(globals)).toBe(false);
    expect(await reasons(globals)).toEqual([]);
  }, 60_000);

  it("leaves an attempt over it that fails without a local fault retried, never counted", async () => {
    const globals = await freshProcess();
    for (let index = 0; index < 2 * LOCAL_FAULT_RUN_LIMIT; index += 1)
      take(globals, NOT_LOCAL);
    expect(await exhausted(globals)).toBe(false);
    expect(await reasons(globals)).toEqual([
      { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
    ]);
  });
});
