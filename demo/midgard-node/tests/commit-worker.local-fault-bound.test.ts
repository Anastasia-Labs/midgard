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
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

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

/**
 * This node's commit attempts while its Ed25519 verifier faults on a due
 * withdrawal. Each attempt classifies the withdrawal as the block build does,
 * fails at the worker's boundary and reaches the parent as its output. A
 * fault is retried; `LOCAL_FAULT_RUN_LIMIT` attempts in a row failing on it
 * exit the node non-zero, and a restarted node meets the same bound again.
 * A failure that is not the fault is not counted.
 */
/** The reason the node exits under, from source `commit_worker`. */
const EXIT_REASON = "commit_worker_verifier_unavailable";

const injected = vi.hoisted(() => ({ fault: undefined as Error | undefined }));

vi.mock(
  "../../midgard-core/node_modules/libsodium-wrappers-sumo",
  async (importOriginal) => {
    const actual = (await importOriginal<{ default: object }>()).default;
    return {
      default: new Proxy(actual, {
        get: (target, key, receiver) =>
          key === "crypto_sign_verify_detached" && injected.fault !== undefined
            ? () => {
                throw injected.fault;
              }
            : (Reflect.get(target, key, receiver) as unknown),
      }),
    };
  },
);

beforeAll(() => ledgerEd25519SignatureReady);

afterEach(() => {
  injected.fault = undefined;
});

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const LOVELACE = 7_000_000n;
const U: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, 0xaa),
  [LedgerUtils.Columns.OUTREF]: encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, 0xaa),
    outputIndex: 0,
  }),
  [LedgerUtils.Columns.OUTPUT]: encodeMidgardTxOutput({
    address: midgardAddressFromText(wallet.address),
    value: { lovelace: LOVELACE, assets: new Map() },
  }),
  [LedgerUtils.Columns.ADDRESS]: wallet.address,
};

/** A due withdrawal of U, honestly signed; `info` replaces its event info. */
const order = async (info?: Buffer): Promise<WithdrawalsDB.Entry> => {
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
        { transactionId: "01".repeat(32), outputIndex: 0n },
        SDK.OutputReference,
      ),
      "hex",
    ),
    raw_event_info:
      info ?? Buffer.from(Data.to(signed, SDK.WithdrawalInfo), "hex"),
    settlement_event_info: null,
    inclusion_time: new Date(0),
    withdrawal_l1_tx_hash: Buffer.alloc(32, 0x01),
    withdrawal_l1_output_index: 0,
    asset_name: Buffer.alloc(32, 0x01),
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
  };
};

/** A fresh process's globals. */
const freshProcess = () =>
  Effect.runPromise(Effect.provide(Globals, Globals.Default));

/**
 * One commit attempt over `entry`: the worker classifies it as the block
 * build does and reports its outcome, which the parent takes. Returns the
 * parent's output and the validities the attempt derived.
 */
const attempt = async (globals: Globals, entry: WithdrawalsDB.Entry) => {
  const validities: string[] = [];
  const output = await Effect.runPromise(
    captureCommitWorkerFailure(
      indexSelectedLedgerOutputs([U]).pipe(
        Effect.flatMap((baseOutputs) =>
          classifyWithdrawalsInOrder({ entries: [entry], baseOutputs }),
        ),
        Effect.map((classified): WorkerOutput => {
          validities.push(...classified.map(({ validity }) => validity));
          return { type: "NothingToCommitOutput" };
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
  return { exit, messages: messages.flat() };
};

describe("commit attempts under a verifier fault", () => {
  it("retries a fault below the bound and succeeds once it clears", async () => {
    injected.fault = new Error("injected verifier fault");
    const globals = await freshProcess();
    const entry = await order();
    const outputs = await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
    expect(outputs.every((output) => output?.type === "FailureOutput")).toBe(
      true,
    );
    expect(await exhausted(globals)).toBe(false);
    expect(await reasons(globals)).toEqual([
      { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
    ]);

    injected.fault = undefined;
    const cleared = await attempt(globals, entry);
    expect(cleared.output).toEqual({ type: "NothingToCommitOutput" });
    expect(cleared.validities).toEqual(["WithdrawalIsValid"]);
    // The success ended the run: the bound counts afresh from it.
    injected.fault = new Error("injected verifier fault, again");
    await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
    expect(await exhausted(globals)).toBe(false);
    await attempt(globals, entry);
    expect(await exhausted(globals)).toBe(true);
  }, 60_000);

  it("exits non-zero once the fault persists for the bound, and a fresh process meets it again", async () => {
    injected.fault = new Error("injected persistent verifier fault");
    const entry = await order();
    for (const label of ["first process", "restarted process"]) {
      const globals = await freshProcess();
      await attempts(globals, entry, LOCAL_FAULT_RUN_LIMIT - 1);
      expect(await exhausted(globals), label).toBe(false);
      await attempt(globals, entry);
      expect(await exhausted(globals), label).toBe(true);

      const { exit, messages } = await servingExit(globals);
      expect(
        Exit.isFailure(exit) && findTransientBudgetExhausted(exit.cause),
        label,
      ).toMatchObject({
        source: COMMIT_WORKER_SOURCE,
        reason: EXIT_REASON,
      });
      expect(messages, label).toEqual([
        `node_transient_budget_exhausted stage=serving source=${COMMIT_WORKER_SOURCE} reason=${EXIT_REASON}; a transient failure outlived its bound; the node exits non-zero`,
      ]);
    }
  }, 60_000);

  it("leaves a failure that is not the fault as it was: retried, never counted", async () => {
    injected.fault = new Error("injected verifier fault");
    const globals = await freshProcess();
    // Event info that is not Data fails the attempt before any signature.
    const entry = await order(Buffer.from("ff", "hex"));
    const outputs = await attempts(globals, entry, 2 * LOCAL_FAULT_RUN_LIMIT);
    for (const output of outputs) {
      expect(output?.type).toBe("FailureOutput");
      expect(output).not.toHaveProperty("localFault");
    }
    expect(await exhausted(globals)).toBe(false);
    expect(await reasons(globals)).toEqual([
      { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
    ]);
  }, 60_000);
});
