import "./utils.js";

import {
  findLedgerSignatureVerifierUnavailable,
  ledgerEd25519SignatureReady,
} from "@al-ft/midgard-core";
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  midgardAddressFromText,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect, Logger } from "effect";
import { afterEach, beforeAll, describe, expect, it, vi } from "vitest";

import * as LedgerUtils from "../src/database/utils/ledger.js";
import * as WithdrawalsDB from "../src/database/withdrawals.js";
import {
  applyCommitWorkerReadiness,
  COMMIT_WORKER_FAILED,
  COMMIT_WORKER_SOURCE,
} from "../src/fibers/block-commitment.worker-readiness.js";
import { type MidgardMpf, processMpfs } from "../src/mpf/index.js";
import { ledgerEntryToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../src/mpf/ledger-hydration.js";
import { Globals } from "../src/services/globals.js";
import { activeLivenessReasons } from "../src/services/liveness-halt.js";
import { captureCommitWorkerFailure } from "../src/workers/commit-block-header.run-commit-block-header-worker-program.js";
import { insertWithdrawals } from "./helpers/event-rows.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

/**
 * This node's own block, built while its Ed25519 verifier faults on a due
 * withdrawal: the build fails, naming the verifier's fault, before any
 * withdrawal is classified or written. The worker reports the failure and
 * the parent raises `commit_worker_failed`, so the next commit tick retries
 * it (bounded: `commit-worker.local-fault-bound.test.ts`). Nothing is judged
 * and nothing is dropped; once the fault clears the same withdrawal is
 * classified and projected.
 */
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

// The native owner is replaced by the in-process trace builder over the same
// base ledger; the withdrawal classification stays real.
const base = vi.hoisted(() => ({
  entries: [] as { readonly key: Buffer; readonly value: Buffer }[],
}));
vi.mock("../src/mpf/transition-trace.js", async () => {
  const actual = await vi.importActual<
    typeof import("../src/mpf/transition-trace.js")
  >("../src/mpf/transition-trace.js");
  const { MidgardMpf: Mpf } = await vi.importActual<
    typeof import("../src/mpf/store.midgard-mpf.js")
  >("../src/mpf/store.midgard-mpf.js");
  return {
    ...actual,
    buildNativeTransitionTraceResult: ({
      nativeMpf,
      ...rest
    }: Parameters<typeof actual.buildNativeTransitionTraceResult>[0]) =>
      Effect.gen(function* () {
        const ledgerMpf = yield* Mpf.createScratchFromList(
          "own-withdrawal-fault-ledger",
          base.entries,
        );
        const result = yield* actual.buildTransitionTraceResult({
          ledgerMpf,
          ...rest,
        });
        nativeMpf.candidateRoot = result.finalUtxosRoot;
        nativeMpf.eventLog = Buffer.alloc(0);
        nativeMpf.eventLogDigest = "00".repeat(32);
        nativeMpf.eventRoots = [];
        return result;
      }),
  };
});

beforeAll(() => ledgerEd25519SignatureReady);

afterEach(() => {
  injected.fault = undefined;
});

const wallet = walletFromSeed(
  "test test test test test test test test test test test junk",
  { network: "Preprod" },
);
const privateKey = CML.PrivateKey.from_bech32(wallet.paymentKey);
const T0 = Date.parse("2026-01-01T00:00:00.000Z");
const at = (seconds: number) => new Date(T0 + seconds * 1_000);
const hex = (buffer: Buffer) => buffer.toString("hex");
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
const BASE = [U];

const order = async (): Promise<WithdrawalsDB.Entry> => {
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
  const info: SDK.WithdrawalInfo = {
    body,
    signature: SDK.signWithdrawalBody(privateKey, body),
    validity: "WithdrawalIsValid",
  };
  const id = { transactionId: "01".repeat(32), outputIndex: 0n };
  return {
    [WithdrawalsDB.Columns.ID]: Buffer.from(
      Data.to(id, SDK.OutputReference),
      "hex",
    ),
    raw_event_info: Buffer.from(Data.to(info, SDK.WithdrawalInfo), "hex"),
    settlement_event_info: null,
    inclusion_time: at(1),
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

const run = <A, E>(effect: Effect.Effect<A, E, unknown>) =>
  Effect.runPromise(
    provideDatabaseLayers(effect).pipe(
      Effect.provide(Logger.remove(Logger.defaultLogger)),
    ) as Effect.Effect<A, E, never>,
  );

const rowsOf = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql`SELECT status, validity, settlement_event_info FROM withdrawal_utxos`;
});

/** Builds this node's block over `BASE` with the due withdrawal in it. */
const build = (baseRoot: string) =>
  Effect.gen(function* () {
    const { MidgardMpf } = yield* Effect.promise(
      () => import("../src/mpf/store.midgard-mpf.js"),
    );
    const transactionsMpf = yield* MidgardMpf.createScratch(
      "own-withdrawal-fault-transactions",
    );
    const produced = yield* Effect.either(
      processMpfs(transactionsMpf as unknown as MidgardMpf, [], {
        currentBlockStartTime: at(0),
        fixedBlockEndTime: at(10),
        initialLedgerEntries: BASE,
        nativeMpf: {
          handle: { baseRoot },
          ownerBinarySha256: "00".repeat(32),
        } as never,
      }),
    );
    return { produced, rows: yield* rowsOf };
  });

describe("this node's block under a verifier fault on a due withdrawal", () => {
  it("fails the build with the local fault, judges and drops nothing, then builds once it clears", async () => {
    base.entries = BASE.map(ledgerEntryToInsertBatchOp);
    const baseRoot = await Effect.runPromise(
      computeLedgerMpfRootFromLedgerEntries(BASE),
    );
    const withdrawal = await order();
    await run(
      Effect.zipRight(resetApplicationTables, insertWithdrawals([withdrawal])),
    );

    const fault = new Error("injected verifier fault during block building");
    injected.fault = fault;
    const faulted = await run(build(baseRoot));
    expect(faulted.produced._tag).toBe("Left");
    if (faulted.produced._tag !== "Left") return;
    const failure = faulted.produced.left;
    expect(failure._tag).toBe("DatabaseError");
    expect(failure.message).toBe(
      `Ed25519 verification is unavailable: ${fault.message}`,
    );
    // The typed fault is the cause, carrying the verifier's own error.
    const local = findLedgerSignatureVerifierUnavailable(failure.cause);
    expect(local).toBe(failure.cause);
    expect((local as { cause?: unknown } | undefined)?.cause).toBe(fault);
    // The worker reports it as a failed attempt on a local fault, which the
    // parent raises as `commit_worker_failed` and the next commit tick
    // retries, within a bound.
    const output = await Effect.runPromise(
      captureCommitWorkerFailure(Effect.fail(failure)),
    );
    expect(output).toMatchObject({
      type: "FailureOutput",
      localFault: "verifier_unavailable",
    });
    expect(output.type === "FailureOutput" && output.error).toContain(
      fault.message,
    );
    const reasons = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;
        yield* applyCommitWorkerReadiness(globals, output);
        return (yield* activeLivenessReasons(globals)).map(
          ({ source, reason }) => ({ source, reason }),
        );
      }).pipe(Effect.provide(Globals.Default)),
    );
    expect(reasons).toEqual([
      { source: COMMIT_WORKER_SOURCE, reason: COMMIT_WORKER_FAILED },
    ]);
    // No verdict and no drop: the order is still due, unclassified.
    expect(faulted.rows).toEqual([
      {
        status: WithdrawalsDB.Status.Awaiting,
        validity: null,
        settlement_event_info: null,
      },
    ]);

    injected.fault = undefined;
    const retried = await run(build(baseRoot));
    expect(retried.produced._tag).toBe("Right");
    if (retried.produced._tag !== "Right") return;
    expect(
      retried.produced.right.includedWithdrawalEntries.map((row) => [
        hex(row[WithdrawalsDB.Columns.ID]),
        row[WithdrawalsDB.Columns.VALIDITY],
      ]),
    ).toEqual([
      [hex(withdrawal[WithdrawalsDB.Columns.ID]), "WithdrawalIsValid"],
    ]);
    expect(retried.rows.map((row) => [row.validity, row.status])).toEqual([
      ["WithdrawalIsValid", WithdrawalsDB.Status.Projected],
    ]);
    expect(retried.produced.right.ledgerDelta.spent.map(hex)).toEqual([
      hex(U[LedgerUtils.Columns.OUTREF]),
    ]);
  }, 120_000);
});
