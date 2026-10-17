import { createHash } from "node:crypto";

import { MidgardCekProgramMaterialMissingRootError } from "@al-ft/midgard-core/cek-proof";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type BlockReplayContext,
  type CommittedTransaction,
  isReplayLocalFault,
  locally,
  replayBlockEvents,
} from "@al-ft/midgard-fault-proofs/block-replay";
import * as SDK from "@al-ft/midgard-sdk";
import {
  decodeMidgardLedgerTxFromCanonicalCbor,
  type MidgardCommittedScriptRun,
  midgardOutRefToCborHex,
} from "@al-ft/midgard-validation";
import { Data, Effect } from "effect";

import * as Ledger from "../database/utils/ledger.js";
import { importCheckCommittedRunBytes } from "../import-check/kill.js";
import {
  computeDaPayloadRoots,
  headerCounts,
  headerRoots,
  rootMismatches,
} from "../workers/commit-block-header/da-payload.compute-da-payload-roots.js";
import { computeLedgerMpfRootFromLedgerEntries } from "./ledger-hydration.js";

/** The shared block replay's context over the node's ledger rows. */
export type ImportedBlockReplayContext = Omit<
  BlockReplayContext,
  "parentEntries" | "ledgerRoot" | "committedVerdicts"
> & {
  readonly parentEntries: readonly Ledger.MinimalEntry[];
};

const ledgerRows = (
  entries: readonly { readonly outRef: Buffer; readonly output: Buffer }[],
): Ledger.MinimalEntry[] =>
  entries.map(({ outRef, output }) => ({
    [Ledger.Columns.OUTREF]: outRef,
    [Ledger.Columns.OUTPUT]: output,
  }));

/**
 * - `missing`: material the block needs is not available yet;
 * - `invalid`: the block (its header, DA payload or replay) is at fault;
 * - `local_fault`: this node's own computation failed where the block's
 *   input cannot make it fail (hashing, the payload roots after their keys
 *   checked, the import's bookkeeping, a runtime fault named at its
 *   origin); retried;
 * - `incomplete`: the import failed for a reason it cannot pin on the block
 *   (a ledger root, or an unclassified typed replay failure); retried.
 */
export type ForeignBlockVerificationReason =
  | "missing"
  | "invalid"
  | "local_fault"
  | "incomplete";

export class ForeignBlockVerificationError extends Data.TaggedError(
  "ForeignBlockVerificationError",
)<{
  readonly foreignHeaderHash: string;
  readonly reason: ForeignBlockVerificationReason;
  readonly detail: string;
}> {}

/**
 * One imported L2 transaction with script runs, which the import did not
 * evaluate: the transaction is applied by its committed verdict, and its
 * runs are checked after the import, rebuilt from the transaction, its
 * sidecar and the outputs it resolves. `position` is the transaction's order
 * among the block's transactions with script runs. The rest summarizes its
 * runs: their count, declared cpu, bytes (script, sidecar and context per
 * run, the context estimated as the transaction and the outputs it
 * resolves: none is built at import) and distinct scripts (sha256).
 */
export type ImportScriptCheck = Readonly<{
  transactionId: Buffer;
  position: number;
  transactionCbor: Buffer;
  programMaterialSidecarCbor: Buffer;
  runs: number;
  cpu: bigint;
  runBytes: number;
  scriptHashes: readonly Buffer[];
}>;

/** What a check's runs are rebuilt with: the block's validation context. */
export type ImportCheckContext = Readonly<{
  blockTimeMs: number;
  blockSlot: bigint;
  expectedNetworkId: bigint;
  minFeeA: bigint;
  minFeeB: bigint;
  l1SlotConfig: L1SlotConfig;
}>;

/**
 * A block's checks: each transaction once, and each output a check spends
 * or references once, by out-ref (CBOR hex). Nothing grows with a
 * transaction's runs.
 */
export type ImportBlockChecks = Readonly<{
  context: ImportCheckContext;
  checks: readonly ImportScriptCheck[];
  outputs: ReadonlyMap<string, Buffer>;
}>;

/** The block's checks, built one transaction at a time as the replay goes. */
const blockChecksCollector = (context: ImportCheckContext) => {
  const checks: ImportScriptCheck[] = [];
  const outputs = new Map<string, Buffer>();
  let runs = 0;
  let cpu = 0n;
  let runBytes = 0;
  const scripts = new Map<string, Buffer>();
  return {
    committedVerdicts: {
      // The bookkeeping is this node's own: a throw from it is local. The
      // transaction's decoding reads the block's bytes, so a throw from it
      // keeps the block's attribution.
      onScriptRun: (run: MidgardCommittedScriptRun) =>
        locally("import check bookkeeping", () => {
          runs += 1;
          cpu += run.executionBudget?.cpu ?? 0n;
          // Its context is counted once the transaction is in.
          runBytes += importCheckCommittedRunBytes(run, 0);
          const hash = createHash("sha256").update(run.scriptBytes).digest();
          scripts.set(hash.toString("hex"), hash);
        }),
      onTransaction: (transaction: CommittedTransaction) => {
        if (runs > 0) {
          const tx = decodeMidgardLedgerTxFromCanonicalCbor(
            transaction.canonicalTransactionCbor,
          );
          const keys = [...tx.spendInputs, ...tx.referenceInputs].map(
            midgardOutRefToCborHex,
          );
          locally("import check bookkeeping", () => {
            // No run's context is built at import: each is estimated as
            // the transaction and the outputs it resolves.
            let contextBytes = transaction.canonicalTransactionCbor.byteLength;
            for (const key of keys) {
              const output = transaction.ledger.get(key);
              if (output === undefined) continue;
              outputs.set(key, output);
              contextBytes += output.byteLength;
            }
            runBytes += runs * contextBytes;
          });
          checks.push({
            transactionId: transaction.transactionId,
            position: checks.length,
            transactionCbor: transaction.canonicalTransactionCbor,
            programMaterialSidecarCbor: transaction.programMaterialSidecarCbor,
            runs,
            cpu,
            runBytes,
            scriptHashes: [...scripts.values()],
          });
        }
        runs = 0;
        cpu = 0n;
        runBytes = 0;
        scripts.clear();
      },
    },
    result: (): ImportBlockChecks => ({ context, checks, outputs }),
  };
};

/** A caller callback's failure, passed through the import unchanged. */
class CallbackFailure {
  constructor(readonly cause: unknown) {}
}

/** This node's own computation failed: never the block's fault. */
class LocalFailure {
  constructor(
    readonly what: string,
    readonly cause: unknown,
  ) {}
}

/** The first payload section that names one key twice, if any: such a
 * section has no root, which is the block's fault. Keys compare as the
 * root computation reads them. */
const repeatedKeySection = (body: SDK.DaPayload["block_body"]) =>
  (
    [
      ["withdrawals", body.withdrawals],
      ["forced_transactions", body.forced_transactions],
      ["transactions", body.transactions],
      ["deposits", body.deposits],
      ["transition_trace", body.transition_trace],
      ["event_to_step", body.event_to_step],
      ["validation_traces", body.validation_traces],
    ] as const
  ).find(
    ([, entries]) =>
      new Set(entries.map(([key]) => Buffer.from(key, "hex").toString("hex")))
        .size !== entries.length,
  )?.[0];

const passThrough = <A>(effect: Effect.Effect<A, unknown>) =>
  Effect.mapError(effect, (cause) => new CallbackFailure(cause));

/** Full import authentication. A caller must supply the exact canonical parent
 * and deployment observation; matching ledger roots or DA signatures do not
 * substitute for replay. No successful result is cached across observations.
 *
 * Every L2 transaction is applied by its committed verdict: the import runs
 * no script, and returns the transactions with script runs as
 * `scriptChecks` for the import checker. Every other check runs in full.
 *
 * Failures: the caller's callbacks' pass through unchanged (their verdicts
 * are the caller's). A `ReplayLocalFault` defect (named where it arose) and
 * this import's own computations are `local_fault`; a missing CEK root is
 * `missing`; any other defect is `invalid`: it may come from reading the
 * block's or its events' bytes, so it is never taken for a local fault.
 * Any other typed failure is `incomplete`. */
export const verifyAndImportBlock = (
  input: ImportedBlockReplayContext & {
    readonly headerHash: string;
    readonly parentHeaderHash: string;
    readonly parentUtxosRoot: string;
    readonly payload?: SDK.DaPayload;
  },
) =>
  Effect.gen(function* () {
    const fail = (reason: ForeignBlockVerificationReason, detail: string) =>
      Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: input.headerHash,
          reason,
          detail,
        }),
      );
    if (
      input.header.prevHeaderHash !== input.parentHeaderHash ||
      input.header.prevUtxosRoot !== input.parentUtxosRoot
    ) {
      return yield* fail(
        "invalid",
        "foreign header does not extend the verified parent",
      );
    }
    if (
      input.header.protocolVersion !== 1n ||
      input.header.expectedNetworkId !== input.expectedNetworkId ||
      input.header.minFeeA !== input.minFeeA ||
      input.header.minFeeB !== input.minFeeB ||
      input.header.blockSlot !== input.blockSlot
    )
      return yield* fail(
        "invalid",
        "foreign header validation context differs from deployment",
      );
    if (input.payload === undefined)
      return yield* fail("missing", "foreign DA payload is unavailable");
    const payload = input.payload;
    // A header that does not encode is the observation's fault (a throw);
    // a hash that fails on encoded bytes is this node's.
    const hashOf = (header: SDK.Header) =>
      SDK.hashBlockHeader(header).pipe(
        Effect.mapError((cause) => new LocalFailure("header hashing", cause)),
      );
    if (
      (yield* hashOf(input.header)) !== input.headerHash ||
      payload.block_body.header_hash !== input.headerHash ||
      (yield* hashOf(payload.block_body.header)) !== input.headerHash ||
      payload.version !== SDK.DA_PAYLOAD_VERSION
    )
      return yield* fail(
        "invalid",
        "foreign DA/header identity differs from the canonical observation",
      );
    const baseRoot = yield* computeLedgerMpfRootFromLedgerEntries(
      input.parentEntries,
    );
    if (baseRoot !== input.parentUtxosRoot)
      return yield* fail(
        "invalid",
        "foreign replay parent snapshot root mismatch",
      );
    // Roots the payload's own entries cannot form (a key named twice) are
    // its fault; once its keys are distinct, a failure is this node's.
    const repeated = repeatedKeySection(payload.block_body);
    if (repeated !== undefined)
      return yield* fail(
        "invalid",
        `foreign DA payload names a ${repeated} key twice`,
      );
    const roots = yield* computeDaPayloadRoots(payload).pipe(
      Effect.mapError((cause) => new LocalFailure("payload roots", cause)),
    );
    const mismatches = [...rootMismatches(headerRoots(input.header), roots)];
    const body = payload.block_body;
    const memberCounts: SDK.DaPayloadCounts = {
      depositCount: BigInt(body.deposits.length),
      withdrawalCount: BigInt(body.withdrawals.length),
      forcedTransactionCount: BigInt(body.forced_transactions.length),
      l2TransactionCount: BigInt(body.transactions.length),
      totalEventCount: BigInt(
        body.deposits.length +
          body.withdrawals.length +
          body.forced_transactions.length +
          body.transactions.length,
      ),
      transitionStepCount: BigInt(body.transition_trace.length),
      validationTraceCount: BigInt(body.validation_traces.length),
    };
    for (const key of Object.keys(
      memberCounts,
    ) as (keyof SDK.DaPayloadCounts)[]) {
      if (
        memberCounts[key] !== headerCounts(input.header)[key] ||
        body.counts[key] !== memberCounts[key]
      )
        mismatches.push(key);
    }
    if (body.event_to_step.length !== body.transition_trace.length)
      mismatches.push("event_to_step_count");
    if (body.transition_trace.length !== Number(memberCounts.totalEventCount))
      mismatches.push("transition_step_count");
    if (mismatches.length > 0)
      return yield* fail(
        "invalid",
        `foreign DA commitment mismatch: ${mismatches.join(",")}`,
      );
    const collector = blockChecksCollector({
      blockTimeMs: input.blockTimeMs,
      blockSlot: input.blockSlot,
      expectedNetworkId: input.expectedNetworkId,
      minFeeA: input.minFeeA,
      minFeeB: input.minFeeB,
      l1SlotConfig: input.l1SlotConfig,
    });
    const entries = ledgerRows(
      yield* replayBlockEvents({
        ...input,
        parentEntries: input.parentEntries.map((entry) => ({
          outRef: entry[Ledger.Columns.OUTREF],
          output: entry[Ledger.Columns.OUTPUT],
        })),
        ledgerRoot: (ledger) =>
          computeLedgerMpfRootFromLedgerEntries(ledgerRows(ledger)),
        payload,
        replayUserEvent: (event) => passThrough(input.replayUserEvent(event)),
        verifyForcedSource: (source) =>
          passThrough(input.verifyForcedSource(source)),
        committedVerdicts: collector.committedVerdicts,
      }),
    );
    const replayRoot = yield* computeLedgerMpfRootFromLedgerEntries(entries);
    if (replayRoot !== input.header.utxosRoot)
      return yield* fail("invalid", "foreign replay UTxO root mismatch");
    // Replay compares every transition, mapping and validation descriptor by its
    // canonical bytes, independently of the payload's internally consistent roots.
    return {
      headerHash: input.headerHash,
      root: replayRoot,
      entries,
      scriptChecks: collector.result(),
    };
  }).pipe(
    Effect.catchAllDefect((cause) =>
      Effect.fail(
        new ForeignBlockVerificationError({
          foreignHeaderHash: input.headerHash,
          reason: isReplayLocalFault(cause)
            ? "local_fault"
            : cause instanceof MidgardCekProgramMaterialMissingRootError
              ? "missing"
              : // A `BlockReplayFault`, or a throw not named local.
                "invalid",
          detail: String(cause),
        }),
      ),
    ),
    Effect.catchAll((cause: unknown) =>
      cause instanceof CallbackFailure ||
      cause instanceof ForeignBlockVerificationError
        ? Effect.fail(cause instanceof CallbackFailure ? cause.cause : cause)
        : Effect.fail(
            new ForeignBlockVerificationError({
              foreignHeaderHash: input.headerHash,
              reason:
                cause instanceof LocalFailure
                  ? "local_fault"
                  : cause instanceof MidgardCekProgramMaterialMissingRootError
                    ? "missing"
                    : "incomplete",
              detail:
                cause instanceof LocalFailure
                  ? `${cause.what} failed locally: ${String(cause.cause)}`
                  : String(cause),
            }),
          ),
    ),
  );
