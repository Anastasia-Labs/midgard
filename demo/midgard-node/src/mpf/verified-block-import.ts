import { createHash } from "node:crypto";

import { findLedgerSignatureVerifierUnavailable } from "@al-ft/midgard-core";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
import {
  type BlockReplayContext,
  type CommittedTransaction,
  isReplayLocalFault,
  locally,
  replayBlockEvents,
} from "@al-ft/midgard-fault-proofs/block-replay";
import type { Ledger } from "@al-ft/midgard-fault-proofs/ledger-store";
import * as SDK from "@al-ft/midgard-sdk/core";
import * as SDKDa from "@al-ft/midgard-sdk/da";
import type { MidgardCommittedScriptRun } from "@al-ft/midgard-validation/types";
import { Data, Effect } from "effect";

import type * as LedgerRows from "../database/utils/ledger.js";
import { importCheckCommittedRunBytes } from "../import-check/kill.js";
import {
  computeDaPayloadRoots,
  headerCommittedCounts,
  headerCommittedRoots,
  headerCounts,
  headerRoots,
  rootMismatches,
} from "../workers/commit-block-header/da-payload.compute-da-payload-roots.js";

/** The shared block replay's context, on the parent's ledger. */
export type ImportedBlockReplayContext = Omit<
  BlockReplayContext,
  "committedVerdicts" | "onReplayedEvent"
>;

/** A block's net delta on its parent's ledger: the out-refs it spent, and
 * the outputs at out-refs the parent does not hold, each in out-ref order.
 * An out-ref names one output (its transaction id hashes the body that
 * carries it), so a touched out-ref the parent holds is either spent or
 * unchanged, and in neither set. The two never share an out-ref. */
export type LedgerDelta = Readonly<{
  spent: readonly Buffer[];
  produced: readonly LedgerRows.MinimalEntry[];
}>;

/**
 * - `missing`: the block's DA payload is not available yet;
 * - `invalid`: the block (its header, DA payload or replay) is at fault;
 * - `local_fault`: this node's own computation failed where the block's
 *   input cannot make it fail (hashing, the payload roots after their keys
 *   checked, the import's bookkeeping, a read of the parent's ledger, a
 *   runtime fault named at its origin); retried;
 * - `incomplete`: the import failed for a reason it cannot pin on the block
 *   (a ledger root, or an unclassified typed replay failure); retried.
 * - `verifier_unavailable`: this node's Ed25519 verifier could not answer
 *   (`LedgerSignatureVerifierUnavailableError`), a local fault and never the
 *   block's; retried on the driver's backoff, within a bound.
 * - `replay_held`: the block's event replay failed locally (a
 *   `ReplayLocalFault` marked `held`), which input can reach; held on the
 *   block and retried on a backoff for as long as it recurs, never bounded
 *   by an exit (`landed-blocks/replay-held.ts`).
 */
export type ForeignBlockVerificationReason =
  | "missing"
  | "invalid"
  | "local_fault"
  | "replay_held"
  | "incomplete"
  | "verifier_unavailable";

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
 * runs are checked after the import, rebuilt from the transaction and the
 * outputs it resolves. `position` is the transaction's order among the
 * block's transactions with script runs. The rest summarizes its runs: their
 * count, declared cpu, bytes (script and context per run, the context
 * estimated as the transaction and the outputs it resolves: none is built at
 * import) and distinct scripts (sha256).
 */
export type ImportScriptCheck = Readonly<{
  transactionId: Buffer;
  position: number;
  transactionCbor: Buffer;
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
      // The bookkeeping is this node's own: a throw from it is local.
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
          locally("import check bookkeeping", () => {
            // No run's context is built at import: each is estimated as
            // the transaction and the outputs it resolves.
            let contextBytes = transaction.canonicalTransactionCbor.byteLength;
            for (const [key, output] of transaction.resolvedOutputs) {
              outputs.set(key, output);
              contextBytes += output.byteLength;
            }
            runBytes += runs * contextBytes;
          });
          checks.push({
            transactionId: transaction.transactionId,
            position: checks.length,
            transactionCbor: transaction.canonicalTransactionCbor,
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
const repeatedKeySection = (body: SDKDa.DaPayload["block_body"]) =>
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

/** `verifier_unavailable` when `cause` is the Ed25519 verifier's local fault. */
const verifierUnavailable = (foreignHeaderHash: string, cause: unknown) => {
  const fault = findLedgerSignatureVerifierUnavailable(cause);
  return fault === undefined
    ? undefined
    : new ForeignBlockVerificationError({
        foreignHeaderHash,
        reason: "verifier_unavailable",
        detail: `${fault.name}: ${fault.message}`,
      });
};

const byHex = (left: string, right: string) =>
  left < right ? -1 : left > right ? 1 : 0;

/**
 * The block's net delta on `parent` from each touched out-ref's first change
 * (`firstRemoved`: whether it removed the out-ref) and last output
 * (`null`: removed). A first removal names an out-ref `parent` held, so one
 * removed first and last is spent unread; every other touched out-ref is
 * read from `parent`, for whether it held it. Nothing
 * the block did not touch is read.
 */
const netDelta = (
  parent: Ledger,
  firstRemoved: ReadonlyMap<string, boolean>,
  last: ReadonlyMap<string, Buffer | null>,
) =>
  Effect.gen(function* () {
    const toRead = [...firstRemoved]
      .filter(([key, removed]) => !removed || last.get(key) !== null)
      .map(([key]) => Buffer.from(key, "hex"));
    const held = yield* Effect.tryPromise({
      try: () => parent.read(toRead),
      catch: (cause) => new LocalFailure("parent ledger read", cause),
    });
    const spent: string[] = [];
    const produced: [string, Buffer][] = [];
    for (const [key, removed] of firstRemoved) {
      const output = last.get(key) ?? null;
      const before = held.get(key)?.output;
      if (output === null) {
        if (removed || before !== undefined) spent.push(key);
      } else if (before === undefined) produced.push([key, output]);
    }
    return {
      spent: spent.sort(byHex).map((key) => Buffer.from(key, "hex")),
      produced: produced
        .sort(([left], [right]) => byHex(left, right))
        .map(([key, output]) => ({
          outref: Buffer.from(key, "hex"),
          output: Buffer.from(output),
        })),
    } satisfies LedgerDelta;
  });

/** Full import authentication. A caller must supply the exact canonical parent
 * and deployment observation; matching ledger roots or DA signatures do not
 * substitute for replay. No successful result is cached across observations.
 * The parent's ledger is this node's own (its root is the parent's): it is
 * read, never checked. The result is the ledger after the block, which is
 * not persisted, and the block's net delta on the parent.
 *
 * Every L2 transaction is applied by its committed verdict: the import runs
 * no script, and returns the transactions with script runs as
 * `scriptChecks` for the import checker. Every other check runs in full.
 *
 * Failures: a fault of the Ed25519 verifier, wherever it is raised (a
 * callback's included), is `verifier_unavailable`, checked first; otherwise
 * the caller's callbacks' pass through unchanged (their verdicts are the
 * caller's). A `ReplayLocalFault` defect (named where it arose, a read of
 * the parent's ledger among them) and this import's own computations are
 * `local_fault`, one marked `held` (the event replay's) `replay_held`; any
 * other defect is `invalid`: it may come from reading the block's or its
 * events' bytes, so it is never taken for a local fault.
 * Any other typed failure is `incomplete`. */
export const verifyAndImportBlock = (
  input: ImportedBlockReplayContext & {
    readonly headerHash: string;
    readonly parentHeaderHash: string;
    readonly payload?: SDKDa.DaPayload;
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
      input.header.prevUtxosRoot !== input.parent.root
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
      payload.version !== SDKDa.DA_PAYLOAD_VERSION
    )
      return yield* fail(
        "invalid",
        "foreign DA/header identity differs from the canonical observation",
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
    const mismatches = [
      ...rootMismatches(headerRoots(input.header), headerCommittedRoots(roots)),
    ];
    const body = payload.block_body;
    const memberCounts: SDKDa.DaPayloadCounts = {
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
    ) as (keyof SDKDa.DaPayloadCounts)[]) {
      if (
        headerCommittedCounts(memberCounts)[key] !==
          headerCounts(input.header)[key] ||
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
    // Each touched out-ref's first change (a removal names an out-ref the
    // parent held) and its last output (`null`: removed).
    const firstRemoved = new Map<string, boolean>();
    const last = new Map<string, Buffer | null>();
    const replayed = yield* replayBlockEvents({
      ...input,
      payload,
      replayUserEvent: (event) => passThrough(input.replayUserEvent(event)),
      verifyForcedSource: (source) =>
        passThrough(input.verifyForcedSource(source)),
      committedVerdicts: collector.committedVerdicts,
      onReplayedEvent: ({ mutations }) => {
        for (const { key, output } of mutations) {
          if (!firstRemoved.has(key)) firstRemoved.set(key, output === null);
          last.set(key, output);
        }
      },
    });
    if (replayed.root !== input.header.utxosRoot)
      return yield* fail("invalid", "foreign replay UTxO root mismatch");
    const delta = yield* netDelta(input.parent, firstRemoved, last);
    // Replay compares every transition and mapping by its canonical bytes,
    // independently of the payload's internally consistent roots; validation
    // descriptors, which the header does not commit, only by verdict.
    return {
      headerHash: input.headerHash,
      after: replayed,
      ...delta,
      scriptChecks: collector.result(),
    };
  }).pipe(
    Effect.catchAllDefect((cause) =>
      Effect.fail(
        verifierUnavailable(input.headerHash, cause) ??
          new ForeignBlockVerificationError({
            foreignHeaderHash: input.headerHash,
            reason: isReplayLocalFault(cause)
              ? cause.held === true
                ? "replay_held"
                : "local_fault"
              : // A `BlockReplayFault`, or a throw not named local.
                "invalid",
            detail: String(cause),
          }),
      ),
    ),
    Effect.catchAll((cause: unknown) =>
      Effect.fail(
        verifierUnavailable(
          input.headerHash,
          cause instanceof CallbackFailure ? cause.cause : cause,
        ) ??
          (cause instanceof CallbackFailure
            ? cause.cause
            : cause instanceof ForeignBlockVerificationError
              ? cause
              : new ForeignBlockVerificationError({
                  foreignHeaderHash: input.headerHash,
                  reason:
                    cause instanceof LocalFailure
                      ? "local_fault"
                      : "incomplete",
                  detail:
                    cause instanceof LocalFailure
                      ? `${cause.what} failed locally: ${String(cause.cause)}`
                      : String(cause),
                })),
      ),
    ),
  );
