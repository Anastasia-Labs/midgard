import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Fiber } from "effect";

import { MpfError } from "./errors.js";
import { MidgardMpf } from "./store.js";
import { type TransitionTraceSourceEvent } from "./trace-events.js";
import {
  buildTransactionsSourceRoot,
  countedRootFromEncodedEntries,
  type NativeMpfBuildContext,
} from "./transition-trace.apply-trace-ledger-ops-to-mpf.js";
import {
  buildNativeTransitionTraceResult,
  type NativeRootProbeResult,
} from "./transition-trace.build-native-transition-trace-result.js";
import { type MpfInsertBatchOp } from "./types.js";

/**
 * Runs the complete root-building portion of the Architecture G production
 * commit path without the database-specific transaction classification phase.
 * Inputs are the exact encoded entries that processMpfs applies after decoding.
 * This is intentionally colocated with processMpfs so benchmarks cannot replace
 * production root algorithms with harness-specific approximations.
 */
export const buildNativeRootProbe = ({
  nativeMpf,
  sourceEvents,
  transactionOps,
  deposits = [],
  withdrawals = [],
  forcedTransactions = [],
}: {
  readonly nativeMpf: NativeMpfBuildContext;
  readonly sourceEvents: readonly TransitionTraceSourceEvent[];
  readonly transactionOps: readonly MpfInsertBatchOp[];
  readonly deposits?: readonly MpfInsertBatchOp[];
  readonly withdrawals?: readonly MpfInsertBatchOp[];
  readonly forcedTransactions?: readonly MpfInsertBatchOp[];
}): Effect.Effect<NativeRootProbeResult, MpfError> =>
  Effect.acquireUseRelease(
    MidgardMpf.createScratch("architecture-g-production-probe-transactions"),
    (transactionsMpf) =>
      Effect.gen(function* () {
        const startedAt = performance.now();
        const timedTransactionSourceRoot = yield* Effect.gen(function* () {
          const phaseStartedAt = performance.now();
          const root = yield* buildTransactionsSourceRoot(transactionOps);
          return { root, durationMs: performance.now() - phaseStartedAt };
        }).pipe(Effect.fork);
        const timedTransactionMpfApply = yield* Effect.gen(function* () {
          const phaseStartedAt = performance.now();
          yield* transactionsMpf.applyBatch(transactionOps);
          const rawTxRoot = yield* transactionsMpf.rootHex();
          return {
            rawTxRoot,
            durationMs: performance.now() - phaseStartedAt,
          };
        }).pipe(Effect.fork);
        const transitionTraceStartedAt = performance.now();
        const transitionTraceBuild = yield* buildNativeTransitionTraceResult({
          nativeMpf,
          sourceEvents,
          withdrawalCount: withdrawals.length,
          forcedTransactionCount: forcedTransactions.length,
          l2TransactionCount: transactionOps.length,
          depositCount: deposits.length,
        });
        const transitionTraceBuildMs =
          performance.now() - transitionTraceStartedAt;
        const [timedTxRoot, timedRawTxRoot] = yield* Effect.all(
          [
            Fiber.join(timedTransactionSourceRoot),
            Fiber.join(timedTransactionMpfApply),
          ],
          { concurrency: "unbounded" },
        );

        const auxiliaryRootsStartedAt = performance.now();
        const eventRoot = (
          domain: SDK.RootDomain,
          entries: readonly MpfInsertBatchOp[],
        ): Effect.Effect<string, MpfError> =>
          entries.length === 0
            ? Effect.succeed(SDK.EMPTY_MERKLE_TREE_ROOT)
            : countedRootFromEncodedEntries(domain, entries);
        const [depositsRoot, withdrawalsRoot, forcedTransactionsRoot] =
          yield* Effect.all(
            [
              eventRoot(SDK.ROOT_DOMAINS.deposits, deposits),
              eventRoot(SDK.ROOT_DOMAINS.withdrawals, withdrawals),
              eventRoot(
                SDK.ROOT_DOMAINS.forcedTransactionsV1,
                forcedTransactions,
              ),
            ],
            { concurrency: "unbounded" },
          );
        const auxiliaryRootsMs = performance.now() - auxiliaryRootsStartedAt;
        const utxoRoot = nativeMpf.candidateRoot;
        if (utxoRoot === undefined) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "Architecture G production probe",
              new Error("Native owner did not return a candidate UTxO root"),
            ),
          );
        }
        if (transitionTraceBuild.finalUtxosRoot !== utxoRoot) {
          return yield* Effect.fail(
            MpfError.rootBuild(
              "Architecture G production probe",
              new Error(
                `Transition trace final root mismatch: trace=${transitionTraceBuild.finalUtxosRoot},candidate=${utxoRoot}`,
              ),
            ),
          );
        }
        return {
          utxoRoot,
          rawTxRoot: timedRawTxRoot.rawTxRoot,
          txRoot: timedTxRoot.root,
          transitionTraceRoot: transitionTraceBuild.transitionTraceRoot,
          eventToStepRoot: transitionTraceBuild.eventToStepRoot,
          depositsRoot,
          withdrawalsRoot,
          forcedTransactionsRoot,
          transitionRoots: transitionTraceBuild.transitionTraceMembers.map(
            (member) => ({
              pre: member.value.pre_utxos_root,
              post: member.value.post_utxos_root,
            }),
          ),
          durationMs: performance.now() - startedAt,
          phaseMs: {
            transactionSourceRoot: timedTxRoot.durationMs,
            transitionTraceBuild: transitionTraceBuildMs,
            transactionMpfApply: timedRawTxRoot.durationMs,
            auxiliaryRoots: auxiliaryRootsMs,
          },
          transitionTraceBuild,
        };
      }),
    (transactionsMpf) => transactionsMpf.close().pipe(Effect.orDie),
  );
