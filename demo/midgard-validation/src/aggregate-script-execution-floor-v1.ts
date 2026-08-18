import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";

/**
 * C52 "Aggregate script-execution floor" (GOAL_SPEC.md §8.3, line 901):
 * "A Cardano-capable transaction receives at least the target snapshot's
 * aggregate memory/steps across bounded proof transactions."
 *
 * The target snapshot's per-transaction execution ceiling is
 * `MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits` /
 * `...CpuUnits` (16,500,000 memory / 10,000,000,000 CPU — the §3.1.10
 * merged Cardano-mainnet-vs-target-network capability floor; see
 * capability-parity-v1.ts's `maxTxExecutionMemoryUnits`, pinned to
 * `"16500000"` in capability-parity-v1.test.ts:49).
 *
 * §3.3 "Execution fit" requires every individual proof transaction to stay
 * at or below the deployment's measured protocol limits with at least a 20%
 * reserve (docs/consensus-profile-v1.md §10; the same reserve is applied at
 * complete-item-proof-fit-emulator-v1.test.ts:84-89 as
 * `RESERVED_MEMORY_UNITS`/`RESERVED_CPU_UNITS`). One proof transaction can
 * therefore carry at most 80% of the target snapshot's ceiling per
 * transaction, which is strictly less than the ceiling itself
 * (16,500,000 * 0.8 = 13,200,000 < 16,500,000; 10,000,000,000 * 0.8 =
 * 8,000,000,000 < 10,000,000,000) — so no single proof transaction can ever
 * certify a Cardano-capable transaction's complete execution budget.
 *
 * `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`
 * ("Accepted proof decomposition tradeoff") resolves that gap: "It is
 * explicitly acceptable for a proof ... to require more Cardano
 * transactions in order to preserve Cardano-level L2 transaction capacity,"
 * subject to every individual transaction fitting the live Cardano limits,
 * "bounded in transaction count," and — "Reducing the number of proof
 * transactions is an optimization objective. It is not a valid reason to
 * impose a lower Midgard transaction constraint." That fixes the bound as a
 * minimum, not an arbitrary constant: the bounded proof-transaction count is
 * the smallest number of §3.3-reserved proof transactions whose usable
 * execution units sum to at least the target snapshot's aggregate ceiling
 * on both axes simultaneously. Fewer would leave the aggregate short of the
 * floor by construction (see `checkAggregateScriptExecutionFloorV1`'s
 * adjacent-reject behavior); more is exactly the proof-transaction count the
 * decision record calls out to minimize instead.
 */

const targetSnapshotMemoryUnits = BigInt(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits,
);
const targetSnapshotCpuUnits = BigInt(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits,
);

// §3.3 execution fit: at or below the deployment's measured protocol limits
// with at least a 20% reserve, i.e. at most 80% of the limit is usable by any
// one proof transaction. Expressed as an exact integer ratio (4/5) so the
// derivation below never depends on floating-point rounding.
const EXECUTION_RESERVE_USABLE_NUMERATOR_V1 = 4n;
const EXECUTION_RESERVE_USABLE_DENOMINATOR_V1 = 5n;

/** Integer ceiling division for non-negative bigints. */
const ceilDiv = (numerator: bigint, denominator: bigint): bigint =>
  (numerator + denominator - 1n) / denominator;

/**
 * The target snapshot's per-transaction execution ceilings, restated as
 * bigints. Not an independently configurable constant: re-derives whenever
 * `MIDGARD_CONSENSUS_LIMITS_V1` changes (F05 invalidation trigger "any C70
 * snapshot change re-derives the floor").
 */
export const TARGET_SNAPSHOT_MEMORY_UNITS_V1 = targetSnapshotMemoryUnits;
export const TARGET_SNAPSHOT_CPU_UNITS_V1 = targetSnapshotCpuUnits;

/** The most execution units one §3.3-reserved proof transaction may use. */
export const PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1 =
  (targetSnapshotMemoryUnits * EXECUTION_RESERVE_USABLE_NUMERATOR_V1) /
  EXECUTION_RESERVE_USABLE_DENOMINATOR_V1;
export const PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1 =
  (targetSnapshotCpuUnits * EXECUTION_RESERVE_USABLE_NUMERATOR_V1) /
  EXECUTION_RESERVE_USABLE_DENOMINATOR_V1;

/**
 * The bounded proof-transaction count: the smallest N such that N maximally
 * §3.3-reserved proof transactions reach the target snapshot's aggregate
 * memory AND CPU ceiling simultaneously. Computed, never asserted, so it
 * moves automatically with the C70 target snapshot or the §3.3 reserve
 * ratio.
 */
export const BOUNDED_PROOF_TRANSACTION_COUNT_V1 = (() => {
  const requiredByMemory = ceilDiv(
    targetSnapshotMemoryUnits,
    PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  );
  const requiredByCpu = ceilDiv(
    targetSnapshotCpuUnits,
    PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
  );
  return requiredByMemory > requiredByCpu ? requiredByMemory : requiredByCpu;
})();

/**
 * The aggregate script-execution floor: the minimum aggregate memory/CPU a
 * Cardano-capable transaction's bounded proof-transaction sequence must
 * reach. By construction this is at least the target snapshot's own ceiling
 * on each axis (`PER_PROOF_TRANSACTION_USABLE_*_UNITS_V1 *
 * BOUNDED_PROOF_TRANSACTION_COUNT_V1 >= TARGET_SNAPSHOT_*_UNITS_V1`), which
 * is exactly C52's acceptance criterion.
 */
export const AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1 =
  PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1 *
  BOUNDED_PROOF_TRANSACTION_COUNT_V1;
export const AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1 =
  PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1 *
  BOUNDED_PROOF_TRANSACTION_COUNT_V1;

/** One proof transaction's measured execution units. */
export type ProofTransactionExecutionUnitsV1 = Readonly<{
  memoryUnits: bigint;
  cpuUnits: bigint;
}>;

/** The result of checking a candidate bounded proof-transaction sequence. */
export type AggregateScriptExecutionFloorCheckV1 = Readonly<{
  proofTransactionCount: bigint;
  aggregateMemoryUnits: bigint;
  aggregateCpuUnits: bigint;
  meetsMemoryFloor: boolean;
  meetsCpuFloor: boolean;
  accepted: boolean;
}>;

/**
 * Checks a candidate bounded proof-transaction sequence against the C52
 * aggregate script-execution floor.
 *
 * Each supplied proof transaction must itself already respect the §3.3
 * per-transaction reserve (`PER_PROOF_TRANSACTION_USABLE_*_UNITS_V1`) — a
 * real proof transaction that exceeded its own reserve would already be
 * rejected by §3.3's execution-fit gate before this aggregate check ever
 * runs, so a candidate that claims otherwise is malformed input, not a
 * borderline case, and fails closed.
 *
 * `accepted` is true only when the aggregate reaches the floor on both the
 * memory and CPU axes. One fewer proof transaction than the bounded count
 * (even each maximally utilized) always falls short by exactly one
 * transaction's usable units, and an aggregate one unit below the floor on
 * either axis is rejected the same way — both are adjacent-boundary
 * rejections, not special-cased.
 */
export const checkAggregateScriptExecutionFloorV1 = (
  proofTransactions: readonly ProofTransactionExecutionUnitsV1[],
): AggregateScriptExecutionFloorCheckV1 => {
  if (proofTransactions.length === 0) {
    throw new Error(
      "aggregate script-execution floor check requires at least one proof transaction",
    );
  }
  let aggregateMemoryUnits = 0n;
  let aggregateCpuUnits = 0n;
  for (const proofTransaction of proofTransactions) {
    if (proofTransaction.memoryUnits < 0n || proofTransaction.cpuUnits < 0n) {
      throw new Error("proof-transaction execution units must be non-negative");
    }
    if (
      proofTransaction.memoryUnits >
        PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1 ||
      proofTransaction.cpuUnits > PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1
    ) {
      throw new Error(
        "proof-transaction execution units exceed the §3.3 per-transaction reserve",
      );
    }
    aggregateMemoryUnits += proofTransaction.memoryUnits;
    aggregateCpuUnits += proofTransaction.cpuUnits;
  }
  const meetsMemoryFloor =
    aggregateMemoryUnits >= AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1;
  const meetsCpuFloor =
    aggregateCpuUnits >= AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1;
  return {
    proofTransactionCount: BigInt(proofTransactions.length),
    aggregateMemoryUnits,
    aggregateCpuUnits,
    meetsMemoryFloor,
    meetsCpuFloor,
    accepted: meetsMemoryFloor && meetsCpuFloor,
  };
};
