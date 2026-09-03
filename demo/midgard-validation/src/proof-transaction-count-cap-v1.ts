import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile-v1";

/**
 * C52 "Proof-transaction count cap" (GOAL_SPEC.md §8.3, line 901):
 * no fault proof may require more than the sanctioned proof-transaction
 * sanity cap of §3.3-reserved proof transactions.
 *
 * Owner ruling (2026-08-18): the earlier "aggregate script-execution floor"
 * framing — a bounded proof-transaction count derived as the smallest N
 * reaching the target snapshot's aggregate ceiling, with an aggregate floor
 * of N × usable — is retired. A capacity floor is not the real constraint.
 * The governing constraint is that a fault proof completes within the
 * challenge period before the commitment merges; single-party proofs have
 * no interaction latency, so even a proof requiring on the order of 1,000+
 * transactions is acceptable. What must never happen is a pathologically
 * large proof: the check is therefore an upper bound, not a lower one.
 *
 * The per-transaction reserve arithmetic is unchanged from the retired
 * framing. The target snapshot's per-transaction execution ceiling is
 * `MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits` /
 * `...CpuUnits` (16,500,000 memory / 10,000,000,000 CPU — the §3.1.10
 * merged Cardano-mainnet-vs-target-network capability floor; see
 * capability-parity-v1.ts's `maxTxExecutionMemoryUnits`, pinned to
 * `"16500000"` in capability-parity-v1.test.ts:49). §3.3 "Execution fit"
 * requires every individual proof transaction to stay at or below the
 * deployment's measured protocol limits with at least a 20% reserve
 * (docs/consensus-profile-v1.md §10; the same reserve is applied at
 * complete-item-proof-fit-emulator-v1.test.ts:84-89 as
 * `RESERVED_MEMORY_UNITS`/`RESERVED_CPU_UNITS`), so one proof transaction
 * can carry at most 80% of the target snapshot's ceiling.
 *
 * The cap check inverts the retired floor's direction: for each fault
 * proof, the number of §3.3-reserved proof transactions it requires —
 * the ceiling of its measured execution cost over the per-transaction
 * usable budget, taken per axis, worst axis governing — must stay at or
 * below `PROOF_TRANSACTION_COUNT_CAP`. This asserts no shipped proof is
 * pathologically large, while deliberately NOT imposing any minimum:
 * `docs/midgard/decisions/0001-cardano-l1-transaction-capability-floor.md`
 * ("Accepted proof decomposition tradeoff") makes reducing the
 * proof-transaction count an optimization objective, never a constraint
 * source.
 */

const targetSnapshotMemoryUnits = BigInt(
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits,
);
const targetSnapshotCpuUnits = BigInt(
  MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxCpuUnits,
);

// §3.3 execution fit: at or below the deployment's measured protocol limits
// with at least a 20% reserve, i.e. at most 80% of the limit is usable by any
// one proof transaction. Expressed as an exact integer ratio (4/5) so the
// derivation below never depends on floating-point rounding.
const EXECUTION_RESERVE_USABLE_NUMERATOR = 4n;
const EXECUTION_RESERVE_USABLE_DENOMINATOR = 5n;

/** Integer ceiling division for non-negative bigints. */
const ceilDiv = (numerator: bigint, denominator: bigint): bigint =>
  (numerator + denominator - 1n) / denominator;

/**
 * The target snapshot's per-transaction execution ceilings, restated as
 * bigints. Not an independently configurable constant: re-derives whenever
 * `MIDGARD_CONSENSUS_LIMITS` changes, so the per-transaction usable
 * budget below moves automatically with any C70 snapshot change.
 */
export const TARGET_SNAPSHOT_MEMORY_UNITS = targetSnapshotMemoryUnits;
export const TARGET_SNAPSHOT_CPU_UNITS = targetSnapshotCpuUnits;

/** The most execution units one §3.3-reserved proof transaction may use. */
export const PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS =
  (targetSnapshotMemoryUnits * EXECUTION_RESERVE_USABLE_NUMERATOR) /
  EXECUTION_RESERVE_USABLE_DENOMINATOR;
export const PER_PROOF_TRANSACTION_USABLE_CPU_UNITS =
  (targetSnapshotCpuUnits * EXECUTION_RESERVE_USABLE_NUMERATOR) /
  EXECUTION_RESERVE_USABLE_DENOMINATOR;

/**
 * The proof-transaction count sanity cap: no fault proof may require more
 * than this many §3.3-reserved proof transactions to carry its complete
 * measured execution cost.
 *
 * Unlike the retired bounded count, this is an owner-asserted constant
 * (ruling 2026-08-18), not a derived value: it encodes "pathologically
 * large", which is a judgment about challenge-period completion, not
 * arithmetic over the target snapshot. The ruling's basis: fault proofs
 * are single-party (no interaction latency), so the challenge period
 * comfortably accommodates ~1,000+ sequential proof transactions; 5,000
 * is a sanity ceiling well above any acceptable proof, not a capacity
 * target.
 */
export const PROOF_TRANSACTION_COUNT_CAP = 5_000n;

/** One fault proof's complete measured execution cost across all its work. */
export type FaultProofExecutionCost = Readonly<{
  memoryUnits: bigint;
  cpuUnits: bigint;
}>;

/** The result of checking one fault proof against the count cap. */
export type ProofTransactionCountCapCheck = Readonly<{
  requiredByMemory: bigint;
  requiredByCpu: bigint;
  requiredProofTransactionCount: bigint;
  proofTransactionCountCap: bigint;
  accepted: boolean;
}>;

/**
 * The number of §3.3-reserved proof transactions a fault proof with the
 * given measured execution cost requires: the ceiling of its cost over the
 * per-transaction usable budget, computed per axis, with the worst axis
 * governing. A zero-cost input requires zero proof transactions — the
 * function is pure arithmetic over the measured cost and does not model
 * any per-proof fixed transaction overhead.
 */
export const requiredProofTransactionCountV1 = (
  cost: FaultProofExecutionCost,
): bigint => {
  if (cost.memoryUnits < 0n || cost.cpuUnits < 0n) {
    throw new Error("fault-proof execution cost must be non-negative");
  }
  const requiredByMemory = ceilDiv(
    cost.memoryUnits,
    PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS,
  );
  const requiredByCpu = ceilDiv(
    cost.cpuUnits,
    PER_PROOF_TRANSACTION_USABLE_CPU_UNITS,
  );
  return requiredByMemory > requiredByCpu ? requiredByMemory : requiredByCpu;
};

/**
 * Checks one fault proof's measured execution cost against the
 * proof-transaction count cap.
 *
 * `accepted` is true exactly when the required proof-transaction count is
 * at or below `PROOF_TRANSACTION_COUNT_CAP`. A proof whose cost pushes
 * the required count one transaction past the cap on either axis is
 * rejected — an adjacent-boundary rejection, not a special case.
 */
export const checkProofTransactionCountCap = (
  cost: FaultProofExecutionCost,
): ProofTransactionCountCapCheck => {
  if (cost.memoryUnits < 0n || cost.cpuUnits < 0n) {
    throw new Error("fault-proof execution cost must be non-negative");
  }
  const requiredByMemory = ceilDiv(
    cost.memoryUnits,
    PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS,
  );
  const requiredByCpu = ceilDiv(
    cost.cpuUnits,
    PER_PROOF_TRANSACTION_USABLE_CPU_UNITS,
  );
  const requiredProofTransactionCount =
    requiredByMemory > requiredByCpu ? requiredByMemory : requiredByCpu;
  return {
    requiredByMemory,
    requiredByCpu,
    requiredProofTransactionCount,
    proofTransactionCountCap: PROOF_TRANSACTION_COUNT_CAP,
    accepted: requiredProofTransactionCount <= PROOF_TRANSACTION_COUNT_CAP,
  };
};
