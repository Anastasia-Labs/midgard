#!/usr/bin/env node
/**
 * C52 "Aggregate script-execution floor" (GOAL_SPEC.md §8.3, line 901).
 *
 * Standalone re-derivation gate: independently recomputes the §3.3 20%
 * per-proof-transaction execution reserve and the bounded aggregate floor
 * from the live `MIDGARD_CONSENSUS_LIMITS_V1` target snapshot, and checks
 * the recomputed values against what
 * `demo/midgard-validation/src/aggregate-script-execution-floor-v1.ts`
 * actually exports — so a drifted or hand-edited constant in that module
 * fails this gate instead of silently shipping. It then exercises the
 * module's own checking function at the floor boundary (accept) and at both
 * adjacent-reject cases (one fewer proof transaction; one execution unit
 * short), and exercises the companion deterministic proof-priority ordering
 * in `deterministic-proof-priority-v1.ts` for determinism and its canonical
 * tie-break.
 *
 * Deterministic: pure bigint arithmetic over already-committed source, no
 * network access, no timestamps, no reliance on `Date.now()` or process
 * environment beyond argv.
 */

import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

const scriptDirectory = resolve(fileURLToPath(new URL(".", import.meta.url)));
const repositoryRoot = resolve(scriptDirectory, "..", "..");

// Resolved by absolute path rather than the bare `@al-ft/midgard-core`
// specifier: this script lives in `demo/scripts/`, which has no
// `node_modules` linkage of its own, unlike `demo/midgard-validation/`
// (whose `node_modules/@al-ft/midgard-core` workspace symlink is what lets
// the floor module below resolve that same bare specifier from its own
// location).
const consensusProfileModulePath = resolve(
  repositoryRoot,
  "demo/midgard-core/dist/consensus-profile-v1.js",
);
const floorModulePath = resolve(
  repositoryRoot,
  "demo/midgard-validation/src/aggregate-script-execution-floor-v1.ts",
);
const priorityModulePath = resolve(
  repositoryRoot,
  "demo/midgard-validation/src/deterministic-proof-priority-v1.ts",
);

const { MIDGARD_CONSENSUS_LIMITS_V1 } = await import(
  consensusProfileModulePath
);

const {
  AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1,
  AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1,
  BOUNDED_PROOF_TRANSACTION_COUNT_V1,
  checkAggregateScriptExecutionFloorV1,
  PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
  PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  TARGET_SNAPSHOT_CPU_UNITS_V1,
  TARGET_SNAPSHOT_MEMORY_UNITS_V1,
} = await import(floorModulePath);

const {
  compareProofTransactionPriorityV1,
  selectBoundedProofTransactionSequenceV1,
} = await import(priorityModulePath);

const failures = [];
const fail = (message) => {
  failures.push(message);
};

// ---------------------------------------------------------------------------
// Phase 1 — independently re-derive the floor arithmetic from the live
// target snapshot and check it against the module's exported constants.
// ---------------------------------------------------------------------------

const targetSnapshotMemoryUnits = BigInt(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits,
);
const targetSnapshotCpuUnits = BigInt(
  MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits,
);

// §3.3 "Execution fit": at or below the deployment's measured protocol
// limits with at least a 20% reserve, i.e. at most 4/5 of the limit is
// usable by any one proof transaction. Exact integer arithmetic only.
const expectedUsableMemoryUnits = (targetSnapshotMemoryUnits * 4n) / 5n;
const expectedUsableCpuUnits = (targetSnapshotCpuUnits * 4n) / 5n;

const ceilDiv = (numerator, denominator) =>
  (numerator + denominator - 1n) / denominator;

// The bounded proof-transaction count: the smallest N such that N maximally
// §3.3-reserved proof transactions reach the target snapshot's aggregate
// ceiling on both axes at once (docs/midgard/decisions/
// 0001-cardano-l1-transaction-capability-floor.md, "Accepted proof
// decomposition tradeoff": minimizing proof-transaction count is an
// optimization objective, so the bound is the minimum that suffices).
const requiredByMemory = ceilDiv(
  targetSnapshotMemoryUnits,
  expectedUsableMemoryUnits,
);
const requiredByCpu = ceilDiv(targetSnapshotCpuUnits, expectedUsableCpuUnits);
const expectedBoundedProofTransactionCount =
  requiredByMemory > requiredByCpu ? requiredByMemory : requiredByCpu;

const expectedAggregateMemoryFloor =
  expectedUsableMemoryUnits * expectedBoundedProofTransactionCount;
const expectedAggregateCpuFloor =
  expectedUsableCpuUnits * expectedBoundedProofTransactionCount;

const assertBigintEqual = (label, actual, expected) => {
  if (actual !== expected) {
    fail(
      `ERR_C52_DERIVATION_MISMATCH: ${label} exported ${String(actual)} but the independent re-derivation from the live target snapshot computes ${String(expected)}`,
    );
  }
};

assertBigintEqual(
  "TARGET_SNAPSHOT_MEMORY_UNITS_V1",
  TARGET_SNAPSHOT_MEMORY_UNITS_V1,
  targetSnapshotMemoryUnits,
);
assertBigintEqual(
  "TARGET_SNAPSHOT_CPU_UNITS_V1",
  TARGET_SNAPSHOT_CPU_UNITS_V1,
  targetSnapshotCpuUnits,
);
assertBigintEqual(
  "PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1",
  PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  expectedUsableMemoryUnits,
);
assertBigintEqual(
  "PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1",
  PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
  expectedUsableCpuUnits,
);
assertBigintEqual(
  "BOUNDED_PROOF_TRANSACTION_COUNT_V1",
  BOUNDED_PROOF_TRANSACTION_COUNT_V1,
  expectedBoundedProofTransactionCount,
);
assertBigintEqual(
  "AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1",
  AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1,
  expectedAggregateMemoryFloor,
);
assertBigintEqual(
  "AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1",
  AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1,
  expectedAggregateCpuFloor,
);

// C52's own acceptance criterion: the bounded aggregate floor must reach at
// least the target snapshot's per-transaction ceiling on both axes.
if (AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1 < targetSnapshotMemoryUnits) {
  fail(
    `ERR_C52_FLOOR_BELOW_TARGET: aggregate memory floor ${String(AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1)} is below the target snapshot's ${String(targetSnapshotMemoryUnits)}`,
  );
}
if (AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1 < targetSnapshotCpuUnits) {
  fail(
    `ERR_C52_FLOOR_BELOW_TARGET: aggregate CPU floor ${String(AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1)} is below the target snapshot's ${String(targetSnapshotCpuUnits)}`,
  );
}

// ---------------------------------------------------------------------------
// Phase 2 — exercise the checking function at the floor boundary and at
// both adjacent-reject cases.
// ---------------------------------------------------------------------------

const maximalProofTransaction = () => ({
  memoryUnits: PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  cpuUnits: PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
});
const maximalSequence = (count) =>
  Array.from({ length: Number(count) }, maximalProofTransaction);

const atFloor = checkAggregateScriptExecutionFloorV1(
  maximalSequence(BOUNDED_PROOF_TRANSACTION_COUNT_V1),
);
if (!atFloor.accepted) {
  fail(
    "ERR_C52_FLOOR_NOT_ACCEPTED: the bounded proof-transaction sequence at maximal per-transaction utilization was not accepted at the aggregate floor",
  );
}

const oneFewer = checkAggregateScriptExecutionFloorV1(
  maximalSequence(BOUNDED_PROOF_TRANSACTION_COUNT_V1 - 1n),
);
if (oneFewer.accepted) {
  fail(
    "ERR_C52_ADJACENT_ACCEPT: one fewer proof transaction than the bounded count was incorrectly accepted",
  );
}

const oneUnitUnderMemorySequence = maximalSequence(
  BOUNDED_PROOF_TRANSACTION_COUNT_V1,
);
const lastIndex = oneUnitUnderMemorySequence.length - 1;
oneUnitUnderMemorySequence[lastIndex] = {
  memoryUnits: oneUnitUnderMemorySequence[lastIndex].memoryUnits - 1n,
  cpuUnits: oneUnitUnderMemorySequence[lastIndex].cpuUnits,
};
const oneUnitUnderMemory = checkAggregateScriptExecutionFloorV1(
  oneUnitUnderMemorySequence,
);
if (oneUnitUnderMemory.accepted) {
  fail(
    "ERR_C52_ADJACENT_ACCEPT: an aggregate one memory unit below the floor was incorrectly accepted",
  );
}

const oneUnitUnderCpuSequence = maximalSequence(
  BOUNDED_PROOF_TRANSACTION_COUNT_V1,
);
oneUnitUnderCpuSequence[lastIndex] = {
  memoryUnits: oneUnitUnderCpuSequence[lastIndex].memoryUnits,
  cpuUnits: oneUnitUnderCpuSequence[lastIndex].cpuUnits - 1n,
};
const oneUnitUnderCpu = checkAggregateScriptExecutionFloorV1(
  oneUnitUnderCpuSequence,
);
if (oneUnitUnderCpu.accepted) {
  fail(
    "ERR_C52_ADJACENT_ACCEPT: an aggregate one CPU unit below the floor was incorrectly accepted",
  );
}

// ---------------------------------------------------------------------------
// Phase 3 — deterministic proof-priority ordering: determinism and the
// canonical tie-break.
// ---------------------------------------------------------------------------

const descriptors = [
  { proofTransactionId: "cc", memoryUnits: 5_000_000n, cpuUnits: 1_000_000n },
  { proofTransactionId: "aa", memoryUnits: 9_000_000n, cpuUnits: 2_000_000n },
  { proofTransactionId: "bb", memoryUnits: 9_000_000n, cpuUnits: 3_000_000n },
  { proofTransactionId: "dd", memoryUnits: 1_000_000n, cpuUnits: 9_000_000n },
];
const expectedOrder = ["bb", "aa", "cc", "dd"];
const orderedTwice = [
  [...descriptors].sort(compareProofTransactionPriorityV1),
  [...descriptors].reverse().sort(compareProofTransactionPriorityV1),
];
for (const ordered of orderedTwice) {
  const orderedIds = ordered.map((entry) => entry.proofTransactionId);
  if (JSON.stringify(orderedIds) !== JSON.stringify(expectedOrder)) {
    fail(
      `ERR_C52_PRIORITY_NONDETERMINISTIC: expected priority order ${JSON.stringify(expectedOrder)}, got ${JSON.stringify(orderedIds)}`,
    );
  }
}

const equalWeightLeft = {
  proofTransactionId: "f0",
  memoryUnits: 4_200_000n,
  cpuUnits: 1_500_000n,
};
const equalWeightRight = {
  proofTransactionId: "0f",
  memoryUnits: 4_200_000n,
  cpuUnits: 1_500_000n,
};
if (
  compareProofTransactionPriorityV1(equalWeightLeft, equalWeightRight) === 0
) {
  fail(
    "ERR_C52_PRIORITY_TIE: two distinct descriptors with equal execution weight compared equal instead of resolving through the canonical id tie-break",
  );
}
const tieBreakSelection = selectBoundedProofTransactionSequenceV1(
  [equalWeightLeft, equalWeightRight],
  1n,
);
if (
  tieBreakSelection.length !== 1 ||
  tieBreakSelection[0].proofTransactionId !== "0f"
) {
  fail(
    `ERR_C52_PRIORITY_TIE_BREAK: expected the ascending-id tie-break to select "0f", got ${JSON.stringify(tieBreakSelection.map((entry) => entry.proofTransactionId))}`,
  );
}

// ---------------------------------------------------------------------------

if (failures.length > 0) {
  for (const failure of failures) {
    console.error(failure);
  }
  process.exit(1);
}

console.log(
  `C52 aggregate script-execution floor: PASS (bounded proof-transaction count ${String(BOUNDED_PROOF_TRANSACTION_COUNT_V1)}, ` +
    `per-transaction usable ${String(PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1)} memory / ${String(PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1)} CPU, ` +
    `aggregate floor ${String(AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1)} memory / ${String(AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1)} CPU)`,
);
process.exit(0);
