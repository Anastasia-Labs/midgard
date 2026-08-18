#!/usr/bin/env node
/**
 * C52 "Proof-transaction count cap" (GOAL_SPEC.md §8.3, line 901).
 *
 * Standalone re-derivation gate: independently recomputes the §3.3 20%
 * per-proof-transaction execution reserve from the live
 * `MIDGARD_CONSENSUS_LIMITS_V1` target snapshot and checks the recomputed
 * values against what
 * `demo/midgard-validation/src/proof-transaction-count-cap-v1.ts` actually
 * exports — so a drifted or hand-edited constant in that module fails this
 * gate instead of silently shipping. It then exercises the module's
 * cap-checking function at the cap boundary (accept) and one unit past it
 * on each axis (reject), and exercises the companion deterministic
 * proof-priority ordering in `deterministic-proof-priority-v1.ts` for
 * determinism and its canonical tie-break.
 *
 * Owner ruling (2026-08-18): the earlier aggregate-floor framing (bounded
 * count derived as the minimum reaching the snapshot ceiling; aggregate
 * floor = count × usable) is retired. The cap is an owner-asserted sanity
 * bound — this gate verifies the module ships exactly the ruled cap of
 * 5,000 and that the required-count arithmetic is the worst-axis ceiling
 * over the per-transaction usable budget.
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
// the cap module below resolve that same bare specifier from its own
// location).
const consensusProfileModulePath = resolve(
  repositoryRoot,
  "demo/midgard-core/dist/consensus-profile-v1.js",
);
const capModulePath = resolve(
  repositoryRoot,
  "demo/midgard-validation/src/proof-transaction-count-cap-v1.ts",
);
const priorityModulePath = resolve(
  repositoryRoot,
  "demo/midgard-validation/src/deterministic-proof-priority-v1.ts",
);

const { MIDGARD_CONSENSUS_LIMITS_V1 } = await import(
  consensusProfileModulePath
);

const {
  checkProofTransactionCountCapV1,
  PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
  PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  PROOF_TRANSACTION_COUNT_CAP_V1,
  requiredProofTransactionCountV1,
  TARGET_SNAPSHOT_CPU_UNITS_V1,
  TARGET_SNAPSHOT_MEMORY_UNITS_V1,
} = await import(capModulePath);

const {
  compareProofTransactionPriorityV1,
  selectBoundedProofTransactionSequenceV1,
} = await import(priorityModulePath);

const failures = [];
const fail = (message) => {
  failures.push(message);
};

// ---------------------------------------------------------------------------
// Phase 1 — independently re-derive the per-transaction usable budget from
// the live target snapshot, check it against the module's exported
// constants, and check the cap is exactly the ruled constant.
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

// The cap is owner-asserted (ruling 2026-08-18), not derived: this gate
// pins the shipped constant to exactly the ruled value so a silent edit
// fails here.
assertBigintEqual(
  "PROOF_TRANSACTION_COUNT_CAP_V1",
  PROOF_TRANSACTION_COUNT_CAP_V1,
  5_000n,
);

// Required-count arithmetic: worst-axis ceiling over the usable budget,
// re-derived here on probe costs and compared against the module.
const probeCosts = [
  // The target snapshot's own per-transaction ceiling on both axes: needs
  // exactly 2 proof transactions under the 4/5 reserve (ceil(5/4) = 2).
  {
    label: "target-snapshot-scale proof",
    memoryUnits: targetSnapshotMemoryUnits,
    cpuUnits: targetSnapshotCpuUnits,
  },
  // Memory-dominant and CPU-dominant probes: the worst axis governs.
  {
    label: "memory-dominant probe",
    memoryUnits: expectedUsableMemoryUnits * 6n + 1n,
    cpuUnits: expectedUsableCpuUnits,
  },
  {
    label: "cpu-dominant probe",
    memoryUnits: expectedUsableMemoryUnits,
    cpuUnits: expectedUsableCpuUnits * 6n + 1n,
  },
  // The ruling's explicitly acceptable scale: ~1,000 proof transactions.
  {
    label: "thousand-transaction proof",
    memoryUnits: expectedUsableMemoryUnits * 1_000n,
    cpuUnits: expectedUsableCpuUnits * 1_000n,
  },
];

for (const probe of probeCosts) {
  const expectedRequired = (() => {
    const byMemory = ceilDiv(probe.memoryUnits, expectedUsableMemoryUnits);
    const byCpu = ceilDiv(probe.cpuUnits, expectedUsableCpuUnits);
    return byMemory > byCpu ? byMemory : byCpu;
  })();
  const actualRequired = requiredProofTransactionCountV1({
    memoryUnits: probe.memoryUnits,
    cpuUnits: probe.cpuUnits,
  });
  if (actualRequired !== expectedRequired) {
    fail(
      `ERR_C52_DERIVATION_MISMATCH: ${probe.label} required count ${String(actualRequired)} but the independent re-derivation computes ${String(expectedRequired)}`,
    );
  }
  if (actualRequired > PROOF_TRANSACTION_COUNT_CAP_V1) {
    fail(
      `ERR_C52_PROBE_OVER_CAP: ${probe.label} unexpectedly requires ${String(actualRequired)} proof transactions, over the cap ${String(PROOF_TRANSACTION_COUNT_CAP_V1)}`,
    );
  }
}

const targetSnapshotRequired = requiredProofTransactionCountV1({
  memoryUnits: targetSnapshotMemoryUnits,
  cpuUnits: targetSnapshotCpuUnits,
});

// ---------------------------------------------------------------------------
// Phase 2 — exercise the cap check at the boundary and one unit past it on
// each axis.
// ---------------------------------------------------------------------------

const exactlyAtCapCost = {
  memoryUnits: expectedUsableMemoryUnits * PROOF_TRANSACTION_COUNT_CAP_V1,
  cpuUnits: expectedUsableCpuUnits * PROOF_TRANSACTION_COUNT_CAP_V1,
};
const atCap = checkProofTransactionCountCapV1(exactlyAtCapCost);
if (
  !atCap.accepted ||
  atCap.requiredProofTransactionCount !== PROOF_TRANSACTION_COUNT_CAP_V1
) {
  fail(
    `ERR_C52_CAP_BOUNDARY: a proof requiring exactly the cap (${String(PROOF_TRANSACTION_COUNT_CAP_V1)}) was not accepted at the boundary (required ${String(atCap.requiredProofTransactionCount)}, accepted ${String(atCap.accepted)})`,
  );
}

const oneMemoryUnitOver = checkProofTransactionCountCapV1({
  memoryUnits: exactlyAtCapCost.memoryUnits + 1n,
  cpuUnits: exactlyAtCapCost.cpuUnits,
});
if (oneMemoryUnitOver.accepted) {
  fail(
    "ERR_C52_ADJACENT_ACCEPT: a proof one memory unit past the cap's usable budget was incorrectly accepted",
  );
}

const oneCpuUnitOver = checkProofTransactionCountCapV1({
  memoryUnits: exactlyAtCapCost.memoryUnits,
  cpuUnits: exactlyAtCapCost.cpuUnits + 1n,
});
if (oneCpuUnitOver.accepted) {
  fail(
    "ERR_C52_ADJACENT_ACCEPT: a proof one CPU unit past the cap's usable budget was incorrectly accepted",
  );
}

// Fail-closed on malformed input: negative measured cost must throw, never
// report a count.
let negativeThrew = false;
try {
  requiredProofTransactionCountV1({ memoryUnits: -1n, cpuUnits: 0n });
} catch {
  negativeThrew = true;
}
if (!negativeThrew) {
  fail(
    "ERR_C52_NEGATIVE_ACCEPTED: a negative measured cost did not fail closed",
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
  `C52 proof-transaction count cap: PASS (cap ${String(PROOF_TRANSACTION_COUNT_CAP_V1)}, ` +
    `per-transaction usable ${String(PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1)} memory / ${String(PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1)} CPU, ` +
    `target-snapshot-scale proof requires ${String(targetSnapshotRequired)} proof transactions)`,
);
process.exit(0);
