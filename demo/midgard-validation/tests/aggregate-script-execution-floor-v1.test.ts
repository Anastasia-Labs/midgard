import { MIDGARD_CONSENSUS_LIMITS_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import {
  AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1,
  AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1,
  BOUNDED_PROOF_TRANSACTION_COUNT_V1,
  checkAggregateScriptExecutionFloorV1,
  PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
  PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  type ProofTransactionExecutionUnitsV1,
} from "../src/aggregate-script-execution-floor-v1.js";
import {
  compareProofTransactionPriorityV1,
  type ProofTransactionDescriptorV1,
  selectBoundedProofTransactionSequenceV1,
} from "../src/deterministic-proof-priority-v1.js";

// Independent re-derivation of the §3.3 20% execution reserve, mirroring
// RESERVED_MEMORY_UNITS/RESERVED_CPU_UNITS at
// complete-item-proof-fit-emulator-v1.test.ts:84-89. This test file computes
// its own expectation from MIDGARD_CONSENSUS_LIMITS_V1 rather than trusting
// the module under test to report its own arithmetic correctly.
const independentUsableUnits = (limit: number): bigint =>
  (BigInt(limit) * 4n) / 5n;

const maximalProofTransaction = (): ProofTransactionExecutionUnitsV1 => ({
  memoryUnits: PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1,
  cpuUnits: PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1,
});

const maximalSequence = (count: bigint): ProofTransactionExecutionUnitsV1[] =>
  Array.from({ length: Number(count) }, () => maximalProofTransaction());

describe("aggregate script-execution floor V1 (C52)", () => {
  it("derives the per-proof-transaction memory floor from the §3.3 20% reserve off the target snapshot", () => {
    const expectedUsableMemoryUnits = independentUsableUnits(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits,
    );

    expect(PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1).toBe(
      expectedUsableMemoryUnits,
    );
    // The reserve strictly reduces the ceiling: no single proof transaction
    // can carry the target snapshot's whole memory budget.
    expect(PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS_V1).toBeLessThan(
      BigInt(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits),
    );
    // The aggregate floor, spread across the bounded proof-transaction
    // count, must reach at least the target snapshot's own memory ceiling —
    // C52's acceptance criterion, made checkable.
    expect(AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1).toBe(
      expectedUsableMemoryUnits * BOUNDED_PROOF_TRANSACTION_COUNT_V1,
    );
    expect(
      AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1 >=
        BigInt(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxMemoryUnits),
    ).toBe(true);
  });

  it("derives the per-proof-transaction step/CPU floor from the §3.3 20% reserve off the target snapshot", () => {
    const expectedUsableCpuUnits = independentUsableUnits(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits,
    );

    expect(PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1).toBe(
      expectedUsableCpuUnits,
    );
    expect(PER_PROOF_TRANSACTION_USABLE_CPU_UNITS_V1).toBeLessThan(
      BigInt(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits),
    );
    expect(AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1).toBe(
      expectedUsableCpuUnits * BOUNDED_PROOF_TRANSACTION_COUNT_V1,
    );
    expect(
      AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1 >=
        BigInt(MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxCpuUnits),
    ).toBe(true);
  });

  it("produces a deterministic total priority ordering over proof-transaction descriptors", () => {
    const descriptors: ProofTransactionDescriptorV1[] = [
      {
        proofTransactionId: "cc",
        memoryUnits: 5_000_000n,
        cpuUnits: 1_000_000n,
      },
      {
        proofTransactionId: "aa",
        memoryUnits: 9_000_000n,
        cpuUnits: 2_000_000n,
      },
      {
        proofTransactionId: "bb",
        memoryUnits: 9_000_000n,
        cpuUnits: 3_000_000n,
      },
      {
        proofTransactionId: "dd",
        memoryUnits: 1_000_000n,
        cpuUnits: 9_000_000n,
      },
    ];

    const sortedOnce = [...descriptors].sort(compareProofTransactionPriorityV1);
    const sortedFromReverse = [...descriptors]
      .reverse()
      .sort(compareProofTransactionPriorityV1);
    const sortedFromShuffle = [
      descriptors[2]!,
      descriptors[0]!,
      descriptors[3]!,
      descriptors[1]!,
    ].sort(compareProofTransactionPriorityV1);

    const orderedIds = sortedOnce.map((entry) => entry.proofTransactionId);
    // Highest memoryUnits first (bb/aa tie at 9,000,000; broken by cpuUnits,
    // then by id), then the lower-memory descriptors in descending memory
    // order.
    expect(orderedIds).toEqual(["bb", "aa", "cc", "dd"]);
    // The ordering does not depend on input order: sorting any permutation
    // of the same descriptor set yields the identical sequence.
    expect(sortedFromReverse.map((entry) => entry.proofTransactionId)).toEqual(
      orderedIds,
    );
    expect(sortedFromShuffle.map((entry) => entry.proofTransactionId)).toEqual(
      orderedIds,
    );
  });

  it("resolves equal-weight proof transactions through the canonical id tie-break", () => {
    const equalWeightLeft: ProofTransactionDescriptorV1 = {
      proofTransactionId: "f0",
      memoryUnits: 4_200_000n,
      cpuUnits: 1_500_000n,
    };
    const equalWeightRight: ProofTransactionDescriptorV1 = {
      proofTransactionId: "0f",
      memoryUnits: 4_200_000n,
      cpuUnits: 1_500_000n,
    };
    const identicalToLeft: ProofTransactionDescriptorV1 = {
      ...equalWeightLeft,
    };

    // Equal execution weight, distinct ids: the tie-break makes this a
    // strict order in both directions, never a comparator tie.
    expect(
      compareProofTransactionPriorityV1(equalWeightLeft, equalWeightRight),
    ).toBeGreaterThan(0);
    expect(
      compareProofTransactionPriorityV1(equalWeightRight, equalWeightLeft),
    ).toBeLessThan(0);
    // Only a genuinely identical descriptor (same id) compares equal.
    expect(
      compareProofTransactionPriorityV1(equalWeightLeft, identicalToLeft),
    ).toBe(0);

    const selected = selectBoundedProofTransactionSequenceV1(
      [equalWeightLeft, equalWeightRight],
      1n,
    );
    // Ascending id wins the tie-break: "0f" sorts before "f0".
    expect(selected).toEqual([equalWeightRight]);
  });

  it("rejects one fewer proof transaction than the bounded count and one unit below the floor", () => {
    const atFloor = checkAggregateScriptExecutionFloorV1(
      maximalSequence(BOUNDED_PROOF_TRANSACTION_COUNT_V1),
    );
    expect(atFloor.accepted).toBe(true);
    expect(atFloor.aggregateMemoryUnits).toBe(
      AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1,
    );
    expect(atFloor.aggregateCpuUnits).toBe(
      AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1,
    );

    // One fewer proof transaction than the bounded count, even maximally
    // utilized, is short by exactly one transaction's usable units on both
    // axes and must be rejected.
    const oneFewerTransactions = maximalSequence(
      BOUNDED_PROOF_TRANSACTION_COUNT_V1 - 1n,
    );
    const oneFewerResult =
      checkAggregateScriptExecutionFloorV1(oneFewerTransactions);
    expect(oneFewerResult.accepted).toBe(false);
    expect(oneFewerResult.meetsMemoryFloor).toBe(false);
    expect(oneFewerResult.meetsCpuFloor).toBe(false);

    // The bounded count of proof transactions, but one memory unit short of
    // the floor, must be rejected on the memory axis alone.
    const oneUnitUnderMemory = maximalSequence(
      BOUNDED_PROOF_TRANSACTION_COUNT_V1,
    );
    oneUnitUnderMemory[oneUnitUnderMemory.length - 1] = {
      memoryUnits:
        oneUnitUnderMemory[oneUnitUnderMemory.length - 1]!.memoryUnits - 1n,
      cpuUnits: oneUnitUnderMemory[oneUnitUnderMemory.length - 1]!.cpuUnits,
    };
    const memoryShortResult =
      checkAggregateScriptExecutionFloorV1(oneUnitUnderMemory);
    expect(memoryShortResult.accepted).toBe(false);
    expect(memoryShortResult.meetsMemoryFloor).toBe(false);
    expect(memoryShortResult.aggregateMemoryUnits).toBe(
      AGGREGATE_SCRIPT_EXECUTION_MEMORY_FLOOR_V1 - 1n,
    );

    // Symmetrically for the CPU axis, one step short of the floor.
    const oneUnitUnderCpu = maximalSequence(BOUNDED_PROOF_TRANSACTION_COUNT_V1);
    oneUnitUnderCpu[oneUnitUnderCpu.length - 1] = {
      memoryUnits: oneUnitUnderCpu[oneUnitUnderCpu.length - 1]!.memoryUnits,
      cpuUnits: oneUnitUnderCpu[oneUnitUnderCpu.length - 1]!.cpuUnits - 1n,
    };
    const cpuShortResult =
      checkAggregateScriptExecutionFloorV1(oneUnitUnderCpu);
    expect(cpuShortResult.accepted).toBe(false);
    expect(cpuShortResult.meetsCpuFloor).toBe(false);
    expect(cpuShortResult.aggregateCpuUnits).toBe(
      AGGREGATE_SCRIPT_EXECUTION_CPU_FLOOR_V1 - 1n,
    );
  });
});
