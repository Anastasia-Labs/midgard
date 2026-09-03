import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile-v1";
import { describe, expect, it } from "vitest";

import {
  compareProofTransactionPriority,
  type ProofTransactionDescriptor,
  selectBoundedProofTransactionSequence,
} from "../src/deterministic-proof-priority-v1.js";
import {
  checkProofTransactionCountCap,
  type FaultProofExecutionCost,
  PER_PROOF_TRANSACTION_USABLE_CPU_UNITS,
  PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS,
  PROOF_TRANSACTION_COUNT_CAP,
  requiredProofTransactionCountV1,
} from "../src/proof-transaction-count-cap-v1.js";

// Independent re-derivation of the §3.3 20% execution reserve, mirroring
// RESERVED_MEMORY_UNITS/RESERVED_CPU_UNITS at
// complete-item-proof-fit-emulator-v1.test.ts:84-89. This test file computes
// its own expectation from MIDGARD_CONSENSUS_LIMITS rather than trusting
// the module under test to report its own arithmetic correctly.
const independentUsableUnits = (limit: number): bigint =>
  (BigInt(limit) * 4n) / 5n;

describe("proof-transaction count cap V1 (C52)", () => {
  it("derives the per-proof-transaction usable budget from the §3.3 20% reserve off the target snapshot", () => {
    const expectedUsableMemoryUnits = independentUsableUnits(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits,
    );
    const expectedUsableCpuUnits = independentUsableUnits(
      MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxCpuUnits,
    );

    expect(PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS).toBe(
      expectedUsableMemoryUnits,
    );
    expect(PER_PROOF_TRANSACTION_USABLE_CPU_UNITS).toBe(expectedUsableCpuUnits);
    // The reserve strictly reduces the ceiling: no single proof transaction
    // can carry the target snapshot's whole execution budget on either axis.
    expect(PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS).toBeLessThan(
      BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits),
    );
    expect(PER_PROOF_TRANSACTION_USABLE_CPU_UNITS).toBeLessThan(
      BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxCpuUnits),
    );
  });

  it("computes the required proof-transaction count as the worst-axis ceiling over the usable budget", () => {
    // A proof costing exactly the target snapshot's own per-transaction
    // ceiling on both axes needs 2 proof transactions under the 4/5 reserve
    // (ceil(5/4) = 2 on each axis) — the retired floor framing's N, now just
    // one unremarkable point well below the cap.
    const targetSnapshotScaleProof: FaultProofExecutionCost = {
      memoryUnits: BigInt(
        MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxMemoryUnits,
      ),
      cpuUnits: BigInt(MIDGARD_CONSENSUS_LIMITS.minSupportedL1MaxTxCpuUnits),
    };
    expect(requiredProofTransactionCountV1(targetSnapshotScaleProof)).toBe(2n);

    // The worst axis governs: a memory-dominant proof needing 7 transactions
    // by memory but only 1 by CPU requires 7, and symmetrically for CPU.
    const memoryDominant: FaultProofExecutionCost = {
      memoryUnits: PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS * 6n + 1n,
      cpuUnits: PER_PROOF_TRANSACTION_USABLE_CPU_UNITS,
    };
    expect(requiredProofTransactionCountV1(memoryDominant)).toBe(7n);
    const cpuDominant: FaultProofExecutionCost = {
      memoryUnits: PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS,
      cpuUnits: PER_PROOF_TRANSACTION_USABLE_CPU_UNITS * 6n + 1n,
    };
    expect(requiredProofTransactionCountV1(cpuDominant)).toBe(7n);
  });

  it("accepts a proof on the order of a thousand transactions — the ruling's explicitly acceptable scale", () => {
    // Single-party proofs have no interaction latency, so the challenge
    // period accommodates ~1,000+ sequential proof transactions; the cap is
    // a pathology bound, not a capacity target.
    const thousandTransactionProof: FaultProofExecutionCost = {
      memoryUnits: PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS * 1_000n,
      cpuUnits: PER_PROOF_TRANSACTION_USABLE_CPU_UNITS * 1_000n,
    };
    const result = checkProofTransactionCountCap(thousandTransactionProof);
    expect(result.requiredProofTransactionCount).toBe(1_000n);
    expect(result.accepted).toBe(true);
  });

  it("accepts exactly at the cap and rejects one memory unit past it", () => {
    const exactlyAtCap: FaultProofExecutionCost = {
      memoryUnits:
        PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS * PROOF_TRANSACTION_COUNT_CAP,
      cpuUnits:
        PER_PROOF_TRANSACTION_USABLE_CPU_UNITS * PROOF_TRANSACTION_COUNT_CAP,
    };
    const atCap = checkProofTransactionCountCap(exactlyAtCap);
    expect(atCap.requiredProofTransactionCount).toBe(
      PROOF_TRANSACTION_COUNT_CAP,
    );
    expect(atCap.accepted).toBe(true);

    // One memory unit past the cap's worth of usable budget pushes the
    // required count to cap+1 on the memory axis alone — an
    // adjacent-boundary rejection.
    const oneMemoryUnitOver = checkProofTransactionCountCap({
      memoryUnits: exactlyAtCap.memoryUnits + 1n,
      cpuUnits: exactlyAtCap.cpuUnits,
    });
    expect(oneMemoryUnitOver.requiredByMemory).toBe(
      PROOF_TRANSACTION_COUNT_CAP + 1n,
    );
    expect(oneMemoryUnitOver.requiredByCpu).toBe(PROOF_TRANSACTION_COUNT_CAP);
    expect(oneMemoryUnitOver.accepted).toBe(false);
  });

  it("rejects one CPU unit past the cap symmetrically", () => {
    const oneCpuUnitOver = checkProofTransactionCountCap({
      memoryUnits:
        PER_PROOF_TRANSACTION_USABLE_MEMORY_UNITS * PROOF_TRANSACTION_COUNT_CAP,
      cpuUnits:
        PER_PROOF_TRANSACTION_USABLE_CPU_UNITS * PROOF_TRANSACTION_COUNT_CAP +
        1n,
    });
    expect(oneCpuUnitOver.requiredByCpu).toBe(PROOF_TRANSACTION_COUNT_CAP + 1n);
    expect(oneCpuUnitOver.requiredByMemory).toBe(PROOF_TRANSACTION_COUNT_CAP);
    expect(oneCpuUnitOver.accepted).toBe(false);
  });

  it("treats zero cost as zero required transactions and fails closed on negative cost", () => {
    const zeroCost = checkProofTransactionCountCap({
      memoryUnits: 0n,
      cpuUnits: 0n,
    });
    expect(zeroCost.requiredProofTransactionCount).toBe(0n);
    expect(zeroCost.accepted).toBe(true);

    expect(() =>
      requiredProofTransactionCountV1({ memoryUnits: -1n, cpuUnits: 0n }),
    ).toThrow("non-negative");
    expect(() =>
      checkProofTransactionCountCap({ memoryUnits: 0n, cpuUnits: -1n }),
    ).toThrow("non-negative");
  });

  it("produces a deterministic total priority ordering over proof-transaction descriptors", () => {
    const descriptors: ProofTransactionDescriptor[] = [
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

    const sortedOnce = [...descriptors].sort(compareProofTransactionPriority);
    const sortedFromReverse = [...descriptors]
      .reverse()
      .sort(compareProofTransactionPriority);
    const sortedFromShuffle = [
      descriptors[2]!,
      descriptors[0]!,
      descriptors[3]!,
      descriptors[1]!,
    ].sort(compareProofTransactionPriority);

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
    const equalWeightLeft: ProofTransactionDescriptor = {
      proofTransactionId: "f0",
      memoryUnits: 4_200_000n,
      cpuUnits: 1_500_000n,
    };
    const equalWeightRight: ProofTransactionDescriptor = {
      proofTransactionId: "0f",
      memoryUnits: 4_200_000n,
      cpuUnits: 1_500_000n,
    };
    const identicalToLeft: ProofTransactionDescriptor = {
      ...equalWeightLeft,
    };

    // Equal execution weight, distinct ids: the tie-break makes this a
    // strict order in both directions, never a comparator tie.
    expect(
      compareProofTransactionPriority(equalWeightLeft, equalWeightRight),
    ).toBeGreaterThan(0);
    expect(
      compareProofTransactionPriority(equalWeightRight, equalWeightLeft),
    ).toBeLessThan(0);
    // Only a genuinely identical descriptor (same id) compares equal.
    expect(
      compareProofTransactionPriority(equalWeightLeft, identicalToLeft),
    ).toBe(0);

    const selected = selectBoundedProofTransactionSequence(
      [equalWeightLeft, equalWeightRight],
      1n,
    );
    // Ascending id wins the tie-break: "0f" sorts before "f0".
    expect(selected).toEqual([equalWeightRight]);
  });
});
