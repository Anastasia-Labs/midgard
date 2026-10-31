import { MIDGARD_POSIX_TIME_NONE } from "@al-ft/midgard-core/codec";
import {
  invalidRangeViolationReason,
  type NativeTxBodyCompact,
  normalizeNativeTxValidityRange,
} from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { RejectCodes, runPhaseAValidation } from "../src/index.js";
import {
  expectSinglePhaseBRejection,
  phaseBConfig,
  preState,
  runPhaseB,
} from "./phase-b.harness.js";
import {
  FUNDED_OUTPUT_LOVELACE,
  makeNativeTx,
  makeOutput,
  makePhaseBCandidate,
  makeQueued,
  outRefFromByte,
} from "./validation-fixtures.js";

// The validity upper bound is Cardano's exclusive `invalid_hereafter`: a
// transaction is valid at slot `s` iff `start <= s < end`, so an interval with
// both bounds present and `start >= end` admits no slot and is malformed. The
// Phase B harness slot is `phaseBConfig.nowCardanoSlotNo` (100).
const runPhaseAOn = (start: bigint, end: bigint) => {
  const fixture = makeNativeTx({
    validityIntervalStart: start,
    validityIntervalEnd: end,
  });
  return Effect.runPromise(
    runPhaseAValidation([makeQueued(fixture.txId, fixture.txCbor)], {
      expectedNetworkId: 0n,
      minFeeA: 0n,
      minFeeB: 0n,
      concurrency: 1,
      strictnessProfile: "phase-a-unit",
    }),
  );
};

describe("phase A exclusive validity upper bound", () => {
  it("rejects an empty validity interval as malformed", async () => {
    const result = await runPhaseAOn(20n, 20n);
    expect(result.accepted).toHaveLength(0);
    expect(result.rejected).toMatchObject([
      { code: RejectCodes.InvalidValidityIntervalFormat, detail: "20 >= 20" },
    ]);
  });

  it("admits a one-slot validity interval", async () => {
    const result = await runPhaseAOn(20n, 21n);
    expect(result.rejected).toHaveLength(0);
    expect(result.accepted).toHaveLength(1);
  });
});

const candidateWithInterval = (
  outRefByte: number,
  start: bigint,
  end: bigint,
  requiresLocalScriptDiscovery: boolean,
) => {
  const candidate = makePhaseBCandidate({
    spent: [outRefFromByte(outRefByte)],
    outputLovelace: FUNDED_OUTPUT_LOVELACE,
    validityIntervalStart: start,
    validityIntervalEnd: end,
  });
  return {
    ...candidate,
    derived: { ...candidate.derived, requiresLocalScriptDiscovery },
  };
};

const stateFor = (outRefByte: number) =>
  preState([[outRefFromByte(outRefByte), makeOutput(FUNDED_OUTPUT_LOVELACE)]]);

const compactBody = (start: bigint, end: bigint): NativeTxBodyCompact => ({
  spend_inputs_hash: "00".repeat(32),
  reference_inputs_hash: "00".repeat(32),
  outputs_hash: "00".repeat(32),
  fee: 0n,
  validity_interval_start: start,
  validity_interval_end: end,
  required_observers_hash: "00".repeat(32),
  required_signers_hash: "00".repeat(32),
  mint_hash: "00".repeat(32),
  script_integrity_hash: "00".repeat(32),
  auxiliary_data_hash: "00".repeat(32),
  network_id: 0n,
});

const slot = phaseBConfig.nowCardanoSlotNo;
const none = MIDGARD_POSIX_TIME_NONE;

// Well-formed intervals around the harness slot: each bound absent, at, or
// one either side of the slot, with `start < end` whenever both are present.
const intervals = [none, slot - 1n, slot, slot + 1n]
  .flatMap((start) =>
    [none, slot - 1n, slot, slot + 1n, slot + 2n].map(
      (end) => [start, end] as const,
    ),
  )
  .filter(([start, end]) => start === none || end === none || start < end);

describe.each([
  { path: "plain", requiresLocalScriptDiscovery: false },
  { path: "script-discovery", requiresLocalScriptDiscovery: true },
])(
  "phase B exclusive validity upper bound ($path path)",
  ({ requiresLocalScriptDiscovery }) => {
    it("rejects a candidate whose upper bound is the current slot", async () => {
      const result = await runPhaseB(
        [candidateWithInterval(0x2a, 90n, slot, requiresLocalScriptDiscovery)],
        stateFor(0x2a),
      );

      const rejection = expectSinglePhaseBRejection(
        result,
        RejectCodes.ValidityIntervalMismatch,
      );
      expect(rejection.detail).toBe("100 >= 100");
    });

    it("accepts a candidate whose upper bound is one past the current slot", async () => {
      const result = await runPhaseB(
        [
          candidateWithInterval(
            0x2b,
            90n,
            slot + 1n,
            requiresLocalScriptDiscovery,
          ),
        ],
        stateFor(0x2b),
      );

      expect(result.rejected).toHaveLength(0);
      expect(result.accepted).toHaveLength(1);
    });

    it("agrees with the invalid-range proof's normalization on every interval around the slot", async () => {
      const verdicts = await Promise.all(
        intervals.map(async ([start, end], index) => {
          const outRefByte = 0x40 + index;
          const result = await runPhaseB(
            [
              candidateWithInterval(
                outRefByte,
                start,
                end,
                requiresLocalScriptDiscovery,
              ),
            ],
            stateFor(outRefByte),
          );
          const proofReason = invalidRangeViolationReason({
            blockSlot: slot,
            normalizedRange: normalizeNativeTxValidityRange(
              compactBody(start, end),
            ),
          });
          return {
            start,
            end,
            machineAccepts: result.accepted.length === 1,
            proofConvicts: proofReason !== null,
          };
        }),
      );

      for (const verdict of verdicts) {
        expect(verdict, `[${verdict.start}, ${verdict.end})`).toMatchObject({
          machineAccepts: !verdict.proofConvicts,
        });
      }
    });
  },
);
