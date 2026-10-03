import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect } from "vitest";

import {
  type PhaseBConfig,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "../src/index.js";
import type {
  PhaseAValidatedTx,
  RejectCode,
  RejectedTx,
} from "../src/types.js";
import { makeNativeTx, makeQueued } from "./validation-fixtures.js";

/**
 * Shared drivers for the rejection-subject suites: each runs one transaction
 * through a validation phase and returns its single rejection, after checking
 * the rejection code the subject has to bridge back to.
 */

const phaseAConfig = {
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  concurrency: 1,
  strictnessProfile: "phase-a-unit",
};

export const phaseARejection = async (
  fixture: ReturnType<typeof makeNativeTx>,
  code: RejectCode,
): Promise<RejectedTx> => {
  const result = await Effect.runPromise(
    runPhaseAValidation(
      [makeQueued(fixture.txId, fixture.txCbor)],
      phaseAConfig,
    ),
  );
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0]!.code).toBe(code);
  return result.rejected[0]!;
};

export const phaseBRejection = async (
  candidate: PhaseAValidatedTx,
  state: readonly (readonly [Buffer, Buffer])[],
  code: RejectCode,
  config: Partial<PhaseBConfig> = {},
): Promise<RejectedTx> => {
  const result = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [candidate],
      new Map(
        state.map(([outRef, output]) => [outRef.toString("hex"), output]),
      ),
      { nowCardanoSlotNo: 100n, bucketConcurrency: 1, ...config },
    ),
  );
  expect(result.accepted).toHaveLength(0);
  expect(result.rejected).toHaveLength(1);
  expect(result.rejected[0]!.code).toBe(code);
  return result.rejected[0]!;
};

/** An unprotected enterprise address paying to `scriptHash`. */
export const scriptAddressBytes = (scriptHash: string): Buffer => {
  const hash = CML.ScriptHash.from_hex(scriptHash);
  const credential = CML.Credential.new_script(hash);
  const address = CML.EnterpriseAddress.new(0, credential).to_address();
  try {
    return Buffer.from(address.to_raw_bytes());
  } finally {
    address.free();
    credential.free();
    hash.free();
  }
};
