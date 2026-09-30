import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core";
import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import type { OperatorVerdict } from "@al-ft/midgard-sdk";
import {
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import { forcedVerdictForRejection } from "../../../src/index.js";

/**
 * The verdict the node's forced-transaction classifier commits for one forced
 * transaction: Phase A over its canonical forced bytes, then Phase B against
 * `ledger`, then the shared forced-rejection writer. Emulator suites commit
 * this verdict, so what they prove or refuse is what the node would write.
 */
export const nodeForcedVerdict = async ({
  transactionId,
  forcedCanonicalCbor,
  ledger = [],
  programMaterialSidecarCbor = encodeMidgardCekProgramMaterialSidecar([]),
  nowCardanoSlotNo = 0n,
}: {
  readonly transactionId: Buffer;
  readonly forcedCanonicalCbor: Buffer;
  /** Ledger entries as `[out-ref item bytes, output bytes]`. */
  readonly ledger?: readonly (readonly [Buffer, Buffer])[];
  readonly programMaterialSidecarCbor?: Buffer;
  readonly nowCardanoSlotNo?: bigint;
}): Promise<OperatorVerdict> => {
  const phaseA = await Effect.runPromise(
    runPhaseAValidation(
      [
        {
          sourceKind: "forced",
          txId: transactionId,
          txCbor: forcedCanonicalCbor,
          programMaterialSidecarCbor,
          arrivalSeq: 0n,
          createdAt: new Date(0),
        },
      ],
      {
        expectedNetworkId: 0n,
        minFeeA: 0n,
        minFeeB: 0n,
        concurrency: 1,
        strictnessProfile: "phase1_midgard",
        consensusProfile: MIDGARD_CONSENSUS_PROFILE,
      },
    ),
  );
  const phaseARejection = phaseA.rejected[0];
  if (phaseARejection !== undefined)
    return forcedVerdictForRejection(phaseARejection);
  const phaseB = await Effect.runPromise(
    runPhaseBValidationWithPatch(
      [phaseA.accepted[0]!],
      new Map(
        ledger.map(([outRef, output]) => [outRef.toString("hex"), output]),
      ),
      { nowCardanoSlotNo, bucketConcurrency: 1, enforceScriptBudget: true },
    ),
  );
  const phaseBRejection = phaseB.rejected[0];
  return phaseBRejection === undefined
    ? "ForcedTxValid"
    : forcedVerdictForRejection(phaseBRejection);
};
