import {
  inspectMidgardRedeemerSequenceHeads,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core";
import {
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { buildDataRefusalTrace } from "./redeemer-data-refusal-deployed.build-trace.js";

it.each([
  ["580100", 0, false],
  ["d8799f01580100ff", 4, false],
  ["580100", 0, true],
  ["d8799f01580100ff", 4, true],
] as const)(
  "keeps forced source %s unchanged and names its Data fault at %s before source discovery (missing=%s)",
  async (data, offset, missingScript) => {
    const fixture = await buildDataRefusalTrace(data, undefined, missingScript);
    const phaseA = await Effect.runPromise(
      runPhaseAValidation(
        [
          {
            sourceKind: "forced",
            txId: fixture.transaction.txId,
            txCbor: fixture.txCbor,
            programMaterialSidecarCbor: fixture.programMaterialSidecarCbor,
            arrivalSeq: 0n,
            createdAt: new Date(0),
          },
        ],
        {
          consensusProfile: MIDGARD_CONSENSUS_PROFILE,
          expectedNetworkId: 0n,
          minFeeA: 0n,
          minFeeB: 0n,
          concurrency: 1,
          strictnessProfile: "phase-a-unit",
        },
      ),
    );
    expect(phaseA.rejected).toEqual([]);
    expect(phaseA.accepted).toHaveLength(1);
    const retained = phaseA.accepted[0]!.ledgerTx.redeemers[0]!.dataCbor;
    expect(retained.toString("hex")).toBe(data);
    expect(inspectMidgardRedeemerSequenceHeads(retained)).toEqual({
      kind: "refusal",
      offset,
    });
    const phaseB = await Effect.runPromise(
      runPhaseBValidationWithPatch(
        phaseA.accepted,
        new Map(
          fixture.ledgerWitnessEntries.map(({ outRef, output }) => [
            outRef.toString("hex"),
            output,
          ]),
        ),
        { nowCardanoSlotNo: 0n, bucketConcurrency: 1 },
      ),
    );
    expect(phaseB.accepted).toEqual([]);
    expect(phaseB.rejected).toEqual([
      expect.objectContaining({
        code: "E_INVALID_FIELD_TYPE",
        consensusPhase: "scriptSources",
        subject: { arm: "RedeemerMalformed", index: 0n },
      }),
    ]);
    expect(phaseB.rejected[0]!.detail).toBe(
      `noncanonical redeemer Data at 0:${offset}`,
    );
  },
);
