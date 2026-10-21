/** The validation-trace dispute runner at a step of a phase one-step-phase.ts
 * resolves, on the emulator, over a deployment that installs each such
 * phase's resolver and semantic resolvers. The successor's thread disputes
 * the step: the runner plays it through the phase resolver, which commits
 * the transition alone, to the semantic resolver the evidence selects,
 * which awards it (validation-trace-dispute-one-step-resolution-drive.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { NO_AUXILIARY_WITNESS_CBOR } from "../../src/validation-trace-dispute/one-step-canonical-decode.js";
import {
  PHASE_CHALLENGER_SUCCESSOR_HASH,
  PHASE_RESOLUTIONS,
  type ValidationTracePhase,
  type ValidationTracePhaseHolder,
} from "../../src/validation-trace-dispute/one-step-phase.js";
import {
  planValidationTraceOneStep,
  type ValidationTracePhasePlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";

/**
 * The drive of the successor's thread through the step of `phase` after our
 * trace's state `from`, whose evidence selects the phase's semantic
 * resolver `semantic`, set up with the thread standing at `at`.
 */
export const phaseDrive = ({
  from,
  phase,
  semantic = 0,
  at,
}: {
  readonly from: number;
  readonly phase: ValidationTracePhase;
  readonly semantic?: number;
  readonly at: ValidationTracePhaseHolder;
}) => {
  const resolution = PHASE_RESOLUTIONS[phase];
  const semanticHolder: ValidationTracePhaseHolder =
    resolution.semantics[semantic]!;
  const drive = resolutionDrive({
    from,
    at,
    path: [resolution.prepare, semanticHolder],
    labels: [
      `PrepareSelected ${phase}`,
      resolution.semantics.length === 1
        ? `Verify ${phase}`
        : `Verify ${phase} ${semantic.toString()}`,
    ],
    installs: { phases: true },
    /** Our plan of the step, by the successor hash the holder keeps. */
    planAt: async (
      dispute,
      holder,
      utxo,
    ): Promise<ValidationTracePhasePlan> => {
      const planning = await planValidationTraceOneStep({
        material: dispute.material,
        challengerHighHash: Data.from<string>(
          plutusConstrFieldCbor(utxo.datum!, [
            ...PHASE_CHALLENGER_SUCCESSOR_HASH[holder],
          ]),
        ),
        phases: [phase],
      });
      if (planning.kind !== "plan" || planning.plan.kind !== "phase")
        throw new Error(`the thread's step is not a ${phase} step`);
      return planning.plan;
    },
  });

  return {
    dispute: drive.dispute,
    setUp: drive.setUp,
    awards: drive.awards,
    pays: drive.pays,
    otherTransition: drive.otherTransition,

    /** Our plan sends the thread's step through the phase's resolver to the
     * semantic resolver its evidence selects, committing the transition
     * alone. */
    routes: async () => {
      const plan = await drive.plan();
      expect(plan.stateIndex).toBe(from);
      expect(plan.preState.phase).toBe(phase);
      expect(plan.resolverIndex).toBe(resolution.resolverIndex);
      expect(plan.semanticResolverIndex).toBe(semantic);
      expect(plan.auxiliaryCbor).toBe(NO_AUXILIARY_WITNESS_CBOR);
      expect(plan.phase).toStrictEqual({
        phase,
        prepare: resolution.prepare,
        semantic: semanticHolder,
      });
    },
  };
};
