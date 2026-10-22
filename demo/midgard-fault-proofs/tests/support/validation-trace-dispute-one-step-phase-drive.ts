/** The validation-trace dispute runner at a step of a phase one-step-phase.ts
 * resolves, on the emulator, over a deployment that installs each such
 * phase's resolver and semantic resolvers. The successor's thread disputes
 * the step: the runner plays it through the phase resolver, which commits
 * the transition and its auxiliary evidence (none, or the item of a field
 * carried inline), to the semantic resolver the evidence selects, which
 * awards it (validation-trace-dispute-one-step-resolution-drive.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { Constr, Data } from "@lucid-evolution/lucid";
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
import type { TwoThreadCase } from "./validation-trace-dispute-one-step-case.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";

/** The evidence a step's semantic resolver takes: none, or the item
 * `itemIndex` of the field `fieldIndex`, whose preimage it carries inline. */
export type PhaseEvidence =
  | undefined
  | Readonly<{ fieldIndex: number; itemIndex: number }>;

/**
 * The drive of the successor's thread through the step of `phase` after our
 * trace's state `from`, whose evidence selects the phase's semantic
 * resolver `semantic` beside `evidence`, set up with the thread standing at
 * `at`.
 */
export const phaseDrive = ({
  from,
  phase,
  semantic = 0,
  evidence,
  at,
  minFeeA,
  failingReferenceInputs,
}: {
  readonly from: number;
  readonly phase: ValidationTracePhase;
  readonly semantic?: number;
  readonly evidence?: PhaseEvidence;
  readonly at: ValidationTracePhaseHolder;
  /** The ledger's per-byte fee while the resolution plays. */
  readonly minFeeA?: number;
  /** How many reference inputs the failing transaction reads; by default
   * none. */
  readonly failingReferenceInputs?: number;
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
    installs: {
      phases: true,
      ...(failingReferenceInputs === undefined
        ? {}
        : { failingReferenceInputs }),
    },
    ...(minFeeA === undefined ? {} : { minFeeA }),
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
    bytesOf: drive.bytesOf,
    unbuilt: drive.unbuilt,
    otherTransition: drive.otherTransition,

    /** Our plan sends the thread's step through the phase's resolver to the
     * semantic resolver its evidence selects, committing the transition
     * beside that evidence: none, or the item of the field our trace reads
     * at the step, the field's preimage carried inline. */
    routes: async () => {
      const plan = await drive.plan();
      expect(plan.stateIndex).toBe(from);
      expect(plan.preState.phase).toBe(phase);
      expect(plan.resolverIndex).toBe(resolution.resolverIndex);
      expect(plan.semanticResolverIndex).toBe(semantic);
      const expected = await expectedEvidence(drive.dispute(), from, evidence);
      expect(Data.from(plan.auxiliaryCbor)).toStrictEqual(expected);
      const { evidenceCbors, ...holders } = plan.phase;
      expect(holders).toStrictEqual({
        phase,
        prepare: resolution.prepare,
        semantic: semanticHolder,
      });
      expect(evidenceCbors.map((field) => Data.from(field))).toStrictEqual(
        expected.fields,
      );
    },
  };
};

/** The auxiliary evidence of the step after our trace's state `from`: none,
 * or `TransactionFieldChunkWitness` over the preimage of the field our
 * trace reads at the step, carried `Inline`. */
const expectedEvidence = async (
  dispute: TwoThreadCase,
  from: number,
  evidence: PhaseEvidence,
): Promise<Constr<Data>> => {
  if (evidence === undefined) return Data.from(NO_AUXILIARY_WITNESS_CBOR);
  const auxiliary = (await dispute.material.trace()).witnesses[from]!.auxiliary;
  if (auxiliary?.kind !== "transactionFieldChunk")
    throw new Error("our trace reads no field item at the step");
  expect(auxiliary.fieldIndex).toBe(evidence.fieldIndex);
  expect(auxiliary.itemIndex).toBe(evidence.itemIndex);
  return new Constr(1, [
    BigInt(evidence.fieldIndex),
    BigInt(evidence.itemIndex),
    new Constr(0, [auxiliary.fieldPreimage.toString("hex")]),
  ]);
};
