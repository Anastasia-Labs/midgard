/** The validation-trace dispute runner at a step of a phase one-step-phase.ts
 * resolves, on the emulator, over a deployment that installs each such
 * phase's resolver and semantic resolvers. The successor's thread disputes
 * the step: the runner plays it through the phase resolver, which commits
 * the transition and its auxiliary evidence (none, or the item of a field
 * carried inline), to the semantic resolver the evidence selects, which
 * awards it, invoking beside an item the yield its kind selects
 * (validation-trace-dispute-one-step-resolution-drive.ts). */
import type { MidgardVersionedScript } from "@al-ft/midgard-core/codec/versioned-script";
import {
  plutusConstrFieldCbor,
  replacePlutusConstrFieldCbor,
} from "@al-ft/midgard-core/plutus-data-cbor";
import { buildValidationOneStepArgument } from "@al-ft/midgard-validation";
import { Constr, Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

import { NO_AUXILIARY_WITNESS_CBOR } from "../../src/validation-trace-dispute/one-step-canonical-decode.js";
import {
  PHASE_CHALLENGER_SUCCESSOR_HASH,
  PHASE_RESOLUTIONS,
  phaseSemanticHop,
  phaseYieldListReads,
  phaseYieldReads,
  type ValidationTracePhase,
  type ValidationTracePhaseHolder,
  type ValidationTracePhaseYield,
} from "../../src/validation-trace-dispute/one-step-phase.js";
import {
  planValidationTraceOneStep,
  type ValidationTracePhasePlan,
} from "../../src/validation-trace-dispute/one-step-plan.js";
import type { ScenarioSigners } from "../helpers/plutus-execution-block-fixture.js";
import type { TwoThreadCase } from "./validation-trace-dispute-one-step-case.js";
import { resolutionDrive } from "./validation-trace-dispute-one-step-resolution-drive.js";

/** `SignerMembershipProof`'s constructor among the signer-set proofs. */
const SIGNER_MEMBERSHIP_PROOF = 1;

/** `TransactionFieldItemWitness`'s constructor among the auxiliary
 * witnesses. */
const TRANSACTION_FIELD_ITEM_WITNESS = 28;

/** The evidence a step's semantic resolver takes: none, the item
 * `itemIndex` of the field `fieldIndex` (by default whichever item our
 * trace reads at the step; a step whose control fixes the item takes the
 * field alone), whose preimage it carries inline, with the
 * yield `itemYield` its semantic resolver invokes beside it, at its
 * `yield_kind` `kind`, if any; or a required-signer item, whose field it
 * carries inline beside the proof that the signer signed; or our trace's
 * own auxiliary witness at the step, of the constructor `auxiliary`, whose
 * fields the semantic resolver takes, or, with `yieldList`, from which it
 * derives its evidence, invoking the yields `yieldList` in order; or that
 * auxiliary witness whole beside its role `role`, with the yield
 * `itemYield` of that role; or the descriptor step claim of that auxiliary
 * witness, which opens its item's header or tail as `opens` says, with the
 * yield `itemYield` that verifies it. */
export type PhaseEvidence =
  | undefined
  | Readonly<{
      auxiliary: number;
      yieldList?: readonly ValidationTracePhaseYield[];
    }>
  | Readonly<{
      auxiliary: number;
      role: number;
      itemYield: PhaseItemYield;
    }>
  | Readonly<{
      auxiliary: number;
      opens: "header" | "tail";
      itemYield: PhaseItemYield;
    }>
  | Readonly<{
      fieldIndex: number;
      itemIndex?: number;
      itemYield?: PhaseItemYield;
    }>
  | Readonly<{ requiredSigner: true }>;

/** A yield a semantic resolver invokes beside its evidence, at its
 * `yield_kind` `kind`, if it takes one. */
type PhaseItemYield = Readonly<{
  kind?: number;
  name: ValidationTracePhaseYield;
}>;

/** The fields of a redeemer item control but its traversal, which a
 * descriptor step claim carries. */
const NARROW_CONTROL_FIELDS = 15;

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
  action,
  evidence,
  at,
  minFeeA,
  failingRedeemerData,
  failingReferenceInputs,
  failingSigners,
  failingScript,
  failingNativeSpend,
  failingNativeMint,
}: {
  readonly from: number;
  readonly phase: ValidationTracePhase;
  readonly semantic?: number;
  /** The constructor of the semantic resolver's action, when not its
   * first. */
  readonly action?: number;
  readonly evidence?: PhaseEvidence;
  readonly at: ValidationTracePhaseHolder;
  /** The ledger's per-byte fee while the resolution plays. */
  readonly minFeeA?: number;
  /** The failing transaction's redeemer, as Plutus Data CBOR. */
  readonly failingRedeemerData?: Uint8Array;
  /** How many reference inputs the failing transaction reads; by default
   * none. */
  readonly failingReferenceInputs?: number;
  /** Who signs the failing transaction beside its owner. */
  readonly failingSigners?: ScenarioSigners;
  /** The script the failing transaction's spend runs. */
  readonly failingScript?: MidgardVersionedScript;
  /** Whether the failing transaction also spends an output a native script
   * holds. */
  readonly failingNativeSpend?: boolean;
  /** Whether the failing transaction also mints under a native script's
   * policy. */
  readonly failingNativeMint?: boolean;
}) => {
  const resolution = PHASE_RESOLUTIONS[phase];
  const semanticHolder: ValidationTracePhaseHolder =
    resolution.semantics[semantic]!;
  const itemYield =
    evidence !== undefined && "itemYield" in evidence
      ? evidence.itemYield
      : undefined;
  const yieldList =
    evidence !== undefined && "yieldList" in evidence
      ? evidence.yieldList
      : undefined;
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
      ...(failingRedeemerData === undefined ? {} : { failingRedeemerData }),
      ...(failingReferenceInputs === undefined
        ? {}
        : { failingReferenceInputs }),
      ...(failingSigners === undefined ? {} : { failingSigners }),
      ...(failingScript === undefined ? {} : { failingScript }),
      ...(failingNativeSpend === undefined ? {} : { failingNativeSpend }),
      ...(failingNativeMint === undefined ? {} : { failingNativeMint }),
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

    /** The semantic resolver's hop invoking, beside the item, the yield of
     * the other kind, `other`, in place of the one its kind selects. */
    otherYield: async (other: PhaseItemYield) =>
      await drive.rewrittenHop((_, plan) => {
        const hop = phaseSemanticHop({
          prover: "",
          transitionCbor: plan.transitionCbor,
          evidenceCbors: plan.phase.evidenceCbors,
          itemYield: phaseYieldReads(drive.dispute().deployment, {
            ...plan.phase,
            itemYield: other,
          })!,
        });
        return { redeemer: hop.redeemer, reads: hop.reads! };
      }),

    /** The semantic resolver's hop invoking the yields `other` in place of
     * those its evidence names. */
    otherYieldList: async (other: readonly ValidationTracePhaseYield[]) =>
      await drive.rewrittenHop((_, plan) => {
        const hop = phaseSemanticHop({
          prover: "",
          transitionCbor: plan.transitionCbor,
          evidenceCbors: plan.phase.evidenceCbors,
          yieldList: phaseYieldListReads(drive.dispute().deployment, {
            ...plan.phase,
            yieldList: other,
          })!,
        });
        return hop.reads === undefined
          ? { redeemer: hop.redeemer }
          : { redeemer: hop.redeemer, reads: hop.reads };
      }),

    /** The semantic resolver's hop whose descriptor step claim opens the
     * other end of its item than the step does. */
    otherOpening: async () =>
      await drive.rewrittenHop((honest) => ({
        redeemer: (inputIndex, outputIndex, context) => {
          const redeemer = honest.redeemer(inputIndex, outputIndex, context);
          const opens = [0, 3, 1];
          const tail = Data.from<Constr<Data>>(
            plutusConstrFieldCbor(redeemer, opens),
          ).index;
          return replacePlutusConstrFieldCbor(
            redeemer,
            opens,
            Data.to(new Constr(1 - Number(tail), [])),
          );
        },
      })),

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
      const auxiliary = Data.from<Constr<Data>>(plan.auxiliaryCbor);
      const expected = await expectedEvidence(
        drive.dispute(),
        from,
        evidence,
        auxiliary,
      );
      expect(auxiliary).toStrictEqual(expected);
      const { evidenceCbors, ...holders } = plan.phase;
      expect(holders).toStrictEqual({
        phase,
        prepare: resolution.prepare,
        semantic: semanticHolder,
        ...(action === undefined ? {} : { action }),
        ...(itemYield === undefined ? {} : { itemYield }),
        ...(yieldList === undefined ? {} : { yieldList }),
      });
      const fields = evidenceCbors.map((field) => Data.from(field));
      if (evidence !== undefined && "role" in evidence)
        expect(fields).toStrictEqual([expected, BigInt(evidence.role)]);
      else if (evidence !== undefined && "opens" in evidence) {
        // The claim's control is the step's own, but its traversal, and it
        // opens the end of the item the step opens.
        const [claim] = fields as [Constr<Data>];
        const control = expected.fields[1] as Constr<Data>;
        expect(fields).toHaveLength(1);
        expect(claim.fields[0]).toStrictEqual(
          new Constr(0, control.fields.slice(0, NARROW_CONTROL_FIELDS)),
        );
        expect(claim.fields[1]).toStrictEqual(
          new Constr(evidence.opens === "tail" ? 1 : 0, []),
        );
      } else if (yieldList === undefined)
        expect(fields).toStrictEqual(expected.fields);
    },
  };
};

/** The auxiliary evidence of the step after our trace's state `from`: none,
 * `TransactionFieldChunkWitness` over the preimage of the field our trace
 * reads at the step, carried `Inline` (`TransactionFieldItemWitness`, the
 * carriage alone, where the step's control fixes the item), or
 * `RequiredSignerItemWitness` over
 * the required-signer field carried `Inline` and the signer's membership
 * among the transaction's signers (`plan`'s own proof, whose kind is
 * checked). */
const expectedEvidence = async (
  dispute: TwoThreadCase,
  from: number,
  evidence: PhaseEvidence,
  plan: Constr<Data>,
): Promise<Constr<Data>> => {
  if (evidence === undefined) return Data.from(NO_AUXILIARY_WITNESS_CBOR);
  const trace = await dispute.material.steps(from);
  if ("auxiliary" in evidence) {
    const own = Data.from<Constr<Data>>(
      buildValidationOneStepArgument({
        trace,
        stateIndex: from,
      }).auxiliaryCbor.toString("hex"),
    );
    expect(Number(own.index)).toBe(evidence.auxiliary);
    return own;
  }
  const auxiliary = trace.witnesses[from]!.auxiliary;
  if ("requiredSigner" in evidence) {
    if (auxiliary?.kind !== "requiredSignerItem")
      throw new Error("our trace reads no required signer at the step");
    const proof = plan.fields[1];
    expect(proof instanceof Constr && Number(proof.index)).toBe(
      SIGNER_MEMBERSHIP_PROOF,
    );
    return new Constr(2, [
      new Constr(0, [auxiliary.fieldPreimage.toString("hex")]),
      proof!,
    ]);
  }
  if (auxiliary?.kind === "transactionFieldItem") {
    expect(auxiliary.fieldIndex).toBe(evidence.fieldIndex);
    expect(evidence.itemIndex).toBeUndefined();
    return new Constr(TRANSACTION_FIELD_ITEM_WITNESS, [
      new Constr(0, [auxiliary.fieldPreimage.toString("hex")]),
    ]);
  }
  if (auxiliary?.kind !== "transactionFieldChunk")
    throw new Error("our trace reads no field item at the step");
  expect(auxiliary.fieldIndex).toBe(evidence.fieldIndex);
  if (evidence.itemIndex !== undefined)
    expect(auxiliary.itemIndex).toBe(evidence.itemIndex);
  return new Constr(1, [
    BigInt(evidence.fieldIndex),
    BigInt(auxiliary.itemIndex),
    new Constr(0, [auxiliary.fieldPreimage.toString("hex")]),
  ]);
};
