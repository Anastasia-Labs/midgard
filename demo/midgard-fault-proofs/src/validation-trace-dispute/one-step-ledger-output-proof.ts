/** The evidence of a ledger-output proof step and of its finalize, at a
 * resolve-inputs membership or a script-sources output proof, whose
 * semantic resolvers delegate to the shared ledger-output-proof yields: a
 * step names the stage yield its pre control selects
 * (`ledger_output_proof_roles.stage_role`) and the scalar attestation
 * yields that stage reads (`stage_attestation_roles`), beside the pre and
 * successor controls; a finalize names the descriptor yields of the fact
 * group it attaches (`descriptor_role`), or none at the thin terminal. */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";
import { ValidationOneStepWitness } from "@al-ft/midgard-sdk";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  deriveLedgerOutputProofFinalizePlan,
  deriveLedgerOutputProofStepPlan,
} from "../ledger-output-proof-plan.js";
import {
  bytes as itemBytes,
  controlData,
  items,
} from "../ledger-output-proof-plan.ledger-output-proof-datum-role-index.js";
import { bytes, integer } from "./one-step-data.js";

/** The stage yields, by `stage_role` index. */
const STAGE_YIELDS = Object.freeze([
  "ledgerOutputProofStructure",
  "ledgerOutputProofValue",
  "ledgerOutputProofDatumFoldMap",
  "ledgerOutputProofDatumFinalizeFrame",
  "ledgerOutputProofDatumHeadScalar",
  "ledgerOutputProofDatumAttachInteger",
  "ledgerOutputProofDatumFoldList",
  "ledgerOutputProofDatumAdvanceInteger",
  "ledgerOutputProofReferenceScript",
  "ledgerOutputProofScriptHash",
  "ledgerOutputProofNativeScript",
  "ledgerOutputProofStructureAssets",
  "ledgerOutputProofStructureOptional",
  "ledgerOutputProofStructureFinish",
  "ledgerOutputProofDatumHeadSequence",
  "ledgerOutputProofDatumHeadMap",
  "ledgerOutputProofDatumHeadLargeConstructor",
  "ledgerOutputProofDatumAttachBytes",
  "ledgerOutputProofDatumAdvanceBytes",
  "ledgerOutputProofDatumFinish",
  "ledgerOutputProofDatumLargeConstructor",
  "ledgerOutputProofDatumLargeFields",
  "ledgerOutputProofDatumClose",
  "ledgerOutputProofSpan",
] as const);

/** The scalar attestation yields, by the attestation a stage reads. */
const ATTESTATION_YIELDS = Object.freeze({
  scalarInteger: "ledgerOutputProofScalarInteger",
  scalarBytes: "ledgerOutputProofScalarBytes",
} as const);

/** The descriptor yields, by `descriptor_role` index. */
const DESCRIPTOR_YIELDS = Object.freeze([
  "ledgerOutputDescriptorScanFacts",
  "ledgerOutputDescriptorReferenceScript",
  "ledgerOutputDescriptorDatumSummary",
  "ledgerOutputDescriptorValueSummary",
] as const);

/** Every yield a membership step may invoke. */
export const MEMBERSHIP_STEP_YIELDS = Object.freeze([
  ...STAGE_YIELDS,
  ...Object.values(ATTESTATION_YIELDS),
]);

/** Every yield a membership finalize may invoke. */
export const MEMBERSHIP_FINALIZE_YIELDS = DESCRIPTOR_YIELDS;

export type LedgerOutputProofYield =
  | (typeof MEMBERSHIP_STEP_YIELDS)[number]
  | (typeof MEMBERSHIP_FINALIZE_YIELDS)[number];

/** A step's evidence: the semantic resolver's fields after the transition,
 * as their Data's CBOR, and the yields whose reference-input indices follow
 * them, in order. */
export type LedgerOutputProofEvidence = Readonly<{
  evidenceCbors: readonly string[];
  yieldList: readonly LedgerOutputProofYield[];
}>;

/** `LedgerOutputProofStepWitness`'s constructor among the auxiliary
 * witnesses. */
const STEP_AUXILIARY = 30;

/** `LedgerOutputProofFinalizeWitness`'s. */
const FINALIZE_AUXILIARY = 31;

const auxiliaryIndexOf = (auxiliaryCbor: string): number | undefined => {
  const auxiliary = Data.from(auxiliaryCbor);
  return auxiliary instanceof Constr ? Number(auxiliary.index) : undefined;
};

/** The descriptor the pending input of the resolve-inputs control in
 * `workCbor` commits. */
const pendingDescriptorCbor = (workCbor: string): string =>
  itemBytes(
    items(
      controlData(
        itemBytes(items(controlData(workCbor), 11, "control")[9], "pending"),
      ),
      5,
      "pending input",
    )[3],
    "pending descriptor",
  );

/** What a ledger-output proof step's or finalize's evidence is derived
 * from. */
type EvidenceSource = Readonly<{
  transitionCbor: string;
  auxiliaryCbor: string;
  successorWitnessCbor: Uint8Array | undefined;
}>;

/**
 * The step evidence of the resolver `resolverIndex`'s semantic resolver
 * `semanticResolverIndex` (`VerifyMembershipStep`,
 * `VerifyOutputProofStep`) for the step whose transition is
 * `transitionCbor` beside `auxiliaryCbor`: the proof witness, the pre
 * control, the successor control (empty when the step rejects; else read
 * from the successor's work witness `successorWitnessCbor`), the claimed
 * scalar, the stage role, and the stage yield then its attestations.
 */
const outputProofStepEvidence =
  (resolverIndex: number, semanticResolverIndex: number) =>
  ({
    transitionCbor,
    auxiliaryCbor,
    successorWitnessCbor,
  }: EvidenceSource): LedgerOutputProofEvidence => {
    if (auxiliaryIndexOf(auxiliaryCbor) !== STEP_AUXILIARY)
      throw new Error(
        "validation-trace output proof step carries evidence other than an output proof step",
      );
    const plan = deriveLedgerOutputProofStepPlan({
      resolverIndex,
      semanticResolverIndex,
      transitionCbor: Buffer.from(transitionCbor, "hex"),
      auxiliaryCbor: Buffer.from(auxiliaryCbor, "hex"),
      ...(successorWitnessCbor === undefined
        ? {}
        : { ledgerOutputProofSuccessorWorkWitnessCbor: successorWitnessCbor }),
    });
    const stage = STAGE_YIELDS[plan.roleIndex];
    if (stage === undefined)
      throw new Error("validation-trace output proof stage has no yield");
    return Object.freeze({
      evidenceCbors: Object.freeze([
        plutusConstrFieldCbor(auxiliaryCbor, [0]),
        bytes(plan.controlCbor),
        bytes(plan.nextControlCbor),
        Data.to(plan.claimedScalar),
        integer(plan.roleIndex),
      ]),
      yieldList: Object.freeze([
        stage,
        ...plan.attestationRoles.map((role) => ATTESTATION_YIELDS[role]),
      ]),
    });
  };

/**
 * The finalize evidence of the resolver `resolverIndex`'s semantic resolver
 * `semanticResolverIndex` (`VerifyMembershipFinalize`,
 * `VerifyOutputProofFinalize`) for the step whose transition is
 * `transitionCbor` beside `auxiliaryCbor`: the descriptor under proof (read
 * from the step's work witness and the terminal descriptor its control
 * determines by `descriptorOf`), the signer proof, the terminal control,
 * the claimed value and datum summaries, the fact group it attaches, and
 * that group's descriptor yields (none at the thin terminal).
 */
const outputProofFinalizeEvidence =
  (
    resolverIndex: number,
    semanticResolverIndex: number,
    descriptorOf: (workCbor: string, terminalDescriptorCbor: string) => string,
  ) =>
  ({
    transitionCbor,
    auxiliaryCbor,
  }: EvidenceSource): LedgerOutputProofEvidence => {
    if (auxiliaryIndexOf(auxiliaryCbor) !== FINALIZE_AUXILIARY)
      throw new Error(
        "validation-trace output proof finalize carries evidence other than an output proof finalize",
      );
    const plan = deriveLedgerOutputProofFinalizePlan({
      resolverIndex,
      semanticResolverIndex,
      transitionCbor: Buffer.from(transitionCbor, "hex"),
    });
    const { work_witness_cbor: workCbor } = Data.from(
      transitionCbor,
      ValidationOneStepWitness,
    );
    return Object.freeze({
      evidenceCbors: Object.freeze([
        bytes(descriptorOf(workCbor, plan.descriptorCbor)),
        plutusConstrFieldCbor(auxiliaryCbor, [0]),
        bytes(plan.controlCbor),
        Data.to(plan.claimedValueSummary),
        Data.to(plan.claimedDatumSummary),
        Data.to(plan.attachRoles.map(BigInt)),
      ]),
      yieldList: Object.freeze(
        plan.attachRoles.map((role) => {
          const name = DESCRIPTOR_YIELDS[role];
          if (name === undefined)
            throw new Error("validation-trace descriptor role has no yield");
          return name;
        }),
      ),
    });
  };

/** A resolve-inputs membership step's evidence. */
export const membershipStepEvidence = outputProofStepEvidence(7, 3);

/** A resolve-inputs membership finalize's evidence, over the descriptor
 * its pending input commits. */
export const membershipFinalizeEvidence = outputProofFinalizeEvidence(
  7,
  4,
  pendingDescriptorCbor,
);

/** A script-sources output proof step's evidence. */
export const scriptSourcesOutputProofStepEvidence = outputProofStepEvidence(
  8,
  2,
);

/** A script-sources output proof finalize's evidence, over the terminal
 * descriptor its control determines. */
export const scriptSourcesOutputProofFinalizeEvidence =
  outputProofFinalizeEvidence(8, 3, (_, terminal) => terminal);
