import { Data, type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";

import { isPrerequisiteTurn } from "../fault-proof-turn.js";
import {
  type FaultProofFieldGrammarDeployment,
  routeFaultProofFieldGrammar,
} from "../field-grammar-certificate.route-fault-proof-field-grammar.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import type { ResolvedProverSigner } from "../runtime.js";
import {
  SignerWitnessScanDatumSchema,
  SignerWitnessTerminalDatumSchema,
} from "../signer-witness-scan/schemas.js";
import {
  SIGNER_WITNESS_SCAN_STEP_INDEX,
  SIGNER_WITNESS_TERMINAL_STEP_INDEX,
} from "../signer-witness-scan/submit-scan.js";
import type { ProofMaterialsAccess } from "../workflow/proof-materials-view.js";
import type { ProtectedOutputSignerMissingContracts } from "./contracts.js";
import { actuateProtectedOutputSignerFieldOpening } from "./field-opening-actuation.js";
import { planProtectedOutputSignerOutputOpening } from "./field-plans.js";
import {
  type ProtectedOutputSignerMissingEvidence,
  requireProtectedOutputSignerScanEvidence,
} from "./protected-output-signer-missing.js";
import {
  ProtectedOutputSignerStep02DatumSchema,
  ProtectedOutputSignerStep02RedeemerSchema,
} from "./schemas.js";
import { buildProtectedOutputSignerOpeningTransition } from "./submit-opening-transition.js";

export const buildProtectedOutputSignerMissingStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  fieldGrammar,
  materials,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ProtectedOutputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ProtectedOutputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo: UTxO;
  /** The field grammar certificate, for an outputs field the walk cannot read. */
  readonly fieldGrammar?: FaultProofFieldGrammarDeployment;
  /** The workflow's view of its proof-materials address, on a workflow. */
  readonly materials?: ProofMaterialsAccess;
}) => {
  const current = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "protected-output-signer-missing",
    stepIndex: 1,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    bound: { subject: unknown; output_index: bigint };
    witness_set_hash: string;
  }>({
    threadUtxo: current.threadUtxo,
    signer,
    schema: ProtectedOutputSignerStep02DatumSchema as never,
    family: "protected-output-signer-missing",
    stepIndex: 1,
  });
  if (
    state.bound.output_index !== evidence.outputIndex ||
    state.witness_set_hash !== evidence.witnessSetHashHex
  )
    throw new Error(
      "protected-output-signer-missing: output coordinate or witness anchor changed",
    );
  const planned = planProtectedOutputSignerOutputOpening({
    evidence,
    nativeTxCompactCbor,
    owner: signer.paymentKeyHash,
  });
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "protected-output-signer-missing",
    stepIndex: 1,
  });
  const actuated = await actuateProtectedOutputSignerFieldOpening({
    materials,
    lucid,
    contracts,
    signer,
    planned,
    nativeTxCompactCbor,
    witnessSetCompactCbor,
    stepReference,
    certificateReferenceScriptUtxo,
    label: "protected-output-signer-missing outputs",
  });
  if (isPrerequisiteTurn(actuated)) return actuated;
  const coordinateOutOfRange = evidence.route === "coordinate_out_of_range";
  // The out-of-range arm reads the field's item count; every other arm reads
  // the bound output.
  const grammar = await routeFaultProofFieldGrammar({
    lucid,
    signer,
    plan: planned.plan,
    read: coordinateOutOfRange ? "count" : "item",
    itemCount: planned.itemCount,
    source: {
      sourceKind: planned.sourceKind,
      compactCbor: nativeTxCompactCbor,
      witnessSetCompactCbor,
    },
    carriageUtxos: [
      ...actuated.carriageUtxos,
      ...(actuated.certificateUtxo === undefined
        ? []
        : [actuated.certificateUtxo]),
    ],
    fieldPreimageCertificatePolicyId:
      contracts.fieldPreimageCertificatePolicyId,
    fieldGrammar,
    label: "protected-output-signer-missing outputs",
  });
  // The witness-scan route writes the unopened scan state (`start`); every
  // direct route writes the verdict step 02 derives on chain
  // (`direct_verdict_v1`) straight to the terminal.
  const scan = evidence.route === "witness_scan";
  const nextDatum = scan
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            witness_set_hash: evidence.witnessSetHashHex,
            payment_credential:
              requireProtectedOutputSignerScanEvidence(evidence)
                .paymentCredentialHex,
            checkpoint_hash: "",
          },
        } as never,
        SignerWitnessScanDatumSchema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            signer_required: false,
            signer_present: false,
          },
        } as never,
        SignerWitnessTerminalDatumSchema as never,
      );
  const result = await buildProtectedOutputSignerOpeningTransition({
    lucid,
    contracts,
    categoryId,
    signer,
    threadOutRef,
    stepIndex: 1,
    nextStepIndex: scan
      ? SIGNER_WITNESS_SCAN_STEP_INDEX
      : SIGNER_WITNESS_TERMINAL_STEP_INDEX,
    nextDatum,
    openingFor: actuated.openingFor,
    coordinateOutOfRange,
    grammarRefInputIndex: grammar.grammarRefInputIndex,
    referenceScriptUtxo,
    carriageReferenceInputs: [
      ...actuated.referenceInputs,
      ...grammar.referenceInputs,
    ],
    redeemerSchema: ProtectedOutputSignerStep02RedeemerSchema as never,
  });
  return {
    ...result,
    route: evidence.route,
    stage: scan ? ("scanning" as const) : ("terminal" as const),
  };
};
