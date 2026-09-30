import { Data } from "@lucid-evolution/lucid";

import { requireLinearFaultThreadUtxo } from "../../src/linear-fault-family.js";
import { submitLinearFaultFinalize } from "../../src/linear-fault-finalize.js";
import {
  UnusedScriptAuthenticatedWitnessSchema,
  UnusedScriptStep03DatumSchema,
  UnusedScriptStep04DatumSchema,
  UnusedScriptStep05DatumSchema,
  UnusedScriptStep05RedeemerSchema,
  UnusedScriptStep06DatumSchema,
  UnusedScriptStep06RedeemerSchema,
} from "../../src/unused-script-witness/schemas.js";
import type { FaultProofWitnessReferenceScripts } from "../../src/witness-reference-scripts.js";
import {
  type Common,
  continueRaw,
  datumOf,
  type PurposeOpening,
  type ScanState,
  stepState,
} from "./unused-script-witness-emulator.continue-raw.js";
import { FAMILY } from "./unused-script-witness-emulator.unused-script-witness-fixture-spec.js";

/** Step 05 with the openings, budget and next datum supplied verbatim. */
export const submitUnusedStep05Raw = async ({
  openings,
  itemBudget,
  next,
  ...common
}: Common & {
  readonly openings: readonly PurposeOpening[];
  readonly itemBudget: bigint;
  readonly next:
    | { readonly kind: "scan"; readonly state: ScanState }
    | {
        readonly kind: "decision";
        readonly state: Data.Static<
          typeof UnusedScriptStep06DatumSchema
        >["data"];
      };
}) =>
  await continueRaw({
    common,
    stepIndex: 4,
    nextAddress:
      common.contracts.steps[next.kind === "decision" ? 5 : 4]
        .spendingScriptAddress,
    nextDatum: datumOf(
      common,
      next.state,
      next.kind === "decision"
        ? UnusedScriptStep06DatumSchema
        : UnusedScriptStep05DatumSchema,
    ),
    redeemerSchema: UnusedScriptStep05RedeemerSchema,
    args: (input_index, output_index) => ({
      input_index,
      output_index,
      openings,
      item_budget: itemBudget,
    }),
  });

/** Step 06 without the off-chain contradiction guard: the validator decides. */
export const submitUnusedStep06Raw = async ({
  witnessReferenceScripts,
  ...common
}: Common & {
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
}) => {
  const stepIndex = 5;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid: common.lucid,
    contracts: common.contracts,
    categoryId: common.categoryId,
    family: FAMILY,
    stepIndex,
    threadOutRef: common.threadOutRef,
  });
  return await submitLinearFaultFinalize({
    lucid: common.lucid,
    family: FAMILY,
    stepIndex,
    step: common.contracts.steps[stepIndex],
    computationThread: common.contracts.computationThread,
    fraudProof: common.contracts.fraudProof,
    signer: common.signer,
    threadUtxo,
    threadToken,
    spendRedeemerSchema: UnusedScriptStep06RedeemerSchema,
    buildFamilyArgs: (layout) => ({
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    }),
    referenceScriptUtxo: common.referenceScriptUtxo,
    witnessReferenceScripts,
    awaitConfirmation: true,
  });
};

/** Reads the authenticated witness a step-03 thread currently carries. */
export const readUnusedAuthenticatedWitness = async (common: Common) =>
  await stepState<Data.Static<typeof UnusedScriptAuthenticatedWitnessSchema>>(
    common,
    2,
    UnusedScriptStep03DatumSchema,
  );

/** Reads the scan state a step-04/05 thread currently carries. */
export const readUnusedScanState = async (common: Common, stepIndex: 3 | 4) =>
  await stepState<ScanState>(
    common,
    stepIndex,
    stepIndex === 3
      ? UnusedScriptStep04DatumSchema
      : UnusedScriptStep05DatumSchema,
  );
