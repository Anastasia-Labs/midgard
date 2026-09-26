import { asDataType } from "@al-ft/midgard-core/lucid-data";
import {
  faultProofStepDatumSchema,
  FieldOpening,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Constr,
  Data,
  type LucidEvolution,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
} from "../field-opening.js";
import { submitLinearFaultCancel } from "../linear-fault-cancel.js";
import { submitLinearFaultFinalize } from "../linear-fault-finalize.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { submitMissingNativeScriptTxBinding } from "../missing-native-script-tx/submit-native-binding.js";
import {
  fetchUtxoByOutRef,
  parseOutRef,
  type ResolvedProverSigner,
} from "../runtime.js";
import { requireComputationThreadToken } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import type { ValueNotPreservedContracts } from "./contracts.js";
import type { ConservationAction, ConservationPosition } from "./union-plan.js";
import {
  ConservationAcceptedSourceRedeemer,
  ConservationClaim,
  ConservationSource,
  ConservationTerminalArgs,
  ConservationTerminalRedeemer,
} from "./union-schemas.js";

export const ConservationRawDatumSchema = faultProofStepDatumSchema(Data.Any());
type RawDatum = Data.Static<typeof ConservationRawDatumSchema>;
const RawDatum = asDataType<RawDatum>(ConservationRawDatumSchema);
export const conservationDatum = (
  owner: string,
  state: string | null,
): string =>
  Data.to(
    { fraud_prover: owner, data: state === null ? null : Data.from(state) },
    RawDatum,
  );
export const conservationStep = (
  contracts: ValueNotPreservedContracts,
  position: ConservationPosition | "entry",
) => (position === "entry" ? contracts.steps[0] : contracts[position]);
export type ConservationSubmissionAction = Omit<
  ConservationAction,
  "position" | "inputState"
> & {
  readonly position: ConservationPosition | "entry";
  readonly inputState: string | null;
};

/** One restartable transition; the live datum must equal the reconstructed checkpoint. */
export const submitConservationAction = async ({
  lucid,
  contracts,
  categoryId,
  headerHash,
  signer,
  threadOutRef,
  action,
  referenceScriptUtxo,
  field,
  chunkUtxos,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: ValueNotPreservedContracts;
  readonly categoryId: string;
  readonly headerHash: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly action: ConservationSubmissionAction;
  readonly referenceScriptUtxo: UTxO;
  readonly field?: {
    readonly planned: FaultProofFieldOpeningPlan;
    readonly carriageUtxos: readonly UTxO[];
    readonly certificateUtxo?: UTxO;
  };
  readonly chunkUtxos?: readonly UTxO[];
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const step = conservationStep(contracts, action.position);
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "conservation thread"),
    label: "value conservation thread",
  });
  if (
    threadUtxo.address !== step.spendingScriptAddress ||
    threadUtxo.datum == null ||
    Data.to(Data.from(threadUtxo.datum, RawDatum), RawDatum) !==
      conservationDatum(signer.paymentKeyHash, action.inputState)
  )
    throw new Error(
      "value conservation: live checkpoint differs from authenticated replay",
    );
  if (
    referenceScriptUtxo.scriptRef == null ||
    validatorToScriptHash(referenceScriptUtxo.scriptRef) !==
      step.spendingScriptHash
  )
    throw new Error("value conservation: wrong step reference script");
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId,
    categoryLabel: "value conservation",
  });
  if (threadToken.fraudulentHeaderHash !== headerHash)
    throw new Error("value conservation: evidence targets another header");
  signer.selectWallet(lucid);
  if (action.position === "unionTerminal") {
    const args = Data.from(action.args, ConservationTerminalArgs);
    const result = await submitLinearFaultFinalize({
      lucid,
      family: "value conservation",
      stepIndex: 16,
      step,
      computationThread: contracts.computationThread,
      fraudProof: contracts.fraudProof,
      signer,
      threadUtxo,
      threadToken,
      spendRedeemerSchema: ConservationTerminalRedeemer,
      buildFamilyArgs: ({
        inputIndex,
        outputIndex,
        fraudProofMintRedeemerIndex,
      }) => ({
        ...args,
        input_index: inputIndex,
        output_index: outputIndex,
        fraud_proof_mint_redeemer_index: fraudProofMintRedeemerIndex,
      }),
      referenceScriptUtxo,
      witnessReferenceScripts,
      preSubmitBoundary,
      awaitConfirmation,
    });
    return { kind: "proven" as const, ...result };
  }
  if (action.nextPosition === null || action.outputState === null)
    throw new Error("value conservation: incomplete continuation");
  const next = contracts[action.nextPosition];
  const nextDatum = conservationDatum(
    signer.paymentKeyHash,
    action.outputState,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: next.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "value conservation");
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "value conservation continuation",
    );
    const args = Data.from(action.args);
    if (!(args instanceof Constr))
      throw new Error("value conservation: malformed action arguments");
    args.fields[0] = requireInputIndex(ctx, threadUtxo, "value conservation");
    args.fields[1] = outputIndex;
    if (action.fieldIndex !== undefined) {
      if (field === undefined || field.planned.fieldIndex !== action.fieldIndex)
        throw new Error(
          "value conservation: missing authenticated field carriage",
        );
      args.fields[2] = Data.from(
        Data.to(
          faultProofFieldOpening({
            planned: field.planned,
            referenceInputs: ctx.referenceInputs,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            label: "value conservation",
          }),
          FieldOpening,
        ),
      );
    }
    if (action.position === "unionOutputScan" && chunkUtxos !== undefined) {
      args.fields[2] = new Constr(1, [
        chunkUtxos.map((utxo) =>
          requireReferenceInputIndex(
            ctx,
            utxo,
            "value conservation field chunk",
          ),
        ),
      ]);
    }
    return Data.to(new Constr(1, [args]));
  }) satisfies BuildTxWithRedeemer;
  const txHash = await submitLinearFaultContinue({
    lucid,
    signerPaymentKeyHash: signer.paymentKeyHash,
    threadUtxo,
    threadUnit: threadToken.unit,
    stepReference: referenceScriptUtxo,
    stepScript: step.spendingScript,
    stepRole: `value conservation ${action.position}`,
    nextAddress: next.spendingScriptAddress,
    nextDatum,
    redeemer,
    carriageUtxos: [...(field?.carriageUtxos ?? []), ...(chunkUtxos ?? [])],
    extraReferenceInputs:
      field?.certificateUtxo === undefined ? [] : [field.certificateUtxo],
    preSubmitBoundary,
    awaitConfirmation,
  });
  if (outputIndex === undefined)
    throw new Error("value conservation: output layout was not resolved");
  return {
    kind: "advanced" as const,
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex}`,
  };
};

export const submitConservationCancel = (
  params: Omit<
    Parameters<typeof submitLinearFaultCancel>[0],
    "family" | "steps" | "computationThread"
  > & { readonly contracts: ValueNotPreservedContracts },
) =>
  submitLinearFaultCancel({
    ...params,
    family: "value conservation",
    steps: [
      ...params.contracts.steps,
      params.contracts.unionAcceptedSource,
      params.contracts.unionForcedSource,
      params.contracts.unionEvent,
      params.contracts.unionPreState,
      params.contracts.unionInputs,
      params.contracts.unionInputValue,
      params.contracts.unionAssets,
      params.contracts.unionFieldGrammar,
      params.contracts.unionOutputs,
      params.contracts.unionOutputScan,
      params.contracts.unionMint,
      params.contracts.unionUpdate,
      params.contracts.unionTerminal,
    ],
    computationThread: params.contracts.computationThread,
  });

/** The carried native source door also supports published maximum MPF proofs. */
export const submitConservationAcceptedSource = async (
  params: Omit<
    Parameters<typeof submitMissingNativeScriptTxBinding>[0],
    | "contracts"
    | "stepIndex"
    | "nextDatum"
    | "spendRedeemerSchema"
    | "wrapInclusionArgs"
    | "wrapInclusionCarriage"
  > & {
    readonly contracts: ValueNotPreservedContracts;
    readonly source: ConservationSource;
  },
) => {
  if (
    params.source.claim === "ForcedConservation" ||
    params.txInclusion.nativeTx.validity_code !== 0n ||
    params.source.transaction_id !== params.txInclusion.nativeTxId
  )
    throw new Error("value conservation: wrong accepted source");
  if (
    params.threadUtxo.address !==
      params.contracts.unionAcceptedSource.spendingScriptAddress ||
    params.threadUtxo.datum == null ||
    Data.to(Data.from(params.threadUtxo.datum, RawDatum), RawDatum) !==
      conservationDatum(
        params.signer.paymentKeyHash,
        Data.to(params.source.claim, ConservationClaim),
      )
  )
    throw new Error("value conservation: accepted claim checkpoint differs");
  return await submitMissingNativeScriptTxBinding({
    ...params,
    contracts: {
      ...params.contracts,
      steps: [
        params.contracts.unionAcceptedSource,
        params.contracts.unionEvent,
      ],
    },
    stepIndex: 0,
    nextDatum: conservationDatum(
      params.signer.paymentKeyHash,
      Data.to(params.source, ConservationSource),
    ),
    spendRedeemerSchema: ConservationAcceptedSourceRedeemer,
    wrapInclusionCarriage: (inclusion) => inclusion,
  });
};
