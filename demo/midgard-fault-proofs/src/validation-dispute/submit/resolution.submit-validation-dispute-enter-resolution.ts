import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ValidationBoundarySpendRedeemer,
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
  type ValidationMachineState,
  type ValidationTraceProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../step-support.js";
import { computationThreadOutputPredicate } from "../../tx-layout.js";
import { witnessSpendingValidatorCarriage } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { type ContinueLayout, makeGameHandoffRedeemer } from "./redeemers.js";
import { VALIDATION_SEMANTIC_RESOLVER_COUNTS } from "./reference-scripts.js";
import { requireDisputeDatum } from "./reveal.js";
import {
  requireL1ProofEnvelope,
  threadAssets,
} from "./transaction-material.js";
import {
  reachOptionalPreSubmitBoundary,
  requireValidityRange,
  type ValidationDisputeValidityRange,
  validationDisputeValidityRange,
} from "./validity.js";

export const makePrepareResolutionRedeemer = ({
  threadUtxo,
  outputAddress,
  outputDatum,
  threadUnit,
  resolverIndex,
  preState,
  operatorPost,
  challengerPost,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly outputAddress: string;
  readonly outputDatum: string;
  readonly threadUnit: string;
  readonly resolverIndex: bigint;
  readonly preState: ValidationMachineState;
  readonly operatorPost: ValidationTraceProof;
  readonly challengerPost: ValidationTraceProof;
  readonly onLayout: (layout: ContinueLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "validation dispute prepare resolution",
    );
    const layout: ContinueLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "validation dispute prepare resolution",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        computationThreadOutputPredicate({
          address: outputAddress,
          datum: outputDatum,
          unit: threadUnit,
        }),
        "validation dispute prepare resolution",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            PrepareResolution: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              resolver_index: resolverIndex,
              evidence: {
                pre_state: preState,
                operator_post: operatorPost,
                challenger_post: challengerPost,
              },
            },
          },
        ],
      },
      ValidationBoundarySpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export type SubmitValidationDisputeEnterResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValidationDisputeEnterResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  gameReferenceScriptUtxo,
  validityRange = validationDisputeValidityRange(Date.now()),
  awaitConfirmation = true,
  preSubmitBoundary,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** The mandatory published V1 validation-trace game script. */
  readonly gameReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputeEnterResolutionResult> => {
  const range = requireValidityRange(validityRange);
  const { validationTraceDisputeCategory, contracts } =
    await resolveValidationTraceDisputeDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation-dispute game UTxO",
  });
  const gameContract = contracts.validationTraceDispute.game;
  if (threadUtxo.address !== gameContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute game validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before one-step resolution",
    );
  }
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute resolution handoff requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const outputDatum = Data.to(inputDatum, ValidationDisputeDatum);
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const gameScriptCarriage = witnessSpendingValidatorCarriage({
    script: gameContract.spendingScript,
    referenceUtxo: gameReferenceScriptUtxo,
    label: "validation-dispute game validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makeGameHandoffRedeemer({
        threadUtxo,
        outputAddress:
          contracts.validationTraceDispute.boundary.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        destination: "resolution",
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      contracts.validationTraceDispute.boundary.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const withReferenceScript =
    gameScriptCarriage.referenceInputs.length === 0
      ? base
      : base.readFrom([...gameScriptCarriage.referenceInputs]);
  const tx = gameScriptCarriage.attach(withReferenceScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute resolution handoff layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute resolution handoff",
  );
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: "validation-dispute game validator",
        utxo: gameReferenceScriptUtxo,
      },
    ],
  });
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const VALIDATION_RESOLVER_PHASES = [
  "CanonicalDecode",
  "CompactBinding",
  "StaticLedgerRules",
  "InputSets",
  "Signatures",
  "PhaseANativeScripts",
  "PhaseAScriptPreconditions",
  "ResolveInputs",
  "ScriptSources",
  "NativeScripts",
  "ScriptIntegrity",
  "Cek",
  "ValueAndMint",
  "LedgerDelta",
] as const satisfies readonly ValidationMachineState["phase"][];

export const validationResolverIndex = (
  phase: ValidationMachineState["phase"],
): number => {
  const resolverIndex = VALIDATION_RESOLVER_PHASES.indexOf(
    phase as (typeof VALIDATION_RESOLVER_PHASES)[number],
  );
  if (resolverIndex < 0) {
    throw new Error(`Validation phase ${phase} has no one-step resolver`);
  }
  return resolverIndex;
};

/**
 * Every one of the fourteen resolver indices is a `prepare_selected`
 * validator since R5 item 1 split the cek and ValueAndMint direct resolvers,
 * so the prepare-resolver deployment order is the resolver order itself.
 */
export const validationPrepareResolverDeploymentIndex = (
  resolverIndex: number,
): number => {
  if (
    resolverIndex >= 0 &&
    resolverIndex < VALIDATION_SEMANTIC_RESOLVER_COUNTS.length
  ) {
    return resolverIndex;
  }
  throw new Error(
    `Validation resolver ${resolverIndex.toString()} is not staged`,
  );
};

export type SubmitValidationDisputePrepareResolutionResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};
