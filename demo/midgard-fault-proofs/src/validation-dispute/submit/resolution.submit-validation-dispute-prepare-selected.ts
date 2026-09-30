import { PreparedValidationResolutionDatum } from "@al-ft/midgard-sdk";
import {
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
import { witnessSpendingValidatorCarriage } from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import { type ValidationOneStepSubmissionArgument } from "./evidence.js";
import { type ContinueLayout } from "./redeemers.js";
import {
  hasValidationAuxiliaryShape,
  requireStagedOneStepArgument,
  requireValidationCanonicalDecodePrepareReferenceScriptUtxo,
  VALIDATION_AUXILIARY_SHAPES,
} from "./reference-scripts.js";
import {
  validationPrepareResolverDeploymentIndex,
  validationResolverIndex,
} from "./resolution.submit-validation-dispute-enter-resolution.js";
import {
  requireResolutionDatum,
  type SubmitValidationDisputePrepareSelectedResult,
} from "./resolution.submit-validation-dispute-prepare-resolution.js";
import { makePrepareSelectedRedeemer } from "./semantic-redeemers.js";
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

export const submitValidationDisputePrepareSelected = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  oneStepArgument,
  referenceScriptUtxo,
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
  readonly oneStepArgument: ValidationOneStepSubmissionArgument;
  /** Explicit prepare-resolver reference; otherwise resolved from deployment info. */
  readonly referenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputePrepareSelectedResult> => {
  const range = requireValidityRange(validityRange);
  const {
    deploymentInfo: parsedDeploymentInfo,
    validationTraceDisputeCategory,
    contracts,
  } = await resolveValidationTraceDisputeDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
  });
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "validation prepare-resolver UTxO",
  });
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireResolutionDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation semantic preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const resolverIndex = validationResolverIndex(
    inputDatum.data.pre_state.phase,
  );
  if (resolverIndex !== oneStepArgument.resolverIndex) {
    throw new Error(
      "Validation one-step argument does not match the authenticated phase resolver",
    );
  }
  const staged = requireStagedOneStepArgument(oneStepArgument);
  const isPrepareCompleteCanonicalItem =
    oneStepArgument.resolverIndex === 0 &&
    staged.semanticResolverIndex === 1 &&
    hasValidationAuxiliaryShape(
      staged.auxiliary,
      VALIDATION_AUXILIARY_SHAPES.transactionFieldItem,
    );
  // Option B (#620): the prepare-selected redeemer never carries the auxiliary
  // — the canonical-decode validator computes the transition-only evidence hash
  // itself — so no preimage bytes ride in this transaction on any tier and the
  // retired by-hash escape (#597's envelope-pressure valve) has nothing left to
  // relieve.
  const prepareContract =
    contracts.validationTraceDispute.prepareResolvers[
      validationPrepareResolverDeploymentIndex(resolverIndex)
    ];
  const semanticContract =
    contracts.validationTraceDispute.semanticResolvers[
      staged.semanticResolverGlobalIndex
    ];
  if (prepareContract === undefined || semanticContract === undefined) {
    throw new Error("Validation staged resolver deployment is incomplete");
  }
  if (threadUtxo.address !== prepareContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at resolver ${resolverIndex.toString()}`,
    );
  }
  // The complete-canonical-item step transaction sources the prepare-resolver
  // validator from the published reference script (#617 follow-up to #597
  // ruling a). Option B removed the tier-1 preimage from this redeemer, but
  // the ~5.6 KiB applied validator body still must not ride inside the
  // 16,384-byte L1 envelope.
  const prepareReferenceScriptUtxo =
    referenceScriptUtxo ??
    (isPrepareCompleteCanonicalItem
      ? await requireValidationCanonicalDecodePrepareReferenceScriptUtxo({
          lucid,
          deploymentInfo: parsedDeploymentInfo,
          expectedScriptHash: prepareContract.spendingScriptHash,
        })
      : undefined);
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        resolution: inputDatum.data,
        evidence_hash: staged.evidenceHash,
      },
    },
    PreparedValidationResolutionDatum,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const prepareScriptCarriage = witnessSpendingValidatorCarriage({
    script: prepareContract.spendingScript,
    referenceUtxo: prepareReferenceScriptUtxo,
    label: "validation-dispute prepare-resolver validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareSelectedRedeemer({
        threadUtxo,
        outputAddress: semanticContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex,
        semanticResolverIndex: staged.semanticResolverIndex,
        transition: staged.transition,
        auxiliary: staged.auxiliaryWitness,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      semanticContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const withReferenceScript =
    prepareScriptCarriage.referenceInputs.length === 0
      ? base
      : base.readFrom([...prepareScriptCarriage.referenceInputs]);
  const tx = prepareScriptCarriage.attach(withReferenceScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation semantic preparation layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), "Validation semantic preparation");
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: "validation-dispute prepare-resolver validator",
        utxo: prepareReferenceScriptUtxo,
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
    resolverIndex,
    semanticResolverIndex: staged.semanticResolverIndex,
    semanticResolverGlobalIndex: staged.semanticResolverGlobalIndex,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
