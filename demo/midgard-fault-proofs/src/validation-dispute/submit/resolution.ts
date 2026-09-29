import {
  PreparedValidationResolutionDatum,
  type PreparedValidationResolutionDatum as PreparedValidationResolutionDatumData,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  ValidationBoundarySpendRedeemer,
  validationDisputeCoreFromData,
  ValidationDisputeDatum,
  type ValidationMachineState,
  ValidationResolutionDatum,
  type ValidationResolutionDatum as ValidationResolutionDatumData,
  type ValidationTraceProof,
  WinningValidationResolutionDatum,
  type WinningValidationResolutionDatum as WinningValidationResolutionDatumData,
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
import { type ValidationOneStepSubmissionArgument } from "./evidence.js";
import { type ContinueLayout, makeGameHandoffRedeemer } from "./redeemers.js";
import {
  hasValidationAuxiliaryShape,
  requireStagedOneStepArgument,
  requireValidationCanonicalDecodePrepareReferenceScriptUtxo,
  VALIDATION_AUXILIARY_SHAPES,
  VALIDATION_SEMANTIC_RESOLVER_COUNTS,
} from "./reference-scripts.js";
import { requireDisputeDatum } from "./reveal.js";
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

const makePrepareResolutionRedeemer = ({
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
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            resolver_index: resolverIndex,
            evidence: {
              pre_state: preState,
              operator_post: operatorPost,
              challenger_post: challengerPost,
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
const validationPrepareResolverDeploymentIndex = (
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

export const submitValidationDisputePrepareResolution = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  preState,
  operatorPost,
  challengerPost,
  boundaryReferenceScriptUtxo,
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
  readonly preState: ValidationMachineState;
  readonly operatorPost: ValidationTraceProof;
  readonly challengerPost: ValidationTraceProof;
  /** The mandatory published V1 validation-trace boundary script. */
  readonly boundaryReferenceScriptUtxo?: UTxO;
  readonly validityRange?: ValidationDisputeValidityRange;
  readonly awaitConfirmation?: boolean;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
}): Promise<SubmitValidationDisputePrepareResolutionResult> => {
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
    label: "validation-dispute boundary UTxO",
  });
  const boundaryContract = contracts.validationTraceDispute.boundary;
  if (threadUtxo.address !== boundaryContract.spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at the validation-dispute boundary validator`,
    );
  }
  const token = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: validationTraceDisputeCategory.categoryId,
    categoryLabel: "validation-trace-dispute",
  });
  const inputDatum = requireDisputeDatum(threadUtxo);
  if (inputDatum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Validation-dispute boundary preparation requires fraud prover ${inputDatum.fraud_prover}, got ${signer.paymentKeyHash}`,
    );
  }
  const dispute = validationDisputeCoreFromData(inputDatum.data.dispute);
  if (dispute.turn.type !== "readyForOneStep") {
    throw new Error(
      "Validation dispute must finish bisection before boundary preparation",
    );
  }
  const resolverIndex = validationResolverIndex(preState.phase);
  const resolverContract =
    contracts.validationTraceDispute.resolvers[resolverIndex];
  if (resolverContract === undefined) {
    throw new Error(
      `Validation resolver ${resolverIndex.toString()} is missing from the deployment`,
    );
  }
  if (
    operatorPost.state_hash !== inputDatum.data.dispute.operator_high_hash ||
    challengerPost.state_hash !== inputDatum.data.dispute.challenger_high_hash
  ) {
    throw new Error(
      "Validation boundary successor proofs do not match the authenticated dispute",
    );
  }
  const outputDatum = Data.to(
    {
      fraud_prover: inputDatum.fraud_prover,
      data: {
        version: 1n,
        pre_state: preState,
        operator_successor_hash: operatorPost.state_hash,
        challenger_successor_hash: challengerPost.state_hash,
      },
    },
    ValidationResolutionDatum,
  );
  let layout: ContinueLayout | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const boundaryScriptCarriage = witnessSpendingValidatorCarriage({
    script: boundaryContract.spendingScript,
    referenceUtxo: boundaryReferenceScriptUtxo,
    label: "validation-dispute boundary validator",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom(
      [threadUtxo],
      makePrepareResolutionRedeemer({
        threadUtxo,
        outputAddress: resolverContract.spendingScriptAddress,
        outputDatum,
        threadUnit: token.unit,
        resolverIndex: BigInt(resolverIndex),
        preState,
        operatorPost,
        challengerPost,
        onLayout: (resolvedLayout) => {
          layout = resolvedLayout;
        },
      }),
    )
    .pay.ToContract(
      resolverContract.spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      threadAssets(threadUtxo, token.unit),
    )
    .validFrom(range.validFrom)
    .validTo(range.validTo)
    .addSignerKey(signer.paymentKeyHash);
  const withReferenceScript =
    boundaryScriptCarriage.referenceInputs.length === 0
      ? base
      : base.readFrom([...boundaryScriptCarriage.referenceInputs]);
  const tx = boundaryScriptCarriage.attach(withReferenceScript);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve validation-dispute boundary layout",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(
    signed.toCBOR(),
    "Validation-dispute boundary preparation",
  );
  await reachOptionalPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: "validation-dispute boundary validator",
        utxo: boundaryReferenceScriptUtxo,
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
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

const requireResolutionDatum = (
  threadUtxo: UTxO,
): ValidationResolutionDatumData & {
  readonly data: NonNullable<ValidationResolutionDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, ValidationResolutionDatum);
  if (datum.data === null) {
    throw new Error("Validation resolution requires initialized V1 state");
  }
  return datum as ValidationResolutionDatumData & {
    readonly data: NonNullable<ValidationResolutionDatumData["data"]>;
  };
};

export const requirePreparedResolutionDatum = (
  threadUtxo: UTxO,
): PreparedValidationResolutionDatumData & {
  readonly data: NonNullable<PreparedValidationResolutionDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Prepared validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, PreparedValidationResolutionDatum);
  if (datum.data === null) {
    throw new Error(
      "Prepared validation resolution requires initialized V1 state",
    );
  }
  return datum as PreparedValidationResolutionDatumData & {
    readonly data: NonNullable<PreparedValidationResolutionDatumData["data"]>;
  };
};

export const requireWinningResolutionDatum = (
  threadUtxo: UTxO,
): WinningValidationResolutionDatumData & {
  readonly data: NonNullable<WinningValidationResolutionDatumData["data"]>;
} => {
  if (threadUtxo.datum == null) {
    throw new Error(
      `Winning validation resolution UTxO ${outRefLabel(threadUtxo)} is missing datum`,
    );
  }
  const datum = Data.from(threadUtxo.datum, WinningValidationResolutionDatum);
  if (datum.data === null || datum.data.version !== 1n) {
    throw new Error(
      "Winning validation resolution requires canonical V1 state",
    );
  }
  return datum as WinningValidationResolutionDatumData & {
    readonly data: NonNullable<WinningValidationResolutionDatumData["data"]>;
  };
};

export type SubmitValidationDisputePrepareSelectedResult = {
  readonly txHash: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly resolverIndex: number;
  readonly semanticResolverIndex: number;
  readonly semanticResolverGlobalIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

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
