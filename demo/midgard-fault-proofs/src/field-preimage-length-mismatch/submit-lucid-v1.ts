import {
  acceptedVerdictSubjectV1,
  type CommittedFieldClaimV1,
  FieldPreimageLengthStep01DatumV1Schema,
  FieldPreimageLengthStep01RedeemerV1Schema,
  FieldPreimageLengthStep02DatumV1Schema,
  FieldPreimageLengthStep02RedeemerV1Schema,
  FieldPreimageLengthStep03DatumV1Schema,
  FieldPreimageLengthStep03RedeemerV1Schema,
  type ForcedInclusionTxV1,
  forcedVerdictSubjectV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  type HeaderV1,
  HUB_ORACLE_ASSET_NAME,
  type OutputReference,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  type RootMembershipProof,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import { submitCommittedFieldShapeCancel } from "../committed-field-shape/submit-committed-field-shape-cancel.js";
import { submitCommittedFieldShapeInit } from "../committed-field-shape/submit-committed-field-shape-init.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireComputationThreadToken,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../tx-layout.js";
import {
  witnessMintingPolicyCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import type { FraudProofPreSubmitBoundaryV1 } from "../workflow/transaction-boundary-v1.js";
import {
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { ManifestBoundFieldPreimageLengthConfigV1 } from "./production-config-v1.js";
import type { PreparedFieldPreimageLengthWorkflowV1 } from "./workflow-v1.js";

const LABEL = "field-preimage-length-mismatch";

const initAdapterContracts = (
  config: ManifestBoundFieldPreimageLengthConfigV1,
) => {
  const chain = config.contracts.fieldPreimageLengthMismatch;
  return {
    steps: [chain.steps[0], chain.steps[1]] as const,
    computationThread: config.contracts.computationThread,
    fraudProof: config.contracts.fraudProof,
    hubOraclePolicyId: config.binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId: config.binding.definition.stateQueue.policyId,
    fieldPreimageCertificatePolicyId:
      config.contracts.fieldPreimageCertificate.policyId,
  };
};

/** Generic registered-category Init, specialized to this family's first step. */
export const submitFieldPreimageLengthInitV1 = async ({
  config,
  fraudulentBlockOutRef,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly fraudulentBlockOutRef: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) =>
  await submitCommittedFieldShapeInit({
    lucid: config.lucid,
    blueprint: config.binding.blueprint,
    network: config.binding.network,
    contracts: initAdapterContracts(config),
    category: config.binding.resolvedContracts.category,
    catalogue: config.binding.catalogue,
    signer: config.signer,
    fraudulentBlockOutRef,
    fraudulentHeaderHash: config.binding.definition.headerHash,
    witnessReferenceScripts: config.referenceScripts.witnesses,
    preSubmitBoundary,
    awaitConfirmation,
  });

/** Real generic cancel, adapted to one of the four physical validators. */
export const submitFieldPreimageLengthCancelV1 = async ({
  config,
  threadOutRef,
  stepIndex,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const step = config.contracts.fieldPreimageLengthMismatch.steps[stepIndex];
  const contracts = {
    ...initAdapterContracts(config),
    steps: [step, step] as const,
  };
  const referenceScriptUtxo =
    stepIndex === 0
      ? config.referenceScripts.step01
      : stepIndex === 1
        ? config.referenceScripts.step02Accepted
        : stepIndex === 2
          ? config.referenceScripts.step02Forced
          : config.referenceScripts.step03;
  return await submitCommittedFieldShapeCancel({
    lucid: config.lucid,
    contracts,
    categoryId: config.binding.resolvedContracts.category.categoryId,
    signer: config.signer,
    threadOutRef,
    referenceScriptUtxo,
    witnessReferenceScripts: config.referenceScripts.witnesses,
    preSubmitBoundary,
    awaitConfirmation,
  });
};

const requireReference = ({
  utxo,
  expectedHash,
  role,
}: {
  readonly utxo: UTxO;
  readonly expectedHash: string;
  readonly role: string;
}): UTxO => {
  if (utxo.scriptRef == null) {
    throw new Error(`${LABEL}: ${role} reference carries no script`);
  }
  const actual = validatorToScriptHash(utxo.scriptRef);
  if (actual !== expectedHash) {
    throw new Error(
      `${LABEL}: ${role} reference hashes to ${actual}, expected ${expectedHash}`,
    );
  }
  return utxo;
};

const requireThread = async ({
  config,
  threadOutRef,
  stepIndex,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly stepIndex: 0 | 1 | 2 | 3;
}) => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid: config.lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: `${LABEL} thread`,
  });
  const step = config.contracts.fieldPreimageLengthMismatch.steps[stepIndex];
  if (threadUtxo.address !== step.spendingScriptAddress) {
    throw new Error(
      `${LABEL}: thread ${outRefLabel(threadUtxo)} is not at physical step ${(
        stepIndex + 1
      ).toString()}`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: config.contracts.computationThread.policyId,
    categoryId: config.binding.resolvedContracts.category.categoryId,
    categoryLabel: LABEL,
  });
  return { threadUtxo, threadToken, step };
};

export type SubmitFieldPreimageLengthForcedDispatchV1Result = Readonly<{
  txHash: string;
  nextThreadOutRef: string;
  computationThreadUnit: string;
  inputIndex: number;
  outputIndex: number;
}>;

export type FieldPreimageLengthClaimResolverV1 = (
  completeReferenceInputs: readonly UTxO[],
) => CommittedFieldClaimV1;

/** Real accepted-source dispatch with an authenticated PHAS inclusion. */
export const submitFieldPreimageLengthAcceptedDispatchV1 = async ({
  config,
  threadOutRef,
  stateQueueBlockOutRef,
  inclusion: txInclusion,
  claim,
  claimResolver,
  carriageReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly inclusion: SubmitStep01TxInclusion;
  readonly claim?: CommittedFieldClaimV1;
  readonly claimResolver?: FieldPreimageLengthClaimResolverV1;
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchV1Result> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 0,
  });
  requireInitialStepDatum({ threadUtxo, signer: config.signer });
  requireNativeTxMatchesCompactCbor(txInclusion);
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid: config.lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${LABEL} state-queue block`,
    }),
    requireSingletonUtxo({
      lucid: config.lucid,
      address: credentialToAddress(
        config.binding.network,
        scriptHashToCredential(
          config.binding.resolvedContracts.hubOraclePolicyId,
        ),
      ),
      unit: toUnit(
        config.binding.resolvedContracts.hubOraclePolicyId,
        HUB_ORACLE_ASSET_NAME,
      ),
      label: `${LABEL} hub oracle`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: config.binding.definition.stateQueue.policyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(`${LABEL}: accepted source targets a different header`);
  }
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      config.binding.blueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasAddress = phasMembershipRewardAddress(
    config.binding.network,
    phasScript,
  );
  const carriage = witnessWithdrawalValidatorCarriageV1({
    script: phasScript,
    referenceUtxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
    label: `${LABEL} PHAS membership`,
  });
  const reference = requireReference({
    utxo: config.referenceScripts.step01,
    expectedHash: step.spendingScriptHash,
    role: "step-01",
  });
  const references = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    reference,
    ...carriage.referenceInputs,
    ...carriageReferenceInputs,
  ];
  const resolvedClaim =
    claimResolver?.(references) ??
    claim ??
    (() => {
      throw new Error(`${LABEL}: accepted dispatch omitted field claim`);
    })();
  const next = config.contracts.fieldPreimageLengthMismatch.acceptedStep02;
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: {
        BoundSource: {
          subject: acceptedVerdictSubjectV1(txInclusion.nativeTxId),
          source_cbor: txInclusion.l2TransactionSourceCbor,
        },
      },
    } as never,
    FieldPreimageLengthStep02DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: next.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} accepted dispatch`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} accepted output`,
      ),
    };
    const inclusion = {
      RedeemerCarriedInclusion: [
        {
          input_index: layout.inputIndex,
          output_index: layout.outputIndex,
          hub_ref_input_index: requireReferenceInputIndex(
            ctx,
            hubOracleUtxo,
            `${LABEL} hub oracle`,
          ),
          state_queue_node_ref_input_index: requireReferenceInputIndex(
            ctx,
            stateQueueBlockUtxo,
            `${LABEL} state queue`,
          ),
          native_tx_id: txInclusion.nativeTxId,
          l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
          transactions_phas_root: txInclusion.transactionsPhasRoot,
          tx_membership_proof: txInclusion.txMembershipProof,
          inclusion_proof_script_withdraw_redeemer_index:
            requireWithdrawalRedeemerIndex(
              ctx,
              phasAddress,
              `${LABEL} PHAS membership`,
            ),
        },
      ],
    };
    return Data.to(
      {
        Continue: [{ BindAccepted: { inclusion, claim: resolvedClaim } }],
      } as never,
      FieldPreimageLengthStep01RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const base = config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(references)
    .withdraw(
      phasAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(config.signer.paymentKeyHash);
  const unsigned = await carriage
    .attach(base)
    .complete({ localUPLCEval: true });
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length step-01",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
        {
          role: "membership proof withdrawal",
          utxo: config.referenceScripts.witnesses.phasMembershipWithdraw,
          expectedScript: phasScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  }
  if (awaitConfirmation) {
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};

/** Real Lucid step-01 forced dispatch; the direction comes from admitted evidence. */
export const submitFieldPreimageLengthForcedDispatchV1 = async ({
  config,
  threadOutRef,
  direction,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly direction: 0n | 1n;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchV1Result> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 0,
  });
  requireInitialStepDatum({ threadUtxo, signer: config.signer });
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const next = config.contracts.fieldPreimageLengthMismatch.forcedStep02;
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: { PendingForced: { direction } },
    } as never,
    FieldPreimageLengthStep02DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: next.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} forced dispatch`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} forced output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            RecordForced: {
              direction,
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep01RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step01,
    expectedHash: step.spendingScriptHash,
    role: "step-01",
  });
  const tx = config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      next.spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(config.signer.paymentKeyHash)
    .readFrom([reference]);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw new Error(`${LABEL}: forced dispatch layout did not resolve`);
  }
  const resolvedLayout = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length step-01",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  }
  if (awaitConfirmation) {
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
  };
};

/** Authenticates an accepted source's inline field opening into terminal state. */
export const submitFieldPreimageLengthAcceptedAuthenticationV1 = async ({
  config,
  threadOutRef,
  claim,
  claimResolver,
  prepared,
  carriageReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly claim?: CommittedFieldClaimV1;
  readonly claimResolver?: FieldPreimageLengthClaimResolverV1;
  readonly prepared: PreparedFieldPreimageLengthWorkflowV1;
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchV1Result> => {
  if (prepared.direction !== "wrongfulAcceptance") {
    throw new Error(
      `${LABEL}: accepted authenticator received forced evidence`,
    );
  }
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 1,
  });
  const terminal = config.contracts.fieldPreimageLengthMismatch.steps[3];
  const state = {
    subject: acceptedVerdictSubjectV1(prepared.transactionId),
    field_index: BigInt(prepared.fieldIndex),
    declared_length: BigInt(prepared.declaredLength),
    actual_length: BigInt(prepared.actualLength),
  };
  const datum = Data.to(
    { fraud_prover: config.signer.paymentKeyHash, data: state } as never,
    FieldPreimageLengthStep03DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: terminal.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} accepted auth`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} terminal output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            AuthenticateAccepted: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              claim: resolvedClaim,
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step02Accepted,
    expectedHash: step.spendingScriptHash,
    role: "accepted step-02",
  });
  const completeReferences = [reference, ...carriageReferenceInputs];
  const resolvedClaim =
    claimResolver?.(completeReferences) ??
    claim ??
    (() => {
      throw new Error(`${LABEL}: accepted authentication omitted field claim`);
    })();
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const unsigned = await config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      terminal.spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(config.signer.paymentKeyHash)
    .readFrom([reference, ...carriageReferenceInputs])
    .complete({ localUPLCEval: true });
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length accepted step-02",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  }
  if (awaitConfirmation) {
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};

export const submitFieldPreimageLengthForcedAuthenticationV1 = async ({
  config,
  threadOutRef,
  header,
  membership,
  claim,
  claimResolver,
  prepared,
  carriageReferenceInputs = [],
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly header: HeaderV1;
  readonly membership: RootMembershipProof<
    OutputReference,
    ForcedInclusionTxV1
  >;
  readonly claim?: CommittedFieldClaimV1;
  readonly claimResolver?: FieldPreimageLengthClaimResolverV1;
  readonly prepared: PreparedFieldPreimageLengthWorkflowV1;
  readonly carriageReferenceInputs?: readonly UTxO[];
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFieldPreimageLengthForcedDispatchV1Result> => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 2,
  });
  const verdict = membership.value.verdict;
  const rejectionReason =
    verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason;
  if (
    prepared.transactionId !== membership.value.tx_id ||
    (prepared.direction === "wrongfulAcceptance" ? 0n : 1n) !==
      (rejectionReason === null ? 0n : 1n)
  ) {
    throw new Error(`${LABEL}: forced leaf differs from admitted evidence`);
  }
  const subject = forcedVerdictSubjectV1({
    transactionId: membership.value.tx_id,
    sourceKey: membership.key,
    rejectionReason,
  });
  const terminal = config.contracts.fieldPreimageLengthMismatch.steps[3];
  const datum = Data.to(
    {
      fraud_prover: config.signer.paymentKeyHash,
      data: {
        subject,
        field_index: BigInt(prepared.fieldIndex),
        declared_length: BigInt(prepared.declaredLength),
        actual_length: BigInt(prepared.actualLength),
      },
    } as never,
    FieldPreimageLengthStep03DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: terminal.spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} forced auth`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} terminal output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            AuthenticateForced: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              header,
              membership,
              claim: resolvedClaim,
            },
          },
        ],
      } as never,
      FieldPreimageLengthStep02RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const reference = requireReference({
    utxo: config.referenceScripts.step02Forced,
    expectedHash: step.spendingScriptHash,
    role: "forced step-02",
  });
  const completeReferences = [reference, ...carriageReferenceInputs];
  const resolvedClaim =
    claimResolver?.(completeReferences) ??
    claim ??
    (() => {
      throw new Error(`${LABEL}: forced authentication omitted field claim`);
    })();
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const unsigned = await config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .pay.ToContract(
      terminal.spendingScriptAddress,
      { kind: "inline", value: datum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [threadToken.unit]: 1n },
    )
    .addSignerKey(config.signer.paymentKeyHash)
    .readFrom(completeReferences)
    .complete({ localUPLCEval: true });
  if (layout === undefined) throw new Error(`${LABEL}: layout did not resolve`);
  const resolved = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length forced step-02",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash)
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  if (awaitConfirmation)
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
  };
};

export const submitFieldPreimageLengthTerminalV1 = async ({
  config,
  threadOutRef,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly config: ManifestBoundFieldPreimageLengthConfigV1;
  readonly threadOutRef: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const { threadUtxo, threadToken, step } = await requireThread({
    config,
    threadOutRef,
    stepIndex: 3,
  });
  config.signer.selectWallet(config.lucid);
  const feeInput = selectFeeInput(await config.lucid.wallet().getUtxos());
  const fraudProofUnit = toUnit(
    config.contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const proofDatum = Data.to(
    { fraud_prover: config.signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const outputMatches = outputWithDatumAndUnitPredicate({
    address: config.contracts.fraudProof.spendingScriptAddress,
    datum: proofDatum,
    unit: fraudProofUnit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
      }
    | undefined;
  let threadMintIndex: bigint | undefined;
  const spend = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, `${LABEL} terminal`);
    layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${LABEL} proof output`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        config.contracts.fraudProof.policyId,
        `${LABEL} proof mint`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      } as never,
      FieldPreimageLengthStep03RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const burn = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.contracts.computationThread.policyId,
      `${LABEL} thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const mint = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.contracts.fraudProof.policyId,
      `${LABEL} proof mint`,
    );
    threadMintIndex = requireMintRedeemerIndex(
      ctx,
      config.contracts.computationThread.policyId,
      `${LABEL} thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: threadMintIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const ctCarriage = witnessMintingPolicyCarriageV1({
    script: config.contracts.computationThread.mintingScript,
    referenceUtxo: config.referenceScripts.witnesses.computationThreadMint,
    label: `${LABEL} thread mint`,
  });
  const proofCarriage = witnessMintingPolicyCarriageV1({
    script: config.contracts.fraudProof.mintingScript,
    referenceUtxo: config.referenceScripts.witnesses.fraudProofMint,
    label: `${LABEL} proof mint`,
  });
  const reference = requireReference({
    utxo: config.referenceScripts.step03,
    expectedHash: step.spendingScriptHash,
    role: "step-03",
  });
  const base = config.lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spend)
    .mintAssets({ [threadToken.unit]: -1n }, burn)
    .mintAssets({ [fraudProofUnit]: 1n }, mint)
    .pay.ToContract(
      config.contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: proofDatum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [fraudProofUnit]: 1n },
    )
    .addSignerKey(config.signer.paymentKeyHash)
    .readFrom([
      reference,
      ...ctCarriage.referenceInputs,
      ...proofCarriage.referenceInputs,
    ]);
  const unsigned = await proofCarriage
    .attach(ctCarriage.attach(base))
    .complete({ localUPLCEval: true });
  if (layout === undefined || threadMintIndex === undefined)
    throw new Error(`${LABEL}: terminal layout did not resolve`);
  const resolved = layout;
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 field-preimage-length terminal",
          utxo: reference,
          expectedScript: step.spendingScript,
        },
        {
          role: "thread mint",
          utxo: config.referenceScripts.witnesses.computationThreadMint,
          expectedScript: config.contracts.computationThread.mintingScript,
        },
        {
          role: "proof mint",
          utxo: config.referenceScripts.witnesses.fraudProofMint,
          expectedScript: config.contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash)
    throw new Error(`${LABEL}: provider returned a different transaction id`);
  if (awaitConfirmation)
    await config.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    fraudProofOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    fraudProofUnit,
  };
};

// Keep the initial datum schema live in this production module. It prevents a
// future ABI-only import cleanup from accidentally dropping the generic Init
// schema used by the bound builder set.
export const FIELD_PREIMAGE_LENGTH_INIT_DATUM_SCHEMA_V1 =
  FieldPreimageLengthStep01DatumV1Schema;
