import { planMidgardFieldCarriage } from "@al-ft/midgard-core";
import {
  type CommittedFieldClaim,
  committedFieldShapeEvidenceFromCommittedField,
  CommittedFieldShapeStep01SpendRedeemer,
  type CommittedFieldShapeStep02State,
  type FieldCarriage,
  HUB_ORACLE_ASSET_NAME,
  isCommittedFieldShapeViolation,
  type NativeTxInclusionCarriage,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { faultProofRawFieldCarriage } from "../field-opening.js";
import {
  builtChunkReferenceIndices,
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  type PublishedProofChunk,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { CommittedFieldShapeContracts } from "./contracts.js";
import { deriveCommittedFieldShapeNextOutput } from "./derive-committed-field-shape-next-output.js";
import type { PreparedCommittedFieldShape } from "./prepare-committed-field-shape.js";
import {
  committedFieldShapeClaimDetails,
  STEP_LABEL,
  type SubmitCommittedFieldShapeStep01Result,
} from "./submit-committed-field-shape-step-01.committed-field-shape-claim-details.js";
import {
  committedFieldShapeSubmitError,
  requireCommittedFieldShapeReferenceScript,
  requireCommittedFieldShapeThreadUtxo,
} from "./submit-common.js";

/**
 * Authenticates one committed slot and walks one batch of its envelope: the
 * thread returns to step 01 while items remain and moves to step 02 with the
 * on-chain-derived verdict. Every prepared member is re-derived here; a stale or
 * edited plan cannot be converted into a transaction contradicting the
 * validator.
 */
export const submitCommittedFieldShapeStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  prepared,
  publishedProofChunks,
  publishedFieldCarriageUtxos,
  fieldCertificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: CommittedFieldShapeContracts;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly prepared: Pick<
    PreparedCommittedFieldShape,
    "claim" | "evidence" | "step02State"
  >;
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  /** Authenticated §8.7 raw field publications, in deterministic plan order. */
  readonly publishedFieldCarriageUtxos?: readonly UTxO[];
  /** Authenticated §8.6 certificate for a tier-3 committed field. */
  readonly fieldCertificateUtxo?: UTxO;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitCommittedFieldShapeStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireCommittedFieldShapeThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  const [stateQueueBlockUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${STEP_LABEL} state-queue block UTxO`,
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${STEP_LABEL} hub oracle`,
    }),
  ]);
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw committedFieldShapeSubmitError(
      `state-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);
  const { fieldIndex, inlinePreimage, witnessSetHash } =
    committedFieldShapeClaimDetails(prepared.claim);
  // Tier-2/3 claims name no bytes; the prepared evidence carries them and the
  // verdict re-derivation below rejects any disagreement.
  const preimage =
    inlinePreimage ?? Buffer.from(prepared.evidence.committedPreimage, "hex");
  if (
    witnessSetHash !== undefined &&
    witnessSetHash !== txInclusion.nativeTx.witness_set_hash
  ) {
    throw committedFieldShapeSubmitError(
      `witness claim hashes to compact witness set ${witnessSetHash}, not the committed ${txInclusion.nativeTx.witness_set_hash}.`,
    );
  }
  const evidence = committedFieldShapeEvidenceFromCommittedField({
    badTxId: txInclusion.nativeTxId,
    fieldIndex,
    committedPreimage: preimage,
  });
  if (!evidence.isViolation) {
    throw committedFieldShapeSubmitError(
      `field ${fieldIndex.toString()} earns non-convicting verdict ${evidence.verdictName}; an honest or disjoint fault cannot advance.`,
    );
  }
  const derivedState: CommittedFieldShapeStep02State = {
    bad_tx_id: evidence.badTxId,
    field_index: BigInt(evidence.fieldIndex),
    verdict: BigInt(evidence.verdict),
  };
  if (
    evidence.badTxId !== prepared.evidence.badTxId ||
    evidence.fieldIndex !== prepared.evidence.fieldIndex ||
    evidence.fieldStride !== prepared.evidence.fieldStride ||
    evidence.committedPreimage !== prepared.evidence.committedPreimage ||
    evidence.committedPreimageByteCount !==
      prepared.evidence.committedPreimageByteCount ||
    evidence.verdict !== prepared.evidence.verdict ||
    evidence.verdictName !== prepared.evidence.verdictName ||
    evidence.isViolation !== prepared.evidence.isViolation ||
    derivedState.bad_tx_id !== prepared.step02State.bad_tx_id ||
    derivedState.field_index !== prepared.step02State.field_index ||
    derivedState.verdict !== prepared.step02State.verdict ||
    !isCommittedFieldShapeViolation({
      fieldIndex: Number(derivedState.field_index),
      verdict: Number(derivedState.verdict),
    })
  ) {
    throw committedFieldShapeSubmitError(
      "prepared evidence/state does not equal the verdict re-derived from the claimed committed bytes.",
    );
  }

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({ walletUtxos, chunks }),
  );
  const validatedStepReference = requireCommittedFieldShapeReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: carriedByChunks ? chunkedVerifyScript : phasMembershipScript,
    referenceUtxo: carriedByChunks
      ? witnessReferenceScripts?.chunkedVerifyWithdraw
      : witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${STEP_LABEL} ${carriedByChunks ? "chunked verify" : "PHAS membership"}`,
  });
  const fieldPlan = planMidgardFieldCarriage({
    owner: Buffer.from(signer.paymentKeyHash, "hex"),
    txId: Buffer.from(txInclusion.nativeTxId, "hex"),
    fieldIndex,
    preimage,
  });
  const fieldPublications = publishedFieldCarriageUtxos ?? [];
  if (fieldPublications.length !== fieldPlan.publications.length) {
    throw committedFieldShapeSubmitError(
      `${fieldPlan.tier} carriage requires exactly ${fieldPlan.publications.length.toString()} authenticated publication UTxOs, received ${fieldPublications.length.toString()}.`,
    );
  }
  if (
    (fieldPlan.tier === "Certified") !==
    (fieldCertificateUtxo !== undefined)
  ) {
    throw committedFieldShapeSubmitError(
      "only a Certified carriage may carry the exact field certificate UTxO.",
    );
  }
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
    ...fieldPublications,
    ...(fieldCertificateUtxo === undefined ? [] : [fieldCertificateUtxo]),
    validatedStepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const claimWithCarriage = (carriage: FieldCarriage): CommittedFieldClaim =>
    "BodyFieldClaim" in prepared.claim
      ? { BodyFieldClaim: { ...prepared.claim.BodyFieldClaim, carriage } }
      : {
          WitnessFieldClaim: { ...prepared.claim.WitnessFieldClaim, carriage },
        };
  const nextOutput = deriveCommittedFieldShapeNextOutput({
    threadUtxo,
    signer,
    contracts,
    badTxId: evidence.badTxId,
    fieldIndex,
    committedPreimage: preimage,
  });
  if (
    nextOutput.step02State !== null &&
    (nextOutput.step02State.bad_tx_id !== derivedState.bad_tx_id ||
      nextOutput.step02State.field_index !== derivedState.field_index ||
      nextOutput.step02State.verdict !== derivedState.verdict)
  ) {
    throw committedFieldShapeSubmitError(
      "the walk's final step-02 state differs from the verdict over the whole field.",
    );
  }
  const outputMatches = computationThreadOutputPredicate({
    address: nextOutput.address,
    datum: nextOutput.datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    const common = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      hub_ref_input_index: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        `${STEP_LABEL} hub oracle`,
      ),
      state_queue_node_ref_input_index: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        `${STEP_LABEL} state-queue node`,
      ),
      native_tx_id: txInclusion.nativeTxId,
      l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
    };
    const resolvedChunkIndices = builtChunkReferenceIndices({
      ctx,
      chunks,
      label: STEP_LABEL,
    });
    const inclusion: NativeTxInclusionCarriage = carriedByChunks
      ? {
          PublishedChunkInclusion: [
            {
              ...common,
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedInclusion: [
            {
              ...common,
              tx_membership_proof: txInclusion.txMembershipProof,
              inclusion_proof_script_withdraw_redeemer_index:
                requireWithdrawalRedeemerIndex(
                  ctx,
                  phasRewardAddress,
                  `${STEP_LABEL} PHAS membership`,
                ),
            },
          ],
        };
    return Data.to(
      {
        Continue: [
          {
            inclusion,
            claim: claimWithCarriage(
              // Carriage indices name positions in the builder's final
              // reference-input order, never in a hand-assembled list.
              faultProofRawFieldCarriage({
                plan: fieldPlan,
                referenceInputs: ctx.referenceInputs,
                ...(fieldPlan.tier === "Certified"
                  ? {
                      certificatePolicyId:
                        contracts.fieldPreimageCertificatePolicyId,
                    }
                  : {}),
                label: `${STEP_LABEL} committed field`,
              }),
            ),
          },
        ],
      },
      CommittedFieldShapeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs);
  const withCarriage = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((ctx) =>
        chunkedMembershipClaimRedeemer({
          merkleRoot: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.l2TransactionSourceCbor,
          orderedChunkReferenceInputIndices: builtChunkReferenceIndices({
            ctx,
            chunks,
            label: STEP_LABEL,
          }),
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        phasRewardAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.l2TransactionSourceCbor,
          membershipProofCbor: txInclusion.txMembershipProofCbor,
        }),
      );
  const paid = withCarriage.pay
    .ToContract(
      nextOutput.address,
      { kind: "inline", value: nextOutput.datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = membershipCarriage.attach(paid);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw committedFieldShapeSubmitError(
      "BuildTxWithRedeemer did not resolve the step-01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof committed-field-shape step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
        {
          role: "membership proof withdrawal",
          utxo: witnessReferenceScripts?.phasMembershipWithdraw,
          expectedScript: phasMembershipScript,
        },
        {
          role: "V1 MPF chunked-verify withdrawal",
          utxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
          expectedScript: chunkedVerifyScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw committedFieldShapeSubmitError(
      `step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    nextStepAddress: nextOutput.address,
    complete: nextOutput.complete,
    evidence,
    step02State: nextOutput.step02State,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
    awaitedConfirmation: awaitConfirmation,
  };
};
