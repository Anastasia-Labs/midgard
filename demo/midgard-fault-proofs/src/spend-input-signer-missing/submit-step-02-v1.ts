import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core";
import {
  type FieldOpening,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening-v1.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary-v1.js";
import type { SpendInputSignerMissingContracts } from "./contracts-v1.js";
import { planSpendInputSignerInputOpening } from "./field-plans-v1.js";
import {
  SpendInputSignerStep02DatumSchema,
  SpendInputSignerStep02RedeemerSchema,
  SpendInputSignerStep03DatumSchema,
} from "./schemas-v1.js";
import type { SpendInputSignerMissingEvidence } from "./spend-input-signer-missing-v1.js";

export const submitSpendInputSignerMissingStep02 = async ({
  lucid,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  nativeTxCompactCbor,
  witnessSetCompactCbor,
  referenceScriptUtxo,
  certificateReferenceScriptUtxo,
  membershipReferenceScriptUtxo,
  publishCarriage = false,
  publicationBoundary,
  certificateBoundary,
  onCarriageReady,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: SpendInputSignerMissingContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: SpendInputSignerMissingEvidence;
  readonly nativeTxCompactCbor: string;
  readonly witnessSetCompactCbor: string;
  readonly referenceScriptUtxo: UTxO;
  readonly certificateReferenceScriptUtxo?: UTxO;
  readonly membershipReferenceScriptUtxo: UTxO;
  readonly publishCarriage?: boolean;
  readonly publicationBoundary?: FraudProofPreSubmitBoundary;
  readonly certificateBoundary?: FraudProofPreSubmitBoundary;
  readonly onCarriageReady?: () => Promise<void>;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 1;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "spend-input-signer-missing",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    input_index: bigint;
    prior_root: string;
    witness_set_hash: string;
  }>({
    threadUtxo,
    signer,
    schema: SpendInputSignerStep02DatumSchema as never,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (
    state.input_index !== BigInt(evidence.inputIndex) ||
    state.prior_root !== evidence.resolved.priorRoot ||
    state.witness_set_hash !== evidence.witnessSetHashHex
  )
    throw new Error(
      "spend-input-signer-missing: authenticated bind state changed",
    );
  if (evidence.resolved.membershipProof === undefined)
    throw new Error(
      "spend-input-signer-missing: production predecessor membership object is absent",
    );
  const planned = planSpendInputSignerInputOpening({
    evidence,
    nativeTxCompactCbor,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriage({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "spend-input-signer-missing spend inputs",
    preSubmitBoundary: publicationBoundary,
  });
  const certificateUtxo =
    planned.plan.tier === "Certified"
      ? (
          await certifyFaultProofFieldCarriage({
            lucid,
            network,
            signer,
            planned,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            certificateMintingScript:
              contracts.fieldPreimageCertificateMintingScript,
            certificateReferenceScriptUtxo:
              certificateReferenceScriptUtxo ??
              (() => {
                throw new Error(
                  "spend-input-signer-missing: certified input opening requires certificate reference script",
                );
              })(),
            chunkUtxos: carriageUtxos,
            compactCbor: nativeTxCompactCbor,
            witnessSetCompactCbor,
            preSubmitBoundary: certificateBoundary,
          })
        ).certificateUtxo
      : undefined;
  await onCarriageReady?.();
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[1].spendingScriptHash,
    family: "spend-input-signer-missing",
    stepIndex,
  });
  if (membershipReferenceScriptUtxo.scriptRef == null)
    throw new Error(
      "spend-input-signer-missing: predecessor membership reference script is absent",
    );
  const membershipScript = membershipReferenceScriptUtxo.scriptRef;
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: membershipScript,
    referenceUtxo: membershipReferenceScriptUtxo,
    label: "spend-input-signer-missing predecessor membership",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    stepReference,
    ...membershipCarriage.referenceInputs,
  ];
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "spend-input-signer-missing spend inputs",
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        transaction_id: evidence.subject.transaction_id,
        witness_set_hash: evidence.witnessSetHashHex,
        payment_credential: evidence.paymentCredentialHex,
      },
    } as never,
    SpendInputSignerStep03DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[2].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "spend-input-signer-missing step-02",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "spend-input-signer-missing step-02",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "spend-input-signer-missing step-02 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            spend_inputs_opening: opening,
            descriptor_cbor: evidence.resolved.descriptorCborHex,
            membership: {
              RedeemerCarriedMembership: {
                membership_proof: evidence.resolved.membershipProof,
                membership_proof_script_redeemer_index:
                  requireWithdrawalRedeemerIndex(
                    ctx,
                    membershipAddress,
                    "spend-input-signer-missing membership",
                  ),
              },
            },
          },
        ],
      } as never,
      SpendInputSignerStep02RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const feeInput = selectFeeInput(
    carriageUtxos.reduce<readonly UTxO[]>(
      (values, utxo) => excludeUtxo(values, utxo),
      await lucid.wallet().getUtxos(),
    ),
  );
  const unsigned = await membershipCarriage
    .attach(
      lucid
        .newTx()
        .collectFrom([feeInput])
        .collectFrom([threadUtxo], redeemer)
        .readFrom(referenceInputs)
        .withdraw(
          membershipAddress,
          0n,
          encodeRawPhasMembershipProofRedeemer({
            root: evidence.resolved.priorRoot,
            keyBytes: encodeMidgardSpendInputItem({
              txId: Buffer.from(evidence.resolved.transactionId, "hex"),
              outputIndex: evidence.resolved.outputIndex,
            }).toString("hex"),
            valueBytes: evidence.resolved.descriptorCborHex,
            membershipProofCbor: evidence.resolved.membershipProofCborHex,
          }),
        )
        .pay.ToContract(
          contracts.steps[2].spendingScriptAddress,
          { kind: "inline", value: nextDatum },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        )
        .addSignerKey(signer.paymentKeyHash),
    )
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error("spend-input-signer-missing: step-02 layout unresolved");
  const signed = await unsigned.sign.withWallet().complete();
  const expectedHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "spend-input-signer-missing-step-02",
        utxo: stepReference,
        expectedScript: contracts.steps[1].spendingScript,
      }),
      workflowReferenceScript({
        role: "spend-input-signer-missing-membership",
        utxo: membershipReferenceScriptUtxo,
        expectedScript: membershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedHash)
    throw new Error(
      "spend-input-signer-missing: step-02 transaction hash changed",
    );
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${outputIndex.toString()}`,
    carriageTier: planned.plan.tier,
  };
};
