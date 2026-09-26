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
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  certifyFaultProofFieldCarriage,
  faultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import { witnessWithdrawalValidatorCarriage } from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import type { SpendInputSignerMissingContracts } from "./contracts.js";
import { planSpendInputSignerInputOpening } from "./field-plans.js";
import {
  SpendInputSignerStep02DatumSchema,
  SpendInputSignerStep02RedeemerSchema,
  SpendInputSignerStep03DatumSchema,
  SpendInputSignerStep05DatumSchema,
} from "./schemas.js";
import {
  requireSpendInputSignerResolvedOutput,
  requireSpendInputSignerScanEvidence,
  type SpendInputSignerMissingEvidence,
} from "./spend-input-signer-missing.js";

/**
 * Step 02 resolves the bound spend input and classifies its credential the
 * way the validator does: a pub-key credential is handed to the witness scan
 * at step 03; a script credential, or a coordinate past the field-0 count,
 * closes at step 05 with the direct verdict. The out-of-range arm reads no
 * prior-ledger membership, so it carries none.
 */
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
    state.prior_root !== evidence.priorRoot ||
    state.witness_set_hash !== evidence.witnessSetHashHex
  )
    throw new Error(
      "spend-input-signer-missing: authenticated bind state changed",
    );
  const outOfRange = evidence.route === "coordinate_out_of_range";
  const resolved = outOfRange
    ? undefined
    : requireSpendInputSignerResolvedOutput(evidence);
  if (resolved !== undefined && resolved.membershipProof === undefined)
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
    ...(resolved === undefined ? [] : membershipCarriage.referenceInputs),
  ];
  const opening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: "spend-input-signer-missing spend inputs",
  });
  // The witness-scan route hands the credential to step 03; every direct
  // route writes the terminal verdict step 02 derives on chain
  // (`direct_verdict_v1`) and continues at step 05.
  const scan = evidence.route === "witness_scan";
  const nextStepIndex = scan ? 2 : 4;
  const nextDatum = scan
    ? Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            transaction_id: evidence.subject.transaction_id,
            witness_set_hash: evidence.witnessSetHashHex,
            payment_credential:
              requireSpendInputSignerScanEvidence(evidence)
                .paymentCredentialHex,
          },
        } as never,
        SpendInputSignerStep03DatumSchema as never,
      )
    : Data.to(
        {
          fraud_prover: signer.paymentKeyHash,
          data: {
            subject: evidence.subject,
            signer_required: false,
            signer_missing: false,
          },
        } as never,
        SpendInputSignerStep05DatumSchema as never,
      );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[nextStepIndex].spendingScriptAddress,
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
            // Past the field's count no membership is read; the carriage
            // slot is filled with an empty proof the validator never opens.
            descriptor_cbor: resolved?.descriptorCborHex ?? "",
            membership: {
              RedeemerCarriedMembership: {
                membership_proof: resolved?.membershipProof ?? [],
                membership_proof_script_redeemer_index:
                  resolved === undefined
                    ? 0n
                    : requireWithdrawalRedeemerIndex(
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
  const withMembership = (tx: TxBuilder): TxBuilder =>
    resolved === undefined
      ? tx
      : membershipCarriage.attach(
          tx.withdraw(
            membershipAddress,
            0n,
            encodeRawPhasMembershipProofRedeemer({
              root: evidence.priorRoot,
              keyBytes: encodeMidgardSpendInputItem({
                txId: Buffer.from(resolved.transactionId, "hex"),
                outputIndex: resolved.outputIndex,
              }).toString("hex"),
              valueBytes: resolved.descriptorCborHex,
              membershipProofCbor: resolved.membershipProofCborHex,
            }),
          ),
        );
  const unsigned = await withMembership(
    lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([threadUtxo], redeemer)
      .readFrom(referenceInputs)
      .pay.ToContract(
        contracts.steps[nextStepIndex].spendingScriptAddress,
        { kind: "inline", value: nextDatum },
        {
          lovelace: threadUtxo.assets.lovelace ?? 0n,
          [threadToken.unit]: 1n,
        },
      )
      .addSignerKey(signer.paymentKeyHash),
  ).complete({ localUPLCEval: true });
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
      ...(resolved === undefined
        ? []
        : [
            workflowReferenceScript({
              role: "spend-input-signer-missing-membership",
              utxo: membershipReferenceScriptUtxo,
              expectedScript: membershipScript,
            }),
          ]),
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
    route: evidence.route,
    stage: scan ? ("step03" as const) : ("step05" as const),
  };
};
