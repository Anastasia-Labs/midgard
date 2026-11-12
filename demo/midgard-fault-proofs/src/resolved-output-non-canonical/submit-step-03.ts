import {
  encodeMidgardSpendInputItem,
  initialMidgardLedgerOutputScanControl,
} from "@al-ft/midgard-core";
import {
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
  requireLinearFaultReferenceScript,
  requireLinearFaultStepState,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { completeStep, fundStep, submitStep } from "../step-completion.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import type { ResolvedOutputNonCanonicalContracts } from "./contracts.js";
import {
  type ResolvedOutputEvidence,
  resolvedOutputScanControlData,
} from "./resolved-output-non-canonical.js";
import {
  ResolvedOutputStep03DatumSchema,
  ResolvedOutputStep03RedeemerSchema,
  ResolvedOutputStep04DatumSchema,
} from "./schemas.js";

export const submitResolvedOutputNonCanonicalStep03 = async ({
  lucid,
  network,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  evidence,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly contracts: ResolvedOutputNonCanonicalContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidence;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepState<{
    subject: unknown;
    prior_root: string;
    out_ref: { transactionId: string; outputIndex: bigint };
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep03DatumSchema as never,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  if (
    state.prior_root !== evidence.resolved.priorRoot ||
    state.out_ref.transactionId !== evidence.resolved.transactionId ||
    state.out_ref.outputIndex !== BigInt(evidence.resolved.outputIndex)
  )
    throw new Error(
      "resolved-output-non-canonical: authenticated out-ref checkpoint changed",
    );
  if (evidence.resolved.membershipProof === undefined)
    throw new Error(
      "resolved-output-non-canonical: production predecessor membership object is absent",
    );
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[2].spendingScriptHash,
    family: "resolved-output-non-canonical",
    stepIndex,
  });
  // `getCompiledScript` needs the deployment blueprint, so production supplies
  // the already-published canonical PHAS witness and uses its exact script.
  const membershipReference = witnessReferenceScripts?.phasMembershipWithdraw;
  if (membershipReference?.scriptRef == null)
    throw new Error(
      "resolved-output-non-canonical: predecessor membership reference script is absent",
    );
  const exactMembershipScript = membershipReference.scriptRef;
  const membershipAddress = phasMembershipRewardAddress(
    network,
    exactMembershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriage({
    script: exactMembershipScript,
    referenceUtxo: membershipReference,
    label: "resolved-output-non-canonical predecessor membership",
  });
  const nextDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        subject: evidence.subject,
        descriptor_cbor: evidence.resolved.descriptorCborHex,
        control: resolvedOutputScanControlData(
          initialMidgardLedgerOutputScanControl(),
        ),
      },
    } as never,
    ResolvedOutputStep04DatumSchema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const keyBytes = encodeMidgardSpendInputItem({
    txId: Buffer.from(evidence.resolved.transactionId, "hex"),
    outputIndex: evidence.resolved.outputIndex,
  }).toString("hex");
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical step-03",
    );
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "resolved-output-non-canonical step-03",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "resolved-output-non-canonical step-03 output",
    );
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            descriptor_cbor: evidence.resolved.descriptorCborHex,
            membership: {
              RedeemerCarriedMembership: {
                membership_proof: evidence.resolved.membershipProof,
                membership_proof_script_redeemer_index:
                  requireWithdrawalRedeemerIndex(
                    ctx,
                    membershipAddress,
                    "resolved-output-non-canonical membership",
                  ),
              },
            },
          },
        ],
      } as never,
      ResolvedOutputStep03RedeemerSchema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const funding = await fundStep(lucid, { signer });
  const signed = await completeStep(
    membershipCarriage.attach(
      funding.tx
        .collectFrom([threadUtxo], redeemer)
        .readFrom([stepReference, ...membershipCarriage.referenceInputs])
        .withdraw(
          membershipAddress,
          0n,
          encodeRawPhasMembershipProofRedeemer({
            root: evidence.resolved.priorRoot,
            keyBytes,
            valueBytes: evidence.resolved.descriptorCborHex,
            membershipProofCbor: evidence.resolved.membershipProofCborHex,
          }),
        )
        .pay.ToContract(
          contracts.steps[3].spendingScriptAddress,
          { kind: "inline", value: nextDatum },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        ),
    ),
    funding,
  );
  if (outputIndex === undefined)
    throw new Error("resolved-output-non-canonical: step-03 layout unresolved");
  const txHash = await submitStep(lucid, signed, {
    referenceScripts: [
      workflowReferenceScript({
        role: "resolved-output-non-canonical-step-03",
        utxo: stepReference,
        expectedScript: contracts.steps[2].spendingScript,
      }),
      workflowReferenceScript({
        role: "resolved-output-non-canonical-membership",
        utxo: membershipReference,
        expectedScript: exactMembershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
    awaitConfirmation,
  });
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
