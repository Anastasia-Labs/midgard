import {
  encodeMidgardSpendInputItemV1,
  initialMidgardLedgerOutputScanControlV1,
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
  requireLinearFaultReferenceScriptV1,
  requireLinearFaultStepStateV1,
  requireLinearFaultThreadUtxoV1,
} from "../linear-fault-family-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessWithdrawalValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptV1,
} from "../workflow/transaction-boundary-v1.js";
import type { ResolvedOutputNonCanonicalContractsV1 } from "./contracts-v1.js";
import {
  type ResolvedOutputEvidenceV1,
  resolvedOutputScanControlDataV1,
} from "./resolved-output-non-canonical-v1.js";
import {
  ResolvedOutputStep03DatumV1Schema,
  ResolvedOutputStep03RedeemerV1Schema,
  ResolvedOutputStep04DatumV1Schema,
} from "./schemas-v1.js";

export const submitResolvedOutputNonCanonicalStep03V1 = async ({
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
  readonly contracts: ResolvedOutputNonCanonicalContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly evidence: ResolvedOutputEvidenceV1;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}) => {
  const stepIndex = 2;
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    family: "resolved-output-non-canonical",
    stepIndex,
    threadOutRef,
  });
  const state = requireLinearFaultStepStateV1<{
    subject: unknown;
    prior_root: string;
    out_ref: { transactionId: string; outputIndex: bigint };
  }>({
    threadUtxo,
    signer,
    schema: ResolvedOutputStep03DatumV1Schema as never,
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
  signer.selectWallet(lucid);
  const stepReference = requireLinearFaultReferenceScriptV1({
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
  const membershipCarriage = witnessWithdrawalValidatorCarriageV1({
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
        control: resolvedOutputScanControlDataV1(
          initialMidgardLedgerOutputScanControlV1(),
        ),
      },
    } as never,
    ResolvedOutputStep04DatumV1Schema as never,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[3].spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const keyBytes = encodeMidgardSpendInputItemV1({
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
      ResolvedOutputStep03RedeemerV1Schema as never,
    );
  }) satisfies BuildTxWithRedeemer;
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const unsigned = await membershipCarriage
    .attach(
      lucid
        .newTx()
        .collectFrom([feeInput])
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
        )
        .addSignerKey(signer.paymentKeyHash),
    )
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error("resolved-output-non-canonical: step-03 layout unresolved");
  const signed = await unsigned.sign.withWallet().complete();
  const expected = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: [
      workflowReferenceScriptV1({
        role: "resolved-output-non-canonical-step-03",
        utxo: stepReference,
        expectedScript: contracts.steps[2].spendingScript,
      }),
      workflowReferenceScriptV1({
        role: "resolved-output-non-canonical-membership",
        utxo: membershipReference,
        expectedScript: exactMembershipScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error("resolved-output-non-canonical: step-03 hash mismatch");
  if (awaitConfirmation)
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
