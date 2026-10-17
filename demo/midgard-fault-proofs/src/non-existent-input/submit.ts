import { computeHash32 } from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldOpening,
  planFaultProofFieldOpening,
} from "../field-opening.js";
import {
  type LinearFaultContracts,
  requireLinearFaultReferenceScript,
  requireLinearFaultThreadUtxo,
} from "../linear-fault-family.js";
import { submitLinearFaultContinue } from "../linear-fault-submit.js";
import { type ResolvedProverSigner } from "../runtime.js";
import { requireInitialStepDatum, selectFeeInput } from "../step-support.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "../workflow/transaction-boundary.js";
import {
  admitNonExistentInputForcedArtifact,
  nonExistentInputForcedArtifact,
} from "./artifact.js";
import type { PreparedNonExistentInputWrongfulRejection } from "./wrongful-rejection.js";

export type NonExistentInputForcedContracts = LinearFaultContracts & {
  computationThread: { policyId: string; mintingScript: Script };
  fraudProof: {
    policyId: string;
    mintingScript: Script;
    spendingScriptAddress: string;
  };
};
export const nonExistentInputForcedFieldPlan = (
  prepared: PreparedNonExistentInputWrongfulRejection,
  owner: string,
) =>
  planFaultProofFieldOpening({
    anchorSourceKind: prepared.subject.source_kind === 1n ? 1n : 0n,
    fieldIndex: 0,
    anchorTxId: prepared.subject.transaction_id,
    nativeTxCompactCbor:
      prepared.forcedSource.membership.value.submitted_source.compact_cbor,
    itemCbors: prepared.inputItems,
    owner,
    label: "nonExistentInput forced field zero",
  });

export const nonExistentInputForcedStates = (
  prepared: PreparedNonExistentInputWrongfulRejection,
) => {
  const { header, membership } = prepared.forcedSource;
  const event_key: SDK.EventKey = {
    ForcedTransactionEventKey: { tx_order_id: membership.key },
  };
  return [
    {
      ForcedState: {
        subject: prepared.subject,
        event_key,
        event_root: header.eventToStepRoot,
        event_count: header.totalEventCount,
        trace_root: header.transitionTraceRoot,
        trace_count: header.transitionStepCount,
      },
    },
    {
      ForcedState: {
        event_key,
        trace_root: header.transitionTraceRoot,
        trace_count: header.transitionStepCount,
        step_index: prepared.eventMembership.value.step_index,
        selected_input: prepared.selectedInput,
      },
    },
    {
      ForcedState: {
        selected_input: prepared.selectedInput,
        pre_utxos_root: prepared.transitionMembership.value.pre_utxos_root,
      },
    },
  ] as const;
};
export const submitNonExistentInputForcedStep = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  prepared,
  stepIndex,
  referenceScripts,
  carriageUtxos = [],
  certificatePolicyId,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  lucid: LucidEvolution;
  contracts: NonExistentInputForcedContracts;
  categoryId: string;
  signer: ResolvedProverSigner;
  threadOutRef: string;
  prepared: PreparedNonExistentInputWrongfulRejection;
  stepIndex: 0 | 1 | 2 | 3;
  referenceScripts: {
    steps: readonly UTxO[];
    computationThreadMint: UTxO;
    fraudProofMint: UTxO;
  };
  carriageUtxos?: readonly UTxO[];
  certificatePolicyId?: string;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
  awaitConfirmation?: boolean;
}) => {
  // The public builder and durable runner share the same admission path.
  await admitNonExistentInputForcedArtifact(
    nonExistentInputForcedArtifact(prepared),
  );
  const family = "non-existent-input";
  const { threadUtxo, threadToken } = await requireLinearFaultThreadUtxo({
    lucid,
    contracts,
    categoryId,
    family,
    stepIndex,
    threadOutRef,
  });
  const states = nonExistentInputForcedStates(prepared);
  const datumSchemas = [
    SDK.NonExistentInputStep02ThreadDatum,
    SDK.NonExistentInputStep03ThreadDatum,
    SDK.NonExistentInputStep04ThreadDatum,
  ] as const;
  if (stepIndex === 0) requireInitialStepDatum({ threadUtxo, signer });
  else {
    const expected = Data.to(
      {
        fraud_prover: signer.paymentKeyHash,
        data: states[stepIndex - 1],
      } as never,
      datumSchemas[stepIndex - 1] as never,
    );
    if (
      threadUtxo.datum == null ||
      Data.to(Data.from(threadUtxo.datum)) !== Data.to(Data.from(expected))
    )
      throw new Error("nonExistentInput: thread state changed");
  }
  signer.selectWallet(lucid);
  const step = contracts.steps[stepIndex]!;
  const stepReference = requireLinearFaultReferenceScript({
    utxo: referenceScripts.steps[stepIndex]!,
    expectedScriptHash: step.spendingScriptHash,
    family,
    stepIndex,
  });
  let outputIndex: bigint | undefined;
  if (stepIndex !== 3) {
    const datum = Data.to(
      { fraud_prover: signer.paymentKeyHash, data: states[stepIndex] } as never,
      datumSchemas[stepIndex] as never,
    );
    const next = contracts.steps[stepIndex + 1]!;
    const outputMatches = computationThreadOutputPredicate({
      address: next.spendingScriptAddress,
      datum,
      unit: threadToken.unit,
    });
    const redeemer = ((ctx) => {
      SDK.requireOwnSpendPurpose(ctx, threadUtxo, family);
      const input_index = SDK.requireInputIndex(ctx, threadUtxo, family);
      outputIndex = SDK.requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        family,
      );
      if (stepIndex === 0)
        return Data.to(
          {
            Continue: [
              {
                source: {
                  ForcedSource: {
                    ...prepared.forcedSource,
                    input_index,
                    output_index: outputIndex,
                  },
                },
              },
            ],
          } as never,
          SDK.NonExistentInputStep01SpendRedeemer as never,
        );
      if (stepIndex === 1)
        return Data.to(
          {
            Continue: [
              {
                ForcedArgs: {
                  input_index,
                  output_index: outputIndex,
                  spend_inputs_opening: faultProofFieldOpening({
                    planned: nonExistentInputForcedFieldPlan(
                      prepared,
                      signer.paymentKeyHash,
                    ),
                    referenceInputs: [...carriageUtxos, stepReference],
                    certificatePolicyId,
                    label: family,
                  }),
                  event_membership: prepared.eventMembership,
                },
              },
            ],
          },
          SDK.NonExistentInputStep02ThreadSpendRedeemer,
        );
      return Data.to(
        {
          Continue: [
            {
              ForcedArgs: {
                input_index,
                output_index: outputIndex,
                transition_membership: prepared.transitionMembership,
              },
            },
          ],
        },
        SDK.NonExistentInputStep03ThreadSpendRedeemer,
      );
    }) satisfies BuildTxWithRedeemer;
    const txHash = await submitLinearFaultContinue({
      lucid,
      signerPaymentKeyHash: signer.paymentKeyHash,
      threadUtxo,
      threadUnit: threadToken.unit,
      stepReference,
      stepScript: step.spendingScript,
      stepRole: `${family} forced step ${stepIndex + 1}`,
      nextAddress: next.spendingScriptAddress,
      nextDatum: datum,
      redeemer,
      carriageUtxos,
      preSubmitBoundary,
      awaitConfirmation,
    });
    if (outputIndex === undefined)
      throw new Error("nonExistentInput: unresolved output");
    return { txHash, nextThreadOutRef: `${txHash}#${outputIndex}` };
  }
  const ctReference = requireLinearFaultReferenceScript({
    utxo: referenceScripts.computationThreadMint,
    expectedScriptHash: contracts.computationThread.policyId,
    family,
    stepIndex,
  });
  const proofReference = requireLinearFaultReferenceScript({
    utxo: referenceScripts.fraudProofMint,
    expectedScriptHash: contracts.fraudProof.policyId,
    family,
    stepIndex,
  });
  const proofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    SDK.FraudProofTokenDatum,
  );
  const matches = computationThreadOutputPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum,
    unit: proofUnit,
  });
  const spend = ((ctx) => {
    SDK.requireOwnSpendPurpose(ctx, threadUtxo, family);
    outputIndex = SDK.requireUniqueOutputIndex(ctx.outputs, matches, family);
    return Data.to(
      {
        Continue: [
          {
            ForcedArgs: {
              input_index: SDK.requireInputIndex(ctx, threadUtxo, family),
              output_index: outputIndex,
              fraud_proof_mint_redeemer_index: SDK.requireMintRedeemerIndex(
                ctx,
                contracts.fraudProof.policyId,
                family,
              ),
              membership:
                prepared.ledgerMembership === null
                  ? null
                  : {
                      value_hash: computeHash32(
                        Buffer.from(prepared.ledgerMembership.value, "hex"),
                      ).toString("hex"),
                      proof: prepared.ledgerMembership.proof,
                    },
            },
          },
        ],
      },
      SDK.NonExistentInputStep04ThreadSpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const proofMint = ((ctx) => {
    SDK.requireOwnMintPurpose(ctx, contracts.fraudProof.policyId, family);
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: SDK.requireMintRedeemerIndex(
          ctx,
          contracts.computationThread.policyId,
          family,
        ),
      },
      SDK.FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const unsigned = await lucid
    .newTx()
    .collectFrom([selectFeeInput(await lucid.wallet().getUtxos())])
    .collectFrom([threadUtxo], spend)
    .readFrom([stepReference, ctReference, proofReference])
    .mintAssets(
      { [threadToken.unit]: -1n },
      Data.to(
        { Success: { burning_token_asset_name: threadToken.assetName } },
        SDK.FraudProofComputationThreadRedeemer,
      ),
    )
    .mintAssets({ [proofUnit]: 1n }, proofMint)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: datum },
      { lovelace: threadUtxo.assets.lovelace ?? 0n, [proofUnit]: 1n },
    )
    .addSignerKey(signer.paymentKeyHash)
    .complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const expected = await reachFraudProofPreSubmitBoundary({
    signed,
    boundary: preSubmitBoundary,
    referenceScripts: [
      workflowReferenceScript({
        role: family,
        utxo: stepReference,
        expectedScript: step.spendingScript,
      }),
      workflowReferenceScript({
        role: "computation-thread mint",
        utxo: ctReference,
        expectedScript: contracts.computationThread.mintingScript,
      }),
      workflowReferenceScript({
        role: "fraud-proof mint",
        utxo: proofReference,
        expectedScript: contracts.fraudProof.mintingScript,
      }),
    ],
  });
  const txHash = await signed.submit();
  if (txHash !== expected)
    throw new Error("nonExistentInput: submitted transaction hash differs");
  if (awaitConfirmation) await lucid.awaitTx(txHash);
  return { txHash, fraudProofUnit: proofUnit };
};
