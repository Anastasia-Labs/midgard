import {
  HUB_ORACLE_ASSET_NAME,
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
} from "../submit-step-01.js";
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
import type { MissingNativeScriptTxContractsV1 } from "./contracts-v1.js";
import {
  type MissingNativeScriptTxStepIndexV1,
  missingNativeScriptTxStepLabelV1,
  missingNativeScriptTxSubmitError,
  requireMissingNativeScriptTxReferenceScriptV1,
} from "./submit-common-v1.js";

export type MissingNativeScriptTxBindingResultV1 = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
};

/** Shared body of the two bare-`NativeTxInclusionArgs` binding steps. */
export const submitMissingNativeScriptTxBindingV1 = async ({
  lucid,
  blueprint,
  network,
  contracts,
  signer,
  stepIndex,
  threadUtxo,
  threadToken,
  stateQueueBlockOutRef,
  txInclusion,
  nextDatum,
  spendRedeemerSchema,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly network: Network;
  readonly contracts: MissingNativeScriptTxContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly stepIndex: 0 | 2;
  readonly threadUtxo: UTxO;
  readonly threadToken: {
    readonly unit: string;
    readonly fraudulentHeaderHash: string;
  };
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly nextDatum: string;
  readonly spendRedeemerSchema: Parameters<typeof Data.to>[1];
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  /** Runs after local evaluation/signing and before provider submission. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation: boolean;
}): Promise<MissingNativeScriptTxBindingResultV1> => {
  const label = missingNativeScriptTxStepLabelV1(
    stepIndex as MissingNativeScriptTxStepIndexV1,
  );
  const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${label} hub oracle`,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${label} state-queue block`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw missingNativeScriptTxSubmitError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasAddress = phasMembershipRewardAddress(network, phasScript);
  const nextStep = contracts.steps[stepIndex + 1];
  const outputMatches = computationThreadOutputPredicate({
    address: nextStep.spendingScriptAddress,
    datum: nextDatum,
    unit: threadToken.unit,
  });
  let layout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        hubOracleRefInputIndex: bigint;
        stateQueueNodeRefInputIndex: bigint;
      }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const resolved = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${label} output`,
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        `${label} hub oracle`,
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        `${label} state-queue node`,
      ),
    };
    layout = resolved;
    return Data.to(
      {
        Continue: [
          {
            input_index: resolved.inputIndex,
            output_index: resolved.outputIndex,
            hub_ref_input_index: resolved.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              resolved.stateQueueNodeRefInputIndex,
            native_tx_id: txInclusion.nativeTxId,
            l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasAddress,
                `${label} PHAS membership`,
              ),
          },
        ],
      },
      spendRedeemerSchema,
    );
  }) satisfies BuildTxWithRedeemer;
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${label} PHAS membership`,
  });
  const stepReference = requireMissingNativeScriptTxReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[stepIndex].spendingScriptHash,
    stepIndex,
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    stepReference,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
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
      nextStep.spendingScriptAddress,
      { kind: "inline", value: nextDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw missingNativeScriptTxSubmitError(
      `BuildTxWithRedeemer did not resolve ${label} layout.`,
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: [
      workflowReferenceScriptV1({
        role: `${label}-spend`,
        utxo: stepReference,
        expectedScript: contracts.steps[stepIndex].spendingScript,
      }),
      workflowReferenceScriptV1({
        role: `${label}-phas-membership`,
        utxo: witnessReferenceScripts?.phasMembershipWithdraw,
        expectedScript: phasScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingNativeScriptTxSubmitError(
      `provider returned transaction hash ${txHash}, expected ${expectedTxHash}`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    inputIndex: Number(layout.inputIndex),
    outputIndex: Number(layout.outputIndex),
    hubOracleRefInputIndex: Number(layout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(layout.stateQueueNodeRefInputIndex),
  };
};
