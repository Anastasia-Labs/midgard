/** Bind the disputed transaction and header fee schedule into step-02 state. */
import {
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  MinFeeStep01SpendRedeemer,
  MinFeeStep02Datum,
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
import { Effect } from "effect";

import type { MinFeeContractsV1 } from "./min-fee-contracts-v1.js";
import {
  minFeeStepLabelV1,
  minFeeSubmitError,
  requireMinFeeReferenceScriptV1,
  requireMinFeeThreadUtxoV1,
} from "./min-fee-submit-common-v1.js";
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
} from "./runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessWithdrawalValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";

const STEP_LABEL = minFeeStepLabelV1(0);

export type SubmitMinFeeStep01Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly nativeTxId: string;
  readonly fee: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitMinFeeStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: MinFeeContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  /** Mandatory: min-fee validators are reference-script-only. */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMinFeeStep01Result> => {
  const { threadUtxo, threadToken } = await requireMinFeeThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  requireNativeTxMatchesCompactCbor(txInclusion);
  const [hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: `${STEP_LABEL} hub oracle`,
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: `${STEP_LABEL} state-queue block`,
    }),
  ]);
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw minFeeSubmitError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(
      await Effect.runPromise(
        getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
      ),
    ),
  );
  const state = {
    bad_tx: txInclusion.nativeTx,
    bad_tx_body_fee: txInclusion.nativeTx.body.fee,
    bad_tx_id: txInclusion.nativeTxId,
    min_fee_a: header.minFeeA,
    min_fee_b: header.minFeeB,
  };
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: state },
    MinFeeStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  const stepReference = requireMinFeeReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(network, phasScript);
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${STEP_LABEL} PHAS membership`,
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    stepReference,
    ...phasMembershipCarriage.referenceInputs,
  ];
  let resolved:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const inputIndex = requireInputIndex(ctx, threadUtxo, STEP_LABEL);
    const outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      `${STEP_LABEL} output`,
    );
    resolved = { inputIndex, outputIndex };
    return Data.to(
      {
        Continue: [
          {
            input_index: inputIndex,
            output_index: outputIndex,
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleUtxo,
              `${STEP_LABEL} hub oracle`,
            ),
            state_queue_node_ref_input_index: requireReferenceInputIndex(
              ctx,
              stateQueueBlockUtxo,
              `${STEP_LABEL} state-queue block`,
            ),
            native_tx_id: txInclusion.nativeTxId,
            native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasRewardAddress,
                `${STEP_LABEL} PHAS membership`,
              ),
          },
        ],
      },
      MinFeeStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.nativeTxCompactCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolved === undefined) {
    throw minFeeSubmitError("step-01 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${resolved.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    nativeTxId: txInclusion.nativeTxId,
    fee: state.bad_tx_body_fee,
    minFeeA: state.min_fee_a,
    minFeeB: state.min_fee_b,
    inputIndex: Number(resolved.inputIndex),
    outputIndex: Number(resolved.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
