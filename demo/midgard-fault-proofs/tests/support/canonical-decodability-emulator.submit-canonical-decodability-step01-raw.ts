import {
  CanonicalDecodabilityStep01SpendRedeemer,
  CanonicalDecodabilityStep02Datum,
  type CanonicalDecodabilityStep02State,
  type CommittedFieldClaim,
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
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type CanonicalDecodabilityContracts,
  requireCanonicalDecodabilityReferenceScript,
  requireCanonicalDecodabilityThreadUtxo,
} from "../../src/canonical-decodability/index.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
  type ResolvedProverSigner,
} from "../../src/runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireInitialStepDatum,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { network } from "./submit-init-emulator-shared.js";

/** Guard-bypassing step-01 builder used only for validator-negative tests. */
export const submitCanonicalDecodabilityStep01Raw = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  claim,
  step02State,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: CanonicalDecodabilityContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly claim: CommittedFieldClaim;
  readonly step02State: CanonicalDecodabilityStep02State;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<{ readonly txHash: string; readonly nextThreadOutRef: string }> => {
  const { threadUtxo, threadToken } =
    await requireCanonicalDecodabilityThreadUtxo({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const [stateQueueUtxo, hubOracleUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(stateQueueBlockOutRef, "raw state queue out-ref"),
      label: "raw canonical-decodability state queue node",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(contracts.hubOraclePolicyId),
      ),
      unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "raw canonical-decodability hub oracle",
    }),
  ]);
  const stepReference = requireCanonicalDecodabilityReferenceScript({
    utxo: referenceScriptUtxo,
    expectedScriptHash: contracts.steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(network, phasScript);
  const phasCarriage = witnessWithdrawalValidatorCarriage({
    script: phasScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw canonical step-01 PHAS membership",
  });
  const datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: step02State },
    CanonicalDecodabilityStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum,
    unit: threadToken.unit,
  });
  let outputIndex: bigint | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw canonical step-01");
    const inputIndex = requireInputIndex(
      ctx,
      threadUtxo,
      "raw canonical step-01",
    );
    outputIndex = requireUniqueOutputIndex(
      ctx.outputs,
      outputMatches,
      "raw canonical step-01 output",
    );
    return Data.to(
      {
        Continue: [
          {
            inclusion: {
              RedeemerCarriedInclusion: [
                {
                  input_index: inputIndex,
                  output_index: outputIndex,
                  hub_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    hubOracleUtxo,
                    "raw canonical step-01 hub",
                  ),
                  state_queue_node_ref_input_index: requireReferenceInputIndex(
                    ctx,
                    stateQueueUtxo,
                    "raw canonical step-01 state queue",
                  ),
                  native_tx_id: txInclusion.nativeTxId,
                  l2_transaction_source_cbor:
                    txInclusion.l2TransactionSourceCbor,
                  transactions_phas_root: txInclusion.transactionsPhasRoot,
                  tx_membership_proof: txInclusion.txMembershipProof,
                  inclusion_proof_script_withdraw_redeemer_index:
                    requireWithdrawalRedeemerIndex(
                      ctx,
                      phasRewardAddress,
                      "raw canonical step-01 membership",
                    ),
                },
              ],
            },
            claim,
          },
        ],
      },
      CanonicalDecodabilityStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueUtxo,
      stepReference,
      ...phasCarriage.referenceInputs,
    ])
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.l2TransactionSourceCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const unsigned = await phasCarriage
    .attach(base)
    .complete({ localUPLCEval: true });
  if (outputIndex === undefined)
    throw new Error("Raw step-01 layout unresolved");
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  return { txHash, nextThreadOutRef: `${txHash}#${outputIndex.toString()}` };
};
