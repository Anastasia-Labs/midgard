import {
  HUB_ORACLE_ASSET_NAME,
  InputSetUniquenessStep01SpendRedeemer,
  InputSetUniquenessStep02Datum,
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
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { requireInputSetUniquenessThreadUtxo } from "../../src/input-set-uniqueness/submit-common.js";
import {
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  parseOutRef,
  phasMembershipRewardAddress,
  requireSingletonUtxo,
} from "../../src/runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../../src/step-support.js";
import { computationThreadOutputPredicate } from "../../src/tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessSpendingValidatorCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { type InputSetUniquenessHarness } from "./input-set-uniqueness-emulator.build-input-set-uniqueness-fixture.js";
import { network as emulatorNetwork } from "./submit-init-emulator-shared.js";

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite can watch the VALIDATOR refuse
// (see `expectOnchainRefusal`). Production code never takes these paths.
// ---------------------------------------------------------------------------

/**
 * A raw step-01 bind: the honest submitter's redeemer-carried inclusion
 * transaction minus its §2.4.3(d) validity re-check and header cross-check,
 * so an honestly-rejected committed leaf reaches the validator's own
 * `validity_code == 0` refusal.
 */
export const submitRawInputSetUniquenessBind = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: InputSetUniquenessHarness;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId: harness.category.categoryId,
      stepIndex: 0,
      threadOutRef,
    },
  );
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: "raw input-set-uniqueness state-queue block UTxO",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetwork,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "raw input-set-uniqueness hub oracle",
  });
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(
      harness.realBlueprint,
      PHAS_MEMBERSHIP_WITHDRAW_TITLE,
    ),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    emulatorNetwork,
    phasMembershipScript,
  );
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriage({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts.phasMembershipWithdraw,
    label: "raw input-set-uniqueness PHAS membership",
  });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[0].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw input-set-uniqueness step-01",
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: { bad_tx_id: txInclusion.nativeTxId },
    },
    InputSetUniquenessStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw input-set-uniqueness bind");
    return Data.to(
      {
        Continue: [
          {
            source: {
              AcceptedSource: {
                inclusion: {
                  RedeemerCarriedInclusion: [
                    {
                      input_index: requireInputIndex(
                        ctx,
                        threadUtxo,
                        "raw input-set-uniqueness bind",
                      ),
                      output_index: requireUniqueOutputIndex(
                        ctx.outputs,
                        outputMatches,
                        "raw input-set-uniqueness bind output",
                      ),
                      hub_ref_input_index: requireReferenceInputIndex(
                        ctx,
                        hubOracleUtxo,
                        "raw input-set-uniqueness hub oracle",
                      ),
                      state_queue_node_ref_input_index:
                        requireReferenceInputIndex(
                          ctx,
                          stateQueueBlockUtxo,
                          "raw input-set-uniqueness state-queue node",
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
                          "raw input-set-uniqueness PHAS membership",
                        ),
                    },
                  ],
                },
              },
            },
          },
        ],
      },
      InputSetUniquenessStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueBlockUtxo,
      ...stepCarriage.referenceInputs,
      ...phasMembershipCarriage.referenceInputs,
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
      { kind: "inline", value: step02Datum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(stepCarriage.attach(base));
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};

/** The finalize layout a raw args builder is handed. */
export type RawInputSetUniquenessFinalizeLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};
