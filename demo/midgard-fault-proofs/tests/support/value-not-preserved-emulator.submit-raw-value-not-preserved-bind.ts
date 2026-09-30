import {
  fieldPreimagePublicationDatumCbor,
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
  type Script,
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

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
  type ClaimedAsset,
  type ClaimedImbalanceDirection,
  ValueNotPreservedStep01SpendRedeemer,
  ValueNotPreservedStep02Datum,
  type ValueNotPreservedStep02State,
} from "../../src/value-not-preserved/schemas.js";
import { requireValueNotPreservedThreadUtxo } from "../../src/value-not-preserved/submit-common.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessSpendingValidatorCarriage,
  witnessWithdrawalValidatorCarriage,
} from "../../src/witness-reference-scripts.js";
import { network as emulatorNetwork } from "./submit-init-emulator-shared.js";
import { type ValueNotPreservedHarness } from "./value-not-preserved-emulator.commit-header-after-anchor-block.js";

// ---------------------------------------------------------------------------
// Tampered tier-2 publication
// ---------------------------------------------------------------------------

/**
 * Publishes `bytes` under the exact §8.5 nothing-but-bytes publication datum
 * at the prover's own address — the shape a genuine tier-2 carriage UTxO
 * has, with bytes that do NOT hash to the committed field commitment. The
 * honest content-addressed resolution can never pick it up
 * (`resolveChunkReferenceIndicesV1` matches by exact datum bytes), so the
 * adversarial suite injects it positionally via the step-03 submitter's
 * test-only escape hatch and watches the §8.8 door's
 * `field_commitment(preimage) == expected_hash` re-hash refuse it.
 */
export const publishTamperedFieldPreimagePublication = async ({
  harness,
  bytes,
}: {
  readonly harness: ValueNotPreservedHarness;
  readonly bytes: Uint8Array;
}): Promise<UTxO> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  signer.selectWallet(lucid);
  const datum = fieldPreimagePublicationDatumCbor(bytes);
  const tx = await lucid
    .newTx()
    .pay.ToAddressWithData(
      signer.address,
      { kind: "inline", value: datum },
      { lovelace: 80_000_000n },
    )
    .complete({ localUPLCEval: true });
  const signed = await tx.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  const utxos = await lucid.utxosAt(signer.address);
  const published = utxos.find(
    (utxo) =>
      utxo.txHash === txHash &&
      utxo.datum != null &&
      utxo.datum.toLowerCase() === datum.toLowerCase(),
  );
  if (published === undefined) {
    throw new Error("Tampered publication UTxO not found after confirmation");
  }
  return published;
};

// ---------------------------------------------------------------------------
// Raw builders — the honest submitters' transactions WITHOUT their local
// fail-closed guards, so the adversarial suite can watch the VALIDATOR refuse
// (see `expectOnchainRefusal`). Production code never takes these paths.
// ---------------------------------------------------------------------------

/**
 * A raw step-01 bind: the honest submitter's inclusion transaction minus its
 * §1.4 acceptance-gate re-check, so an honestly-rejected committed leaf
 * reaches the validator's own `validity_code == 0` refusal.
 */
export const submitRawValueNotPreservedBind = async ({
  harness,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  claimedAsset,
  claimedDirection,
  prevUtxosRoot,
  referenceScriptUtxo,
  witnessReferenceScripts,
}: {
  readonly harness: ValueNotPreservedHarness;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly claimedAsset: ClaimedAsset;
  readonly claimedDirection: ClaimedImbalanceDirection;
  readonly prevUtxosRoot: string;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
}): Promise<string> => {
  const lucid = harness.proverLucid;
  const signer = harness.proverSigner;
  const contracts = harness.family;
  const { threadUtxo, threadToken } = await requireValueNotPreservedThreadUtxo({
    lucid,
    contracts,
    categoryId: harness.category.categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: "raw value-not-preserved state-queue block UTxO",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      emulatorNetwork,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "raw value-not-preserved hub oracle",
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
    label: "raw value-not-preserved PHAS membership",
  });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[0].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "raw value-not-preserved step-01",
  });
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const foldState: ValueNotPreservedStep02State = {
    bad_tx_id: txInclusion.nativeTxId,
    claimed_asset: claimedAsset,
    claimed_direction: claimedDirection,
    committed_fee: txInclusion.nativeTx.body.fee,
    prev_utxos_root: prevUtxosRoot,
    input_cursor: 0n,
    claimed_delta: 0n,
  };
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: foldState },
    ValueNotPreservedStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "raw value-not-preserved bind");
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion: {
              input_index: requireInputIndex(
                ctx,
                threadUtxo,
                "raw value-not-preserved bind",
              ),
              output_index: requireUniqueOutputIndex(
                ctx.outputs,
                outputMatches,
                "raw value-not-preserved bind output",
              ),
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo,
                "raw value-not-preserved hub oracle",
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                stateQueueBlockUtxo,
                "raw value-not-preserved state-queue node",
              ),
              native_tx_id: txInclusion.nativeTxId,
              l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
              transactions_phas_root: txInclusion.transactionsPhasRoot,
              tx_membership_proof: txInclusion.txMembershipProof,
              inclusion_proof_script_withdraw_redeemer_index:
                requireWithdrawalRedeemerIndex(
                  ctx,
                  phasRewardAddress,
                  "raw value-not-preserved PHAS membership",
                ),
            },
            claimed_asset: claimedAsset,
            claimed_direction: claimedDirection,
          },
        ],
      },
      ValueNotPreservedStep01SpendRedeemer,
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
  const tx = phasMembershipCarriage.attach(stepCarriage.attach(base));
  const unsigned = await tx.complete({ localUPLCEval: true });
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  await lucid.awaitTx(txHash);
  return txHash;
};
