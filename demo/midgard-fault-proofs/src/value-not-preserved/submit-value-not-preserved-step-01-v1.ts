/**
 * `value-not-preserved` step-01 submitter (offchain plan §4).
 *
 * Binds the challenged, operator-ACCEPTED transaction out of the challenged
 * block's counted `transactions_root` via the one blessed binding path
 * (`pass_native_tx_to_next_step`: hub + state-queue reference inputs, PHAS
 * membership withdrawal), and freezes the single-asset claim into the
 * step-02 fold state. Every check the validator makes that this process can
 * make locally is made locally first, so a doomed transaction is refused
 * before it costs anything:
 *
 * - the state-queue block's header hash must be the thread NFT's;
 * - the acceptance gate: a `validity_code != 0` leaf is an honest no-op
 *   recording, outside this family's domain (§1.4) — refused here exactly as
 *   the validator refuses it;
 * - the claim must be well-formed (28-byte policy, ≤32-byte name);
 * - the forwarded state's `committed_fee` and `prev_utxos_root` must be the
 *   root-committed fee and the challenged header's pre-state ledger root —
 *   the validator recomputes both, so a divergent caller value could only
 *   build an unexecutable transaction.
 */
import {
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import { HUB_ORACLE_ASSET_NAME } from "@al-ft/midgard-sdk";
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
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessSpendingValidatorCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import type { ValueNotPreservedContractsV1 } from "./contracts-v1.js";
import {
  claimedAssetIsWellFormedV1,
  type ClaimedAssetV1,
  type ClaimedImbalanceDirectionV1,
  ValueNotPreservedStep01SpendRedeemer,
  ValueNotPreservedStep02Datum,
  type ValueNotPreservedStep02State,
} from "./schemas-v1.js";
import {
  requireValueNotPreservedReferenceScriptV1,
  requireValueNotPreservedThreadUtxoV1,
  valueNotPreservedStepLabelV1,
  valueNotPreservedSubmitError,
} from "./submit-common-v1.js";

const STEP_LABEL = valueNotPreservedStepLabelV1(0);

export type SubmitValueNotPreservedStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  /** The initial fold state the thread now carries. */
  readonly foldState: ValueNotPreservedStep02State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export const submitValueNotPreservedStep01 = async ({
  lucid,
  blueprint,
  contracts,
  categoryId,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  claimedAsset,
  claimedDirection,
  prevUtxosRoot,
  referenceScriptUtxo,
  witnessReferenceScripts,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: ValueNotPreservedContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  /** The single asset the thread accuses the transaction of not conserving. */
  readonly claimedAsset: ClaimedAssetV1;
  readonly claimedDirection: ClaimedImbalanceDirectionV1;
  /**
   * The challenged header's `prev_utxos_root`, hex. The validator freezes
   * the header's own value; a divergent one here would only build an
   * unexecutable transaction, so it is the caller's job to read it off the
   * challenged header.
   */
  readonly prevUtxosRoot: string;
  /** The mandatory published step-01 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Published witness reference scripts required by this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitValueNotPreservedStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireValueNotPreservedThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: "state-queue block UTxO",
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: "hub oracle",
  });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw valueNotPreservedSubmitError(
      `state-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  requireNativeTxMatchesCompactCbor(txInclusion);
  // §1.4 acceptance gate: only an operator-ACCEPTED transaction is in this
  // family's domain. An invalid transaction honestly recorded as a no-op is
  // correct block production and must never reach the fold.
  if (txInclusion.nativeTx.validity_code !== 0n) {
    throw valueNotPreservedSubmitError(
      `--tx-inclusion.nativeTx carries validity code ${txInclusion.nativeTx.validity_code.toString()}, so the committed leaf is an honest no-op recording — outside this family's domain.`,
    );
  }
  if (!claimedAssetIsWellFormedV1(claimedAsset)) {
    throw valueNotPreservedSubmitError(
      "claimed asset is outside the committed-leaf domain (policy id must be 28 bytes, asset name at most 32).",
    );
  }

  const foldState: ValueNotPreservedStep02State = {
    bad_tx_id: txInclusion.nativeTxId,
    claimed_asset: claimedAsset,
    claimed_direction: claimedDirection,
    committed_fee: txInclusion.nativeTx.body.fee,
    prev_utxos_root: prevUtxosRoot,
    input_cursor: 0n,
    claimed_delta: 0n,
  };

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${STEP_LABEL} PHAS membership`,
  });
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireValueNotPreservedReferenceScriptV1({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[0].spendingScriptHash,
          stepIndex: 0,
        });
  const stepCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.steps[0].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} spending validator`,
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...stepCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: foldState },
    ValueNotPreservedStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        `${STEP_LABEL} output`,
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion: {
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
              hub_ref_input_index: requireReferenceInputIndex(
                ctx,
                hubOracleUtxo,
                `${STEP_LABEL} hub oracle`,
              ),
              state_queue_node_ref_input_index: requireReferenceInputIndex(
                ctx,
                stateQueueBlockUtxo,
                `${STEP_LABEL} state-queue node`,
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
            claimed_asset: claimedAsset,
            claimed_direction: claimedDirection,
          },
        ],
      },
      ValueNotPreservedStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

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
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = phasMembershipCarriage.attach(stepCarriage.attach(base));

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw valueNotPreservedSubmitError(
      "BuildTxWithRedeemer did not resolve the step-01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  return {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    nextThreadOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    foldState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
