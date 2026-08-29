import {
  commitCountedRootProgram,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  ROOT_DOMAINS,
  WithdrawnInputStep01SpendRedeemer,
  WithdrawnInputStep02Datum,
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
  witnessWithdrawalValidatorCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  WITHDRAWN_INPUT_CATEGORY_LABEL,
  type WithdrawnInputContractsV1,
} from "./contracts-v1.js";
import {
  requireWithdrawnInputReferenceScriptV1,
  requireWithdrawnInputThreadUtxoV1,
  withdrawnInputSubmitError,
} from "./submit-common-v1.js";

export type SubmitWithdrawnInputStep01Result = {
  readonly txHash: string;
  readonly nextThreadOutRef: string;
  readonly secondStepAddress: string;
  readonly fraudulentHeaderHash: string;
  readonly badTxId: string;
  readonly withdrawalsRoot: string;
  readonly withdrawalCount: bigint;
  readonly computationThreadUnit: string;
};

export const submitWithdrawnInputStep01 = async ({
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
  readonly contracts: WithdrawnInputContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnInputStep01Result> => {
  const { threadUtxo, threadToken } = await requireWithdrawnInputThreadUtxoV1({
    lucid,
    contracts,
    categoryId,
    stepIndex: 0,
    threadOutRef,
  });
  requireInitialStepDatum({ threadUtxo, signer });
  const stepReference = requireWithdrawnInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    contracts,
    stepIndex: 0,
  });
  const stateQueueBlockUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} state-queue block`,
  });
  const hubOracleUtxo = await requireSingletonUtxo({
    lucid,
    address: credentialToAddress(
      network,
      scriptHashToCredential(contracts.hubOraclePolicyId),
    ),
    unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} hub oracle`,
  });
  const headerHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (headerHash !== threadToken.fraudulentHeaderHash) {
    throw withdrawnInputSubmitError(
      `state-queue header ${headerHash} does not match thread header ${threadToken.fraudulentHeaderHash}.`,
    );
  }
  requireNativeTxMatchesCompactCbor(txInclusion);
  const nodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(nodeView),
  );
  const derivedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: txInclusion.transactionsPhasRoot,
      count: header.l2TransactionCount,
    }),
  );
  if (derivedTransactionsRoot !== header.transactionsRoot) {
    throw withdrawnInputSubmitError(
      `transactions PHAS root derives ${derivedTransactionsRoot}, not header root ${header.transactionsRoot}.`,
    );
  }

  const outputDatum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_id: txInclusion.nativeTxId,
        blocks_withdrawals_root: header.withdrawalsRoot,
        blocks_withdrawal_count: header.withdrawalCount,
      },
    },
    WithdrawnInputStep02Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: contracts.steps[1].spendingScriptAddress,
    datum: outputDatum,
    unit: threadToken.unit,
  });
  const membershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const membershipAddress = phasMembershipRewardAddress(
    network,
    membershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: membershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 01 PHAS membership`,
  });
  let layout:
    | { readonly inputIndex: bigint; readonly outputIndex: bigint }
    | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 01`,
    );
    layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 01`,
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 01 output`,
      ),
    };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: requireReferenceInputIndex(
              ctx,
              hubOracleUtxo,
              `${WITHDRAWN_INPUT_CATEGORY_LABEL} hub oracle`,
            ),
            state_queue_node_ref_input_index: requireReferenceInputIndex(
              ctx,
              stateQueueBlockUtxo,
              `${WITHDRAWN_INPUT_CATEGORY_LABEL} state queue`,
            ),
            native_tx_id: txInclusion.nativeTxId,
            native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                membershipAddress,
                `${WITHDRAWN_INPUT_CATEGORY_LABEL} transaction membership`,
              ),
          },
        ],
      },
      WithdrawnInputStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      hubOracleUtxo,
      stateQueueBlockUtxo,
      stepReference,
      ...membershipCarriage.referenceInputs,
    ])
    .withdraw(
      membershipAddress,
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
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = membershipCarriage.attach(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw withdrawnInputSubmitError("step-01 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const txHash = await signed.submit();
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    nextThreadOutRef: `${txHash}#${layout.outputIndex.toString()}`,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    fraudulentHeaderHash: headerHash,
    badTxId: txInclusion.nativeTxId,
    withdrawalsRoot: header.withdrawalsRoot,
    withdrawalCount: header.withdrawalCount,
    computationThreadUnit: threadToken.unit,
  };
};
