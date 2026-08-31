import {
  commitCountedRootProgram,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
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
  scriptHashToCredential,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { prepareNativeTxInclusionCarriageV1 } from "../native-inclusion-carriage-v1.js";
import {
  type PublishedProofChunkV1,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  parseOutRef,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
} from "../runtime.js";
import {
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { computationThreadOutputPredicate } from "../tx-layout.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
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
  publishedProofChunks,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
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
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
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
  const chunks = publishedProofChunks ?? [];
  const inclusionCarriage = prepareNativeTxInclusionCarriageV1({
    blueprint,
    network,
    txInclusion,
    publishedProofChunks: chunks,
    witnessReferenceScripts,
    label: `${WITHDRAWN_INPUT_CATEGORY_LABEL} step 01`,
    baseReferenceInputs: [hubOracleUtxo, stateQueueBlockUtxo, stepReference],
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
          inclusionCarriage.redeemer(ctx, {
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
          }),
        ],
      },
      WithdrawnInputStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(inclusionCarriage.referenceInputs)
    .pay.ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: outputDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [threadToken.unit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = inclusionCarriage.attachWithdrawal(base);
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (layout === undefined) {
    throw withdrawnInputSubmitError("step-01 layout was not resolved.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof withdrawn-input step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
        ...inclusionCarriage.referenceScriptCandidates,
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw withdrawnInputSubmitError(
      `step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
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
