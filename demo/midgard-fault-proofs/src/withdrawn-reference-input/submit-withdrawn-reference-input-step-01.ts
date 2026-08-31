/**
 * `withdrawn-reference-input` step-01 submitter (Q19 offchain plan §4.2).
 *
 * Thread state carries the §2.5 anchor — the disputed transaction's **id**.
 * Step-02 re-opens field 1 through the §8.8 door, which keeps field 1
 * distinguishable from field 0: plain hashing gives equal item lists equal
 * commitments, while the opening's field position supplies the distinction.
 *
 * Structurally this uses the established native transaction-root inclusion
 * path plus singleton hub-oracle and state-queue reference inputs. Its next
 * state forwards the bad transaction id and the header's counted withdrawals
 * commitment.
 *
 * Nothing in the prepared JSON is trusted. Before a transaction is built this
 * module re-derives, from the **on-chain** state-queue block header, the counted
 * `transactions_root` over the supplied raw PHAS root and the header's own
 * `l2TransactionCount`, and requires it to equal the committed
 * `transactionsRoot`. The bad transaction is re-decoded from its canonical
 * compact CBOR and its canonical id recomputed, so the `bad_tx_id` forwarded
 * to step 02 is derived from committed bytes rather than a prepared field.
 *
 * Proof carriage: this family's on-chain step-01 args are the flat
 * `NativeTxInclusionArgs` record (see
 * `onchain/aiken/lib/midgard/fraud-proofs/withdrawn-reference-input/step-01.ak` and
 * `WithdrawnReferenceInputStep01SpendRedeemerSchema`), i.e. the pre-#545
 * redeemer-carried route only. There is no `NativeTxInclusionCarriage` wrapper
 * and therefore no published-chunk route to mirror.
 */
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
  WithdrawnReferenceInputStep01SpendRedeemer,
  WithdrawnReferenceInputStep02Datum,
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
  parseSubmitStep01TxInclusion,
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
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { WithdrawnReferenceInputContractsV1 } from "./contracts-v1.js";
import {
  requireWithdrawnReferenceInputReferenceScriptV1,
  requireWithdrawnReferenceInputThreadUtxoV1,
  withdrawnReferenceInputSubmitError,
} from "./submit-common-v1.js";

// The withdrawn-reference-input proof commits the bad transaction by the node's native
// transaction root (the same inclusion path as double-spend and
// non-existent-input), so the tx-inclusion material is identical to
// `submit-step-01`'s and to what `prepareWithdrawnReferenceInputV1` returns.
export type WithdrawnReferenceInputStep01TxInclusion = SubmitStep01TxInclusion;
export const parseWithdrawnReferenceInputStep01TxInclusion =
  parseSubmitStep01TxInclusion;

export type SubmitWithdrawnReferenceInputStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly firstStepAddress: string;
  readonly secondStepAddress: string;
  /**
   * The §2.5 anchor this step wrote into thread state — what step-02 reads back
   * and opens field 1 against. It was already reported here before #604; what
   * changed is that it is now the thread's state rather than a convenience.
   */
  readonly badTxId: string;
  readonly blocksWithdrawalsRoot: string;
  readonly blocksWithdrawalCount: bigint;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly referenceScriptOutRef: string;
  readonly awaitedConfirmation: boolean;
};

type WithdrawnReferenceInputStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitWithdrawnReferenceInputStep01 = async ({
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
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly contracts: WithdrawnReferenceInputContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: WithdrawnReferenceInputStep01TxInclusion;
  readonly referenceScriptUtxo: UTxO;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitWithdrawnReferenceInputStep01Result> => {
  const steps = contracts.steps;
  const [{ threadUtxo, threadToken }, hubOracleUtxo, stateQueueBlockUtxo] =
    await Promise.all([
      requireWithdrawnReferenceInputThreadUtxoV1({
        lucid,
        contracts,
        categoryId,
        stepIndex: 0,
        threadOutRef,
      }),
      requireSingletonUtxo({
        lucid,
        address: credentialToAddress(
          network,
          scriptHashToCredential(contracts.hubOraclePolicyId),
        ),
        unit: toUnit(contracts.hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
        label: "hub oracle",
      }),
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(
          stateQueueBlockOutRef,
          "--state-queue-block-out-ref",
        ),
        label: "state-queue block UTxO",
      }),
    ]);
  requireInitialStepDatum({ threadUtxo, signer });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId: contracts.stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw withdrawnReferenceInputSubmitError(
      `State-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderV1FromStateQueueDatum(stateQueueNodeView),
  );
  const countedTransactionsRoot = await Effect.runPromise(
    commitCountedRootProgram({
      domain: ROOT_DOMAINS.transactionsV1,
      phasRoot: txInclusion.transactionsPhasRoot,
      count: header.l2TransactionCount,
    }),
  );
  if (countedTransactionsRoot !== header.transactionsRoot) {
    throw withdrawnReferenceInputSubmitError(
      `--tx-inclusion.transactionsPhasRoot does not open the committed transactions_root: derived=${countedTransactionsRoot}, header=${header.transactionsRoot}.`,
    );
  }

  requireNativeTxMatchesCompactCbor(txInclusion);
  // §2.5's anchor, read off the compact structure the block's
  // `transactions_root` committed — the only provenance `BodyAnchor` accepts.
  const badTxId = txInclusion.nativeTxId;

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const referenceInputs = [hubOracleUtxo, stateQueueBlockUtxo];
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const membershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: "withdrawn-reference-input step 01 PHAS membership",
  });
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_id: badTxId,
        blocks_withdrawals_root: header.withdrawalsRoot,
        blocks_withdrawal_count: header.withdrawalCount,
      },
    },
    WithdrawnReferenceInputStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: WithdrawnReferenceInputStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(
      ctx,
      threadUtxo,
      "withdrawn-reference-input step 01",
    );
    const layout: WithdrawnReferenceInputStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "withdrawn-reference-input step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "withdrawn-reference-input step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "withdrawn-reference-input step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "withdrawn-reference-input step 01 state-queue node",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
            native_tx_id: txInclusion.nativeTxId,
            l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasRewardAddress,
                "withdrawn-reference-input step 01 PHAS membership",
              ),
          },
        ],
      },
      WithdrawnReferenceInputStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const stepReference = requireWithdrawnReferenceInputReferenceScriptV1({
    utxo: referenceScriptUtxo,
    expectedScriptHash: steps[0].spendingScriptHash,
    stepIndex: 0,
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom([
      ...referenceInputs,
      stepReference,
      ...membershipCarriage.referenceInputs,
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
      steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const tx = membershipCarriage.attach(base);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw withdrawnReferenceInputSubmitError(
      "BuildTxWithRedeemer did not resolve withdrawn-reference-input step 01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof withdrawn-reference-input step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
        {
          role: "membership proof withdrawal",
          utxo: witnessReferenceScripts?.phasMembershipWithdraw,
          expectedScript: phasMembershipScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw withdrawnReferenceInputSubmitError(
      `step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
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
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: steps[0].spendingScriptAddress,
    secondStepAddress: steps[1].spendingScriptAddress,
    badTxId,
    blocksWithdrawalsRoot: header.withdrawalsRoot,
    blocksWithdrawalCount: header.withdrawalCount,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    referenceScriptOutRef: `${stepReference.txHash}#${stepReference.outputIndex.toString()}`,
    awaitedConfirmation: awaitConfirmation,
  };
};
