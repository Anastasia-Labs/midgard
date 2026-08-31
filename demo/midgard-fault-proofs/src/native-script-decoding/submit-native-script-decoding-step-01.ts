/**
 * `native-script-decoding` step-01 submitters (offchain plan §4.2).
 *
 * Two arms, two entry points:
 *
 * - `submitNativeScriptDecodingStep01BindNormal` — direction A over a Normal
 *   source. The accepted transaction is bound through the header's counted
 *   `transactions_root` via the shared inclusion machinery, on either
 *   carriage (redeemer-carried proof, or the #545 published chunks for a
 *   proof the 16,384-byte envelope cannot hold). The §2.4.3(d) predicate is
 *   re-checked locally before anything is paid for: a leaf whose embedded
 *   validity scalar does not claim acceptance can never bind on-chain.
 * - `submitNativeScriptDecodingStep01RecordForced` — either direction over a
 *   Forced source. Only the direction is recorded; the forced leaf itself is
 *   opened at step-02 under the thread NFT's header, so this transaction
 *   needs no reference inputs at all.
 */
import {
  HUB_ORACLE_ASSET_NAME,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
  NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
  NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1,
  type NativeScriptDecodingBindStateV1,
  NativeScriptDecodingStep01SpendRedeemer,
  NativeScriptDecodingStep02Datum,
  type NativeTxInclusionCarriage,
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
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunkV1,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "../proof-chunk-carriage.js";
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
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { NativeScriptDecodingContractsV1 } from "./contracts-v1.js";
import {
  nativeScriptDecodingStepLabelV1,
  nativeScriptDecodingSubmitError,
  requireNativeScriptDecodingReferenceScriptV1,
  requireNativeScriptDecodingThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = nativeScriptDecodingStepLabelV1(0);

export type SubmitNativeScriptDecodingStep01Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  /** The `BindStateV1` the thread now carries. */
  readonly bindState: NativeScriptDecodingBindStateV1;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

/** Source the mandatory authenticated step-01 reference script. */
const sourceStepScript = <
  T extends {
    readFrom: (utxos: UTxO[]) => T;
  },
>({
  tx,
  contracts,
  referenceScriptUtxo,
}: {
  readonly tx: T;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly referenceScriptUtxo: UTxO;
}): T =>
  tx.readFrom([
    requireNativeScriptDecodingReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[0].spendingScriptHash,
      stepIndex: 0,
    }),
  ]);

export const submitNativeScriptDecodingStep01BindNormal = async ({
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
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  /** Present → the #545 published-chunk carriage; absent → redeemer-carried. */
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  /** Q3: the mandatory published step-01 reference script. */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep01Result> => {
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
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
    throw nativeScriptDecodingSubmitError(
      `state-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  requireNativeTxMatchesCompactCbor(txInclusion);
  // §2.4.3(d): a normal leaf IS an acceptance verdict only when its own
  // embedded validity scalar claims `TxIsValid`.
  if (txInclusion.nativeTx.validity_code !== 0n) {
    throw nativeScriptDecodingSubmitError(
      `--tx-inclusion.nativeTx carries validity code ${txInclusion.nativeTx.validity_code.toString()}, so the committed leaf is not an acceptance and direction A cannot bind it.`,
    );
  }
  const bindState: NativeScriptDecodingBindStateV1 = {
    direction: NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
    source_kind: NATIVE_SCRIPT_DECODING_SOURCE_KIND_NORMAL_V1,
    verified_tx_id: txInclusion.nativeTxId,
  };

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({ walletUtxos, chunks }),
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  // On the chunked route the merkelized published-chunk verifier stands in
  // for the `phas` membership withdrawal.
  const inclusionCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriageV1({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: `${STEP_LABEL} chunked verify`,
      })
    : witnessWithdrawalValidatorCarriageV1({
        script: phasMembershipScript,
        referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
        label: `${STEP_LABEL} PHAS membership`,
      });
  // The complete final reference-input set MUST stand before any chunk
  // reference-index derivation runs (bug fc635c8f).
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
    requireNativeScriptDecodingReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[0].spendingScriptHash,
      stepIndex: 0,
    }),
    ...inclusionCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: STEP_LABEL,
  });
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: bindState },
    NativeScriptDecodingStep02Datum,
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
    const common = {
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
      l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
    };
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: STEP_LABEL,
    });
    const carriage: NativeTxInclusionCarriage = carriedByChunks
      ? {
          PublishedChunkInclusion: [
            {
              ...common,
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedInclusion: [
            {
              ...common,
              tx_membership_proof: txInclusion.txMembershipProof,
              inclusion_proof_script_withdraw_redeemer_index:
                requireWithdrawalRedeemerIndex(
                  ctx,
                  phasRewardAddress,
                  `${STEP_LABEL} PHAS membership`,
                ),
            },
          ],
        };
    return Data.to(
      { Continue: [{ BindNormalTransaction: { carriage } }] },
      NativeScriptDecodingStep01SpendRedeemer,
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
    .readFrom(referenceInputs);
  const withCarriage = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
        chunkedMembershipClaimRedeemer({
          merkleRoot: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.l2TransactionSourceCbor,
          orderedChunkReferenceInputIndices: resolvedChunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        phasRewardAddress,
        0n,
        encodeRawPhasMembershipProofRedeemer({
          root: txInclusion.transactionsPhasRoot,
          keyBytes: txInclusion.nativeTxId,
          valueBytes: txInclusion.l2TransactionSourceCbor,
          membershipProofCbor: txInclusion.txMembershipProofCbor,
        }),
      );
  const paid = withCarriage.pay
    .ToContract(
      contracts.steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = inclusionCarriage.attach(paid);

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the step-01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof native-script-decoding step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
        {
          role: "V1 MPF chunked-verify withdrawal",
          utxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
          expectedScript: chunkedVerifyScript,
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
    throw nativeScriptDecodingSubmitError(
      `step-01 bind-normal provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    bindState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitNativeScriptDecodingStep01RecordForced = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  direction,
  referenceScriptUtxo,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: NativeScriptDecodingContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** 0 (wrongful acceptance) or 1 (wrongful rejection); fixed for the thread's life. */
  readonly direction: bigint;
  readonly referenceScriptUtxo: UTxO;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNativeScriptDecodingStep01Result> => {
  if (
    direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_ACCEPTANCE_V1 &&
    direction !== NATIVE_SCRIPT_DECODING_DIRECTION_WRONGFUL_REJECTION_V1
  ) {
    throw nativeScriptDecodingSubmitError(
      `direction ${direction.toString()} is outside {0, 1}.`,
    );
  }
  const { threadUtxo, threadToken } =
    await requireNativeScriptDecodingThreadUtxoV1({
      lucid,
      contracts,
      categoryId,
      stepIndex: 0,
      threadOutRef,
    });
  requireInitialStepDatum({ threadUtxo, signer });
  const bindState: NativeScriptDecodingBindStateV1 = {
    direction,
    source_kind: NATIVE_SCRIPT_DECODING_SOURCE_KIND_FORCED_V1,
    verified_tx_id: "",
  };

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step02Datum = Data.to(
    { fraud_prover: signer.paymentKeyHash, data: bindState },
    NativeScriptDecodingStep02Datum,
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
            RecordForcedSource: {
              direction,
              input_index: layout.inputIndex,
              output_index: layout.outputIndex,
            },
          },
        ],
      },
      NativeScriptDecodingStep01SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = sourceStepScript({
    tx: lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom([threadUtxo], redeemer)
      .pay.ToContract(
        contracts.steps[1].spendingScriptAddress,
        { kind: "inline", value: step02Datum },
        threadAssets,
      )
      .addSignerKey(signer.paymentKeyHash),
    contracts,
    referenceScriptUtxo,
  });

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw nativeScriptDecodingSubmitError(
      "BuildTxWithRedeemer did not resolve the step-01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof native-script-decoding step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[0].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw nativeScriptDecodingSubmitError(
      `step-01 record-forced provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: contracts.steps[1].spendingScriptAddress,
    bindState,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};
