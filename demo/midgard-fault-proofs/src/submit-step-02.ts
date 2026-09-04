/**
 * `double-spend` step-02 submitter — binds the second transaction to the same
 * block and forwards both §2.5 anchors to step-03.
 *
 * **Re-derived onto the flat field commitments by #604.** The thread carries
 * `verified_tx1_id`/`verified_tx2_id`; the two field-0 collection commitments it
 * used to forward are gone, because step-03 and step-04 open field 0 through the
 * §8.8 door from the anchors instead.
 */

import {
  DoubleSpendStep02Datum,
  DoubleSpendStep02SpendRedeemer,
  DoubleSpendStep03Datum,
  HUB_ORACLE_ASSET_NAME,
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

import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  requireSingletonUtxo,
  resolveDoubleSpendDeploymentContracts,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  parseSubmitStep01TxInclusion,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireComputationThreadToken,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessSpendingValidatorCarriage,
  witnessWithdrawalValidatorCarriage,
} from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

export type SubmitStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusionPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep02Result = {
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
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly verifiedTx1Id: string;
  readonly nativeTx2Id: string;
  /**
   * The second transaction's §2.5 anchor, as written into thread state. Equal to
   * `nativeTx2Id`; named for what step-03 reads rather than for what this step
   * challenged.
   */
  readonly verifiedTx2Id: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  /** Which route the membership opening took to L1 (issue #545). */
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly publishedChunkOutRefs: readonly string[];
  readonly awaitedConfirmation: boolean;
};

type Step02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

type Step02DatumWithState = DoubleSpendStep02Datum & {
  readonly data: NonNullable<DoubleSpendStep02Datum["data"]>;
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 02 input datum must carry verified tx1 state data.");
  }
  return datum as Step02DatumWithState;
};

export const submitStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
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
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitStep01TxInclusion;
  /**
   * Chunks published by `publishProofChunksV1`, in proof order. When present
   * the second transaction's membership proof reaches L1 through them and
   * never enters this transaction (issue #545).
   */
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  /** Production workflow seam: invoked after local evaluation, before I/O. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep02Result> => {
  const resolvedDeployment = await resolveDoubleSpendDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { doubleSpendCategory, hubOraclePolicyId, contracts } =
    resolvedDeployment;
  const stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;

  const parsedThreadOutRef = parseOutRef(threadOutRef, "--thread-out-ref");
  const parsedStateQueueBlockOutRef = parseOutRef(
    stateQueueBlockOutRef,
    "--state-queue-block-out-ref",
  );
  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedThreadOutRef,
      label: "step-02 computation-thread UTxO",
    }),
    requireSingletonUtxo({
      lucid,
      address: credentialToAddress(
        network,
        scriptHashToCredential(hubOraclePolicyId),
      ),
      unit: toUnit(hubOraclePolicyId, HUB_ORACLE_ASSET_NAME),
      label: "hub oracle",
    }),
    fetchUtxoByOutRef({
      lucid,
      outRef: parsedStateQueueBlockOutRef,
      label: "state-queue block UTxO",
    }),
  ]);
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[1].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 02.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: doubleSpendCategory.categoryId,
    categoryLabel: "double-spend",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(
      `State-queue block header hash ${stateQueueHeaderHash} does not match computation-thread header hash ${threadToken.fraudulentHeaderHash}.`,
    );
  }

  requireNativeTxMatchesCompactCbor(txInclusion);
  if (inputDatum.data.verified_tx1_id === txInclusion.nativeTxId) {
    throw new Error(
      "--tx-inclusion.nativeTxId must differ from the verified tx1 id in the step-02 datum.",
    );
  }

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  // On the chunked route the merkelized published-chunk verifier stands in for
  // the `phas` membership withdrawal; the proof stays in the referenced chunks.
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: contracts.doubleSpend.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "double-spend step 02 validator",
  });
  const inclusionCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriage({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: "double-spend step 02 chunked verify",
      })
    : witnessWithdrawalValidatorCarriage({
        script: phasMembershipScript,
        referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
        label: "double-spend step 02 PHAS membership",
      });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
    ...stepScriptCarriage.referenceInputs,
    ...inclusionCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: "double-spend step 02",
  });
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        verified_tx1_id: inputDatum.data.verified_tx1_id,
        verified_tx2_id: txInclusion.nativeTxId,
      },
    },
    DoubleSpendStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: contracts.doubleSpend.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: Step02Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "double-spend step 02");
    const layout: Step02Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "double-spend step 02"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "double-spend step 02 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "double-spend step 02 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "double-spend step 02 state-queue node",
      ),
    };
    resolvedLayout = layout;
    const common = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      hub_ref_input_index: layout.hubOracleRefInputIndex,
      state_queue_node_ref_input_index: layout.stateQueueNodeRefInputIndex,
      native_tx_id: txInclusion.nativeTxId,
      l2_transaction_source_cbor: txInclusion.l2TransactionSourceCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
    };
    // Same prover-chosen carriage as step-01: the second conflicting
    // transaction's opening is the one that grows with proof depth here.
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: "double-spend step 02",
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
                  "double-spend step 02 PHAS membership",
                ),
            },
          ],
        };
    return Data.to({ Continue: [carriage] }, DoubleSpendStep02SpendRedeemer);
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
  const tx = (
    carriedByChunks
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
        )
  ).pay
    .ToContract(
      contracts.doubleSpend.steps[2].spendingScriptAddress,
      {
        kind: "inline",
        value: step03Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = inclusionCarriage.attach(stepScriptCarriage.attach(tx));

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error("BuildTxWithRedeemer did not resolve step 02 layout.");
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: [
      workflowReferenceScript({
        role: "V1 fraud-proof double-spend step-02",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.doubleSpend.steps[1].spendingScript,
      }),
      carriedByChunks
        ? workflowReferenceScript({
            role: "V1 MPF chunked-verify withdrawal",
            utxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
            expectedScript: chunkedVerifyScript,
          })
        : workflowReferenceScript({
            role: "membership proof withdrawal",
            utxo: witnessReferenceScripts?.phasMembershipWithdraw,
            expectedScript: phasMembershipScript,
          }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
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
    secondStepAddress: contracts.doubleSpend.steps[1].spendingScriptAddress,
    thirdStepAddress: contracts.doubleSpend.steps[2].spendingScriptAddress,
    verifiedTx1Id: inputDatum.data.verified_tx1_id,
    nativeTx2Id: txInclusion.nativeTxId,
    verifiedTx2Id: txInclusion.nativeTxId,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
    publishedChunkOutRefs: chunks.map((chunk) => chunk.outRef),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep02FromFiles = async (
  config: SubmitStep02CliConfig,
): Promise<SubmitStep02Result> => {
  rejectRetiredUnauthenticatedSubmissionRoute({
    command: "submit-step-02",
  });
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await submitStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
