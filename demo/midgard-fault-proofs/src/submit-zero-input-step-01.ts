import { compareOutRefs } from "@al-ft/midgard-core/out-ref";
import {
  HUB_ORACLE_ASSET_NAME,
  nativeTxBodyHasZeroInputViolation,
  type NativeTxInclusionCarriage,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  ZeroInputStep01SpendRedeemer,
  ZeroInputStep02Datum,
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
} from "@lucid-evolution/lucid";

import { rejectRetiredUnauthenticatedSubmissionRouteV1 } from "./legacy-submission-boundary-v1.js";
import {
  chunkedMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  type PublishedProofChunkV1,
  walletInputsExcludingChunks,
} from "./publish-proof-chunks.js";
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
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  resolveZeroInputDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  parseSubmitStep01TxInclusion,
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireComputationThreadToken,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

export type SubmitZeroInputStep01CliConfig = SubmitProviderConfig & {
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

export type SubmitZeroInputStep01Result = {
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
  readonly nativeTxId: string;
  readonly badTxSpendInputsHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  /** Which route the membership opening took to L1 (issue #545). */
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly publishedChunkOutRefs: readonly string[];
  readonly awaitedConfirmation: boolean;
};

type ZeroInputStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitZeroInputStep01 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  publishedProofChunks,
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
   * the membership proof reaches L1 through them and never enters this
   * transaction (issue #545).
   */
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitZeroInputStep01Result> => {
  const resolvedDeployment = await resolveZeroInputDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { zeroInputCategory, hubOraclePolicyId, contracts } =
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
      label: "zero-input step-01 computation-thread UTxO",
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
    threadUtxo.address !== contracts.zeroInput.steps[0].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at zero-input step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: zeroInputCategory.categoryId,
    categoryLabel: "zero-input",
  });
  requireInitialStepDatum({ threadUtxo, signer });
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
  if (
    !nativeTxBodyHasZeroInputViolation({ txBody: txInclusion.nativeTx.body })
  ) {
    throw new Error(
      "--tx-inclusion.nativeTx spends at least one input, so it does not violate the zero-input ledger rule.",
    );
  }
  const badTxSpendInputsHash = txInclusion.nativeTx.body.spend_inputs_hash;

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...chunks.map((chunk) => chunk.utxo),
  ];
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
  const canonicalReferenceOrder = [...referenceInputs].sort(compareOutRefs);
  const resolvedChunkIndices = chunks.map((chunk) => {
    const index = canonicalReferenceOrder.findIndex(
      (utxo) =>
        utxo.txHash === chunk.utxo.txHash &&
        utxo.outputIndex === chunk.utxo.outputIndex,
    );
    if (index < 0) {
      throw new Error(
        `Published proof chunk ${chunk.outRef} is not among the zero-input step 01 reference inputs.`,
      );
    }
    return BigInt(index);
  });
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: { bad_tx_spend_inputs_hash: badTxSpendInputsHash },
    },
    ZeroInputStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.zeroInput.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: ZeroInputStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "zero-input step 01");
    const layout: ZeroInputStep01Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "zero-input step 01"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "zero-input step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "zero-input step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "zero-input step 01 state-queue node",
      ),
    };
    resolvedLayout = layout;
    const common = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      hub_ref_input_index: layout.hubOracleRefInputIndex,
      state_queue_node_ref_input_index: layout.stateQueueNodeRefInputIndex,
      native_tx_id: txInclusion.nativeTxId,
      native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
      transactions_phas_root: txInclusion.transactionsPhasRoot,
    };
    // The prover chooses the carriage. Published chunks are the route for a
    // proof the 16,384-byte envelope cannot carry (issue #545); the redeemer
    // route stays the cheaper one for every proof that fits.
    // The canonical order derived above is the order the ledger will present;
    // re-deriving it from the builder context here proves the two agree.
    for (const [position, chunk] of chunks.entries()) {
      const contextIndex = requireReferenceInputIndex(
        ctx,
        chunk.utxo,
        "zero-input step 01 published proof chunk",
      );
      if (contextIndex !== resolvedChunkIndices[position]) {
        throw new Error(
          `Published proof chunk ${chunk.outRef} landed at reference-input index ${contextIndex.toString()}, not the derived ${String(resolvedChunkIndices[position])}.`,
        );
      }
    }
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
                  "zero-input step 01 PHAS membership",
                ),
            },
          ],
        };
    return Data.to({ Continue: [carriage] }, ZeroInputStep01SpendRedeemer);
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
  // On the published-chunk route the delegated `phas` invocation disappears
  // along with the proof: the step verifies the reassembled proof itself.
  const tx = (
    carriedByChunks
      ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
          chunkedMembershipClaimRedeemer({
            merkleRoot: txInclusion.transactionsPhasRoot,
            keyBytes: txInclusion.nativeTxId,
            valueBytes: txInclusion.nativeTxCompactCbor,
            orderedChunkReferenceInputIndices: resolvedChunkIndices,
          })) satisfies BuildTxWithRedeemer)
      : base.withdraw(
          phasRewardAddress,
          0n,
          encodeRawPhasMembershipProofRedeemer({
            root: txInclusion.transactionsPhasRoot,
            keyBytes: txInclusion.nativeTxId,
            valueBytes: txInclusion.nativeTxCompactCbor,
            membershipProofCbor: txInclusion.txMembershipProofCbor,
          }),
        )
  ).pay
    .ToContract(
      contracts.zeroInput.steps[1].spendingScriptAddress,
      {
        kind: "inline",
        value: step02Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.zeroInput.steps[0].spendingScript);
  const completedTx = carriedByChunks
    ? tx.attach.WithdrawalValidator(chunkedVerifyScript)
    : tx.attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve zero-input step 01 layout.",
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
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: contracts.zeroInput.steps[0].spendingScriptAddress,
    secondStepAddress: contracts.zeroInput.steps[1].spendingScriptAddress,
    nativeTxId: txInclusion.nativeTxId,
    badTxSpendInputsHash,
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

export const submitZeroInputStep01FromFiles = async (
  config: SubmitZeroInputStep01CliConfig,
): Promise<SubmitZeroInputStep01Result> => {
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: "submit-zero-input-step-01",
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
  return await submitZeroInputStep01({
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
