import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  type NonMembershipCarriage,
  NoReferenceInputStep04SpendRedeemer,
  Proof,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  chunkedNonMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunk,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  type ResolvedProverSigner,
  resolveNoReferenceInputDeploymentContracts,
} from "./runtime.js";
import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "./step-support.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  requireStep04Datum,
  type SubmitNoReferenceInputStep04Result,
} from "./submit-no-reference-input-step-04.submit-no-reference-input-step04-result.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
  witnessWithdrawalValidatorCarriage,
} from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

export const submitNoReferenceInputStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  txsNonMembershipProofCbor,
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
  readonly txsNonMembershipProofCbor: string;
  /** Authenticated proof chunks, in proof order, selected only after fit refusal. */
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNoReferenceInputStep04Result> => {
  const { noReferenceInputCategory, contracts } =
    await resolveNoReferenceInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });
  const steps = contracts.noReferenceInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "no-reference-input step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  const proof = Data.from(txsNonMembershipProofCbor, Proof);

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const carriedByChunks = chunks.length > 0;
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const pexcludesScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PEXCLUDES_EXCLUSION_WITHDRAW_TITLE),
  };
  const pexcludesRewardAddress = phasMembershipRewardAddress(
    network,
    pexcludesScript,
  );
  const chunkedVerifyScript = chunkedVerifyWithdrawalScript(blueprint);
  const chunkedVerifyRewardAddress = phasMembershipRewardAddress(
    network,
    chunkedVerifyScript,
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "no-reference-input step 04 validator",
  });
  const nonMembershipCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriage({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: "no-reference-input step 04 chunked verify",
      })
    : witnessWithdrawalValidatorCarriage({
        script: pexcludesScript,
        referenceUtxo: witnessReferenceScripts?.pexcludesWithdraw,
        label: "no-reference-input step 04 pexcludes exclusion",
      });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "no-reference-input step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "no-reference-input step 04 fraud-proof mint",
  });
  const referenceInputs = [
    ...chunks.map((chunk) => chunk.utxo),
    ...stepScriptCarriage.referenceInputs,
    ...nonMembershipCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const resolvedChunkIndices = derivedChunkReferenceIndices({
    referenceInputs,
    chunks,
    label: "no-reference-input step 04",
  });
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

  let spendLayout:
    | {
        inputIndex: bigint;
        outputIndex: bigint;
        fraudProofMintRedeemerIndex: bigint;
        nonMembershipProofScriptRedeemerIndex: bigint;
      }
    | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 04");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 04",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        "no-reference-input step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "no-reference-input step 04 fraud-proof",
      ),
      nonMembershipProofScriptRedeemerIndex: requireWithdrawalRedeemerIndex(
        ctx,
        carriedByChunks ? chunkedVerifyRewardAddress : pexcludesRewardAddress,
        "no-reference-input step 04 txs non-membership",
      ),
    };
    spendLayout = layout;
    requireBuiltChunkReferenceIndices({
      ctx,
      chunks,
      derived: resolvedChunkIndices,
      label: "no-reference-input step 04",
    });
    const carriage: NonMembershipCarriage = carriedByChunks
      ? {
          PublishedChunkNonMembership: [
            {
              ordered_chunk_reference_input_indices: resolvedChunkIndices,
            },
          ],
        }
      : {
          RedeemerCarriedNonMembership: {
            non_membership_proof: proof,
            non_membership_proof_script_redeemer_index:
              layout.nonMembershipProofScriptRedeemerIndex,
          },
        };
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            non_membership_in_txs: carriage,
          },
        ],
      },
      NoReferenceInputStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      "no-reference-input step 04 fraud-proof mint",
    );
    const ctIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "no-reference-input step 04 computation-thread burn",
    );
    computationThreadMintRedeemerIndex = ctIndex;
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: ctIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const computationThreadSuccessRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "no-reference-input step 04 computation-thread burn",
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer);
  // Without published witnesses this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the
  // carriages produced reference inputs at all.
  const base =
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs]);
  const withCarriage = carriedByChunks
    ? base.withdraw(chunkedVerifyRewardAddress, 0n, ((_ctx) =>
        chunkedNonMembershipClaimRedeemer({
          merkleRoot: inputDatum.data.blocks_transactions_root,
          keyBytes: inputDatum.data.missing_reference_input_tx_id,
          orderedChunkReferenceInputIndices: resolvedChunkIndices,
        })) satisfies BuildTxWithRedeemer)
    : base.withdraw(
        pexcludesRewardAddress,
        0n,
        encodeRawPexcludesProofRedeemer({
          root: inputDatum.data.blocks_transactions_root,
          keyBytes: inputDatum.data.missing_reference_input_tx_id,
          nonMembershipProofCbor: txsNonMembershipProofCbor,
        }),
      );
  const chained = withCarriage
    .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      fraudProofAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(
      nonMembershipCarriage.attach(stepScriptCarriage.attach(chained)),
    ),
  );
  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve no-reference-input step 04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof no-reference-input step-04",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.noReferenceInput.steps[3].spendingScript,
        },
        {
          role: carriedByChunks
            ? "V1 MPF chunked-verify withdrawal"
            : "V1 MPF pexcludes withdrawal",
          utxo: carriedByChunks
            ? witnessReferenceScripts?.chunkedVerifyWithdraw
            : witnessReferenceScripts?.pexcludesWithdraw,
          expectedScript: carriedByChunks
            ? chunkedVerifyScript
            : pexcludesScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `no-reference-input step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: steps[3].spendingScriptAddress,
    missingReferenceInputTxId: inputDatum.data.missing_reference_input_tx_id,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    nonMembershipProofScriptRedeemerIndex: Number(
      spendLayout.nonMembershipProofScriptRedeemerIndex,
    ),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    proofCarriage: carriedByChunks ? "published-chunks" : "redeemer",
    publishedChunkOutRefs: chunks.map((chunk) => outRefLabel(chunk.utxo)),
    awaitedConfirmation: awaitConfirmation,
  };
};
