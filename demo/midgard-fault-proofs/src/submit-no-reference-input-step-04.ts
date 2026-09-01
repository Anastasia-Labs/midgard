/**
 * `no-reference-input` step-04 submitter — a non-membership proof, not a field
 * opening.
 *
 * **Unchanged by #604's re-derivation, and checked to be.** The #575 rebind moved
 * this family's step-02 state and redeemer onto the §2.5 anchor and the §8.8
 * door; this step's state and redeemer are exactly what
 * `midgard/fraud_proofs/no_reference_input/step_04` still declares.
 *
 * `no-reference-input` step-04 submitter (Goal task `Q18`, §9.1 output 8).
 *
 * Structural mirror of the `non-existent-input` chain's step 04
 * (`ne-submit-step-04.ts`): proves the challenged input's producing transaction
 * is absent from the block's transactions trie, burns the computation-thread
 * token, and mints the fraud-proof token at the fraud-proof spending address.
 * Only the threaded field differs — `missing_reference_input_tx_id` rather than
 * `missing_input_tx_id`.
 *
 * Nothing in the prepared JSON is trusted for anything the chain re-derives:
 * the exclusion key (the producing transaction id) and the `transactions_root`
 * the proof must open are both read back from the **on-chain** step-04 datum.
 * The prepared file supplies only the MPF proof itself.
 *
 * Proof carriage mirrors Q11 exactly: fitting proofs use the direct pexcludes
 * withdrawal and larger proofs use authenticated published chunks through the
 * shared `NonMembershipCarriage` ABI.
 */
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  type NonMembershipCarriage,
  NoReferenceInputStep04Datum,
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

import { PEXCLUDES_EXCLUSION_WITHDRAW_TITLE } from "./ne-submit-step-03.js";
import {
  chunkedNonMembershipClaimRedeemer,
  chunkedVerifyWithdrawalScript,
  derivedChunkReferenceIndices,
  type PublishedProofChunkV1,
  requireBuiltChunkReferenceIndices,
  walletInputsExcludingChunks,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPexcludesProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  type ResolvedProverSigner,
  resolveNoReferenceInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessSpendingValidatorCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "./workflow/transaction-boundary-v1.js";

export type SubmitNoReferenceInputStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly txsNonMembershipProofPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitNoReferenceInputStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly fourthStepAddress: string;
  readonly missingReferenceInputTxId: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly nonMembershipProofScriptRedeemerIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly proofCarriage: "redeemer" | "published-chunks";
  readonly publishedChunkOutRefs: readonly string[];
  readonly awaitedConfirmation: boolean;
};

type Step04DatumWithState = NoReferenceInputStep04Datum & {
  readonly data: NonNullable<NoReferenceInputStep04Datum["data"]>;
};

const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NoReferenceInputStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 04 input datum must carry missing-reference-input-tx-id state data.",
    );
  }
  return datum as Step04DatumWithState;
};

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
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
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
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "no-reference-input step 04 validator",
  });
  const nonMembershipCarriage = carriedByChunks
    ? witnessWithdrawalValidatorCarriageV1({
        script: chunkedVerifyScript,
        referenceUtxo: witnessReferenceScripts?.chunkedVerifyWithdraw,
        label: "no-reference-input step 04 chunked verify",
      })
    : witnessWithdrawalValidatorCarriageV1({
        script: pexcludesScript,
        referenceUtxo: witnessReferenceScripts?.pexcludesWithdraw,
        label: "no-reference-input step 04 pexcludes exclusion",
      });
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "no-reference-input step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
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
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
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

export const submitNoReferenceInputStep04FromFiles = async (
  config: SubmitNoReferenceInputStep04CliConfig,
): Promise<SubmitNoReferenceInputStep04Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.txsNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitNoReferenceInputStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    txsNonMembershipProofCbor: proofJson as string,
    awaitConfirmation: config.awaitConfirmation,
  });
};
