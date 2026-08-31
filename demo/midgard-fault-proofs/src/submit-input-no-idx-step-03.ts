/**
 * **Re-derived onto the flat field commitments by #604.** This step forwards the
 * *producing* transaction's §2.5 anchor to step-04, where it used to forward that
 * transaction's `outputs_hash`; step-04 opens its field 2 through the §8.8 door.
 * See `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
 * `input-no-idx` step-03 submitter (Goal task `Q13`, §9.1 output 8).
 *
 * Re-enters the block binding for the **producing** transaction of the input
 * step 02 forwarded. Nothing in the prepared file is trusted: the counted
 * `transactions_root` is re-derived from the on-chain header, the producing
 * transaction is re-decoded from its canonical compact CBOR, and its canonical
 * id must equal the `bad_input_tx_id` read back from the on-chain step-02
 * datum. Only that transaction's own committed `outputs_hash` is forwarded.
 */
import {
  commitCountedRootProgram,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  InputNoIdxStep03Datum,
  InputNoIdxStep03SpendRedeemer,
  InputNoIdxStep04Datum,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  ROOT_DOMAINS,
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

import { prepareNativeTxInclusionCarriageV1 } from "./native-inclusion-carriage-v1.js";
import {
  type PublishedProofChunkV1,
  walletInputsExcludingChunks,
} from "./proof-chunk-carriage.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  requireSingletonUtxo,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveInputNoIdxDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  parseSubmitStep01TxInclusion,
  requireComputationThreadToken,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessSpendingValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "./workflow/transaction-boundary-v1.js";

export type SubmitInputNoIdxStep03CliConfig = SubmitProviderConfig & {
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

export type SubmitInputNoIdxStep03Result = {
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
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  /**
   * The §2.5 anchor this step forwards to step-04 — the *producing*
   * transaction's id. Before #604 the thread carried that transaction's
   * `outputs_hash` instead; step-04 now opens its field 2 through the §8.8 door,
   * so what it needs forwarded is the anchor.
   */
  readonly producingTxId: string;
  readonly badInputOutputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InputNoIdxStep03Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

type InputNoIdxStep03DatumWithState = InputNoIdxStep03Datum & {
  readonly data: NonNullable<InputNoIdxStep03Datum["data"]>;
};

const requireStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InputNoIdxStep03DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InputNoIdxStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Input-no-idx step 03 input datum must carry the challenged input.",
    );
  }
  return datum as InputNoIdxStep03DatumWithState;
};

export const submitInputNoIdxStep03 = async ({
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
  readonly publishedProofChunks?: readonly PublishedProofChunkV1[];
  /** The mandatory published step-03 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInputNoIdxStep03Result> => {
  const resolvedDeployment = await resolveInputNoIdxDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { nonExistentInputNoIndexCategory, hubOraclePolicyId, contracts } =
    resolvedDeployment;
  const chain = contracts.nonExistentInputNoIndex;
  const stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;

  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "input-no-idx step-03 computation-thread UTxO",
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
      outRef: parseOutRef(stateQueueBlockOutRef, "--state-queue-block-out-ref"),
      label: "state-queue block UTxO",
    }),
  ]);
  if (threadUtxo.address !== chain.steps[2].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at input-no-idx step 03.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputNoIndexCategory.categoryId,
    categoryLabel: "input-no-idx",
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  const stateQueueHeaderHash = resolveFraudulentHeaderHash({
    stateQueuePolicyId,
    fraudulentBlockUtxo: stateQueueBlockUtxo,
  });
  if (stateQueueHeaderHash !== threadToken.fraudulentHeaderHash) {
    throw new Error(
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
    throw new Error(
      `--tx-inclusion.transactionsPhasRoot does not open the committed transactions_root: derived=${countedTransactionsRoot}, header=${header.transactionsRoot}.`,
    );
  }

  requireNativeTxMatchesCompactCbor(txInclusion);
  if (txInclusion.nativeTxId !== inputDatum.data.bad_input_tx_id) {
    throw new Error(
      `--tx-inclusion is transaction ${txInclusion.nativeTxId}, but the thread challenges an input produced by ${inputDatum.data.bad_input_tx_id}.`,
    );
  }
  // §2.5's anchor for the producing transaction, read off the compact structure
  // the block's `transactions_root` committed.
  const producingTxId = txInclusion.nativeTxId;
  const badInputOutputIndex = inputDatum.data.bad_input_output_index;

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: chain.steps[2].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "input-no-idx step 03 validator",
  });
  const inclusionCarriage = prepareNativeTxInclusionCarriageV1({
    blueprint,
    network,
    txInclusion,
    publishedProofChunks: chunks,
    witnessReferenceScripts,
    label: "input-no-idx step 03",
    baseReferenceInputs: [
      hubOracleUtxo,
      stateQueueBlockUtxo,
      ...stepScriptCarriage.referenceInputs,
    ],
  });
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        producing_tx_id: producingTxId,
        bad_input_output_index: badInputOutputIndex,
      },
    },
    InputNoIdxStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: chain.steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: InputNoIdxStep03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "input-no-idx step 03");
    const layout: InputNoIdxStep03Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "input-no-idx step 03"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "input-no-idx step 03 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "input-no-idx step 03 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "input-no-idx step 03 state-queue node",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          inclusionCarriage.redeemer(ctx, {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            hub_ref_input_index: layout.hubOracleRefInputIndex,
            state_queue_node_ref_input_index:
              layout.stateQueueNodeRefInputIndex,
          }),
        ],
      },
      InputNoIdxStep03SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(inclusionCarriage.referenceInputs)
    .pay.ToContract(
      chain.steps[3].spendingScriptAddress,
      { kind: "inline", value: step04Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = inclusionCarriage.attachWithdrawal(
    stepScriptCarriage.attach(tx),
  );

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve input-no-idx step 03 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof non-existent-input-no-index step-03",
          utxo: referenceScriptUtxo,
          expectedScript:
            contracts.nonExistentInputNoIndex.steps[2].spendingScript,
        },
        ...inclusionCarriage.referenceScriptCandidates,
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `input-no-idx step-03 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    thirdStepAddress: chain.steps[2].spendingScriptAddress,
    fourthStepAddress: chain.steps[3].spendingScriptAddress,
    producingTxId,
    badInputOutputIndex: Number(badInputOutputIndex),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitInputNoIdxStep03FromFiles = async (
  config: SubmitInputNoIdxStep03CliConfig,
): Promise<SubmitInputNoIdxStep03Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await submitInputNoIdxStep03({
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
