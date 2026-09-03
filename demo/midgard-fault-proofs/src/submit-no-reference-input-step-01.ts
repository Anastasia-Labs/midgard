/**
 * `no-reference-input` step-01 submitter (Goal task `Q18`, §9.1 output 8).
 *
 * **Re-derived onto the flat field commitments by #604.** Thread state carries
 * the §2.5 anchor — the disputed transaction's **id** — where it used to carry
 * that transaction's `reference_inputs_hash`. Step-02 re-opens field 1 through
 * the §8.8 door, which is also what keeps field 1 distinguishable from field 0:
 * §4's plain hashing gives the two the same commitment for the same items, and
 * only the position tells them apart. See
 * `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
 * Structural mirror of the `non-existent-input` chain's step 01
 * (`ne-submit-step-01.ts`): the same applied-parameter order, the same native
 * transaction-root inclusion path, the same singleton hub-oracle and
 * state-queue reference inputs. Only the field lifted out of the bad
 * transaction differs — the native `reference_inputs_hash` rather than
 * `spend_inputs_hash`.
 *
 * Nothing in the prepared JSON is trusted. Before a transaction is built this
 * module re-derives, from the **on-chain** state-queue block header, the counted
 * `transactions_root` over the supplied raw PHAS root and the header's own
 * `l2TransactionCount`, and requires it to equal the committed
 * `transactionsRoot`. The bad transaction is re-decoded from its canonical
 * compact CBOR and its canonical id recomputed, so the
 * `bad_tx_reference_inputs_hash` forwarded to step 02 is the transaction's own
 * committed value rather than a prepared field.
 *
 * Proof carriage uses the shared direct-or-published transaction inclusion
 * sum, so the 64-step maximum remains admissible without inflating a redeemer.
 */
import {
  commitCountedRootProgram,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  NoReferenceInputStep01SpendRedeemer,
  NoReferenceInputStep02Datum,
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

import { prepareNativeTxInclusionCarriage } from "./native-inclusion-carriage-v1.js";
import {
  type PublishedProofChunk,
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
  resolveNoReferenceInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  parseSubmitStep01TxInclusion,
  requireComputationThreadToken,
  requireInitialStepDatum,
  requireNativeTxMatchesCompactCbor,
  selectFeeInput,
  type SubmitStep01TxInclusion,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessSpendingValidatorCarriage,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary-v1.js";

// The no-reference-input proof commits the bad transaction by the node's native
// transaction root (the same inclusion path as double-spend and
// non-existent-input), so the tx-inclusion material is identical to
// `submit-step-01`'s and to what `prepare-no-reference-input` writes as
// `nri-tx-inclusion.json`.
export type NoReferenceInputStep01TxInclusion = SubmitStep01TxInclusion;
export const parseNoReferenceInputStep01TxInclusion =
  parseSubmitStep01TxInclusion;

export type SubmitNoReferenceInputStep01CliConfig = SubmitProviderConfig & {
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

export type SubmitNoReferenceInputStep01Result = {
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
  readonly blocksPrevUtxosRoot: string;
  readonly blocksTransactionsRoot: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type NoReferenceInputStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitNoReferenceInputStep01 = async ({
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
  readonly txInclusion: NoReferenceInputStep01TxInclusion;
  /** Present selects the authenticated published-chunk inclusion route. */
  readonly publishedProofChunks?: readonly PublishedProofChunk[];
  /** The mandatory published step-01 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitNoReferenceInputStep01Result> => {
  const resolvedDeployment = await resolveNoReferenceInputDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { noReferenceInputCategory, hubOraclePolicyId, contracts } =
    resolvedDeployment;
  const stateQueuePolicyId = resolvedDeployment.stateQueuePolicyId!;
  const steps = contracts.noReferenceInput.steps;

  const [threadUtxo, hubOracleUtxo, stateQueueBlockUtxo] = await Promise.all([
    fetchUtxoByOutRef({
      lucid,
      outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
      label: "no-reference-input step-01 computation-thread UTxO",
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
  if (threadUtxo.address !== steps[0].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at no-reference-input step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: noReferenceInputCategory.categoryId,
    categoryLabel: "no-reference-input",
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

  const stateQueueNodeView = await Effect.runPromise(
    getLinkedListNodeViewFromUTxO(stateQueueBlockUtxo),
  );
  const header = await Effect.runPromise(
    getHeaderFromStateQueueDatum(stateQueueNodeView),
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
  // §2.5's anchor, read off the compact structure the block's
  // `transactions_root` committed — the only provenance `BodyAnchor` accepts.
  const badTxId = txInclusion.nativeTxId;

  signer.selectWallet(lucid);
  const chunks = publishedProofChunks ?? [];
  const feeInput = selectFeeInput(
    walletInputsExcludingChunks({
      walletUtxos: await lucid.wallet().getUtxos(),
      chunks,
    }),
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: steps[0].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "no-reference-input step 01 validator",
  });
  const inclusionCarriage = prepareNativeTxInclusionCarriage({
    blueprint,
    network,
    txInclusion,
    publishedProofChunks: chunks,
    witnessReferenceScripts,
    label: "no-reference-input step 01",
    baseReferenceInputs: [
      hubOracleUtxo,
      stateQueueBlockUtxo,
      ...stepScriptCarriage.referenceInputs,
    ],
  });
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_tx_id: badTxId,
        blocks_prev_utxos_root: header.prevUtxosRoot,
        blocks_transactions_root: txInclusion.transactionsPhasRoot,
      },
    },
    NoReferenceInputStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: NoReferenceInputStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "no-reference-input step 01");
    const layout: NoReferenceInputStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "no-reference-input step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "no-reference-input step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "no-reference-input step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "no-reference-input step 01 state-queue node",
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
      NoReferenceInputStep01SpendRedeemer,
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
    .readFrom(inclusionCarriage.referenceInputs)
    .pay.ToContract(
      steps[1].spendingScriptAddress,
      { kind: "inline", value: step02Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = inclusionCarriage.attachWithdrawal(
    stepScriptCarriage.attach(base),
  );

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve no-reference-input step 01 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof no-reference-input step-01",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.noReferenceInput.steps[0].spendingScript,
        },
        ...inclusionCarriage.referenceScriptCandidates,
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `no-reference-input step-01 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    blocksPrevUtxosRoot: header.prevUtxosRoot,
    blocksTransactionsRoot: txInclusion.transactionsPhasRoot,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitNoReferenceInputStep01FromFiles = async (
  config: SubmitNoReferenceInputStep01CliConfig,
): Promise<SubmitNoReferenceInputStep01Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await submitNoReferenceInputStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseNoReferenceInputStep01TxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
