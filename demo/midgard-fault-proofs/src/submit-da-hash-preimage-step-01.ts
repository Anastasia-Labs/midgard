/**
 * `da-hash-preimage` step-01 submitter (Goal task `Q44`, §9.1 output 8).
 *
 * Unlike the retired RF-043 step routes, nothing in the prepared JSON is
 * trusted. Before a transaction is built this module re-derives, from the
 * **on-chain** state-queue block header:
 *
 * - the counted `transactions_root` over the supplied raw PHAS root and the
 *   header's own `l2TransactionCount`, which must equal the committed
 *   `transactionsRoot`; and
 * - the violation itself, by re-running the Q44 rule over the committed leaf
 *   bytes.
 *
 * A prepared file that claims a violation the chain does not support is
 * therefore rejected locally, before any submission.
 */
import {
  commitCountedRootProgram,
  daHashPreimageEvidenceFromCommittedLeafV1,
  DaHashPreimageStep01SpendRedeemer,
  DaHashPreimageStep02Datum,
  getHeaderV1FromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  HUB_ORACLE_ASSET_NAME,
  Proof,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
  requireWithdrawalRedeemerIndex,
  ROOT_DOMAINS,
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

import { parseHex, requireRecord } from "./json-file.js";
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
  resolveDaHashPreimageDeploymentContracts,
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  PHAS_MEMBERSHIP_WITHDRAW_TITLE,
  requireComputationThreadToken,
  requireInitialStepDatum,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessSpendingValidatorCarriageV1,
  witnessWithdrawalValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";

/** Prepared committed-leaf inclusion produced by `prepare-da-hash-preimage`. */
export type SubmitDaHashPreimageTxInclusion = {
  readonly committedTxId: string;
  readonly committedLeafValueCbor: string;
  readonly transactionsPhasRoot: string;
  readonly txMembershipProof: Proof;
  readonly txMembershipProofCbor: string;
};

export const parseSubmitDaHashPreimageTxInclusion = (
  value: unknown,
): SubmitDaHashPreimageTxInclusion => {
  const record = requireRecord(value, "--tx-inclusion");
  const committedTxId = parseHex(
    record.committedTxId,
    "--tx-inclusion.committedTxId",
    32,
  );
  const committedLeafValueCbor = parseHex(
    record.committedLeafValueCbor,
    "--tx-inclusion.committedLeafValueCbor",
  );
  const transactionsPhasRoot = parseHex(
    record.transactionsPhasRoot,
    "--tx-inclusion.transactionsPhasRoot",
    32,
  );
  const txMembershipProofCbor = parseHex(
    record.txMembershipProofCbor,
    "--tx-inclusion.txMembershipProofCbor",
  );
  return {
    committedTxId,
    committedLeafValueCbor,
    transactionsPhasRoot,
    txMembershipProof: Data.from(txMembershipProofCbor, Proof),
    txMembershipProofCbor,
  };
};

export type SubmitDaHashPreimageStep01CliConfig = SubmitProviderConfig & {
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

export type SubmitDaHashPreimageStep01Result = {
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
  readonly committedTxId: string;
  readonly derivedTxId: string;
  readonly committedLeafByteCount: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type DaHashPreimageStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitDaHashPreimageStep01 = async ({
  lucid,
  blueprint,
  deploymentInfo,
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
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusion: SubmitDaHashPreimageTxInclusion;
  /** The mandatory published step-01 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitDaHashPreimageStep01Result> => {
  const resolvedDeployment = await resolveDaHashPreimageDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { daHashPreimageCategory, hubOraclePolicyId, contracts } =
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
      label: "da-hash-preimage step-01 computation-thread UTxO",
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
    threadUtxo.address !==
    contracts.daHashPreimage.steps[0].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at da-hash-preimage step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: daHashPreimageCategory.categoryId,
    categoryLabel: "da-hash-preimage",
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
    getHeaderV1FromStateQueueDatum(stateQueueNodeView),
  );

  // Re-derive the counted commitment the L1 verifier will check, from the
  // on-chain header rather than from the prepared file.
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

  // Re-run the rule over the committed leaf bytes.
  const evidence = daHashPreimageEvidenceFromCommittedLeafV1({
    committedTxId: txInclusion.committedTxId,
    committedLeafValue: Buffer.from(txInclusion.committedLeafValueCbor, "hex"),
  });
  if (!evidence.isViolation) {
    throw new Error(
      `--tx-inclusion committed leaf ${evidence.committedTxId} derives its own key; a valid block cannot be challenged.`,
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const phasRewardAddress = phasMembershipRewardAddress(
    network,
    phasMembershipScript,
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.daHashPreimage.steps[0].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "da-hash-preimage step 01 validator",
  });
  const phasMembershipCarriage = witnessWithdrawalValidatorCarriageV1({
    script: phasMembershipScript,
    referenceUtxo: witnessReferenceScripts?.phasMembershipWithdraw,
    label: "da-hash-preimage step 01 PHAS membership",
  });
  const referenceInputs = [
    hubOracleUtxo,
    stateQueueBlockUtxo,
    ...stepScriptCarriage.referenceInputs,
    ...phasMembershipCarriage.referenceInputs,
  ];
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        committed_tx_id: evidence.committedTxId,
        derived_tx_id: evidence.derivedTxId,
        committed_leaf_byte_count: BigInt(evidence.committedLeafByteCount),
      },
    },
    DaHashPreimageStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.daHashPreimage.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: DaHashPreimageStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "da-hash-preimage step 01");
    const layout: DaHashPreimageStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "da-hash-preimage step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "da-hash-preimage step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "da-hash-preimage step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "da-hash-preimage step 01 state-queue node",
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
            native_tx_id: txInclusion.committedTxId,
            native_tx_compact_cbor: txInclusion.committedLeafValueCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasRewardAddress,
                "da-hash-preimage step 01 PHAS membership",
              ),
          },
        ],
      },
      DaHashPreimageStep01SpendRedeemer,
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
    .readFrom(referenceInputs)
    .withdraw(
      phasRewardAddress,
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: txInclusion.transactionsPhasRoot,
        keyBytes: txInclusion.committedTxId,
        valueBytes: txInclusion.committedLeafValueCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.daHashPreimage.steps[1].spendingScriptAddress,
      {
        kind: "inline",
        value: step02Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = phasMembershipCarriage.attach(
    stepScriptCarriage.attach(tx),
  );

  const unsigned = await completedTx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve da-hash-preimage step 01 layout.",
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
    firstStepAddress: contracts.daHashPreimage.steps[0].spendingScriptAddress,
    secondStepAddress: contracts.daHashPreimage.steps[1].spendingScriptAddress,
    committedTxId: evidence.committedTxId,
    derivedTxId: evidence.derivedTxId,
    committedLeafByteCount: evidence.committedLeafByteCount,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitDaHashPreimageStep01FromFiles = async (
  config: SubmitDaHashPreimageStep01CliConfig,
): Promise<SubmitDaHashPreimageStep01Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await submitDaHashPreimageStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseSubmitDaHashPreimageTxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
