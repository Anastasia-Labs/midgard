import {
  computeMidgardNativeTxIdV1,
  decodeMidgardNativeTxCompactV1,
  encodeMidgardNativeTxCompactV1,
  formatUnknownError,
  type MidgardNativeTxCompactV1 as CoreNativeTxCompact,
  MidgardTxValidityCodes,
  normalizeHex,
} from "@al-ft/midgard-core";
import {
  DoubleSpendStep01SpendRedeemer,
  DoubleSpendStep02Datum,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  NativeTxCompact,
  type NativeTxCompact as NativeTxCompactData,
  Proof,
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
  parseHex,
  parseInteger,
  parseSignedInteger,
  requireRecord,
} from "./json-file.js";
import {
  compareUtxoOutRefs,
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
import { computationThreadOutputPredicate } from "./tx-layout.js";

export const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";
export const MIN_FEE_INPUT_LOVELACE = 10_000_000n;

export type SubmitStep01CliConfig = SubmitProviderConfig & {
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

export type SubmitStep01TxInclusion = {
  readonly nativeTxId: string;
  readonly nativeTx: NativeTxCompactData;
  readonly nativeTxCompactCbor: string;
  // Raw transactions MPF root the membership proof opens. Authenticated on-chain
  // against the block header's counted `transactions_root`.
  readonly transactionsPhasRoot: string;
  readonly txMembershipProof: Proof;
  readonly txMembershipProofCbor: string;
};

export type SubmitStep01Result = {
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
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

const bytesHex = (bytes: Uint8Array): string =>
  Buffer.from(bytes).toString("hex");

export const nativeTxFromCoreCompact = (
  tx: CoreNativeTxCompact,
): NativeTxCompactData => ({
  body: {
    spend_inputs_hash: bytesHex(tx.transactionBody.spendInputsHash),
    reference_inputs_hash: bytesHex(tx.transactionBody.referenceInputsHash),
    outputs_hash: bytesHex(tx.transactionBody.outputsHash),
    fee: tx.transactionBody.fee,
    validity_interval_start: tx.transactionBody.validityIntervalStart,
    validity_interval_end: tx.transactionBody.validityIntervalEnd,
    required_observers_hash: bytesHex(tx.transactionBody.requiredObserversHash),
    required_signers_hash: bytesHex(tx.transactionBody.requiredSignersHash),
    mint_hash: bytesHex(tx.transactionBody.mintHash),
    script_integrity_hash: bytesHex(tx.transactionBody.scriptIntegrityHash),
    auxiliary_data_hash: bytesHex(tx.transactionBody.auxiliaryDataHash),
    network_id: tx.transactionBody.networkId,
  },
  witness_set_hash: bytesHex(tx.transactionWitnessSetHash),
  validity_code: MidgardTxValidityCodes[tx.validity],
});

export const parseNativeTxCompact = (
  value: unknown,
  label: string,
): NativeTxCompactData => {
  const record = requireRecord(value, label);
  const bodyRecord = requireRecord(record.body, `${label}.body`);
  const tx: NativeTxCompactData = {
    body: {
      spend_inputs_hash: parseHex(
        bodyRecord.spend_inputs_hash,
        `${label}.body.spend_inputs_hash`,
        32,
      ),
      reference_inputs_hash: parseHex(
        bodyRecord.reference_inputs_hash,
        `${label}.body.reference_inputs_hash`,
        32,
      ),
      outputs_hash: parseHex(
        bodyRecord.outputs_hash,
        `${label}.body.outputs_hash`,
        32,
      ),
      fee: parseInteger(bodyRecord.fee, `${label}.body.fee`),
      validity_interval_start: parseSignedInteger(
        bodyRecord.validity_interval_start,
        `${label}.body.validity_interval_start`,
      ),
      validity_interval_end: parseSignedInteger(
        bodyRecord.validity_interval_end,
        `${label}.body.validity_interval_end`,
      ),
      required_observers_hash: parseHex(
        bodyRecord.required_observers_hash,
        `${label}.body.required_observers_hash`,
        32,
      ),
      required_signers_hash: parseHex(
        bodyRecord.required_signers_hash,
        `${label}.body.required_signers_hash`,
        32,
      ),
      mint_hash: parseHex(bodyRecord.mint_hash, `${label}.body.mint_hash`, 32),
      script_integrity_hash: parseHex(
        bodyRecord.script_integrity_hash,
        `${label}.body.script_integrity_hash`,
        32,
      ),
      auxiliary_data_hash: parseHex(
        bodyRecord.auxiliary_data_hash,
        `${label}.body.auxiliary_data_hash`,
        32,
      ),
      network_id: parseInteger(
        bodyRecord.network_id,
        `${label}.body.network_id`,
      ),
    },
    witness_set_hash: parseHex(
      record.witness_set_hash,
      `${label}.witness_set_hash`,
      32,
    ),
    validity_code: parseInteger(record.validity_code, `${label}.validity_code`),
  };
  return Data.from(Data.to(tx, NativeTxCompact), NativeTxCompact);
};

export const parseSubmitStep01TxInclusion = (
  value: unknown,
): SubmitStep01TxInclusion => {
  const record = requireRecord(value, "--tx-inclusion");
  const nativeTxId = parseHex(
    record.nativeTxId,
    "--tx-inclusion.nativeTxId",
    32,
  );
  const nativeTx = parseNativeTxCompact(
    record.nativeTx,
    "--tx-inclusion.nativeTx",
  );
  const nativeTxCompactCbor = parseHex(
    record.nativeTxCompactCbor,
    "--tx-inclusion.nativeTxCompactCbor",
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
    nativeTxId,
    nativeTx,
    nativeTxCompactCbor,
    transactionsPhasRoot,
    txMembershipProof: Data.from(txMembershipProofCbor, Proof),
    txMembershipProofCbor,
  };
};

export const requireNativeTxMatchesCompactCbor = (
  inclusion: SubmitStep01TxInclusion,
): CoreNativeTxCompact => {
  let decoded: CoreNativeTxCompact;
  try {
    decoded = decodeMidgardNativeTxCompactV1(
      Buffer.from(inclusion.nativeTxCompactCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `--tx-inclusion.nativeTxCompactCbor is not a valid native compact transaction: ${formatUnknownError(cause)}`,
    );
  }
  const canonicalCbor = encodeMidgardNativeTxCompactV1(decoded).toString("hex");
  if (canonicalCbor !== inclusion.nativeTxCompactCbor) {
    throw new Error(
      "--tx-inclusion.nativeTxCompactCbor is not canonical native compact CBOR.",
    );
  }
  const decodedNativeTx = nativeTxFromCoreCompact(decoded);
  if (
    Data.to(decodedNativeTx, NativeTxCompact) !==
    Data.to(inclusion.nativeTx, NativeTxCompact)
  ) {
    throw new Error(
      "--tx-inclusion.nativeTx does not match nativeTxCompactCbor.",
    );
  }
  const computedTxId = computeMidgardNativeTxIdV1(decoded).toString("hex");
  if (computedTxId !== inclusion.nativeTxId) {
    throw new Error(
      `--tx-inclusion.nativeTxId mismatch: provided=${inclusion.nativeTxId}, computed=${computedTxId}.`,
    );
  }
  return decoded;
};

export const singlePositiveNonAdaAsset = (
  utxo: UTxO,
  label: string,
): readonly [string, bigint] => {
  const nonAdaAssets = Object.entries(utxo.assets).filter(
    ([unit, amount]) => unit !== "lovelace" && amount > 0n,
  );
  if (nonAdaAssets.length !== 1) {
    throw new Error(
      `Expected ${label} ${outRefLabel(utxo)} to carry exactly one non-ADA asset, found ${nonAdaAssets.length.toString()}.`,
    );
  }
  return nonAdaAssets[0]!;
};

export const requireComputationThreadToken = ({
  utxo,
  computationThreadPolicyId,
  categoryId,
  categoryLabel,
}: {
  readonly utxo: UTxO;
  readonly computationThreadPolicyId: string;
  readonly categoryId: string;
  readonly categoryLabel: string;
}): {
  readonly unit: string;
  readonly assetName: string;
  readonly fraudulentHeaderHash: string;
} => {
  const [unit, amount] = singlePositiveNonAdaAsset(utxo, "thread UTxO");
  if (amount !== 1n) {
    throw new Error(
      `Expected computation-thread token amount 1 at ${outRefLabel(utxo)}, found ${amount.toString()}.`,
    );
  }
  const expectedPrefix = `${computationThreadPolicyId}${categoryId}`;
  if (!unit.startsWith(expectedPrefix)) {
    throw new Error(
      `Thread UTxO ${outRefLabel(utxo)} does not carry a ${categoryLabel} computation-thread token for policy ${computationThreadPolicyId}.`,
    );
  }
  const assetName = unit.slice(computationThreadPolicyId.length);
  const fraudulentHeaderHash = normalizeHex(unit.slice(expectedPrefix.length), {
    fieldName: `Computation-thread asset name ${assetName} suffix`,
    byteLength: 28,
    trim: false,
  });
  return { unit, assetName, fraudulentHeaderHash };
};

export const requireInitialStepDatum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): void => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(
    threadUtxo.datum,
    FraudProofComputationThreadStepDatum,
  );
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data !== null) {
    throw new Error("Step 01 input datum must have null state data.");
  }
};

export const selectFeeInput = (walletUtxos: readonly UTxO[]): UTxO => {
  const candidates = walletUtxos
    .filter((utxo) => {
      const nonAdaAssets = Object.entries(utxo.assets).filter(
        ([unit, amount]) => unit !== "lovelace" && amount > 0n,
      );
      return (
        nonAdaAssets.length === 0 &&
        (utxo.assets.lovelace ?? 0n) >= MIN_FEE_INPUT_LOVELACE
      );
    })
    .sort((left, right) => {
      const leftLovelace = left.assets.lovelace ?? 0n;
      const rightLovelace = right.assets.lovelace ?? 0n;
      if (rightLovelace > leftLovelace) {
        return 1;
      }
      if (rightLovelace < leftLovelace) {
        return -1;
      }
      return compareUtxoOutRefs(left, right);
    });
  const feeInput = candidates[0];
  if (feeInput === undefined) {
    throw new Error(
      `Prover wallet must contain a pure-ADA UTxO with at least ${MIN_FEE_INPUT_LOVELACE.toString()} lovelace for fees.`,
    );
  }
  return feeInput;
};

export const submitStep01 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
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
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep01Result> => {
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
      label: "step-01 computation-thread UTxO",
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
    threadUtxo.address !== contracts.doubleSpend.firstStep.spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: doubleSpendCategory.categoryId,
    categoryLabel: "double-spend",
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
  const step02Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        verified_tx1_id: txInclusion.nativeTxId,
        verified_tx1_spend_inputs_hash:
          txInclusion.nativeTx.body.spend_inputs_hash,
      },
    },
    DoubleSpendStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.doubleSpend.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: Step01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "double-spend step 01");
    const layout: Step01Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "double-spend step 01"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "double-spend step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "double-spend step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "double-spend step 01 state-queue node",
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
            native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
            transactions_phas_root: txInclusion.transactionsPhasRoot,
            tx_membership_proof: txInclusion.txMembershipProof,
            inclusion_proof_script_withdraw_redeemer_index:
              requireWithdrawalRedeemerIndex(
                ctx,
                phasRewardAddress,
                "double-spend step 01 PHAS membership",
              ),
          },
        ],
      },
      DoubleSpendStep01SpendRedeemer,
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
        keyBytes: txInclusion.nativeTxId,
        valueBytes: txInclusion.nativeTxCompactCbor,
        membershipProofCbor: txInclusion.txMembershipProofCbor,
      }),
    )
    .pay.ToContract(
      contracts.doubleSpend.steps[1].spendingScriptAddress,
      {
        kind: "inline",
        value: step02Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.doubleSpend.steps[0].spendingScript)
    .attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error("BuildTxWithRedeemer did not resolve step 01 layout.");
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
    firstStepAddress: contracts.doubleSpend.steps[0].spendingScriptAddress,
    secondStepAddress: contracts.doubleSpend.steps[1].spendingScriptAddress,
    nativeTxId: txInclusion.nativeTxId,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep01FromFiles = async (
  config: SubmitStep01CliConfig,
): Promise<SubmitStep01Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await submitStep01({
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
