import {
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
  credentialToAddress,
  scriptHashToCredential,
  toUnit,
} from "@lucid-evolution/lucid";
import {
  DoubleSpendStep01SpendRedeemer,
  DoubleSpendStep02Datum,
  FraudProofComputationThreadStepDatum,
  HUB_ORACLE_ASSET_NAME,
  NativeTxCompact,
  Proof,
  STATE_QUEUE_NODE_ASSET_NAME_PREFIX,
  getHeaderFromStateQueueDatum,
  getLinkedListNodeViewFromUTxO,
  type NativeTxCompact as NativeTxCompactData,
} from "@al-ft/midgard-sdk";
import {
  MidgardTxValidityCodes,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxCompact,
  encodeMidgardNativeTxCompact,
  type MidgardNativeTxCompact as CoreNativeTxCompact,
} from "@al-ft/midgard-core";
import { Effect } from "effect";
import {
  parseHex,
  parseInteger,
  parseSignedInteger,
  requireRecord,
} from "./json-file.js";
import {
  compareOutRefs,
  DEFAULT_CONFIRMATION_POLL_MS,
  encodeRawPhasMembershipProofRedeemer,
  fetchUtxoByOutRef,
  getCompiledScript,
  makeLucidForSubmit,
  outRefLabel,
  parsedOutRefFromUtxo,
  parseOutRef,
  phasMembershipRewardAddress,
  readJsonFile,
  referenceInputIndex,
  requireSingletonUtxo,
  resolveDoubleSpendDeploymentContracts,
  resolveFraudulentHeaderHash,
  resolveProverSigner,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";

export const PHAS_MEMBERSHIP_WITHDRAW_TITLE = "phas.membership.withdraw";
const STEP_01_OUTPUT_INDEX = 0n;
export const PHAS_WITHDRAW_REDEEMER_INDEX = 0n;
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
  const txMembershipProofCbor = parseHex(
    record.txMembershipProofCbor,
    "--tx-inclusion.txMembershipProofCbor",
  );
  return {
    nativeTxId,
    nativeTx,
    nativeTxCompactCbor,
    txMembershipProof: Data.from(txMembershipProofCbor, Proof),
    txMembershipProofCbor,
  };
};

export const requireNativeTxMatchesCompactCbor = (
  inclusion: SubmitStep01TxInclusion,
): CoreNativeTxCompact => {
  let decoded: CoreNativeTxCompact;
  try {
    decoded = decodeMidgardNativeTxCompact(
      Buffer.from(inclusion.nativeTxCompactCbor, "hex"),
    );
  } catch (cause) {
    throw new Error(
      `--tx-inclusion.nativeTxCompactCbor is not a valid native compact transaction: ${String(cause)}`,
    );
  }
  const canonicalCbor = encodeMidgardNativeTxCompact(decoded).toString("hex");
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
  const computedTxId = computeMidgardNativeTxId(decoded).toString("hex");
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
  doubleSpendCategoryId,
}: {
  readonly utxo: UTxO;
  readonly computationThreadPolicyId: string;
  readonly doubleSpendCategoryId: string;
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
  const expectedPrefix = `${computationThreadPolicyId}${doubleSpendCategoryId}`;
  if (!unit.startsWith(expectedPrefix)) {
    throw new Error(
      `Thread UTxO ${outRefLabel(utxo)} does not carry a double-spend computation-thread token for policy ${computationThreadPolicyId}.`,
    );
  }
  const assetName = unit.slice(computationThreadPolicyId.length);
  const fraudulentHeaderHash = unit.slice(expectedPrefix.length);
  if (!/^[0-9a-f]{56}$/.test(fraudulentHeaderHash)) {
    throw new Error(
      `Computation-thread asset name ${assetName} does not end with a 28-byte fraudulent header hash.`,
    );
  }
  return { unit, assetName, fraudulentHeaderHash };
};

const requireInitialStepDatum = ({
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
      return compareOutRefs(
        parsedOutRefFromUtxo(left),
        parsedOutRefFromUtxo(right),
      );
    });
  const feeInput = candidates[0];
  if (feeInput === undefined) {
    throw new Error(
      `Prover wallet must contain a pure-ADA UTxO with at least ${MIN_FEE_INPUT_LOVELACE.toString()} lovelace for fees.`,
    );
  }
  return feeInput;
};

export const inputIndex = (threadUtxo: UTxO, feeInput: UTxO): bigint => {
  const ordered = [threadUtxo, feeInput].sort((left, right) =>
    compareOutRefs(parsedOutRefFromUtxo(left), parsedOutRefFromUtxo(right)),
  );
  const index = ordered.findIndex(
    (utxo) =>
      utxo.txHash === threadUtxo.txHash &&
      utxo.outputIndex === threadUtxo.outputIndex,
  );
  if (index < 0) {
    throw new Error(`Thread input not found after input ordering.`);
  }
  return BigInt(index);
};

export const requireMatchingScriptHash = ({
  label,
  deployed,
  derived,
}: {
  readonly label: string;
  readonly deployed: string;
  readonly derived: string;
}): void => {
  if (deployed !== derived) {
    throw new Error(
      `${label} mismatch: deployment=${deployed}, derived=${derived}.`,
    );
  }
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
    doubleSpendCategoryId: doubleSpendCategory.categoryId,
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
  requireNativeTxMatchesCompactCbor(txInclusion);

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const sortedReferenceInputs = [hubOracleUtxo, stateQueueBlockUtxo].sort(
    (left, right) =>
      compareOutRefs(parsedOutRefFromUtxo(left), parsedOutRefFromUtxo(right)),
  );
  const spendInputIndex = inputIndex(threadUtxo, feeInput);
  const phasMembershipScript: Script = {
    type: "PlutusV3",
    script: getCompiledScript(blueprint, PHAS_MEMBERSHIP_WITHDRAW_TITLE),
  };
  const continueArgs = {
    input_index: spendInputIndex,
    output_index: STEP_01_OUTPUT_INDEX,
    hub_ref_input_index: referenceInputIndex(
      sortedReferenceInputs,
      hubOracleUtxo,
    ),
    state_queue_node_ref_input_index: referenceInputIndex(
      sortedReferenceInputs,
      stateQueueBlockUtxo,
    ),
    native_tx_id: txInclusion.nativeTxId,
    native_tx_compact_cbor: txInclusion.nativeTxCompactCbor,
    tx_membership_proof: txInclusion.txMembershipProof,
    inclusion_proof_script_withdraw_redeemer_index:
      PHAS_WITHDRAW_REDEEMER_INDEX,
  };
  const redeemer = Data.to(
    {
      Continue: [continueArgs],
    },
    DoubleSpendStep01SpendRedeemer,
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
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const tx = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer)
    .readFrom(sortedReferenceInputs)
    .withdraw(
      phasMembershipRewardAddress(network, phasMembershipScript),
      0n,
      encodeRawPhasMembershipProofRedeemer({
        root: header.transactionsRoot,
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
    nextThreadOutRef: `${txHash}#${STEP_01_OUTPUT_INDEX.toString()}`,
    stateQueueBlockOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    firstStepAddress: contracts.doubleSpend.steps[0].spendingScriptAddress,
    secondStepAddress: contracts.doubleSpend.steps[1].spendingScriptAddress,
    nativeTxId: txInclusion.nativeTxId,
    inputIndex: Number(spendInputIndex),
    outputIndex: Number(STEP_01_OUTPUT_INDEX),
    hubOracleRefInputIndex: Number(
      referenceInputIndex(sortedReferenceInputs, hubOracleUtxo),
    ),
    stateQueueNodeRefInputIndex: Number(
      referenceInputIndex(sortedReferenceInputs, stateQueueBlockUtxo),
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
