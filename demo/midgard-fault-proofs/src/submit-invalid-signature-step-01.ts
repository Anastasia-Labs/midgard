/**
 * ⚠️ **STALE AS OF #575 — do not build a datum or redeemer from this module
 * and expect chain to accept it. Owner: #579.** The rebind, its three concrete
 * divergences, and why they are not re-derived in this lane are explained once
 * in `docs/fault-proofs/offchain-builder-staleness-575.md`.
 *
 * `invalid-signature` step-01 submitter (Goal task `Q15`, §9.1 output 8).
 *
 * Nothing in the prepared JSON is trusted. Before a transaction is built this
 * module re-derives, from evidence the chain itself authenticates:
 *
 * - the bad transaction's canonical compact CBOR, its native id and its typed
 *   view, which the on-chain step re-opens against the block header's counted
 *   `transactions_root`; and
 * - the committed `witness_set_hash`, by re-encoding the supplied witness-set
 *   compact exactly as the on-chain §8.8 field-access door does. The standalone
 *   `verify_native_tx_witness_set` helper this mirrored was deleted by #575;
 *   `authenticated_field_view`
 *   (`onchain/aiken/lib/midgard/native-tx-field-access-v1.ak`) now makes the
 *   same `blake2b_256(encode_native_tx_witness_set_compact(...))` check inline
 *   and will not read any of fields 6–8 until it passes.
 *
 * A prepared file whose witness-set compact is not the preimage of the
 * committed hash is therefore rejected locally, before any submission: a valid
 * block cannot be challenged with invented witnesses.
 *
 * Unlike `zero-input` and `invalid-range`, the family's on-chain `Args` is
 * `NativeTxInclusionArgs` rather than `NativeTxInclusionCarriage`
 * (`lib/midgard/fraud-proofs/invalid-signature/step-01.ak`), so the membership
 * opening has exactly one route to L1 — the redeemer-carried one. The
 * published-chunk carriage of issue #545 is not expressible here and is
 * deliberately absent rather than emulated.
 */
import {
  HUB_ORACLE_ASSET_NAME,
  InvalidSignatureStep01SpendRedeemer,
  InvalidSignatureStep02Datum,
  invalidSignatureStep02StateFromBadTxV1,
  invalidSignatureWitnessSetCommitmentV1,
  type NativeTxWitnessSetCompact,
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
} from "@lucid-evolution/lucid";

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
  type ResolvedProverSigner,
  resolveFraudulentHeaderHash,
  resolveInvalidSignatureDeploymentContracts,
  resolveProverSigner,
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

/**
 * Preimage of the bad transaction's committed `witness_set_hash`, as
 * `prepare-invalid-signature` writes it to
 * `invalid-signature-witness-set-compact.json`.
 */
export const parseSubmitInvalidSignatureWitnessSetCompact = (
  value: unknown,
): NativeTxWitnessSetCompact => {
  const record = requireRecord(value, "--witness-set-compact");
  return {
    addr_tx_wits_hash: parseHex(
      record.addrTxWitsHash,
      "--witness-set-compact.addrTxWitsHash",
      32,
    ),
    script_tx_wits_hash: parseHex(
      record.scriptTxWitsHash,
      "--witness-set-compact.scriptTxWitsHash",
      32,
    ),
    redeemer_tx_wits_hash: parseHex(
      record.redeemerTxWitsHash,
      "--witness-set-compact.redeemerTxWitsHash",
      32,
    ),
  };
};

export type SubmitInvalidSignatureStep01CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly stateQueueBlockOutRef: string;
  readonly txInclusionPath: string;
  readonly witnessSetCompactPath: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInvalidSignatureStep01Result = {
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
  readonly badTxWitnessSetHash: string;
  readonly badAddrTxWitsHash: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly hubOracleRefInputIndex: number;
  readonly stateQueueNodeRefInputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InvalidSignatureStep01Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly stateQueueNodeRefInputIndex: bigint;
};

export const submitInvalidSignatureStep01 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  stateQueueBlockOutRef,
  txInclusion,
  badTxWitnessSetCompact,
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
  /** Preimage of the committed `witness_set_hash`, opened by this step. */
  readonly badTxWitnessSetCompact: NativeTxWitnessSetCompact;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInvalidSignatureStep01Result> => {
  const resolvedDeployment = await resolveInvalidSignatureDeploymentContracts({
    blueprint,
    deploymentInfo,
    network,
    requireStateQueueMint: true,
  });
  const { invalidSignatureCategory, hubOraclePolicyId, contracts } =
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
      label: "invalid-signature step-01 computation-thread UTxO",
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
    contracts.invalidSignature.steps[0].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at invalid-signature step 01.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: invalidSignatureCategory.categoryId,
    categoryLabel: "invalid-signature",
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
  // Twin of the on-chain §8.8 field-access door: the supplied compact must
  // re-encode to the `witness_set_hash` the committed transaction pins. The
  // standalone `verify_native_tx_witness_set` helper is gone as of #575;
  // `authenticated_field_view` makes the identical check before it will read
  // any of fields 6–8, and for a downstream step the hash it checks against is
  // the anchored one thread state carries in `WitnessAnchor`.
  // The signature itself cannot be tested yet — the transaction commits its
  // address witnesses only as a hash, whose preimage step 02 supplies.
  const badTxWitnessSetHash = txInclusion.nativeTx.witness_set_hash;
  const derivedWitnessSetHash = invalidSignatureWitnessSetCommitmentV1(
    badTxWitnessSetCompact,
  );
  if (derivedWitnessSetHash !== badTxWitnessSetHash) {
    throw new Error(
      `--witness-set-compact does not open the committed witness_set_hash: derived=${derivedWitnessSetHash}, committed=${badTxWitnessSetHash}.`,
    );
  }
  const badAddrTxWitsHash = badTxWitnessSetCompact.addr_tx_wits_hash;

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
      data: invalidSignatureStep02StateFromBadTxV1({
        badTxId: txInclusion.nativeTxId,
        badAddrTxWitsHash,
      }),
    },
    InvalidSignatureStep02Datum,
  );
  const step02OutputMatches = computationThreadOutputPredicate({
    address: contracts.invalidSignature.steps[1].spendingScriptAddress,
    datum: step02Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: InvalidSignatureStep01Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "invalid-signature step 01");
    const layout: InvalidSignatureStep01Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "invalid-signature step 01",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step02OutputMatches,
        "invalid-signature step 01 output",
      ),
      hubOracleRefInputIndex: requireReferenceInputIndex(
        ctx,
        hubOracleUtxo,
        "invalid-signature step 01 hub oracle",
      ),
      stateQueueNodeRefInputIndex: requireReferenceInputIndex(
        ctx,
        stateQueueBlockUtxo,
        "invalid-signature step 01 state-queue node",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            tx_inclusion_args: {
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
                  "invalid-signature step 01 PHAS membership",
                ),
            },
            bad_tx_witness_set_compact: badTxWitnessSetCompact,
          },
        ],
      },
      InvalidSignatureStep01SpendRedeemer,
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
      contracts.invalidSignature.steps[1].spendingScriptAddress,
      {
        kind: "inline",
        value: step02Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(
      contracts.invalidSignature.steps[0].spendingScript,
    )
    .attach.WithdrawalValidator(phasMembershipScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve invalid-signature step 01 layout.",
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
    firstStepAddress: contracts.invalidSignature.steps[0].spendingScriptAddress,
    secondStepAddress:
      contracts.invalidSignature.steps[1].spendingScriptAddress,
    nativeTxId: txInclusion.nativeTxId,
    badTxWitnessSetHash,
    badAddrTxWitsHash,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    hubOracleRefInputIndex: Number(resolvedLayout.hubOracleRefInputIndex),
    stateQueueNodeRefInputIndex: Number(
      resolvedLayout.stateQueueNodeRefInputIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitInvalidSignatureStep01FromFiles = async (
  config: SubmitInvalidSignatureStep01CliConfig,
): Promise<SubmitInvalidSignatureStep01Result> => {
  const [
    blueprint,
    deploymentInfo,
    txInclusionJson,
    witnessSetCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.txInclusionPath),
    readJsonFile(config.witnessSetCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitInvalidSignatureStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(txInclusionJson),
    badTxWitnessSetCompact: parseSubmitInvalidSignatureWitnessSetCompact(
      witnessSetCompactJson,
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
