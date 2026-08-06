/**
 * `reference-input-no-idx` step-02 submitter (Goal task `Q31`).
 *
 * Opens the reference-inputs commitment carried by step 01. Nothing in the
 * prepared file is trusted: the complete preimage is re-encoded with the
 * canonical `encode_midgard_tx_input` twin and re-committed with
 * `bounded_collection_v1.from_items(1, ...)`, and the result must equal the
 * `verified_tx_reference_inputs_hash` read back from the **on-chain** step-01
 * datum. Reference inputs and spend inputs share the per-item encoder but not
 * the consensus field index, so a spend-inputs preimage can never open this
 * commitment. Only then is the challenged reference input forwarded to step 03.
 *
 * Unlike the `input-no-idx` spend-side mirror, this family's on-chain step 02
 * takes a single flat `Args` record: there is no `Complete`/`CompletePublished`/
 * `FoldStart`/`FoldNext` sum, hence no publication reference and no ordered
 * fold to drive from here. The complete list always travels in the redeemer.
 */
import {
  type MidgardTxInput,
  referenceInputNoIdxReferenceInputsCommitmentV1,
  ReferenceInputNoIdxStep02Datum,
  ReferenceInputNoIdxStep02SpendRedeemer,
  ReferenceInputNoIdxStep03Datum,
  referenceInputNoIdxStep03StateFromBadInputV1,
  requireInputIndex,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  parseHex,
  parseSafeNonNegativeInteger,
  requireRecord,
} from "./json-file.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";

/** Prepared reference-inputs preimage produced by `prepare-reference-input-no-idx`. */
export type SubmitReferenceInputNoIdxReferenceInputsPreimage = {
  readonly referenceInputsPreimage: readonly MidgardTxInput[];
  readonly badReferenceInputIndex: number;
};

/**
 * `prepare-reference-input-no-idx` writes its `referenceInputsPreimage`
 * artifact as a bare JSON array of `{ txId, index }` entries and keeps the
 * challenged position in the sibling plan file, so both the artifact shape and
 * the canonical `{ tx_id, output_index }` shape are accepted here. The
 * challenged position is a caller selection, not evidence: it is only bounds
 * checked, and the violation itself is re-run against the producing
 * transaction's committed outputs in step 04.
 */
export const parseSubmitReferenceInputNoIdxReferenceInputsPreimage = ({
  value,
  badReferenceInputIndex,
}: {
  readonly value: unknown;
  readonly badReferenceInputIndex?: string | number;
}): SubmitReferenceInputNoIdxReferenceInputsPreimage => {
  const record = Array.isArray(value)
    ? undefined
    : requireRecord(value, "--reference-inputs-preimage");
  const rawEntries =
    record === undefined ? value : record.referenceInputsPreimage;
  if (!Array.isArray(rawEntries)) {
    throw new Error(
      "--reference-inputs-preimage must be a JSON array, or a JSON object with a referenceInputsPreimage array.",
    );
  }
  const referenceInputsPreimage = rawEntries.map((item, index) => {
    const label = `--reference-inputs-preimage[${index.toString()}]`;
    const entry = requireRecord(item, label);
    return {
      tx_id: parseHex(entry.tx_id ?? entry.txId, `${label}.tx_id`, 32),
      output_index: parseSafeNonNegativeInteger(
        entry.output_index ?? entry.index,
        `${label}.output_index`,
      ),
    };
  });
  const rawBadReferenceInputIndex =
    badReferenceInputIndex ?? record?.badReferenceInputIndex;
  if (rawBadReferenceInputIndex === undefined) {
    throw new Error(
      "--bad-reference-input-index is required: the reference-inputs preimage artifact does not carry the challenged position.",
    );
  }
  return {
    referenceInputsPreimage,
    badReferenceInputIndex: Number(
      parseSafeNonNegativeInteger(
        rawBadReferenceInputIndex,
        "--bad-reference-input-index",
      ),
    ),
  };
};

export type SubmitReferenceInputNoIdxStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly referenceInputsPreimagePath: string;
  readonly badReferenceInputIndex?: string | number;
  readonly awaitConfirmation?: boolean;
};

export type SubmitReferenceInputNoIdxStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly verifiedTxReferenceInputsHash: string;
  readonly referenceInputsPreimageItemCount: number;
  readonly badReferenceInputIndex: number;
  readonly badReferenceInputTxId: string;
  readonly badReferenceInputOutputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type ReferenceInputNoIdxStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type ReferenceInputNoIdxStep02DatumWithState =
  ReferenceInputNoIdxStep02Datum & {
    readonly data: NonNullable<ReferenceInputNoIdxStep02Datum["data"]>;
  };

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): ReferenceInputNoIdxStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, ReferenceInputNoIdxStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Reference-input-no-idx step 02 input datum must carry the verified reference-inputs hash.",
    );
  }
  return datum as ReferenceInputNoIdxStep02DatumWithState;
};

export const submitReferenceInputNoIdxStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  referenceInputsPreimage,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceInputsPreimage: SubmitReferenceInputNoIdxReferenceInputsPreimage;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitReferenceInputNoIdxStep02Result> => {
  const { referenceInputNoIdxCategory, contracts } =
    await resolveReferenceInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const chain = contracts.referenceInputNoIdx;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "reference-input-no-idx step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at reference-input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: referenceInputNoIdxCategory.categoryId,
    categoryLabel: "reference-input-no-idx",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const verifiedTxReferenceInputsHash =
    inputDatum.data.verified_tx_reference_inputs_hash;

  // Re-derive the commitment the validator will recompute, from the preimage
  // itself, and require it to open the on-chain state.
  const derivedCommitment = referenceInputNoIdxReferenceInputsCommitmentV1(
    referenceInputsPreimage.referenceInputsPreimage,
  );
  if (derivedCommitment !== verifiedTxReferenceInputsHash) {
    throw new Error(
      `--reference-inputs-preimage does not open the committed reference-inputs hash: derived=${derivedCommitment}, thread=${verifiedTxReferenceInputsHash}.`,
    );
  }
  const badReferenceInput =
    referenceInputsPreimage.referenceInputsPreimage[
      referenceInputsPreimage.badReferenceInputIndex
    ];
  if (badReferenceInput === undefined) {
    throw new Error(
      `--bad-reference-input-index ${referenceInputsPreimage.badReferenceInputIndex.toString()} is out of range for a ${referenceInputsPreimage.referenceInputsPreimage.length.toString()}-item preimage.`,
    );
  }

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: referenceInputNoIdxStep03StateFromBadInputV1(badReferenceInput),
    },
    ReferenceInputNoIdxStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: chain.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: ReferenceInputNoIdxStep02Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "reference-input-no-idx step 02");
    const layout: ReferenceInputNoIdxStep02Layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "reference-input-no-idx step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "reference-input-no-idx step 02 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            reference_inputs_preimage: [
              ...referenceInputsPreimage.referenceInputsPreimage,
            ],
            bad_reference_input_index: BigInt(
              referenceInputsPreimage.badReferenceInputIndex,
            ),
          },
        ],
      },
      ReferenceInputNoIdxStep02SpendRedeemer,
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
    .pay.ToContract(
      chain.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(chain.steps[1].spendingScript);

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve reference-input-no-idx step 02 layout.",
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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: chain.steps[1].spendingScriptAddress,
    thirdStepAddress: chain.steps[2].spendingScriptAddress,
    verifiedTxReferenceInputsHash,
    referenceInputsPreimageItemCount:
      referenceInputsPreimage.referenceInputsPreimage.length,
    badReferenceInputIndex: referenceInputsPreimage.badReferenceInputIndex,
    badReferenceInputTxId: badReferenceInput.tx_id,
    badReferenceInputOutputIndex: Number(badReferenceInput.output_index),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitReferenceInputNoIdxStep02FromFiles = async (
  config: SubmitReferenceInputNoIdxStep02CliConfig,
): Promise<SubmitReferenceInputNoIdxStep02Result> => {
  const [blueprint, deploymentInfo, referenceInputsPreimageJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.referenceInputsPreimagePath),
      makeLucidForSubmit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await submitReferenceInputNoIdxStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    referenceInputsPreimage:
      parseSubmitReferenceInputNoIdxReferenceInputsPreimage({
        value: referenceInputsPreimageJson,
        ...(config.badReferenceInputIndex === undefined
          ? {}
          : { badReferenceInputIndex: config.badReferenceInputIndex }),
      }),
    awaitConfirmation: config.awaitConfirmation,
  });
};
