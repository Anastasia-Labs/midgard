/**
 * `reference-input-no-idx` step-02 submitter (Goal task `Q31`).
 *
 * Opens §2.5 field **1** of the transaction carried by step 01 and forwards the
 * challenged reference input to step 03.
 *
 * **Re-derived onto the §8.8 door by #604** — and this family was *unbannered*
 * while stale, because #576 rebound it (`b824ad6ea`) one day after
 * `docs/fault-proofs/offchain-builder-staleness-575.md` listed it as an
 * exclusion. Thread state carries `verified_tx_id`, the §2.5 anchor; the
 * redeemer carries a `FieldOpeningV1` rather than a reproduced
 * `reference_inputs_preimage`.
 *
 * **Position, not encoding, is what separates field 1 from field 0.** The header
 * this replaces claimed a spend-inputs preimage "can never open this commitment"
 * because the items were committed under a different `from_items` index; §4
 * removed field-index domain separation, so identical items commit identically
 * in both slots and it is the index named at the door — mirrored here by
 * {@link planFaultProofFieldOpeningV1} — that refuses the substitution.
 *
 * This family's on-chain step 02 takes a single flat `Args` record: there is no
 * `Complete`/`CompletePublished`/`FoldStart`/`FoldNext` sum, hence no fold to
 * drive from here. What varies now is only §8's carriage tier, and the plan
 * chooses it from the preimage's own length.
 */
import {
  encodeMidgardTxInputCanonicalV1,
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
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
  faultProofFieldOpeningV1,
  parseNativeTxCompactCborV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "./field-opening-v1.js";
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
import { witnessSpendingValidatorCarriageV1 } from "./witness-reference-scripts-v1.js";

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
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604: the door authenticates field 1 against it.
   */
  readonly nativeTxCompactPath: string;
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
  /** §4's flat commitment for field 1 — re-derived here and by the door. */
  readonly verifiedTxReferenceInputsHash: string;
  /** The §2.5 anchor the thread carried, and the id these compact bytes derive to. */
  readonly verifiedTxId: string;
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
      "Reference-input-no-idx step 02 input datum must carry the disputed transaction's §2.5 anchor.",
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
  nativeTxCompactCbor,
  referenceScriptUtxo,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceInputsPreimage: SubmitReferenceInputNoIdxReferenceInputsPreimage;
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** The published step-02 reference script; inline-attached when absent. */
  readonly referenceScriptUtxo?: UTxO;
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
  const verifiedTxId = inputDatum.data.verified_tx_id;

  // Re-run the door off-chain: these items must be the §5.1 preimage the
  // anchored transaction committed *at field 1*, and the compact bytes must
  // re-derive to the anchor the thread carries.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.referenceInputs,
    anchorTxId: verifiedTxId,
    nativeTxCompactCbor,
    itemCbors: referenceInputsPreimage.referenceInputsPreimage.map(
      encodeMidgardTxInputCanonicalV1,
    ),
    owner: signer.paymentKeyHash,
    label: "Reference-input-no-idx step 02 reference-inputs",
  });
  const verifiedTxReferenceInputsHash = planned.commitment;

  signer.selectWallet(lucid);
  // Publish tier-2 field carriage before selecting the final fee input and
  // resolving indices into the complete reference-input set.
  const published = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Reference-input-no-idx step 02 reference-inputs field",
  });
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: chain.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "reference-input-no-idx step 02 validator",
  });
  // The complete reference-input set the built transaction will declare, in
  // build order — the opening derivation must see all of it (bug fc635c8f).
  const referenceInputs = [...published, ...stepScriptCarriage.referenceInputs];
  const referenceInputsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    label: "Reference-input-no-idx step 02 reference-inputs",
  });
  const badReferenceInput =
    referenceInputsPreimage.referenceInputsPreimage[
      referenceInputsPreimage.badReferenceInputIndex
    ];
  if (badReferenceInput === undefined) {
    throw new Error(
      `--bad-reference-input-index ${referenceInputsPreimage.badReferenceInputIndex.toString()} is out of range for a ${referenceInputsPreimage.referenceInputsPreimage.length.toString()}-item preimage.`,
    );
  }

  // A tier-2 publication sits at the prover address under a large inline datum
  // (and its min-ADA), so it tops the fee selector's descending-lovelace sort;
  // exclude datum-carrying UTxOs so the referenced publication is never spent.
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
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
            reference_inputs_opening: referenceInputsOpening,
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

  const collected = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  // Without a published witness this step reads nothing, and `readFrom([])`
  // is an error rather than a no-op, so the branch is on whether the carriage
  // produced reference inputs at all.
  const tx = (
    referenceInputs.length === 0
      ? collected
      : collected.readFrom([...referenceInputs])
  ).pay
    .ToContract(
      chain.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = stepScriptCarriage.attach(tx);

  const unsigned = await completedTx.complete({ localUPLCEval: true });
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
    verifiedTxId,
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
  const [
    blueprint,
    deploymentInfo,
    referenceInputsPreimageJson,
    nativeTxCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.referenceInputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
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
    nativeTxCompactCbor: parseNativeTxCompactCborV1(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
