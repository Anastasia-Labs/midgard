import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  InputNoIdxStep02SpendRedeemer,
  InputNoIdxStep03Datum,
  MIDGARD_FIELD_INDEX,
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
  faultProofFieldOpening,
  parseNativeTxCompactCbor,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveInputNoIdxDeploymentContracts,
  resolveProverSigner,
} from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./step-support.js";
import {
  type InputNoIdxStep02Layout,
  parseSubmitInputNoIdxInputsPreimage,
  requireStep02Datum,
  type SubmitInputNoIdxInputsPreimage,
  type SubmitInputNoIdxStep02CliConfig,
  type SubmitInputNoIdxStep02Result,
} from "./submit-input-no-idx-step-02.parse-submit-input-no-idx-inputs-preimage.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriage } from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

export const submitInputNoIdxStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  inputsPreimage,
  nativeTxCompactCbor,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  referenceScriptUtxo,
  publicationPreSubmitBoundary,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly inputsPreimage: SubmitInputNoIdxInputsPreimage;
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** Force §8 tier 2; see {@link SubmitInputNoIdxStep02CliConfig.publishCarriage}. */
  readonly publishCarriage?: boolean;
  readonly publishedCarriageUtxos?: readonly UTxO[];
  readonly certificateUtxo?: UTxO;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitInputNoIdxStep02Result> => {
  const { nonExistentInputNoIndexCategory, contracts } =
    await resolveInputNoIdxDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const chain = contracts.nonExistentInputNoIndex;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "input-no-idx step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== chain.steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at input-no-idx step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputNoIndexCategory.categoryId,
    categoryLabel: "input-no-idx",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });
  const verifiedTxId = inputDatum.data.verified_tx_id;

  // Re-run the door off-chain, before anything is paid for: the compact bytes
  // must re-derive to the anchor the thread carries, and this list must be the
  // §5.1 preimage that transaction commits at field 0 specifically.
  const planned = planFaultProofFieldOpening({
    anchorSourceKind: 0n,
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: verifiedTxId,
    nativeTxCompactCbor,
    itemCbors: inputsPreimage.inputsPreimage.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: "Input-no-idx step 02 spend-inputs",
  });
  const badInput = inputsPreimage.inputsPreimage[inputsPreimage.badInputsIndex];
  if (badInput === undefined) {
    throw new Error(
      `--inputs-preimage.badInputsIndex ${inputsPreimage.badInputsIndex.toString()} is out of range for a ${inputsPreimage.inputsPreimage.length.toString()}-item preimage.`,
    );
  }

  signer.selectWallet(lucid);
  // §8's ladder decides whether anything has to exist on-chain first. Tier 1
  // publishes nothing; tiers 2–3 publish raw carriage located by content (§8.7),
  // and a chunk that already exists at this address is reused rather than
  // republished.
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Input-no-idx step 02 spend-inputs",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    carriageUtxos.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      walletUtxos,
    ),
  );
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: chain.steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "input-no-idx step 02 validator",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
  ];
  const spendInputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificate.policyId,
    label: "Input-no-idx step 02 spend-inputs",
  });

  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        bad_input_tx_id: badInput.tx_id,
        bad_input_output_index: badInput.output_index,
      },
    },
    InputNoIdxStep03Datum,
  );
  const outputMatches = computationThreadOutputPredicate({
    address: chain.steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: InputNoIdxStep02Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "input-no-idx step 02");
    const layout: InputNoIdxStep02Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "input-no-idx step 02"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputMatches,
        "input-no-idx step 02 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            spend_inputs_opening: spendInputsOpening,
            bad_inputs_index: BigInt(inputsPreimage.badInputsIndex),
          },
        ],
      },
      InputNoIdxStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const threadAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [threadToken.unit]: 1n,
  };

  const txWithInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], redeemer);
  const txWithReferences =
    referenceInputs.length === 0
      ? txWithInputs
      : txWithInputs.readFrom([...referenceInputs]);
  const tx = txWithReferences.pay
    .ToContract(
      chain.steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = stepScriptCarriage.attach(tx);

  const unsigned = await completedTx.complete({
    localUPLCEval: true,
    ...(referenceInputs.length === 0
      ? {}
      : {
          presetWalletInputs: referenceInputs.reduce<readonly UTxO[]>(
            (candidates, utxo) => excludeUtxo(candidates, utxo),
            walletUtxos,
          ) as UTxO[],
        }),
  });
  if (resolvedLayout === undefined) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve input-no-idx step 02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof non-existent-input-no-index step-02",
          utxo: referenceScriptUtxo,
          expectedScript:
            contracts.nonExistentInputNoIndex.steps[1].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `input-no-idx step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    secondStepAddress: chain.steps[1].spendingScriptAddress,
    thirdStepAddress: chain.steps[2].spendingScriptAddress,
    verifiedTxId,
    verifiedTxInputsHash: planned.commitment,
    inputsPreimageItemCount: planned.itemCount,
    badInputsIndex: inputsPreimage.badInputsIndex,
    badInputTxId: badInput.tx_id,
    badInputOutputIndex: Number(badInput.output_index),
    carriageTier: planned.plan.tier,
    carriageOutRefs: carriageUtxos.map(outRefLabel),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitInputNoIdxStep02FromFiles = async (
  config: SubmitInputNoIdxStep02CliConfig,
): Promise<SubmitInputNoIdxStep02Result> => {
  const [
    blueprint,
    deploymentInfo,
    inputsPreimageJson,
    nativeTxCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.inputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitInputNoIdxStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    inputsPreimage: parseSubmitInputNoIdxInputsPreimage(inputsPreimageJson),
    nativeTxCompactCbor: parseNativeTxCompactCbor(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    ...(config.publishCarriage === undefined
      ? {}
      : { publishCarriage: config.publishCarriage }),
    awaitConfirmation: config.awaitConfirmation,
  });
};
