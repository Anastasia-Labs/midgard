/**
 * `non-existent-input` step-02 submitter — the step that selects the challenged
 * input out of the disputed transaction's spend-input field.
 *
 * **Re-derived onto the §8.8 door by #604.** The redeemer used to reproduce the
 * whole `inputs_preimage: List<MidgardTxInput>` so the validator could re-hash
 * it against the commitment the thread carried. It now carries a
 * `FieldOpening` — the disputed transaction's compact structure plus one §8
 * carriage tier — and the validator reads item `bad_input_index` off the
 * authenticated view arithmetically. The complete input list is still this
 * builder's input, because it is the §5.1 preimage; what changed is that it
 * travels as bytes under a carriage tier rather than as a typed list.
 */

import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  MIDGARD_FIELD_INDEX,
  type MidgardTxInput,
  NonExistentInputStep02Datum,
  NonExistentInputStep02SpendRedeemer,
  NonExistentInputStep03Datum,
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
import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  type ResolvedProverSigner,
  resolveNonExistentInputDeploymentContracts,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriage } from "./witness-reference-scripts.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary.js";

/** One spend input of the bad transaction, as committed by its inputs hash. */
export type NeInputPreimageEntry = {
  readonly txId: string;
  readonly index: number | bigint;
};

const toMidgardTxInput = (entry: NeInputPreimageEntry): MidgardTxInput => ({
  tx_id: entry.txId,
  output_index: BigInt(entry.index),
});

export type NeSubmitStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly inputsPreimagePath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — the disputed transaction's
   * compact structure. New in #604: the door re-derives the anchored id from
   * these bytes and authenticates field 0 against them.
   */
  readonly nativeTxCompactPath: string;
  readonly badInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type NeSubmitStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly secondStepAddress: string;
  readonly thirdStepAddress: string;
  readonly missingInput: MidgardTxInput;
  /** The door's authenticated item count for field 0. */
  readonly spendInputsItemCount: number;
  /** Which §8 tier field 0's preimage travelled under. */
  readonly spendInputsCarriageTier: string;
  readonly badInputIndex: number;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step02DatumWithState = NonExistentInputStep02Datum & {
  readonly data: NonNullable<NonExistentInputStep02Datum["data"]>;
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, NonExistentInputStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 02 input datum must carry bad-tx-inputs state data.");
  }
  return datum as Step02DatumWithState;
};

export const neSubmitStep02 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  inputsPreimage,
  nativeTxCompactCbor,
  badInputIndex,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  certificatePolicyId,
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
  readonly inputsPreimage: readonly NeInputPreimageEntry[];
  /** The disputed transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly badInputIndex: bigint;
  /**
   * Force §8 tier 2 for field 0's preimage: publish the bytes as raw
   * carriage and reference them, instead of carrying them in this step's own
   * redeemer. Programmatic only, mirroring `submitInputNoIdxStep02` — it is
   * the one demotion §8 leaves open, and it changes which transaction pays,
   * never what the door authenticates. Below the tier-1 bound the ladder
   * picks `Inline` on its own, which is what capped this family's admissible
   * spend-input cardinality at the L1 byte frontier (#612).
   */
  readonly publishCarriage?: boolean;
  /** Pre-reconciled §8 publications; production workflows must supply them. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Authenticated tier-3 field-preimage certificate, when selected. */
  readonly certificateUtxo?: UTxO;
  /** Exact manifest-bound certificate policy for the tier-3 route. */
  readonly certificatePolicyId?: string;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Durable boundary for legacy direct publication; production pre-publishes. */
  readonly publicationPreSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<NeSubmitStep02Result> => {
  const { nonExistentInputCategory, contracts } =
    await resolveNonExistentInputDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });
  const steps = contracts.nonExistentInput.steps;

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "non-existent-input step-02 computation-thread UTxO",
  });
  if (threadUtxo.address !== steps[1].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at non-existent-input step 02.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: nonExistentInputCategory.categoryId,
    categoryLabel: "non-existent-input",
  });
  const inputDatum = requireStep02Datum({ threadUtxo, signer });

  if (badInputIndex < 0n || badInputIndex >= BigInt(inputsPreimage.length)) {
    throw new Error(
      `badInputIndex ${badInputIndex.toString()} is out of bounds for ${inputsPreimage.length.toString()} inputs.`,
    );
  }
  const midgardInputs = inputsPreimage.map(toMidgardTxInput);
  const missingInput = midgardInputs[Number(badInputIndex)]!;
  // The complete list *is* the §5.1 preimage. Planning it against the anchored
  // transaction is what the validator's `opened_field_view` will re-derive, so a
  // list that does not open the disputed transaction's field 0 is refused here
  // rather than at a validator that has already been paid for.
  const planned = planFaultProofFieldOpening({
    fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
    anchorTxId: inputDatum.data.bad_tx_id,
    nativeTxCompactCbor,
    itemCbors: midgardInputs.map(encodeMidgardTxInputCanonical),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: "Non-existent-input step 02 spend-inputs",
  });

  signer.selectWallet(lucid);
  // §8's ladder decides whether anything has to exist on-chain before this
  // transaction can reference it. Tier 1 publishes nothing and the list is
  // empty; tiers 2–3 publish raw carriage located by content (§8.7), reusing
  // a chunk that already exists at this address rather than republishing.
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriage({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Non-existent-input step 02 spend-inputs",
      preSubmitBoundary: publicationPreSubmitBoundary,
    }));
  if ((certificateUtxo === undefined) !== (certificatePolicyId === undefined)) {
    throw new Error(
      "Non-existent-input tier-3 certificate UTxO and policy identity must be supplied together.",
    );
  }
  const stepScriptCarriage = witnessSpendingValidatorCarriage({
    script: steps[1].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "non-existent-input step 02 validator",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
  ];
  const spendInputsOpening: FieldOpening = faultProofFieldOpening({
    planned,
    referenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
    label: "Non-existent-input step 02 spend-inputs",
  });
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    carriageUtxos.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      walletUtxos,
    ),
  );
  const step03Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        missing_input: missingInput,
        blocks_prev_utxos_root: inputDatum.data.blocks_prev_utxos_root,
        blocks_transactions_root: inputDatum.data.blocks_transactions_root,
      },
    },
    NonExistentInputStep03Datum,
  );
  const step03OutputMatches = computationThreadOutputPredicate({
    address: steps[2].spendingScriptAddress,
    datum: step03Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: { inputIndex: bigint; outputIndex: bigint } | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "non-existent-input step 02");
    const layout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "non-existent-input step 02",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step03OutputMatches,
        "non-existent-input step 02 output",
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
            bad_input_index: badInputIndex,
          },
        ],
      },
      NonExistentInputStep02SpendRedeemer,
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
  // Tier 1 references nothing, and `readFrom([])` is an error rather than a
  // no-op, so the branch is on whether §8 produced carriage at all.
  const tx = (
    referenceInputs.length === 0
      ? txWithInputs
      : txWithInputs.readFrom([...referenceInputs])
  ).pay
    .ToContract(
      steps[2].spendingScriptAddress,
      { kind: "inline", value: step03Datum },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash);
  const completedTx = stepScriptCarriage.attach(tx);
  const unsigned = await completedTx.complete({
    localUPLCEval: true,
    // With carriage published at the prover's own address, balancing must
    // not pick those UTxOs back up as wallet inputs while the redeemer
    // references them — same guard as `submitInputNoIdxStep02`.
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
      "BuildTxWithRedeemer did not resolve non-existent-input step 02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof non-existent-input step-02",
          utxo: referenceScriptUtxo,
          expectedScript: steps[1].spendingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `non-existent-input step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    computationThreadUnit: threadToken.unit,
    secondStepAddress: steps[1].spendingScriptAddress,
    thirdStepAddress: steps[2].spendingScriptAddress,
    missingInput,
    spendInputsItemCount: planned.itemCount,
    spendInputsCarriageTier: planned.plan.tier,
    badInputIndex: Number(badInputIndex),
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const neSubmitStep02FromFiles = async (
  config: NeSubmitStep02CliConfig,
): Promise<NeSubmitStep02Result> => {
  rejectRetiredUnauthenticatedSubmissionRoute({
    command: "submit-non-existent-input-step-02",
  });
  const [blueprint, deploymentInfo, inputsJson, nativeTxCompactJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.inputsPreimagePath),
      readJsonFile(config.nativeTxCompactPath),
      makeLucidForSubmit(config),
    ]);
  const signer = resolveProverSigner(config);
  return await neSubmitStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    inputsPreimage: inputsJson as readonly NeInputPreimageEntry[],
    nativeTxCompactCbor: parseNativeTxCompactCbor(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    badInputIndex: BigInt(config.badInputIndex),
    awaitConfirmation: config.awaitConfirmation,
  });
};
