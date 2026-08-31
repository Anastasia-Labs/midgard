/**
 * `double-spend` step-04 submitter — opens tx2's spend-input field and concludes
 * the proof.
 *
 * **Re-derived onto the §8.8 door by #604.** Like step-03, this step lost its
 * bespoke published witness UTxO: `tx2_spend_inputs_ref_input_index` is replaced
 * by `tx2_spend_inputs_opening`, and §8's carriage ladder decides whether
 * anything is published at all. Thread state carries `verified_tx2_id`, the §2.5
 * anchor, rather than tx2's field-0 commitment.
 */

import {
  DoubleSpendStep04Datum,
  DoubleSpendStep04SpendRedeemer,
  type FieldOpeningV1,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  toUnit,
  type TxBuilder,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type PreparedClaimRegistryMutationV1,
  prepareDeploymentClaimRegistryMutationV1,
  requirePreparedClaimRegistryMutationV1,
} from "./claim-registry-transaction-v1.js";
import { parseDoubleSpentInputIndex } from "./double-spend-inputs.js";
import {
  faultProofFieldOpeningV1,
  parseNativeTxCompactCborV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "./field-opening-v1.js";
import { rejectRetiredUnauthenticatedSubmissionRouteV1 } from "./legacy-submission-boundary-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  readJsonFile,
  resolveDoubleSpendDeploymentContracts,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  excludeUtxo,
  spendInputsWitnessFromCbors,
} from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { parseSpendInputCbors } from "./submit-step-03.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
  witnessSpendingValidatorCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptV1,
} from "./workflow/transaction-boundary-v1.js";

export type SubmitStep04CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly tx2InputsPath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — **tx2's** compact structure. New
   * in #604: the door authenticates its field 0 against `verified_tx2_id`.
   */
  readonly nativeTxCompactPath: string;
  readonly doubleSpentInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep04Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  readonly fourthStepAddress: string;
  /** The §2.5 anchor the thread carried for tx2. */
  readonly verifiedTx2Id: string;
  /** §4's flat commitment for tx2's field 0 — re-derived here and by the door. */
  readonly verifiedTx2SpendInputsHash: string;
  readonly doubleSpentInputIndex: number;
  readonly doubleSpentInput: MidgardTxInput;
  readonly doubleSpentInputCbor: string;
  /** Which §8 tier tx2's field-0 preimage travelled under. */
  readonly tx2SpendInputsCarriageTier: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step04DatumWithState = DoubleSpendStep04Datum & {
  readonly data: NonNullable<DoubleSpendStep04Datum["data"]>;
};

type Step04ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

type Step04SpendLayout = Omit<
  Step04ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

const requireStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step04DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error("Step 04 input datum must carry verified tx2 state data.");
  }
  return datum as Step04DatumWithState;
};

const sameMidgardTxInput = (
  left: MidgardTxInput,
  right: MidgardTxInput,
): boolean =>
  left.tx_id === right.tx_id && left.output_index === right.output_index;

const fraudProofOutputPredicate = ({
  fraudProofAddress,
  fraudProofUnit,
  fraudProofDatum,
}: {
  readonly fraudProofAddress: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
}): ((output: TxOutput) => boolean) =>
  outputWithDatumAndUnitPredicate({
    address: fraudProofAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });

const makeStep04SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  tx2SpendInputsOpening,
  doubleSpentInputIndex,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly tx2SpendInputsOpening: FieldOpeningV1;
  readonly doubleSpentInputIndex: bigint;
  readonly onLayout: (layout: Step04SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "step 04 computation thread");
    const layout: Step04SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "step 04 computation thread",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "step 04 fraud-proof",
      ),
    };
    onLayout(layout);
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
            tx2_spend_inputs_opening: tx2SpendInputsOpening,
            double_spent_input_index: doubleSpentInputIndex,
          },
        ],
      },
      DoubleSpendStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeFraudProofMintRedeemer = ({
  fraudProofPolicyId,
  computationThreadPolicyId,
  computationThreadAssetName,
  onComputationThreadMintRedeemerIndex,
}: {
  readonly fraudProofPolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
  readonly onComputationThreadMintRedeemerIndex: (index: bigint) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(ctx, fraudProofPolicyId, "step 04 fraud-proof mint");
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "step 04 computation-thread burn",
    );
    onComputationThreadMintRedeemerIndex(computationThreadMintRedeemerIndex);
    return Data.to(
      {
        computation_thread_token_asset_name: computationThreadAssetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

const makeComputationThreadSuccessRedeemer = ({
  computationThreadPolicyId,
  computationThreadAssetName,
}: {
  readonly computationThreadPolicyId: string;
  readonly computationThreadAssetName: string;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      computationThreadPolicyId,
      "step 04 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const submitStep04 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx2SpendInputCbors,
  nativeTxCompactCbor,
  doubleSpentInputIndex,
  publishCarriage = false,
  publishedCarriageUtxos,
  certificateUtxo,
  certificatePolicyId,
  referenceScriptUtxo,
  witnessReferenceScripts,
  claimRegistryMutation,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx2SpendInputCbors: readonly string[];
  /** tx2's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly doubleSpentInputIndex: bigint;
  /**
   * Force §8 tier 2 for tx2's field-0 preimage; see `submitStep03`'s
   * same-named option. Programmatic only — the retired CLI route never
   * sets it, and below the tier-1 bound the ladder would otherwise carry
   * the preimage inline in this step's redeemer (#612).
   */
  readonly publishCarriage?: boolean;
  /** Pre-observed tier-2/3 publications for journaled workflow use. */
  readonly publishedCarriageUtxos?: readonly UTxO[];
  /** Pre-minted §8.6 certificate, required when the plan selects tier 3. */
  readonly certificateUtxo?: UTxO;
  readonly certificatePolicyId?: string;
  /** The mandatory published step-04 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  /** Pre-prepared claim-registry `CloseClaim`; self-prepared when omitted. */
  readonly claimRegistryMutation?: PreparedClaimRegistryMutationV1;
  /** Production workflow seam for carriage and proof-step submissions. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep04Result> => {
  const { doubleSpendCategory, contracts } =
    await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
      requireFraudProofSpend: true,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-04 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[3].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 04.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: doubleSpendCategory.categoryId,
    categoryLabel: "double-spend",
  });
  const inputDatum = requireStep04Datum({ threadUtxo, signer });
  // The door's own checks, run before a transaction is built.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: inputDatum.data.verified_tx2_id,
    nativeTxCompactCbor,
    itemCbors: tx2SpendInputCbors.map((inputCbor) =>
      Buffer.from(inputCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: "Double-spend step 04 tx2 spend-inputs",
  });
  const tx2SpendInputsHash = planned.commitment;
  if (doubleSpentInputIndex >= BigInt(tx2SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx2SpendInputCbors.length.toString()} tx2 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const tx2SpendInputsWitness = spendInputsWitnessFromCbors(
    tx2SpendInputCbors,
    "--tx2-inputs",
  );
  const doubleSpentInputCbor =
    tx2SpendInputCbors[Number(doubleSpentInputIndex)]!;
  const doubleSpentInput =
    tx2SpendInputsWitness.inputs[Number(doubleSpentInputIndex)]!;
  if (
    !sameMidgardTxInput(doubleSpentInput, inputDatum.data.double_spent_input)
  ) {
    throw new Error(
      `--tx2-inputs[${doubleSpentInputIndex.toString()}] does not match the double-spent input carried by step 04 datum.`,
    );
  }

  signer.selectWallet(lucid);
  const carriageUtxos =
    publishedCarriageUtxos ??
    (await publishFaultProofFieldCarriageV1({
      lucid,
      signer,
      planned,
      publisherAddress: signer.address,
      label: "Double-spend step 04 tx2 spend-inputs",
      preSubmitBoundary,
    }));
  const stepScriptCarriage = witnessSpendingValidatorCarriageV1({
    script: contracts.doubleSpend.steps[3].spendingScript,
    referenceUtxo: referenceScriptUtxo,
    label: "double-spend step 04 validator",
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "double-spend step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "double-spend step 04 fraud-proof mint",
  });
  const resolvedClaimRegistryMutation = requirePreparedClaimRegistryMutationV1({
    mutation:
      claimRegistryMutation ??
      (await prepareDeploymentClaimRegistryMutationV1({
        lucid,
        claimRegistryReferenceUtxo: witnessReferenceScripts?.claimRegistrySpend,
        blueprint,
        deploymentInfo,
        network,
        computationThreadPolicyId: contracts.computationThread.policyId,
        claimId: threadToken.assetName,
        kind: "close",
      })),
    kind: "close",
    claimId: threadToken.assetName,
    label: "double-spend step 04",
  });
  const referenceInputs = [
    ...carriageUtxos,
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...stepScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
    ...resolvedClaimRegistryMutation.referenceInputs,
  ];
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    [...carriageUtxos, ...resolvedClaimRegistryMutation.referenceInputs].reduce<
      readonly UTxO[]
    >((candidates, utxo) => excludeUtxo(candidates, utxo), walletUtxos),
  );
  const tx2SpendInputsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
    label: "Double-spend step 04 tx2 spend-inputs",
  });
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofAssets = {
    lovelace: threadUtxo.assets.lovelace ?? 0n,
    [fraudProofUnit]: 1n,
  };
  let spendLayout: Step04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  const computationThreadSuccessRedeemer = makeComputationThreadSuccessRedeemer(
    {
      computationThreadPolicyId: contracts.computationThread.policyId,
      computationThreadAssetName: threadToken.assetName,
    },
  );

  const makeStep04Tx = (): TxBuilder => {
    const withInputs = lucid
      .newTx()
      .collectFrom([feeInput])
      .collectFrom(
        [threadUtxo],
        makeStep04SpendRedeemer({
          threadUtxo,
          fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
          fraudProofPolicyId: contracts.fraudProof.policyId,
          fraudProofUnit,
          fraudProofDatum,
          tx2SpendInputsOpening,
          doubleSpentInputIndex,
          onLayout: (layout) => {
            spendLayout = layout;
          },
        }),
      );
    // Tier 1 references nothing, and `readFrom([])` is an error rather than a
    // no-op, so the branch is on whether §8 produced carriage at all.
    const chained = (
      referenceInputs.length === 0
        ? withInputs
        : withInputs.readFrom([...referenceInputs])
    )
      .mintAssets({ [threadToken.unit]: -1n }, computationThreadSuccessRedeemer)
      .mintAssets(
        { [fraudProofUnit]: 1n },
        makeFraudProofMintRedeemer({
          fraudProofPolicyId: contracts.fraudProof.policyId,
          computationThreadPolicyId: contracts.computationThread.policyId,
          computationThreadAssetName: threadToken.assetName,
          onComputationThreadMintRedeemerIndex: (index) => {
            computationThreadMintRedeemerIndex = index;
          },
        }),
      )
      .pay.ToContract(
        contracts.fraudProof.spendingScriptAddress,
        { kind: "inline", value: fraudProofDatum },
        fraudProofAssets,
      )
      .addSignerKey(signer.paymentKeyHash);
    return fraudProofMintCarriage.attach(
      computationThreadMintCarriage.attach(
        stepScriptCarriage.attach(resolvedClaimRegistryMutation.apply(chained)),
      ),
    );
  };

  const unsigned = await makeStep04Tx().complete({
    localUPLCEval: true,
    // With carriage published at the prover's own address, balancing must not
    // pick those UTxOs back up as wallet inputs while the redeemer references
    // them — same guard as `submitInputNoIdxStep02`.
    ...(referenceInputs.length === 0
      ? {}
      : {
          presetWalletInputs: referenceInputs.reduce<readonly UTxO[]>(
            (candidates, utxo) => excludeUtxo(candidates, utxo),
            walletUtxos,
          ) as UTxO[],
        }),
  });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error("BuildTxWithRedeemer did not resolve step 04 layout.");
  }
  const resolvedLayout: Step04ResolvedLayout = {
    ...spendLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: [
      workflowReferenceScriptV1({
        role: "V1 fraud-proof double-spend step-04",
        utxo: referenceScriptUtxo,
        expectedScript: contracts.doubleSpend.steps[3].spendingScript,
      }),
      workflowReferenceScriptV1({
        role: "V1 fraud-proof computation-thread minting",
        utxo: witnessReferenceScripts?.computationThreadMint,
        expectedScript: contracts.computationThread.mintingScript,
      }),
      workflowReferenceScriptV1({
        role: "V1 fraud-proof token minting",
        utxo: witnessReferenceScripts?.fraudProofMint,
        expectedScript: contracts.fraudProof.mintingScript,
      }),
      workflowReferenceScriptV1({
        role: "claim-registry spending",
        utxo: resolvedClaimRegistryMutation.referenceScriptUtxo,
        expectedScript: resolvedClaimRegistryMutation.registryScript,
      }),
    ],
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
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
    fraudProofOutRef: `${txHash}#${resolvedLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx2Id: inputDatum.data.verified_tx2_id,
    verifiedTx2SpendInputsHash: tx2SpendInputsHash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInput,
    doubleSpentInputCbor,
    tx2SpendInputsCarriageTier: planned.plan.tier,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      resolvedLayout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      resolvedLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep04FromFiles = async (
  config: SubmitStep04CliConfig,
): Promise<SubmitStep04Result> => {
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: "submit-step-04",
  });
  const [blueprint, deploymentInfo, tx2InputsJson, nativeTxCompactJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.tx2InputsPath),
      readJsonFile(config.nativeTxCompactPath),
      makeLucidForSubmit(config),
    ]);
  const tx2SpendInputCbors = parseSpendInputCbors(
    tx2InputsJson,
    "--tx2-inputs",
  );
  const signer = resolveProverSigner(config);
  return await submitStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    tx2SpendInputCbors,
    nativeTxCompactCbor: parseNativeTxCompactCborV1(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    doubleSpentInputIndex: parseDoubleSpentInputIndex({
      value: config.doubleSpentInputIndex,
      inputCount: tx2SpendInputCbors.length,
      inputLabel: "tx2",
    }),
    awaitConfirmation: config.awaitConfirmation,
  });
};
