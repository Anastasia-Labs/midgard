/**
 * `double-spend` step-03 submitter — opens tx1's spend-input field and selects
 * the doubly-spent input.
 *
 * **Re-derived onto the §8.8 door by #604, and this step lost a whole mechanism
 * to it.** The redeemer used to carry `tx1_spend_inputs_ref_input_index`, an
 * index into a *bespoke* published witness UTxO this builder created and
 * referenced (`ensureSpendInputsReferenceWitness`). §8's carriage ladder
 * subsumes that: tier 2 is the door's own `RawUtxo` arm, carried inside the
 * opening and content-addressed by §8.7 rather than named by out-ref, and tier 1
 * needs no publication at all. So the family-specific publication is gone and
 * `tx1_spend_inputs_opening` replaces its index.
 *
 * The thread now carries `verified_tx1_id`/`verified_tx2_id` — the §2.5 anchors
 * — rather than the two field-0 commitments, which is why this builder takes
 * tx1's compact structure.
 */

import {
  DoubleSpendStep03Datum,
  DoubleSpendStep03SpendRedeemer,
  DoubleSpendStep04Datum,
  type FieldOpeningV1,
  MIDGARD_FIELD_INDEX_V1,
  type MidgardTxInput,
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

import { parseDoubleSpentInputIndex } from "./double-spend-inputs.js";
import {
  faultProofFieldOpeningV1,
  parseNativeTxCompactCborV1,
  planFaultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "./field-opening-v1.js";
import { parseHex } from "./json-file.js";
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
import { computationThreadOutputPredicate } from "./tx-layout.js";

export type SubmitStep03CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly tx1InputsPath: string;
  /**
   * JSON `{ "nativeTxCompactCbor": "<hex>" }` — **tx1's** compact structure. New
   * in #604: the door authenticates its field 0 against `verified_tx1_id`.
   */
  readonly nativeTxCompactPath: string;
  readonly doubleSpentInputIndex: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitStep03Result = {
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
  readonly thirdStepAddress: string;
  readonly fourthStepAddress: string;
  /** The two §2.5 anchors the thread carried. */
  readonly verifiedTx1Id: string;
  readonly verifiedTx2Id: string;
  /** §4's flat commitment for tx1's field 0 — re-derived here and by the door. */
  readonly verifiedTx1SpendInputsHash: string;
  readonly doubleSpentInputIndex: number;
  readonly doubleSpentInput: MidgardTxInput;
  readonly doubleSpentInputCbor: string;
  /** Which §8 tier tx1's field-0 preimage travelled under. */
  readonly tx1SpendInputsCarriageTier: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step03Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

export const parseSpendInputCbors = (
  value: unknown,
  label: string,
): readonly string[] => {
  if (!Array.isArray(value)) {
    throw new Error(
      `${label} must be a JSON array of raw input CBOR hex strings.`,
    );
  }
  return value.map((entry, index) =>
    parseHex(entry, `${label}[${index.toString()}]`),
  );
};

type Step03DatumWithState = DoubleSpendStep03Datum & {
  readonly data: NonNullable<DoubleSpendStep03Datum["data"]>;
};

const requireStep03Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): Step03DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, DoubleSpendStep03Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Step 03 input datum must carry both disputed transactions' §2.5 anchors.",
    );
  }
  return datum as Step03DatumWithState;
};

export const submitStep03 = async ({
  lucid,
  blueprint,
  deploymentInfo,
  network,
  signer,
  threadOutRef,
  tx1SpendInputCbors,
  nativeTxCompactCbor,
  doubleSpentInputIndex,
  publishCarriage = false,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly tx1SpendInputCbors: readonly string[];
  /** tx1's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  readonly doubleSpentInputIndex: bigint;
  /**
   * Force §8 tier 2 for tx1's field-0 preimage: publish the bytes as raw
   * carriage and reference them, instead of carrying them in this step's own
   * redeemer. Programmatic only, mirroring `submitInputNoIdxStep02` — it is
   * the one demotion §8 leaves open, and it changes which transaction pays,
   * never what the door authenticates. Below the tier-1 bound the ladder
   * picks `Inline` on its own, which is what capped this family's admissible
   * spend-input cardinality at the L1 byte frontier (#612).
   */
  readonly publishCarriage?: boolean;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitStep03Result> => {
  const { doubleSpendCategory, contracts } =
    await resolveDoubleSpendDeploymentContracts({
      blueprint,
      deploymentInfo,
      network,
    });

  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "step-03 computation-thread UTxO",
  });
  if (
    threadUtxo.address !== contracts.doubleSpend.steps[2].spendingScriptAddress
  ) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at double-spend step 03.`,
    );
  }

  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: doubleSpendCategory.categoryId,
    categoryLabel: "double-spend",
  });
  const inputDatum = requireStep03Datum({ threadUtxo, signer });
  // The door's own checks, run before a transaction is built: tx1's compact
  // bytes re-derive to the anchor the thread carries, and these items are the
  // §5.1 preimage that transaction commits at field 0.
  const planned = planFaultProofFieldOpeningV1({
    fieldIndex: MIDGARD_FIELD_INDEX_V1.spendInputs,
    anchorTxId: inputDatum.data.verified_tx1_id,
    nativeTxCompactCbor,
    itemCbors: tx1SpendInputCbors.map((inputCbor) =>
      Buffer.from(inputCbor, "hex"),
    ),
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
    label: "Double-spend step 03 tx1 spend-inputs",
  });
  const tx1SpendInputsHash = planned.commitment;
  if (doubleSpentInputIndex >= BigInt(tx1SpendInputCbors.length)) {
    throw new Error(
      `doubleSpentInputIndex ${doubleSpentInputIndex.toString()} is out of bounds for ${tx1SpendInputCbors.length.toString()} tx1 inputs.`,
    );
  }
  if (doubleSpentInputIndex > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error("doubleSpentInputIndex exceeds the safe integer range.");
  }
  const tx1SpendInputsWitness = spendInputsWitnessFromCbors(
    tx1SpendInputCbors,
    "--tx1-inputs",
  );
  const doubleSpentInputCbor =
    tx1SpendInputCbors[Number(doubleSpentInputIndex)]!;
  const doubleSpentInput =
    tx1SpendInputsWitness.inputs[Number(doubleSpentInputIndex)]!;

  signer.selectWallet(lucid);
  // §8's ladder decides whether anything has to exist on-chain before this
  // transaction can reference it. Tier 1 publishes nothing and the list is empty.
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: "Double-spend step 03 tx1 spend-inputs",
  });
  const referenceInputs = [...carriageUtxos];
  const walletUtxos = await lucid.wallet().getUtxos();
  const feeInput = selectFeeInput(
    carriageUtxos.reduce<readonly UTxO[]>(
      (candidates, utxo) => excludeUtxo(candidates, utxo),
      walletUtxos,
    ),
  );
  const tx1SpendInputsOpening: FieldOpeningV1 = faultProofFieldOpeningV1({
    planned,
    referenceInputs,
    label: "Double-spend step 03 tx1 spend-inputs",
  });
  const step04Datum = Data.to(
    {
      fraud_prover: signer.paymentKeyHash,
      data: {
        verified_tx2_id: inputDatum.data.verified_tx2_id,
        double_spent_input: doubleSpentInput,
      },
    },
    DoubleSpendStep04Datum,
  );
  const step04OutputMatches = computationThreadOutputPredicate({
    address: contracts.doubleSpend.steps[3].spendingScriptAddress,
    datum: step04Datum,
    unit: threadToken.unit,
  });
  let resolvedLayout: Step03Layout | undefined;
  const redeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "double-spend step 03");
    const layout: Step03Layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "double-spend step 03"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        step04OutputMatches,
        "double-spend step 03 output",
      ),
    };
    resolvedLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            tx1_spend_inputs_opening: tx1SpendInputsOpening,
            double_spent_input_index: doubleSpentInputIndex,
          },
        ],
      },
      DoubleSpendStep03SpendRedeemer,
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
      contracts.doubleSpend.steps[3].spendingScriptAddress,
      {
        kind: "inline",
        value: step04Datum,
      },
      threadAssets,
    )
    .addSignerKey(signer.paymentKeyHash)
    .attach.SpendingValidator(contracts.doubleSpend.steps[2].spendingScript);

  const unsigned = await tx.complete({
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
  if (resolvedLayout === undefined) {
    throw new Error("BuildTxWithRedeemer did not resolve step 03 layout.");
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
    thirdStepAddress: contracts.doubleSpend.steps[2].spendingScriptAddress,
    fourthStepAddress: contracts.doubleSpend.steps[3].spendingScriptAddress,
    verifiedTx1Id: inputDatum.data.verified_tx1_id,
    verifiedTx2Id: inputDatum.data.verified_tx2_id,
    verifiedTx1SpendInputsHash: tx1SpendInputsHash,
    doubleSpentInputIndex: Number(doubleSpentInputIndex),
    doubleSpentInput,
    doubleSpentInputCbor,
    tx1SpendInputsCarriageTier: planned.plan.tier,
    inputIndex: Number(resolvedLayout.inputIndex),
    outputIndex: Number(resolvedLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitStep03FromFiles = async (
  config: SubmitStep03CliConfig,
): Promise<SubmitStep03Result> => {
  rejectRetiredUnauthenticatedSubmissionRouteV1({
    command: "submit-step-03",
  });
  const [blueprint, deploymentInfo, tx1InputsJson, nativeTxCompactJson, lucid] =
    await Promise.all([
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.tx1InputsPath),
      readJsonFile(config.nativeTxCompactPath),
      makeLucidForSubmit(config),
    ]);
  const tx1SpendInputCbors = parseSpendInputCbors(
    tx1InputsJson,
    "--tx1-inputs",
  );
  const signer = resolveProverSigner(config);
  return await submitStep03({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    tx1SpendInputCbors,
    nativeTxCompactCbor: parseNativeTxCompactCborV1(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    doubleSpentInputIndex: parseDoubleSpentInputIndex({
      value: config.doubleSpentInputIndex,
      inputCount: tx1SpendInputCbors.length,
      inputLabel: "tx1",
    }),
    awaitConfirmation: config.awaitConfirmation,
  });
};
