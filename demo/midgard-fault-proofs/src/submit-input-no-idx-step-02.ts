/**
 * `input-no-idx` step-02 submitter (Goal task `Q13`, §9.1 output 8).
 *
 * Opens §2.5 field 0 of the transaction carried by step-01 and forwards the
 * challenged `(tx_id, output_index)` to step-03.
 *
 * **Re-derived onto the §8.8 door by #604, and this step lost two mechanisms to
 * it.** The on-chain `Args` used to be a four-arm sum, and this module drove all
 * four:
 *
 *   * `Complete` reproduced the whole input list in the redeemer;
 *   * `CompletePublished` referenced a **bespoke** `PublishedSpendInputsV1`
 *     typed datum that this module published, matched by out-ref, and checked
 *     field by field;
 *   * `FoldStart`/`FoldNext` streamed the collection one counted opening at a
 *     time, resuming through the computation thread itself.
 *
 * All four existed because the collection had to be reproduced *inside the step*
 * to re-hash it against the commitment the thread carried. §4's flat commitment
 * and the §8.8 door removed that need entirely: the door hashes the preimage
 * once and reads item `n` by arithmetic. So the redeemer has exactly one route,
 * and the prover's only remaining choice is *how the preimage travels* — which
 * is §8's carriage ladder, not a family-specific mechanism.
 *
 * Concretely:
 *
 *   * the typed publication is **deleted**, not re-pointed. Its replacement is
 *     §8.5 raw carriage — a nothing-but-bytes inline datum published through
 *     `buildUnsignedFieldPreimagePublicationV1Program` and located by *content*
 *     (§8.7), so a republished copy is interchangeable with the one it replaces.
 *     The bespoke datum could not be: it bound the publication to one computation
 *     thread and one prover, which is precisely the coupling §8.7 forbids;
 *   * the ordered fold is **gone**, and with it
 *     `submitInputNoIdxStep02UntilTerminal` and the `submit-input-no-idx-fold`
 *     command. There is no `FoldStart` arm on-chain to emit.
 *
 * Nothing in the prepared file is trusted: the anchor is read from the
 * **on-chain** step-01 datum, and the supplied list must be the §5.1 preimage the
 * anchored transaction commits at field 0 — checked by
 * {@link planFaultProofFieldOpening} before a transaction is built.
 */
import {
  encodeMidgardTxInputCanonical,
  type FieldOpening,
  InputNoIdxStep02Datum,
  InputNoIdxStep02SpendRedeemer,
  InputNoIdxStep03Datum,
  MIDGARD_FIELD_INDEX,
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

import {
  faultProofFieldOpening,
  parseNativeTxCompactCbor,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "./field-opening-v1.js";
import { parseHex, requireRecord } from "./json-file.js";
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
  type SubmitProviderConfig,
} from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { computationThreadOutputPredicate } from "./tx-layout.js";
import { witnessSpendingValidatorCarriage } from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "./workflow/transaction-boundary-v1.js";

/** Prepared spend-inputs preimage produced by `prepare-input-no-idx`. */
export type SubmitInputNoIdxInputsPreimage = {
  readonly inputsPreimage: readonly MidgardTxInput[];
  readonly badInputsIndex: number;
};

const parseNonNegativeInteger = (value: unknown, label: string): number => {
  const parsed = typeof value === "number" ? value : Number(value);
  if (!Number.isInteger(parsed) || parsed < 0) {
    throw new Error(`${label} must be a non-negative integer.`);
  }
  return parsed;
};

export const parseSubmitInputNoIdxInputsPreimage = (
  value: unknown,
): SubmitInputNoIdxInputsPreimage => {
  const record = requireRecord(value, "--inputs-preimage");
  const rawInputs = record.inputsPreimage;
  if (!Array.isArray(rawInputs)) {
    throw new Error("--inputs-preimage.inputsPreimage must be a JSON array.");
  }
  const inputsPreimage = rawInputs.map((item, index) => {
    const entry = requireRecord(
      item,
      `--inputs-preimage.inputsPreimage[${index.toString()}]`,
    );
    return {
      tx_id: parseHex(
        entry.tx_id,
        `--inputs-preimage.inputsPreimage[${index.toString()}].tx_id`,
        32,
      ),
      output_index: BigInt(
        parseNonNegativeInteger(
          entry.output_index,
          `--inputs-preimage.inputsPreimage[${index.toString()}].output_index`,
        ),
      ),
    };
  });
  return {
    inputsPreimage,
    badInputsIndex: parseNonNegativeInteger(
      record.badInputsIndex,
      "--inputs-preimage.badInputsIndex",
    ),
  };
};

export type SubmitInputNoIdxStep02CliConfig = SubmitProviderConfig & {
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
  /**
   * Force §8 tier 2 for field 0's preimage: publish the bytes as raw carriage
   * and reference them, instead of carrying them in this step's own redeemer.
   *
   * **Programmatic only — `bin.ts` parses no `--publish-carriage` flag**, so a
   * config assembled from argv never sets it and the shipped CLI always lets
   * the ladder decide. It is settable only by a caller that builds this config
   * in process, and it is forwarded from here to the same-named option on
   * {@link submitInputNoIdxStep02}, which is what the emulator leg exercises
   * directly. That the CLI does not expose it is deliberate: it is the **only**
   * tier choice §8 leaves open, and it changes which transaction pays rather
   * than what the door authenticates, so there is nothing an operator gains by
   * naming it. Above the tier-1 bound the ladder publishes on its own and the
   * option is redundant.
   */
  readonly publishCarriage?: boolean;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInputNoIdxStep02Result = {
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
  /** The §2.5 anchor the thread carried, and the id these compact bytes derive to. */
  readonly verifiedTxId: string;
  /** §4's flat commitment for field 0 — re-derived here and by the door. */
  readonly verifiedTxInputsHash: string;
  readonly inputsPreimageItemCount: number;
  readonly badInputsIndex: number;
  readonly badInputTxId: string;
  readonly badInputOutputIndex: number;
  /** Which §8 tier field 0's preimage travelled under. */
  readonly carriageTier: string;
  /** Out-refs of the §8.5 raw carriage this submission referenced, in §8.4 order. */
  readonly carriageOutRefs: readonly string[];
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InputNoIdxStep02Layout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
};

type InputNoIdxStep02DatumWithState = InputNoIdxStep02Datum & {
  readonly data: NonNullable<InputNoIdxStep02Datum["data"]>;
};

const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InputNoIdxStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InputNoIdxStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Input-no-idx step 02 input datum must carry the disputed transaction's §2.5 anchor.",
    );
  }
  return datum as InputNoIdxStep02DatumWithState;
};

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
