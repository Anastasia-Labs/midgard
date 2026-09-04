/**
 * `input-set-uniqueness` step-02 submitter — the step that both concludes and
 * finalizes the proof: the claimed items are opened through the §8.8 door
 * against the thread's anchored transaction id, the byte-equality conviction
 * is re-checked locally, and then the computation-thread NFT burns while the
 * permanent fraud-proof token mints to the fraud-proof address.
 *
 * Three claim arms, mirroring the validator:
 *
 * - `duplicateSpendInputs` / `duplicateReferenceInputs` open one field (0 or
 *   1) via `opened_field_view` and compare two of its §5.3 items at
 *   `first_index < second_index`.
 * - `spendReferenceOverlap` pays the §3 anchor once (`anchored_native_tx`),
 *   opens both fields against it, and compares one item of each — no index
 *   relation, since the same position in two different lists is only a fault
 *   when the out-refs match.
 *
 * Every conviction predicate is twinned locally fail-closed before anything
 * is paid for: indices in range, `first < second` where the arm requires it,
 * and byte equality of the claimed items — §2.5 fields 0/1 share the §5.3
 * out-ref item encoding, so item byte equality *is* out-ref equality.
 */
import {
  type FieldCarriage,
  fieldOpeningForField,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  InputSetUniquenessStep02Args,
  InputSetUniquenessStep02Datum,
  InputSetUniquenessStep02SpendRedeemer,
  type InputSetUniquenessStep02State,
  MIDGARD_FIELD_INDEX,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  faultProofFieldCarriage,
  faultProofFieldOpening,
  type FaultProofFieldOpeningPlan,
  planFaultProofFieldOpening,
  publishFaultProofFieldCarriage,
} from "../field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { selectFeeInput } from "../submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../witness-reference-scripts.js";
import type { FraudProofPreSubmitBoundary } from "../workflow/transaction-boundary.js";
import {
  reachFraudProofPreSubmitBoundary,
  workflowReferenceScriptsUsedByTransaction,
} from "../workflow/transaction-boundary.js";
import type { InputSetUniquenessContracts } from "./contracts.js";
import type { InputSetUniquenessClaim } from "./scan.js";
import {
  inputSetUniquenessStepLabel,
  inputSetUniquenessSubmitError,
  requireInputSetUniquenessReferenceScript,
  requireInputSetUniquenessStepState,
  requireInputSetUniquenessThreadUtxo,
} from "./submit-common.js";

const STEP_LABEL = inputSetUniquenessStepLabel(1);

export type SubmitInputSetUniquenessStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudProofOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofAssetName: string;
  readonly fraudProofUnit: string;
  readonly fraudProofAddress: string;
  /** The anchored transaction the conviction opened. */
  readonly badTxId: string;
  readonly claim: InputSetUniquenessClaim;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type Step02SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

const uniqueUtxos = (utxos: readonly UTxO[]): readonly UTxO[] => {
  const seen = new Set<string>();
  return utxos.filter((utxo) => {
    const key = `${utxo.txHash}#${utxo.outputIndex.toString()}`;
    if (seen.has(key)) {
      return false;
    }
    seen.add(key);
    return true;
  });
};

const normalizedItems = (
  items: readonly string[],
  fieldLabel: string,
): readonly string[] =>
  items.map((item, index) => {
    const lowered = item.toLowerCase();
    if (!/^([0-9a-f]{2})+$/u.test(lowered)) {
      throw inputSetUniquenessSubmitError(
        `${STEP_LABEL} ${fieldLabel} item ${index.toString()} is not hexadecimal.`,
      );
    }
    return lowered;
  });

const requireItemAt = (
  items: readonly string[],
  index: bigint,
  fieldLabel: string,
): string => {
  if (index < 0n || index >= BigInt(items.length)) {
    throw inputSetUniquenessSubmitError(
      `${STEP_LABEL} ${fieldLabel} index ${index.toString()} is outside the field's ${items.length.toString()} committed items.`,
    );
  }
  return items[Number(index)] as string;
};

/** Twin of the validator's per-arm conviction predicate, fail-closed. */
export const assertInputSetUniquenessClaimConvicts = ({
  claim,
  spendInputItemCbors,
  referenceInputItemCbors,
}: {
  readonly claim: InputSetUniquenessClaim;
  readonly spendInputItemCbors: readonly string[];
  readonly referenceInputItemCbors: readonly string[];
}): void => {
  const spends = normalizedItems(spendInputItemCbors, "spend-input");
  const references = normalizedItems(
    referenceInputItemCbors,
    "reference-input",
  );
  if (claim.kind === "spendReferenceOverlap") {
    const spendItem = requireItemAt(spends, claim.spendIndex, "spend-input");
    const referenceItem = requireItemAt(
      references,
      claim.referenceIndex,
      "reference-input",
    );
    if (spendItem !== referenceItem) {
      throw inputSetUniquenessSubmitError(
        `${STEP_LABEL} spend input ${claim.spendIndex.toString()} and reference input ${claim.referenceIndex.toString()} name different out-refs; the sets are disjoint at the claimed positions.`,
      );
    }
    return;
  }
  const fieldLabel =
    claim.kind === "duplicateSpendInputs" ? "spend-input" : "reference-input";
  const items = claim.kind === "duplicateSpendInputs" ? spends : references;
  if (claim.firstIndex >= claim.secondIndex) {
    throw inputSetUniquenessSubmitError(
      `${STEP_LABEL} duplicate claim needs first_index < second_index; got ${claim.firstIndex.toString()} and ${claim.secondIndex.toString()}.`,
    );
  }
  const first = requireItemAt(items, claim.firstIndex, fieldLabel);
  const second = requireItemAt(items, claim.secondIndex, fieldLabel);
  if (first !== second) {
    throw inputSetUniquenessSubmitError(
      `${STEP_LABEL} ${fieldLabel} items ${claim.firstIndex.toString()} and ${claim.secondIndex.toString()} name different out-refs; there is no duplicate at the claimed positions.`,
    );
  }
};

export const submitInputSetUniquenessStep02 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  claim,
  nativeTxCompactCbor,
  spendInputItemCbors,
  referenceInputItemCbors,
  publishedSpendCarriageUtxos,
  spendCertificateUtxo,
  publishedReferenceCarriageUtxos,
  referenceCertificateUtxo,
  publishMissingCarriage = true,
  referenceScriptUtxo,
  witnessReferenceScripts,
  preSubmitBoundary,
  awaitConfirmation = true,
  unsafeSpendFieldRawUtxoForTest,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: InputSetUniquenessContracts;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly claim: InputSetUniquenessClaim;
  /** The disputed transaction's compact CBOR — the door re-derives the anchored id from these bytes. */
  readonly nativeTxCompactCbor: string;
  /** §2.5 field 0's canonical §5.3 items, hex, in committed order. */
  readonly spendInputItemCbors: readonly string[];
  /** §2.5 field 1's canonical §5.3 items, hex, in committed order. */
  readonly referenceInputItemCbors: readonly string[];
  /** Pre-authenticated §8 publications for field 0. */
  readonly publishedSpendCarriageUtxos?: readonly UTxO[];
  /** Pre-authenticated §8.6 certificate for field 0 when tier 3 is selected. */
  readonly spendCertificateUtxo?: UTxO;
  /** Pre-authenticated §8 publications for field 1. */
  readonly publishedReferenceCarriageUtxos?: readonly UTxO[];
  /** Pre-authenticated §8.6 certificate for field 1 when tier 3 is selected. */
  readonly referenceCertificateUtxo?: UTxO;
  /** Diagnostic/emulator fallback only. Production workflows set false. */
  readonly publishMissingCarriage?: boolean;
  /** The mandatory published step-02 reference script. */
  readonly referenceScriptUtxo?: UTxO;
  /** Published witness reference scripts required by this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
  /**
   * Test-only adversarial injection for the tier-2 refusal suite: the
   * `DuplicateSpendInputs` redeemer names this UTxO as the field's `RawUtxo`
   * publication (it is added to the transaction's reference inputs) instead
   * of the honestly published one, so the door's `field_commitment` re-hash
   * is what refuses the transaction on-chain. Never set outside tests.
   */
  readonly unsafeSpendFieldRawUtxoForTest?: UTxO;
}): Promise<SubmitInputSetUniquenessStep02Result> => {
  const { threadUtxo, threadToken } = await requireInputSetUniquenessThreadUtxo(
    {
      lucid,
      contracts,
      categoryId,
      stepIndex: 1,
      threadOutRef,
    },
  );
  const state: InputSetUniquenessStep02State =
    requireInputSetUniquenessStepState({
      threadUtxo,
      signer,
      schema: InputSetUniquenessStep02Datum,
      stepIndex: 1,
    });
  const badTxId = state.bad_tx_id;

  // Local twin of the validator's conviction predicate, before any planning.
  assertInputSetUniquenessClaimConvicts({
    claim,
    spendInputItemCbors,
    referenceInputItemCbors,
  });

  // The door's own pairing checks, run off-chain: the compact bytes must
  // re-derive to the thread's anchored id, and each supplied item list must
  // commit to the §2.5 slot it claims. The family's openings are §5.3
  // fixed-stride out-ref lists, so a typical committed input set lands on
  // tier-1 inline carriage; §8.4 partitions on the preimage's size alone, and
  // a genuinely large input set (over the 14,336-byte tier-1 bound — roughly
  // 358 forty-byte out-ref items) plans a tier-2 `RawUtxo` publication, which
  // is published below and consumed as a reference input.
  const planField = (fieldIndex: number, items: readonly string[]) =>
    planFaultProofFieldOpening({
      fieldIndex,
      anchorTxId: badTxId,
      nativeTxCompactCbor,
      itemCbors: items.map((item) => Buffer.from(item, "hex")),
      owner: signer.paymentKeyHash,
      label: STEP_LABEL,
    });
  let plannedSpend: FaultProofFieldOpeningPlan | undefined;
  let plannedReference: FaultProofFieldOpeningPlan | undefined;
  if (claim.kind !== "duplicateReferenceInputs") {
    plannedSpend = planField(
      MIDGARD_FIELD_INDEX.spendInputs,
      spendInputItemCbors,
    );
  }
  if (claim.kind !== "duplicateSpendInputs") {
    plannedReference = planField(
      MIDGARD_FIELD_INDEX.referenceInputs,
      referenceInputItemCbors,
    );
  }

  // §8.4: whatever the plans require published (a tier-2 `RawUtxo`
  // publication, tier-3 chunks) must exist before the step transaction
  // references it; tier-1 plans publish nothing and this loop is a no-op.
  // Publications share the prover wallet, so they run serially.
  const resolveCarriage = async ({
    fieldLabel,
    planned,
    supplied,
    certificate,
  }: {
    readonly fieldLabel: string;
    readonly planned: FaultProofFieldOpeningPlan | undefined;
    readonly supplied: readonly UTxO[] | undefined;
    readonly certificate: UTxO | undefined;
  }): Promise<readonly UTxO[]> => {
    if (planned === undefined) return [];
    const publications =
      supplied ??
      (planned.plan.publications.length === 0
        ? []
        : publishMissingCarriage
          ? await publishFaultProofFieldCarriage({
              lucid,
              signer,
              planned,
              publisherAddress: signer.address,
              label: `${STEP_LABEL} ${fieldLabel} field`,
            })
          : (() => {
              throw inputSetUniquenessSubmitError(
                `${STEP_LABEL} ${fieldLabel} requires authenticated pre-published carriage.`,
              );
            })());
    if (planned.plan.tier === "Certified" && certificate === undefined) {
      throw inputSetUniquenessSubmitError(
        `${STEP_LABEL} ${fieldLabel} requires its authenticated field-preimage certificate.`,
      );
    }
    return [
      ...publications,
      ...(certificate === undefined ? [] : [certificate]),
    ];
  };
  const spendCarriage = await resolveCarriage({
    fieldLabel: "spend-inputs",
    planned: plannedSpend,
    supplied: publishedSpendCarriageUtxos,
    certificate: spendCertificateUtxo,
  });
  const referenceCarriage = await resolveCarriage({
    fieldLabel: "reference-inputs",
    planned: plannedReference,
    supplied: publishedReferenceCarriageUtxos,
    certificate: referenceCertificateUtxo,
  });
  const published = [...spendCarriage, ...referenceCarriage];
  const stepReference =
    referenceScriptUtxo === undefined
      ? undefined
      : requireInputSetUniquenessReferenceScript({
          utxo: referenceScriptUtxo,
          expectedScriptHash: contracts.steps[1].spendingScriptHash,
          stepIndex: 1,
        });
  const stepCarriage = witnessSpendingValidatorCarriage({
    script: contracts.steps[1].spendingScript,
    referenceUtxo: stepReference,
    label: `${STEP_LABEL} spending validator`,
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${STEP_LABEL} computation-thread mint`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${STEP_LABEL} fraud-proof mint`,
  });
  // §8.7: positional carriage indices count into the ledger's canonically
  // sorted reference-input list, so the carriage resolvers must see the
  // transaction's complete reference-input set.
  const referenceInputs = uniqueUtxos([
    ...published,
    ...(unsafeSpendFieldRawUtxoForTest === undefined
      ? []
      : [unsafeSpendFieldRawUtxoForTest]),
    ...stepCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ]);

  const claimArgs = (
    layout: Step02SpendLayout,
    ctx: Parameters<BuildTxWithRedeemer>[0],
  ): InputSetUniquenessStep02Args => {
    const common = {
      input_index: layout.inputIndex,
      output_index: layout.outputIndex,
      fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
    };
    if (claim.kind === "duplicateSpendInputs") {
      if (plannedSpend === undefined) {
        throw inputSetUniquenessSubmitError(
          `${STEP_LABEL} did not plan the spend-inputs opening.`,
        );
      }
      return {
        DuplicateSpendInputs: {
          ...common,
          first_index: claim.firstIndex,
          second_index: claim.secondIndex,
          spend_inputs_opening:
            unsafeSpendFieldRawUtxoForTest === undefined
              ? faultProofFieldOpening({
                  planned: plannedSpend,
                  referenceInputs,
                  certificatePolicyId:
                    contracts.fieldPreimageCertificatePolicyId,
                  label: STEP_LABEL,
                })
              : fieldOpeningForField({
                  fieldIndex: MIDGARD_FIELD_INDEX.spendInputs,
                  nativeTxCompactCbor: plannedSpend.nativeTxCompactCbor,
                  carriage: {
                    RawUtxo: {
                      ref_input_index: requireReferenceInputIndex(
                        ctx,
                        unsafeSpendFieldRawUtxoForTest,
                        `${STEP_LABEL} substituted publication`,
                      ),
                    },
                  } satisfies FieldCarriage,
                }),
        },
      };
    }
    if (claim.kind === "duplicateReferenceInputs") {
      if (plannedReference === undefined) {
        throw inputSetUniquenessSubmitError(
          `${STEP_LABEL} did not plan the reference-inputs opening.`,
        );
      }
      return {
        DuplicateReferenceInputs: {
          ...common,
          first_index: claim.firstIndex,
          second_index: claim.secondIndex,
          reference_inputs_opening: faultProofFieldOpening({
            planned: plannedReference,
            referenceInputs,
            certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
            label: STEP_LABEL,
          }),
        },
      };
    }
    if (plannedSpend === undefined || plannedReference === undefined) {
      throw inputSetUniquenessSubmitError(
        `${STEP_LABEL} did not plan both field openings for the overlap claim.`,
      );
    }
    return {
      SpendReferenceOverlap: {
        ...common,
        spend_index: claim.spendIndex,
        reference_index: claim.referenceIndex,
        native_tx_compact_cbor: plannedSpend.nativeTxCompactCbor,
        spend_inputs_carriage: faultProofFieldCarriage({
          planned: plannedSpend,
          referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: STEP_LABEL,
        }),
        reference_inputs_carriage: faultProofFieldCarriage({
          planned: plannedReference,
          referenceInputs,
          certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
          label: STEP_LABEL,
        }),
      },
    };
  };

  signer.selectWallet(lucid);
  // A tier-2/3 publication sits at the prover's own address under a large
  // inline datum (and the min-ADA that goes with it), so it can top the fee
  // selector's descending-lovelace sort — it must not be spent by the very
  // transaction that references it.
  const feeInput = selectFeeInput(
    (await lucid.wallet().getUtxos()).filter(
      (utxo) => utxo.datum == null && utxo.datumHash == null,
    ),
  );
  const fraudProofUnit = toUnit(
    contracts.fraudProof.policyId,
    threadToken.assetName,
  );
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout: Step02SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const layout: Step02SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, STEP_LABEL),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        `${STEP_LABEL} fraud-proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        `${STEP_LABEL} fraud-proof`,
      ),
    };
    spendLayout = layout;
    return Data.to(
      { Continue: [claimArgs(layout, ctx)] },
      InputSetUniquenessStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      { Success: { burning_token_asset_name: threadToken.assetName } },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const fraudProofMintRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.fraudProof.policyId,
      `${STEP_LABEL} fraud-proof mint`,
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index:
          computationThreadMintRedeemerIndex,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .mintAssets({ [threadToken.unit]: -1n }, threadBurnRedeemer)
    .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .addSignerKey(signer.paymentKeyHash);
  const withReferences =
    referenceInputs.length === 0 ? base : base.readFrom([...referenceInputs]);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(stepCarriage.attach(withReferences)),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw inputSetUniquenessSubmitError(
      "BuildTxWithRedeemer did not resolve the step-02 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundary({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransaction({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof input-set-uniqueness step-02",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[1].spendingScript,
        },
        {
          role: "V1 fraud-proof computation-thread minting",
          utxo: witnessReferenceScripts?.computationThreadMint,
          expectedScript: contracts.computationThread.mintingScript,
        },
        {
          role: "V1 fraud-proof token minting",
          utxo: witnessReferenceScripts?.fraudProofMint,
          expectedScript: contracts.fraudProof.mintingScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw inputSetUniquenessSubmitError(
      `step-02 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    badTxId,
    claim,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex,
    ),
    awaitedConfirmation: awaitConfirmation,
  };
};
