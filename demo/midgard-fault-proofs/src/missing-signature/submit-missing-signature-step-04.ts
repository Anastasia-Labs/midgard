/**
 * `missing-signature` step-04 submitter (offchain plan §4.2, §5 frontier 3).
 *
 * Opens witness field 7 (`address_witnesses`) through the §8.8 door — the
 * `WitnessAnchor` arm, checked against the thread-anchored
 * `verified_witness_set_hash`, never a locally derived one — and walks it in
 * deterministic bounded batches. An interior batch self-loops step-04 with a
 * canonical checkpoint hash; the terminal batch proves absence, burns the
 * computation thread, mints the permanent fraud-proof token, and locks it at
 * the always-fails fraud-proof address.
 *
 * The absence predicate is re-run locally first with the exact twin: a preimage
 * in which the accused key IS present would be `NotAFault` (valid witness)
 * or `invalid-signature`'s fault (present-but-invalid, §7.3/D6) — either
 * way, refused here rather than burned on-chain.
 *
 * Field 7 is the family's fat field (~96 bytes per witness), so unlike
 * step-02 the tier is genuinely load-bearing: whatever the door's planner
 * picks, this submitter publishes and reads back.
 */
import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  type MidgardAddressWitness,
  MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE_V1,
  missingSignatureFieldWalkCheckpointV1,
  missingSignatureRequiredSignerIsPresentV1,
  MissingSignatureStep04Datum,
  MissingSignatureStep04SpendRedeemer,
  type MissingSignatureStep04State,
  type NativeTxWitnessSetCompact,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
  resolveMissingSignatureFieldWalkCheckpointV1,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type PreparedClaimRegistryMutationV1,
  prepareFamilyClaimRegistryMutationV1,
  requirePreparedClaimRegistryMutationV1,
} from "../claim-registry-transaction-v1.js";
import {
  faultProofFieldOpeningV1,
  publishFaultProofFieldCarriageV1,
} from "../field-opening-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "../runtime.js";
import { excludeUtxo } from "../spend-input-witness.js";
import { selectFeeInput } from "../submit-step-01.js";
import {
  computationThreadOutputPredicate,
  outputWithDatumAndUnitPredicate,
} from "../tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "../witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "../workflow/transaction-boundary-v1.js";
import type { MissingSignatureContractsV1 } from "./contracts-v1.js";
import { planMissingSignatureAddressWitnessesOpeningV1 } from "./evidence-v1.js";
import {
  missingSignatureStepLabelV1,
  missingSignatureSubmitError,
  requireMissingSignatureReferenceScriptV1,
  requireMissingSignatureStepStateV1,
  requireMissingSignatureThreadUtxoV1,
} from "./submit-common-v1.js";

const STEP_LABEL = missingSignatureStepLabelV1(3);

type SubmitMissingSignatureStep04CommonResult = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};

export type SubmitMissingSignatureStep04Result =
  | (SubmitMissingSignatureStep04CommonResult & {
      readonly kind: "advanced";
      readonly nextThreadOutRef: string;
      readonly nextItemIndex: number;
      readonly checkpointCbor: string;
      readonly checkpointHash: string;
    })
  | (SubmitMissingSignatureStep04CommonResult & {
      readonly kind: "proven";
      readonly fraudProofPolicyId: string;
      readonly fraudProofUnit: string;
      /** `txHash#index` of the permanent fraud-proof token UTxO. */
      readonly fraudProofOutRef: string;
      readonly fraudProofMintRedeemerIndex: number;
      readonly computationThreadMintRedeemerIndex: number;
    });

type Step04SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex?: bigint;
};

export const submitMissingSignatureStep04 = async ({
  lucid,
  contracts,
  categoryId,
  signer,
  threadOutRef,
  addrTxWits,
  nativeTxCompactCbor,
  witnessSetCompact,
  publishCarriage = false,
  certificateUtxo,
  referenceScriptUtxo,
  witnessReferenceScripts,
  claimRegistryMutation,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: MissingSignatureContractsV1;
  readonly categoryId: string;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  /** Complete positional address-witness list — field 7's §5.1 preimage. */
  readonly addrTxWits: readonly MidgardAddressWitness[];
  /** The accused transaction's §2.5 compact structure, as committed. */
  readonly nativeTxCompactCbor: string;
  /** That transaction's compact witness set — §2.5's other half. */
  readonly witnessSetCompact: NativeTxWitnessSetCompact;
  /** Force §8 tier-2 carriage publication (testing knob). */
  readonly publishCarriage?: boolean;
  /** Pre-minted §8.6 certificate when the planner selects tier 3. */
  readonly certificateUtxo?: UTxO;
  /** §2.3: the published step-04 reference script (required; never inline). */
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  readonly claimRegistryMutation?: PreparedClaimRegistryMutationV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitMissingSignatureStep04Result> => {
  const { threadUtxo, threadToken } = await requireMissingSignatureThreadUtxoV1(
    {
      lucid,
      contracts,
      categoryId,
      stepIndex: 3,
      threadOutRef,
    },
  );
  const state: MissingSignatureStep04State = requireMissingSignatureStepStateV1(
    {
      threadUtxo,
      signer,
      schema: MissingSignatureStep04Datum,
      stepIndex: 3,
    },
  );

  // The absence fold, locally first (§7.3, D6): a present key — valid or
  // not — is not this family's fault, and the validator would refuse it.
  if (
    missingSignatureRequiredSignerIsPresentV1({
      verificationKey: state.missing_required_signer_vkey,
      addrTxWits,
    })
  ) {
    throw missingSignatureSubmitError(
      `the accused verification key ${state.missing_required_signer_vkey} appears in the address-witness preimage — a present-but-invalid witness is invalid-signature's fault (Q15), and a valid one is no fault at all.`,
    );
  }

  // The §8.8 door: plan against the thread-anchored witness-set hash,
  // publish whatever the tier demands, open.
  const planned = planMissingSignatureAddressWitnessesOpeningV1({
    anchorTxId: state.verified_tx_id,
    nativeTxCompactCbor,
    addrTxWits,
    witnessSet: witnessSetCompact,
    anchorWitnessSetHash: state.verified_witness_set_hash,
    owner: signer.paymentKeyHash,
    publish: publishCarriage,
  });
  const checkpoint = resolveMissingSignatureFieldWalkCheckpointV1({
    txId: state.verified_tx_id,
    itemCount: planned.itemCount,
    totalLength: planned.preimage.length,
    committedHash: state.field_walk_checkpoint_hash,
  });
  const nextItemIndex = checkpoint?.nextItemIndex ?? 0;
  const remaining = planned.itemCount - nextItemIndex;
  const willFinalize =
    remaining <= MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE_V1;
  const nextCheckpoint = willFinalize
    ? null
    : missingSignatureFieldWalkCheckpointV1({
        txId: state.verified_tx_id,
        itemCount: planned.itemCount,
        totalLength: planned.preimage.length,
        nextItemIndex:
          nextItemIndex + MISSING_SIGNATURE_WITNESS_SCAN_BATCH_SIZE_V1,
      });
  signer.selectWallet(lucid);
  const carriageUtxos = await publishFaultProofFieldCarriageV1({
    lucid,
    signer,
    planned,
    publisherAddress: signer.address,
    label: `${STEP_LABEL} address-witnesses`,
  });
  const fieldReferenceInputs = [
    ...(certificateUtxo === undefined ? [] : [certificateUtxo]),
    ...carriageUtxos,
  ];
  const computationThreadBurnCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${STEP_LABEL} computation-thread burn`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${STEP_LABEL} fraud-proof mint`,
  });
  // The computation-thread mint runs on the finalize branch alone, so only
  // that branch carries the claim-registry mutation: an interior scan batch
  // mints nothing and must not close the claim.
  const resolvedClaimRegistryMutation = willFinalize
    ? requirePreparedClaimRegistryMutationV1({
        mutation:
          claimRegistryMutation ??
          (await prepareFamilyClaimRegistryMutationV1({
            lucid,
            claimRegistry: contracts.claimRegistry,
            claimRegistryReferenceUtxo:
              witnessReferenceScripts?.claimRegistrySpend,
            hubOraclePolicyId: contracts.hubOraclePolicyId,
            computationThreadPolicyId: contracts.computationThread.policyId,
            claimId: threadToken.assetName,
            kind: "close",
          })),
        kind: "close",
        claimId: threadToken.assetName,
        label: STEP_LABEL,
      })
    : undefined;
  // The mint witnesses execute on the finalize branch alone; a scan batch
  // must not reference them, or the §8.7 opening indices would drift.
  const referenceInputs = [
    ...fieldReferenceInputs,
    requireMissingSignatureReferenceScriptV1({
      utxo: referenceScriptUtxo,
      expectedScriptHash: contracts.steps[3].spendingScriptHash,
      stepIndex: 3,
    }),
    ...(willFinalize
      ? [
          ...computationThreadBurnCarriage.referenceInputs,
          ...fraudProofMintCarriage.referenceInputs,
          ...(resolvedClaimRegistryMutation?.referenceInputs ?? []),
        ]
      : []),
  ];
  const addrTxWitsOpening = faultProofFieldOpeningV1({
    planned,
    // §8.7 indices are into the complete reference-input set, including the
    // step's own reference script.
    referenceInputs,
    certificatePolicyId: contracts.fieldPreimageCertificatePolicyId,
    label: `${STEP_LABEL} address-witnesses`,
  });

  const walletUtxos = await lucid.wallet().getUtxos();
  const walletUtxosSansCarriage = [
    ...carriageUtxos,
    ...(resolvedClaimRegistryMutation?.referenceInputs ?? []),
  ].reduce<readonly UTxO[]>(
    (candidates, utxo) => excludeUtxo(candidates, utxo),
    walletUtxos,
  );
  const feeInput = selectFeeInput(walletUtxosSansCarriage);
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
  const fraudProofOutputMatches = outputWithDatumAndUnitPredicate({
    address: contracts.fraudProof.spendingScriptAddress,
    datum: fraudProofDatum,
    unit: fraudProofUnit,
  });
  let spendLayout: Step04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const nextState: MissingSignatureStep04State | null =
    nextCheckpoint === null
      ? null
      : {
          ...state,
          field_walk_checkpoint_hash: nextCheckpoint.checkpointHash,
        };
  const nextDatum =
    nextState === null
      ? null
      : Data.to(
          { fraud_prover: signer.paymentKeyHash, data: nextState },
          MissingSignatureStep04Datum,
        );
  const nextOutputMatches =
    nextDatum === null
      ? null
      : computationThreadOutputPredicate({
          address: contracts.steps[3].spendingScriptAddress,
          datum: nextDatum,
          unit: threadToken.unit,
        });

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, STEP_LABEL);
    const inputIndex = requireInputIndex(ctx, threadUtxo, STEP_LABEL);
    const layout: Step04SpendLayout = willFinalize
      ? {
          inputIndex,
          outputIndex: requireUniqueOutputIndex(
            ctx.outputs,
            fraudProofOutputMatches,
            `${STEP_LABEL} fraud-proof output`,
          ),
          fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
            ctx,
            contracts.fraudProof.policyId,
            `${STEP_LABEL} fraud-proof mint`,
          ),
        }
      : {
          inputIndex,
          outputIndex: requireUniqueOutputIndex(
            ctx.outputs,
            nextOutputMatches!,
            `${STEP_LABEL} scan output`,
          ),
        };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          willFinalize
            ? {
                Finalize: {
                  input_index: layout.inputIndex,
                  output_index: layout.outputIndex,
                  fraud_proof_mint_redeemer_index:
                    layout.fraudProofMintRedeemerIndex!,
                  addr_tx_wits_opening: addrTxWitsOpening,
                  checkpoint_cbor: checkpoint?.checkpointCbor ?? null,
                },
              }
            : {
                Scan: {
                  input_index: layout.inputIndex,
                  output_index: layout.outputIndex,
                  addr_tx_wits_opening: addrTxWitsOpening,
                  checkpoint_cbor: checkpoint?.checkpointCbor ?? null,
                },
              },
        ],
      },
      MissingSignatureStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
  const computationThreadBurnRedeemer = ((ctx) => {
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
    const index = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      `${STEP_LABEL} computation-thread burn`,
    );
    computationThreadMintRedeemerIndex = index;
    return Data.to(
      {
        computation_thread_token_asset_name: threadToken.assetName,
        computation_thread_mint_redeemer_index: index,
      },
      FraudProofTokenMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const withInputs = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer);
  const withReferences =
    referenceInputs.length === 0
      ? withInputs
      : withInputs.readFrom(referenceInputs);
  const completed = willFinalize
    ? fraudProofMintCarriage.attach(
        computationThreadBurnCarriage.attach(
          resolvedClaimRegistryMutation!.apply(
            withReferences
              .mintAssets(
                { [threadToken.unit]: -1n },
                computationThreadBurnRedeemer,
              )
              .mintAssets({ [fraudProofUnit]: 1n }, fraudProofMintRedeemer)
              .pay.ToContract(
                contracts.fraudProof.spendingScriptAddress,
                { kind: "inline", value: fraudProofDatum },
                fraudProofAssets,
              )
              .addSignerKey(signer.paymentKeyHash),
          ),
        ),
      )
    : withReferences.pay
        .ToContract(
          contracts.steps[3].spendingScriptAddress,
          { kind: "inline", value: nextDatum! },
          {
            lovelace: threadUtxo.assets.lovelace ?? 0n,
            [threadToken.unit]: 1n,
          },
        )
        .addSignerKey(signer.paymentKeyHash);
  const unsigned = await completed.complete({
    localUPLCEval: true,
    ...(carriageUtxos.length === 0
      ? {}
      : { presetWalletInputs: walletUtxosSansCarriage as UTxO[] }),
  });
  if (
    spendLayout === undefined ||
    (willFinalize && computationThreadMintRedeemerIndex === undefined)
  ) {
    throw missingSignatureSubmitError(
      "BuildTxWithRedeemer did not resolve the step-04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof missing-signature step-04",
          utxo: referenceScriptUtxo,
          expectedScript: contracts.steps[3].spendingScript,
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
        ...(resolvedClaimRegistryMutation === undefined
          ? []
          : [
              {
                role: "claim-registry spending",
                utxo: resolvedClaimRegistryMutation.referenceScriptUtxo,
                expectedScript: resolvedClaimRegistryMutation.registryScript,
              },
            ]),
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw missingSignatureSubmitError(
      `step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
    );
  }
  if (awaitConfirmation) {
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }

  const common = {
    txHash,
    walletSource: signer.source,
    proverAddress: signer.address,
    fraudProver: signer.paymentKeyHash,
    threadOutRef,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
    computationThreadUnit: threadToken.unit,
    inputIndex: Number(spendLayout.inputIndex),
    outputIndex: Number(spendLayout.outputIndex),
    awaitedConfirmation: awaitConfirmation,
  };
  if (!willFinalize) {
    return {
      ...common,
      kind: "advanced",
      nextThreadOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
      nextItemIndex: nextCheckpoint!.nextItemIndex,
      checkpointCbor: nextCheckpoint!.checkpointCbor,
      checkpointHash: nextCheckpoint!.checkpointHash,
    };
  }
  return {
    ...common,
    kind: "proven",
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofUnit,
    fraudProofOutRef: `${txHash}#${spendLayout.outputIndex.toString()}`,
    fraudProofMintRedeemerIndex: Number(
      spendLayout.fraudProofMintRedeemerIndex!,
    ),
    computationThreadMintRedeemerIndex: Number(
      computationThreadMintRedeemerIndex!,
    ),
  };
};
