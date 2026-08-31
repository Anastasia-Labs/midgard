/**
 * `fabricated-deposit` step-04 submitter (Goal task `Q39`, §9.1 output 8).
 *
 * Finalizes the thread into a permanent fraud-proof token: burns the computation
 * thread, mints the fraud-proof token under the *same* asset name and locks it at
 * the always-fails fraud-proof address.
 *
 * Because the mint is permanent, both of the validator's own last-chance checks
 * are re-run locally first, and both refuse rather than repair:
 *
 * - **identity** — the thread token's asset name must be this family's category id
 *   followed by the challenged header hash the carried state names, so a
 *   conviction cannot be filed against the wrong block; and
 * - **establishment** — `isFabricatedDepositFaultV1`, the twin of
 *   `fabricated_deposit_fault_is_established_v1`, must hold for the carried
 *   state, so a stale or content-identical "fault" is never made permanent.
 */
import {
  FabricatedDepositStep04Datum,
  FabricatedDepositStep04SpendRedeemer,
  type FabricatedDepositStep04State,
  FraudProofComputationThreadRedeemer,
  FraudProofTokenDatum,
  FraudProofTokenMintRedeemer,
  isFabricatedDepositFaultV1,
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
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type PreparedClaimRegistryMutationV1,
  prepareFamilyClaimRegistryMutationV1,
  requirePreparedClaimRegistryMutationV1,
} from "./claim-registry-transaction-v1.js";
import { requireFabricatedReferenceScriptV1 } from "./fabricated-reference-script-v1.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  outRefLabel,
  parseOutRef,
  type ResolvedProverSigner,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { excludeUtxo } from "./spend-input-witness.js";
import {
  FABRICATED_DEPOSIT_CATEGORY_LABEL,
  type FabricatedDepositContractsV1,
} from "./submit-fabricated-deposit-step-01.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "./submit-step-01.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";
import {
  type FaultProofWitnessReferenceScriptsV1,
  witnessMintingPolicyCarriageV1,
} from "./witness-reference-scripts-v1.js";
import {
  type FraudProofPreSubmitBoundaryV1,
  reachFraudProofPreSubmitBoundaryV1,
  workflowReferenceScriptsUsedByTransactionV1,
} from "./workflow/transaction-boundary-v1.js";

export const requireFabricatedDepositStep04Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): FabricatedDepositStep04State => {
  if (threadUtxo.datum === null || threadUtxo.datum === undefined) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} has no inline fabricated-deposit step-04 datum.`,
    );
  }
  const datum = Data.from(threadUtxo.datum, FabricatedDepositStep04Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} belongs to fraud prover ${datum.fraud_prover}, not ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} carries no fabricated-deposit step-04 state.`,
    );
  }
  return datum.data;
};

/**
 * Twin of the step-04 validator's two last-chance checks. Throws — fail-closed —
 * when the carried state is filed against a different header than the thread
 * token names, or when the carried fault is not an established
 * fabricated-deposit fault.
 */
export const assertFabricatedDepositStep04FinalizableV1 = ({
  state,
  fraudulentHeaderHash,
}: {
  readonly state: FabricatedDepositStep04State;
  readonly fraudulentHeaderHash: string;
}): void => {
  if (state.challenged_header_hash !== fraudulentHeaderHash) {
    throw new Error(
      `Fabricated-deposit step 04 refuses to finalize: thread state names challenged header ${state.challenged_header_hash}, but the thread token names ${fraudulentHeaderHash}.`,
    );
  }
  if (!isFabricatedDepositFaultV1(state)) {
    throw new Error(
      "Fabricated-deposit step 04 refuses to finalize: the carried fault is not an established fabricated-deposit fault (identical content commitments, or an event outside the challenged block's window).",
    );
  }
};

export type SubmitFabricatedDepositStep04CliConfig = SubmitProviderConfig & {
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitFabricatedDepositStep04Result = {
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
  readonly fault: FabricatedDepositStep04State["fault"];
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type FabricatedDepositStep04SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export const submitFabricatedDepositStep04 = async ({
  lucid,
  contracts,
  signer,
  threadOutRef,
  referenceScriptUtxo,
  witnessReferenceScripts,
  claimRegistryMutation,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: FabricatedDepositContractsV1;
  readonly signer: ResolvedProverSigner;
  readonly threadOutRef: string;
  readonly referenceScriptUtxo: UTxO;
  /** Required published witness reference scripts for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScriptsV1;
  /** Exact live claim-registry Close resolved after the confirmed Open. */
  readonly claimRegistryMutation?: PreparedClaimRegistryMutationV1;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundaryV1;
  readonly awaitConfirmation?: boolean;
}): Promise<SubmitFabricatedDepositStep04Result> => {
  const threadUtxo = await fetchUtxoByOutRef({
    lucid,
    outRef: parseOutRef(threadOutRef, "--thread-out-ref"),
    label: "fabricated-deposit step-04 computation-thread UTxO",
  });
  if (threadUtxo.address !== contracts.steps[3].spendingScriptAddress) {
    throw new Error(
      `Thread UTxO ${outRefLabel(threadUtxo)} is not locked at fabricated-deposit step 04.`,
    );
  }
  const threadToken = requireComputationThreadToken({
    utxo: threadUtxo,
    computationThreadPolicyId: contracts.computationThread.policyId,
    categoryId: contracts.categoryId,
    categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
  });
  const closeMutation = requirePreparedClaimRegistryMutationV1({
    mutation:
      claimRegistryMutation ??
      (await prepareFamilyClaimRegistryMutationV1({
        lucid,
        claimRegistry: contracts.claimRegistry,
        claimRegistryReferenceUtxo: witnessReferenceScripts?.claimRegistrySpend,
        hubOraclePolicyId: contracts.hubOraclePolicyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        claimId: threadToken.assetName,
        kind: "close",
      })),
    kind: "close",
    claimId: threadToken.assetName,
    label: "fabricated-deposit step 04",
  });
  const state = requireFabricatedDepositStep04Datum({ threadUtxo, signer });
  assertFabricatedDepositStep04FinalizableV1({
    state,
    fraudulentHeaderHash: threadToken.fraudulentHeaderHash,
  });

  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(
    closeMutation.referenceInputs.reduce<readonly UTxO[]>(
      (utxos, reference) => excludeUtxo(utxos, reference),
      await lucid.wallet().getUtxos(),
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
  let spendLayout: FabricatedDepositStep04SpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;

  const spendRedeemer = ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "fabricated-deposit step 04");
    const layout: FabricatedDepositStep04SpendLayout = {
      inputIndex: requireInputIndex(
        ctx,
        threadUtxo,
        "fabricated-deposit step 04",
      ),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputMatches,
        "fabricated-deposit step 04 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        contracts.fraudProof.policyId,
        "fabricated-deposit step 04 fraud-proof",
      ),
    };
    spendLayout = layout;
    return Data.to(
      {
        Continue: [
          {
            input_index: layout.inputIndex,
            output_index: layout.outputIndex,
            fraud_proof_mint_redeemer_index: layout.fraudProofMintRedeemerIndex,
          },
        ],
      },
      FabricatedDepositStep04SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

  const threadBurnRedeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      contracts.computationThread.policyId,
      "fabricated-deposit step 04 computation-thread burn",
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
      "fabricated-deposit step 04 fraud-proof mint",
    );
    computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      contracts.computationThread.policyId,
      "fabricated-deposit step 04 computation-thread burn",
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

  const computationThreadMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: "fabricated-deposit step 04 computation-thread mint",
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriageV1({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: "fabricated-deposit step 04 fraud-proof mint",
  });
  const base = lucid
    .newTx()
    .collectFrom([feeInput])
    .collectFrom([threadUtxo], spendRedeemer)
    .readFrom([
      requireFabricatedReferenceScriptV1({
        utxo: referenceScriptUtxo,
        expectedScriptHash: contracts.steps[3].spendingScriptHash,
        categoryLabel: FABRICATED_DEPOSIT_CATEGORY_LABEL,
        stepIndex: 3,
      }),
      ...computationThreadMintCarriage.referenceInputs,
      ...fraudProofMintCarriage.referenceInputs,
    ])
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
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(closeMutation.apply(base)),
  );

  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    spendLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(
      "BuildTxWithRedeemer did not resolve fabricated-deposit step 04 layout.",
    );
  }
  const signed = await unsigned.sign.withWallet().complete();
  const expectedTxHash = await reachFraudProofPreSubmitBoundaryV1({
    signed,
    referenceScripts: workflowReferenceScriptsUsedByTransactionV1({
      signed,
      candidates: [
        {
          role: "V1 fraud-proof fabricated-deposit step-04",
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
        {
          role: "claim-registry spending",
          utxo: closeMutation.referenceScriptUtxo,
          expectedScript: closeMutation.registryScript,
        },
      ],
    }),
    boundary: preSubmitBoundary,
  });
  const txHash = await signed.submit();
  if (txHash !== expectedTxHash) {
    throw new Error(
      `fabricated-deposit step-04 provider returned ${txHash}, expected ${expectedTxHash}.`,
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
    computationThreadPolicyId: contracts.computationThread.policyId,
    computationThreadAssetName: threadToken.assetName,
    computationThreadUnit: threadToken.unit,
    fraudProofPolicyId: contracts.fraudProof.policyId,
    fraudProofAssetName: threadToken.assetName,
    fraudProofUnit,
    fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
    fourthStepAddress: contracts.steps[3].spendingScriptAddress,
    fault: state.fault,
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

export const submitFabricatedDepositStep04FromFiles = async (
  config: SubmitFabricatedDepositStep04CliConfig & {
    readonly contracts: FabricatedDepositContractsV1;
    readonly referenceScriptUtxo: UTxO;
  },
): Promise<SubmitFabricatedDepositStep04Result> => {
  const lucid = await makeLucidForSubmit(config);
  const signer = resolveProverSigner(config);
  return await submitFabricatedDepositStep04({
    lucid,
    contracts: config.contracts,
    signer,
    threadOutRef: config.threadOutRef,
    referenceScriptUtxo: config.referenceScriptUtxo,
    awaitConfirmation: config.awaitConfirmation,
  });
};
