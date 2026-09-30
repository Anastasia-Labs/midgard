import {
  FraudProofTokenDatum,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnSpendPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type LucidEvolution,
  type Network,
  type Script,
  toUnit,
  type TxSigned,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  DEFAULT_CONFIRMATION_POLL_MS,
  outRefLabel,
  type ResolvedProverSigner,
  resolveValidationTraceDisputeDeploymentContracts,
} from "../../runtime.js";
import {
  requireComputationThreadToken,
  selectFeeInput,
} from "../../step-support.js";
import { outputWithDatumAndUnitPredicate } from "../../tx-layout.js";
import {
  type FaultProofWitnessReferenceScripts,
  witnessMintingPolicyCarriage,
  witnessSpendingValidatorCarriage,
} from "../../witness-reference-scripts.js";
import { type FraudProofPreSubmitBoundary } from "../../workflow/transaction-boundary.js";
import {
  type FinalizeLayout,
  makeComputationThreadSuccessRedeemer,
  makeFraudProofMintRedeemer,
} from "./redeemers.js";
import { type ValidationFinalizationResult } from "./semantic-redeemers.make-semantic-resolution-redeemer.js";
import { requireL1ProofEnvelope } from "./transaction-material.js";
import {
  reachOptionalPreSubmitBoundary,
  type ValidationDisputeValidityRange,
} from "./validity.js";

type ValidationFinalizingSpendLayout = Omit<
  FinalizeLayout,
  "computationThreadMintRedeemerIndex"
> & {
  /** Supplied order is semantic (root order); values are canonical tx indices. */
  readonly materialReferenceInputIndices: readonly bigint[];
};

const makeValidationFinalizingSpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  materialReferenceUtxos,
  label,
  encodeRedeemer,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly materialReferenceUtxos: readonly UTxO[];
  readonly label: string;
  readonly encodeRedeemer: (layout: ValidationFinalizingSpendLayout) => string;
  readonly onLayout: (layout: ValidationFinalizingSpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, label);
    const layout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, label),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        outputWithDatumAndUnitPredicate({
          address: fraudProofAddress,
          datum: fraudProofDatum,
          unit: fraudProofUnit,
        }),
        `${label} fraud proof`,
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        `${label} fraud-proof mint`,
      ),
      materialReferenceInputIndices: materialReferenceUtxos.map((utxo) =>
        requireReferenceInputIndex(ctx, utxo, `${label} CEK material`),
      ),
    };
    onLayout(layout);
    return encodeRedeemer(layout);
  }) satisfies BuildTxWithRedeemer;

type ValidationFinalizationTransactionParams = {
  readonly lucid: LucidEvolution;
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly contracts: Awaited<
    ReturnType<typeof resolveValidationTraceDisputeDeploymentContracts>
  >["contracts"];
  readonly signer: ResolvedProverSigner;
  readonly threadUtxo: UTxO;
  readonly threadOutRef: string;
  readonly token: ReturnType<typeof requireComputationThreadToken>;
  readonly spendingScript: {
    readonly spendingScript: Script;
  };
  /**
   * Published authenticated reference-script UTxO carrying the spending
   * validator. When present the transaction consumes the validator through
   * `readFrom` and must not embed the validator body inside the L1 proof
   * envelope.
   */
  readonly spendingScriptReferenceUtxo?: UTxO;
  /** Required published shared minting witnesses for this transaction. */
  readonly witnessReferenceScripts?: FaultProofWitnessReferenceScripts;
  readonly spendLabel: string;
  readonly encodeSpendRedeemer: (
    layout: ValidationFinalizingSpendLayout,
  ) => string;
  readonly materialReferenceUtxos?: readonly UTxO[];
  readonly validityRange: ValidationDisputeValidityRange;
  /** Optional Q51 pre-submit boundary (workflow ruling R5). */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
};

type PreparedValidationFinalizationTransaction = {
  readonly lucid: LucidEvolution;
  readonly signed: TxSigned;
  readonly threadOutRef: string;
  readonly fraudProofUnit: string;
  readonly layout: FinalizeLayout;
  readonly materialReferenceInputOutRefs: readonly string[];
  readonly materialReferenceInputIndices: readonly number[];
};

const prepareValidationFinalizationTransaction = async ({
  lucid,
  contracts,
  signer,
  threadUtxo,
  threadOutRef,
  token,
  spendingScript,
  spendingScriptReferenceUtxo,
  witnessReferenceScripts,
  spendLabel,
  encodeSpendRedeemer,
  materialReferenceUtxos = [],
  validityRange,
}: ValidationFinalizationTransactionParams): Promise<PreparedValidationFinalizationTransaction> => {
  const fraudProofUnit = toUnit(contracts.fraudProof.policyId, token.assetName);
  const fraudProofDatum = Data.to(
    { fraud_prover: signer.paymentKeyHash },
    FraudProofTokenDatum,
  );
  let partialLayout: ValidationFinalizingSpendLayout | undefined;
  let computationThreadMintRedeemerIndex: bigint | undefined;
  signer.selectWallet(lucid);
  const feeInput = selectFeeInput(await lucid.wallet().getUtxos());
  const materialOutRefs = materialReferenceUtxos.map(outRefLabel);
  if (new Set(materialOutRefs).size !== materialOutRefs.length) {
    throw new Error(`${spendLabel} CEK material references must be unique`);
  }
  const spendingScriptCarriage = witnessSpendingValidatorCarriage({
    script: spendingScript.spendingScript,
    referenceUtxo: spendingScriptReferenceUtxo,
    label: `${spendLabel} spending validator`,
  });
  const computationThreadMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.computationThread.mintingScript,
    referenceUtxo: witnessReferenceScripts?.computationThreadMint,
    label: `${spendLabel} computation-thread mint`,
  });
  const fraudProofMintCarriage = witnessMintingPolicyCarriage({
    script: contracts.fraudProof.mintingScript,
    referenceUtxo: witnessReferenceScripts?.fraudProofMint,
    label: `${spendLabel} fraud-proof mint`,
  });
  const referenceInputs = [
    ...materialReferenceUtxos,
    ...spendingScriptCarriage.referenceInputs,
    ...computationThreadMintCarriage.referenceInputs,
    ...fraudProofMintCarriage.referenceInputs,
  ];
  const referenceOutRefs = referenceInputs.map(outRefLabel);
  if (new Set(referenceOutRefs).size !== referenceOutRefs.length) {
    throw new Error(`${spendLabel} reference inputs must be unique`);
  }
  let withReferenceInputs = lucid.newTx().collectFrom([feeInput]);
  if (referenceInputs.length > 0) {
    withReferenceInputs = withReferenceInputs.readFrom(referenceInputs);
  }
  const base = withReferenceInputs
    .collectFrom(
      [threadUtxo],
      makeValidationFinalizingSpendRedeemer({
        threadUtxo,
        fraudProofAddress: contracts.fraudProof.spendingScriptAddress,
        fraudProofPolicyId: contracts.fraudProof.policyId,
        fraudProofUnit,
        fraudProofDatum,
        materialReferenceUtxos,
        label: spendLabel,
        encodeRedeemer: encodeSpendRedeemer,
        onLayout: (layout) => {
          partialLayout = layout;
        },
      }),
    )
    .mintAssets(
      { [token.unit]: -1n },
      makeComputationThreadSuccessRedeemer({
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
      }),
    )
    .mintAssets(
      { [fraudProofUnit]: 1n },
      makeFraudProofMintRedeemer({
        fraudProofPolicyId: contracts.fraudProof.policyId,
        computationThreadPolicyId: contracts.computationThread.policyId,
        computationThreadAssetName: token.assetName,
        onComputationThreadMintRedeemerIndex: (index) => {
          computationThreadMintRedeemerIndex = index;
        },
      }),
    )
    .pay.ToContract(
      contracts.fraudProof.spendingScriptAddress,
      { kind: "inline", value: fraudProofDatum },
      {
        lovelace: threadUtxo.assets.lovelace ?? 0n,
        [fraudProofUnit]: 1n,
      },
    )
    .validFrom(validityRange.validFrom)
    .validTo(validityRange.validTo);
  const tx = fraudProofMintCarriage.attach(
    computationThreadMintCarriage.attach(spendingScriptCarriage.attach(base)),
  );
  const unsigned = await tx.complete({ localUPLCEval: true });
  if (
    partialLayout === undefined ||
    computationThreadMintRedeemerIndex === undefined
  ) {
    throw new Error(`BuildTxWithRedeemer did not resolve ${spendLabel} layout`);
  }
  const layout: FinalizeLayout = {
    ...partialLayout,
    computationThreadMintRedeemerIndex,
  };
  const signed = await unsigned.sign.withWallet().complete();
  requireL1ProofEnvelope(signed.toCBOR(), spendLabel);
  return {
    lucid,
    signed,
    threadOutRef,
    fraudProofUnit,
    layout,
    materialReferenceInputOutRefs: materialOutRefs,
    materialReferenceInputIndices:
      partialLayout.materialReferenceInputIndices.map(Number),
  };
};

const submitPreparedValidationFinalizationTransaction = async ({
  prepared,
  awaitConfirmation,
  preSubmitBoundary,
  referenceScriptCandidates,
}: {
  readonly prepared: PreparedValidationFinalizationTransaction;
  readonly awaitConfirmation: boolean;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly referenceScriptCandidates?: readonly {
    readonly role: string;
    readonly utxo: UTxO | undefined;
    readonly expectedScript?: Script;
  }[];
}): Promise<ValidationFinalizationResult> => {
  await reachOptionalPreSubmitBoundary({
    signed: prepared.signed,
    boundary: preSubmitBoundary,
    referenceScriptCandidates,
  });
  const txHash = await prepared.signed.submit();
  if (awaitConfirmation) {
    await prepared.lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
  }
  return {
    txHash,
    threadOutRef: prepared.threadOutRef,
    fraudProofOutRef: `${txHash}#${prepared.layout.outputIndex.toString()}`,
    fraudProofUnit: prepared.fraudProofUnit,
    inputIndex: Number(prepared.layout.inputIndex),
    outputIndex: Number(prepared.layout.outputIndex),
    computationThreadMintRedeemerIndex: Number(
      prepared.layout.computationThreadMintRedeemerIndex,
    ),
    fraudProofMintRedeemerIndex: Number(
      prepared.layout.fraudProofMintRedeemerIndex,
    ),
    materialReferenceInputOutRefs: prepared.materialReferenceInputOutRefs,
    materialReferenceInputIndices: prepared.materialReferenceInputIndices,
    awaitedConfirmation: awaitConfirmation,
  };
};

export const submitValidationFinalizationTransaction = async (
  params: ValidationFinalizationTransactionParams & {
    readonly awaitConfirmation: boolean;
  },
): Promise<ValidationFinalizationResult> =>
  submitPreparedValidationFinalizationTransaction({
    prepared: await prepareValidationFinalizationTransaction(params),
    awaitConfirmation: params.awaitConfirmation,
    preSubmitBoundary: params.preSubmitBoundary,
    referenceScriptCandidates: [
      {
        role: `${params.spendLabel} spending validator`,
        utxo: params.spendingScriptReferenceUtxo,
      },
      {
        role: "V1 fraud-proof computation-thread minting",
        utxo: params.witnessReferenceScripts?.computationThreadMint,
      },
      {
        role: "V1 fraud-proof token minting",
        utxo: params.witnessReferenceScripts?.fraudProofMint,
      },
    ],
  });
