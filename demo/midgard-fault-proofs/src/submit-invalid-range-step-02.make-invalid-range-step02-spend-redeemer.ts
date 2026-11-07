import {
  FraudProofComputationThreadRedeemer,
  FraudProofTokenMintRedeemer,
  InvalidRangeStep02Datum,
  InvalidRangeStep02SpendRedeemer,
  invalidRangeViolationReason,
  type NormalizedTimeRange,
  requireInputIndex,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  requireUniqueOutputIndex,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  Data,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  outRefLabel,
  type ResolvedProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import { outputWithDatumAndUnitPredicate } from "./tx-layout.js";

export type SubmitInvalidRangeStep02CliConfig = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly walletSeedPhrase?: string;
  readonly walletSeedPhraseEnv?: string;
  readonly walletPrivateKey?: string;
  readonly walletPrivateKeyEnv?: string;
  readonly threadOutRef: string;
  readonly awaitConfirmation?: boolean;
};

export type SubmitInvalidRangeStep02Result = {
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
  readonly secondStepAddress: string;
  readonly blockSlot: bigint;
  readonly normalizedValidityRange: NormalizedTimeRange;
  readonly violationReason: NonNullable<
    ReturnType<typeof invalidRangeViolationReason>
  >;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly computationThreadMintRedeemerIndex: number;
  readonly fraudProofMintRedeemerIndex: number;
  readonly awaitedConfirmation: boolean;
};

type InvalidRangeStep02DatumWithState = InvalidRangeStep02Datum & {
  readonly data: NonNullable<InvalidRangeStep02Datum["data"]>;
};

export type InvalidRangeStep02ResolvedLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly computationThreadMintRedeemerIndex: bigint;
  readonly fraudProofMintRedeemerIndex: bigint;
};

export type InvalidRangeStep02SpendLayout = Omit<
  InvalidRangeStep02ResolvedLayout,
  "computationThreadMintRedeemerIndex"
>;

export const requireStep02Datum = ({
  threadUtxo,
  signer,
}: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
}): InvalidRangeStep02DatumWithState => {
  if (threadUtxo.datum == null) {
    throw new Error(`Thread UTxO ${outRefLabel(threadUtxo)} is missing datum.`);
  }
  const datum = Data.from(threadUtxo.datum, InvalidRangeStep02Datum);
  if (datum.fraud_prover !== signer.paymentKeyHash) {
    throw new Error(
      `Thread UTxO fraud_prover ${datum.fraud_prover} does not match prover signer ${signer.paymentKeyHash}.`,
    );
  }
  if (datum.data === null) {
    throw new Error(
      "Invalid-range step 02 input datum must carry verified validity-range state data.",
    );
  }
  return datum as InvalidRangeStep02DatumWithState;
};

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

export const makeInvalidRangeStep02SpendRedeemer = ({
  threadUtxo,
  fraudProofAddress,
  fraudProofPolicyId,
  fraudProofUnit,
  fraudProofDatum,
  onLayout,
}: {
  readonly threadUtxo: UTxO;
  readonly fraudProofAddress: string;
  readonly fraudProofPolicyId: string;
  readonly fraudProofUnit: string;
  readonly fraudProofDatum: string;
  readonly onLayout: (layout: InvalidRangeStep02SpendLayout) => void;
}): BuildTxWithRedeemer =>
  ((ctx) => {
    requireOwnSpendPurpose(ctx, threadUtxo, "invalid-range step 02");
    const layout: InvalidRangeStep02SpendLayout = {
      inputIndex: requireInputIndex(ctx, threadUtxo, "invalid-range step 02"),
      outputIndex: requireUniqueOutputIndex(
        ctx.outputs,
        fraudProofOutputPredicate({
          fraudProofAddress,
          fraudProofUnit,
          fraudProofDatum,
        }),
        "invalid-range step 02 fraud-proof",
      ),
      fraudProofMintRedeemerIndex: requireMintRedeemerIndex(
        ctx,
        fraudProofPolicyId,
        "invalid-range step 02 fraud-proof",
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
          },
        ],
      },
      InvalidRangeStep02SpendRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;

export const makeFraudProofMintRedeemer = ({
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
    requireOwnMintPurpose(
      ctx,
      fraudProofPolicyId,
      "invalid-range step 02 fraud-proof mint",
    );
    const computationThreadMintRedeemerIndex = requireMintRedeemerIndex(
      ctx,
      computationThreadPolicyId,
      "invalid-range step 02 computation-thread burn",
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

export const makeComputationThreadSuccessRedeemer = ({
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
      "invalid-range step 02 computation-thread burn",
    );
    return Data.to(
      {
        Success: { burning_token_asset_name: computationThreadAssetName },
      },
      FraudProofComputationThreadRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
