import { missingSignatureStepLabel } from "./submit-common.js";

export const STEP_LABEL = missingSignatureStepLabel(3);

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

export type Step04SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex?: bigint;
};
