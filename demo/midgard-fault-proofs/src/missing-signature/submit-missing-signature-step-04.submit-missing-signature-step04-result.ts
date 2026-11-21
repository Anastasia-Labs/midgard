import type {
  LocallyEvaluatedTransaction,
  OutsideWorkflowSubmission,
} from "../workflow/transaction-boundary.js";
import { missingSignatureStepLabel } from "./submit-common.js";

export const STEP_LABEL = missingSignatureStepLabel(3);

type MissingSignatureStep04CommonBuild = {
  readonly transaction: LocallyEvaluatedTransaction;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly inputIndex: number;
  readonly outputIndex: number;
};

export type MissingSignatureStep04Build =
  | (MissingSignatureStep04CommonBuild & {
      readonly kind: "advanced";
      readonly nextThreadOutRef: string;
      readonly nextItemIndex: number;
      readonly checkpointCbor: string;
      readonly checkpointHash: string;
    })
  | (MissingSignatureStep04CommonBuild & {
      readonly kind: "proven";
      readonly fraudProofPolicyId: string;
      readonly fraudProofUnit: string;
      /** `txHash#index` of the permanent fraud-proof token UTxO. */
      readonly fraudProofOutRef: string;
      readonly fraudProofMintRedeemerIndex: number;
      readonly computationThreadMintRedeemerIndex: number;
    });

export type SubmitMissingSignatureStep04Result =
  OutsideWorkflowSubmission<MissingSignatureStep04Build>;

export type Step04SpendLayout = {
  readonly inputIndex: bigint;
  readonly outputIndex: bigint;
  readonly fraudProofMintRedeemerIndex?: bigint;
};
