import type { MintAuthorizationStep03State } from "@al-ft/midgard-sdk";

import { mintAuthorizationStepLabel } from "./submit-common.js";

export const STEP_LABEL = mintAuthorizationStepLabel(1);

export type SubmitMintAuthorizationStep02Result = {
  readonly txHash: string;
  readonly walletSource: string;
  readonly proverAddress: string;
  readonly fraudProver: string;
  readonly threadOutRef: string;
  readonly nextThreadOutRef: string;
  readonly fraudulentHeaderHash: string;
  readonly computationThreadUnit: string;
  readonly thirdStepAddress: string;
  /** The step-03 state the thread now carries. */
  readonly step03State: MintAuthorizationStep03State;
  readonly inputIndex: number;
  readonly outputIndex: number;
  readonly awaitedConfirmation: boolean;
};
