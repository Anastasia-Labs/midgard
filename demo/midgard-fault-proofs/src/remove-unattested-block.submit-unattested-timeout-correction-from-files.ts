import { submitUnattestedTimeoutCorrection } from "./remove-unattested-block.submit-unattested-timeout-correction.js";
import { type SubmitUnattestedTimeoutCorrectionResult } from "./remove-unattested-block.timeout-correction-plan.js";
import {
  makeLucidForSubmit,
  type ProverSignerConfig,
  readJsonFile,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";

export type RemoveUnattestedBlockCliConfig = SubmitProviderConfig &
  ProverSignerConfig & {
    readonly deploymentInfoPath: string;
    readonly awaitConfirmation?: boolean;
  };

/**
 * The `remove-unattested-block` command. It reads the queue and the
 * correction lock and submits the step they call for; an earlier step still
 * in flight surfaces as the ledger refusing this one,
 * `TimeoutCorrectionAttemptInFlightError`.
 */
export const submitUnattestedTimeoutCorrectionFromFiles = async (
  config: RemoveUnattestedBlockCliConfig,
): Promise<SubmitUnattestedTimeoutCorrectionResult> => {
  const [lucid, deploymentInfo] = await Promise.all([
    makeLucidForSubmit(config),
    readJsonFile(config.deploymentInfoPath),
  ]);
  return submitUnattestedTimeoutCorrection({
    lucid,
    deploymentInfo,
    network: config.network,
    signer: resolveProverSigner(config),
    awaitConfirmation: config.awaitConfirmation,
  });
};
