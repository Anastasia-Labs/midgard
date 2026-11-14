import { createFileTimeoutCorrectionJournalStore } from "./remove-unattested-block.parse-timeout-correction-journal.js";
import { type SubmitUnattestedTimeoutCorrectionResult } from "./remove-unattested-block.reconcile-last-timeout-correction-step.js";
import { submitUnattestedTimeoutCorrection } from "./remove-unattested-block.submit-unattested-timeout-correction.js";
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
    readonly journalPath: string;
    readonly awaitConfirmation?: boolean;
  };

/**
 * The `remove-unattested-block` command. No follower runs in a CLI process,
 * so the run submits unjournaled (`no_follower`) and has no recovery reader:
 * an earlier attempt still in flight surfaces as the ledger refusing the
 * replacement, `TimeoutCorrectionAttemptInFlightError`.
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
    journalStore: createFileTimeoutCorrectionJournalStore(config.journalPath),
    awaitConfirmation: config.awaitConfirmation,
  });
};
