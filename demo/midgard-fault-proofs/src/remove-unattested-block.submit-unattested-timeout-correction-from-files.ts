import { createFileTimeoutCorrectionJournalStore } from "./remove-unattested-block.parse-timeout-correction-journal.js";
import { type SubmitUnattestedTimeoutCorrectionResult } from "./remove-unattested-block.reconcile-last-timeout-correction-step.js";
import { createLocalKupmiosTimeoutCorrectionRecovery } from "./remove-unattested-block.recover-timeout-correction-attempt.js";
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

export const submitUnattestedTimeoutCorrectionFromFiles = async (
  config: RemoveUnattestedBlockCliConfig,
): Promise<SubmitUnattestedTimeoutCorrectionResult> => {
  const [lucid, deploymentInfo] = await Promise.all([
    makeLucidForSubmit(config),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const kupoUrl = config.kupoUrl ?? process.env.L1_KUPO_KEY;
  const ogmiosUrl = config.ogmiosUrl ?? process.env.L1_OGMIOS_KEY;
  if (
    (config.provider ?? process.env.L1_PROVIDER) !== "Kupmios" ||
    kupoUrl === undefined ||
    ogmiosUrl === undefined
  )
    throw new Error(
      "Timeout-correction recovery requires configured local Kupmios.",
    );
  const recovery = createLocalKupmiosTimeoutCorrectionRecovery({
    deploymentManifest: deploymentInfo,
    kupoUrl,
    ogmiosUrl,
    network: config.network,
  });
  return submitUnattestedTimeoutCorrection({
    lucid,
    deploymentInfo,
    recovery,
    network: config.network,
    signer: resolveProverSigner(config),
    journalStore: createFileTimeoutCorrectionJournalStore(config.journalPath),
    awaitConfirmation: config.awaitConfirmation,
  });
};
