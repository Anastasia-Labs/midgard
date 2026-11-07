import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import { parseSubmitStep01TxInclusion } from "./step-support.js";
import { submitZeroInputStep01 } from "./submit-zero-input-step-01.submit-zero-input-step01.js";
import {
  type SubmitZeroInputStep01CliConfig,
  type SubmitZeroInputStep01Result,
} from "./submit-zero-input-step-01.types.js";

export const submitZeroInputStep01FromFiles = async (
  config: SubmitZeroInputStep01CliConfig,
): Promise<SubmitZeroInputStep01Result> => {
  rejectRetiredUnauthenticatedSubmissionRoute({
    command: "submit-zero-input-step-01",
  });
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  return await submitZeroInputStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseSubmitStep01TxInclusion(txInclusionJson),
    awaitConfirmation: config.awaitConfirmation,
  });
};
