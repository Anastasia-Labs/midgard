import { rejectRetiredUnauthenticatedSubmissionRoute } from "./legacy-submission-boundary.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import { parseSubmitStep01TxInclusion } from "./step-support.js";
import { submitFaultProofTurns } from "./submit-fault-proof-turns.js";
import { buildInvalidRangeStep01 } from "./submit-invalid-range-step-01.submit-invalid-range-step01.js";
import {
  type SubmitInvalidRangeStep01CliConfig,
  type SubmitInvalidRangeStep01Result,
} from "./submit-invalid-range-step-01.types.js";

export const submitInvalidRangeStep01FromFiles = async (
  config: SubmitInvalidRangeStep01CliConfig,
): Promise<SubmitInvalidRangeStep01Result> => {
  rejectRetiredUnauthenticatedSubmissionRoute({
    command: "submit-invalid-range-step-01",
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
  return await submitFaultProofTurns(
    buildInvalidRangeStep01,
    {
      lucid,
      blueprint,
      deploymentInfo,
      network: config.network,
      signer,
      threadOutRef: config.threadOutRef,
      stateQueueBlockOutRef: config.stateQueueBlockOutRef,
      txInclusion: parseSubmitStep01TxInclusion(txInclusionJson),
    },
    {
      label: "invalid-range-step01",
      awaitConfirmation: config.awaitConfirmation ?? true,
    },
  );
};
