import {
  createLocalStateQueueMutationLeaseCoordinator,
  type RemoveFraudulentBlockCliConfig,
  type SubmitRemoveFraudulentBlockResult,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { submitRemoveFraudulentBlock } from "./remove-fraudulent-block.submit-remove-fraudulent-block.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";

export const submitRemoveFraudulentBlockFromFiles = async (
  config: RemoveFraudulentBlockCliConfig,
): Promise<SubmitRemoveFraudulentBlockResult> => {
  const stateQueueMutationLeaseCoordinator =
    createLocalStateQueueMutationLeaseCoordinator();
  const [lucid, blueprint, deploymentInfo] = await Promise.all([
    makeLucidForSubmit(config),
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const signer = resolveProverSigner(config);
  return await submitRemoveFraudulentBlock({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    fraudCategory: config.fraudCategory,
    fraudulentHeaderHash: config.fraudulentHeaderHash,
    awaitConfirmation: config.awaitConfirmation,
    requireReferenceScripts: true,
    stateQueueMutationLeaseCoordinator,
  });
};
