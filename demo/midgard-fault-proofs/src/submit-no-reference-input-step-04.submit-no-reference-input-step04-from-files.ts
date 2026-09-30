import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import { submitNoReferenceInputStep04 } from "./submit-no-reference-input-step-04.submit-no-reference-input-step04.js";
import {
  type SubmitNoReferenceInputStep04CliConfig,
  type SubmitNoReferenceInputStep04Result,
} from "./submit-no-reference-input-step-04.submit-no-reference-input-step04-result.js";

export const submitNoReferenceInputStep04FromFiles = async (
  config: SubmitNoReferenceInputStep04CliConfig,
): Promise<SubmitNoReferenceInputStep04Result> => {
  const [blueprint, deploymentInfo, proofJson, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.txsNonMembershipProofPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitNoReferenceInputStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    txsNonMembershipProofCbor: proofJson as string,
    awaitConfirmation: config.awaitConfirmation,
  });
};
