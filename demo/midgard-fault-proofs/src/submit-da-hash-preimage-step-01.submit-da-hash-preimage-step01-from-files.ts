import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import {
  parseSubmitDaHashPreimageTxInclusion,
  type SubmitDaHashPreimageStep01CliConfig,
  type SubmitDaHashPreimageStep01Result,
} from "./submit-da-hash-preimage-step-01.parse-submit-da-hash-preimage-tx-inclusion.js";
import { buildDaHashPreimageStep01 } from "./submit-da-hash-preimage-step-01.submit-da-hash-preimage-step01.js";
import { submitOutsideWorkflow } from "./workflow/transaction-boundary.js";

export const submitDaHashPreimageStep01FromFiles = async (
  config: SubmitDaHashPreimageStep01CliConfig,
): Promise<SubmitDaHashPreimageStep01Result> => {
  const [blueprint, deploymentInfo, txInclusionJson, lucid] = await Promise.all(
    [
      readJsonFile(config.blueprintPath),
      readJsonFile(config.deploymentInfoPath),
      readJsonFile(config.txInclusionPath),
      makeLucidForSubmit(config),
    ],
  );
  const signer = resolveProverSigner(config);
  const { transaction, ...built } = await buildDaHashPreimageStep01({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    txInclusion: parseSubmitDaHashPreimageTxInclusion(txInclusionJson),
  });
  const awaitConfirmation = config.awaitConfirmation ?? true;
  const txHash = await submitOutsideWorkflow(lucid, transaction, {
    label: "da-hash-preimage step-01",
    awaitConfirmation,
  });
  return { txHash, ...built, awaitedConfirmation: awaitConfirmation };
};
