import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import {
  buildDaHashPreimageStep02,
  type SubmitDaHashPreimageStep02CliConfig,
  type SubmitDaHashPreimageStep02Result,
} from "./submit-da-hash-preimage-step-02.js";
import { submitUnjournaled } from "./workflow/transaction-boundary.js";

export const submitDaHashPreimageStep02FromFiles = async (
  config: SubmitDaHashPreimageStep02CliConfig,
): Promise<SubmitDaHashPreimageStep02Result> => {
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  const { transaction, ...built } = await buildDaHashPreimageStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
  });
  const awaitConfirmation = config.awaitConfirmation ?? true;
  const txHash = await submitUnjournaled(lucid, transaction, {
    label: "da-hash-preimage step-02",
    awaitConfirmation,
  });
  return { txHash, ...built, awaitedConfirmation: awaitConfirmation };
};
