import { parseNativeTxCompactCbor } from "./field-opening.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import {
  parseSubmitReferenceInputNoIdxOutputsPreimage,
  type SubmitReferenceInputNoIdxStep04CliConfig,
  type SubmitReferenceInputNoIdxStep04Result,
} from "./submit-reference-input-no-idx-step-04.make-reference-input-no-idx-step04-spend-redeemer.js";
import { submitReferenceInputNoIdxStep04 } from "./submit-reference-input-no-idx-step-04.submit-reference-input-no-idx-step04.js";

export const submitReferenceInputNoIdxStep04FromFiles = async (
  config: SubmitReferenceInputNoIdxStep04CliConfig,
): Promise<SubmitReferenceInputNoIdxStep04Result> => {
  const [
    blueprint,
    deploymentInfo,
    outputsPreimageJson,
    nativeTxCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.outputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitReferenceInputNoIdxStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    outputsPreimage:
      parseSubmitReferenceInputNoIdxOutputsPreimage(outputsPreimageJson),
    nativeTxCompactCbor: parseNativeTxCompactCbor(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
