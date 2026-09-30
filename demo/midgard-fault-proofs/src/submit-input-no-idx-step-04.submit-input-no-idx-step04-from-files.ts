import { parseNativeTxCompactCbor } from "./field-opening.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import {
  parseSubmitInputNoIdxOutputsPreimage,
  type SubmitInputNoIdxStep04CliConfig,
  type SubmitInputNoIdxStep04Result,
} from "./submit-input-no-idx-step-04.make-input-no-idx-step04-spend-redeemer.js";
import { submitInputNoIdxStep04 } from "./submit-input-no-idx-step-04.submit-input-no-idx-step04.js";

export const submitInputNoIdxStep04FromFiles = async (
  config: SubmitInputNoIdxStep04CliConfig,
): Promise<SubmitInputNoIdxStep04Result> => {
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
  return await submitInputNoIdxStep04({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    outputsPreimage: parseSubmitInputNoIdxOutputsPreimage(outputsPreimageJson),
    nativeTxCompactCbor: parseNativeTxCompactCbor(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
