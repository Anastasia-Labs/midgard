import { resolveFaultProofFieldGrammarFromDeploymentInfo } from "./field-grammar-certificate.js";
import {
  parseNativeTxCompactCbor,
  parseNativeTxWitnessSetCompactCbor,
} from "./field-opening.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveInputNoIdxDeploymentContracts,
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
    witnessSetCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.outputsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    config.witnessSetCompactPath === undefined
      ? undefined
      : readJsonFile(config.witnessSetCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  // The deployed field grammar certificate, resolved from the same blueprint
  // and deployment info the step runs against, so a field past the walked
  // count limit is counted through it.
  const { contracts } = await resolveInputNoIdxDeploymentContracts({
    blueprint,
    deploymentInfo,
    network: config.network,
  });
  const fieldGrammar = await resolveFaultProofFieldGrammarFromDeploymentInfo({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    fieldPreimageCertificatePolicyId:
      contracts.fieldPreimageCertificate.policyId,
    label: "submit-input-no-idx-step-04",
  });
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
    ...(witnessSetCompactJson === undefined
      ? {}
      : {
          witnessSetCompactCbor: parseNativeTxWitnessSetCompactCbor(
            witnessSetCompactJson,
            "--witness-set-compact",
          ),
        }),
    fieldGrammar,
    awaitConfirmation: config.awaitConfirmation,
  });
};
