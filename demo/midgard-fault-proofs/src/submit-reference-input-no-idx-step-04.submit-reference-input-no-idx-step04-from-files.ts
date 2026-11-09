import { resolveFaultProofFieldGrammarFromDeploymentInfo } from "./field-grammar-certificate.js";
import {
  parseNativeTxCompactCbor,
  parseNativeTxWitnessSetCompactCbor,
} from "./field-opening.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
  resolveReferenceInputNoIdxDeploymentContracts,
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
  const { contracts } = await resolveReferenceInputNoIdxDeploymentContracts({
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
    label: "submit-reference-input-no-idx-step-04",
  });
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
