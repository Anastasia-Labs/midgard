import { parseNativeTxCompactCbor } from "./field-opening.js";
import { parseSafeNonNegativeInteger } from "./json-file.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import { parseSubmitInvalidSignatureWitnessSetCompact } from "./submit-invalid-signature-step-01.js";
import {
  parseSubmitInvalidSignatureAddrTxWitsPreimage,
  type SubmitInvalidSignatureStep02CliConfig,
  type SubmitInvalidSignatureStep02Result,
} from "./submit-invalid-signature-step-02.make-invalid-signature-step02-spend-redeemer.js";
import { submitInvalidSignatureStep02 } from "./submit-invalid-signature-step-02.submit-invalid-signature-step02.js";

export const submitInvalidSignatureStep02FromFiles = async (
  config: SubmitInvalidSignatureStep02CliConfig,
): Promise<SubmitInvalidSignatureStep02Result> => {
  const [
    blueprint,
    deploymentInfo,
    addrTxWitsPreimageJson,
    nativeTxCompactJson,
    witnessSetCompactJson,
    lucid,
  ] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    readJsonFile(config.addrTxWitsPreimagePath),
    readJsonFile(config.nativeTxCompactPath),
    readJsonFile(config.witnessSetCompactPath),
    makeLucidForSubmit(config),
  ]);
  const signer = resolveProverSigner(config);
  return await submitInvalidSignatureStep02({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer,
    threadOutRef: config.threadOutRef,
    addrTxWitsPreimage: parseSubmitInvalidSignatureAddrTxWitsPreimage(
      addrTxWitsPreimageJson,
    ),
    nativeTxCompactCbor: parseNativeTxCompactCbor(
      nativeTxCompactJson,
      "--native-tx-compact",
    ),
    witnessSetCompact: parseSubmitInvalidSignatureWitnessSetCompact(
      witnessSetCompactJson,
    ),
    badAddrTxWitIndex: parseSafeNonNegativeInteger(
      config.badAddrTxWitIndex,
      "--bad-addr-tx-wit-index",
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};
