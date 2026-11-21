import { parseNativeTxCompactCbor } from "./field-opening.plan-fault-proof-field-opening.js";
import { parseSafeNonNegativeInteger } from "./json-file.js";
import {
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
} from "./runtime.js";
import { submitFaultProofTurns } from "./submit-fault-proof-turns.js";
import { parseSubmitInvalidSignatureWitnessSetCompact } from "./submit-invalid-signature-step-01.js";
import {
  parseSubmitInvalidSignatureAddrTxWitsPreimage,
  type SubmitInvalidSignatureStep02CliConfig,
  type SubmitInvalidSignatureStep02Result,
} from "./submit-invalid-signature-step-02.make-invalid-signature-step02-spend-redeemer.js";
import { buildInvalidSignatureStep02 } from "./submit-invalid-signature-step-02.submit-invalid-signature-step02.js";

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
  return await submitFaultProofTurns(
    buildInvalidSignatureStep02,
    {
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
    },
    {
      label: "invalid-signature-step02",
      awaitConfirmation: config.awaitConfirmation ?? true,
    },
  );
};
