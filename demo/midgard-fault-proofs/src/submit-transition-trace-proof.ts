import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { readJsonFile } from "./json-file.js";
import {
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  parseOutRef,
  type ProverSignerConfig,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "./runtime.js";
import {
  submitTransitionTraceProof,
  type SubmitTransitionTraceProofResult,
} from "./transition-trace/submit.js";
import { readValidationDisputeCborFile } from "./validation-dispute/from-files.js";

export type SubmitTransitionTraceProofFromCborFileConfig =
  SubmitProviderConfig &
    ProverSignerConfig & {
      readonly blueprintPath: string;
      readonly deploymentInfoPath: string;
      readonly threadOutRef: string;
      readonly transitionFaultProofPath: string;
      readonly referenceInputOutRefs?: readonly string[];
      readonly awaitConfirmation?: boolean;
    };

/** Decode a strict TransitionFaultProof file and submit its route/final pair. */
export const submitTransitionTraceProofFromCborFile = async (
  config: SubmitTransitionTraceProofFromCborFileConfig,
): Promise<SubmitTransitionTraceProofResult> => {
  const proofCbor = await readValidationDisputeCborFile(
    config.transitionFaultProofPath,
    "--transition-fault-proof",
  );
  const proof = Data.from(proofCbor, SDK.TransitionFaultProof);
  if (Data.to(proof, SDK.TransitionFaultProof) !== proofCbor) {
    throw new Error(
      "--transition-fault-proof must contain canonical TransitionFaultProof CBOR",
    );
  }

  const referenceInputOutRefs = config.referenceInputOutRefs ?? [];
  if (new Set(referenceInputOutRefs).size !== referenceInputOutRefs.length) {
    throw new Error("--reference-input values must be unique");
  }

  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  const additionalReferenceInputs = await Promise.all(
    referenceInputOutRefs.map((outRef, index) =>
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRef(outRef, `--reference-input[${index.toString()}]`),
        label: `transition-trace reference input ${index.toString()}`,
      }),
    ),
  );

  return await submitTransitionTraceProof({
    lucid,
    blueprint,
    deploymentInfo,
    network: config.network,
    signer: resolveProverSigner(config),
    threadOutRef: config.threadOutRef,
    proof,
    additionalReferenceInputs,
    awaitConfirmation: config.awaitConfirmation,
  });
};
