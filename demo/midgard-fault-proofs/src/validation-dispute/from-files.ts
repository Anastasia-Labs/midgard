import { readFile } from "node:fs/promises";

import {
  decodeMidgardCekProgramEnvelope,
  hashMidgardCekProgramEnvelope,
} from "@al-ft/midgard-core";
import {
  ValidationBoundaryEvidence,
  ValidationClaimWitness,
  ValidationTraceDescriptor,
  ValidationTraceProof,
  validationTraceProofCoreFromData,
} from "@al-ft/midgard-sdk";
import { parseCekProgramMaterialNecessityReceiptSet } from "@al-ft/midgard-validation";
import { Data, type Network } from "@lucid-evolution/lucid";

import { readJsonFile } from "../json-file.js";
import {
  makeLucidForSubmit,
  type ProverSignerConfig,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "../runtime.js";
import {
  submitValidationDisputeAward,
  type SubmitValidationDisputeAwardResult,
  submitValidationDisputeEnterResolution,
  type SubmitValidationDisputeEnterResolutionResult,
  submitValidationDisputeEnterTimeout,
  type SubmitValidationDisputeEnterTimeoutResult,
  submitValidationDisputeOpen,
  type SubmitValidationDisputeOpenResult,
  submitValidationDisputePrepareResolution,
  type SubmitValidationDisputePrepareResolutionResult,
  submitValidationDisputePrepareSelected,
  type SubmitValidationDisputePrepareSelectedResult,
  submitValidationDisputeReveal,
  type SubmitValidationDisputeRevealResult,
  submitValidationDisputeSemanticResolution,
  type SubmitValidationDisputeSemanticResolutionResult,
  submitValidationDisputeTimeout,
  type SubmitValidationDisputeTimeoutResult,
  submitValidationDisputeVerifySource,
  type SubmitValidationDisputeVerifySourceResult,
  validateCekSubmissionEvidence,
  type ValidationOneStepSubmissionArgument,
} from "./submit.js";

type ValidationDisputeFromFilesBase = Omit<
  SubmitProviderConfig & ProverSignerConfig,
  "network"
> & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
  readonly network: Network;
  readonly threadOutRef: string;
  readonly awaitConfirmation?: boolean;
};

const exactCborHex = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.length % 2 !== 0 ||
    !/^[0-9a-f]+$/u.test(value)
  ) {
    throw new Error(`${label} must be non-empty lowercase CBOR hex`);
  }
  return value;
};

export const readValidationDisputeCborFile = async (
  path: string,
  label: string,
): Promise<string> => {
  const text = (await readFile(path, "utf8")).trim();
  if (/^[0-9a-f]+$/u.test(text)) {
    return exactCborHex(text, label);
  }
  let parsed: unknown;
  try {
    parsed = JSON.parse(text);
  } catch {
    throw new Error(`${label} file must contain raw CBOR hex or JSON`);
  }
  if (
    typeof parsed !== "object" ||
    parsed === null ||
    Array.isArray(parsed) ||
    !("cborHex" in parsed)
  ) {
    throw new Error(`${label} JSON must contain exactly a cborHex field`);
  }
  const record = parsed as Record<string, unknown>;
  if (Object.keys(record).length !== 1) {
    throw new Error(`${label} JSON must contain exactly a cborHex field`);
  }
  return exactCborHex(record.cborHex, `${label}.cborHex`);
};

const runtimeFromFiles = async (config: ValidationDisputeFromFilesBase) => {
  const [blueprint, deploymentInfo, lucid] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
    makeLucidForSubmit(config),
  ]);
  return {
    blueprint,
    deploymentInfo,
    lucid,
    signer: resolveProverSigner(config),
  };
};

export type ValidationOneStepArgumentFromFiles = {
  readonly validationTransitionCborPath: string;
  readonly validationAuxiliaryCborPath: string;
  readonly validationResolverIndex: number;
  readonly validationSemanticResolverIndex: number;
  readonly validationCekEnvelopeCborPath?: string;
  readonly validationCekProgramMaterialSidecarCborPath?: string;
  readonly validationCekIncrementalNecessityReceiptSetPath?: string;
};

export const validationOneStepArgumentFromFiles = async (
  config: ValidationOneStepArgumentFromFiles,
): Promise<ValidationOneStepSubmissionArgument> => {
  if (
    !Number.isSafeInteger(config.validationResolverIndex) ||
    config.validationResolverIndex < 0 ||
    config.validationResolverIndex >= 14
  ) {
    throw new Error(
      "validation resolver index must be an integer from 0 through 13",
    );
  }
  if (
    !Number.isSafeInteger(config.validationSemanticResolverIndex) ||
    config.validationSemanticResolverIndex < 0
  ) {
    throw new Error(
      "validation semantic resolver index must be a non-negative integer",
    );
  }
  const hasEnvelopePath = config.validationCekEnvelopeCborPath !== undefined;
  const hasSidecarPath =
    config.validationCekProgramMaterialSidecarCborPath !== undefined;
  if (hasEnvelopePath !== hasSidecarPath) {
    throw new Error(
      "CEK route material requires both explicit envelope and program-material sidecar paths",
    );
  }
  if (
    config.validationCekIncrementalNecessityReceiptSetPath !== undefined &&
    !hasEnvelopePath
  ) {
    throw new Error(
      "CEK incremental necessity receipts require explicit CEK route material paths",
    );
  }
  const [
    transitionCbor,
    auxiliaryCbor,
    envelopeCbor,
    programMaterialSidecarCbor,
    necessityReceiptSet,
  ] = await Promise.all([
    readValidationDisputeCborFile(
      config.validationTransitionCborPath,
      "validation one-step transition",
    ),
    readValidationDisputeCborFile(
      config.validationAuxiliaryCborPath,
      "validation one-step auxiliary witness",
    ),
    config.validationCekEnvelopeCborPath === undefined
      ? Promise.resolve(undefined)
      : readValidationDisputeCborFile(
          config.validationCekEnvelopeCborPath,
          "CEK route envelope",
        ),
    config.validationCekProgramMaterialSidecarCborPath === undefined
      ? Promise.resolve(undefined)
      : readValidationDisputeCborFile(
          config.validationCekProgramMaterialSidecarCborPath,
          "CEK route program-material sidecar",
        ),
    config.validationCekIncrementalNecessityReceiptSetPath === undefined
      ? Promise.resolve(undefined)
      : readJsonFile(
          config.validationCekIncrementalNecessityReceiptSetPath,
        ).then(parseCekProgramMaterialNecessityReceiptSet),
  ]);
  const envelopeBytes =
    envelopeCbor === undefined ? undefined : Buffer.from(envelopeCbor, "hex");
  const argument: ValidationOneStepSubmissionArgument = {
    resolverIndex: config.validationResolverIndex,
    semanticResolverIndex: config.validationSemanticResolverIndex,
    transitionCbor: Buffer.from(transitionCbor, "hex"),
    auxiliaryCbor: Buffer.from(auxiliaryCbor, "hex"),
    ...(envelopeBytes === undefined || programMaterialSidecarCbor === undefined
      ? {}
      : {
          cekRouteMaterial: {
            envelopeCbor: envelopeBytes,
            programMaterialSidecarCbor: Buffer.from(
              programMaterialSidecarCbor,
              "hex",
            ),
            programEnvelopeHash: hashMidgardCekProgramEnvelope(
              decodeMidgardCekProgramEnvelope(envelopeBytes),
            ),
          },
        }),
    ...(necessityReceiptSet === undefined
      ? {}
      : { cekIncrementalNecessityReceiptSet: necessityReceiptSet }),
  };
  validateCekSubmissionEvidence(argument);
  return argument;
};

export const submitValidationDisputeOpenFromFiles = async (
  config: ValidationDisputeFromFilesBase & {
    readonly stateQueueBlockOutRef: string;
    readonly claimCborPath: string;
    readonly challengerDescriptorCborPath: string;
  },
): Promise<SubmitValidationDisputeOpenResult> => {
  const [runtime, claimCbor, challengerDescriptorCbor] = await Promise.all([
    runtimeFromFiles(config),
    readValidationDisputeCborFile(
      config.claimCborPath,
      "validation claim witness",
    ),
    readValidationDisputeCborFile(
      config.challengerDescriptorCborPath,
      "challenger validation-trace descriptor",
    ),
  ]);
  return await submitValidationDisputeOpen({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    stateQueueBlockOutRef: config.stateQueueBlockOutRef,
    claim: Data.from(claimCbor, ValidationClaimWitness),
    challengerDescriptor: Data.from(
      challengerDescriptorCbor,
      ValidationTraceDescriptor,
    ),
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputeRevealFromFiles = async (
  config: ValidationDisputeFromFilesBase & {
    readonly role: "operator" | "challenger";
    readonly proofCborPath: string;
  },
): Promise<SubmitValidationDisputeRevealResult> => {
  const [runtime, proofCbor] = await Promise.all([
    runtimeFromFiles(config),
    readValidationDisputeCborFile(
      config.proofCborPath,
      "validation-trace midpoint proof",
    ),
  ]);
  const proof = validationTraceProofCoreFromData(
    Data.from(proofCbor, ValidationTraceProof),
  );
  return await submitValidationDisputeReveal({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    role: config.role,
    proof,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputeVerifySourceFromFiles = async (
  config: ValidationDisputeFromFilesBase,
): Promise<SubmitValidationDisputeVerifySourceResult> => {
  const runtime = await runtimeFromFiles(config);
  return await submitValidationDisputeVerifySource({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputeEnterResolutionFromFiles = async (
  config: ValidationDisputeFromFilesBase,
): Promise<SubmitValidationDisputeEnterResolutionResult> => {
  const runtime = await runtimeFromFiles(config);
  return await submitValidationDisputeEnterResolution({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputePrepareResolutionFromFiles = async (
  config: ValidationDisputeFromFilesBase & {
    readonly boundaryEvidenceCborPath: string;
  },
): Promise<SubmitValidationDisputePrepareResolutionResult> => {
  const [runtime, boundaryEvidenceCbor] = await Promise.all([
    runtimeFromFiles(config),
    readValidationDisputeCborFile(
      config.boundaryEvidenceCborPath,
      "validation one-step boundary evidence",
    ),
  ]);
  const boundaryEvidence = Data.from(
    boundaryEvidenceCbor,
    ValidationBoundaryEvidence,
  );
  return await submitValidationDisputePrepareResolution({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    preState: boundaryEvidence.pre_state,
    operatorPost: boundaryEvidence.operator_post,
    challengerPost: boundaryEvidence.challenger_post,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputePrepareSelectedFromFiles = async (
  config: ValidationDisputeFromFilesBase & ValidationOneStepArgumentFromFiles,
): Promise<SubmitValidationDisputePrepareSelectedResult> => {
  const [runtime, oneStepArgument] = await Promise.all([
    runtimeFromFiles(config),
    validationOneStepArgumentFromFiles(config),
  ]);
  return await submitValidationDisputePrepareSelected({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    oneStepArgument,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputeSemanticResolutionFromFiles = async (
  config: ValidationDisputeFromFilesBase &
    ValidationOneStepArgumentFromFiles &
    ValidationCekPublicationOutRefsFromFiles,
): Promise<SubmitValidationDisputeSemanticResolutionResult> => {
  const cekProgramMaterialReferenceOutRefs =
    validationCekProgramMaterialReferenceOutRefsFromFiles(config);
  const [runtime, oneStepArgument] = await Promise.all([
    runtimeFromFiles(config),
    validationOneStepArgumentFromFiles(config),
  ]);
  return await submitValidationDisputeSemanticResolution({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    oneStepArgument,
    ...(cekProgramMaterialReferenceOutRefs === undefined
      ? {}
      : { cekProgramMaterialReferenceOutRefs }),
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputeAwardFromFiles = async (
  config: ValidationDisputeFromFilesBase,
): Promise<SubmitValidationDisputeAwardResult> => {
  const runtime = await runtimeFromFiles(config);
  return await submitValidationDisputeAward({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export type ValidationCekPublicationOutRefsFromFiles = {
  /**
   * Caller-confirmed exact single-publication material outref
   * (`txHash#outputIndex`), required before the single-publication reference
   * route may be selected.
   */
  readonly validationCekSinglePublicationOutRef?: string;
  /**
   * Caller-confirmed exact multi-output material outrefs in strict material
   * root order, required before minimum multi-output reconstruction may be
   * selected.
   */
  readonly validationCekMinimumMultiOutputOutRefs?: readonly string[];
};

export const validationCekProgramMaterialReferenceOutRefsFromFiles = (
  config: ValidationCekPublicationOutRefsFromFiles,
):
  | {
      readonly singlePublication?: string;
      readonly minimumMultiOutput?: readonly string[];
    }
  | undefined => {
  const singlePublication = config.validationCekSinglePublicationOutRef;
  const minimumMultiOutput = config.validationCekMinimumMultiOutputOutRefs;
  if (singlePublication === undefined && minimumMultiOutput === undefined) {
    return undefined;
  }
  if (minimumMultiOutput !== undefined && minimumMultiOutput.length === 0) {
    throw new Error(
      "CEK minimum-multi material outrefs must list at least one txHash#outputIndex entry in root order",
    );
  }
  return {
    ...(singlePublication === undefined ? {} : { singlePublication }),
    ...(minimumMultiOutput === undefined
      ? {}
      : { minimumMultiOutput: [...minimumMultiOutput] }),
  };
};

export const submitValidationDisputeEnterTimeoutFromFiles = async (
  config: ValidationDisputeFromFilesBase,
): Promise<SubmitValidationDisputeEnterTimeoutResult> => {
  const runtime = await runtimeFromFiles(config);
  return await submitValidationDisputeEnterTimeout({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    awaitConfirmation: config.awaitConfirmation,
  });
};

export const submitValidationDisputeTimeoutFromFiles = async (
  config: ValidationDisputeFromFilesBase,
): Promise<SubmitValidationDisputeTimeoutResult> => {
  const runtime = await runtimeFromFiles(config);
  return await submitValidationDisputeTimeout({
    ...runtime,
    network: config.network,
    threadOutRef: config.threadOutRef,
    awaitConfirmation: config.awaitConfirmation,
  });
};
