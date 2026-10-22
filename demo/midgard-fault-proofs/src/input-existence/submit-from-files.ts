/** Canonical file entrypoint: admit the complete source/event/ledger artifact
 * before provider construction, then resolve every actual reference by outref. */
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";

import {
  fetchUtxoByOutRef,
  makeLucidForSubmit,
  readJsonFile,
  resolveProverSigner,
  type SubmitProviderConfig,
} from "../runtime.js";
import { exactJournalRecord } from "../workflow/native-index-artifact.js";
import { admitInputExistenceArtifact } from "./artifact.js";
import { createInputExistenceDeploymentStep } from "./deployment-step.js";

export type InputExistenceStepFileConfig = SubmitProviderConfig &
  Readonly<{
    blueprintPath: string;
    deploymentInfoPath: string;
    artifactPath: string;
    referencesPath: string;
    walletSeedPhrase?: string;
    walletSeedPhraseEnv?: string;
    walletPrivateKey?: string;
    walletPrivateKeyEnv?: string;
    threadOutRef: string;
    stateQueueBlockOutRef?: string;
    awaitConfirmation?: boolean;
  }>;

const reference = (value: unknown, label: string): string => {
  if (typeof value !== "string") throw new Error(`${label} must be an outref`);
  parseOutRefLabel(value);
  return value;
};
const referenceList = (value: unknown, label: string): readonly string[] => {
  if (!Array.isArray(value)) throw new Error(`${label} must be an outref list`);
  return value.map((item: unknown) => reference(item, label));
};

export const parseInputExistenceFileReferences = (value: unknown) => {
  const row = exactJournalRecord(
    value,
    [
      "steps",
      "computationThreadMint",
      "fraudProofMint",
      "phasMembershipWithdraw",
      "chunkedVerifyWithdraw",
      "pexcludesWithdraw",
      "fieldCarriage",
      "certificatePolicyId",
    ],
    "input-existence references",
  );
  const steps = referenceList(row.steps, "input-existence step references");
  if (steps.length !== 4)
    throw new Error("input-existence needs four step references");
  const certificatePolicyId = row.certificatePolicyId;
  if (
    certificatePolicyId !== null &&
    (typeof certificatePolicyId !== "string" ||
      !/^[0-9a-f]{56}$/u.test(certificatePolicyId))
  )
    throw new Error(
      "input-existence certificate policy must be a canonical policy id or null",
    );
  return {
    steps,
    computationThreadMint: reference(
      row.computationThreadMint,
      "computation-thread mint",
    ),
    fraudProofMint: reference(row.fraudProofMint, "fraud-proof mint"),
    phasMembershipWithdraw: reference(
      row.phasMembershipWithdraw,
      "PHAS withdrawal",
    ),
    chunkedVerifyWithdraw: reference(
      row.chunkedVerifyWithdraw,
      "chunked-verifier withdrawal",
    ),
    pexcludesWithdraw: reference(row.pexcludesWithdraw, "pexcludes withdrawal"),
    fieldCarriage: referenceList(
      row.fieldCarriage,
      "field-carriage references",
    ),
    certificatePolicyId,
  };
};

export const createInputExistenceFileSubmitter =
  (field: 0 | 1, stepIndex: 0 | 1 | 2 | 3) =>
  async (config: InputExistenceStepFileConfig) => {
    const [blueprint, deploymentInfo, artifact, referencesJson] =
      await Promise.all([
        readJsonFile(config.blueprintPath),
        readJsonFile(config.deploymentInfoPath),
        readJsonFile(config.artifactPath),
        readJsonFile(config.referencesPath),
      ]);
    const signer = resolveProverSigner(config);
    const prepared = admitInputExistenceArtifact(
      artifact,
      signer.paymentKeyHash,
    );
    if (prepared.field !== field)
      throw new Error("input-existence file names another family");
    const refs = parseInputExistenceFileReferences(referencesJson);
    reference(config.threadOutRef, "input-existence thread");
    if (
      stepIndex === 0 &&
      prepared.source.kind === "accepted" &&
      config.stateQueueBlockOutRef === undefined
    )
      throw new Error(
        "accepted input-existence source needs its state-queue block outref",
      );
    if (config.stateQueueBlockOutRef !== undefined)
      reference(
        config.stateQueueBlockOutRef,
        "input-existence state-queue block",
      );
    const lucid = await makeLucidForSubmit(config);
    const resolveReference = (outref: string) =>
      fetchUtxoByOutRef({
        lucid,
        outRef: parseOutRefLabel(outref),
        label: "input-existence published reference",
      });
    const steps = await Promise.all(refs.steps.map(resolveReference));
    const [
      computationThreadMint,
      fraudProofMint,
      phasMembershipWithdraw,
      chunkedVerifyWithdraw,
      pexcludesWithdraw,
      fieldCarriageUtxos,
    ] = await Promise.all([
      resolveReference(refs.computationThreadMint),
      resolveReference(refs.fraudProofMint),
      resolveReference(refs.phasMembershipWithdraw),
      resolveReference(refs.chunkedVerifyWithdraw),
      resolveReference(refs.pexcludesWithdraw),
      Promise.all(refs.fieldCarriage.map(resolveReference)),
    ]);
    return createInputExistenceDeploymentStep(
      field,
      stepIndex,
    )({
      lucid,
      blueprint,
      deploymentInfo,
      network: config.network,
      signer,
      prepared,
      threadOutRef: config.threadOutRef,
      stateQueueBlockOutRef: config.stateQueueBlockOutRef,
      referenceScripts: { steps, computationThreadMint, fraudProofMint },
      witnessReferenceScripts: {
        computationThreadMint,
        fraudProofMint,
        phasMembershipWithdraw,
        chunkedVerifyWithdraw,
        pexcludesWithdraw,
      },
      fieldCarriageUtxos,
      ...(refs.certificatePolicyId === null
        ? {}
        : { certificatePolicyId: refs.certificatePolicyId }),
      awaitConfirmation: config.awaitConfirmation,
    });
  };
