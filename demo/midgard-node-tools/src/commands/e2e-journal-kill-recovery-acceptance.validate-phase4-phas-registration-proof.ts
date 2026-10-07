import * as SDK from "@al-ft/midgard-sdk";
import { getAddressDetails } from "@lucid-evolution/lucid";
import { exactObjectKeys } from "midgard-node/exact-object-keys";
import { loadPhasMembershipWithdrawalScript } from "midgard-node/phas-membership";

import {
  ISOLATED_COMPOSE_PREFIX,
  ISOLATED_DATABASE_PREFIX,
  type Phase4MatchedSnapshotIdentity,
  type Phase4PhasRegistrationProof,
} from "./e2e-journal-kill-recovery-acceptance.validate-phase4-process-isolation-values.js";

export const validatePhase4PhasRegistrationProof = (
  value: unknown,
  label: string,
): Phase4PhasRegistrationProof => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "source",
      "readOnly",
      "registered",
      "cardanoImage",
      "networkMagic",
      "manifestId",
      "registrationTxHash",
      "rewardAddress",
      "rewardAddressBase16",
      "scriptHash",
      "transactionBody",
      "registrationDepositLovelace",
      "confirmation",
      "observedAtTip",
    ])
  ) {
    throw new Error(`${label} fields do not match the exact V1 schema`);
  }
  const proof = value as Record<string, unknown> &
    Partial<Phase4PhasRegistrationProof>;
  if (
    !exactObjectKeys(proof.cardanoImage, ["ref", "id"]) ||
    !exactObjectKeys(proof.transactionBody, [
      "schemaVersion",
      "artifactSha256",
      "cborSha256",
      "cborSizeBytes",
      "cardanoCliTxHash",
      "certificate",
    ]) ||
    !exactObjectKeys(proof.transactionBody.certificate, [
      "kind",
      "index",
      "count",
      "credentialType",
      "scriptHash",
    ]) ||
    !exactObjectKeys(proof.confirmation, ["slot", "blockHeaderHash"]) ||
    !exactObjectKeys(proof.observedAtTip, ["slot", "hash"])
  ) {
    throw new Error(`${label} nested fields do not match the exact V1 schema`);
  }
  if (
    proof.schemaVersion !== "midgard-phase4-phas-registration-proof-v1" ||
    proof.source !== "cardano-cli-local-state-query" ||
    proof.readOnly !== true ||
    proof.registered !== true
  ) {
    throw new Error(`${label} is not an exact read-only registration proof`);
  }
  if (
    typeof proof.cardanoImage?.ref !== "string" ||
    !/@sha256:[a-f0-9]{64}$/u.test(proof.cardanoImage.ref) ||
    typeof proof.cardanoImage.id !== "string" ||
    !/^sha256:[a-f0-9]{64}$/u.test(proof.cardanoImage.id)
  ) {
    throw new Error(`${label} has invalid pinned Cardano image identity`);
  }
  if (
    !Number.isSafeInteger(proof.networkMagic) ||
    (proof.networkMagic ?? 0) <= 0 ||
    !/^[a-f0-9]{64}$/u.test(proof.manifestId ?? "") ||
    !/^[a-f0-9]{64}$/u.test(proof.registrationTxHash ?? "") ||
    !/^stake_test1[0-9a-z]+$/u.test(proof.rewardAddress ?? "") ||
    proof.rewardAddressBase16 !== `f0${proof.scriptHash ?? ""}` ||
    !/^[a-f0-9]{56}$/u.test(proof.scriptHash ?? "") ||
    !Number.isSafeInteger(proof.registrationDepositLovelace) ||
    (proof.registrationDepositLovelace ?? 0) <= 0
  ) {
    throw new Error(`${label} has invalid PHAS registration identity`);
  }
  const canonicalIdentity = SDK.phasMembershipIdentity(
    "Custom",
    loadPhasMembershipWithdrawalScript(),
  );
  if (
    proof.rewardAddress !== canonicalIdentity.rewardAddress ||
    proof.scriptHash !== canonicalIdentity.scriptHash
  ) {
    throw new Error(`${label} is not the canonical deployed PHAS identity`);
  }
  let addressDetails: ReturnType<typeof getAddressDetails>;
  try {
    addressDetails = getAddressDetails(proof.rewardAddress!);
  } catch (cause) {
    throw new Error(`${label} reward address is not valid canonical bech32`, {
      cause,
    });
  }
  if (
    addressDetails.type !== "Reward" ||
    addressDetails.networkId !== 0 ||
    addressDetails.address.hex !== proof.rewardAddressBase16 ||
    addressDetails.stakeCredential?.type !== "Script" ||
    addressDetails.stakeCredential.hash !== proof.scriptHash
  ) {
    throw new Error(
      `${label} reward account is not the exact testnet PHAS script credential`,
    );
  }
  const transactionBody = proof.transactionBody;
  if (
    transactionBody?.schemaVersion !==
      "midgard-phas-registration-transaction-body-v1" ||
    !/^[a-f0-9]{64}$/u.test(transactionBody.artifactSha256 ?? "") ||
    !/^[a-f0-9]{64}$/u.test(transactionBody.cborSha256 ?? "") ||
    !Number.isSafeInteger(transactionBody.cborSizeBytes) ||
    (transactionBody.cborSizeBytes ?? 0) <= 0 ||
    transactionBody.cardanoCliTxHash !== proof.registrationTxHash ||
    transactionBody.certificate?.kind !== "stake_registration" ||
    transactionBody.certificate.index !== 0 ||
    transactionBody.certificate.count !== 1 ||
    transactionBody.certificate.credentialType !== "script" ||
    transactionBody.certificate.scriptHash !== proof.scriptHash
  ) {
    throw new Error(
      `${label} does not bind the exact cardano-cli-inspected PHAS registration transaction body`,
    );
  }
  if (
    !Number.isSafeInteger(proof.confirmation?.slot) ||
    (proof.confirmation?.slot ?? -1) < 0 ||
    !/^[a-f0-9]{64}$/u.test(proof.confirmation?.blockHeaderHash ?? "") ||
    !Number.isSafeInteger(proof.observedAtTip?.slot) ||
    (proof.observedAtTip?.slot ?? -1) < 0 ||
    !/^[a-f0-9]{64}$/u.test(proof.observedAtTip?.hash ?? "") ||
    (proof.confirmation?.slot ?? Number.MAX_SAFE_INTEGER) >
      (proof.observedAtTip?.slot ?? -1)
  ) {
    throw new Error(`${label} has invalid confirmation-point evidence`);
  }
  return proof as Phase4PhasRegistrationProof;
};

export const decodePhase4MatchedSnapshotIdentity = (
  value: unknown,
): Phase4MatchedSnapshotIdentity => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "composeProject",
      "networkMagic",
      "postgresDatabase",
      "deploymentManifestSha256",
      "blueprintSha256",
      "images",
      "artifacts",
      "phasRegistration",
      "cardanoTip",
      "kupoCheckpoint",
    ])
  ) {
    throw new Error(
      "Phase 4 matched snapshot identity fields do not match the exact V1 schema",
    );
  }
  if (
    !exactObjectKeys(value.images, [
      "cardanoNode",
      "ogmios",
      "kupo",
      "postgres",
    ]) ||
    !exactObjectKeys(value.artifacts, [
      "sourceSha256",
      "distSha256",
      "toolsSourceSha256",
      "toolsDistSha256",
      "genesisSha256",
      "configSha256",
      "acceptanceEnvSha256",
      "composeSha256",
      "phase4AssetsSha256",
      "phasRegistrationProofSha256",
    ]) ||
    !exactObjectKeys(value.cardanoTip, ["slot", "hash"])
  ) {
    throw new Error(
      "Phase 4 matched snapshot identity nested fields do not match the exact V1 schema",
    );
  }
  for (const imageName of [
    "cardanoNode",
    "ogmios",
    "kupo",
    "postgres",
  ] as const) {
    const image = value.images[imageName];
    if (
      !exactObjectKeys(image, ["ref", "id"]) ||
      typeof image.ref !== "string" ||
      !/@sha256:[a-f0-9]{64}$/u.test(image.ref) ||
      typeof image.id !== "string" ||
      !/^sha256:[a-f0-9]{64}$/u.test(image.id)
    ) {
      throw new Error(
        `Phase 4 matched snapshot ${imageName} image identity is noncanonical`,
      );
    }
  }
  if (
    value.schemaVersion !== "midgard-phase4-matched-snapshot-identity-v1" ||
    typeof value.composeProject !== "string" ||
    !value.composeProject.startsWith(ISOLATED_COMPOSE_PREFIX) ||
    !/^[a-z0-9_-]+$/u.test(value.composeProject) ||
    !Number.isSafeInteger(value.networkMagic) ||
    (value.networkMagic as number) <= 0 ||
    typeof value.postgresDatabase !== "string" ||
    !value.postgresDatabase.startsWith(ISOLATED_DATABASE_PREFIX) ||
    typeof value.deploymentManifestSha256 !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.deploymentManifestSha256) ||
    typeof value.blueprintSha256 !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.blueprintSha256) ||
    !Number.isSafeInteger(value.cardanoTip.slot) ||
    (value.cardanoTip.slot as number) < 0 ||
    typeof value.cardanoTip.hash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.cardanoTip.hash) ||
    value.kupoCheckpoint !== value.cardanoTip.slot
  ) {
    throw new Error(
      "Phase 4 matched snapshot identity contains a noncanonical V1 value",
    );
  }
  for (const digest of Object.values(value.artifacts)) {
    if (typeof digest !== "string" || !/^[a-f0-9]{64}$/u.test(digest)) {
      throw new Error(
        "Phase 4 matched snapshot identity contains a noncanonical artifact digest",
      );
    }
  }
  const phasRegistration = validatePhase4PhasRegistrationProof(
    value.phasRegistration,
    "Phase 4 matched snapshot PHAS registration proof",
  );
  const cardanoNodeImage = value.images.cardanoNode as {
    readonly ref: string;
    readonly id: string;
  };
  if (
    phasRegistration.networkMagic !== value.networkMagic ||
    phasRegistration.cardanoImage.ref !== cardanoNodeImage.ref ||
    phasRegistration.cardanoImage.id !== cardanoNodeImage.id ||
    phasRegistration.observedAtTip.slot !== value.cardanoTip.slot ||
    phasRegistration.observedAtTip.hash !== value.cardanoTip.hash
  ) {
    throw new Error(
      "Phase 4 matched snapshot PHAS proof is not bound to the snapshot identity",
    );
  }
  return value as Phase4MatchedSnapshotIdentity;
};
