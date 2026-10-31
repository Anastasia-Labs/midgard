import { getAddressDetails } from "@lucid-evolution/lucid";

import { exactObjectKeys } from "../exact-object-keys.js";
import type {
  PhasMembershipRegistrationTransactionBodyEvidence,
  PhasMembershipRewardRegistrationResult,
} from "./phas-membership-registration.js";

export const decodePhasMembershipRegistrationTransactionBodyEvidence = (
  value: unknown,
): PhasMembershipRegistrationTransactionBodyEvidence => {
  if (
    !exactObjectKeys(value, [
      "schemaVersion",
      "txHash",
      "cborSha256",
      "cborSizeBytes",
      "certificate",
    ]) ||
    !exactObjectKeys(value.certificate, [
      "kind",
      "index",
      "count",
      "credentialType",
      "scriptHash",
    ])
  ) {
    throw new Error(
      "PHAS registration transaction-body evidence fields do not match the exact V1 schema",
    );
  }
  if (
    value.schemaVersion !== "midgard-phas-registration-transaction-body-v1" ||
    typeof value.txHash !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.txHash) ||
    typeof value.cborSha256 !== "string" ||
    !/^[a-f0-9]{64}$/u.test(value.cborSha256) ||
    !Number.isSafeInteger(value.cborSizeBytes) ||
    (value.cborSizeBytes as number) <= 0 ||
    value.certificate.kind !== "stake_registration" ||
    value.certificate.index !== 0 ||
    value.certificate.count !== 1 ||
    value.certificate.credentialType !== "script" ||
    typeof value.certificate.scriptHash !== "string" ||
    !/^[a-f0-9]{56}$/u.test(value.certificate.scriptHash)
  ) {
    throw new Error(
      "PHAS registration transaction-body evidence contains a noncanonical V1 value",
    );
  }
  return value as PhasMembershipRegistrationTransactionBodyEvidence;
};

export const decodePhasMembershipRewardRegistrationResult = (
  value: unknown,
): PhasMembershipRewardRegistrationResult => {
  if (
    !exactObjectKeys(value, [
      "status",
      "rewardAddress",
      "scriptHash",
      "txHash",
      "transactionBody",
    ])
  ) {
    throw new Error(
      "PHAS registration result fields do not match the exact V1 schema",
    );
  }
  if (
    typeof value.rewardAddress !== "string" ||
    !/^stake(?:_test)?1[0-9a-z]+$/u.test(value.rewardAddress) ||
    typeof value.scriptHash !== "string" ||
    !/^[a-f0-9]{56}$/u.test(value.scriptHash)
  ) {
    throw new Error("PHAS registration result identity is noncanonical");
  }
  let rewardAddressDetails: ReturnType<typeof getAddressDetails>;
  try {
    rewardAddressDetails = getAddressDetails(value.rewardAddress);
  } catch (cause) {
    throw new Error("PHAS registration result reward address is invalid", {
      cause,
    });
  }
  if (
    rewardAddressDetails.type !== "Reward" ||
    rewardAddressDetails.stakeCredential?.type !== "Script" ||
    rewardAddressDetails.stakeCredential.hash !== value.scriptHash
  ) {
    throw new Error(
      "PHAS registration result reward address is not bound to its script hash",
    );
  }
  if (value.status === "already_registered") {
    if (value.txHash !== null || value.transactionBody !== null) {
      throw new Error(
        "PHAS already-registered result must not contain transaction evidence",
      );
    }
    return value as PhasMembershipRewardRegistrationResult;
  }
  if (value.status !== "registration_submitted") {
    throw new Error("PHAS registration result status is not canonical V1");
  }
  const transactionBody =
    decodePhasMembershipRegistrationTransactionBodyEvidence(
      value.transactionBody,
    );
  if (
    value.txHash !== transactionBody.txHash ||
    value.scriptHash !== transactionBody.certificate.scriptHash
  ) {
    throw new Error(
      "PHAS registration result is not bound to its transaction evidence",
    );
  }
  return value as PhasMembershipRewardRegistrationResult;
};
