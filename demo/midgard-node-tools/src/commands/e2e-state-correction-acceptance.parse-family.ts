import {
  exactKeys,
  FAMILY_KEYS,
  type ForcedClassificationDrill,
  literal,
  parseChainPoint,
  positiveLovelace,
  record,
  sha256,
  type StateCorrectionFamilyDrill,
  string,
  stringArray,
  type WithdrawalReservePayout,
} from "./e2e-state-correction-acceptance.required-state-correction-recovery-drill-ids.js";

export const parseFamily = (
  value: unknown,
  index: number,
): StateCorrectionFamilyDrill => {
  const field = `families[${index.toString()}]`;
  const candidate = record(value, field);
  exactKeys(candidate, FAMILY_KEYS, field);
  const expectedSlashLovelace = positiveLovelace(
    candidate.expectedSlashLovelace,
    `${field}.expectedSlashLovelace`,
  );
  const observedSlashLovelace = positiveLovelace(
    candidate.observedSlashLovelace,
    `${field}.observedSlashLovelace`,
  );
  const expectedProverRewardLovelace = positiveLovelace(
    candidate.expectedProverRewardLovelace,
    `${field}.expectedProverRewardLovelace`,
  );
  const observedProverRewardLovelace = positiveLovelace(
    candidate.observedProverRewardLovelace,
    `${field}.observedProverRewardLovelace`,
  );
  if (expectedSlashLovelace !== observedSlashLovelace) {
    throw new Error(`${field} observed slash does not equal expected slash`);
  }
  if (expectedProverRewardLovelace !== observedProverRewardLovelace) {
    throw new Error(
      `${field} observed prover reward does not equal expected prover reward`,
    );
  }
  return {
    familyId: string(candidate.familyId, `${field}.familyId`),
    violationId: string(candidate.violationId, `${field}.violationId`),
    headerHash: (() => {
      const parsed = string(candidate.headerHash, `${field}.headerHash`);
      if (!/^[0-9a-f]{56}$/u.test(parsed)) {
        throw new Error(`${field}.headerHash must be 28-byte lowercase hex`);
      }
      return parsed;
    })(),
    routeId: string(candidate.routeId, `${field}.routeId`),
    detectionSource: literal(
      candidate.detectionSource,
      "public-l1-da",
      `${field}.detectionSource`,
    ),
    watcherDriven: literal(
      candidate.watcherDriven,
      true,
      `${field}.watcherDriven`,
    ),
    initTxHash: sha256(candidate.initTxHash, `${field}.initTxHash`),
    proofStepTxHashes: stringArray(
      candidate.proofStepTxHashes,
      `${field}.proofStepTxHashes`,
      sha256,
    ),
    proofTokenTxHash: sha256(
      candidate.proofTokenTxHash,
      `${field}.proofTokenTxHash`,
    ),
    removalTxHash: sha256(candidate.removalTxHash, `${field}.removalTxHash`),
    correctionTxHash: sha256(
      candidate.correctionTxHash,
      `${field}.correctionTxHash`,
    ),
    permanentProofTokenRetained: literal(
      candidate.permanentProofTokenRetained,
      true,
      `${field}.permanentProofTokenRetained`,
    ),
    stateQueueNodeRemoved: literal(
      candidate.stateQueueNodeRemoved,
      true,
      `${field}.stateQueueNodeRemoved`,
    ),
    correctedQueueObserved: literal(
      candidate.correctedQueueObserved,
      true,
      `${field}.correctedQueueObserved`,
    ),
    expectedSlashLovelace,
    observedSlashLovelace,
    expectedProverRewardLovelace,
    observedProverRewardLovelace,
    chainPoint: parseChainPoint(candidate.chainPoint, `${field}.chainPoint`),
    finalStateRoot: sha256(candidate.finalStateRoot, `${field}.finalStateRoot`),
  };
};

const WITHDRAWAL_KEYS = [
  "withdrawalOrderTxHash",
  "reserveTxHash",
  "payoutInitTxHash",
  "payoutAddTxHashes",
  "payoutConcludeTxHash",
  "expectedDestination",
  "observedDestination",
  "expectedPayoutValueSha256",
  "observedPayoutValueSha256",
  "expectedReserveValueSha256",
  "observedReserveValueSha256",
  "reserveAccountingExact",
  "finalStatus",
  "chainPoint",
] as const;

export const parseWithdrawal = (value: unknown): WithdrawalReservePayout => {
  const field = "withdrawalReservePayout";
  const candidate = record(value, field);
  exactKeys(candidate, WITHDRAWAL_KEYS, field);
  const expectedDestination = string(
    candidate.expectedDestination,
    `${field}.expectedDestination`,
  );
  const observedDestination = string(
    candidate.observedDestination,
    `${field}.observedDestination`,
  );
  const expectedPayoutValueSha256 = sha256(
    candidate.expectedPayoutValueSha256,
    `${field}.expectedPayoutValueSha256`,
  );
  const observedPayoutValueSha256 = sha256(
    candidate.observedPayoutValueSha256,
    `${field}.observedPayoutValueSha256`,
  );
  const expectedReserveValueSha256 = sha256(
    candidate.expectedReserveValueSha256,
    `${field}.expectedReserveValueSha256`,
  );
  const observedReserveValueSha256 = sha256(
    candidate.observedReserveValueSha256,
    `${field}.observedReserveValueSha256`,
  );
  if (expectedDestination !== observedDestination) {
    throw new Error(`${field} payout destination mismatch`);
  }
  if (expectedPayoutValueSha256 !== observedPayoutValueSha256) {
    throw new Error(`${field} payout value mismatch`);
  }
  if (expectedReserveValueSha256 !== observedReserveValueSha256) {
    throw new Error(`${field} reserve value mismatch`);
  }
  return {
    withdrawalOrderTxHash: sha256(
      candidate.withdrawalOrderTxHash,
      `${field}.withdrawalOrderTxHash`,
    ),
    reserveTxHash: sha256(candidate.reserveTxHash, `${field}.reserveTxHash`),
    payoutInitTxHash: sha256(
      candidate.payoutInitTxHash,
      `${field}.payoutInitTxHash`,
    ),
    payoutAddTxHashes: stringArray(
      candidate.payoutAddTxHashes,
      `${field}.payoutAddTxHashes`,
      sha256,
    ),
    payoutConcludeTxHash: sha256(
      candidate.payoutConcludeTxHash,
      `${field}.payoutConcludeTxHash`,
    ),
    expectedDestination,
    observedDestination,
    expectedPayoutValueSha256,
    observedPayoutValueSha256,
    expectedReserveValueSha256,
    observedReserveValueSha256,
    reserveAccountingExact: literal(
      candidate.reserveAccountingExact,
      true,
      `${field}.reserveAccountingExact`,
    ),
    finalStatus: literal(candidate.finalStatus, "paid", `${field}.finalStatus`),
    chainPoint: parseChainPoint(candidate.chainPoint, `${field}.chainPoint`),
  };
};

const FORCED_CLASSIFICATION_KEYS = [
  "direction",
  "operatorClassification",
  "canonicalClassification",
  "finalClassification",
  "detectionSource",
  "watcherDriven",
  "routeId",
  "evidenceTxHash",
  "correctionTxHash",
  "corrected",
  "chainPoint",
] as const;

export const parseForcedClassification = (
  value: unknown,
  index: number,
): ForcedClassificationDrill => {
  const field = `forcedClassifications[${index.toString()}]`;
  const candidate = record(value, field);
  exactKeys(candidate, FORCED_CLASSIFICATION_KEYS, field);
  const direction = string(candidate.direction, `${field}.direction`);
  if (
    direction !== "valid-marked-invalid" &&
    direction !== "invalid-marked-valid"
  ) {
    throw new Error(`${field}.direction is not a required forced direction`);
  }
  const expected =
    direction === "valid-marked-invalid"
      ? {
          operator: "invalid" as const,
          canonical: "valid" as const,
          final: "valid" as const,
        }
      : {
          operator: "valid" as const,
          canonical: "invalid" as const,
          final: "invalid" as const,
        };
  return {
    direction,
    operatorClassification: literal(
      candidate.operatorClassification,
      expected.operator,
      `${field}.operatorClassification`,
    ),
    canonicalClassification: literal(
      candidate.canonicalClassification,
      expected.canonical,
      `${field}.canonicalClassification`,
    ),
    finalClassification: literal(
      candidate.finalClassification,
      expected.final,
      `${field}.finalClassification`,
    ),
    detectionSource: literal(
      candidate.detectionSource,
      "public-l1-da",
      `${field}.detectionSource`,
    ),
    watcherDriven: literal(
      candidate.watcherDriven,
      true,
      `${field}.watcherDriven`,
    ),
    routeId: string(candidate.routeId, `${field}.routeId`),
    evidenceTxHash: sha256(candidate.evidenceTxHash, `${field}.evidenceTxHash`),
    correctionTxHash: sha256(
      candidate.correctionTxHash,
      `${field}.correctionTxHash`,
    ),
    corrected: literal(candidate.corrected, true, `${field}.corrected`),
    chainPoint: parseChainPoint(candidate.chainPoint, `${field}.chainPoint`),
  };
};

export const RECOVERY_KEYS = [
  "id",
  "status",
  "failClosed",
  "duplicateSubmissions",
  "lostEvidence",
  "falseVerifiedStates",
  "unrecoverableWorkflows",
  "manualRepair",
  "watcherReadyAfterRecovery",
  "evidenceSha256",
] as const;
