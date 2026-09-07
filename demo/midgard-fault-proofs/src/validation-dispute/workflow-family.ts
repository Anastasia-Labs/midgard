import { DEPLOYMENT_MANIFEST_CONTRACT_NAMES } from "@al-ft/midgard-core/deployment-manifest-identity";

import { FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY } from "../runtime.js";

/**
 * validationTraceDispute production-workflow identity (ruling R1: a
 * family-specific binding/observation/runner path, never a widening of the
 * shared linear-family machinery).
 */
export const VALIDATION_TRACE_DISPUTE_CATEGORY =
  "validationTraceDispute" as const;
/** Catalogue category id (catalogue.ts pins index 6). */
export const VALIDATION_TRACE_DISPUTE_CATEGORY_ID = "00000006" as const;

/**
 * Manifest contract names for the interactive control chain (ruling R4: the
 * exact deployment entries the dispute submitters consume). The opener entry
 * is the category's sole `FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY` row;
 * every prepare/semantic/stage/yield reference beyond these is resolved at
 * action time from deployment info inside the production submit helpers.
 */
export const VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES = Object.freeze({
  opener: "validationTraceDispute",
  source: "validationTraceDisputeSource",
  game: "validationTraceDisputeGame",
  boundary: "validationTraceDisputeBoundary",
  timeout: "validationTraceDisputeTimeout",
  award: "validationTraceDisputeAward",
} as const);

/**
 * Witness scripts the dispute submitters attach by reference: the two token
 * policies, plus the `phas.membership.withdraw` verifier the shared
 * `submitInit` executes when the runner opens the thread (its carriage is
 * fail-closed — owner ruling 2026-08-26 — so the runner must bind it).
 */
export const VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES = Object.freeze({
  computationThreadMint: "computationThreadMint",
  fraudProofMint: "fraudProofMint",
  phasMembershipWithdraw: "phasMembershipWithdraw",
} as const);

/** Manifest names consumed by the shared cursor-family removal action. */
export const VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES = Object.freeze({
  correctionLockSpend: "correctionLockSpend",
  stateQueueSpend: "stateQueueSpend",
  stateQueueMint: "stateQueueMint",
  stateQueueFraudRemovalWithdraw: "stateQueueFraudRemovalWithdraw",
  activeOperatorsSpend: "activeOperatorsSpend",
  activeOperatorsMint: "activeOperatorsMint",
  retiredOperatorsSpend: "retiredOperatorsSpend",
  retiredOperatorsMint: "retiredOperatorsMint",
  schedulerSpend: "schedulerSpend",
} as const);

/**
 * The additional non-family entry `resolveValidationTraceDisputeDeploymentContracts`
 * requires beyond the category chain.
 */
export const VALIDATION_TRACE_DISPUTE_CEK_PROGRAM_MATERIAL_CONTRACT_NAME =
  "cekProgramMaterialSpend" as const;

export type ValidationTraceDisputeControlRole =
  keyof typeof VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES;

const manifestNames: readonly string[] = DEPLOYMENT_MANIFEST_CONTRACT_NAMES;

/**
 * Startup completeness assertion over the R4 roster: every bound role is a
 * finalized-manifest contract name and the opener is the category's declared
 * first-step deployment entry.
 */
export const assertValidationTraceDisputeRosterIsManifestBound = (): void => {
  const roster = [
    ...Object.values(VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES),
    ...Object.values(VALIDATION_TRACE_DISPUTE_WITNESS_CONTRACT_NAMES),
    ...Object.values(VALIDATION_TRACE_DISPUTE_REMOVAL_CONTRACT_NAMES),
    VALIDATION_TRACE_DISPUTE_CEK_PROGRAM_MATERIAL_CONTRACT_NAME,
  ];
  for (const name of roster) {
    if (!manifestNames.includes(name)) {
      throw new Error(
        `validationTraceDispute roster names a contract outside the deployment manifest: ${name}`,
      );
    }
  }
  const declared =
    FRAUD_PROOF_DEPLOYMENT_ENTRIES_BY_CATEGORY[
      VALIDATION_TRACE_DISPUTE_CATEGORY
    ];
  if (
    declared.length !== 1 ||
    declared[0] !== VALIDATION_TRACE_DISPUTE_CONTROL_CONTRACT_NAMES.opener
  ) {
    throw new Error(
      "validationTraceDispute first-step deployment entry diverged from the category roster",
    );
  }
};
