import { readFile } from "node:fs/promises";

import { describe, expect, it } from "vitest";

type CommitSources = {
  readonly worker: string;
  readonly submission: string;
};

const readCommitSources = async (): Promise<CommitSources> => ({
  worker: await readFile(
    new URL("../src/workers/commit-block-header.ts", import.meta.url),
    "utf8",
  ),
  submission: await readFile(
    new URL(
      "../src/workers/commit-block-header/submission.ts",
      import.meta.url,
    ),
    "utf8",
  ),
});

const section = (
  source: string,
  startMarker: string,
  endMarker?: string,
): string => {
  const start = source.indexOf(startMarker);
  if (start < 0) return "";
  if (endMarker === undefined) return source.slice(start);
  const end = source.indexOf(endMarker, start + startMarker.length);
  return end < 0 ? source.slice(start) : source.slice(start, end);
};

const canonicalV1CommitViolations = ({
  worker,
  submission,
}: CommitSources): readonly string[] => {
  const violations: string[] = [];
  const mpfProcessing = section(
    worker,
    "const mpfProcessingStartedAtMs",
    "const {",
  );
  const depositOnly = section(
    submission,
    "export const submitDepositOnlyCommit",
    "export const submitTxBackedCommit",
  );
  const txBacked = section(
    submission,
    "export const submitTxBackedCommit",
    "export const deferProcessedCommitPayloadUntilConfirmation",
  );
  const recovery = section(
    submission,
    "export const recoverLocalFinalizationAgainstConfirmedBlock",
  );

  const proofValidationSetup = section(
    mpfProcessing,
    "const proofValidationSlotConfig",
    "const processed = yield* processMpfs(",
  );

  if (worker.includes("isMidgardConsensusProfileV1")) {
    violations.push("worker_profile_selector");
  }
  // Forced proof validation must run for every commit build. The property is:
  // the slot mapping is read straight from provider-free worker input with no
  // selector of any kind in front of it, an absent mapping fails the build
  // closed, and the MPF-processing phase acquires no provider handle. The
  // last clause is why this check no longer demands an unconditional
  // `proofValidationLucid`: acquiring Lucid before CandidateReady breaks the
  // provider-free-candidate invariant, so unconditional proof validation has
  // to be reached without it.
  if (
    !/^const proofValidationSlotConfig =\s*workerInput\.data\.forcedValidationSlotConfig;/.test(
      proofValidationSetup,
    ) ||
    proofValidationSetup.includes("?") ||
    !/if \(proofValidationSlotConfig === undefined\) \{\s*return yield\* Effect\.fail\(/.test(
      proofValidationSetup,
    ) ||
    mpfProcessing.includes("proofValidationLucid") ||
    mpfProcessing.includes("acquireCommitLucidOnce")
  ) {
    violations.push("proof_validation_lucid_not_unconditional");
  }
  if (
    !/forcedValidation:\s*\{[\s\S]*?slotForUnixTime:[\s\S]*?unixTimeToSlotForConfig\(\s*unixTimeMs,\s*proofValidationSlotConfig,?\s*\)/.test(
      mpfProcessing,
    )
  ) {
    violations.push("forced_validation_not_unconditional");
  }

  if (submission.includes("isMidgardConsensusProfileV1")) {
    violations.push("submission_profile_selector");
  }
  if (!/const cekProgramMaterial = yield\* Effect\.try\(\{/.test(depositOnly)) {
    violations.push("forced_program_material_not_unconditional");
  }
  if (
    !depositOnly.includes(
      "Cannot build V1 DA from missing or conflicting forced-transaction program material",
    )
  ) {
    violations.push("forced_program_material_failure_missing");
  }
  if (
    !/yield\* TxAdmissionsDB\.retrieveProgramMaterialSidecars\(\s*processedMempoolTxs\.map/.test(
      txBacked,
    )
  ) {
    violations.push("normal_program_material_not_unconditional");
  }
  if (
    !txBacked.includes(
      "Cannot build V1 block without one durable program-material sidecar per normal transaction",
    )
  ) {
    violations.push("normal_program_material_failure_missing");
  }

  if (
    !/const confirmedHeader =\s*yield\* getHeaderV1FromStateQueueDatumLocal\(\s*latestBlock\.datum,\s*\);/.test(
      recovery,
    ) ||
    !/const confirmedHeaderHash = yield\* hashBlockHeaderV1Local\(confirmedHeader\);/.test(
      recovery,
    )
  ) {
    violations.push("header_v1_recovery_not_unconditional");
  }
  if (recovery.includes("proofProfile") || recovery.includes("!proofProfile")) {
    violations.push("recovery_profile_selector");
  }
  if (
    !recovery.includes(
      "PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT",
    ) ||
    !recovery.includes("confirmedHeader.validationTracesRoot")
  ) {
    violations.push("recovery_validation_root_missing");
  }
  if (
    !recovery.includes(
      "PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT",
    ) ||
    !recovery.includes("confirmedHeader.validationTraceCount")
  ) {
    violations.push("recovery_validation_count_missing");
  }

  return violations;
};

describe("canonical V1 commit profile", () => {
  it("has no launch-profile alternate for proof validation, DA material, or header recovery", async () => {
    expect(canonicalV1CommitViolations(await readCommitSources())).toEqual([]);
  });

  it("detects launch-profile and empty-program-material fallback mutations", async () => {
    const sources = await readCommitSources();
    const workerProfileMutation = {
      ...sources,
      worker: sources.worker.replace(
        /const proofValidationSlotConfig =\s*workerInput\.data\.forcedValidationSlotConfig;/,
        "const proofValidationSlotConfig = isMidgardConsensusProfileV1(profile)\n      ? workerInput.data.forcedValidationSlotConfig\n      : undefined;",
      ),
    };
    const workerProviderMutation = {
      ...sources,
      worker: sources.worker.replace(
        "const processed = yield* processMpfs(",
        "const proofValidationLucid = (yield* acquireCommitLucidOnce).api;\n    const processed = yield* processMpfs(",
      ),
    };
    const workerForcedValidationMutation = {
      ...sources,
      worker: sources.worker.replace(
        "forcedValidation: {",
        "forcedValidation:\n          proofValidationSlotConfig === undefined\n            ? undefined\n            : {",
      ),
    };
    const forcedMaterialMutation = {
      ...sources,
      submission: sources.submission.replace(
        "const cekProgramMaterial = yield* Effect.try({",
        "const cekProgramMaterial = isMidgardConsensusProfileV1(profile) ? yield* Effect.try({",
      ),
    };
    const normalMaterialMutation = {
      ...sources,
      submission: sources.submission.replace(
        "yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(",
        "isMidgardConsensusProfileV1(profile) ? yield* TxAdmissionsDB.retrieveProgramMaterialSidecars(",
      ),
    };

    expect(canonicalV1CommitViolations(workerProfileMutation)).toContain(
      "worker_profile_selector",
    );
    expect(canonicalV1CommitViolations(workerProfileMutation)).toContain(
      "proof_validation_lucid_not_unconditional",
    );
    expect(canonicalV1CommitViolations(workerProviderMutation)).toContain(
      "proof_validation_lucid_not_unconditional",
    );
    expect(
      canonicalV1CommitViolations(workerForcedValidationMutation),
    ).toContain("forced_validation_not_unconditional");
    expect(canonicalV1CommitViolations(forcedMaterialMutation)).toContain(
      "submission_profile_selector",
    );
    expect(canonicalV1CommitViolations(normalMaterialMutation)).toContain(
      "submission_profile_selector",
    );
  });

  it("detects removal of either missing-program-material failure", async () => {
    const sources = await readCommitSources();
    const missingForcedFailure = {
      ...sources,
      submission: sources.submission.replace(
        "Cannot build V1 DA from missing or conflicting forced-transaction program material",
        "program material ignored",
      ),
    };
    const missingNormalFailure = {
      ...sources,
      submission: sources.submission.replace(
        "Cannot build V1 block without one durable program-material sidecar per normal transaction",
        "program material ignored",
      ),
    };

    expect(canonicalV1CommitViolations(missingForcedFailure)).toContain(
      "forced_program_material_failure_missing",
    );
    expect(canonicalV1CommitViolations(missingNormalFailure)).toContain(
      "normal_program_material_failure_missing",
    );
  });

  it("detects profile-gated or incomplete validation-root recovery mutations", async () => {
    const sources = await readCommitSources();
    const profileGatedRecovery = {
      ...sources,
      submission: sources.submission.replace(
        "const confirmedHeader =",
        "const proofProfile = isMidgardConsensusProfileV1(profile);\n    const confirmedHeader =",
      ),
    };
    const missingValidationRoot = {
      ...sources,
      submission: sources.submission.replace(
        "PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT",
        "PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT",
      ),
    };
    const missingValidationCount = {
      ...sources,
      submission: sources.submission.replace(
        "PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT",
        "PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT",
      ),
    };

    expect(canonicalV1CommitViolations(profileGatedRecovery)).toContain(
      "recovery_profile_selector",
    );
    expect(canonicalV1CommitViolations(missingValidationRoot)).toContain(
      "recovery_validation_root_missing",
    );
    expect(canonicalV1CommitViolations(missingValidationCount)).toContain(
      "recovery_validation_count_missing",
    );
  });
});
