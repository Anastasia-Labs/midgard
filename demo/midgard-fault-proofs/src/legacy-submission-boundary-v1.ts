/**
 * Q03/RF-043: the pre-canonical diagnostic submission surface is retired.
 *
 * Provenance labels and prepared JSON are caller-controlled data.  Until a
 * submitter revalidates an authenticated canonical evidence source and derives
 * every redeemer field from that source, these routes must fail before any
 * blueprint, provider, wallet, or chain-state access.
 */

const RETIRED_STEP_COMMANDS = new Set([
  "submit-step-01",
  "submit-step-02",
  "submit-step-03",
  "submit-step-04",
  "submit-invalid-range-step-01",
  "submit-invalid-range-step-02",
  "submit-zero-input-step-01",
  "submit-zero-input-step-02",
  "submit-non-existent-input-step-01",
  "submit-non-existent-input-step-02",
  "submit-non-existent-input-step-03",
  "submit-non-existent-input-step-04",
]);

const RETIRED_INIT_CATEGORIES = new Set([
  "doubleSpend",
  "invalidRange",
  "zeroInput",
  "nonExistentInput",
  "nonExistentInputNoIndex",
]);

export const RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_CODE_V1 =
  "RF-043_LEGACY_SUBMISSION_RETIRED" as const;

export const RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX_V1 =
  `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_CODE_V1}: unauthenticated diagnostic submission routes are retired` as const;

/** Returns whether a CLI/function route is part of the retired surface. */
export const isRetiredUnauthenticatedSubmissionRouteV1 = ({
  command,
  fraudCategory,
}: {
  readonly command: string | undefined;
  readonly fraudCategory?: string;
}): boolean =>
  (command !== undefined && RETIRED_STEP_COMMANDS.has(command)) ||
  (command === "submit-init" &&
    (fraudCategory === undefined ||
      RETIRED_INIT_CATEGORIES.has(fraudCategory)));

/**
 * The single fail-closed guard used by the shipped CLI and file entrypoints.
 * The direct builders are intentionally absent from the production barrel;
 * `command` remains a stable route name so every shipped caller produces the
 * same deterministic error and tests can cover the complete surface.
 */
export const rejectRetiredUnauthenticatedSubmissionRouteV1 = ({
  command,
  fraudCategory,
}: {
  readonly command: string | undefined;
  readonly fraudCategory?: string;
}): void => {
  if (!isRetiredUnauthenticatedSubmissionRouteV1({ command, fraudCategory })) {
    return;
  }
  const categorySuffix =
    command === "submit-init" && fraudCategory !== undefined
      ? ` fraudCategory=${fraudCategory}`
      : "";
  throw new Error(
    `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX_V1}: ${String(command)}${categorySuffix}; authenticated canonical evidence submission is unavailable`,
  );
};
