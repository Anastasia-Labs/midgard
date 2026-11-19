/**
 * Q03/RF-043: the diagnostic submission surface is an allow-list.
 *
 * Provenance labels and prepared JSON are caller-controlled data.  Until a
 * submitter revalidates an authenticated canonical evidence source and derives
 * every redeemer field from that source, the retired routes fail before any
 * blueprint, provider, wallet, or chain-state access.  Every other submission
 * route (a `submit-*` or `remove-*` command) must be admitted below, and an
 * unadmitted one fails at the same point with its own error.  Commands that
 * submit nothing (`prepare-*`, `inspect-contracts`, the workflow commands) are
 * the router's own.
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
  "fabricatedDeposit",
  "fabricatedWithdrawal",
  "nativeScriptDecoding",
  "missingSignature",
  "withdrawnReferenceInput",
  "canonicalDecodability",
  "committedFieldShape",
  "minFee",
  "withdrawalMistag",
  "doubleWithdraw",
  "l2TxMistag",
  "withdrawnInput",
]);

/** The routed submission commands that are not retired. */
export const ADMITTED_SUBMISSION_COMMANDS: readonly string[] = Object.freeze([
  "submit-init",
  "submit-transition-trace-proof",
  "submit-fabricated-deposit-step-01",
  "submit-fabricated-deposit-step-02",
  "submit-fabricated-deposit-step-03",
  "submit-fabricated-deposit-step-04",
  "submit-fabricated-withdrawal-step-01",
  "submit-fabricated-withdrawal-step-02",
  "submit-fabricated-withdrawal-step-03",
  "submit-fabricated-withdrawal-step-04",
  "submit-da-hash-preimage-step-01",
  "submit-da-hash-preimage-step-02",
  "submit-input-no-idx-step-01",
  "submit-input-no-idx-step-02",
  // Routed to its own refusal: step 02 has a single route.
  "submit-input-no-idx-fold",
  "submit-input-no-idx-step-03",
  "submit-input-no-idx-step-04",
  "submit-no-reference-input-step-01",
  "submit-no-reference-input-step-02",
  "submit-no-reference-input-step-03",
  "submit-no-reference-input-step-04",
  "submit-reference-input-no-idx-step-01",
  "submit-reference-input-no-idx-step-02",
  "submit-reference-input-no-idx-step-03",
  "submit-reference-input-no-idx-step-04",
  "submit-invalid-signature-step-01",
  "submit-invalid-signature-step-02",
  "remove-fraudulent-block",
  "remove-unattested-block",
]);

/** The `submit-init` fraud categories that are not retired. */
export const ADMITTED_SUBMIT_INIT_CATEGORIES: readonly string[] = Object.freeze(
  [
    "transitionTrace",
    "validationTraceDispute",
    "daHashPreimage",
    "noReferenceInput",
    "referenceInputNoIdx",
    "invalidSignature",
    "valueNotPreserved",
    "inputSetUniqueness",
    "mintAuthorization",
    "networkId",
    "nativeScriptInvalid",
    "minAda",
    "fieldPreimageLengthMismatch",
    "fieldItemWidthIllegal",
    "witnessScriptDecoding",
    "scriptIntegrityHashMissing",
    "transactionOutputNonCanonical",
    "resolvedOutputNonCanonical",
    "mintDeclaredAssetLimit",
    "spendInputSignerMissing",
    "protectedOutputSignerMissing",
    "observersForbiddenOnUntaggedNetwork",
    "observerOrderInvalid",
    "redeemerCanonicity",
    "outputReferenceScriptDecoding",
    "executionSourceScriptDecoding",
    "receivePurposeLanguage",
    "unusedScriptWitness",
    "missingScriptSource",
    "missingRedeemer",
    "unusedRedeemer",
    "executionNativeScriptInvalid",
    "scriptIntegrityHashMismatch",
    "distinctAssetAccumulationLimit",
    "mintItemNonCanonical",
  ],
);

const ADMITTED_SUBMISSION_COMMAND_SET = new Set(ADMITTED_SUBMISSION_COMMANDS);
const ADMITTED_SUBMIT_INIT_CATEGORY_SET = new Set(
  ADMITTED_SUBMIT_INIT_CATEGORIES,
);

export const RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_CODE =
  "RF-043_LEGACY_SUBMISSION_RETIRED" as const;

export const RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX =
  `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_CODE}: unauthenticated diagnostic submission routes are retired` as const;

export const UNADMITTED_SUBMISSION_ROUTE_ERROR_CODE =
  "RF-043_SUBMISSION_ROUTE_NOT_ADMITTED" as const;

export const UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX =
  `${UNADMITTED_SUBMISSION_ROUTE_ERROR_CODE}: the diagnostic submission route is not admitted` as const;

/** Returns whether a CLI/function route is part of the retired surface. */
export const isRetiredUnauthenticatedSubmissionRoute = ({
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
export const rejectRetiredUnauthenticatedSubmissionRoute = ({
  command,
  fraudCategory,
}: {
  readonly command: string | undefined;
  readonly fraudCategory?: string;
}): void => {
  if (!isRetiredUnauthenticatedSubmissionRoute({ command, fraudCategory })) {
    return;
  }
  const categorySuffix =
    command === "submit-init" && fraudCategory !== undefined
      ? ` fraudCategory=${fraudCategory}`
      : "";
  throw new Error(
    `${RETIRED_UNAUTHENTICATED_SUBMISSION_ERROR_PREFIX}: ${String(command)}${categorySuffix}; authenticated canonical evidence submission is unavailable`,
  );
};

/** Returns whether a command names a submission route. */
const isSubmissionCommand = (command: string | undefined): command is string =>
  command !== undefined &&
  (command.startsWith("submit-") || command.startsWith("remove-"));

/**
 * The CLI's submission gate: a retired route fails with the retired-route
 * error, then a submission route that is not admitted, or a `submit-init`
 * whose category is not admitted, fails with the unadmitted-route error. A
 * command that submits nothing passes to the router.
 */
export const rejectUnadmittedSubmissionRoute = ({
  command,
  fraudCategory,
}: {
  readonly command: string | undefined;
  readonly fraudCategory?: string;
}): void => {
  rejectRetiredUnauthenticatedSubmissionRoute({ command, fraudCategory });
  if (!isSubmissionCommand(command)) {
    return;
  }
  if (!ADMITTED_SUBMISSION_COMMAND_SET.has(command)) {
    throw new Error(`${UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX}: ${command}`);
  }
  if (
    command === "submit-init" &&
    (fraudCategory === undefined ||
      !ADMITTED_SUBMIT_INIT_CATEGORY_SET.has(fraudCategory))
  ) {
    throw new Error(
      `${UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX}: submit-init fraudCategory=${fraudCategory}`,
    );
  }
};
