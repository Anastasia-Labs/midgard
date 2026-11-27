/**
 * The diagnostic submission surface is an allow-list.
 *
 * Every submission route (a `submit-*` or `remove-*` command) must be admitted
 * below, and an unadmitted one fails before any blueprint, provider, wallet, or
 * chain-state access. Commands that submit nothing (`prepare-*`,
 * `inspect-contracts`, the workflow commands) are the router's own.
 */

/** The routed submission commands. */
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

/** The admitted `submit-init` fraud categories. */
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

const UNADMITTED_SUBMISSION_ROUTE_ERROR_CODE =
  "SUBMISSION_ROUTE_NOT_ADMITTED" as const;

export const UNADMITTED_SUBMISSION_ROUTE_ERROR_PREFIX =
  `${UNADMITTED_SUBMISSION_ROUTE_ERROR_CODE}: the diagnostic submission route is not admitted` as const;

/** Returns whether a command names a submission route. */
const isSubmissionCommand = (command: string | undefined): command is string =>
  command !== undefined &&
  (command.startsWith("submit-") || command.startsWith("remove-"));

/**
 * The CLI's submission gate: a submission route that is not admitted, or a
 * `submit-init` whose category is not admitted, fails with the
 * unadmitted-route error. A command that submits nothing passes to the router.
 */
export const rejectUnadmittedSubmissionRoute = ({
  command,
  fraudCategory,
}: {
  readonly command: string | undefined;
  readonly fraudCategory?: string;
}): void => {
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
