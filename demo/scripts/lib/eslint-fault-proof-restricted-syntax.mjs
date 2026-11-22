/**
 * The fault-proof `no-restricted-syntax` config objects, which
 * demo/eslint.config.mjs spreads in place: production source neither submits
 * outside a workflow nor drives a removal outside its allow-lists, workflow
 * code does not submit fault-proof turns, and a fault-proof builder does not
 * preset its own wallet inputs.
 *
 * Builders return their signed transaction and the workflow runner submits
 * it, holding it in the process's in-flight set until it lands or its bound
 * passes. Blind spots: a direct `signed.submit()` call,
 * `submitCapturedTransaction` (the workflow adapters submit through it, so it
 * stays open), a computed member access such as
 * `module["submitOutsideWorkflow"]`, a local function with a restricted name,
 * and a call through a renamed re-export.
 */

// A fault-proof builder that presets wallet inputs without narrowing them
// hides the restricted wallet's collateral from Lucid.
const presetWalletInputsRestriction = {
  selector:
    "Property[key.name='presetWalletInputs'], Property[key.value='presetWalletInputs'], AssignmentExpression[left.property.name='presetWalletInputs']",
  message:
    "A fault-proof builder does not choose its own presetWalletInputs: a needless preset hides the wallet's collateral from Lucid. Fix: spread the wallet inputs of stepWalletFunding or plainWalletInputs (src/funding/plain-funding-output.ts) into complete, passing what the transaction must not spend as `excluding`.",
};

// The fault-proof sources that preset wallet inputs deliberately.
const presetWalletInputsOwners = [
  "midgard-fault-proofs/src/funding/**",
  "midgard-fault-proofs/src/validation-trace-dispute/**",
  "midgard-fault-proofs/src/remove-unattested-block*",
];

// `submitOutsideWorkflow` sends a transaction no workflow's in-flight set
// holds, so production source never reaches it: import, re-export, member
// access or destructuring.
const outsideWorkflowSubmissionRestrictions = [
  "ImportSpecifier[imported.name='submitOutsideWorkflow']",
  "ExportSpecifier[local.name='submitOutsideWorkflow']",
  "MemberExpression[property.name='submitOutsideWorkflow']",
  "ObjectPattern > Property[key.name='submitOutsideWorkflow']",
].map((selector) => ({
  selector,
  message:
    "submitOutsideWorkflow sends a transaction no workflow's in-flight set holds: only the diagnostic CLI, the removal driver, the turn submitter and the field-grammar certifier submit that way. Fix: return the signed transaction from the builder and let the workflow runner, a CLI from-files module or a test submit it.",
}));

// The turn submitters send a builder's prerequisite and continuation turns
// outside any workflow; workflow code takes each turn through
// workflowStepTransaction instead, and only the diagnostic CLI submits them.
const turnSubmissionNames = ["settleFaultProofTurns", "submitFaultProofTurns"];
const turnSubmissionRestrictions = turnSubmissionNames.flatMap((name) =>
  [
    `ImportSpecifier[imported.name='${name}']`,
    `ExportSpecifier[local.name='${name}']`,
    `MemberExpression[property.name='${name}']`,
    `ObjectPattern > Property[key.name='${name}']`,
  ].map((selector) => ({
    selector,
    message: `${name} submits fault-proof turns outside any workflow: only the diagnostic CLI and its from-files modules submit turns that way. Fix: take the turn through workflowStepTransaction (src/workflow/step-transaction.ts), which refuses a prerequisite and returns a continuation as the step.`,
  })),
);

// The workflow code of the fault proofs: the shared workflow modules and each
// family's workflow adapter, transaction port, actuator and recovery modules.
const workflowModules = [
  "midgard-fault-proofs/src/workflow/**",
  "midgard-fault-proofs/src/*/workflow*.ts",
  "midgard-fault-proofs/src/*/authenticated-workflow*.ts",
  "midgard-fault-proofs/src/*/actuator*.ts",
  "midgard-fault-proofs/src/*/adapter.ts",
  "midgard-fault-proofs/src/*/transaction-port.ts",
  "midgard-fault-proofs/src/*/transactions.ts",
  "midgard-fault-proofs/src/*/recovery.ts",
  "midgard-fault-proofs/src/*/runtime*.ts",
  "midgard-fault-proofs/src/*/v1.ts",
  "midgard-fault-proofs/src/*/walk-family.ts",
  "midgard-fault-proofs/src/field-preimage-length-mismatch/config.create-concrete-field-preimage-length-lucid-builders.ts",
];

// The workflow modules among the wallet-input preset owners.
const presetOwningWorkflowModules = [
  "midgard-fault-proofs/src/validation-trace-dispute/adapter.ts",
  "midgard-fault-proofs/src/validation-trace-dispute/transactions.ts",
  "midgard-fault-proofs/src/validation-trace-dispute/workflow.ts",
];

// The one submitter outside a workflow among the workflow modules.
const transactionBoundary =
  "midgard-fault-proofs/src/workflow/transaction-boundary.ts";

// The fault-proof sources that submit outside a workflow: the boundary
// itself, the diagnostic CLI and its from-files modules, the removal driver,
// the turn submitter and the field-grammar certifier.
const outsideWorkflowSubmitters = [
  transactionBoundary,
  "midgard-fault-proofs/src/bin*.ts",
  "midgard-fault-proofs/src/*-from-files.ts",
  "midgard-fault-proofs/src/remove-fraudulent-block.submit-remove-fraudulent-block.ts",
  "midgard-fault-proofs/src/submit-fault-proof-turns.ts",
  "midgard-fault-proofs/src/field-grammar-certificate.certify-fault-proof-field-grammar.ts",
];

// `submitRemoveFraudulentBlock` submits removals outside any workflow and owns
// their lease from acquisition to release; only the CLI removal module and
// proven-correction removal drive it.
const removalDriverRestrictions = [
  "ImportSpecifier[imported.name='submitRemoveFraudulentBlock']",
  "MemberExpression[property.name='submitRemoveFraudulentBlock']",
  "ObjectPattern > Property[key.name='submitRemoveFraudulentBlock']",
].map((selector) => ({
  selector,
  message:
    "submitRemoveFraudulentBlock submits a removal outside any workflow: only the CLI removal module and proven-correction removal drive it. Fix: build with buildRemoveFraudulentBlock and submit through the workflow's removal adapter.",
}));

export const faultProofRestrictedSyntaxConfigs = [
  // Fault-proof `no-restricted-syntax` gates. A later config object replaces
  // an earlier one's options for the same rule, so each object below lists
  // every restriction its files carry; the objects run from the widest file
  // set to the narrowest, and each allow-list object drops only its own
  // restriction.
  {
    // Production source sends fault-proof transactions through the workflow
    // runner. Submission outside a workflow belongs to the diagnostic CLI and
    // to the tests; a removal is submitted outside a workflow only by the
    // removal driver's callers.
    files: ["midgard-watcher/src/**/*.ts", "midgard-node/src/**/*.ts"],
    rules: {
      "no-restricted-syntax": [
        "error",
        ...outsideWorkflowSubmissionRestrictions,
        ...removalDriverRestrictions,
        ...turnSubmissionRestrictions,
      ],
    },
  },
  {
    files: ["midgard-fault-proofs/src/**/*.ts"],
    rules: {
      "no-restricted-syntax": [
        "error",
        ...outsideWorkflowSubmissionRestrictions,
        ...removalDriverRestrictions,
      ],
    },
  },
  {
    // A fault-proof builder's wallet inputs come from one rule. Lucid takes
    // collateral only from among preset wallet inputs, so a builder that
    // presets the wallet without narrowing it drops the collateral the
    // watcher's restricted wallet offers. The validation-trace dispute and the unattested-block
    // removal own deliberate presets of their own.
    files: ["midgard-fault-proofs/src/**/*.ts"],
    ignores: presetWalletInputsOwners,
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...outsideWorkflowSubmissionRestrictions,
        ...removalDriverRestrictions,
      ],
    },
  },
  {
    files: outsideWorkflowSubmitters,
    ignores: presetWalletInputsOwners,
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...removalDriverRestrictions,
      ],
    },
  },
  {
    // Workflow code takes each fault-proof turn through
    // workflowStepTransaction and never submits one itself.
    files: workflowModules,
    ignores: [...presetWalletInputsOwners, transactionBoundary],
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...outsideWorkflowSubmissionRestrictions,
        ...removalDriverRestrictions,
        ...turnSubmissionRestrictions,
      ],
    },
  },
  {
    files: presetOwningWorkflowModules,
    rules: {
      "no-restricted-syntax": [
        "error",
        ...outsideWorkflowSubmissionRestrictions,
        ...removalDriverRestrictions,
        ...turnSubmissionRestrictions,
      ],
    },
  },
  {
    files: [transactionBoundary],
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...removalDriverRestrictions,
        ...turnSubmissionRestrictions,
      ],
    },
  },
  {
    // The one CLI module among the preset owners: the unattested-block
    // timeout correction's.
    files: [
      "midgard-fault-proofs/src/remove-unattested-block.submit-unattested-timeout-correction-from-files.ts",
    ],
    rules: {
      "no-restricted-syntax": ["error", ...removalDriverRestrictions],
    },
  },
  {
    // The CLI removal module submits outside a workflow and drives the
    // removal.
    files: [
      "midgard-fault-proofs/src/remove-fraudulent-block.submit-remove-fraudulent-block-from-files.ts",
    ],
    rules: {
      "no-restricted-syntax": ["error", presetWalletInputsRestriction],
    },
  },
  {
    // Proven-correction removal drives the removal for the watcher.
    files: ["midgard-fault-proofs/src/proven-correction-removal.ts"],
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...outsideWorkflowSubmissionRestrictions,
      ],
    },
  },
];
