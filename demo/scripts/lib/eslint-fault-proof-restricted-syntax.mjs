/**
 * The fault-proof `no-restricted-syntax` config objects, which
 * demo/eslint.config.mjs spreads in place: production source neither submits
 * unjournaled nor drives a removal outside its allow-lists, journaled workflow
 * code does not submit fault-proof turns, and a fault-proof builder does not
 * preset its own wallet inputs.
 *
 * Builders return their signed transaction and the journaled runner submits
 * it. Blind spots: a direct `signed.submit()` call, `submitCapturedTransaction`
 * (the journaled adapters submit through it, so it stays open), a computed
 * member access such as `module["submitUnjournaled"]`, a local function with a
 * restricted name (intent-journal.ts in midgard-node declares a
 * `submitUnjournaled` of its own), and a call through a renamed re-export.
 */

// A fault-proof builder that presets wallet inputs without narrowing them
// hides the restricted wallet's reserved collateral from Lucid.
const presetWalletInputsRestriction = {
  selector:
    "Property[key.name='presetWalletInputs'], Property[key.value='presetWalletInputs'], AssignmentExpression[left.property.name='presetWalletInputs']",
  message:
    "A fault-proof builder does not choose its own presetWalletInputs: a needless preset hides the wallet's reserved collateral from Lucid. Fix: spread the wallet inputs of stepWalletFunding or plainWalletInputs (src/funding/plain-funding-output.ts) into complete, passing what the transaction must not spend as `excluding`.",
};

// The fault-proof sources that preset wallet inputs deliberately.
const presetWalletInputsOwners = [
  "midgard-fault-proofs/src/funding/**",
  "midgard-fault-proofs/src/validation-trace-dispute/**",
  "midgard-fault-proofs/src/remove-unattested-block*",
];

// `submitUnjournaled` sends a transaction no workflow journal records, so
// production source never reaches it: import, re-export, member access or
// destructuring.
const unjournaledSubmissionRestrictions = [
  "ImportSpecifier[imported.name='submitUnjournaled']",
  "ExportSpecifier[local.name='submitUnjournaled']",
  "MemberExpression[property.name='submitUnjournaled']",
  "ObjectPattern > Property[key.name='submitUnjournaled']",
].map((selector) => ({
  selector,
  message:
    "submitUnjournaled sends a transaction no workflow journal records: only the diagnostic CLI, the removal driver, the turn submitter and the field-grammar certifier submit that way. Fix: return the signed transaction from the builder and let the journaled workflow runner, a CLI from-files module or a test submit it.",
}));

// The turn submitters send a builder's prerequisite and continuation turns
// unjournaled; journaled workflow code takes each turn through
// journaledStepTransaction instead, and only the diagnostic CLI submits them.
const turnSubmissionNames = [
  "submitPrerequisiteTurn",
  "settleFaultProofTurns",
  "submitFaultProofTurns",
];
const turnSubmissionRestrictions = turnSubmissionNames.flatMap((name) =>
  [
    `ImportSpecifier[imported.name='${name}']`,
    `ExportSpecifier[local.name='${name}']`,
    `MemberExpression[property.name='${name}']`,
    `ObjectPattern > Property[key.name='${name}']`,
  ].map((selector) => ({
    selector,
    message: `${name} submits fault-proof turns no workflow journal records: only the diagnostic CLI and its from-files modules submit turns that way. Fix: take the turn through journaledStepTransaction (src/workflow/step-transaction.ts), which refuses a prerequisite and journals a continuation as the step.`,
  })),
);

// The journaled workflow code of the fault proofs: the shared workflow
// modules and each family's workflow adapter, transaction port, actuator and
// recovery modules.
const journaledWorkflowModules = [
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

// The journaled workflow modules among the wallet-input preset owners.
const presetOwningWorkflowModules = [
  "midgard-fault-proofs/src/validation-trace-dispute/adapter.ts",
  "midgard-fault-proofs/src/validation-trace-dispute/transactions.ts",
  "midgard-fault-proofs/src/validation-trace-dispute/workflow.ts",
];

// The one unjournaled submitter among the journaled workflow modules.
const transactionBoundary =
  "midgard-fault-proofs/src/workflow/transaction-boundary.ts";

// The fault-proof sources that submit unjournaled: the boundary itself, the
// diagnostic CLI and its from-files modules, the removal driver, the turn
// submitter and the field-grammar certifier.
const unjournaledSubmitters = [
  transactionBoundary,
  "midgard-fault-proofs/src/bin*.ts",
  "midgard-fault-proofs/src/*-from-files.ts",
  "midgard-fault-proofs/src/remove-fraudulent-block.submit-remove-fraudulent-block.ts",
  "midgard-fault-proofs/src/submit-fault-proof-turns.ts",
  "midgard-fault-proofs/src/field-grammar-certificate.certify-fault-proof-field-grammar.ts",
];

// `submitRemoveFraudulentBlock` submits removals unjournaled and owns their
// lease from acquisition to release; only the CLI removal module and
// proven-correction removal drive it.
const removalDriverRestrictions = [
  "ImportSpecifier[imported.name='submitRemoveFraudulentBlock']",
  "MemberExpression[property.name='submitRemoveFraudulentBlock']",
  "ObjectPattern > Property[key.name='submitRemoveFraudulentBlock']",
].map((selector) => ({
  selector,
  message:
    "submitRemoveFraudulentBlock submits a removal no workflow journal records: only the CLI removal module and proven-correction removal drive it. Fix: build with buildRemoveFraudulentBlock and submit through the journaled removal adapter.",
}));

export const faultProofRestrictedSyntaxConfigs = [
  // Fault-proof `no-restricted-syntax` gates. A later config object replaces
  // an earlier one's options for the same rule, so each object below lists
  // every restriction its files carry; the objects run from the widest file
  // set to the narrowest, and each allow-list object drops only its own
  // restriction.
  {
    // Production source sends fault-proof transactions through the journaled
    // workflow runner. Unjournaled submission belongs to the diagnostic CLI
    // and to the tests; a removal is submitted unjournaled only by the
    // removal driver's callers.
    files: ["midgard-watcher/src/**/*.ts", "midgard-node/src/**/*.ts"],
    rules: {
      "no-restricted-syntax": [
        "error",
        ...unjournaledSubmissionRestrictions,
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
        ...unjournaledSubmissionRestrictions,
        ...removalDriverRestrictions,
      ],
    },
  },
  {
    // A fault-proof builder's wallet inputs come from one rule. Lucid takes
    // collateral only from among preset wallet inputs, so a builder that
    // presets the wallet without narrowing it drops the collateral the
    // watcher's restricted wallet reserves, and the reservation refuses the
    // transaction. The validation-trace dispute and the unattested-block
    // removal own deliberate presets of their own.
    files: ["midgard-fault-proofs/src/**/*.ts"],
    ignores: presetWalletInputsOwners,
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...unjournaledSubmissionRestrictions,
        ...removalDriverRestrictions,
      ],
    },
  },
  {
    files: unjournaledSubmitters,
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
    // Journaled workflow code takes each fault-proof turn through
    // journaledStepTransaction and never submits one itself.
    files: journaledWorkflowModules,
    ignores: [...presetWalletInputsOwners, transactionBoundary],
    rules: {
      "no-restricted-syntax": [
        "error",
        presetWalletInputsRestriction,
        ...unjournaledSubmissionRestrictions,
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
        ...unjournaledSubmissionRestrictions,
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
    // The CLI removal module submits unjournaled and drives the removal.
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
        ...unjournaledSubmissionRestrictions,
      ],
    },
  },
];
