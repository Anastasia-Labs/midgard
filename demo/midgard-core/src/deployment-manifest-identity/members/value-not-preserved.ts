import { defineDeploymentFamily } from "../members.types.js";

export const valueNotPreservedMembers = defineDeploymentFamily(
  "valueNotPreserved",
  {
    fraudProofValueNotPreserved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof value-not-preserved step-01",
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofValueNotPreservedStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof value-not-preserved step-02",
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofValueNotPreservedStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof value-not-preserved step-03",
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofValueNotPreservedStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof value-not-preserved step-04",
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofValueNotPreservedUnionAcceptedSource: {
      purpose: "spend",
      referenceRole: "value conservation accepted-source",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_accepted_source.main.spend",
      familyPaths: [["unionAcceptedSource"]],
    },
    fraudProofValueNotPreservedUnionForcedSource: {
      purpose: "spend",
      referenceRole: "value conservation forced-source",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_forced_source.main.spend",
      familyPaths: [["unionForcedSource"]],
    },
    fraudProofValueNotPreservedUnionEvent: {
      purpose: "spend",
      referenceRole: "value conservation event",
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/union_event.main.spend",
      familyPaths: [["unionEvent"]],
    },
    fraudProofValueNotPreservedUnionPreState: {
      purpose: "spend",
      referenceRole: "value conservation pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_pre_state.main.spend",
      familyPaths: [["unionPreState"]],
    },
    fraudProofValueNotPreservedUnionInputs: {
      purpose: "spend",
      referenceRole: "value conservation inputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_inputs.main.spend",
      familyPaths: [["unionInputs"]],
    },
    fraudProofValueNotPreservedUnionInputValue: {
      purpose: "spend",
      referenceRole: "value conservation input-value",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_input_value.main.spend",
      familyPaths: [["unionInputValue"]],
    },
    fraudProofValueNotPreservedUnionAssets: {
      purpose: "spend",
      referenceRole: "value conservation assets",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_assets.main.spend",
      familyPaths: [["unionAssets"]],
    },
    fraudProofValueNotPreservedUnionFieldGrammar: {
      purpose: "spend",
      referenceRole: "value conservation field-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_field_grammar.main.spend",
      familyPaths: [["unionFieldGrammar"]],
    },
    fraudProofValueNotPreservedUnionOutputs: {
      purpose: "spend",
      referenceRole: "value conservation outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_outputs.main.spend",
      familyPaths: [["unionOutputs"]],
    },
    fraudProofValueNotPreservedUnionOutputScan: {
      purpose: "spend",
      referenceRole: "value conservation output-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_output_scan.main.spend",
      familyPaths: [["unionOutputScan"]],
    },
    fraudProofValueNotPreservedUnionMint: {
      purpose: "spend",
      referenceRole: "value conservation mint",
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/union_mint.main.spend",
      familyPaths: [["unionMint"]],
    },
    fraudProofValueNotPreservedUnionUpdate: {
      purpose: "spend",
      referenceRole: "value conservation update",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_update.main.spend",
      familyPaths: [["unionUpdate"]],
    },
    fraudProofValueNotPreservedUnionTerminal: {
      purpose: "spend",
      referenceRole: "value conservation terminal",
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_terminal.main.spend",
      familyPaths: [["unionTerminal"]],
    },
  },
);
