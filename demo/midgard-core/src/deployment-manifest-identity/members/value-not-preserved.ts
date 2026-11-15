import { defineDeploymentFamily } from "../members.types.js";

export const valueNotPreservedMembers = defineDeploymentFamily(
  "valueNotPreserved",
  {
    fraudProofValueNotPreserved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof value-not-preserved step-01",
        token: "V1FpValueNotPreservedS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofValueNotPreservedStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof value-not-preserved step-02",
        token: "V1FpValueNotPreservedS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofValueNotPreservedStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof value-not-preserved step-03",
        token: "V1FpValueNotPreservedS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofValueNotPreservedStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof value-not-preserved step-04",
        token: "V1FpValueNotPreservedS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofValueNotPreservedUnionAcceptedSource: {
      purpose: "spend",
      reference: {
        role: "value conservation accepted-source",
        token: "ValueConservationAcceptedSource",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_accepted_source.main.spend",
      familyPaths: [["unionAcceptedSource"]],
    },
    fraudProofValueNotPreservedUnionForcedSource: {
      purpose: "spend",
      reference: {
        role: "value conservation forced-source",
        token: "ValueConservationForcedSource",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_forced_source.main.spend",
      familyPaths: [["unionForcedSource"]],
    },
    fraudProofValueNotPreservedUnionEvent: {
      purpose: "spend",
      reference: {
        role: "value conservation event",
        token: "ValueConservationEvent",
      },
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/union_event.main.spend",
      familyPaths: [["unionEvent"]],
    },
    fraudProofValueNotPreservedUnionPreState: {
      purpose: "spend",
      reference: {
        role: "value conservation pre-state",
        token: "ValueConservationPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_pre_state.main.spend",
      familyPaths: [["unionPreState"]],
    },
    fraudProofValueNotPreservedUnionInputs: {
      purpose: "spend",
      reference: {
        role: "value conservation inputs",
        token: "ValueConservationInputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_inputs.main.spend",
      familyPaths: [["unionInputs"]],
    },
    fraudProofValueNotPreservedUnionInputValue: {
      purpose: "spend",
      reference: {
        role: "value conservation input-value",
        token: "ValueConservationInputValue",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_input_value.main.spend",
      familyPaths: [["unionInputValue"]],
    },
    fraudProofValueNotPreservedUnionAssets: {
      purpose: "spend",
      reference: {
        role: "value conservation assets",
        token: "ValueConservationAssets",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_assets.main.spend",
      familyPaths: [["unionAssets"]],
    },
    fraudProofValueNotPreservedUnionFieldGrammar: {
      purpose: "spend",
      reference: {
        role: "value conservation field-grammar",
        token: "ValueConservationFieldGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_field_grammar.main.spend",
      familyPaths: [["unionFieldGrammar"]],
    },
    fraudProofValueNotPreservedUnionOutputs: {
      purpose: "spend",
      reference: {
        role: "value conservation outputs",
        token: "ValueConservationOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_outputs.main.spend",
      familyPaths: [["unionOutputs"]],
    },
    fraudProofValueNotPreservedUnionOutputScan: {
      purpose: "spend",
      reference: {
        role: "value conservation output-scan",
        token: "ValueConservationOutputScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_output_scan.main.spend",
      familyPaths: [["unionOutputScan"]],
    },
    fraudProofValueNotPreservedUnionMint: {
      purpose: "spend",
      reference: {
        role: "value conservation mint",
        token: "ValueConservationMint",
      },
      commands: [],
      validatorTitle: "fraud_proofs/value_not_preserved/union_mint.main.spend",
      familyPaths: [["unionMint"]],
    },
    fraudProofValueNotPreservedUnionUpdate: {
      purpose: "spend",
      reference: {
        role: "value conservation update",
        token: "ValueConservationUpdate",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_update.main.spend",
      familyPaths: [["unionUpdate"]],
    },
    fraudProofValueNotPreservedUnionTerminal: {
      purpose: "spend",
      reference: {
        role: "value conservation terminal",
        token: "ValueConservationTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/value_not_preserved/union_terminal.main.spend",
      familyPaths: [["unionTerminal"]],
    },
  },
);
