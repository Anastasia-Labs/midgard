import { defineDeploymentFamily } from "../members.types.js";

export const transitionTraceMembers = defineDeploymentFamily(
  "transitionTrace",
  {
    fraudProofTransitionTrace: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace route",
      manifestPosition: 445,
      publicationPosition: 351,
      referenceRolePosition: 132,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/route_v1.main.spend",
      familyPaths: [["firstStep"], ["route"], ["steps", "0"]],
    },
    fraudProofTransitionTraceControl: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-0",
      manifestPosition: 446,
      publicationPosition: 352,
      referenceRolePosition: 133,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/control_v1.main.spend",
      familyPaths: [
        ["finals", "0"],
        ["steps", "1"],
      ],
    },
    fraudProofTransitionTraceSource: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-1",
      manifestPosition: 447,
      publicationPosition: 353,
      referenceRolePosition: 134,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/source_v1.main.spend",
      familyPaths: [
        ["finals", "1"],
        ["steps", "2"],
      ],
    },
    fraudProofTransitionTraceWithdrawal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-2",
      manifestPosition: 448,
      publicationPosition: 354,
      referenceRolePosition: 135,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/withdrawal_v1.main.spend",
      familyPaths: [
        ["finals", "2"],
        ["steps", "3"],
      ],
    },
    fraudProofTransitionTraceForced: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-3",
      manifestPosition: 449,
      publicationPosition: 355,
      referenceRolePosition: 136,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/forced_v1.main.spend",
      familyPaths: [
        ["finals", "3"],
        ["steps", "4"],
      ],
    },
    fraudProofTransitionTraceAcceptedTransaction: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-4",
      manifestPosition: 450,
      publicationPosition: 356,
      referenceRolePosition: 137,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_v1.main.spend",
      familyPaths: [
        ["finals", "4"],
        ["steps", "5"],
      ],
    },
    fraudProofTransitionTraceDeposit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-5",
      manifestPosition: 451,
      publicationPosition: 357,
      referenceRolePosition: 138,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/deposit_v1.main.spend",
      familyPaths: [
        ["finals", "5"],
        ["steps", "6"],
      ],
    },
    fraudProofTransitionTraceL1Event: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-6",
      manifestPosition: 452,
      publicationPosition: 358,
      referenceRolePosition: 139,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/l1_event_v1.main.spend",
      familyPaths: [
        ["finals", "6"],
        ["steps", "7"],
      ],
    },
    fraudProofTransitionTraceDuplicate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-7",
      manifestPosition: 453,
      publicationPosition: 359,
      referenceRolePosition: 140,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/duplicate_v1.main.spend",
      familyPaths: [
        ["finals", "7"],
        ["steps", "8"],
      ],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 open yield",
      manifestPosition: 568,
      publicationPosition: 569,
      referenceRolePosition: 383,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open.withdraw",
      familyPaths: [["yields", "l2Open"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2DeleteWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 delete yield",
      manifestPosition: 569,
      publicationPosition: 570,
      referenceRolePosition: 384,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_delete.withdraw",
      familyPaths: [["yields", "l2Delete"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputsWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 outputs yield",
      manifestPosition: 570,
      publicationPosition: 571,
      referenceRolePosition: 385,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_outputs.withdraw",
      familyPaths: [["yields", "l2Outputs"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputStepWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 output step yield",
      manifestPosition: 571,
      publicationPosition: 572,
      referenceRolePosition: 386,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_output_step.withdraw",
      familyPaths: [["yields", "l2OutputStep"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputDescriptorWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 output descriptor yield",
      manifestPosition: 572,
      publicationPosition: 573,
      referenceRolePosition: 387,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_step.descriptor.withdraw",
      familyPaths: [["yields", "l2OutputDescriptor"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputSummaryWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 output summary yield",
      manifestPosition: 573,
      publicationPosition: 574,
      referenceRolePosition: 388,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_step.summary.withdraw",
      familyPaths: [["yields", "l2OutputSummary"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 summaries yield",
      manifestPosition: 574,
      publicationPosition: 575,
      referenceRolePosition: 389,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_summaries.summaries.withdraw",
      familyPaths: [["yields", "l2Summaries"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 replay yield",
      manifestPosition: 575,
      publicationPosition: 576,
      referenceRolePosition: 390,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
      familyPaths: [["yields", "l2Replay"]],
    },
    fraudProofTransitionTraceDepositProjectionWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 projection yield",
      manifestPosition: 576,
      publicationPosition: 577,
      referenceRolePosition: 391,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_yields.projection.withdraw",
      familyPaths: [["yields", "depositProjection"]],
    },
    fraudProofTransitionTraceL1EventWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-6 L1 event yield",
      manifestPosition: 577,
      publicationPosition: 578,
      referenceRolePosition: 392,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/l1_event_yield.timing.withdraw",
      familyPaths: [["yields", "l1Event"]],
    },
    fraudProofTransitionTraceForcedTimingWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-6 forced timing yield",
      manifestPosition: 578,
      publicationPosition: 579,
      referenceRolePosition: 393,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/l1_event_yield.forced_timing.withdraw",
      familyPaths: [["yields", "forcedTiming"]],
    },
    fraudProofTransitionTraceDepositSummariesWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 summaries yield",
      manifestPosition: 579,
      publicationPosition: 580,
      referenceRolePosition: 394,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_summaries.summaries.withdraw",
      familyPaths: [["yields", "depositSummaries"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 assembly yield",
      manifestPosition: 580,
      publicationPosition: 581,
      referenceRolePosition: 377,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
      familyPaths: [["yields", "l2Assembly"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 scan yield",
      manifestPosition: 581,
      publicationPosition: 582,
      referenceRolePosition: 378,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
      familyPaths: [["yields", "l2Scan"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 value yield",
      manifestPosition: 582,
      publicationPosition: 583,
      referenceRolePosition: 379,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_value.value_output.withdraw",
      familyPaths: [["yields", "l2Value"]],
    },
    fraudProofTransitionTraceDepositAssemblyWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 assembly yield",
      manifestPosition: 583,
      publicationPosition: 584,
      referenceRolePosition: 380,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
      familyPaths: [["yields", "depositAssembly"]],
    },
    fraudProofTransitionTraceDepositScanWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 scan yield",
      manifestPosition: 584,
      publicationPosition: 585,
      referenceRolePosition: 381,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
      familyPaths: [["yields", "depositScan"]],
    },
    fraudProofTransitionTraceDepositValueWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 value yield",
      manifestPosition: 585,
      publicationPosition: 586,
      referenceRolePosition: 382,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_value.value_output.withdraw",
      familyPaths: [["yields", "depositValue"]],
    },
    fraudProofTransitionTraceDepositReplayWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 replay yield",
      manifestPosition: 586,
      publicationPosition: 587,
      referenceRolePosition: 375,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
      familyPaths: [["yields", "depositReplay"]],
    },
  },
);
