import { defineDeploymentFamily } from "../members.types.js";

export const transitionTraceMembers = defineDeploymentFamily(
  "transitionTrace",
  {
    fraudProofTransitionTrace: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace route",
      manifestPosition: 375,
      publicationPosition: 281,
      referenceRolePosition: 125,
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/route_v1.main.spend",
      familyPaths: [["firstStep"], ["route"], ["steps", "0"]],
    },
    fraudProofTransitionTraceControl: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transition-trace final-0",
      manifestPosition: 376,
      publicationPosition: 282,
      referenceRolePosition: 126,
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
      manifestPosition: 377,
      publicationPosition: 283,
      referenceRolePosition: 127,
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
      manifestPosition: 378,
      publicationPosition: 284,
      referenceRolePosition: 128,
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
      manifestPosition: 379,
      publicationPosition: 285,
      referenceRolePosition: 129,
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
      manifestPosition: 380,
      publicationPosition: 286,
      referenceRolePosition: 130,
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
      manifestPosition: 381,
      publicationPosition: 287,
      referenceRolePosition: 131,
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
      manifestPosition: 382,
      publicationPosition: 288,
      referenceRolePosition: 132,
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
      manifestPosition: 383,
      publicationPosition: 289,
      referenceRolePosition: 133,
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
      manifestPosition: 502,
      publicationPosition: 503,
      referenceRolePosition: 380,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open.withdraw",
      familyPaths: [["yields", "l2Open"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 summaries yield",
      manifestPosition: 503,
      publicationPosition: 504,
      referenceRolePosition: 381,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_summaries.summaries.withdraw",
      familyPaths: [["yields", "l2Summaries"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 replay yield",
      manifestPosition: 504,
      publicationPosition: 505,
      referenceRolePosition: 382,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
      familyPaths: [["yields", "l2Replay"]],
    },
    fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 claim structure yield",
      manifestPosition: 505,
      publicationPosition: 506,
      referenceRolePosition: 383,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.claim_structure.withdraw",
      familyPaths: [["yields", "claimStructure"]],
    },
    fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 claim source yield",
      manifestPosition: 506,
      publicationPosition: 507,
      referenceRolePosition: 384,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.claim_source.withdraw",
      familyPaths: [["yields", "claimSource"]],
    },
    fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 claim endpoints yield",
      manifestPosition: 507,
      publicationPosition: 508,
      referenceRolePosition: 385,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.claim_endpoints.withdraw",
      familyPaths: [["yields", "claimEndpoints"]],
    },
    fraudProofTransitionTraceDepositProjectionWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 projection yield",
      manifestPosition: 508,
      publicationPosition: 509,
      referenceRolePosition: 386,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_yields.projection.withdraw",
      familyPaths: [["yields", "depositProjection"]],
    },
    fraudProofTransitionTraceL1EventWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-6 L1 event yield",
      manifestPosition: 509,
      publicationPosition: 510,
      referenceRolePosition: 387,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/l1_event_yield.timing.withdraw",
      familyPaths: [["yields", "l1Event"]],
    },
    fraudProofTransitionTraceForcedTimingWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-6 forced timing yield",
      manifestPosition: 510,
      publicationPosition: 511,
      referenceRolePosition: 388,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/l1_event_yield.forced_timing.withdraw",
      familyPaths: [["yields", "forcedTiming"]],
    },
    fraudProofTransitionTraceDepositSummariesWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 summaries yield",
      manifestPosition: 511,
      publicationPosition: 512,
      referenceRolePosition: 389,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_summaries.summaries.withdraw",
      familyPaths: [["yields", "depositSummaries"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw: {
      purpose: "withdraw",
      referenceRole:
        "V1 fraud-proof transition-trace final-4 L2 assembly yield",
      manifestPosition: 512,
      publicationPosition: 513,
      referenceRolePosition: 374,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
      familyPaths: [["yields", "l2Assembly"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 scan yield",
      manifestPosition: 513,
      publicationPosition: 514,
      referenceRolePosition: 375,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
      familyPaths: [["yields", "l2Scan"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-4 L2 value yield",
      manifestPosition: 514,
      publicationPosition: 515,
      referenceRolePosition: 376,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_value.value_output.withdraw",
      familyPaths: [["yields", "l2Value"]],
    },
    fraudProofTransitionTraceDepositAssemblyWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 assembly yield",
      manifestPosition: 515,
      publicationPosition: 516,
      referenceRolePosition: 377,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
      familyPaths: [["yields", "depositAssembly"]],
    },
    fraudProofTransitionTraceDepositScanWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 scan yield",
      manifestPosition: 516,
      publicationPosition: 517,
      referenceRolePosition: 378,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
      familyPaths: [["yields", "depositScan"]],
    },
    fraudProofTransitionTraceDepositValueWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 value yield",
      manifestPosition: 517,
      publicationPosition: 518,
      referenceRolePosition: 379,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_value.value_output.withdraw",
      familyPaths: [["yields", "depositValue"]],
    },
    fraudProofTransitionTraceDepositReplayWithdraw: {
      purpose: "withdraw",
      referenceRole: "V1 fraud-proof transition-trace final-5 replay yield",
      manifestPosition: 518,
      publicationPosition: 519,
      referenceRolePosition: 372,
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
      familyPaths: [["yields", "depositReplay"]],
    },
  },
);
