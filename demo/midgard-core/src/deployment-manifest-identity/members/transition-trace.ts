import { defineDeploymentFamily } from "../members.types.js";

export const transitionTraceMembers = defineDeploymentFamily(
  "transitionTrace",
  {
    fraudProofTransitionTrace: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace route",
        token: "V1FpTransitionTraceRoute",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/route_v1.main.spend",
      familyPaths: [["firstStep"], ["route"], ["steps", "0"]],
    },
    fraudProofTransitionTraceControl: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-0",
        token: "V1FpTransitionTraceFinal0",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/control_v1.main.spend",
      familyPaths: [
        ["finals", "0"],
        ["steps", "1"],
      ],
    },
    fraudProofTransitionTraceSource: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-1",
        token: "V1FpTransitionTraceFinal1",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/source_v1.main.spend",
      familyPaths: [
        ["finals", "1"],
        ["steps", "2"],
      ],
    },
    fraudProofTransitionTraceWithdrawal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-2",
        token: "V1FpTransitionTraceFinal2",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/withdrawal_v1.main.spend",
      familyPaths: [
        ["finals", "2"],
        ["steps", "3"],
      ],
    },
    fraudProofTransitionTraceForced: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-3",
        token: "V1FpTransitionTraceFinal3",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/forced_v1.main.spend",
      familyPaths: [
        ["finals", "3"],
        ["steps", "4"],
      ],
    },
    fraudProofTransitionTraceAcceptedTransaction: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-4",
        token: "V1FpTransitionTraceFinal4",
      },
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
      reference: {
        role: "V1 fraud-proof transition-trace final-5",
        token: "V1FpTransitionTraceFinal5",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/deposit_v1.main.spend",
      familyPaths: [
        ["finals", "5"],
        ["steps", "6"],
      ],
    },
    fraudProofTransitionTraceL1Event: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-6",
        token: "V1FpTransitionTraceFinal6",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/l1_event_v1.main.spend",
      familyPaths: [
        ["finals", "6"],
        ["steps", "7"],
      ],
    },
    fraudProofTransitionTraceDuplicate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof transition-trace final-7",
        token: "V1FpTransitionTraceFinal7",
      },
      commands: [],
      validatorTitle: "fraud_proofs/transition_trace/duplicate_v1.main.spend",
      familyPaths: [
        ["finals", "7"],
        ["steps", "8"],
      ],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 open yield",
        token: "V1FpTtF4L2OpenYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_open.withdraw",
      familyPaths: [["yields", "l2Open"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2DeleteWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 delete yield",
        token: "V1FpTtF4L2DeleteYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_delete.withdraw",
      familyPaths: [["yields", "l2Delete"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputsWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 outputs yield",
        token: "V1FpTtF4L2OutputsYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_outputs.withdraw",
      familyPaths: [["yields", "l2Outputs"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputStepWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 output step yield",
        token: "V1FpTtF4L2OutputStepYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_output_step.withdraw",
      familyPaths: [["yields", "l2OutputStep"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputDescriptorWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 output descriptor yield",
        token: "V1FpTtF4OutputDescriptorYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_step.descriptor.withdraw",
      familyPaths: [["yields", "l2OutputDescriptor"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2OutputSummaryWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 output summary yield",
        token: "V1FpTtF4OutputSummaryYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_step.summary.withdraw",
      familyPaths: [["yields", "l2OutputSummary"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 summaries yield",
        token: "V1FpTtF4L2SummariesYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_summaries.summaries.withdraw",
      familyPaths: [["yields", "l2Summaries"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 replay yield",
        token: "V1FpTtF4L2ReplayYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
      familyPaths: [["yields", "l2Replay"]],
    },
    fraudProofTransitionTraceDepositProjectionWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-5 projection yield",
        token: "V1FpTtF5ProjectionYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_yields.projection.withdraw",
      familyPaths: [["yields", "depositProjection"]],
    },
    fraudProofTransitionTraceL1EventWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-6 L1 event yield",
        token: "V1FpTtF6L1EventYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/l1_event_yield.timing.withdraw",
      familyPaths: [["yields", "l1Event"]],
    },
    fraudProofTransitionTraceForcedTimingWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-6 forced timing yield",
        token: "V1FpTtF6ForcedTimeYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/l1_event_yield.forced_timing.withdraw",
      familyPaths: [["yields", "forcedTiming"]],
    },
    fraudProofTransitionTraceDepositSummariesWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-5 summaries yield",
        token: "V1FpTtF5SummariesYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_summaries.summaries.withdraw",
      familyPaths: [["yields", "depositSummaries"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 assembly yield",
        token: "V1FpTtF4L2AssemblyYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
      familyPaths: [["yields", "l2Assembly"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 scan yield",
        token: "V1FpTtF4ScanYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
      familyPaths: [["yields", "l2Scan"]],
    },
    fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-4 L2 value yield",
        token: "V1FpTtF4ValueYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_value.value_output.withdraw",
      familyPaths: [["yields", "l2Value"]],
    },
    fraudProofTransitionTraceDepositAssemblyWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-5 assembly yield",
        token: "V1FpTtF5AssemblyYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_assembly.assembly.withdraw",
      familyPaths: [["yields", "depositAssembly"]],
    },
    fraudProofTransitionTraceDepositScanWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-5 scan yield",
        token: "V1FpTtF5ScanYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/output_scan.scan_output.withdraw",
      familyPaths: [["yields", "depositScan"]],
    },
    fraudProofTransitionTraceDepositValueWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-5 value yield",
        token: "V1FpTtF5ValueYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/deposit_value.value_output.withdraw",
      familyPaths: [["yields", "depositValue"]],
    },
    fraudProofTransitionTraceDepositReplayWithdraw: {
      purpose: "withdraw",
      reference: {
        role: "V1 fraud-proof transition-trace final-5 replay yield",
        token: "V1FpTtF5ReplayYield",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/transition_trace/accepted_transaction_yields.l2_replay.withdraw",
      familyPaths: [["yields", "depositReplay"]],
    },
  },
);
