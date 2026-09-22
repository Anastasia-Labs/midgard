/** Authenticated deployment identities of the terminal semantic yields. */
export const TRANSITION_TRACE_YIELD_REFERENCES = {
  l2Open: {
    entry: "fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw",
    role: "V1 fraud-proof transition-trace final-4 L2 open yield",
  },
  l2Summaries: {
    entry: "fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw",
    role: "V1 fraud-proof transition-trace final-4 L2 summaries yield",
  },
  l2Replay: {
    entry: "fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw",
    role: "V1 fraud-proof transition-trace final-4 L2 replay yield",
  },
  claimStructure: {
    entry: "fraudProofTransitionTraceAcceptedTransactionClaimStructureWithdraw",
    role: "V1 fraud-proof transition-trace final-4 claim structure yield",
  },
  claimSource: {
    entry: "fraudProofTransitionTraceAcceptedTransactionClaimSourceWithdraw",
    role: "V1 fraud-proof transition-trace final-4 claim source yield",
  },
  claimEndpoints: {
    entry: "fraudProofTransitionTraceAcceptedTransactionClaimEndpointsWithdraw",
    role: "V1 fraud-proof transition-trace final-4 claim endpoints yield",
  },
  depositProjection: {
    entry: "fraudProofTransitionTraceDepositProjectionWithdraw",
    role: "V1 fraud-proof transition-trace final-5 projection yield",
  },
  depositSummaries: {
    entry: "fraudProofTransitionTraceDepositSummariesWithdraw",
    role: "V1 fraud-proof transition-trace final-5 summaries yield",
  },
  l2Assembly: {
    entry: "fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw",
    role: "V1 fraud-proof transition-trace final-4 L2 assembly yield",
  },
  l2Scan: {
    entry: "fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw",
    role: "V1 fraud-proof transition-trace final-4 L2 scan yield",
  },
  l2Value: {
    entry: "fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw",
    role: "V1 fraud-proof transition-trace final-4 L2 value yield",
  },
  depositAssembly: {
    entry: "fraudProofTransitionTraceDepositAssemblyWithdraw",
    role: "V1 fraud-proof transition-trace final-5 assembly yield",
  },
  depositScan: {
    entry: "fraudProofTransitionTraceDepositScanWithdraw",
    role: "V1 fraud-proof transition-trace final-5 scan yield",
  },
  depositValue: {
    entry: "fraudProofTransitionTraceDepositValueWithdraw",
    role: "V1 fraud-proof transition-trace final-5 value yield",
  },
  depositReplay: {
    entry: "fraudProofTransitionTraceDepositReplayWithdraw",
    role: "V1 fraud-proof transition-trace final-5 replay yield",
  },
} as const;
