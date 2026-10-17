import { spend, withdraw } from "./recipes.types.js";

export const transitionTraceRecipes = {
  fraudProofTransitionTrace: spend(
    (c) => c.fraudProofContracts.transitionTrace.route,
  ),
  fraudProofTransitionTraceControl: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[0],
  ),
  fraudProofTransitionTraceSource: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[1],
  ),
  fraudProofTransitionTraceWithdrawal: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[2],
  ),
  fraudProofTransitionTraceForced: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[3],
  ),
  fraudProofTransitionTraceAcceptedTransaction: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[4],
  ),
  fraudProofTransitionTraceDeposit: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[5],
  ),
  fraudProofTransitionTraceL1Event: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[6],
  ),
  fraudProofTransitionTraceDuplicate: spend(
    (c) => c.fraudProofContracts.transitionTrace.finals[7],
  ),
  fraudProofTransitionTraceAcceptedTransactionL2OpenWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Open,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2DeleteWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Delete,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2OutputsWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Outputs,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2OutputStepWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2OutputStep,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2OutputDescriptorWithdraw:
    withdraw(
      (c) => c.fraudProofContracts.transitionTrace.yields.l2OutputDescriptor,
    ),
  fraudProofTransitionTraceAcceptedTransactionL2OutputSummaryWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2OutputSummary,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2SummariesWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Summaries,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2ReplayWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Replay,
  ),
  fraudProofTransitionTraceDepositProjectionWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.depositProjection,
  ),
  fraudProofTransitionTraceL1EventWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l1Event,
  ),
  fraudProofTransitionTraceForcedTimingWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.forcedTiming,
  ),
  fraudProofTransitionTraceDepositSummariesWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.depositSummaries,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2AssemblyWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Assembly,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2ScanWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Scan,
  ),
  fraudProofTransitionTraceAcceptedTransactionL2ValueWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.l2Value,
  ),
  fraudProofTransitionTraceDepositAssemblyWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.depositAssembly,
  ),
  fraudProofTransitionTraceDepositScanWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.depositScan,
  ),
  fraudProofTransitionTraceDepositValueWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.depositValue,
  ),
  fraudProofTransitionTraceDepositReplayWithdraw: withdraw(
    (c) => c.fraudProofContracts.transitionTrace.yields.depositReplay,
  ),
} as const;
