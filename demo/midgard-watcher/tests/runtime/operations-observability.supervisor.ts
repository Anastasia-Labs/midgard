import type {
  WatcherFaultProofSupervisor,
  WatcherFaultProofSupervisorStatus,
} from "../../src/fault-proofs/fault-proof-supervisor.js";

export const supervisor = () => {
  let status: WatcherFaultProofSupervisorStatus = Object.freeze({
    phase: "accepting",
    recovered: true,
    unfinishedObjectiveCount: 1,
    queuedJobCount: 1,
    activeJob: null,
    blockedJob: null,
    deadlineHealth: "safe",
    earliestDeadlineJob: null,
    remainingSafeStartMs: "1000000",
    journalIntegrity: null,
    journalUnavailable: null,
    journalCapacity: false,
    journalDecisionMissing: [],
    journalBusy: null,
  });
  return {
    runtime: Object.freeze({
      status: () => status,
    }) as unknown as WatcherFaultProofSupervisor,
    setStatus: (next: Partial<WatcherFaultProofSupervisorStatus>) => {
      status = Object.freeze({ ...status, ...next });
    },
  };
};
