export type Phase2BenchmarkReport = Record<string, unknown>;

export declare const verifyStageBReport: (
  report: Phase2BenchmarkReport,
  options?: {
    readonly minimumAcceptedTps?: number;
    readonly minimumDurationMs?: number;
    readonly shortAssert?: boolean;
    readonly chunkSize?: number;
    readonly writeBehindMaxBatch?: number;
    readonly minimumReplicaAcceptedTps?: number;
    readonly minimumReplicaDurationMs?: number;
  },
) => Phase2BenchmarkReport;

export declare const verifyPhase2BenchmarkReports: (
  mode: string,
  reports: readonly Phase2BenchmarkReport[],
  options?: {
    readonly expectedFullCorpus?: {
      readonly sha256: string;
      readonly rowCount: number;
    };
  },
) => unknown;
