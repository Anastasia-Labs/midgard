export declare const clockTicks: () => number;
export declare const parseProcStatTicks: (text: string) => {
  readonly userTicks: number;
  readonly systemTicks: number;
};

export type ProcessCpuSample = {
  readonly atMs: number;
  readonly mainThreadTicks: number;
  readonly processTicks: number;
};

export declare const sampleProcessCpu: (
  pid: number,
  procRoot?: string,
) => ProcessCpuSample;

export declare const cpuShares: (
  before: ProcessCpuSample,
  after: ProcessCpuSample,
  ticksPerSecond?: number,
) => {
  readonly seconds: number;
  readonly mainThreadCores: number;
  readonly processCores: number;
};

export declare const writeReport: (file: string, report: object) => string;
export declare const summaryLines: (report: {
  readonly load: import("./submit-ingress-load.client.mjs").LoadResult;
  readonly postgres?: {
    readonly admissionRowsPerSecond: number;
    readonly tuplesInsertedPerSecond: number;
    readonly tuplesUpdatedPerSecond: number;
    readonly commitsPerSecond: number;
  };
  readonly nodeCpu?: {
    readonly mainThreadCores: number;
    readonly processCores: number;
  };
  readonly drainStub?: { readonly marked: number; readonly errors: number };
}) => string[];
