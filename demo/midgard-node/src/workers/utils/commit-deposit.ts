export type WorkerInput = {
  data: {
    firstRun: boolean;
  };
};

export type SuccessfulRootCalculationOutput = {
  type: "SuccessfulRootCalculationOutput";
  mptRoot: string;
  inclusionTime: bigint;
};

export type FailedRootCalculationOutput = {
  type: "FailedRootCalculationOutput";
  error: string;
};

export type NoTxForRootCalculation = {
  type: "NoTxForCoRootCalculation"
};

export type WorkerOutput =
  | SuccessfulRootCalculationOutput
  | FailedRootCalculationOutput
  | NoTxForRootCalculation;
