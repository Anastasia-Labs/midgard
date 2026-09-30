type FitMeasurement = ReturnType<
  typeof import("./support/emulator/measurement.js").measureCompleteSignedTransaction
>;

export const fitRows: {
  shape: string;
  stages: readonly FitMeasurement[];
  scriptIndex?: bigint;
  scriptFieldBytes?: number;
  signerFieldBytes?: number;
}[] = [];
