import { type NetworkIdForcedScanStep } from "./forced-scan-plan.js";
import { networkIdSubmitError } from "./submit-common.js";
import {
  type SubmitNetworkIdForcedScanParams,
  type SubmitNetworkIdForcedScanResult,
} from "./submit-network-id-forced-scan.require-scan-state.js";
import {
  type DriveNetworkIdForcedScanResult,
  submitNetworkIdForcedScanAction,
} from "./submit-network-id-forced-scan.submit-network-id-forced-scan-action.js";

/**
 * Submits a planned scan in order, threading each transaction's successor
 * out-ref into the next. The last `Advance` hands the thread to step 02, so
 * the returned out-ref is the one step 02's finalization spends.
 */
export const driveNetworkIdForcedScan = async ({
  onStep,
  ...params
}: Omit<SubmitNetworkIdForcedScanParams, "threadOutRef"> & {
  readonly threadOutRef: string;
  /** Observation seam for suites that measure each submitted transaction. */
  readonly onStep?: (
    step: NetworkIdForcedScanStep,
    submit: () => Promise<SubmitNetworkIdForcedScanResult>,
  ) => Promise<SubmitNetworkIdForcedScanResult>;
}): Promise<DriveNetworkIdForcedScanResult> => {
  const results: SubmitNetworkIdForcedScanResult[] = [];
  let threadOutRef = params.threadOutRef;
  for (const step of params.scan.steps) {
    const submit = async () =>
      await submitNetworkIdForcedScanAction({
        ...params,
        threadOutRef,
        step,
      });
    const result =
      onStep === undefined ? await submit() : await onStep(step, submit);
    results.push(result);
    threadOutRef = result.nextThreadOutRef;
  }
  const completing = results.at(-1);
  if (completing?.step02State == null) {
    throw networkIdSubmitError(
      "forced scan finished without writing step 02's terminal state",
    );
  }
  return {
    results,
    step02ThreadOutRef: completing.nextThreadOutRef,
    step02State: completing.step02State,
  };
};
