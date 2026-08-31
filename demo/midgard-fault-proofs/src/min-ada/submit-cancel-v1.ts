import { submitLinearFaultCancelV1 } from "../linear-fault-cancel-v1.js";
import {
  MIN_ADA_CATEGORY_LABEL,
  type MinAdaContractsV1,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancelV1>[0];
export const submitMinAdaCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: MinAdaContractsV1;
  },
) =>
  await submitLinearFaultCancelV1({
    ...params,
    family: MIN_ADA_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });
