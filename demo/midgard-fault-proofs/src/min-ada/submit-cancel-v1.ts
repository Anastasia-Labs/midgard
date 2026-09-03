import { submitLinearFaultCancel } from "../linear-fault-cancel-v1.js";
import {
  MIN_ADA_CATEGORY_LABEL,
  type MinAdaContracts,
} from "./contracts-v1.js";

type Base = Parameters<typeof submitLinearFaultCancel>[0];
export const submitMinAdaCancel = async (
  params: Omit<Base, "family" | "steps" | "computationThread"> & {
    readonly contracts: MinAdaContracts;
  },
) =>
  await submitLinearFaultCancel({
    ...params,
    family: MIN_ADA_CATEGORY_LABEL,
    steps: params.contracts.steps,
    computationThread: params.contracts.computationThread,
  });
