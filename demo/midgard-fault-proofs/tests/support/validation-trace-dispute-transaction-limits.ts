import { expect } from "vitest";

import { EMULATOR_PROTOCOL_PARAMETERS } from "./emulator/protocol-parameters.js";
import {
  type CompleteSignedTransactionMeasurement,
  measureCompleteSignedTransaction,
} from "./submit-init-emulator-shared.js";

/** The signed transaction's size and units, within the emulator's limits. */
export const measureWithinTransactionLimits = (
  transactionCbor: string,
): CompleteSignedTransactionMeasurement => {
  const measured = measureCompleteSignedTransaction(transactionCbor);
  expect(measured.completeSignedBytes).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
  );
  expect(measured.executionMemory).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxExMem,
  );
  expect(measured.executionSteps).toBeLessThanOrEqual(
    EMULATOR_PROTOCOL_PARAMETERS.maxTxExSteps,
  );
  return measured;
};
