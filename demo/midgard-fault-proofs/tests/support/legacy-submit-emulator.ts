/**
 * Test-only access to the retired diagnostic builders.
 *
 * These routines are intentionally absent from the package barrel. Keeping
 * this import seam under tests preserves emulator construction coverage
 * without reintroducing a production submit API or a compatibility route.
 */
export { buildStep01 } from "../../src/double-spend/submit-step-01.js";
export { buildStep02 } from "../../src/double-spend/submit-step-02.js";
export {
  buildStep03,
  parseSpendInputCbors,
} from "../../src/double-spend/submit-step-03.js";
export { buildStep04 } from "../../src/double-spend/submit-step-04.js";
export { neSubmitStep01 } from "../../src/non-existent-input/submit-step-01.js";
export { neSubmitStep02 } from "../../src/non-existent-input/submit-step-02.js";
export { neSubmitStep03 } from "../../src/non-existent-input/submit-step-03.js";
export { neSubmitStep04 } from "../../src/non-existent-input/submit-step-04.js";
export {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../../src/step-support.js";
export { buildInit } from "../../src/submit-init.js";
export { buildInvalidRangeStep01 } from "../../src/submit-invalid-range-step-01.js";
export { buildInvalidRangeStep02V1 } from "../../src/submit-invalid-range-step-02.js";
export { buildZeroInputStep01 } from "../../src/submit-zero-input-step-01.js";
export { buildZeroInputStep02V1 } from "../../src/submit-zero-input-step-02.js";
export * from "./submit-unjournaled.legacy.js";
