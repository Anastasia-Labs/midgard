/**
 * The legacy builders the tests drive, each bound into a function that builds
 * its step and submits it with no journal (see ./submit-unjournaled.ts).
 */
import { buildStep01 } from "../../src/double-spend/submit-step-01.js";
import { buildStep02 } from "../../src/double-spend/submit-step-02.js";
import { buildStep03 } from "../../src/double-spend/submit-step-03.js";
import { buildStep04 } from "../../src/double-spend/submit-step-04.js";
import { neSubmitStep01 } from "../../src/non-existent-input/submit-step-01.js";
import { neSubmitStep02 } from "../../src/non-existent-input/submit-step-02.js";
import { neSubmitStep03 } from "../../src/non-existent-input/submit-step-03.js";
import { neSubmitStep04 } from "../../src/non-existent-input/submit-step-04.js";
import { buildInit } from "../../src/submit-init.js";
import { buildInvalidRangeStep01 } from "../../src/submit-invalid-range-step-01.js";
import { buildInvalidRangeStep02V1 } from "../../src/submit-invalid-range-step-02.js";
import { buildZeroInputStep01 } from "../../src/submit-zero-input-step-01.js";
import { buildZeroInputStep02V1 } from "../../src/submit-zero-input-step-02.js";
import { unjournaled, unjournaledTurns } from "./submit-unjournaled.js";

export const submitInit = unjournaled(buildInit);
export const submitInvalidRangeStep01 = unjournaled(buildInvalidRangeStep01);
export const submitInvalidRangeStep02V1 = unjournaled(
  buildInvalidRangeStep02V1,
);
export const submitNonExistentInputStep01 = unjournaled(neSubmitStep01);
export const submitNonExistentInputStep02 = unjournaled(neSubmitStep02);
export const submitNonExistentInputStep03 = unjournaled(neSubmitStep03);
export const submitNonExistentInputStep04 = unjournaled(neSubmitStep04);
export const submitStep01 = unjournaled(buildStep01);
export const submitStep02 = unjournaled(buildStep02);
export const submitStep03 = unjournaledTurns(buildStep03);
export const submitStep04 = unjournaledTurns(buildStep04);
export const submitZeroInputStep01 = unjournaled(buildZeroInputStep01);
export const submitZeroInputStep02V1 = unjournaled(buildZeroInputStep02V1);
