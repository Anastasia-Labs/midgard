/**
 * The legacy builders the tests drive, each bound into a function that builds
 * its step and submits it with no journal (see ./submit-steps.ts).
 */
import { buildStep01 } from "../../src/double-spend/submit-step-01.js";
import { buildStep02 } from "../../src/double-spend/submit-step-02.js";
import { buildStep03 } from "../../src/double-spend/submit-step-03.js";
import { buildStep04 } from "../../src/double-spend/submit-step-04.submit-step04.js";
import { neSubmitStep01 } from "../../src/non-existent-input/submit-step-01.js";
import { neSubmitStep02 } from "../../src/non-existent-input/submit-step-02.js";
import { neSubmitStep03 } from "../../src/non-existent-input/submit-step-03.js";
import { neSubmitStep04 } from "../../src/non-existent-input/submit-step-04.ne-submit-step04.js";
import { buildInit } from "../../src/submit-init.submit-init.js";
import { buildInvalidRangeStep01 } from "../../src/submit-invalid-range-step-01.submit-invalid-range-step01.js";
import { buildInvalidRangeStep02V1 } from "../../src/submit-invalid-range-step-02.submit-invalid-range-step02-v1.js";
import { buildZeroInputStep01 } from "../../src/submit-zero-input-step-01.submit-zero-input-step01.js";
import { buildZeroInputStep02V1 } from "../../src/submit-zero-input-step-02.submit-zero-input-step02-v1.js";
import { stepSubmitter, turnsSubmitter } from "./submit-steps.js";

export const submitInit = stepSubmitter(buildInit);
export const submitInvalidRangeStep01 = stepSubmitter(buildInvalidRangeStep01);
export const submitInvalidRangeStep02V1 = stepSubmitter(
  buildInvalidRangeStep02V1,
);
export const submitNonExistentInputStep01 = stepSubmitter(neSubmitStep01);
export const submitNonExistentInputStep02 = stepSubmitter(neSubmitStep02);
export const submitNonExistentInputStep03 = stepSubmitter(neSubmitStep03);
export const submitNonExistentInputStep04 = stepSubmitter(neSubmitStep04);
export const submitStep01 = stepSubmitter(buildStep01);
export const submitStep02 = stepSubmitter(buildStep02);
export const submitStep03 = turnsSubmitter(buildStep03);
export const submitStep04 = turnsSubmitter(buildStep04);
export const submitZeroInputStep01 = stepSubmitter(buildZeroInputStep01);
export const submitZeroInputStep02V1 = stepSubmitter(buildZeroInputStep02V1);
