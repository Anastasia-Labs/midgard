/**
 * The zero-input builders the tests drive, each bound into a function that
 * builds its step and submits it with no journal (see ./submit-steps.ts).
 */
import {
  buildZeroInputStep01Accepted,
  buildZeroInputStep01Forced,
} from "../../src/zero-input/submit-step-01.js";
import { buildZeroInputStep02V1 } from "../../src/zero-input/submit-step-02.js";
import { stepSubmitter } from "./submit-steps.js";

export const submitZeroInputStep01Accepted = stepSubmitter(
  buildZeroInputStep01Accepted,
);
export const submitZeroInputStep01Forced = stepSubmitter(
  buildZeroInputStep01Forced,
);
export const submitZeroInputStep02V1 = stepSubmitter(buildZeroInputStep02V1);
