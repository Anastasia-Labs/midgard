/**
 * The zero-input builders the tests drive, each bound into a function that
 * builds its step and submits it with no journal (see ./submit-unjournaled.ts).
 */
import {
  buildZeroInputStep01Accepted,
  buildZeroInputStep01Forced,
} from "../../src/zero-input/submit-step-01.js";
import { buildZeroInputStep02V1 } from "../../src/zero-input/submit-step-02.js";
import { unjournaled } from "./submit-unjournaled.js";

export const submitZeroInputStep01Accepted = unjournaled(
  buildZeroInputStep01Accepted,
);
export const submitZeroInputStep01Forced = unjournaled(
  buildZeroInputStep01Forced,
);
export const submitZeroInputStep02V1 = unjournaled(buildZeroInputStep02V1);
