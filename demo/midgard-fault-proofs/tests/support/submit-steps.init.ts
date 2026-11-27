/**
 * The init builders the tests drive, each bound into a function that builds its
 * step and submits it with no journal (see ./submit-steps.ts).
 */
import { buildInit } from "../../src/submit-init.submit-init.js";
import { buildResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import { stepSubmitter } from "./submit-steps.js";

export const submitInit = stepSubmitter(buildInit);
export const submitResolvedInit = stepSubmitter(buildResolvedInit);
