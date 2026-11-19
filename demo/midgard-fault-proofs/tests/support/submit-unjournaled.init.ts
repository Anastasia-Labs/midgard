/**
 * The init builders the tests drive, each bound into a function that builds its
 * step and submits it with no journal (see ./submit-unjournaled.ts).
 */
import { buildInit } from "../../src/submit-init.submit-init.js";
import { buildResolvedInit } from "../../src/submit-init.submit-resolved-init.js";
import { unjournaled } from "./submit-unjournaled.js";

export const submitInit = unjournaled(buildInit);
export const submitResolvedInit = unjournaled(buildResolvedInit);
