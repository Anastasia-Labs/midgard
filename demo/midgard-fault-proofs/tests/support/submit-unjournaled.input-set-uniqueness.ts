/**
 * The input-set-uniqueness builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import { buildInputSetUniquenessForcedStep01 } from "../../src/input-set-uniqueness/submit-input-set-uniqueness-forced-step-01.js";
import { buildInputSetUniquenessInit } from "../../src/input-set-uniqueness/submit-input-set-uniqueness-init.js";
import { buildInputSetUniquenessStep01 } from "../../src/input-set-uniqueness/submit-input-set-uniqueness-step-01.js";
import { buildInputSetUniquenessStep02 } from "../../src/input-set-uniqueness/submit-input-set-uniqueness-step-02.submit-input-set-uniqueness-step02.js";
import { buildInputSetUniquenessStep03 } from "../../src/input-set-uniqueness/submit-input-set-uniqueness-step-03.js";
import {
  buildInputSetUniquenessStep04Advance,
  buildInputSetUniquenessStep04Finalize,
} from "../../src/input-set-uniqueness/submit-input-set-uniqueness-step-04.js";
import { unjournaled, unjournaledTurns } from "./submit-unjournaled.js";

export const submitInputSetUniquenessForcedStep01 = unjournaled(
  buildInputSetUniquenessForcedStep01,
);
export const submitInputSetUniquenessInit = unjournaled(
  buildInputSetUniquenessInit,
);
export const submitInputSetUniquenessStep01 = unjournaled(
  buildInputSetUniquenessStep01,
);
export const submitInputSetUniquenessStep02 = unjournaledTurns(
  buildInputSetUniquenessStep02,
);
export const submitInputSetUniquenessStep03 = unjournaledTurns(
  buildInputSetUniquenessStep03,
);
export const submitInputSetUniquenessStep04Advance = unjournaledTurns(
  buildInputSetUniquenessStep04Advance,
);
export const submitInputSetUniquenessStep04Finalize = unjournaled(
  buildInputSetUniquenessStep04Finalize,
);
