/**
 * The input-set-uniqueness builders the tests drive, each bound into a function
 * that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import {
  buildInputSetUniquenessForcedStep01,
  buildInputSetUniquenessInit,
  buildInputSetUniquenessStep01,
  buildInputSetUniquenessStep02,
  buildInputSetUniquenessStep03,
  buildInputSetUniquenessStep04Advance,
  buildInputSetUniquenessStep04Finalize,
} from "../../src/input-set-uniqueness/index.js";
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
