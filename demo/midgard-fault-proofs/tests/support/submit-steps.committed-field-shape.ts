/**
 * The committed-field-shape builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal (see
 * ./submit-steps.ts).
 */
import { buildCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import { stepSubmitter } from "./submit-steps.js";

export const submitCommittedFieldShapeInit = stepSubmitter(
  buildCommittedFieldShapeInit,
);
