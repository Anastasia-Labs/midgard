/**
 * The committed-field-shape builders the tests drive, each bound into a
 * function that builds its step and submits it with no journal (see
 * ./submit-unjournaled.ts).
 */
import { buildCommittedFieldShapeInit } from "../../src/committed-field-shape/submit-committed-field-shape-init.js";
import { unjournaled } from "./submit-unjournaled.js";

export const submitCommittedFieldShapeInit = unjournaled(
  buildCommittedFieldShapeInit,
);
