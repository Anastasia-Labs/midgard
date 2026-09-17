import { RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION } from "../receive-purpose-language/manifest-workflow.js";
import { LINEAR_FAMILY_DEFINITIONS } from "./linear-family-definitions.js";

/**
 * The manifest-bound definitions whose adapter arm is a cursor spec, keyed
 * by category. Cursor categories have no closed list to guard against; the
 * table-driven assembly test checks each key against its definition.
 */
export const CURSOR_FAMILY_DEFINITIONS = Object.freeze({
  receivePurposeLanguage: RECEIVE_PURPOSE_LANGUAGE_FAMILY_DEFINITION,
});

/** Every definition the assembly builds, linear and cursor, keyed by category. */
export const FAMILY_DEFINITIONS = Object.freeze({
  ...LINEAR_FAMILY_DEFINITIONS,
  ...CURSOR_FAMILY_DEFINITIONS,
});

export type AssembledFamilyCategory = keyof typeof FAMILY_DEFINITIONS;
