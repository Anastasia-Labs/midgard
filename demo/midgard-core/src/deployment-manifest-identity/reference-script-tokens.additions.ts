import { DEPLOYMENT_MANIFEST_BASE_REFERENCE_TOKEN_NAMES } from "./reference-script-tokens.js";

// Reference-script token names added after the base table.
const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAME_ADDITIONS = {
  "V1 validation-trace CEK core staged bytes": "V1VtCekCoreStaByt",
  "V1 validation-trace CEK core staged integer": "V1VtCekCoreStaInt",
  "V1 validation-trace CEK core staged product": "V1VtCekCoreStaPro",
  "V1 validation-trace CEK core semantic serialise loop":
    "V1VtCekCoreSemSerLoo",
} as const;

export const DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAMES = Object.freeze({
  ...DEPLOYMENT_MANIFEST_BASE_REFERENCE_TOKEN_NAMES,
  ...DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_TOKEN_NAME_ADDITIONS,
} as const);
