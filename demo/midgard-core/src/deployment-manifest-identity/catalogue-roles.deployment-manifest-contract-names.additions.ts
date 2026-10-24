import { DEPLOYMENT_MANIFEST_BASE_CONTRACT_NAMES } from "./catalogue-roles.deployment-manifest-contract-names.js";

// Contract names appended after the base table, in manifest order.
const DEPLOYMENT_MANIFEST_CONTRACT_NAME_ADDITIONS = [
  "validationTraceDisputeCekCoreStagedBytes",
  "validationTraceDisputeCekCoreStagedInteger",
  "validationTraceDisputeCekCoreStagedProduct",
  "validationTraceDisputeCekCoreSemanticSerialiseLoop",
  "validationTraceDisputeCekCoreStagedFailure",
  "validationTraceDisputeCekCoreStagedConversion",
] as const;

export const DEPLOYMENT_MANIFEST_CONTRACT_NAMES = Object.freeze([
  ...DEPLOYMENT_MANIFEST_BASE_CONTRACT_NAMES,
  ...DEPLOYMENT_MANIFEST_CONTRACT_NAME_ADDITIONS,
] as const);
