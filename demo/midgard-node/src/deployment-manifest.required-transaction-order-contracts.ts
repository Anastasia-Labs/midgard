import { MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION } from "@al-ft/midgard-core/consensus-profile";

export const DEPLOYMENT_MANIFEST_SCHEMA_VERSION =
  MIDGARD_DEPLOYMENT_MANIFEST_SCHEMA_VERSION;

export const REQUIRED_TRANSACTION_ORDER_CONTRACTS = Object.freeze([
  "txOrderSpend",
  "txOrderMint",
  // #579: all three retired tx-field names are removed here too. This vector
  // gates what a transaction-order deployment is REQUIRED to carry, so keeping a
  // name the regenerated blueprint cannot resolve would make every deployment
  // fail a requirement it has no way to satisfy.
  "cekProgramMaterialSpend",
  "validationTraceDispute",
  "validationTraceDisputeSource",
  "validationTraceDisputeGame",
  "validationTraceDisputeBoundary",
  "validationTraceDisputeTimeout",
  "validationTraceDisputeAward",
] as const);
