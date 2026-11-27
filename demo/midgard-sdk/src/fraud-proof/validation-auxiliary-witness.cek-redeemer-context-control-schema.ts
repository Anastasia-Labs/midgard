import "./validation-auxiliary-witness.ledger-output-proof-witness-schema.js";

import { Data } from "@lucid-evolution/lucid";

import { ProofSchema } from "../common.js";
import {
  DataSequenceSummarySchema,
  DataSummarySchema,
} from "./validation-auxiliary-witness.data-node-schema.js";

export const ValueAssetMutationWitnessSchema = Data.Object({
  delta_was_present: Data.Boolean(),
  old_delta: Data.Integer(),
  delta_proof: ProofSchema,
});

export const CekRedeemerContextControlSchema = Data.Object({
  cursor: Data.Integer(),
  map_items: DataSequenceSummarySchema,
  active_scan_hash: Data.Bytes(),
  active_redeemer_leaf: Data.Bytes(),
  active_purpose: DataSummarySchema,
  current_redeemer: DataSummarySchema,
  purpose_bound: Data.Integer(),
});

export const CekFinalContextControlSchema = Data.Object({
  tx_info: DataSummarySchema,
  redeemer: DataSummarySchema,
  script_info: DataSummarySchema,
});

export const CekContextPartsControlSchema = Data.Object({
  redeemer_items: DataSequenceSummarySchema,
  redeemer: DataSummarySchema,
  script_info: DataSummarySchema,
});

export const CekTxInfoAssemblyControlSchema = Data.Object({
  tail_fields: DataSequenceSummarySchema,
  redeemer: DataSummarySchema,
  script_info: DataSummarySchema,
});
