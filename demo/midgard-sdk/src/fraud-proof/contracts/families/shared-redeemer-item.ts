import type { Network } from "@lucid-evolution/lucid";

import type { SpendingValidator } from "../../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeSpendingValidator,
} from "../blueprint.js";

const prefix =
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_";

/** Order is the on-chain authenticated family/action/stage roster. */
export const REDEEMER_ITEM_EXECUTOR_KEYS = [
  "fold_map_executor_v1",
  "finalize_frame_executor_v1",
  "open_header_executor",
  "open_tail_executor",
  "head_scalar_executor",
  "head_sequence_executor",
  "head_map_executor",
  "head_large_constructor_executor",
  "attach_integer_executor",
  "attach_bytes_executor",
  "fold_list_executor",
  "advance_integer_executor",
  "advance_bytes_executor",
  "advance_large_constructor_executor",
  "advance_large_fields_executor",
  "close_executor",
  "finish_data_executor",
] as const;

export type SharedRedeemerItemStages = {
  readonly entry: SpendingValidator;
  readonly traversalNormalizer: SpendingValidator;
  readonly outerNormalizer: SpendingValidator;
  readonly sourceAuthenticator: SpendingValidator;
  readonly executors: readonly SpendingValidator[];
  readonly settlement: SpendingValidator;
};

export type BuildCekRedeemerItemStagesParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly computationThreadPolicyId: string;
  readonly deploymentId: string;
  readonly returnScriptHash: string;
};

/** Build the shared item chain after the fixed CEK return tail, avoiding a hash cycle. */
const buildRedeemerItemStages = ({
  blueprint,
  network,
  computationThreadPolicyId,
  deploymentId,
  returnScriptHash,
  carrier,
}: BuildCekRedeemerItemStagesParams & {
  readonly carrier: "cek" | "scriptSources";
}): SharedRedeemerItemStages => {
  for (const [label, value, length] of [
    ["deployment", deploymentId, 64],
    ["thread policy", computationThreadPolicyId, 56],
    ["return script", returnScriptHash, 56],
  ] as const)
    if (value.length !== length || !/^[0-9a-f]+$/.test(value))
      throw new Error(`Invalid shared redeemer item ${label}`);
  const build = (
    key: string,
    params: Parameters<typeof applyBlueprintParams>[2],
  ) =>
    makeSpendingValidator(
      network,
      applyBlueprintParams(blueprint, `${prefix}${key}.main.spend`, params),
    );
  const base = [deploymentId, computationThreadPolicyId];
  const keys =
    carrier === "cek"
      ? REDEEMER_ITEM_EXECUTOR_KEYS
      : [
          ...REDEEMER_ITEM_EXECUTOR_KEYS,
          "invalid_header_executor",
          "invalid_tail_executor",
        ];
  const executors = keys.map((key) => build(key, base));
  const hashes = executors.map((validator) => validator.spendingScriptHash);
  if (hashes.length !== (carrier === "cek" ? 17 : 19))
    throw new Error("Shared CEK item executor roster is incomplete");
  const sourceAuthenticator = build("source_authenticator", base);
  const outerNormalizer = build("outer_normalizer_v1", [
    ...base,
    sourceAuthenticator.spendingScriptHash,
  ]);
  const traversalNormalizer = build("traversal_normalizer_v1", base);
  const settlement = build(
    carrier === "cek" ? "cek_settlement" : "execution_settlement_v1",
    [
      deploymentId,
      traversalNormalizer.spendingScriptHash,
      outerNormalizer.spendingScriptHash,
      hashes,
      returnScriptHash,
      computationThreadPolicyId,
    ],
  );
  const entry = build(carrier === "cek" ? "cek_envelope" : "envelope_v1", [
    deploymentId,
    traversalNormalizer.spendingScriptHash,
    outerNormalizer.spendingScriptHash,
    hashes,
    settlement.spendingScriptHash,
    computationThreadPolicyId,
  ]);
  return {
    entry,
    traversalNormalizer,
    outerNormalizer,
    sourceAuthenticator,
    executors,
    settlement,
  };
};

export const buildCekRedeemerItemStages = (
  params: BuildCekRedeemerItemStagesParams,
): SharedRedeemerItemStages =>
  buildRedeemerItemStages({ ...params, carrier: "cek" });

export type BuildScriptSourcesRedeemerItemStagesParams = Omit<
  BuildCekRedeemerItemStagesParams,
  "returnScriptHash"
> & { readonly awardScriptHash: string };
export const buildScriptSourcesRedeemerItemStages = ({
  awardScriptHash,
  ...params
}: BuildScriptSourcesRedeemerItemStagesParams): SharedRedeemerItemStages =>
  buildRedeemerItemStages({
    ...params,
    returnScriptHash: awardScriptHash,
    carrier: "scriptSources",
  });
