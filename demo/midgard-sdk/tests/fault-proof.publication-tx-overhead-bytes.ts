import { readFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import {
  Data,
  PROTOCOL_PARAMETERS_DEFAULT,
  type SpendingValidator as LucidSpendingValidator,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import * as SDK from "@/index.js";

import {
  type FaultProofBlueprint,
  parseFaultProofBlueprint,
  type Proof,
  REDEEMER_ITEM_EXECUTOR_KEYS,
} from "../src/index.js";

const moduleDir = dirname(fileURLToPath(import.meta.url));

const repoRoot = resolve(moduleDir, "../../..");

const currentTreeBlueprintPath = process.env.MIDGARD_REAL_BLUEPRINT_PATH;

export const blueprintPath =
  currentTreeBlueprintPath ?? resolve(repoRoot, "onchain/aiken/plutus.json");

export const h32 = "00".repeat(32);

export const h32b = "11".repeat(32);

export const h28 = "22".repeat(28);

export const h28b = "33".repeat(28);

export const h28c = "44".repeat(28);

export const nativeTxBody = {
  spend_inputs_hash: h32,
  reference_inputs_hash: h32,
  outputs_hash: h32,
  fee: 0n,
  validity_interval_start: -1n,
  validity_interval_end: -1n,
  required_observers_hash: h32,
  required_signers_hash: h32,
  mint_hash: h32,
  script_integrity_hash: h32,
  auxiliary_data_hash: h32,
  network_id: 0n,
};

export const proof: Proof = [];

export const spendInputs = [
  { tx_id: "aa".repeat(32), output_index: 0n },
  { tx_id: "bb".repeat(32), output_index: 1n },
];

export const doubleSpentInput = spendInputs[0]!;

export const txInclusionArgs = {
  input_index: 0n,
  output_index: 0n,
  hub_ref_input_index: 1n,
  state_queue_node_ref_input_index: 2n,
  native_tx_id: h32,
  l2_transaction_source_cbor: "80",
  transactions_phas_root: h32,
  tx_membership_proof: proof,
  inclusion_proof_script_withdraw_redeemer_index: 3n,
};

export const roundTrip = <A>(
  value: A,
  schema: Parameters<typeof Data.to>[1],
): A => Data.from(Data.to(value, schema), schema) as A;

export const loadBlueprint = (): FaultProofBlueprint =>
  parseFaultProofBlueprint(
    JSON.parse(readFileSync(blueprintPath, "utf8")) as unknown,
  );

/**
 * Flattens a nested production title constant into its string leaves, so a
 * blueprint allowlist is derived from the constants the builder itself reads
 * instead of being hand-transcribed (and going stale).
 */
export const collectTitles = (node: unknown): readonly string[] =>
  typeof node === "string"
    ? [node]
    : Object.values(node as Record<string, unknown>).flatMap(collectTitles);

/**
 * The CEK material-traversal validators are referenced by literal title inside
 * `buildValidationTraceDisputeFaultProofContracts`; there is no exported
 * constant to derive them from.
 */
/**
 * Publication budget for a single applied fault-proof validator, expressed as
 * the deployment constraint it stands for rather than as a round number.
 *
 * `demo/midgard-fault-proofs/tests/validation-trace-resolver-publication.test.ts`
 * is the authoritative measurement: it publishes each applied validator as a
 * reference script through the Lucid emulator at the default protocol
 * parameters and requires at least a 512-byte L1 margin
 * (`VAN_ROSSEM_PUBLICATION_RESERVE_BYTES`) on the complete signed transaction.
 * Every row of the recorded ledger
 * (`docs/fault-proofs/size-plans/validation-trace-resolver-publication-fit-ledger.json`)
 * shows that publication transaction costing a constant 276 bytes over the
 * applied script it carries, so the equivalent bound on applied script bytes
 * -- which is all this suite can see without an emulator -- is
 * `maxTxSize - reserve - overhead`.
 */
const PUBLICATION_TX_OVERHEAD_BYTES = 276;

const PUBLICATION_RESERVE_BYTES = 512;

export const MAX_APPLIED_SCRIPT_BYTES =
  PROTOCOL_PARAMETERS_DEFAULT.maxTxSize -
  PUBLICATION_RESERVE_BYTES -
  PUBLICATION_TX_OVERHEAD_BYTES;

const REDEEMER_ITEM_PREFIX =
  "fraud_proofs/validation_trace/script_sources_stage_one_redeemer_";

/**
 * The CEK carrier of the shared redeemer-item chain (built by
 * `buildCekRedeemerItemStages`) composes its titles from
 * `REDEEMER_ITEM_EXECUTOR_KEYS` plus the CEK-only envelope/settlement pair, so
 * they are derived here the same way the builder derives them.
 */
export const CEK_REDEEMER_ITEM_TITLES = [
  ...REDEEMER_ITEM_EXECUTOR_KEYS,
  "source_authenticator",
  "outer_normalizer_v1",
  "traversal_normalizer_v1",
  "cek_settlement",
  "cek_envelope",
].map((key) => `${REDEEMER_ITEM_PREFIX}${key}.main.spend`);

export const CEK_MATERIAL_TRAVERSAL_TITLES = [
  "fraud_proofs/validation_trace/cek_material_traversal_v1.main.spend",
  "fraud_proofs/validation_trace/cek_material_traversal_yields.program.withdraw",
  "fraud_proofs/validation_trace/cek_material_traversal_yields.data.withdraw",
] as const;

export const filterBlueprint = (
  blueprint: FaultProofBlueprint,
  titles: readonly string[],
): FaultProofBlueprint => {
  const titleSet = new Set(titles);
  return {
    validators: blueprint.validators.filter((validator) =>
      titleSet.has(validator.title),
    ),
  };
};

export const compiledScript = (
  blueprint: FaultProofBlueprint,
  title: string,
): string => {
  const validator = blueprint.validators.find((entry) => entry.title === title);
  if (validator === undefined) {
    throw new Error(`Missing validator ${title}`);
  }
  return validator.compiledCode;
};

export const spendingScript = (script: string): LucidSpendingValidator => ({
  type: "PlutusV3",
  script,
});

export const spendingScriptHash = (script: string): string =>
  validatorToScriptHash(spendingScript(script));

/**
 * The §8.6 certificate policy id, derived here from the blueprint rather than
 * read off the builder's output, so these expectations stay an independent
 * re-derivation. The certificate validator declares no parameters, so its
 * compiled script is its deployed script and the script hash of that script is
 * the policy id.
 */
export const certificatePolicyId = (blueprint: FaultProofBlueprint): string =>
  spendingScriptHash(
    compiledScript(
      blueprint,
      SDK.FAULT_PROOF_SHARED_TITLES.fieldPreimageCertificateMint,
    ),
  );
