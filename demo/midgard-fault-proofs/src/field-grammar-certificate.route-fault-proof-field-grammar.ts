import type { MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import { compareOutRefs, findOutRefIndex } from "@al-ft/midgard-core/out-ref";
import type {
  FieldGrammarCertificateSource,
  SpendingValidator,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

import { reuseOrCertifyFaultProofFieldGrammar } from "./field-grammar-certificate.certify-fault-proof-field-grammar.js";
import type { ResolvedProverSigner } from "./runtime.js";
import type { FraudProofPreSubmitBoundary } from "./workflow/transaction-boundary.js";

/**
 * The item count up to which a step reads a field's item by walking the
 * field in its own transaction. Past it the step reads the field's item
 * through the field grammar certificate, so its cost no longer grows with the
 * field's item count. Every step that reads an item by either route holds a
 * budget row for its walked route at this count, under tier 3 near the field
 * maximum to the last item (`script-budgets.json`, the
 * `walked_route_at_the_walked_limit` rows; the on-chain fixtures'
 * `walked_item_limit`). The item walk costs about 96,000 memory and 58M CPU
 * units per item, so 64 items leave every step inside the 80% execution
 * reserve.
 */
export const FIELD_GRAMMAR_WALKED_ITEM_LIMIT = 64;

/**
 * The item count up to which a step counts a field by walking it. A count
 * walk reads no item and costs about 35,000 memory and 10M CPU units per
 * item, so 256 items leave every counting step inside the 80% execution
 * reserve. Every step that counts a field by either route holds a budget row
 * for its walked count at this count, under tier 1 (the on-chain fixtures'
 * `walked_count_limit`).
 */
export const FIELD_GRAMMAR_WALKED_COUNT_LIMIT = 256;

/**
 * What a step reads from the field: one item, the field's item count, or
 * both (an item whose coordinate's domain is the field's count).
 */
export type FieldGrammarRead = "item" | "count" | "itemAndCount";

/**
 * The one route rule. A walk counts a field it carries inline or in one raw
 * UTxO but not a chunked (tier-3) one, and the certified count also serves a
 * chunked field. Past the walked limit for what the step reads every read is
 * certified.
 */
export const fieldGrammarRoute = ({
  read,
  tier,
  itemCount,
}: {
  readonly read: FieldGrammarRead;
  readonly tier: MidgardFieldCarriagePlan["tier"];
  readonly itemCount: number;
}): "walked" | "certified" =>
  itemCount >
    (read === "count"
      ? FIELD_GRAMMAR_WALKED_COUNT_LIMIT
      : FIELD_GRAMMAR_WALKED_ITEM_LIMIT) ||
  (read !== "item" && tier === "Certified")
    ? "certified"
    : "walked";

/** The deployed field grammar certificate a certified read runs against. */
export type FaultProofFieldGrammarDeployment = Readonly<{
  certificateValidator: Pick<
    SpendingValidator,
    "spendingScript" | "spendingScriptHash"
  >;
  certificateReferenceScriptUtxo: UTxO;
  preSubmitBoundary?: FraudProofPreSubmitBoundary;
}>;

/** The route a step's transaction takes and what its redeemer names. */
export type FaultProofFieldGrammarRoute = Readonly<{
  route: "walked" | "certified";
  /** The certificate the step references; empty on the walked route. */
  referenceInputs: readonly UTxO[];
  /**
   * The redeemer's `grammar_ref_input_index` against the built transaction's
   * reference inputs, which the ledger holds in canonical `(txHash,
   * outputIndex)` order whatever order the builder added them in.
   */
  grammarRefInputIndex: (referenceInputs: readonly UTxO[]) => bigint | null;
}>;

/** The walked route: no certificate, `grammar_ref_input_index = None`. */
export const WALKED_FIELD_GRAMMAR_ROUTE: FaultProofFieldGrammarRoute =
  Object.freeze({
    route: "walked",
    referenceInputs: Object.freeze([]),
    grammarRefInputIndex: () => null,
  });

/** A certified route through the live `Complete` certificate `utxo`. */
export const certifiedFieldGrammarRoute = (
  utxo: UTxO,
  label: string,
): FaultProofFieldGrammarRoute =>
  Object.freeze({
    route: "certified",
    referenceInputs: Object.freeze([utxo]),
    grammarRefInputIndex: (referenceInputs: readonly UTxO[]) => {
      const index = findOutRefIndex(
        [...referenceInputs].sort(compareOutRefs),
        utxo,
      );
      if (index === undefined)
        throw new Error(
          `${label}: the field grammar certificate is not a reference input`,
        );
      return BigInt(index);
    },
  });

/**
 * Applies the route rule to a step's field and, when it says certified,
 * reuses a live `Complete` certificate for the field or certifies it
 * (`reuseOrCertifyFaultProofFieldGrammar`). Certificate creation is the
 * prover's own transaction, funded from the prover wallet.
 *
 * A caller that deploys no field grammar certificate (`fieldGrammar`
 * absent) keeps the walked route at every count, as every step read its
 * field before the certified route existed.
 */
export const routeFaultProofFieldGrammar = async ({
  lucid,
  signer,
  plan,
  read,
  itemCount,
  source,
  carriageUtxos,
  fieldPreimageCertificatePolicyId,
  fieldGrammar,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly plan: MidgardFieldCarriagePlan;
  readonly read: FieldGrammarRead;
  readonly itemCount: number;
  readonly source: FieldGrammarCertificateSource;
  /** The field's publications and, under tier 3, its §8.6 certificate. */
  readonly carriageUtxos: readonly UTxO[];
  readonly fieldPreimageCertificatePolicyId: string;
  readonly fieldGrammar?: FaultProofFieldGrammarDeployment;
  readonly label: string;
}): Promise<FaultProofFieldGrammarRoute> => {
  if (
    fieldGrammar === undefined ||
    fieldGrammarRoute({ read, tier: plan.tier, itemCount }) === "walked"
  )
    return WALKED_FIELD_GRAMMAR_ROUTE;
  if (source.witnessSetCompactCbor === "")
    throw new Error(
      `${label}: the field grammar certificate authenticates the field against the whole compact transaction, which needs its compact witness set`,
    );
  const { certificateUtxo } = await reuseOrCertifyFaultProofFieldGrammar({
    lucid,
    network: lucid.config().network!,
    signer,
    plan,
    source,
    certificateValidator: fieldGrammar.certificateValidator,
    certificateReferenceScriptUtxo: fieldGrammar.certificateReferenceScriptUtxo,
    carriageUtxos,
    ...(plan.tier === "Certified" ? { fieldPreimageCertificatePolicyId } : {}),
    ...(fieldGrammar.preSubmitBoundary === undefined
      ? {}
      : { preSubmitBoundary: fieldGrammar.preSubmitBoundary }),
  });
  return certifiedFieldGrammarRoute(certificateUtxo, label);
};
