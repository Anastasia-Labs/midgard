/**
 * The builder half of the field grammar certificate. A field grammar
 * certificate lets a step read one item of a variable-width field without
 * walking the whole field, so the step's cost no longer grows with the
 * field's item count.
 *
 * `@al-ft/midgard-sdk`'s `fraud-proof/field-grammar-certificate.ts` owns the
 * wire shapes and the unsigned open/advance/reclaim programs. This module owns
 * the transactions: sizing each batch to the per-transaction budget, opening
 * and advancing a certificate to `Complete`, and reusing the prover's own
 * live one.
 */

import "@al-ft/midgard-core";
import "@al-ft/midgard-core/codec/native-tx-field-access";
import "@al-ft/midgard-sdk";
import "@lucid-evolution/lucid";
import "effect";
import "./runtime.js";
import "./workflow/transaction-boundary.js";
import "./field-opening.publish-fault-proof-field-carriage.js";
import "./field-grammar-certificate.certify-fault-proof-field-grammar.js";
import "./field-grammar-certificate.route-fault-proof-field-grammar.js";
import "./field-grammar-certificate.resolve-fault-proof-field-grammar-deployment.js";
import "./inspect-contracts.js";
import "./field-grammar-certificate.resolve-fault-proof-field-grammar-from-deployment-info.js";
export {
  type CertifiedFaultProofFieldGrammar,
  certifyFaultProofFieldGrammar,
  FIELD_GRAMMAR_NARROW_BATCH,
  FIELD_GRAMMAR_WIDE_BATCH,
  fieldGrammarBatchSize,
  fieldGrammarCertificateAddress,
  fieldGrammarOfCarriagePlan,
  findCompleteFaultProofFieldGrammarCertificate,
  planFieldGrammarBatches,
  reuseOrCertifyFaultProofFieldGrammar,
} from "./field-grammar-certificate.certify-fault-proof-field-grammar.js";
export {
  FIELD_GRAMMAR_CERTIFICATE_MINT,
  FIELD_GRAMMAR_CERTIFICATE_SPEND,
  journalFaultProofFieldGrammar,
  journalFaultProofFieldGrammarAfterCarriage,
  requireFaultProofFieldGrammarDeployment,
  resolveFaultProofFieldGrammarDeployment,
} from "./field-grammar-certificate.resolve-fault-proof-field-grammar-deployment.js";
export { resolveFaultProofFieldGrammarFromDeploymentInfo } from "./field-grammar-certificate.resolve-fault-proof-field-grammar-from-deployment-info.js";
export {
  certifiedFieldGrammarRoute,
  type FaultProofFieldGrammarDeployment,
  type FaultProofFieldGrammarRoute,
  FIELD_GRAMMAR_WALKED_COUNT_LIMIT,
  FIELD_GRAMMAR_WALKED_ITEM_LIMIT,
  type FieldGrammarRead,
  fieldGrammarRoute,
  routeFaultProofFieldGrammar,
  WALKED_FIELD_GRAMMAR_ROUTE,
} from "./field-grammar-certificate.route-fault-proof-field-grammar.js";
