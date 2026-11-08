/**
 * The builder half of the field grammar certificate. A field grammar
 * certificate lets a step read one item of a variable-width field without
 * walking the whole field, so the step's cost no longer grows with the
 * field's item count.
 *
 * `@al-ft/midgard-sdk`'s `fraud-proof/field-grammar-certificate.ts` owns the
 * wire shapes and the unsigned open/advance/reclaim programs. This module owns
 * the transactions: sizing each batch to the per-transaction budget, opening
 * and advancing a certificate to `Complete`, and reusing a live one.
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
