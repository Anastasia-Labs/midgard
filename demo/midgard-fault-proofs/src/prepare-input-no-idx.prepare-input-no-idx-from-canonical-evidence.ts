import {
  admitCanonicalEvidenceForProofBuild,
  type CanonicalBlockEvidence,
} from "./evidence/index.js";
import { type PreparedInputNoIdxOutput } from "./prepare-input-no-idx.midgard-tx-output-from-canonical-cbor.js";
import { prepareInputNoIdxFromTransactions } from "./prepare-input-no-idx.prepare-input-no-idx-from-transactions.js";

/**
 * The security-grade entry point (§9.2/§9.1 output 7): an authenticated L1
 * header observation plus public retained-DA block evidence, and nothing else.
 *
 * `admitCanonicalEvidenceForProofBuild` re-runs both Q03 gates —
 * `assertSecurityGradeEvidenceV1` over every contributing provenance record
 * and `assertNativeInclusionRootAuthenticatedV1` over the raw transactions MPF
 * root the family's two `NativeTxInclusionArgs` will carry — so a diagnostic,
 * operator-private, or unauthenticated-root record can never reach a
 * submittable `input-no-idx` proof.
 */
export const prepareInputNoIdxFromCanonicalEvidence = async ({
  evidence,
  badTxId,
  badInputsIndex,
  outputDir,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly badTxId?: string;
  readonly badInputsIndex?: string | number;
  readonly outputDir?: string;
}): Promise<PreparedInputNoIdxOutput> => {
  const admitted = admitCanonicalEvidenceForProofBuild(evidence);
  return await prepareInputNoIdxFromTransactions({
    headerHash: admitted.headerHash,
    transactions: admitted.transactions,
    expectedTransactionsRoot: admitted.expectedTransactionsRoot,
    ...(badTxId === undefined ? {} : { badTxId }),
    ...(badInputsIndex === undefined ? {} : { badInputsIndex }),
    ...(outputDir === undefined ? {} : { outputDir }),
  });
};
