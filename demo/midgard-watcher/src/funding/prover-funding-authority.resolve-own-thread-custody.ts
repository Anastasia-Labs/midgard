import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_IDS,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { Constr, Data, type UTxO, utxoToCore } from "@lucid-evolution/lucid";

/** The lineage kind of thread custody proved from the chain alone. */
export const OBSERVED_THREAD_CUSTODY_ACTION_KIND =
  "computation_thread.observed";

/**
 * Released custody for a contract input with no stored lineage, proved from
 * the chain alone: the authenticated L1 source shows the output carrying this
 * objective's computation-thread token (this deployment's policy, asset name
 * category id ++ header hash, quantity 1, the only non-ADA asset) under a
 * step datum whose `fraud_prover` is the reservation's funding key. The
 * on-chain init requires that key's signature, so such a thread is the
 * reservation's own. Anything else returns null and the caller refuses it.
 */
export const resolveOwnThreadCustody = async (input: {
  readonly outRef: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly fundingPaymentKeyHash: string;
  readonly computationThreadPolicyId?: string;
  readonly resolveInputs: (
    outRefs: readonly string[],
  ) => Promise<readonly UTxO[]>;
}) => {
  if (input.computationThreadPolicyId === undefined) return null;
  const resolved = await input.resolveInputs([input.outRef]);
  const output = resolved.length === 1 ? resolved[0]! : undefined;
  if (
    output === undefined ||
    `${output.txHash}#${output.outputIndex}` !== input.outRef
  )
    return null;
  const assets = Object.entries(output.assets).filter(
    ([unit]) => unit !== "lovelace",
  );
  const unit = `${input.computationThreadPolicyId}${FRAUD_PROOF_CATALOGUE_CATEGORY_IDS[input.category]}${input.headerHash}`;
  if (assets.length !== 1 || assets[0]![0] !== unit || assets[0]![1] !== 1n)
    return null;
  if (typeof output.datum !== "string") return null;
  const datum = (() => {
    try {
      return Data.from(output.datum);
    } catch {
      return null;
    }
  })();
  if (
    !(datum instanceof Constr) ||
    datum.index !== 0 ||
    datum.fields.length !== 2 ||
    datum.fields[0] !== input.fundingPaymentKeyHash
  )
    return null;
  return Object.freeze({
    sourceActionKind: OBSERVED_THREAD_CUSTODY_ACTION_KIND,
    sourceOutputIndex: output.outputIndex,
    outRef: input.outRef,
    resolvedOutputCborHex: utxoToCore(output).output().to_canonical_cbor_hex(),
  });
};
