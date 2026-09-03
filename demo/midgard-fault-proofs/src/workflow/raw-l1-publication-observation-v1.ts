import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

import {
  admitFraudProofRawL1Snapshot,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  type FraudProofRawL1SnapshotAuthority,
} from "./raw-l1-snapshot-v1.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy-v1.js";

export const FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER =
  "midgard-fraud-proof-authenticated-publication-observer-v1" as const;

export type FraudProofAuthenticatedPublicationObservation =
  | { readonly kind: "confirmed"; readonly outRef: string }
  | { readonly kind: "not_found" };

export interface FraudProofAuthenticatedPublicationObserver {
  readonly observerVersion: typeof FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER;
  observeExact(input: {
    readonly headerHash: string;
    readonly kind: "proof_chunk" | "field_publication" | "field_certificate";
    readonly address: string;
    readonly expectedOutRef: string;
    readonly expectedDatumCbor: string;
    /** Required for certificates; omitted for ADA-only field publications. */
    readonly expectedUnit?: string;
  }): Promise<FraudProofAuthenticatedPublicationObservation>;
}

const mintQuantity = (body: CML.TransactionBody, unit: string): bigint => {
  const assets = body
    .mint()
    ?.get_assets(CML.ScriptHash.from_hex(unit.slice(0, 56)));
  return assets?.get(CML.AssetName.from_hex(unit.slice(56))) ?? 0n;
};

/**
 * Authenticates one content publication at a release-final Kupo/Ogmios point.
 * The expected out-ref is the hash journaled before network submission, so a
 * third party's same-content output cannot confirm an ambiguous local submit.
 */
export const createFraudProofAuthenticatedPublicationObserver = ({
  authority,
  releaseFinality,
}: {
  readonly authority: FraudProofRawL1SnapshotAuthority;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): FraudProofAuthenticatedPublicationObserver => {
  if (authority.authorityVersion !== FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY) {
    throw new Error(
      "publication observer requires a raw L1 snapshot authority",
    );
  }
  return {
    observerVersion: FRAUD_PROOF_AUTHENTICATED_PUBLICATION_OBSERVER,
    observeExact: async (input) => {
      const historyUnits =
        input.expectedUnit === undefined ? [] : [input.expectedUnit];
      const request = {
        deploymentIdentityDigest: releaseFinality.deploymentIdentityDigest,
        releaseIdentityDigest: releaseFinality.releaseIdentityDigest,
        finalityPolicyDigest: releaseFinality.policyDigest,
        headerHash: input.headerHash,
        scopes: [{ role: input.kind, address: input.address }],
        historyUnits,
      } as const;
      const snapshot = admitFraudProofRawL1Snapshot({
        value: await authority.capture(request),
        request,
        releaseFinality,
      });
      const scoped = snapshot.scopes[0];
      const candidate = scoped?.utxos.find(
        (utxo) => utxo.outRef === input.expectedOutRef,
      );
      if (candidate === undefined) return { kind: "not_found" };
      const output = CML.TransactionOutput.from_cbor_hex(candidate.outputCbor);
      const decoded = coreToTxOutput(output);
      if (
        candidate.datumCbor !== input.expectedDatumCbor ||
        output.datum_hash() !== undefined ||
        candidate.referenceScriptCbor !== null ||
        (input.expectedUnit === undefined
          ? Object.entries(decoded.assets).some(
              ([unit, quantity]) => unit !== "lovelace" && quantity !== 0n,
            )
          : (decoded.assets[input.expectedUnit] ?? 0n) !== 1n ||
            Object.entries(decoded.assets).some(
              ([unit, quantity]) =>
                unit !== "lovelace" &&
                unit !== input.expectedUnit &&
                quantity !== 0n,
            ))
      ) {
        throw new Error(
          "authenticated publication output differs from its journaled content identity",
        );
      }
      if (input.expectedUnit !== undefined) {
        const txHash = input.expectedOutRef.split("#")[0]!;
        const history = snapshot.history[0];
        const transaction = snapshot.transactions.find(
          (candidate) => candidate.txHash === txHash,
        );
        if (
          history === undefined ||
          history.unit !== input.expectedUnit ||
          !history.transactionHashes.includes(txHash) ||
          transaction === undefined ||
          mintQuantity(
            CML.TransactionBody.from_cbor_hex(transaction.bodyCbor),
            input.expectedUnit,
          ) !== 1n
        ) {
          throw new Error(
            "authenticated certificate history does not prove the exact token mint",
          );
        }
      }
      return { kind: "confirmed", outRef: candidate.outRef };
    },
  };
};
