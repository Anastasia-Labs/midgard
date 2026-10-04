import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import {
  buildUnsignedFieldPreimagePublicationProgram,
  type FieldCarriage,
  type FieldOpening,
  fieldOpeningForField,
  fieldPreimagePublicationDatumCbor,
  resolveCertificateReferenceIndex,
  resolveChunkReferenceIndices,
} from "@al-ft/midgard-sdk";
import {
  coreToTxOutput,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type FaultProofFieldOpeningPlan } from "./field-opening.plan-fault-proof-field-opening.js";
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  fetchUtxoByOutRef,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
} from "./workflow/transaction-boundary.js";

/**
 * The §8 carriage as the step's redeemer must spell it, with every positional
 * index resolved by **content** against the door-running transaction's complete
 * reference-input set (§8.7).
 *
 * Tier 1 indexes nothing, so `referenceInputs` is ignored there and a caller
 * that has none may omit it. Above tier 1 the set must be the transaction's
 * whole reference-input list rather than only the carriage: positional indices
 * count into the ledger's canonically-sorted list, and a step references more
 * than its carriage.
 */
export const faultProofFieldCarriage = ({
  planned,
  referenceInputs = [],
  certificatePolicyId,
  label,
}: {
  readonly planned: FaultProofFieldOpeningPlan;
  readonly referenceInputs?: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): FieldCarriage =>
  faultProofRawFieldCarriage({
    plan: planned.plan,
    referenceInputs,
    ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
    label,
  });

/**
 * Resolves §8 carriage for arbitrary committed bytes.
 *
 * Most fraud families open a valid §5.1 field and therefore start from a
 * {@link FaultProofFieldOpeningPlan}. Canonical-decodability is deliberately
 * different: the bytes it proves are malformed and cannot be manufactured by
 * the decoded-item planner. The carriage door authenticates only the raw
 * `(tx id, field index, commitment)` plan, so this narrower resolver is the
 * truthful shared seam for that family.
 */
export const faultProofRawFieldCarriage = ({
  plan,
  referenceInputs = [],
  certificatePolicyId,
  label,
}: {
  readonly plan: MidgardFieldCarriagePlan;
  readonly referenceInputs?: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): FieldCarriage => {
  if (plan.tier === "Inline") {
    if (plan.inlinePreimage === null) {
      throw new Error(`${label} tier-1 plan carries no preimage.`);
    }
    return { Inline: { preimage: plan.inlinePreimage.toString("hex") } };
  }
  const chunkIndices = resolveChunkReferenceIndices({
    plan,
    referenceInputs,
  });
  if (plan.tier === "RawUtxo") {
    const refInputIndex = chunkIndices[0];
    if (refInputIndex === undefined || chunkIndices.length !== 1) {
      throw new Error(
        `${label} tier-2 carriage must resolve exactly one publication.`,
      );
    }
    return { RawUtxo: { ref_input_index: BigInt(refInputIndex) } };
  }
  if (plan.certificate === null) {
    throw new Error(`${label} tier-3 plan carries no §8.6 certificate.`);
  }
  if (certificatePolicyId === undefined) {
    throw new Error(
      `${label} is tier-3 carriage, which names its §8.6 manifest by policy id, but none was supplied.`,
    );
  }
  return {
    Certified: {
      cert_ref_input_index: BigInt(
        resolveCertificateReferenceIndex({
          certificatePolicyId,
          txIdHex: plan.txId.toString("hex"),
          fieldIndex: plan.fieldIndex,
          fieldHashHex: plan.commitment.toString("hex"),
          referenceInputs,
          label,
        }),
      ),
      chunk_ref_input_indices: chunkIndices.map((index) => BigInt(index)),
    },
  };
};

/**
 * The whole opening a rebound step's redeemer carries: the compact bytes, the
 * §2.5-derived arm, and the resolved carriage.
 *
 * The arm is `fieldOpeningForField`'s to choose, not this module's — a family
 * names its field and gets `BodyFieldOpening` or `WitnessFieldOpening`, which is
 * what keeps the §2.5 pairing off the caller's list of things to be right about.
 */
export const faultProofFieldOpening = ({
  planned,
  referenceInputs = [],
  certificatePolicyId,
  label,
}: {
  readonly planned: FaultProofFieldOpeningPlan;
  readonly referenceInputs?: readonly UTxO[];
  readonly certificatePolicyId?: string;
  readonly label: string;
}): FieldOpening =>
  fieldOpeningForField({
    fieldIndex: planned.fieldIndex,
    nativeTxCompactCbor: planned.nativeTxCompactCbor,
    carriage: faultProofFieldCarriage({
      planned,
      referenceInputs,
      ...(certificatePolicyId === undefined ? {} : { certificatePolicyId }),
      label,
    }),
    ...(planned.witnessSet === undefined
      ? {}
      : { witnessSet: planned.witnessSet }),
  });

/**
 * Publishes whatever a plan requires to exist on-chain before the step
 * transaction can reference it, and returns the confirmed carriage UTxOs.
 *
 * Empty under tier 1 — the preimage rides in the step's own redeemer and
 * nothing is published. Under tier 2 this is the whole of the carriage; under
 * tier 3 it is the chunks, and the §8.6 certificate is a separate mint the
 * caller supplies (`buildUnsignedFieldPreimageCertificationProgram`), because
 * it needs the certificate minting policy from the deployment and a fault-proof
 * step builder holds no such role.
 *
 * §8.7: publication is permissionless and content-addressed, so a chunk that
 * already exists at the publisher's address is reused rather than republished.
 */
export const publishFaultProofFieldCarriage = async ({
  lucid,
  signer,
  planned,
  publisherAddress,
  label,
  preSubmitBoundary,
  beforePublication,
  publicationConfirmed,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly planned: FaultProofFieldOpeningPlan;
  readonly publisherAddress: string;
  readonly label: string;
  /** Production workflow seam for each content publication transaction. */
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  /** Refresh reserved inputs before each new publication is built. */
  readonly beforePublication?: () => Promise<void>;
  /** Authenticate inclusion and rotate funding before building another chunk. */
  readonly publicationConfirmed?: (txHash: string) => Promise<void>;
}): Promise<readonly UTxO[]> => {
  const publications = planned.plan.publications;
  if (publications.length === 0) {
    return [];
  }
  signer.selectWallet(lucid);
  const published: UTxO[] = [];
  for (const publication of publications) {
    const datumCbor = fieldPreimagePublicationDatumCbor(publication.bytes);
    // Compared byte-for-byte against the datum, exactly as
    // `resolveChunkReferenceIndices` does when it locates the same UTxO at the
    // door. Matching on anything looser here would reuse a publication the
    // resolver then fails to find.
    const existing = (await lucid.utxosAt(publisherAddress)).find(
      (utxo) => utxo.datum === datumCbor,
    );
    if (existing !== undefined) {
      await publicationConfirmed?.(existing.txHash);
      published.push(existing);
      continue;
    }
    await beforePublication?.();
    const unsigned = await Effect.runPromise(
      buildUnsignedFieldPreimagePublicationProgram(lucid, {
        publication: {
          chunkIndex: publication.chunkIndex,
          datumCbor,
          byteLength: publication.bytes.length,
          digestHex: publication.digest.toString("hex"),
        },
        publisherAddress,
      }),
    );
    const signed = await unsigned.sign.withWallet().complete();
    const expectedTxHash = await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: [],
      boundary: preSubmitBoundary,
    });
    const txHash = await signed.submit();
    if (txHash !== expectedTxHash) {
      throw new Error(
        `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
      );
    }
    await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    await publicationConfirmed?.(txHash);
    const outputs = signed.toTransaction().body().outputs();
    let outputIndex: number | undefined;
    for (let index = 0; index < outputs.len(); index += 1) {
      if (
        outputIndex === undefined &&
        coreToTxOutput(outputs.get(index)).datum === datumCbor
      ) {
        outputIndex = index;
      }
    }
    if (outputIndex === undefined) {
      throw new Error(
        `${label} §8 carriage publication did not produce an output carrying chunk ${publication.chunkIndex.toString()}.`,
      );
    }
    published.push(
      await fetchUtxoByOutRef({
        lucid,
        outRef: { txHash, outputIndex },
        label: `${label} §8 carriage publication`,
      }),
    );
  }
  return published;
};

/** Resolves every publication in plan order from authenticated L1 UTxOs. */
export const resolveFaultProofFieldCarriagePublications = async ({
  lucid,
  publisherAddress,
  planned,
}: {
  readonly lucid: LucidEvolution;
  readonly publisherAddress: string;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
}): Promise<readonly UTxO[] | undefined> => {
  const candidates = await lucid.utxosAt(publisherAddress);
  const claimed = new Set<string>();
  const resolved: UTxO[] = [];
  for (const publication of planned.plan.publications) {
    const expectedDatum = fieldPreimagePublicationDatumCbor(publication.bytes);
    const match = candidates.find((candidate) => {
      const label = `${candidate.txHash}#${candidate.outputIndex.toString()}`;
      return !claimed.has(label) && candidate.datum === expectedDatum;
    });
    if (match === undefined) return undefined;
    claimed.add(`${match.txHash}#${match.outputIndex.toString()}`);
    resolved.push(match);
  }
  return resolved;
};

export type MissingFaultProofFieldPublication = {
  readonly digest: string;
  readonly datumCbor: string;
  readonly chunkIndex: number;
};

/** Deterministically selects the first plan-ordered publication absent on L1. */
export const findMissingFaultProofFieldPublication = async ({
  lucid,
  publisherAddress,
  planned,
}: {
  readonly lucid: LucidEvolution;
  readonly publisherAddress: string;
  readonly planned: FaultProofFieldOpeningPlan;
}): Promise<MissingFaultProofFieldPublication | undefined> => {
  const candidates = await lucid.utxosAt(publisherAddress);
  const claimed = new Set<string>();
  for (const publication of planned.plan.publications) {
    const datumCbor = fieldPreimagePublicationDatumCbor(publication.bytes);
    const match = candidates.find((candidate) => {
      const label = `${candidate.txHash}#${candidate.outputIndex.toString()}`;
      return !claimed.has(label) && candidate.datum === datumCbor;
    });
    if (match === undefined) {
      return {
        digest: publication.digest.toString("hex"),
        datumCbor,
        chunkIndex: publication.chunkIndex,
      };
    }
    claimed.add(`${match.txHash}#${match.outputIndex.toString()}`);
  }
  return undefined;
};
