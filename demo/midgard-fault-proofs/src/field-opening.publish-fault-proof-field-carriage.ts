import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import { type FieldCarriage } from "@al-ft/midgard-sdk/core";
import {
  type FieldOpening,
  fieldOpeningForField,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  buildUnsignedFieldPreimagePublicationProgram,
  resolveCertificateReferenceIndex,
  resolveChunkReferenceIndices,
} from "@al-ft/midgard-sdk/state-queue";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { type PrerequisiteTurn } from "./fault-proof-turn.js";
import {
  fieldPublicationSlotClaims,
  fieldPublicationSlots,
  lucidOwnOutputReader,
} from "./field-opening.own-field-carriage.js";
import { type FaultProofFieldOpeningPlan } from "./field-opening.plan-fault-proof-field-opening.js";
import { type ResolvedProverSigner } from "./runtime.js";
import { finishStep } from "./step-completion.js";
import {
  claimOwnOutputs,
  type OwnOutputJournal,
  readOwnOutputs,
  resolveOwnOutputs,
} from "./workflow/own-outputs.js";

/**
 * The key and transaction count of a carriage plan's prerequisites: its
 * publications, and under tier 3 its certificate.
 */
export const fieldCarriagePlanTransactions = (
  plan: MidgardFieldCarriagePlan,
): PrerequisiteTurn["plan"] => ({
  key: `field-carriage:${plan.txId.toString("hex")}:${plan.fieldIndex.toString()}:${plan.commitment.toString("hex")}`,
  transactions: plan.publications.length + (plan.tier === "Certified" ? 1 : 0),
});

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
 * The carriage UTxOs a plan requires to exist on-chain before the step
 * transaction can reference them, in plan order, when all are live; otherwise
 * the next publication transaction, built and signed but not submitted.
 *
 * Empty under tier 1 — the preimage rides in the step's own redeemer and
 * nothing is published. Under tier 2 this is the whole of the carriage; under
 * tier 3 it is the chunks, and the §8.6 certificate is a separate mint the
 * caller supplies (`buildUnsignedFieldPreimageCertificationProgram`), because
 * it needs the certificate minting policy from the deployment and a fault-proof
 * step builder holds no such role.
 *
 * Each publication is a slot of `ownOutputs`: one of our earlier outputs for
 * the slot that is still live is reused; otherwise the slot's publication is
 * built, and its out-ref, taken from our signed body, is claimed before it is
 * returned. Two chunks with the same bytes are two slots and take two outputs.
 */
export const publishFaultProofFieldCarriage = async ({
  lucid,
  signer,
  planned,
  publisherAddress,
  label,
  ownOutputs,
  beforePublication,
  publicationConfirmed,
}: {
  readonly lucid: LucidEvolution;
  readonly signer: ResolvedProverSigner;
  readonly planned: FaultProofFieldOpeningPlan;
  readonly publisherAddress: string;
  readonly label: string;
  /** Our claims on the carriage slots; a new publication is claimed here. */
  readonly ownOutputs: OwnOutputJournal;
  /** Refresh reserved inputs before each new publication is built. */
  readonly beforePublication?: () => Promise<void>;
  /** Authenticate inclusion of each reused publication. */
  readonly publicationConfirmed?: (txHash: string) => Promise<void>;
}): Promise<readonly UTxO[] | PrerequisiteTurn> => {
  const publications = planned.plan.publications;
  if (publications.length === 0) {
    return [];
  }
  signer.selectWallet(lucid);
  const slots = fieldPublicationSlots({
    plan: planned.plan,
    publisherAddress,
  });
  const live = await readOwnOutputs(
    lucidOwnOutputReader(lucid),
    slots.flatMap(({ key }) =>
      ownOutputs.claims(key).map(({ outRef }) => outRef),
    ),
  );
  const published: UTxO[] = [];
  for (const [index, publication] of publications.entries()) {
    const { key, slot, datumCbor } = slots[index]!;
    const existing = [...ownOutputs.claims(key)]
      .reverse()
      .map(({ outRef }) => live.get(outRef))
      .find((utxo) => utxo !== undefined && slot.matches(utxo));
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
    const [claim] = claimOwnOutputs({ signed, slots: [slot] });
    ownOutputs.claim(key, claim!.outRef);
    return {
      kind: "publication",
      transaction: finishStep(signed, { referenceScripts: [] }),
      label: `${label} §8 carriage publication`,
      output: {
        outRef: claim!.outRef,
        label: `${label} §8 carriage publication`,
      },
      plan: fieldCarriagePlanTransactions(planned.plan),
    };
  }
  return published;
};

/**
 * Our live publications of a plan, in plan order, read by their claimed
 * out-refs; undefined when any slot has none.
 */
export const resolveFaultProofFieldCarriagePublications = async ({
  lucid,
  publisherAddress,
  planned,
  ownOutputs,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosByOutRef">;
  readonly publisherAddress: string;
  readonly planned: Pick<FaultProofFieldOpeningPlan, "plan">;
  readonly ownOutputs: OwnOutputJournal;
}): Promise<readonly UTxO[] | undefined> => {
  const resolution = await resolveOwnOutputs({
    slots: fieldPublicationSlotClaims({
      plan: planned.plan,
      publisherAddress,
      ownOutputs,
    }),
    read: lucidOwnOutputReader(lucid),
  });
  return resolution.kind === "ready" ? resolution.utxos : undefined;
};
