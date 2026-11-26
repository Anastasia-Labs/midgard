import { type MidgardFieldCarriagePlan } from "@al-ft/midgard-core";
import {
  deriveMidgardFieldGrammarCertificate,
  encodeMidgardFieldGrammarCheckpoint,
  MIDGARD_FIELD_GRAMMAR_WAYPOINT_BYTES,
  MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL,
  type MidgardFieldGrammar,
  midgardFieldGrammarCheckpointHash,
  midgardFieldGrammarWaypoints,
  walkMidgardFieldGrammar,
} from "@al-ft/midgard-core/codec/native-tx-field-access";
import { type SpendingValidator } from "@al-ft/midgard-sdk/core";
import {
  buildUnsignedFieldGrammarCertificateAdvanceProgram,
  buildUnsignedFieldGrammarCertificateOpenProgram,
  FieldGrammarCertificate,
  type FieldGrammarCertificateCarriage,
  fieldGrammarCertificateDatumCbor,
  type FieldGrammarCertificateSource,
  fieldGrammarCertificateUnit,
  isCompleteFieldGrammarCertificate,
} from "@al-ft/midgard-sdk/fraud-proof";
import {
  credentialToAddress,
  Data,
  type LucidEvolution,
  type Network,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { faultProofRawFieldCarriage } from "./field-opening.publish-fault-proof-field-carriage.js";
import { type ResolvedProverSigner } from "./runtime.js";
import { finishStep } from "./step-completion.js";
import {
  type LocallyEvaluatedTransaction,
  submitOutsideWorkflow,
  workflowReferenceScript,
} from "./workflow/transaction-boundary.js";

/**
 * Items one transaction certifies in general: the largest batch whose open
 * and final advance stay inside the 80% execution reserve at the largest
 * field any carriage tier holds (`script-budgets.json`, the
 * `field_grammar_certificate_handlers.test` rows).
 */
export const FIELD_GRAMMAR_WIDE_BATCH = 224;
/**
 * Items one transaction certifies when every item of the batch has a one-byte
 * wrapper (a payload of at most 23 bytes), the cheapest header to read.
 */
export const FIELD_GRAMMAR_NARROW_BATCH = 384;
/** The widest wrapper whose header is one byte: that byte and 23 payload. */
const ONE_BYTE_WRAPPER_MAX_WIDTH = 24;

/** The batch the certificate runs from item `next`: wide, narrow, or the rest. */
export const fieldGrammarBatchSize = (
  grammar: MidgardFieldGrammar,
  next: number,
): number => {
  const remaining = grammar.itemCount - next;
  if (!Number.isSafeInteger(next) || next < 0 || remaining <= 0) {
    throw new Error(
      `field grammar batch must start before the field's last item (next=${next.toString()}, count=${grammar.itemCount.toString()})`,
    );
  }
  const narrow = Math.min(FIELD_GRAMMAR_NARROW_BATCH, remaining);
  for (let item = next; item < next + narrow; item += 1) {
    if (
      grammar.offsets[item + 1]! - grammar.offsets[item]! >
      ONE_BYTE_WRAPPER_MAX_WIDTH
    ) {
      return Math.min(FIELD_GRAMMAR_WIDE_BATCH, remaining);
    }
  }
  return narrow;
};

/** Every batch from item `from` to the field's end, in order. */
export const planFieldGrammarBatches = (
  grammar: MidgardFieldGrammar,
  from = 0,
): readonly number[] => {
  const batches: number[] = [];
  for (let next = from; next < grammar.itemCount; ) {
    const batch = fieldGrammarBatchSize(grammar, next);
    batches.push(batch);
    next += batch;
  }
  return batches;
};

/** Certificates of `certificatePolicyId` live at its own enterprise address. */
export const fieldGrammarCertificateAddress = ({
  network,
  certificatePolicyId,
}: {
  readonly network: Network;
  readonly certificatePolicyId: string;
}): string =>
  credentialToAddress(network, { type: "Script", hash: certificatePolicyId });

/** The field's §5.1 preimage, whichever tier carries it. */
const carriedPreimage = (plan: MidgardFieldCarriagePlan): Buffer =>
  plan.inlinePreimage ??
  Buffer.concat(
    [...plan.publications]
      .sort((left, right) => left.chunkIndex - right.chunkIndex)
      .map((publication) => publication.bytes),
  );

/** The field's walked grammar, refused unless it opens the planned slot. */
export const fieldGrammarOfCarriagePlan = (
  plan: MidgardFieldCarriagePlan,
): MidgardFieldGrammar => {
  const grammar = walkMidgardFieldGrammar(
    plan.fieldIndex,
    carriedPreimage(plan),
  );
  if (
    !grammar.fieldHash.equals(plan.commitment) ||
    grammar.totalLength !== plan.totalLength
  ) {
    throw new Error(
      `field ${plan.fieldIndex.toString()} preimage does not match the commitment its carriage plan opens`,
    );
  }
  if (grammar.itemCount === 0) {
    throw new Error(
      `field ${plan.fieldIndex.toString()} is empty and has no item to certify`,
    );
  }
  return grammar;
};

type FieldIdentity = Parameters<typeof isCompleteFieldGrammarCertificate>[1];

const fieldIdentity = (
  certificatePolicyId: string,
  plan: MidgardFieldCarriagePlan,
): FieldIdentity => ({
  certificatePolicyId,
  txIdHex: plan.txId.toString("hex"),
  fieldIndex: plan.fieldIndex,
  fieldHashHex: plan.commitment.toString("hex"),
  totalLength: plan.totalLength,
});

/**
 * Where `ownerKeyHash`'s `InProgress` certificate for this field stands: the
 * UTxO and the item its walk reached, recovered from the waypoint count and
 * the checkpoint digest. Undefined when there is none.
 */
const resumableCertificate = ({
  utxos,
  identity,
  grammar,
  ownerKeyHash,
}: {
  readonly utxos: readonly UTxO[];
  readonly identity: FieldIdentity;
  readonly grammar: MidgardFieldGrammar;
  readonly ownerKeyHash: string;
}): { readonly utxo: UTxO; readonly next: number } | undefined => {
  const txId = Buffer.from(identity.txIdHex, "hex");
  const unit = fieldGrammarCertificateUnit(identity.certificatePolicyId);
  for (const utxo of utxos) {
    if ((utxo.assets[unit] ?? 0n) !== 1n || utxo.datum == null) continue;
    let datum: FieldGrammarCertificate;
    try {
      datum = Data.from(utxo.datum, FieldGrammarCertificate);
    } catch {
      continue;
    }
    if (!("InProgress" in datum)) continue;
    const held = datum.InProgress;
    if (
      held.owner !== ownerKeyHash ||
      held.tx_id !== identity.txIdHex ||
      held.field_index !== BigInt(identity.fieldIndex) ||
      held.field_hash !== identity.fieldHashHex ||
      held.total_length !== BigInt(identity.totalLength)
    ) {
      continue;
    }
    const waypointCount =
      held.waypoints.length / 2 / MIDGARD_FIELD_GRAMMAR_WAYPOINT_BYTES;
    const last = Math.min(
      waypointCount * MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL,
      grammar.itemCount - 1,
    );
    for (
      let next =
        (waypointCount - 1) * MIDGARD_FIELD_GRAMMAR_WAYPOINT_INTERVAL + 1;
      next <= last;
      next += 1
    ) {
      if (
        midgardFieldGrammarCheckpointHash(
          encodeMidgardFieldGrammarCheckpoint(txId, grammar, next),
        ).toString("hex") === held.grammar_checkpoint_hash &&
        midgardFieldGrammarWaypoints(grammar, next).toString("hex") ===
          held.waypoints
      ) {
        return { utxo, next };
      }
    }
  }
  return undefined;
};

export type CertifiedFaultProofFieldGrammar = {
  readonly certificateUtxo: UTxO;
  /** The transactions this call submitted, in order; empty on reuse. */
  readonly txHashes: readonly string[];
  /** The item count of each submitted batch, in order. */
  readonly batches: readonly number[];
};

/** What certifying one field's grammar reads and builds against. */
export type FaultProofFieldGrammarCertification = Readonly<{
  lucid: LucidEvolution;
  network: Network;
  signer: ResolvedProverSigner;
  /** The field's carriage plan: its tier, preimage and commitment. */
  plan: MidgardFieldCarriagePlan;
  source: FieldGrammarCertificateSource;
  certificateValidator: Pick<
    SpendingValidator,
    "spendingScript" | "spendingScriptHash"
  >;
  certificateReferenceScriptUtxo: UTxO;
  /** The publications (and, under tier 3, the §8.6 certificate) carrying the field. */
  carriageUtxos?: readonly UTxO[];
  /** Required under tier 3, whose carriage names its certificate by policy. */
  fieldPreimageCertificatePolicyId?: string;
}>;

/** Our live `InProgress` certificate for the field and the item it resumes from. */
export type FaultProofFieldGrammarProgress = Readonly<{
  certificateUtxo: UTxO;
  next: number;
}>;

/** One signed certificate batch, the items it certifies and the item after it. */
export type FaultProofFieldGrammarBatch = Readonly<{
  transaction: LocallyEvaluatedTransaction;
  batch: number;
  next: number;
}>;

/**
 * Selects the prover's wallet and reads our own live `InProgress`
 * certificate for the field: null when there is none, so the next batch
 * opens a certificate.
 */
export const resumeFaultProofFieldGrammar = async ({
  lucid,
  network,
  signer,
  plan,
  certificateValidator,
}: FaultProofFieldGrammarCertification): Promise<FaultProofFieldGrammarProgress | null> => {
  const certificatePolicyId = certificateValidator.spendingScriptHash;
  signer.selectWallet(lucid);
  const resumed = resumableCertificate({
    utxos: await lucid.utxosAt(
      fieldGrammarCertificateAddress({ network, certificatePolicyId }),
    ),
    identity: fieldIdentity(certificatePolicyId, plan),
    grammar: fieldGrammarOfCarriagePlan(plan),
    ownerKeyHash: signer.paymentKeyHash,
  });
  return resumed === undefined
    ? null
    : { certificateUtxo: resumed.utxo, next: resumed.next };
};

/**
 * Builds and signs one certificate batch for a variable-width field's
 * grammar: the open when `from` is null, and otherwise the advance of the
 * `InProgress` certificate `from` names. It submits nothing. The validator
 * runs only from its hash-checked published reference script.
 */
export const buildFaultProofFieldGrammarBatch = async ({
  lucid,
  network,
  signer,
  plan,
  source,
  certificateValidator,
  certificateReferenceScriptUtxo,
  carriageUtxos = [],
  fieldPreimageCertificatePolicyId,
  from,
}: FaultProofFieldGrammarCertification & {
  readonly from: FaultProofFieldGrammarProgress | null;
}): Promise<FaultProofFieldGrammarBatch> => {
  const certificatePolicyId = certificateValidator.spendingScriptHash;
  const grammar = fieldGrammarOfCarriagePlan(plan);
  const certificateAddress = fieldGrammarCertificateAddress({
    network,
    certificatePolicyId,
  });
  const referenceInputs = [certificateReferenceScriptUtxo, ...carriageUtxos];
  const carriage: FieldGrammarCertificateCarriage = (resolved) =>
    faultProofRawFieldCarriage({
      plan,
      referenceInputs: resolved,
      ...(fieldPreimageCertificatePolicyId === undefined
        ? {}
        : { certificatePolicyId: fieldPreimageCertificatePolicyId }),
      label: "field grammar certificate",
    });
  const output = (next: number) => ({
    certificatePolicyId,
    certificateAddress,
    datumCbor: fieldGrammarCertificateDatumCbor(
      deriveMidgardFieldGrammarCertificate({
        owner: Buffer.from(signer.paymentKeyHash, "hex"),
        txId: plan.txId,
        grammar,
        next,
      }),
    ),
    lovelace: 0n,
  });
  const start = from === null ? 0 : from.next;
  const batch = fieldGrammarBatchSize(grammar, start);
  const unsigned: TxSignBuilder = await Effect.runPromise(
    from === null
      ? buildUnsignedFieldGrammarCertificateOpenProgram(lucid, {
          source,
          carriage,
          batch,
          referenceInputs,
          ...output(batch),
        })
      : buildUnsignedFieldGrammarCertificateAdvanceProgram(lucid, {
          certificateUtxo: from.certificateUtxo,
          source,
          checkpoint: encodeMidgardFieldGrammarCheckpoint(
            plan.txId,
            grammar,
            from.next,
          ).toString("hex"),
          carriage,
          batch,
          referenceInputs,
          ...output(from.next + batch),
        }),
  );
  const signed = await unsigned.sign.withWallet().complete();
  return {
    transaction: finishStep(signed, {
      referenceScripts: [
        workflowReferenceScript({
          role: "field-grammar-certificate",
          utxo: certificateReferenceScriptUtxo,
          expectedScript: certificateValidator.spendingScript,
        }),
      ],
    }),
    batch,
    next: start + batch,
  };
};

/**
 * Certifies a variable-width field's grammar: opens a certificate and
 * advances it batch by batch until it is `Complete`, resuming our own
 * `InProgress` certificate for the field when one is live. A field grammar
 * certificate lets a step read one item of a variable-width field without
 * walking the whole field, so the step's cost does not grow with the
 * field's item count.
 *
 * It sends each certificate batch directly, outside any workflow's lease,
 * and waits for it when `awaitConfirmation` holds (the default) before
 * building the next. The step builders that route a certified read through
 * it (`routeFaultProofFieldGrammar`) therefore submit before they return:
 * the one exception to a builder returning its transaction and submitting
 * nothing.
 */
export const certifyFaultProofFieldGrammar = async ({
  awaitConfirmation = true,
  ...input
}: FaultProofFieldGrammarCertification & {
  readonly awaitConfirmation?: boolean;
}): Promise<CertifiedFaultProofFieldGrammar> => {
  const { lucid, network, plan, certificateValidator } = input;
  const certificatePolicyId = certificateValidator.spendingScriptHash;
  const identity = fieldIdentity(certificatePolicyId, plan);
  const certificateAddress = fieldGrammarCertificateAddress({
    network,
    certificatePolicyId,
  });
  const unit = fieldGrammarCertificateUnit(certificatePolicyId);
  const itemCount = fieldGrammarOfCarriagePlan(plan).itemCount;
  const txHashes: string[] = [];
  const batches: number[] = [];
  let from = await resumeFaultProofFieldGrammar(input);
  let certificateUtxo = from?.certificateUtxo;
  while (from === null || from.next < itemCount) {
    const built = await buildFaultProofFieldGrammarBatch({ ...input, from });
    const txHash = await submitOutsideWorkflow(lucid, built.transaction, {
      label: "field-grammar certificate",
      awaitConfirmation,
    });
    const live = (await lucid.utxosAt(certificateAddress)).find(
      (utxo) => utxo.txHash === txHash && utxo.assets[unit] === 1n,
    );
    if (live === undefined) {
      throw new Error(
        `field grammar certificate transaction ${txHash} left no certificate at ${certificateAddress}`,
      );
    }
    certificateUtxo = live;
    txHashes.push(live.txHash);
    batches.push(built.batch);
    from = { certificateUtxo: live, next: built.next };
  }
  if (
    certificateUtxo === undefined ||
    !isCompleteFieldGrammarCertificate(certificateUtxo, identity)
  ) {
    throw new Error("field grammar certificate did not reach Complete");
  }
  return { certificateUtxo, txHashes, batches };
};

/** The certificate datum's `owner`, or undefined when it has none. */
const certificateOwner = (utxo: UTxO): string | undefined => {
  if (utxo.datum == null) return undefined;
  try {
    const datum = Data.from(utxo.datum, FieldGrammarCertificate);
    return "Complete" in datum ? datum.Complete.owner : datum.InProgress.owner;
  } catch {
    return undefined;
  }
};

/**
 * A live `Complete` certificate for this field whose datum `owner` is
 * `ownerKeyHash`; undefined when there is none. The step relies only on a
 * certificate the prover controls, so another key's certificate never serves.
 */
export const findCompleteFaultProofFieldGrammarCertificate = async ({
  lucid,
  network,
  certificatePolicyId,
  plan,
  ownerKeyHash,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly network: Network;
  readonly certificatePolicyId: string;
  readonly plan: MidgardFieldCarriagePlan;
  /** The prover's payment key hash. */
  readonly ownerKeyHash: string;
}): Promise<UTxO | undefined> => {
  const identity = fieldIdentity(certificatePolicyId, plan);
  return (
    await lucid.utxosAt(
      fieldGrammarCertificateAddress({ network, certificatePolicyId }),
    )
  ).find(
    (utxo) =>
      isCompleteFieldGrammarCertificate(utxo, identity) &&
      certificateOwner(utxo) === ownerKeyHash,
  );
};

/**
 * The `Complete` certificate a step references for this field: the prover's
 * own live one when there is one, and otherwise one this call certifies
 * (resuming the prover's own `InProgress` certificate when one is live). The
 * step relies only on a certificate the prover controls.
 */
export const reuseOrCertifyFaultProofFieldGrammar = async (
  input: Parameters<typeof certifyFaultProofFieldGrammar>[0],
): Promise<CertifiedFaultProofFieldGrammar> => {
  const live = await findCompleteFaultProofFieldGrammarCertificate({
    lucid: input.lucid,
    network: input.network,
    certificatePolicyId: input.certificateValidator.spendingScriptHash,
    plan: input.plan,
    ownerKeyHash: input.signer.paymentKeyHash,
  });
  if (live !== undefined) {
    return { certificateUtxo: live, txHashes: [], batches: [] };
  }
  return await certifyFaultProofFieldGrammar(input);
};
