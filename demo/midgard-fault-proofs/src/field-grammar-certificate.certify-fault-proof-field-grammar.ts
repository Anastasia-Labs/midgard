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
import {
  buildUnsignedFieldGrammarCertificateAdvanceProgram,
  buildUnsignedFieldGrammarCertificateOpenProgram,
  FieldGrammarCertificate,
  type FieldGrammarCertificateCarriage,
  fieldGrammarCertificateDatumCbor,
  type FieldGrammarCertificateSource,
  fieldGrammarCertificateUnit,
  isCompleteFieldGrammarCertificate,
  type SpendingValidator,
} from "@al-ft/midgard-sdk";
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
import {
  DEFAULT_CONFIRMATION_POLL_MS,
  type ResolvedProverSigner,
} from "./runtime.js";
import {
  type FraudProofPreSubmitBoundary,
  reachFraudProofPreSubmitBoundary,
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

/**
 * Certifies a variable-width field's grammar: opens a certificate and
 * advances it batch by batch until it is `Complete`, resuming our own
 * `InProgress` certificate for the field when one is live. A field grammar
 * certificate lets a step read one item of a variable-width field without
 * walking the whole field, so the step's cost no longer grows with the
 * field's item count. The validator runs only from its hash-checked published
 * reference script.
 */
export const certifyFaultProofFieldGrammar = async ({
  lucid,
  network,
  signer,
  plan,
  source,
  certificateValidator,
  certificateReferenceScriptUtxo,
  carriageUtxos = [],
  fieldPreimageCertificatePolicyId,
  preSubmitBoundary,
  awaitConfirmation = true,
}: {
  readonly lucid: LucidEvolution;
  readonly network: Network;
  readonly signer: ResolvedProverSigner;
  /** The field's carriage plan: its tier, preimage and commitment. */
  readonly plan: MidgardFieldCarriagePlan;
  readonly source: FieldGrammarCertificateSource;
  readonly certificateValidator: Pick<
    SpendingValidator,
    "spendingScript" | "spendingScriptHash"
  >;
  readonly certificateReferenceScriptUtxo: UTxO;
  /** The publications (and, under tier 3, the §8.6 certificate) carrying the field. */
  readonly carriageUtxos?: readonly UTxO[];
  /** Required under tier 3, whose carriage names its certificate by policy. */
  readonly fieldPreimageCertificatePolicyId?: string;
  readonly preSubmitBoundary?: FraudProofPreSubmitBoundary;
  readonly awaitConfirmation?: boolean;
}): Promise<CertifiedFaultProofFieldGrammar> => {
  const certificatePolicyId = certificateValidator.spendingScriptHash;
  const grammar = fieldGrammarOfCarriagePlan(plan);
  const identity = fieldIdentity(certificatePolicyId, plan);
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
  const submit = async (unsigned: TxSignBuilder): Promise<UTxO> => {
    const signed = await unsigned.sign.withWallet().complete();
    const expectedTxHash = await reachFraudProofPreSubmitBoundary({
      signed,
      referenceScripts: [
        workflowReferenceScript({
          role: "field-grammar-certificate",
          utxo: certificateReferenceScriptUtxo,
          expectedScript: certificateValidator.spendingScript,
        }),
      ],
      boundary: preSubmitBoundary,
    });
    const txHash = await signed.submit();
    if (txHash !== expectedTxHash) {
      throw new Error(
        `Provider returned transaction hash ${txHash}, expected ${expectedTxHash}.`,
      );
    }
    if (awaitConfirmation) {
      await lucid.awaitTx(txHash, DEFAULT_CONFIRMATION_POLL_MS);
    }
    const unit = fieldGrammarCertificateUnit(certificatePolicyId);
    const live = (await lucid.utxosAt(certificateAddress)).find(
      (utxo) => utxo.txHash === txHash && utxo.assets[unit] === 1n,
    );
    if (live === undefined) {
      throw new Error(
        `field grammar certificate transaction ${txHash} left no certificate at ${certificateAddress}`,
      );
    }
    return live;
  };

  signer.selectWallet(lucid);
  const resumed = resumableCertificate({
    utxos: await lucid.utxosAt(certificateAddress),
    identity,
    grammar,
    ownerKeyHash: signer.paymentKeyHash,
  });
  const txHashes: string[] = [];
  const batches: number[] = [];
  let certificateUtxo: UTxO;
  let next: number;
  if (resumed === undefined) {
    const batch = fieldGrammarBatchSize(grammar, 0);
    certificateUtxo = await submit(
      await Effect.runPromise(
        buildUnsignedFieldGrammarCertificateOpenProgram(lucid, {
          source,
          carriage,
          batch,
          referenceInputs,
          ...output(batch),
        }),
      ),
    );
    txHashes.push(certificateUtxo.txHash);
    batches.push(batch);
    next = batch;
  } else {
    certificateUtxo = resumed.utxo;
    next = resumed.next;
  }
  while (next < grammar.itemCount) {
    const batch = fieldGrammarBatchSize(grammar, next);
    certificateUtxo = await submit(
      await Effect.runPromise(
        buildUnsignedFieldGrammarCertificateAdvanceProgram(lucid, {
          certificateUtxo,
          source,
          checkpoint: encodeMidgardFieldGrammarCheckpoint(
            plan.txId,
            grammar,
            next,
          ).toString("hex"),
          carriage,
          batch,
          referenceInputs,
          ...output(next + batch),
        }),
      ),
    );
    txHashes.push(certificateUtxo.txHash);
    batches.push(batch);
    next += batch;
  }
  if (!isCompleteFieldGrammarCertificate(certificateUtxo, identity)) {
    throw new Error("field grammar certificate did not reach Complete");
  }
  return { certificateUtxo, txHashes, batches };
};

/**
 * A live `Complete` certificate for this field, anyone's; undefined when
 * there is none. Two such certificates differ at most in `owner`.
 */
export const findCompleteFaultProofFieldGrammarCertificate = async ({
  lucid,
  network,
  certificatePolicyId,
  plan,
}: {
  readonly lucid: Pick<LucidEvolution, "utxosAt">;
  readonly network: Network;
  readonly certificatePolicyId: string;
  readonly plan: MidgardFieldCarriagePlan;
}): Promise<UTxO | undefined> => {
  const identity = fieldIdentity(certificatePolicyId, plan);
  return (
    await lucid.utxosAt(
      fieldGrammarCertificateAddress({ network, certificatePolicyId }),
    )
  ).find((utxo) => isCompleteFieldGrammarCertificate(utxo, identity));
};

/**
 * The `Complete` certificate a step references for this field: a live one
 * when there is one, and otherwise one this call certifies (resuming our own
 * `InProgress` certificate when one is live).
 */
export const reuseOrCertifyFaultProofFieldGrammar = async (
  input: Parameters<typeof certifyFaultProofFieldGrammar>[0],
): Promise<CertifiedFaultProofFieldGrammar> => {
  const live = await findCompleteFaultProofFieldGrammarCertificate({
    lucid: input.lucid,
    network: input.network,
    certificatePolicyId: input.certificateValidator.spendingScriptHash,
    plan: input.plan,
  });
  if (live !== undefined) {
    return { certificateUtxo: live, txHashes: [], batches: [] };
  }
  return await certifyFaultProofFieldGrammar(input);
};
