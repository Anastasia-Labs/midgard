import {
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeSingleCbor,
  deriveMidgardNativeTxCompact,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeCbor,
  encodeMidgardNativeTxCanonical,
  encodeMidgardNativeTxCompact,
  encodeMidgardNativeTxProofFieldLengths,
  encodeMidgardNativeTxWitnessSetCompact,
  midgardNativeTxProofFieldPreimageLengths,
} from "@al-ft/midgard-core";
import { wrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { buildCountedRoot } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data, walletFromSeed } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  buildJourneyTransactionControl,
  type JourneyTransactionInput,
} from "./transaction-cases.js";

export const JOURNEY_TRANSACTION_SOURCE_CATEGORIES = [
  "daHashPreimage",
  "canonicalDecodability",
  "committedFieldShape",
  "l2TxMistag",
] as const;
export type JourneyTransactionSourceCategory =
  (typeof JOURNEY_TRANSACTION_SOURCE_CATEGORIES)[number];

/**
 * Source-level operator faults. Begin with a valid retained transaction and
 * change the committed source/field itself, retaining exact malformed bytes.
 * Keep those bytes available when ordinary transaction replay cannot decode
 * them; classification must use the committed source and field proofs.
 */
export const buildJourneyTransactionSourceFault = async (
  input: JourneyTransactionInput & {
    category: JourneyTransactionSourceCategory;
  },
) => {
  const { category, ...controlInput } = input;
  const control = await buildJourneyTransactionControl(controlInput);
  const first = control.payload.block_body.transaction_preimages[0];
  if (first === undefined)
    throw new Error("Source fixture needs its retained valid transaction");
  const original = decodeMidgardNativeTxFullFromCanonicalCbor(
    Buffer.from(first[1], "hex"),
  );
  const body = {
    ...original.body,
    ...(category === "canonicalDecodability"
      ? { requiredSignersPreimageCbor: Buffer.from("8041", "hex") }
      : {}),
    ...(category === "committedFieldShape"
      ? { requiredSignersPreimageCbor: encodeCbor([Buffer.alloc(27, 0xe4)]) }
      : {}),
  };
  const validity =
    category === "l2TxMistag" ? "TxIsInvalid" : original.validity;
  const txId = computeMidgardNativeTxId(
    deriveMidgardNativeTxCompact(body, original.witnessSet, validity),
  ).toString("hex");
  const wallet = walletFromSeed(input.ledgerOwnerSeedPhrase, {
    network: "Custom",
  });
  const key = CML.PrivateKey.from_bech32(wallet.paymentKey);
  const witnessSet = {
    ...original.witnessSet,
    addrTxWitsPreimageCbor: encodeCbor([
      SDK.encodeMidgardAddressWitnessCanonical({
        verification_key: Buffer.from(key.to_public().to_raw_bytes()).toString(
          "hex",
        ),
        signature: key.sign(Buffer.from(txId, "hex")).to_hex(),
      }),
    ]),
  };
  const compact = deriveMidgardNativeTxCompact(body, witnessSet, validity);
  const source: SDK.L2TransactionSource = {
    tx_id: category === "daHashPreimage" ? "e5".repeat(32) : txId,
    source: {
      compact_cbor: encodeMidgardNativeTxCompact(compact).toString("hex"),
      witness_set_compact_cbor: encodeMidgardNativeTxWitnessSetCompact(
        deriveMidgardNativeTxWitnessSetCompact(witnessSet),
      ).toString("hex"),
      field_preimage_lengths_cbor: encodeMidgardNativeTxProofFieldLengths(
        midgardNativeTxProofFieldPreimageLengths({ body, witnessSet }),
      ).toString("hex"),
    },
  };
  // The ordinary encoder correctly refuses this malformed field. Build only
  // the evidence envelope from a known-valid outer structure, preserving the
  // exact bad byte string inside it.
  const validEnvelope = encodeMidgardNativeTxCanonical({
    version: original.version,
    body: {
      ...body,
      requiredSignersPreimageCbor: original.body.requiredSignersPreimageCbor,
    },
    witnessSet,
    validity,
  });
  const outer = decodeSingleCbor(validEnvelope);
  if (!Array.isArray(outer) || !Array.isArray(outer[1]))
    throw new Error("Canonical transaction envelope shape changed");
  outer[1][7] = body.requiredSignersPreimageCbor;
  const canonicalCbor = encodeCbor(outer);
  const sourceBytes = Buffer.from(
    Data.to(source, SDK.L2TransactionSource),
    "hex",
  );
  const root = await buildCountedRoot(SDK.ROOT_DOMAINS.transactionsV1, [
    { key: Buffer.from(txId, "hex"), value: sourceBytes },
  ]);
  const eventKey: SDK.EventKey = { L2TransactionEventKey: { tx_id: txId } };
  const eventKeyHex = Data.to(eventKey, SDK.EventKey);
  const eventToStep = control.payload.block_body.event_to_step.map(
    ([, value]): SDK.DaPayloadEntry => [eventKeyHex, value],
  );
  const validationTraces = control.payload.block_body.validation_traces.map(
    ([, value]): SDK.DaPayloadEntry => [eventKeyHex, value],
  );
  const validationWitnesses =
    control.payload.block_body.validation_trace_witnesses.map(
      ([key, value]): SDK.DaPayloadEntry => [
        SDK.encodeRetainedValidationWitnessKey({
          ...SDK.decodeRetainedValidationWitnessKey(Buffer.from(key, "hex")),
          event_key: eventKey,
        }).toString("hex"),
        value,
      ],
    );
  const transitions = control.payload.block_body.transition_trace.map(
    ([key, value]): SDK.DaPayloadEntry => [
      key,
      Data.to(
        { ...Data.from(value, SDK.TransitionStep), event_key: eventKey },
        SDK.TransitionStep,
      ),
    ],
  );
  const counted = (
    domain: SDK.RootDomain,
    entries: readonly SDK.DaPayloadEntry[],
  ) =>
    buildCountedRoot(
      domain,
      entries.map(([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      })),
    );
  const header = {
    ...control.header,
    transactionsRoot: root.root,
    eventToStepRoot: (await counted(SDK.ROOT_DOMAINS.eventToStep, eventToStep))
      .root,
    validationTracesRoot: (
      await counted(SDK.ROOT_DOMAINS.validationTraces, validationTraces)
    ).root,
    transitionTraceRoot: (
      await counted(SDK.ROOT_DOMAINS.transitionTrace, transitions)
    ).root,
  };
  const headerHash = await Effect.runPromise(SDK.hashBlockHeader(header));
  const payload: SDK.DaPayload = {
    ...control.payload,
    block_body: {
      ...control.payload.block_body,
      header,
      header_hash: headerHash,
      transactions: [[txId, sourceBytes.toString("hex")]],
      event_to_step: eventToStep,
      validation_traces: validationTraces,
      validation_trace_witnesses: validationWitnesses,
      transition_trace: transitions,
      transaction_preimages: [[txId, canonicalCbor.toString("hex")]],
    },
  };
  return {
    header,
    headerHash,
    payload,
    payloadEnvelopeCbor: await wrapDaPayload(SDK.encodeDaPayload(payload), {
      mode: "identity",
    }),
    control,
  };
};
