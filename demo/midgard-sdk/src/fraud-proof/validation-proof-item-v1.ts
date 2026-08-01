import { buildMidgardBoundedItemV1 } from "@al-ft/midgard-core";
import {
  CML,
  Data,
  type LucidEvolution,
  type ProtocolParameters,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { BoundedCollectionItemProofV1 } from "@/ledger-state.js";

import type { ValidationTraceDisputeFaultProofContracts } from "./contracts.js";
import {
  ValidationProofItemDatumV1,
  type ValidationProofItemDatumV1 as ValidationProofItemDatumV1Type,
} from "./validation-auxiliary-witness-v1.js";

const hash32 = (value: string, label: string): string => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return value;
};

const exactNonNegativeSafeInteger = (value: bigint, label: string): number => {
  if (value < 0n || value > BigInt(Number.MAX_SAFE_INTEGER)) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  const exact = Number(value);
  if (!Number.isSafeInteger(exact)) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return exact;
};

export type ValidationProofItemPublicationV1 = {
  readonly datum: ValidationProofItemDatumV1Type;
  readonly datumCbor: string;
};

const MIN_ADA_STABILIZATION_LIMIT = 8;

const resolveProtocolParameters = async (
  lucid: LucidEvolution,
): Promise<ProtocolParameters> => {
  const config = lucid.config();
  if (config.protocolParameters !== undefined) {
    return config.protocolParameters;
  }
  if (config.provider === undefined) {
    throw new Error("Lucid provider is not configured.");
  }
  return await config.provider.getProtocolParameters();
};

export const minimumLovelaceForValidationProofItemPublicationV1 = ({
  contracts,
  publication,
  coinsPerUtxoByte,
}: {
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly publication: ValidationProofItemPublicationV1;
  readonly coinsPerUtxoByte: bigint;
}): bigint => {
  const address = CML.Address.from_bech32(
    contracts.validationTraceDispute.proofItem.spendingScriptAddress,
  );
  const datum = CML.DatumOption.new_datum(
    CML.PlutusData.from_cbor_hex(publication.datumCbor),
  );
  let lovelace = 0n;
  for (let attempt = 0; attempt < MIN_ADA_STABILIZATION_LIMIT; attempt += 1) {
    const required = CML.min_ada_required(
      CML.TransactionOutput.new(
        address,
        CML.Value.from_coin(lovelace),
        datum,
        undefined,
      ),
      coinsPerUtxoByte,
    );
    if (required <= lovelace) {
      return lovelace;
    }
    lovelace = required;
  }
  throw new Error(
    "Failed to stabilize complete validation proof-item min-Ada calculation.",
  );
};

export const deriveValidationProofItemPublicationV1 = ({
  transactionId,
  transactionCommitment,
  collectionProof,
  itemCbor,
}: {
  readonly transactionId: string;
  readonly transactionCommitment: string;
  readonly collectionProof: BoundedCollectionItemProofV1;
  readonly itemCbor: string;
}): ValidationProofItemPublicationV1 => {
  if (!/^(?:[0-9a-f]{2})*$/u.test(itemCbor)) {
    throw new Error("validation proof item must be lowercase hexadecimal CBOR");
  }
  const itemBytes = Buffer.from(itemCbor, "hex");
  const fieldIndex = exactNonNegativeSafeInteger(
    collectionProof.field_index,
    "validation proof field index",
  );
  const itemIndex = exactNonNegativeSafeInteger(
    collectionProof.item_index,
    "validation proof item index",
  );
  if (collectionProof.item_length !== BigInt(itemBytes.length)) {
    throw new Error(
      "validation proof item length does not match collection proof",
    );
  }
  const item = buildMidgardBoundedItemV1({
    fieldIndex,
    itemIndex,
    bytes: itemBytes,
  });
  if (item.commitment.toString("hex") !== collectionProof.item_commitment) {
    throw new Error(
      "validation proof item commitment does not match collection proof",
    );
  }
  const datum: ValidationProofItemDatumV1Type = {
    version: 1n,
    transaction_id: hash32(transactionId, "validation proof transaction id"),
    transaction_commitment: hash32(
      transactionCommitment,
      "validation proof transaction commitment",
    ),
    collection_proof: collectionProof,
    item_cbor: itemCbor,
  };
  return {
    datum,
    datumCbor: Data.to(datum, ValidationProofItemDatumV1),
  };
};

export const buildUnsignedValidationProofItemPublicationV1Program = (
  lucid: LucidEvolution,
  contracts: ValidationTraceDisputeFaultProofContracts,
  publication: ValidationProofItemPublicationV1,
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const protocolParameters = await resolveProtocolParameters(lucid);
      const lovelace = minimumLovelaceForValidationProofItemPublicationV1({
        contracts,
        publication,
        coinsPerUtxoByte: protocolParameters.coinsPerUtxoByte,
      });
      return await lucid
        .newTx()
        .pay.ToAddressWithData(
          contracts.validationTraceDispute.proofItem.spendingScriptAddress,
          { kind: "inline", value: publication.datumCbor },
          { lovelace },
        )
        .complete({ localUPLCEval: true });
    },
    catch: (cause) =>
      new Error(
        `Failed to publish a complete validation proof item: ${
          cause instanceof Error ? cause.message : String(cause)
        }`,
      ),
  });
