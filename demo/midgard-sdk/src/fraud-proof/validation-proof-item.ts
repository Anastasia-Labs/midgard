import {
  CML,
  Data,
  type LucidEvolution,
  type ProtocolParameters,
  type TxSignBuilder,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import type { ValidationTraceDisputeFaultProofContracts } from "./contracts/index.js";
import {
  ValidationProofItemDatum,
  type ValidationProofItemDatum as ValidationProofItemDatumType,
} from "./validation-auxiliary-witness.js";

const hash32 = (value: string, label: string): string => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be 32-byte lowercase hex`);
  }
  return value;
};

export type ValidationProofItemPublication = {
  readonly datum: ValidationProofItemDatumType;
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

export const minimumLovelaceForValidationProofItemPublication = ({
  contracts,
  publication,
  coinsPerUtxoByte,
}: {
  readonly contracts: ValidationTraceDisputeFaultProofContracts;
  readonly publication: ValidationProofItemPublication;
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

/**
 * Builds the publication a `CanonicalDecode` complete-item step reaches by
 * reference input.
 *
 * **What it publishes is the field's whole §5.1 preimage, not one item** (#597,
 * the TypeScript twin of #592's wire change). Under §4 a field commits to a flat
 * `blake2b_256` over its preimage bytes, so a per-item opening has nothing to be
 * checked against and the unit that authenticates is the preimage. The consuming
 * step names this UTxO by reference-input index and never a tier;
 * `canonical_decode_item_semantic_v1`'s `proof_item_from_reference` constructs
 * `Inline { preimage }` from these bytes itself, so no prover can name a carriage
 * the door was not going to hash.
 *
 * The two bindings are what make the publication non-fungible: a look-alike UTxO
 * at the same address cannot pass a preimage off as belonging to a different
 * dispute, because the door checks both against the step's pre-state.
 */
export const deriveValidationProofItemPublication = ({
  transactionId,
  transactionCommitment,
  fieldPreimage,
}: {
  readonly transactionId: string;
  readonly transactionCommitment: string;
  /** The §5.1 enveloped field preimage, lowercase hexadecimal. */
  readonly fieldPreimage: string;
}): ValidationProofItemPublication => {
  if (!/^(?:[0-9a-f]{2})*$/u.test(fieldPreimage)) {
    throw new Error(
      "validation proof field preimage must be lowercase hexadecimal CBOR",
    );
  }
  if (fieldPreimage.length === 0) {
    throw new Error("validation proof field preimage must not be empty");
  }
  const datum: ValidationProofItemDatumType = {
    version: 1n,
    transaction_id: hash32(transactionId, "validation proof transaction id"),
    transaction_commitment: hash32(
      transactionCommitment,
      "validation proof transaction commitment",
    ),
    field_preimage: fieldPreimage,
  };
  return {
    datum,
    datumCbor: Data.to(datum, ValidationProofItemDatum),
  };
};

export const buildUnsignedValidationProofItemPublicationProgram = (
  lucid: LucidEvolution,
  contracts: ValidationTraceDisputeFaultProofContracts,
  publication: ValidationProofItemPublication,
): Effect.Effect<TxSignBuilder, Error> =>
  Effect.tryPromise({
    try: async () => {
      const protocolParameters = await resolveProtocolParameters(lucid);
      const lovelace = minimumLovelaceForValidationProofItemPublication({
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
