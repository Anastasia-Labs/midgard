import {
  credentialToAddress,
  Data,
  type Network,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  type AddressData,
  addressDataFromBech32,
  AddressSchema,
} from "./common.js";
import {
  DaAttestationBuildError,
  failBuild,
} from "./da-attestation.apply-da-attestation-signature-witnesses.js";
import { credentialFromData } from "./da-attestation.incomplete-init-da-attestation-tx-program.js";

/**
 * The bech32 form of the attestation's frozen `rescue_beneficiary`, checked to
 * decode back to exactly the same Plutus address data: the validator compares
 * the refund output's address to the datum field by equality.
 */
export const rescueBeneficiaryBech32 = (
  network: Network,
  beneficiary: AddressData,
): Effect.Effect<string, DaAttestationBuildError> =>
  Effect.gen(function* () {
    const stake = beneficiary.stakeCredential;
    if (stake !== null && !("Inline" in stake)) {
      return yield* failBuild(
        "rescue_refund_address_undecodable",
        "DA attestation rescue beneficiary uses a pointer stake credential, which cannot be paid as a bech32 address",
        JSON.stringify(stake, (_key, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
        ),
      );
    }
    const address =
      stake === null
        ? credentialToAddress(
            network,
            credentialFromData(beneficiary.paymentCredential),
          )
        : credentialToAddress(
            network,
            credentialFromData(beneficiary.paymentCredential),
            credentialFromData(stake.Inline[0]),
          );
    const roundTrip = yield* addressDataFromBech32(address).pipe(
      Effect.mapError(
        (cause) =>
          new DaAttestationBuildError({
            reason: "rescue_refund_address_undecodable",
            message: "Failed to decode the DA attestation apply refund address",
            cause,
          }),
      ),
    );
    if (
      Data.to(roundTrip as never, AddressSchema as never) !==
      Data.to(beneficiary as never, AddressSchema as never)
    ) {
      return yield* failBuild(
        "rescue_refund_beneficiary_mismatch",
        "DA attestation apply refund address does not round-trip to the frozen beneficiary",
        address,
      );
    }
    return address;
  });
