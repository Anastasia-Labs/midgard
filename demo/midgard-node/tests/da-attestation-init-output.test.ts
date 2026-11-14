import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  credentialToAddress,
  Data,
  type LucidEvolution,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import { daAttestationInitOutputLovelace } from "../src/transactions/da-attestation.js";
import { TEST_AVAILABILITY_PARAMETERS } from "./helpers/availability-challenge.js";

const COINS_PER_UTXO_BYTE = 4_310n;
const ATTESTATION_ADDRESS = credentialToAddress("Preprod", {
  type: "Script",
  hash: "ef".repeat(28),
});
const HEADER_HASH = "cd".repeat(28);
const ATTESTATION_UNIT = `${"ef".repeat(28)}${"00".repeat(4)}${HEADER_HASH}`;

const lucidWith = (coinsPerUtxoByte: bigint | undefined) =>
  ({
    config: () => ({
      protocolParameters:
        coinsPerUtxoByte === undefined ? undefined : { coinsPerUtxoByte },
    }),
  }) as unknown as Pick<LucidEvolution, "config">;

const commitment = SDK.buildDaAvailabilityCommitment({
  deploymentIdentity: "ab".repeat(28),
  headerHash: HEADER_HASH,
  payload: new Uint8Array(4_096).fill(7),
  responseGeometry: TEST_AVAILABILITY_PARAMETERS.response_geometry,
});

const daParamsDatum = {
  da_threshold: 2n,
  committee_signers_hash: "12".repeat(32),
} as SDK.DaParamsDatum;

const rescueBeneficiary = Effect.runSync(
  SDK.addressDataFromBech32(
    credentialToAddress("Preprod", { type: "Key", hash: "34".repeat(28) }),
  ),
);

/** The min-UTxO of the attestation output at a given `attestation_count`. */
const minAtCount = (attestationCount: bigint): bigint =>
  calculateMinLovelaceFromUTxO(COINS_PER_UTXO_BYTE, {
    txHash: "00".repeat(32),
    outputIndex: 0,
    address: ATTESTATION_ADDRESS,
    assets: { lovelace: 0n, [ATTESTATION_UNIT]: 1n },
    datum: Data.to(
      {
        header_hash: HEADER_HASH,
        availability_commitment: commitment,
        da_threshold: daParamsDatum.da_threshold,
        committee_signers_hash: daParamsDatum.committee_signers_hash,
        rescue_beneficiary: rescueBeneficiary,
        attested_signers: SDK.EMPTY_ATTESTED_SIGNER_BITMAP,
        attestation_count: attestationCount,
      } as never,
      SDK.DaAttestationDatum as never,
    ),
  });

const initOutput = (coinsPerUtxoByte: bigint | undefined) =>
  daAttestationInitOutputLovelace(lucidWith(coinsPerUtxoByte), {
    attestationAddress: ATTESTATION_ADDRESS,
    attestationUnit: ATTESTATION_UNIT,
    headerHash: HEADER_HASH,
    availabilityCommitment: commitment,
    daParamsDatum,
    rescueBeneficiary,
  });

describe("DA attestation init output lovelace", () => {
  it("covers the output's min-UTxO at every attestation_count add-signatures can reach", () => {
    const locked = Effect.runSync(initOutput(COINS_PER_UTXO_BYTE));
    // Add-signatures keeps the value and grows only the count; the CBOR width
    // of the count steps at 24 and 256.
    for (const count of [0n, 1n, 23n, 24n, 255n, 256n]) {
      expect(locked).toBeGreaterThanOrEqual(minAtCount(count));
    }
    expect(minAtCount(256n)).toBeGreaterThan(minAtCount(0n));
    expect(locked).toBe(minAtCount(256n));
  });

  it("fails without protocol parameters instead of guessing a minimum", () => {
    const failure = Effect.runSync(Effect.flip(initOutput(undefined)));
    expect(failure).toBeInstanceOf(SDK.LucidError);
  });
});
