import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  Data,
  getAddressDetails,
  type Native,
  scriptFromNative,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  DisputeBond,
  DisputeBondSpendRedeemer,
  protocolTreasuryAddress,
  protocolTreasuryScriptHash,
} from "../src/index.js";

// The same vectors are pinned by `midgard/dispute_bond_v1.test` and by the
// profile generator's native-script encoder test.
const placeholderKey = (index: string) =>
  `706c616365686f6c6465723a74726573${"00".repeat(11)}${index}`;
const treasuryScript: Native = {
  type: "atLeast",
  required: 2,
  scripts: ["01", "02", "03"].map((index) => ({
    type: "sig" as const,
    keyHash: placeholderKey(index),
  })),
};
const treasuryScriptCbor =
  "830302838200581c706c616365686f6c6465723a747265730000000000000000000000018200581c706c616365686f6c6465723a747265730000000000000000000000028200581c706c616365686f6c6465723a74726573000000000000000000000003";
const treasuryScriptHash =
  "ab68ab329de629beaa7f6b56e67ed93ead3ccb7c82571c60f798fdcd";

describe("validation-dispute bond schemas", () => {
  it("encode the bond datum exactly as the validator reads it", () => {
    const bond: DisputeBond = {
      challenged_header_hash: "aa".repeat(28),
      fraud_prover: "a1".repeat(28),
      forfeit_after: 1_800_000_000_000n,
    };
    const cbor = Data.to(bond, DisputeBond);
    expect(cbor).toBe(
      "d8799f581caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa581ca1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a1a11b000001a3185c5000ff",
    );
    expect(Data.from(cbor, DisputeBond)).toEqual(bond);
  });

  it("encode Refund and Forfeit with the validator's constructor indices", () => {
    expect(
      Data.to(
        {
          Refund: { fraud_proof_ref_input_index: 1n, output_index: 2n },
        },
        DisputeBondSpendRedeemer,
      ),
    ).toBe("d8799f0102ff");
    expect(
      Data.to({ Forfeit: { output_index: 3n } }, DisputeBondSpendRedeemer),
    ).toBe("d87a9f03ff");
  });
});

describe("protocol treasury", () => {
  it("hashes the native script as the ledger and the on-chain env do", () => {
    expect(scriptFromNative(treasuryScript).script).toBe(treasuryScriptCbor);
    expect(protocolTreasuryScriptHash(treasuryScript)).toBe(treasuryScriptHash);
  });

  it("pays forfeits to the unstaked treasury script address", () => {
    const details = getAddressDetails(
      protocolTreasuryAddress(
        "Preprod",
        SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
      ),
    );
    expect(details.paymentCredential).toEqual({
      type: "Script",
      hash: protocolTreasuryScriptHash(
        SELECTED_DEPLOYMENT_PROFILE.protocol_treasury_native_script,
      ),
    });
    expect(details.stakeCredential).toBeUndefined();
  });
});
