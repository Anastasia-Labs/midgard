import { h28 } from "@al-ft/midgard-test-support/hex";
import { type Constr, Data } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  daAttestationIsStranded,
  DaAttestationMintRedeemer,
  DaAttestationSpendRedeemer,
  EMPTY_ATTESTED_SIGNER_BITMAP,
} from "../src/index.js";
import {
  aikenConstructorFields,
  aikenConstructorOrder,
  availabilityCommitment,
  constructorTagPrefix,
  GOVERNED_COMMITTEE_HASH,
  ROTATED_COMMITTEE_HASH,
} from "./da-attestation-rotation.make-fixture.js";

describe("Q62 DA redeemer constructor ABI", () => {
  it("abi 1 — mint constructors keep the declared Aiken order on the wire", () => {
    const declared = aikenConstructorOrder("MintRedeemer");
    expect(declared).toStrictEqual([
      "Init",
      "ApplyToStateQueue",
      "RescueStrandedAttestation",
    ]);

    // `RescueStrandedAttestation` is appended, so `Init` and
    // `ApplyToStateQueue` keep tags 0 and 1 and no already-deployed redeemer
    // encoding moves.
    const encoded = {
      Init: Data.to(
        {
          Init: {
            output_index: 0n,
            da_params_ref_input_index: 1n,
            state_queue_ref_input_index: 2n,
            state_queue_mint_ref_script_input_index: 3n,
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      ),
      ApplyToStateQueue: Data.to(
        {
          ApplyToStateQueue: {
            da_attestation_input_index: 0n,
            da_params_ref_input_index: 1n,
            state_queue_input_index: 2n,
            state_queue_output_index: 3n,
            state_queue_mint_ref_script_input_index: 4n,
            pool_ref_input_index: 5n,
            refund_output_index: 6n,
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      ),
      RescueStrandedAttestation: Data.to(
        {
          RescueStrandedAttestation: {
            da_attestation_input_index: 0n,
            da_params_ref_input_index: 1n,
            refund_output_index: 2n,
          },
        } satisfies DaAttestationMintRedeemer as never,
        DaAttestationMintRedeemer as never,
      ),
    };

    declared.forEach((constructorName, index) => {
      expect(
        encoded[constructorName as keyof typeof encoded].startsWith(
          constructorTagPrefix(index),
        ),
      ).toBe(true);
    });
  });

  it("abi 2 — spend constructors keep the declared Aiken order on the wire", () => {
    const declared = aikenConstructorOrder("SpendRedeemer");
    expect(declared).toStrictEqual([
      "AddSignatures",
      "BurnForStateQueue",
      "BurnForRescue",
    ]);

    const encoded = {
      AddSignatures: Data.to(
        {
          AddSignatures: {
            output_index: 0n,
            da_params_ref_input_index: 1n,
            signatures: "ab",
          },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      ),
      BurnForStateQueue: Data.to(
        {
          BurnForStateQueue: { mint_redeemer_index: 0n },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      ),
      BurnForRescue: Data.to(
        {
          BurnForRescue: { mint_redeemer_index: 0n },
        } satisfies DaAttestationSpendRedeemer as never,
        DaAttestationSpendRedeemer as never,
      ),
    };

    declared.forEach((constructorName, index) => {
      expect(
        encoded[constructorName as keyof typeof encoded].startsWith(
          constructorTagPrefix(index),
        ),
      ).toBe(true);
    });
  });

  it("abi 3 — the apply redeemer carries the governed-params reference index", () => {
    // The field D-DA4 adds. Without it the apply handler has no way to reach
    // the current DA params, which is precisely how rotation stayed
    // non-retroactive.
    // The pooled-bond apply (#688 register C3) appends the pool reference and
    // the beneficiary refund and no longer names an availability-policy mint.
    expect(aikenConstructorFields("MintRedeemer", "ApplyToStateQueue")).toEqual(
      [
        "da_attestation_input_index",
        "da_params_ref_input_index",
        "state_queue_input_index",
        "state_queue_output_index",
        "state_queue_mint_ref_script_input_index",
        "pool_ref_input_index",
        "refund_output_index",
      ],
    );

    const encoded = Data.to(
      {
        ApplyToStateQueue: {
          da_attestation_input_index: 7n,
          da_params_ref_input_index: 9n,
          state_queue_input_index: 0n,
          state_queue_output_index: 0n,
          state_queue_mint_ref_script_input_index: 0n,
          pool_ref_input_index: 0n,
          refund_output_index: 0n,
        },
      } satisfies DaAttestationMintRedeemer as never,
      DaAttestationMintRedeemer as never,
    );
    const decoded = Data.from(
      encoded,
      DaAttestationMintRedeemer as never,
    ) as DaAttestationMintRedeemer;
    expect("ApplyToStateQueue" in decoded).toBe(true);
    if ("ApplyToStateQueue" in decoded) {
      expect(decoded.ApplyToStateQueue.da_params_ref_input_index).toBe(9n);
      expect(decoded.ApplyToStateQueue.da_attestation_input_index).toBe(7n);
    }
  });
});

describe("DA redeemer field ABI", () => {
  // Plutus encodes a constructor's fields by position, so a TypeScript schema
  // whose field order drifts from the Aiken declaration still encodes and
  // decodes against itself while the validator reads every index from the
  // wrong slot. Each field below is set to its Aiken position: the raw
  // constructor reads 0, 1, 2, ... only when the TypeScript schema encodes the
  // same fields, in the same order, with none missing or extra.
  const cases = [
    ["MintRedeemer", "Init", 0, DaAttestationMintRedeemer],
    ["MintRedeemer", "ApplyToStateQueue", 1, DaAttestationMintRedeemer],
    ["MintRedeemer", "RescueStrandedAttestation", 2, DaAttestationMintRedeemer],
    ["SpendRedeemer", "AddSignatures", 0, DaAttestationSpendRedeemer],
    ["SpendRedeemer", "BurnForStateQueue", 1, DaAttestationSpendRedeemer],
    ["SpendRedeemer", "BurnForRescue", 2, DaAttestationSpendRedeemer],
  ] as const;

  it.each(cases)(
    "%s constructor '%s' (tag %i) encodes its fields in the Aiken order",
    (typeName, constructorName, tag, schema) => {
      const fields = aikenConstructorFields(typeName, constructorName);
      expect(fields.length).toBeGreaterThan(0);
      const fieldValue = (name: string, index: number): bigint | string =>
        name === "signatures" ? "ab" : BigInt(index);
      const cbor = Data.to(
        {
          [constructorName]: Object.fromEntries(
            fields.map((name, index) => [name, fieldValue(name, index)]),
          ),
        } as never,
        schema as never,
      );
      const raw = Data.from(cbor) as Constr<Data>;
      expect(raw.index).toBe(tag);
      expect(raw.fields).toEqual(fields.map(fieldValue));
    },
  );
});

describe("Q62 rotation predicate", () => {
  it("predicate 1 — an attestation matching both governed values is not stranded", () => {
    expect(
      daAttestationIsStranded({
        attestationDatum: {
          committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          da_threshold: 2n,
        },
        daParamsDatum: {
          committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          da_threshold: 2n,
        },
      }),
    ).toBe(false);
  });

  it("predicate 2 — a rotated-out committee strands the attestation", () => {
    expect(
      daAttestationIsStranded({
        attestationDatum: {
          committee_signers_hash: ROTATED_COMMITTEE_HASH,
          da_threshold: 2n,
        },
        daParamsDatum: {
          committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          da_threshold: 2n,
        },
      }),
    ).toBe(true);
  });

  it("predicate 3 — a governed threshold change strands the attestation even on an unchanged committee", () => {
    // The second strand condition D-DA4 introduced. Apply requires both frozen
    // values to still match, so a threshold-only update makes an attestation
    // unappliable; if the predicate ignored the threshold there would be no
    // rescue for it and its ADA would be locked for good.
    expect(
      daAttestationIsStranded({
        attestationDatum: {
          committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          da_threshold: 2n,
        },
        daParamsDatum: {
          committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          da_threshold: 3n,
        },
      }),
    ).toBe(true);
  });
});

export const fixtureDatum = () => ({
  header_hash: h28(0x10),
  availability_commitment: availabilityCommitment(h28(0x10)),
  da_threshold: 2n,
  committee_signers_hash: ROTATED_COMMITTEE_HASH,
  rescue_beneficiary: {
    paymentCredential: { PublicKeyCredential: [h28(0x66)] },
    stakeCredential: null,
  },
  attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
  attestation_count: 1n,
});
