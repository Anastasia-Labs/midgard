/** Reference deployment and the two independent step-01 carriage frontiers. */
import {
  computeHash32,
  computeMidgardNativeTxIdV1,
  encodeMidgardNativeTxCompactV1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
} from "@al-ft/midgard-core";
import {
  AddressData,
  addressDataFromBech32,
  CanonicalDecodabilityStep01SpendRedeemer,
  type CanonicalDecodabilityStep01SpendRedeemer as Step01Redeemer,
} from "@al-ft/midgard-sdk";
import {
  credentialToAddress,
  Data,
  scriptHashToCredential,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1,
  prepareCanonicalDecodabilityV1,
  requireCanonicalDecodabilityReferenceScriptV1,
} from "../src/canonical-decodability/index.js";
import { measureBlueprintValidatorBytes } from "../src/runtime.js";
import {
  buildCanonicalDecodabilityBodyFixtureV1,
  network,
} from "./support/canonical-decodability-emulator-v1.js";
import {
  buildCanonicalDecodabilityChainV1,
  EMULATOR_PROTOCOL_PARAMETERS,
  makeNativeTx,
  readBlueprint,
  realBlueprintPath,
} from "./support/submit-init-emulator-shared.js";

const dataBytes = (hex: string): number => hex.length / 2;

describe("canonical-decodability envelope and deployment frontiers", () => {
  const blueprint = readBlueprint(realBlueprintPath);

  it("applies two distinct validators that fit the oversized publication host", async () => {
    const declaredArities = { step01: 4, step02: 3 } as const;
    for (const [step, title] of Object.entries(
      CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1,
    )) {
      expect(
        measureBlueprintValidatorBytes({
          blueprint,
          title,
          expectedDeclaredParameterCount:
            declaredArities[step as keyof typeof declaredArities],
        }),
        title,
      ).toBeGreaterThan(0);
    }
    const addressData = await Effect.runPromise(
      addressDataFromBech32(
        credentialToAddress(network, scriptHashToCredential("22".repeat(28))),
      ).pipe(Effect.map((value) => Data.from(Data.to(value, AddressData)))),
    );
    const steps = buildCanonicalDecodabilityChainV1({
      realBlueprint: blueprint,
      computationThreadPolicyId: "11".repeat(28),
      fraudProofPolicyId: "33".repeat(28),
      fraudProofTokenAddressData: addressData,
      fieldPreimageCertificatePolicyId: "44".repeat(28),
      hubOraclePolicyId: "55".repeat(28),
    });
    expect(new Set(steps.map((step) => step.spendingScriptHash)).size).toBe(2);
    for (const step of steps) {
      expect(step.spendingScriptCBOR.length / 2 + 2_048).toBeLessThan(
        EMULATOR_PROTOCOL_PARAMETERS.maxTxSize,
      );
    }
  });

  it("encodes both inclusion carriages and keeps the shipped inline claim inside the L1 envelope", async () => {
    const fixture = await buildCanonicalDecodabilityBodyFixtureV1();
    if (fixture.prepared === null)
      throw new Error("Expected violating fixture");
    const common = {
      input_index: 0n,
      output_index: 0n,
      hub_ref_input_index: 0n,
      state_queue_node_ref_input_index: 1n,
      native_tx_id: fixture.badTxId,
      native_tx_compact_cbor: fixture.nativeTxCompactCbor,
      transactions_phas_root: fixture.transactionsRoot,
    };
    const redeemerCarried: Step01Redeemer = {
      Continue: [
        {
          inclusion: {
            RedeemerCarriedInclusion: [
              {
                ...common,
                tx_membership_proof: fixture.txInclusion.txMembershipProof,
                inclusion_proof_script_withdraw_redeemer_index: 0n,
              },
            ],
          },
          claim: fixture.prepared.claim,
        },
      ],
    };
    const publishedChunk: Step01Redeemer = {
      Continue: [
        {
          inclusion: {
            PublishedChunkInclusion: [
              {
                ...common,
                ordered_chunk_reference_input_indices: [2n],
              },
            ],
          },
          claim: fixture.prepared.claim,
        },
      ],
    };
    const carriedBytes = dataBytes(
      Data.to(redeemerCarried, CanonicalDecodabilityStep01SpendRedeemer),
    );
    const publishedBytes = dataBytes(
      Data.to(publishedChunk, CanonicalDecodabilityStep01SpendRedeemer),
    );
    // A single-leaf proof has no branch steps, so replacing that empty proof
    // with one reference-input index is intentionally within one byte here.
    expect(Math.abs(carriedBytes - publishedBytes)).toBeLessThanOrEqual(1);
    expect(carriedBytes + 2_048).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS_V1.minSupportedL1MaxTxBytes,
    );
  });

  it("pins the inline field frontier and refuses to mis-tag larger fields", () => {
    const preimage = Buffer.alloc(
      MIDGARD_MAX_TIER1_REDEEMER_PREIMAGE_BYTES_V1,
      0,
    );
    preimage[0] = 0x80;
    const honest = makeNativeTx({ spendInputCbors: [], fee: 1n });
    const compact = {
      ...honest.compact,
      transactionBody: {
        ...honest.compact.transactionBody,
        outputsHash: computeHash32(preimage),
      },
    };
    const common = {
      badTxId: computeMidgardNativeTxIdV1(compact).toString("hex"),
      nativeTxCompactCbor:
        encodeMidgardNativeTxCompactV1(compact).toString("hex"),
      fieldIndex: 2,
    } as const;
    expect(
      prepareCanonicalDecodabilityV1({
        ...common,
        committedPreimage: preimage,
      }).committedPreimage,
    ).toHaveLength(preimage.length * 2);

    const tooLarge = Buffer.concat([preimage, Buffer.from([0])]);
    const tooLargeCompact = {
      ...compact,
      transactionBody: {
        ...compact.transactionBody,
        outputsHash: computeHash32(tooLarge),
      },
    };
    expect(() =>
      prepareCanonicalDecodabilityV1({
        badTxId: computeMidgardNativeTxIdV1(tooLargeCompact).toString("hex"),
        nativeTxCompactCbor:
          encodeMidgardNativeTxCompactV1(tooLargeCompact).toString("hex"),
        fieldIndex: 2,
        committedPreimage: tooLarge,
      }),
    ).toThrow(/above the .* inline frontier/u);
  });

  it("rejects a missing reference script before transaction construction", () => {
    expect(() =>
      requireCanonicalDecodabilityReferenceScriptV1({
        utxo: {
          txHash: "aa".repeat(32),
          outputIndex: 0,
        } as UTxO,
        expectedScriptHash: "bb".repeat(28),
        stepIndex: 0,
      }),
    ).toThrow(/carries no reference script/u);
  });
});
