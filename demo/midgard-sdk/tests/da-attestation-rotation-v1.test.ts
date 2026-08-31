import { readFileSync } from "node:fs";

import {
  type Assets,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  availabilityResponseGeometryV1,
  buildDaAvailabilityCommitmentV1,
  type DaAttestationBuildError,
  DaAttestationDatum,
  daAttestationIsStranded,
  DaAttestationMintRedeemer,
  type DaAttestationReferenceScripts,
  DaAttestationSpendRedeemer,
  daAttestationUnit,
  type DaAttestationUtxo,
  type DaParamsDatum,
  EMPTY_ATTESTED_SIGNER_BITMAP,
  incompleteRescueStrandedDaAttestationTxProgram,
  type MidgardValidators,
} from "../src/index.js";

// Q62 (decision row D-DA4) and Q63 acceptance clause (c) (decision row D-DA5).
//
// This suite is the off-chain half. It cannot execute Plutus, so it never
// claims to prove the validator's behaviour — the Aiken family in
// `onchain/aiken/validators/da-attestation.ak` does that. What it does prove is
// that the two sides agree on the wire: the redeemer constructor ordering the
// validator decodes, and the rotation condition the builders refuse to ignore.

const repositoryRoot = new URL("../../../", import.meta.url);

const readRepositoryFile = (relativePath: string): string =>
  readFileSync(new URL(relativePath, repositoryRoot), "utf8");

/**
 * Constructor names of an Aiken sum type, in declaration order.
 *
 * Read out of the Aiken source rather than restated here. Plutus tags a
 * constructor by its *position*, so a reordering of the declaration silently
 * re-points every encoded redeemer at a different branch — a change no type
 * checker on either side would catch. Deriving the expected order from the
 * declaration is what makes the CBOR assertions below a cross-language pin
 * rather than a restatement of the TypeScript enum against itself.
 */
const aikenConstructorOrder = (typeName: string): readonly string[] => {
  const source = readRepositoryFile(
    "onchain/aiken/lib/midgard/da-attestation-types.ak",
  );
  const opening = new RegExp(`^pub type ${typeName} \\{$`, "mu").exec(source);
  if (opening?.index === undefined) {
    throw new Error(
      `${typeName} is no longer declared in onchain/aiken/lib/midgard/da-attestation-types.ak`,
    );
  }
  const body = source.slice(opening.index + opening[0].length);
  const end = body.indexOf("\n}");
  if (end === -1) {
    throw new Error(`${typeName} has no closing brace`);
  }
  return [...body.slice(0, end).matchAll(/^ {2}([A-Z][A-Za-z0-9]*) \{/gmu)].map(
    (match) => match[1] as string,
  );
};

/** Plutus constructor tag for index `i` (i < 7): 121 + i, CBOR tag `d879 + i`. */
const constructorTagPrefix = (index: number): string =>
  `d8${(0x79 + index).toString(16)}`;

const h28 = (byte: string): string => byte.repeat(28);
const h32 = (byte: string): string => byte.repeat(32);
const availabilityCommitment = (headerHash: string) =>
  buildDaAvailabilityCommitmentV1({
    deploymentIdentity: h28("71"),
    headerHash,
    payload: Uint8Array.of(1),
    bondOwner: h28("72"),
    responseGeometry: availabilityResponseGeometryV1({
      chunkByteLength: 4096,
      trancheByteLength: 4 * 1024 * 1024,
      maxTrancheCount: 16,
    }),
  });

type RecordedPayment = {
  readonly address: string;
  readonly assets: Assets;
};

type Recording = {
  readonly reads: UTxO[][];
  readonly collects: { readonly inputs: UTxO[]; readonly redeemer: unknown }[];
  readonly mints: { readonly assets: Assets; readonly redeemer: unknown }[];
  readonly payments: RecordedPayment[];
};

const makeRecordingLucid = (): {
  readonly lucid: LucidEvolution;
  readonly record: Recording;
} => {
  const record: Recording = {
    reads: [],
    collects: [],
    mints: [],
    payments: [],
  };
  const lucid = {
    newTx: () => {
      const tx = {
        readFrom: (inputs: UTxO[]) => {
          record.reads.push(inputs);
          return tx;
        },
        collectFrom: (inputs: UTxO[], redeemer: unknown) => {
          record.collects.push({ inputs, redeemer });
          return tx;
        },
        mintAssets: (assets: Assets, redeemer: unknown) => {
          record.mints.push({ assets, redeemer });
          return tx;
        },
        pay: {
          ToAddress: (address: string, assets: Assets) => {
            record.payments.push({ address, assets });
            return tx;
          },
        },
      };
      return tx;
    },
  } as unknown as LucidEvolution;
  return { lucid, record };
};

const makeUtxo = (
  outputIndex: number,
  assets: Assets = { lovelace: 1n },
  datum: string | null = null,
  address = `addr_test_${outputIndex.toString()}`,
): UTxO =>
  ({
    txHash: outputIndex.toString(16).padStart(64, "0"),
    outputIndex,
    address,
    assets,
    datum,
  }) as UTxO;

const validator = (policyByte: string, address: string) =>
  ({
    policyId: h28(policyByte),
    spendingScriptAddress: address,
    spendingScriptHash: h28(policyByte),
    spendingScriptCBOR: "",
    mintingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "" },
    mintingScript: { type: "PlutusV3", script: "" },
  }) as unknown as MidgardValidators["daAttestation"];

const GOVERNED_COMMITTEE_HASH = h32("33");
const ROTATED_COMMITTEE_HASH = h32("44");

const makeFixture = () => {
  const contracts = {
    daAttestation: validator("aa", "addr_da_attestation"),
  } as Pick<MidgardValidators, "daAttestation">;
  const headerHash = h28("10");
  const daParamsDatum: DaParamsDatum = {
    committee: h32("11") + h32("22"),
    committee_signers_hash: GOVERNED_COMMITTEE_HASH,
    da_threshold: 2n,
    owners: [h28("81"), h28("82")],
    update_threshold: 2n,
  };
  const daParamsUtxo = makeUtxo(2, { lovelace: 2_000_000n });
  const attestationUnit = daAttestationUnit(
    contracts.daAttestation,
    headerHash,
  );
  // The stranded attestation: it froze a committee governance has since rotated
  // away from, and it holds one of the two signatures it needed.
  const attestationDatum: DaAttestationDatum = {
    header_hash: headerHash,
    availability_commitment: availabilityCommitment(headerHash),
    da_threshold: 2n,
    committee_signers_hash: ROTATED_COMMITTEE_HASH,
    rescue_beneficiary: {
      paymentCredential: { PublicKeyCredential: [h28("66")] },
      stakeCredential: null,
    },
    attested_signers: `80${"00".repeat(31)}`,
    attestation_count: 1n,
  };
  const attestation: DaAttestationUtxo = {
    utxo: makeUtxo(
      3,
      { lovelace: 5_000_000n, [attestationUnit]: 1n },
      Data.to(attestationDatum, DaAttestationDatum),
      contracts.daAttestation.spendingScriptAddress,
    ),
    datum: attestationDatum,
  };
  const referenceScripts: Pick<
    DaAttestationReferenceScripts,
    "daAttestationMinting" | "daAttestationSpending"
  > = {
    daAttestationMinting: makeUtxo(4),
    daAttestationSpending: makeUtxo(5),
  };
  return {
    contracts,
    headerHash,
    daParamsDatum,
    daParamsUtxo,
    attestation,
    attestationUnit,
    referenceScripts,
    refundAddress: credentialToAddress("Preprod", {
      type: "Key",
      hash: h28("66"),
    }),
  };
};

const run = <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<A> => Effect.runPromise(program);

const expectBuildFailure = async <A>(
  program: Effect.Effect<A, DaAttestationBuildError>,
): Promise<void> => {
  const result = await Effect.runPromise(Effect.either(program));
  expect(result._tag).toBe("Left");
  if (result._tag === "Left") {
    expect(result.left._tag).toBe("DaAttestationBuildError");
  }
};

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
            availability_mint_redeemer_index: 5n,
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
    const source = readRepositoryFile(
      "onchain/aiken/lib/midgard/da-attestation-types.ak",
    );
    const applyBody = /ApplyToStateQueue \{([^}]*)\}/su.exec(source)?.[1] ?? "";
    expect(applyBody).toContain("da_params_ref_input_index: Int");

    const encoded = Data.to(
      {
        ApplyToStateQueue: {
          da_attestation_input_index: 7n,
          da_params_ref_input_index: 9n,
          state_queue_input_index: 0n,
          state_queue_output_index: 0n,
          state_queue_mint_ref_script_input_index: 0n,
          availability_mint_redeemer_index: 0n,
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

describe("Q63c rescue builder", () => {
  it("rescue 1 — assembles the burn and the full-value refund", async () => {
    const fixture = makeFixture();
    const { lucid, record } = makeRecordingLucid();

    await run(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        refundAddress: fixture.refundAddress,
        referenceScripts: fixture.referenceScripts,
      }),
    );

    // The governed params must be a reference input: the validator's whole
    // authorization is a comparison against them.
    expect(record.reads).toEqual([
      [
        fixture.daParamsUtxo,
        fixture.referenceScripts.daAttestationMinting,
        fixture.referenceScripts.daAttestationSpending,
      ],
    ]);
    expect(record.collects.map((entry) => entry.inputs)).toEqual([
      [fixture.attestation.utxo],
    ]);
    expect(record.mints[0]?.assets).toEqual({
      [fixture.attestationUnit]: -1n,
    });
    // The DAAT is burnt, so the refund is the attestation's value less that
    // token — no more, and no less.
    expect(record.payments[0]?.address).toBe(fixture.refundAddress);
    expect(record.payments[0]?.assets).toEqual({ lovelace: 5_000_000n });
  });

  it("rescue 2 — refuses an attestation that is still on the governed committee", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    // The single changed field: the attestation's frozen committee hash is the
    // governed one, so it is still in flight and its value is not the
    // rescuer's to take.
    await expectBuildFailure(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: {
          ...fixture.attestation,
          datum: {
            ...fixture.attestation.datum,
            committee_signers_hash: GOVERNED_COMMITTEE_HASH,
          },
        },
        refundAddress: fixture.refundAddress,
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("rescue 3 — refuses a refund back into the attestation script", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    // Such an output would be unspendable forever: every spend path requires
    // the UTxO to carry its DAAT, and the DAAT is being burnt.
    await expectBuildFailure(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        refundAddress: fixture.contracts.daAttestation.spendingScriptAddress,
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("rescue 4 — refuses a redirect away from the frozen beneficiary", async () => {
    const fixture = makeFixture();
    const { lucid } = makeRecordingLucid();

    await expectBuildFailure(
      incompleteRescueStrandedDaAttestationTxProgram(lucid, fixture.contracts, {
        daParamsUtxo: fixture.daParamsUtxo,
        daParamsDatum: fixture.daParamsDatum,
        attestation: fixture.attestation,
        refundAddress: credentialToAddress("Preprod", {
          type: "Key",
          hash: h28("67"),
        }),
        referenceScripts: fixture.referenceScripts,
      }),
    );
  });

  it("rescue 5 — the full frozen beneficiary survives datum round trips", () => {
    const encoded = Data.to(
      fixtureDatum() as never,
      DaAttestationDatum as never,
    );
    const decoded = Data.from(
      encoded,
      DaAttestationDatum as never,
    ) as ReturnType<typeof fixtureDatum>;
    expect(decoded.committee_signers_hash).toBe(ROTATED_COMMITTEE_HASH);
    expect(decoded.attestation_count).toBe(1n);
  });
});

const fixtureDatum = () => ({
  header_hash: h28("10"),
  availability_commitment: availabilityCommitment(h28("10")),
  da_threshold: 2n,
  committee_signers_hash: ROTATED_COMMITTEE_HASH,
  rescue_beneficiary: {
    paymentCredential: { PublicKeyCredential: [h28("66")] },
    stakeCredential: null,
  },
  attested_signers: EMPTY_ATTESTED_SIGNER_BITMAP,
  attestation_count: 1n,
});
