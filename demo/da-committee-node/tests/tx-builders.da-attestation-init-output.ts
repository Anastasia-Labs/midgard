import { readFile } from "node:fs/promises";

import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import {
  calculateMinLovelaceFromUTxO,
  credentialToAddress,
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE,
  DA_L1_SUBMITTER_FEE_HEADROOM_LOVELACE,
  DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE,
} from "../src/config.js";
import {
  addSignaturesToDaAttestationDatum,
  buildAddSignaturesTx,
  buildInitDaAttestationTx,
  DA_ATTESTATION_WIDEST_COUNT,
  daAttestationApplyValidityRange,
  daAttestationInitOutputLovelace,
  type DaAttestationTarget,
} from "../src/coordinator/tx-builders.js";
import type { DaAttestationValidatorSet } from "../src/l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../src/l1/reference-scripts.js";
import {
  availabilityChallengeValidator,
  outRef,
  stateQueueValidator,
  utxo,
  validator,
} from "./tx-builders.state-queue-validator.js";

describe("DA attestation transaction builders", () => {
  it("updates signer bitmap and count for arbitrary signer indexes", () => {
    const updated = addSignaturesToDaAttestationDatum(
      baseAttestationDatum(),
      [0, 9],
    );
    expect(updated.attested_signers.startsWith("8040")).toBe(true);
    expect(updated.attestation_count).toBe(2n);

    const withExisting = addSignaturesToDaAttestationDatum(updated, [2]);
    expect(withExisting.attested_signers.startsWith("a040")).toBe(true);
    expect(withExisting.attestation_count).toBe(3n);
    expect(() => addSignaturesToDaAttestationDatum(updated, [0])).toThrow(
      /already attested/,
    );
    expect(() =>
      addSignaturesToDaAttestationDatum(baseAttestationDatum(), [2, 2]),
    ).toThrow(/distinct new witnesses/);
    expect(() =>
      addSignaturesToDaAttestationDatum(baseAttestationDatum(), []),
    ).toThrow(/at least one/);
  });

  it("builds AddSignatures with updated datum and defaults to local UPLC evaluation", async () => {
    const builder = new FakeTxBuilder();
    const attestationUtxo = utxo("03", 0, {
      lovelace: 5_000_000n,
      [SDK.daAttestationUnit(contracts.daAttestation, HEADER_HASH)]: 1n,
    });

    await buildAddSignaturesTx({
      lucid: fakeLucid(builder),
      contracts,
      daParamsUtxo: utxo("01", 0),
      attestationUtxo,
      attestationDatum: baseAttestationDatum(),
      packedWitnessesHex: `00${"aa".repeat(64)}09${"bb".repeat(64)}`,
      signerIndexes: [0, 9],
      referenceScripts,
    });

    expect(builder.reads.map((entries) => entries.map(outRef))).toEqual([
      [`${"01".repeat(32)}#0`, `${"05".repeat(32)}#0`],
    ]);
    expect(builder.collects.map((entry) => entry.utxos.map(outRef))).toEqual([
      [`${"03".repeat(32)}#0`],
    ]);
    expect(builder.payments).toHaveLength(1);
    expect(builder.payments[0]?.address).toBe(
      contracts.daAttestation.spendingScriptAddress,
    );
    expect(builder.payments[0]?.assets).toEqual(attestationUtxo.assets);
    const updatedDatum = Data.from(
      builder.payments[0]!.datum.value,
      SDK.DaAttestationDatum as never,
    ) as SDK.DaAttestationDatum;
    expect(updatedDatum.attested_signers.startsWith("8040")).toBe(true);
    expect(updatedDatum.attestation_count).toBe(2n);
    const redeemer = Data.from(
      (builder.collects[0]!.redeemer as (ctx: unknown) => string)({
        outputs: builder.payments.map((payment) => ({
          address: payment.address,
          assets: payment.assets,
          datum: payment.datum.value,
        })),
        referenceInputs: builder.reads[0],
      }),
      SDK.DaAttestationSpendRedeemer as never,
    ) as SDK.DaAttestationSpendRedeemer;
    expect(redeemer).toMatchObject({
      AddSignatures: {
        signatures: `00${"aa".repeat(64)}09${"bb".repeat(64)}`,
      },
    });
    expect(builder.signerKeys).toHaveLength(0);
    expect(builder.completeOptions).toEqual([{ localUPLCEval: true }]);
  });

  it("leaves local evaluator selection to the Lucid instance", async () => {
    const builder = new FakeTxBuilder();
    const attestationUtxo = utxo("03", 0, {
      lovelace: 5_000_000n,
      [SDK.daAttestationUnit(contracts.daAttestation, HEADER_HASH)]: 1n,
    });

    await buildAddSignaturesTx({
      lucid: fakeLucid(builder, {
        kupoUrl: "https://kupo.example.com",
        ogmiosUrl: "http://127.0.0.1:1337",
      }),
      contracts,
      daParamsUtxo: utxo("01", 0),
      attestationUtxo,
      attestationDatum: baseAttestationDatum(),
      packedWitnessesHex: `00${"aa".repeat(64)}`,
      signerIndexes: [0],
      referenceScripts,
    });

    expect(builder.completeOptions).toEqual([{ localUPLCEval: true }]);
  });
});

describe("DA attestation apply validity range", () => {
  const HEADER_END_TIME = 1_800_000_000_000n;
  const DEADLINE = HEADER_END_TIME + SDK.DA_ATTESTATION_TIMEOUT_MS;
  const rangeAt = (currentTime: bigint) =>
    daAttestationApplyValidityRange({
      target: applyTarget(HEADER_END_TIME),
      currentTime,
    });

  it("opens before the submitter's clock so a trailing L1 tip still admits it", async () => {
    const currentTime = DEADLINE - SDK.MAX_VALIDITY_RANGE_LENGTH_MS * 2n;
    await expect(rangeAt(currentTime)).resolves.toEqual({
      validFrom: currentTime - SDK.DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS,
      validTo:
        currentTime -
        SDK.DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS +
        SDK.MAX_VALIDITY_RANGE_LENGTH_MS,
    });
    expect(SDK.DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS).toBe(60_000n);
  });

  it.each([
    [
      "once the maximum range would close 1 ms past it",
      DEADLINE -
        SDK.MAX_VALIDITY_RANGE_LENGTH_MS +
        SDK.DA_ATTESTATION_APPLY_SLOT_LAG_ALLOWANCE_MS +
        1n,
    ],
    ["one millisecond before the deadline", DEADLINE - 1n],
  ])("caps validTo at the attestation deadline %s", async (_, currentTime) => {
    const range = await rangeAt(currentTime);
    expect(range.validTo).toBe(DEADLINE);
    expect(range.validFrom).toBeLessThan(currentTime);
  });

  it.each([
    ["exactly at the deadline, leaving an empty window", DEADLINE],
    ["after the deadline", DEADLINE + 1n],
  ])(
    "rejects with the SDK's typed past-deadline build error %s",
    async (_, currentTime) => {
      const error = await rangeAt(currentTime).then(
        () => undefined,
        (cause: unknown) => cause,
      );
      expect(error).toBeInstanceOf(SDK.DaAttestationBuildError);
      expect(error).toMatchObject({
        _tag: "DaAttestationBuildError",
        reason: "validity_range_past_deadline",
      });
    },
  );
});

describe("DA attestation init output", () => {
  /** The ledger's `coinsPerUtxoByte` the consensus profile targets. */
  const COINS_PER_UTXO_BYTE = BigInt(MIDGARD_CONSENSUS_LIMITS.coinsPerUtxoByte);
  const attestationAddress = credentialToAddress(
    "Preprod",
    { type: "Script", hash: "aa".repeat(28) },
    { type: "Script", hash: "ab".repeat(28) },
  );
  const initContracts = {
    daAttestation: {
      ...validator("aa".repeat(28), attestationAddress),
    },
  };
  const attestationUnit = SDK.daAttestationUnit(
    initContracts.daAttestation,
    HEADER_HASH,
  );
  const minLovelaceAtCount = (
    datum: SDK.DaAttestationDatum,
    count: bigint,
  ): bigint =>
    calculateMinLovelaceFromUTxO(COINS_PER_UTXO_BYTE, {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: attestationAddress,
      assets: { lovelace: 0n, [attestationUnit]: 1n },
      datum: Data.to(
        { ...datum, attestation_count: count } as never,
        SDK.DaAttestationDatum as never,
      ),
    });

  it("locks the attestation's own min-UTxO at its widest signer count, and no bond", async () => {
    const builder = new InitTxBuilder();
    const datum = baseAttestationDatum();
    const lucid = {
      newTx: () => builder,
      config: () => ({
        network: "Preprod",
        protocolParameters: { coinsPerUtxoByte: Number(COINS_PER_UTXO_BYTE) },
      }),
    } as unknown as LucidEvolution;

    await buildInitDaAttestationTx({
      lucid,
      contracts: initContracts,
      daParamsUtxo: utxo("01", 0),
      daParamsDatum: {
        committee: "11".repeat(32) + "22".repeat(32),
        committee_signers_hash: datum.committee_signers_hash,
        da_threshold: datum.da_threshold,
        owners: ["44".repeat(28), "55".repeat(28)],
        update_threshold: 2n,
      },
      target: {
        headerHash: HEADER_HASH,
        stateQueueUtxo: { utxo: utxo("02", 0) },
      } as unknown as DaAttestationTarget,
      referenceScripts,
      rescueBeneficiary: datum.rescue_beneficiary,
      availabilityCommitment: datum.availability_commitment,
    });

    expect(builder.payments).toHaveLength(1);
    const paid = builder.payments[0]!.assets.lovelace!;
    expect(paid).toBe(
      daAttestationInitOutputLovelace({
        attestationAddress,
        attestationUnit,
        attestationDatum: datum,
        coinsPerUtxoByte: COINS_PER_UTXO_BYTE,
      }),
    );
    // Add-signatures carries the value unchanged while the count grows, so
    // every count the committee can reach must stay at or above its minimum.
    // 256 is the committee's cap and the first count whose CBOR integer takes
    // three bytes; a literal, so the widest count cannot shrink unnoticed.
    for (const count of [0n, 23n, 24n, 255n, 256n]) {
      expect(paid, `count ${count.toString()}`).toBeGreaterThanOrEqual(
        minLovelaceAtCount(datum, count),
      );
    }
    expect(paid).toBe(minLovelaceAtCount(datum, DA_ATTESTATION_WIDEST_COUNT));
    expect(
      minLovelaceAtCount(datum, DA_ATTESTATION_WIDEST_COUNT),
    ).toBeGreaterThan(minLovelaceAtCount(datum, 0n));
    // The output is min-ADA only, far below any DA bond.
    expect(paid).toBeLessThan(
      SDK.DA_AVAILABILITY_PROFILE_BOND_AMOUNTS.daBondLovelace,
    );
    expect(builder.completeOptions).toEqual([{ localUPLCEval: true }]);
  });

  it("refuses to size the output without live protocol parameters", async () => {
    const datum = baseAttestationDatum();
    await expect(
      buildInitDaAttestationTx({
        lucid: {
          newTx: () => new InitTxBuilder(),
          config: () => ({ network: "Preprod" }),
        } as unknown as LucidEvolution,
        contracts: initContracts,
        daParamsUtxo: utxo("01", 0),
        daParamsDatum: {} as SDK.DaParamsDatum,
        target: { headerHash: HEADER_HASH } as unknown as DaAttestationTarget,
        referenceScripts,
        rescueBeneficiary: datum.rescue_beneficiary,
        availabilityCommitment: datum.availability_commitment,
      }),
    ).rejects.toThrow(/live protocol parameters/u);
  });

  it("fits the widest canonical attestation within the submitter's attestation allowance", async () => {
    // The worst-case commitment the SDK golden vectors pin: a 64 MiB payload
    // in 64 one-MiB tranches of 16-byte chunks.
    const golden = JSON.parse(
      await readFile(
        new URL(
          "../../midgard-sdk/tests/fixtures/da-commitment-v1.generated.json",
          import.meta.url,
        ),
        "utf8",
      ),
    ) as {
      readonly vectors: readonly {
        readonly label: string;
        readonly commitmentCborHex: string;
      }[];
    };
    const widest = golden.vectors.find(
      (vector) => vector.label === "tranches_64",
    );
    if (widest === undefined) {
      throw new Error("golden vector tranches_64 is missing");
    }
    const beneficiary = await Effect.runPromise(
      SDK.addressDataFromBech32(
        credentialToAddress(
          "Preprod",
          { type: "Key", hash: "56".repeat(28) },
          { type: "Key", hash: "57".repeat(28) },
        ),
      ),
    );
    const commitment = SDK.parseDaAvailabilityCommitmentCbor(
      widest.commitmentCborHex,
    );
    const widestLovelace = daAttestationInitOutputLovelace({
      attestationAddress,
      attestationUnit: SDK.daAttestationUnit(
        initContracts.daAttestation,
        commitment.header_hash,
      ),
      attestationDatum: {
        ...baseAttestationDatum(),
        header_hash: commitment.header_hash,
        availability_commitment: commitment,
        rescue_beneficiary: beneficiary,
        attested_signers: "ff".repeat(32),
      },
      coinsPerUtxoByte: COINS_PER_UTXO_BYTE,
    });

    expect(widestLovelace).toBeLessThanOrEqual(
      DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE,
    );
    expect(DA_L1_SUBMITTER_MIN_PLAIN_ADA_LOVELACE).toBe(
      DA_L1_SUBMITTER_FEE_HEADROOM_LOVELACE +
        DA_ATTESTATION_OUTPUT_LOVELACE_ALLOWANCE,
    );
  });
});

class InitTxBuilder {
  readonly payments: {
    readonly address: string;
    readonly assets: UTxO["assets"];
  }[] = [];
  readonly completeOptions: unknown[] = [];
  readonly pay = {
    ToContract: (
      address: string,
      _datum: unknown,
      assets: UTxO["assets"],
    ): InitTxBuilder => {
      this.payments.push({ address, assets });
      return this;
    },
  };

  readFrom(): InitTxBuilder {
    return this;
  }

  mintAssets(): InitTxBuilder {
    return this;
  }

  async complete(options: unknown): Promise<TxSignBuilder> {
    this.completeOptions.push(options);
    return {} as TxSignBuilder;
  }
}

const applyTarget = (headerEndTime: bigint): DaAttestationTarget =>
  ({
    headerHash: HEADER_HASH,
    stateQueueNode: { header: { endTime: headerEndTime } },
  }) as unknown as DaAttestationTarget;

const HEADER_HASH = "01".repeat(28);

const contracts: DaAttestationValidatorSet = {
  hubOracle: validator("99".repeat(28), "addr_test1huboracle"),
  availabilityChallenge: availabilityChallengeValidator(),
  daAttestation: validator("aa".repeat(28), "addr_test1daattestation"),
  daBondPool: validator("ab".repeat(28), "addr_test1dabondpool"),
  daParamsGovernor: validator("bb".repeat(28), "addr_test1daparams"),
  stateQueue: stateQueueValidator("cc".repeat(28), "addr_test1statequeue"),
};

const referenceScripts: DaAttestationReferenceScripts = {
  daAttestationMinting: utxo("04", 0),
  daAttestationSpending: utxo("05", 0),
  stateQueueMinting: utxo("06", 0),
  stateQueueSpending: utxo("07", 0),
};

const baseAttestationDatum = (): SDK.DaAttestationDatum => ({
  header_hash: HEADER_HASH,
  availability_commitment: SDK.buildDaAvailabilityCommitment({
    deploymentIdentity: "99".repeat(28),
    headerHash: HEADER_HASH,
    payload: Buffer.from("public retained DA"),
    responseGeometry: SDK.availabilityResponseGeometry({
      chunkByteLength: 14_020,
      trancheByteLength: 4 * 1_024 * 1_024,
      maxTrancheCount: 16,
    }),
  }),
  da_threshold: 2n,
  committee_signers_hash: "02".repeat(32),
  rescue_beneficiary: {
    paymentCredential: { PublicKeyCredential: ["56".repeat(28)] },
    stakeCredential: null,
  },
  attested_signers: "00".repeat(32),
  attestation_count: 0n,
});

const fakeLucid = (
  builder: FakeTxBuilder,
  provider?: {
    readonly kupoUrl: string;
    readonly ogmiosUrl: string;
  },
): LucidEvolution =>
  ({
    newTx: () => builder,
    config: () => ({ provider }),
  }) as unknown as LucidEvolution;

class FakeTxBuilder {
  readonly reads: UTxO[][] = [];
  readonly collects: { readonly utxos: UTxO[]; readonly redeemer: unknown }[] =
    [];
  readonly payments: {
    readonly address: string;
    readonly datum: { readonly kind: "inline"; readonly value: string };
    readonly assets: UTxO["assets"];
  }[] = [];
  readonly completeOptions: unknown[] = [];
  readonly signerKeys: string[] = [];
  private readonly failFirstComplete: boolean;

  readonly pay = {
    ToContract: (
      address: string,
      datum: { readonly kind: "inline"; readonly value: string },
      assets: UTxO["assets"],
    ): FakeTxBuilder => {
      this.payments.push({ address, datum, assets });
      return this;
    },
  };

  constructor({
    failFirstComplete = false,
  }: {
    readonly failFirstComplete?: boolean;
  } = {}) {
    this.failFirstComplete = failFirstComplete;
  }

  readFrom(utxos: UTxO[]): FakeTxBuilder {
    this.reads.push(utxos);
    return this;
  }

  collectFrom(utxos: UTxO[], redeemer: unknown): FakeTxBuilder {
    this.collects.push({ utxos, redeemer });
    return this;
  }

  mintAssets(): FakeTxBuilder {
    return this;
  }

  addSignerKey(keyHash: string): FakeTxBuilder {
    this.signerKeys.push(keyHash);
    return this;
  }

  async complete(options: unknown): Promise<TxSignBuilder> {
    this.completeOptions.push(options);
    if (this.failFirstComplete && this.completeOptions.length === 1) {
      throw new Error("exunits over budget");
    }
    return {} as TxSignBuilder;
  }
}
