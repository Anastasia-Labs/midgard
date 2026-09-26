import * as SDK from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  addSignaturesToDaAttestationDatum,
  buildAddSignaturesTx,
  daAttestationApplyValidityRange,
  type DaAttestationTarget,
} from "../src/coordinator/tx-builders.js";
import type { DaAttestationValidatorSet } from "../src/l1/deployment.js";
import type { DaAttestationReferenceScripts } from "../src/l1/reference-scripts.js";

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

const applyTarget = (headerEndTime: bigint): DaAttestationTarget =>
  ({
    headerHash: HEADER_HASH,
    stateQueueNode: { header: { endTime: headerEndTime } },
  }) as unknown as DaAttestationTarget;

const HEADER_HASH = "01".repeat(28);

const availabilityChallengeValidator =
  (): DaAttestationValidatorSet["availabilityChallenge"] => ({
    ...validator("ee".repeat(28), "addr_test1availability"),
    yields: Object.fromEntries(
      ["bond", "open", "settle", "close", "timeout"].map((arm) => [
        arm,
        {
          withdrawalScriptCBOR: "49480100002221200101",
          withdrawalScript: {
            type: "PlutusV3",
            script: "49480100002221200101",
          },
          withdrawalScriptHash: "f1".repeat(28),
        },
      ]),
    ) as SDK.AvailabilityChallengeYieldValidators,
  });

const contracts: DaAttestationValidatorSet = {
  hubOracle: validator("99".repeat(28), "addr_test1huboracle"),
  availabilityChallenge: availabilityChallengeValidator(),
  daAttestation: validator("aa".repeat(28), "addr_test1daattestation"),
  daParamsGovernor: validator("bb".repeat(28), "addr_test1daparams"),
  stateQueue: stateQueueValidator("cc".repeat(28), "addr_test1statequeue"),
};

const referenceScripts: DaAttestationReferenceScripts = {
  availabilityChallengeMinting: utxo("08", 0),
  availabilityChallengeBondWithdrawal: utxo("09", 0),
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
    bondOwner: "76".repeat(28),
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

function stateQueueValidator(
  policyId: string,
  spendingScriptAddress: string,
): DaAttestationValidatorSet["stateQueue"] {
  const yieldValidator = (
    role: string,
  ): DaAttestationValidatorSet["stateQueue"]["yields"]["commit"] => ({
    withdrawalScriptCBOR: "",
    withdrawalScript: { type: "PlutusV3", script: "00" } as never,
    withdrawalScriptHash: role,
  });
  return {
    ...validator(policyId, spendingScriptAddress),
    yields: {
      commit: yieldValidator("c1".repeat(28)),
      unattestedTimeout: yieldValidator("c2".repeat(28)),
      unavailableTimeout: yieldValidator("c3".repeat(28)),
      fraudRemoval: yieldValidator("c4".repeat(28)),
      merge: yieldValidator("c5".repeat(28)),
    },
  };
}

function validator(
  policyId: string,
  spendingScriptAddress: string,
): DaAttestationValidatorSet["daAttestation"] {
  return {
    mintingScriptCBOR: "",
    mintingScript: { type: "PlutusV3", script: "00" } as never,
    policyId,
    spendingScriptCBOR: "",
    spendingScript: { type: "PlutusV3", script: "00" } as never,
    spendingScriptHash: policyId,
    spendingScriptAddress,
  };
}

function utxo(
  byte: string,
  outputIndex: number,
  assets: UTxO["assets"] = { lovelace: 5_000_000n },
  extra: Partial<UTxO> = {},
): UTxO {
  return {
    txHash: byte.repeat(32),
    outputIndex,
    address: "addr_test1fixture",
    assets,
    ...extra,
  } as UTxO;
}

const outRef = (entry: Pick<UTxO, "txHash" | "outputIndex">): string =>
  `${entry.txHash}#${entry.outputIndex.toString()}`;

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
