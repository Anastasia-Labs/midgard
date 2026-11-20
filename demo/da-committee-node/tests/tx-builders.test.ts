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

const HEADER_HASH = "01".repeat(28);

const contracts: DaAttestationValidatorSet = {
  daAttestation: validator("aa".repeat(28), "addr_test1daattestation"),
  daParamsGovernor: validator("bb".repeat(28), "addr_test1daparams"),
  stateQueue: validator("cc".repeat(28), "addr_test1statequeue"),
};

const referenceScripts: DaAttestationReferenceScripts = {
  daAttestationMinting: utxo("04", 0),
  daAttestationSpending: utxo("05", 0),
  stateQueueMinting: utxo("06", 0),
  stateQueueSpending: utxo("07", 0),
};

const baseAttestationDatum = (): SDK.DaAttestationDatum => ({
  header_hash: HEADER_HASH,
  da_threshold: 2n,
  committee_signers_hash: "02".repeat(32),
  attested_signers: "00".repeat(32),
  attestation_count: 0n,
});

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
