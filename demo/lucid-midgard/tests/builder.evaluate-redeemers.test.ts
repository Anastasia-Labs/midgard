import { encodeMidgardCekProgramMaterialSidecar } from "@al-ft/midgard-core/cek-proof";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardRedeemerWitnessFieldPreimage,
  hashMidgardVersionedScript,
  MIDGARD_REDEEMER_PURPOSE_TAGS,
  MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
} from "@al-ft/midgard-core/codec";
import {
  MIDGARD_CONSENSUS_LIMITS,
  MIDGARD_CONSENSUS_PROFILE,
} from "@al-ft/midgard-core/consensus-profile";
import { NAMED_NETWORK_L1_SLOT_CONFIGS } from "@al-ft/midgard-core/l1-slot-config";
import { evaluateMidgardTxRedeemers } from "@al-ft/midgard-validation";
import { buildMidgardCanonicalCekProgram } from "@al-ft/midgard-validation/cek-program";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  BuilderInvariantError,
  decodeMidgardUtxo,
  encodeMidgardTxOutput,
  LucidMidgard,
  type MidgardProvider,
  type MidgardRedeemerBudget,
  type OutRef,
  outRefToCbor,
  ProviderCapabilityError,
} from "../src/index.js";

// Redeemers without exUnits are given the budgets the provider evaluates for
// a draft of the same transaction.

const pubkeyAddress =
  "addr_test1qq4jrrcfzylccwgqu3su865es52jkf7yzrdu9cw3z84nycnn3zz9lvqj7vs95tej896xkekzkufhpuk64ja7pga2g8ksdf8km4";
// (program 1.1.0 (lam context (con unit ()))): accepts any context.
const rawPlutusV3Script = Buffer.from("0101002499", "hex");
const plutusV3Hash = hashMidgardVersionedScript({
  language: "PlutusV3",
  scriptBytes: Buffer.from(
    buildMidgardCanonicalCekProgram(rawPlutusV3Script).envelopeCbor,
  ),
});

const makeProvider = (
  evaluateTx?: MidgardProvider["evaluateTx"],
): MidgardProvider => ({
  getUtxos: async () => [],
  getUtxoByOutRef: async () => undefined,
  getProtocolInfo: async () => ({
    apiVersion: 1,
    network: "Preview",
    midgardNativeTxVersion: 1,
    currentSlot: 0n,
    consensusProfile: MIDGARD_CONSENSUS_PROFILE,
    supportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    codecSupportedScriptLanguages: MIDGARD_SUPPORTED_SCRIPT_LANGUAGES,
    protocolFeeParameters: { minFeeA: 0n, minFeeB: 0n },
    submissionLimits: {
      maxSubmitTxCborBytes:
        MIDGARD_CONSENSUS_PROFILE.limits.maxTxCanonicalCborBytes,
    },
    validation: {
      strictnessProfile: "production",
      localValidationIsAuthoritative: false,
    },
  }),
  getProtocolParameters: async () => ({
    minFeeA: 0n,
    minFeeB: 0n,
    networkId: 0n,
  }),
  getCurrentSlot: async () => 0n,
  submitTx: async () => ({
    txId: "00".repeat(32),
    status: "queued",
    httpStatus: 202,
    duplicate: false,
  }),
  getTxStatus: async (txId) => ({ kind: "queued", txId }),
  diagnostics: () => ({
    endpoint: "memory://canonical-v1",
    protocolInfoSource: "node",
  }),
  ...(evaluateTx === undefined ? {} : { evaluateTx }),
});

const spendAddress = CML.EnterpriseAddress.new(
  0,
  CML.Credential.new_script(CML.ScriptHash.from_hex(plutusV3Hash)),
)
  .to_address()
  .to_bech32();

const scriptUtxo = (byte: number, lovelace: bigint) => {
  const outRef: OutRef = {
    txHash: byte.toString(16).padStart(2, "0").repeat(32),
    outputIndex: 0,
  };
  return decodeMidgardUtxo({
    outRef,
    outRefCbor: outRefToCbor(outRef),
    outputCbor: encodeMidgardTxOutput(spendAddress, { lovelace }),
  });
};

/** `tag:index` → declared units, as the transaction commits them. */
const declaredUnits = (txCbor: Uint8Array): Record<string, string> =>
  Object.fromEntries(
    decodeMidgardRedeemerWitnessFieldPreimage(
      decodeMidgardNativeTxFullFromCanonicalCbor(txCbor).witnessSet
        .redeemerTxWitsPreimageCbor,
    ).map((witness) => [
      `${String(MIDGARD_REDEEMER_PURPOSE_TAGS[witness.purpose])}:${witness.index.toString()}`,
      `${witness.executionUnits.memory.toString()}/${witness.executionUnits.steps.toString()}`,
    ]),
  );

const redeemer = Buffer.from([0x80]);

// The 0x22 input is given exUnits and the 0x11 input is not; inputs sort by
// out-ref, so 0x11 is spend index 0.
const build = async (provider: MidgardProvider) =>
  (await LucidMidgard.new(provider, { network: "Preview", networkId: 0 }))
    .newTx()
    .attach.Script({
      kind: "plutus-v3",
      language: "PlutusV3",
      script: rawPlutusV3Script,
    })
    .collectFrom([scriptUtxo(0x22, 2_000_000n)], {
      data: redeemer,
      exUnits: { mem: 7n, steps: 9n },
    })
    .collectFrom([scriptUtxo(0x11, 1_000_000n)], { data: redeemer })
    .pay.ToAddress(pubkeyAddress, { lovelace: 3_000_000n });

describe("redeemer evaluation", () => {
  it("fills only the missing exUnits from the evaluated budgets", async () => {
    const drafts: Record<string, string>[] = [];
    const completed = await (
      await build(
        makeProvider(async (txHex) => {
          drafts.push(declaredUnits(Buffer.from(txHex, "hex")));
          return [
            { tag: 0, index: 0n, memory: 1200n, steps: 34000n },
            { tag: 0, index: 1n, memory: 5n, steps: 5n },
          ] satisfies MidgardRedeemerBudget[];
        }),
      )
    ).complete({ fee: 0n });

    expect(drafts).toEqual([
      { "0:0": "0/0", "0:1": "7/9" },
      { "0:0": "1200/34000", "0:1": "7/9" },
    ]);
    expect(declaredUnits(completed.txCbor)).toEqual({
      "0:0": "1200/34000",
      "0:1": "7/9",
    });
  });

  it("re-drafts until the evaluated budgets fit the declared ones", async () => {
    let calls = 0;
    const completed = await (
      await build(
        makeProvider(async () => {
          calls += 1;
          const memory = calls === 1 ? 1000n : 1100n;
          return [{ tag: 0, index: 0n, memory, steps: 1n }];
        }),
      )
    ).complete({ fee: 0n });
    expect(calls).toBe(3);
    expect(declaredUnits(completed.txCbor)["0:0"]).toBe("1100/1");
  });

  it("gives up when the budgets never settle", async () => {
    let memory = 0n;
    const builder = await build(
      makeProvider(async () => {
        memory += 10n;
        return [{ tag: 0, index: 0n, memory, steps: 1n }];
      }),
    );
    await expect(builder.complete({ fee: 0n })).rejects.toBeInstanceOf(
      BuilderInvariantError,
    );
  });

  it("rejects a budget for a redeemer the draft does not have", async () => {
    const builder = await build(
      makeProvider(async () => [{ tag: 1, index: 0n, memory: 1n, steps: 1n }]),
    );
    await expect(builder.complete({ fee: 0n })).rejects.toThrow(
      "Evaluated redeemer does not match the draft",
    );
  });

  it("declares budgets the node's Phase B accepts", async () => {
    const inputs = [scriptUtxo(0x22, 2_000_000n), scriptUtxo(0x11, 1_000_000n)];
    const preState = new Map(
      inputs.map((utxo) => [
        Buffer.from(utxo.cbor!.outRef!).toString("hex"),
        Buffer.from(utxo.cbor!.output!),
      ]),
    );
    const l1SlotConfig = NAMED_NETWORK_L1_SLOT_CONFIGS.Preview;
    const provider = makeProvider(async (txHex, programMaterial = []) => {
      const evaluation = await Effect.runPromise(
        evaluateMidgardTxRedeemers({
          txCbor: Buffer.from(txHex, "hex"),
          programMaterialSidecarCbor:
            encodeMidgardCekProgramMaterialSidecar(programMaterial),
          readState: () => Effect.succeed(preState),
          expectedNetworkId: 0n,
          limits: MIDGARD_CONSENSUS_LIMITS,
          config: { l1SlotConfig },
        }),
      );
      if (evaluation.kind !== "evaluated") throw new Error(evaluation.detail);
      return evaluation.redeemers;
    });
    const completed = await (
      await LucidMidgard.new(provider, { network: "Preview", networkId: 0 })
    )
      .newTx()
      .attach.Script({
        kind: "plutus-v3",
        language: "PlutusV3",
        script: rawPlutusV3Script,
      })
      .collectFrom(inputs, { data: redeemer })
      .pay.ToAddress(pubkeyAddress, { lovelace: 3_000_000n })
      .complete({
        fee: 0n,
        localValidation: "phase-b",
        localPreState: preState,
        l1SlotConfig,
        enforceScriptBudget: true,
      });
    expect(completed.metadata.localValidation).toMatchObject({
      acceptedTxIds: [completed.txIdHex],
      rejected: [],
    });
    expect(
      Object.values(declaredUnits(completed.txCbor)).every(
        (units) => units !== "0/0",
      ),
    ).toBe(true);
  });

  it("needs a provider that evaluates", async () => {
    const builder = await build(makeProvider());
    await expect(builder.complete({ fee: 0n })).rejects.toBeInstanceOf(
      ProviderCapabilityError,
    );
  });
});
