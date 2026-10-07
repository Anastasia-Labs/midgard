import { CborMap, encodeCbor } from "@al-ft/l1-node-transport";
import {
  CML,
  credentialToAddress,
  scriptFromNative,
  SLOT_CONFIG_NETWORK,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { blake2b256 } from "../../src/codec.js";
import { decodeLedgerUtxos } from "../../src/decode/utxo.js";
import { witnessDatum } from "../../src/decode/witness.js";
import {
  decodeEraHistory,
  decodeProtocolParameters,
  decodeSystemStart,
  LedgerAnswerError,
  slotConfigFrom,
  toLucidUtxo,
} from "../../src/provider/index.js";
import {
  built,
  cborHead,
  DEVNET_LEDGER,
  ledgerEntry,
  lucidOracle,
  witnessWithDatums,
} from "../support/provider.js";

const fill = (byte: number, length = 32): Buffer => Buffer.alloc(length, byte);
const KEY = "aa".repeat(28);
const SCRIPT = "bb".repeat(28);
const POLICY = "cc".repeat(28);
/** An always-succeeding Plutus script, as Lucid's single-CBOR text. */
const PLUTUS = "4e4d01000033222220051200120011";
/** A Plutus datum in a non-canonical (indefinite) encoding: hashes must use these exact bytes. */
const DATUM = "d8799f4401020304ff";

const keyAddress = credentialToAddress("Custom", { type: "Key", hash: KEY });
const baseAddress = credentialToAddress(
  "Custom",
  { type: "Script", hash: SCRIPT },
  { type: "Key", hash: KEY },
);

const CASES = {
  "plain lovelace at a key address": {
    address: keyAddress,
    assets: { lovelace: 2_000_000n },
  },
  "multi-asset, datum hash, base address": {
    address: baseAddress,
    assets: { lovelace: 3_000_000n, [`${POLICY}`]: 1n, [`${POLICY}0a0b`]: 7n },
    datumHash: "dd".repeat(32),
  },
  "inline datum and a Plutus V3 reference script": {
    address: baseAddress,
    assets: { lovelace: 20_000_000n },
    inlineDatum: DATUM,
    scriptRef: { type: "PlutusV3", script: PLUTUS },
  },
  "Plutus V2 reference script": {
    address: keyAddress,
    assets: { lovelace: 9_000_000n },
    scriptRef: { type: "PlutusV2", script: PLUTUS },
  },
  "native reference script": {
    address: keyAddress,
    assets: { lovelace: 4_000_000n },
    scriptRef: scriptFromNative({ type: "sig", keyHash: KEY }),
  },
} as const;

describe("UTxO mapping", () => {
  it.each(Object.entries(CASES))(
    "maps %s exactly as Lucid reads the same bytes",
    (_name, spec) => {
      const output = built({ txHash: fill(0x51), index: 3 }, spec);
      const mapped = toLucidUtxo(output.outRef, output.summary);
      expect(mapped).toEqual(lucidOracle(output));
      if (mapped.scriptRef !== undefined && mapped.scriptRef !== null)
        expect(validatorToScriptHash(mapped.scriptRef)).toBe(
          output.summary.scriptRef?.hash.toString("hex"),
        );
    },
  );

  it("decodes a ledger UTxO answer into the same outputs", () => {
    const outputs = Object.values(CASES).map((spec, index) =>
      built({ txHash: fill(0x60 + index), index }, spec),
    );
    const answer = Buffer.concat([
      cborHead(5, outputs.length),
      ...outputs.flatMap((output) => {
        const entry = ledgerEntry(output);
        return [
          cborHead(4, 2),
          Buffer.from([0x58, 32]),
          Buffer.from(entry.txHash, "hex"),
          cborHead(0, entry.index),
          output.bytes,
        ];
      }),
    ]);
    expect(
      decodeLedgerUtxos(answer).map((entry) =>
        toLucidUtxo(entry.outRef, entry.output),
      ),
    ).toEqual(outputs.map(lucidOracle));
    // Each output's bytes exactly as answered, not a re-encoding.
    expect(
      decodeLedgerUtxos(answer).map((entry) =>
        entry.outputCbor.toString("hex"),
      ),
    ).toEqual(outputs.map((output) => output.bytes.toString("hex")));
  });
});

describe("witness datums", () => {
  const datum = Buffer.from(DATUM, "hex");
  const other = Buffer.from("d87980", "hex");
  it.each([false, true])(
    "finds a datum by the hash of its exact bytes (tag-258 set: %s)",
    (asSet) => {
      const witness = witnessWithDatums([other, datum], asSet);
      expect(witnessDatum(witness, blake2b256(datum))).toEqual(datum);
      expect(witnessDatum(witness, fill(0x99))).toBeNull();
    },
  );

  it("does not match a re-encoding of the same value", () => {
    const canonical = Buffer.from(
      CML.PlutusData.from_cbor_hex(DATUM).to_canonical_cbor_bytes(),
    );
    expect(canonical.equals(Buffer.from(DATUM, "hex"))).toBe(false);
    expect(
      witnessDatum(
        witnessWithDatums([Buffer.from(DATUM, "hex")]),
        blake2b256(canonical),
      ),
    ).toBeNull();
  });
});

describe("protocol parameters from local state query", () => {
  it("decode the devnet answer to the values cardano-cli reports", () => {
    const cli = DEVNET_LEDGER.cliProtocolParameters as unknown as Record<
      | "txFeePerByte"
      | "txFeeFixed"
      | "maxTxSize"
      | "maxValueSize"
      | "stakeAddressDeposit"
      | "stakePoolDeposit"
      | "dRepDeposit"
      | "govActionDeposit"
      | "utxoCostPerByte"
      | "collateralPercentage"
      | "maxCollateralInputs"
      | "minFeeRefScriptCostPerByte",
      number
    > &
      Record<
        "executionUnitPrices" | "maxTxExecutionUnits" | "protocolVersion",
        Record<string, number>
      >;
    const prices = cli.executionUnitPrices;
    const maxTx = cli.maxTxExecutionUnits;
    const version = cli.protocolVersion;
    expect(
      decodeProtocolParameters(
        Buffer.from(DEVNET_LEDGER.lsq.protocol_params, "hex"),
      ),
    ).toEqual({
      minFeeA: cli.txFeePerByte,
      minFeeB: cli.txFeeFixed,
      maxTxSize: cli.maxTxSize,
      maxValSize: cli.maxValueSize,
      keyDeposit: BigInt(cli.stakeAddressDeposit),
      poolDeposit: BigInt(cli.stakePoolDeposit),
      drepDeposit: BigInt(cli.dRepDeposit),
      govActionDeposit: BigInt(cli.govActionDeposit),
      priceMem: prices.priceMemory,
      priceStep: prices.priceSteps,
      maxTxExMem: BigInt(maxTx.memory!),
      maxTxExSteps: BigInt(maxTx.steps!),
      coinsPerUtxoByte: BigInt(cli.utxoCostPerByte),
      collateralPercentage: cli.collateralPercentage,
      maxCollateralInputs: cli.maxCollateralInputs,
      minFeeRefScriptCostPerByte: cli.minFeeRefScriptCostPerByte,
      costModels: DEVNET_LEDGER.cliProtocolParameters.costModels,
      protocolMajorVersion: version.major,
      protocolMinorVersion: version.minor,
    });
  });

  it("refuses an answer without a cost model rather than defaulting it", () => {
    const fields = [...Array(31).keys()].map(() => 0) as unknown[];
    fields[12] = [11, 0];
    fields[16] = [
      [1, 2],
      [3, 4],
    ];
    fields[17] = [1, 2];
    fields[30] = [15, 1];
    fields[15] = new CborMap([]);
    expect(() => decodeProtocolParameters(encodeCbor(fields as never))).toThrow(
      LedgerAnswerError,
    );
  });
});

describe("slot configuration from the era history", () => {
  it("matches the devnet's genesis: every era has one-second slots from slot 0", () => {
    const config = slotConfigFrom(
      decodeSystemStart(Buffer.from(DEVNET_LEDGER.lsq.system_start, "hex")),
      decodeEraHistory(Buffer.from(DEVNET_LEDGER.lsq.era_history, "hex")),
    );
    expect(config).toEqual({
      zeroTime: Date.parse("2026-10-07T17:14:48Z"),
      zeroSlot: 0,
      slotLength: 1000,
    });
  });

  it("starts at the first era of the current slot length (Preprod's Byron prefix)", () => {
    // Preprod: system start 2022-06-01, four 21,600-slot Byron epochs of 20 s,
    // then one-second slots from slot 86,400.
    const systemStart = encodeCbor([2022, 152, 0]);
    const byronEnd = [1_728_000n * 10n ** 12n, 86_400, 4];
    const history = encodeCbor([
      [[0, 0, 0], byronEnd, [21_600, 20_000, [0, 4320, [0]], 4320]],
      [byronEnd, null, [432_000, 1000, [0, 129_600, [0]], 129_600]],
    ] as never);
    expect(
      slotConfigFrom(decodeSystemStart(systemStart), decodeEraHistory(history)),
    ).toEqual(SLOT_CONFIG_NETWORK.Preprod);
  });
});
