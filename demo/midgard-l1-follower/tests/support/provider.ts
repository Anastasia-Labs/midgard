import { readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

import { L1NodeTransport } from "@al-ft/l1-node-transport";
import { writeFakeSidecar } from "@al-ft/l1-node-transport/testing/fake-sidecar";
import {
  type Assets,
  assetsToValue,
  CML,
  coreToUtxo,
  type Script,
  toScriptRef,
  type UTxO,
} from "@lucid-evolution/lucid";

import { readOutput } from "../../src/decode/output.js";
import {
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  type OutputSummary,
  type OutRef,
  type TrackedSet,
} from "../../src/index.js";
import type { testDatabases } from "./postgres.js";

const fixtureDirectory = fileURLToPath(
  new URL("../fixtures/", import.meta.url),
);

/** LSQ answers and the cardano-cli view of the same devnet node. */
export const DEVNET_LEDGER = JSON.parse(
  readFileSync(join(fixtureDirectory, "provider/devnet-ledger.json"), "utf8"),
) as {
  lsq: { system_start: string; era_history: string; protocol_params: string };
  cliProtocolParameters: Record<string, unknown> & {
    costModels: Record<"PlutusV1" | "PlutusV2" | "PlutusV3", number[]>;
  };
};

export const HANDLER_MODULE = join(
  fixtureDirectory,
  "provider-ledger-handler.mjs",
);

export type OutputSpec = Readonly<{
  address: string;
  assets: Assets;
  datumHash?: string;
  inlineDatum?: string;
  scriptRef?: Script;
}>;

/** The exact CBOR of an output CML builds from `spec`. */
export const outputCbor = (spec: OutputSpec): Buffer => {
  const datum =
    spec.datumHash !== undefined
      ? CML.DatumOption.new_hash(CML.DatumHash.from_hex(spec.datumHash))
      : spec.inlineDatum !== undefined
        ? CML.DatumOption.new_datum(
            CML.PlutusData.from_cbor_hex(spec.inlineDatum),
          )
        : undefined;
  const output = CML.TransactionOutput.new(
    CML.Address.from_bech32(spec.address),
    assetsToValue(spec.assets),
    datum,
    spec.scriptRef === undefined ? undefined : toScriptRef(spec.scriptRef),
  );
  return Buffer.from(output.to_cbor_bytes());
};

export type BuiltOutput = Readonly<{
  outRef: OutRef;
  bytes: Buffer;
  summary: OutputSummary;
}>;

export const built = (outRef: OutRef, spec: OutputSpec): BuiltOutput => {
  const bytes = outputCbor(spec);
  return { outRef, bytes, summary: readOutput(bytes, 0) };
};

/** Lucid's own reading of the same output bytes: the mapping oracle. */
export const lucidOracle = (output: BuiltOutput): UTxO =>
  coreToUtxo(
    CML.TransactionUnspentOutput.new(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(output.outRef.txHash.toString("hex")),
        BigInt(output.outRef.index),
      ),
      CML.TransactionOutput.from_cbor_bytes(output.bytes),
    ),
  );

/** A CBOR head (major type, small length). */
export const cborHead = (major: number, length: number): Buffer =>
  length < 24
    ? Buffer.from([(major << 5) | length])
    : Buffer.from([(major << 5) | 24, length]);

/** A witness set `{4: plutus_data}` holding `datums` (raw CBOR), optionally as a tag-258 set. */
export const witnessWithDatums = (
  datums: readonly Buffer[],
  asSet = false,
): Buffer =>
  Buffer.concat([
    cborHead(5, 1),
    cborHead(0, 4),
    asSet ? Buffer.from([0xd9, 0x01, 0x02]) : Buffer.alloc(0),
    cborHead(4, datums.length),
    ...datums,
  ]);

export type ProviderAdapter = Readonly<{
  name: "sqlite" | "postgres";
  open: (tracked: TrackedSet) => Promise<FactStore>;
}>;

export const providerAdapters = (
  databases: ReturnType<typeof testDatabases>,
  scratch: string,
): readonly ProviderAdapter[] => [
  {
    name: "sqlite",
    open: async (tracked) =>
      openSqliteFactStore({
        securityParameter: 4,
        trackedSet: tracked,
        path: join(scratch, `${String(Math.random()).slice(2)}.db`),
      }),
  },
  {
    name: "postgres",
    open: async (tracked) =>
      openPostgresFactStore({
        securityParameter: 4,
        trackedSet: tracked,
        connection: { connectionString: (await databases.create()).url },
      }),
  },
];

export type LedgerOptions = Readonly<{
  answers?: Record<string, string>;
  utxos?: readonly Readonly<{
    txHash: string;
    index: number;
    address: string;
    output: string;
  }>[];
  mempool?: readonly string[];
  submit?: "accept" | Readonly<{ rejection: string }>;
  refuse?: Record<string, string>;
}>;

/** A real transport over a fake sidecar serving `options`. */
export const fakeLedgerTransport = async (
  directory: string,
  options: LedgerOptions,
): Promise<L1NodeTransport> => {
  const binaryPath = await writeFakeSidecar({
    path: join(directory, `fake-sidecar-${String(Math.random()).slice(2)}`),
    handlerModule: HANDLER_MODULE,
    options,
  });
  const transport = new L1NodeTransport({
    binaryPath,
    socketPath: join(directory, "node.socket"),
    networkMagic: 424242,
    requestTimeoutMs: 10_000,
  });
  await transport.whenReady(10_000);
  return transport;
};

/** A ledger UTxO entry of the fake handler for a built output. */
export const ledgerEntry = (output: BuiltOutput) => ({
  txHash: output.outRef.txHash.toString("hex"),
  index: output.outRef.index,
  address: output.summary.address.toString("hex"),
  output: output.bytes.toString("hex"),
});
