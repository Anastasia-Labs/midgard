import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { expect } from "vitest";

type BlueprintConstructor = {
  readonly title: string;
  readonly index: number;
  readonly fields?: readonly {
    readonly title?: string;
    readonly $ref?: string;
  }[];
};

type BlueprintDefinition = {
  readonly anyOf?: readonly BlueprintConstructor[];
};

type Blueprint = {
  readonly definitions: Record<string, BlueprintDefinition>;
};

type GoldenAbiFixture = {
  readonly schema: string;
  readonly cborHex: string;
  readonly byteLength: number;
  readonly sha256: string;
};

export type GoldenAbiFixtureFile = {
  readonly version: number;
  readonly encoding: "lucid-plutus-data-cbor-hex";
  readonly fixtures: Record<string, GoldenAbiFixture>;
};

const testDir = path.dirname(fileURLToPath(import.meta.url));

export const repoRoot = path.resolve(testDir, "../../..");

const transitionTraceAbiGoldenPath = path.join(
  testDir,
  "fixtures/transition-trace-abi.json",
);

const blueprintPath =
  process.env.MIDGARD_REAL_BLUEPRINT_PATH ??
  path.join(repoRoot, "onchain/aiken/plutus.json");

const blueprint = JSON.parse(readFileSync(blueprintPath, "utf8")) as Blueprint;

const testnetEnv = readFileSync(
  path.join(repoRoot, "onchain/aiken/env/testnet.ak"),
  "utf8",
);

export const ledgerStateSource = readFileSync(
  path.join(repoRoot, "onchain/aiken/lib/midgard/ledger-state.ak"),
  "utf8",
);

export const transitionTraceAbiGolden = JSON.parse(
  readFileSync(transitionTraceAbiGoldenPath, "utf8"),
) as GoldenAbiFixtureFile;

const definition = (name: string): BlueprintDefinition => {
  const found = blueprint.definitions[name];
  expect(found, `missing blueprint definition ${name}`).toBeDefined();
  return found;
};

export const constructor = (
  definitionName: string,
  constructorName: string,
): BlueprintConstructor => {
  const found = definition(definitionName).anyOf?.find(
    (candidate) => candidate.title === constructorName,
  );
  expect(
    found,
    `missing ${constructorName} constructor in ${definitionName}`,
  ).toBeDefined();
  return found!;
};

export const fields = (ctor: BlueprintConstructor): readonly string[] =>
  (ctor.fields ?? []).map((field) => field.title ?? "");

// The Aiken sources are the normative home of these protocol constants and the
// compiler emits none of them into plutus.json, so a text read of the `pub
// const` declaration is the only channel available. It is deliberately narrow
// and fails closed: a missing declaration fails the lookup, and anything but a
// product of decimal literals fails the shape check rather than being silently
// coerced.
const aikenIntegerConst = (
  source: string,
  sourceLabel: string,
  name: string,
): bigint => {
  const match = source.match(
    new RegExp(`pub const ${name}: [^=]+=([\\d_\\s*]+)`, "m"),
  );
  expect(match, `missing ${sourceLabel} Aiken const ${name}`).toBeDefined();
  const expression = match![1]!.trim().replace(/\s+/g, " ");
  expect(expression, `unsupported expression for ${name}`).toMatch(
    /^[\d_]+(?: \* [\d_]+)*$/,
  );
  return expression
    .split(" * ")
    .map((term) => BigInt(term.replaceAll("_", "")))
    .reduce((acc, term) => acc * term, 1n);
};

export const testnetIntegerConst = (name: string): bigint =>
  aikenIntegerConst(testnetEnv, "testnet", name);

export const h28 = "11".repeat(28);

export const h32 = "22".repeat(32);

export const h64 = "33".repeat(64);

export const outputReference: SDK.OutputReference = {
  transactionId: h32,
  outputIndex: 0n,
};

export const address: SDK.AddressData = {
  paymentCredential: { PublicKeyCredential: [h28] },
  stakeCredential: null,
};

export const value: SDK.Value = new Map([["", new Map([["", 1n]])]]);

export const proof: SDK.Proof = [];

export const transitionPhases: readonly SDK.TransitionPhase[] = [
  "Withdrawal",
  "ForcedTransaction",
  "L2Transaction",
  "Deposit",
];

export const eventKeys: readonly SDK.EventKey[] = [
  { WithdrawalEventKey: { withdrawal_id: outputReference } },
  { ForcedTransactionEventKey: { tx_order_id: outputReference } },
  { L2TransactionEventKey: { tx_id: h32 } },
  { DepositEventKey: { deposit_id: outputReference } },
];

export const headerFixture: SDK.Header = {
  prevUtxosRoot: h32,
  utxosRoot: "44".repeat(32),
  withdrawalsRoot: "77".repeat(32),
  forcedTransactionsRoot: "78".repeat(32),
  transactionsRoot: "79".repeat(32),
  depositsRoot: "80".repeat(32),
  transitionTraceRoot: "55".repeat(32),
  eventToStepRoot: "88".repeat(32),
  validationTracesRoot: "89".repeat(32),
  withdrawalCount: 1n,
  forcedTransactionCount: 1n,
  l2TransactionCount: 1n,
  depositCount: 1n,
  totalEventCount: 4n,
  transitionStepCount: 4n,
  validationTraceCount: 4n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28,
  operatorVkey: h28,
  protocolVersion: 1n,
};

export const forcedInclusionTxFixture: SDK.ForcedInclusionTxV1 = {
  tx_id: h32,
  submitted_source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "81",
    field_preimage_lengths_cbor: "82",
  },
  verdict: {
    ForcedTxInvalid: {
      reason: { PlutusExecutionFailed: { execution_index: 0n } },
    },
  },
};

export const l2TransactionSourceFixture: SDK.L2TransactionSource = {
  tx_id: h32,
  source: {
    compact_cbor: "80",
    witness_set_compact_cbor: "81",
    field_preimage_lengths_cbor: "82",
  },
};

export const transitionStepFixture: SDK.TransitionStep = {
  schema_version: 1n,
  step_index: 0n,
  event_key: eventKeys[0]!,
  phase: "Withdrawal",
  pre_utxos_root: h32,
  post_utxos_root: "44".repeat(32),
};

export const secondTransitionStepFixture: SDK.TransitionStep = {
  schema_version: 1n,
  step_index: 1n,
  event_key: eventKeys[1]!,
  phase: "ForcedTransaction",
  pre_utxos_root: "44".repeat(32),
  post_utxos_root: "55".repeat(32),
};

export const eventToStepValueFixture: SDK.EventToStepValue = {
  step_index: 0n,
  phase: "Withdrawal",
};

export const daPayloadBodyFixture: SDK.DaPayloadBody = {
  header_hash: h28,
  header: headerFixture,
  utxos: [["01", "02"]],
  withdrawals: [["03", "04"]],
  forced_transactions: [["05", "06"]],
  transactions: [["07", "08"]],
  deposits: [["09", "0a"]],
  transition_trace: [["0b", "0c"]],
  event_to_step: [["0d", "0e"]],
  transaction_preimages: [["0f", "10"]],
  forced_transaction_preimages: [["11", "12"]],
  cek_program_material: [["13", "14"]],
  validation_traces: [["15", "16"]],
  validation_trace_witnesses: [],
  counts: {
    withdrawalCount: 1n,
    forcedTransactionCount: 1n,
    l2TransactionCount: 1n,
    depositCount: 1n,
    totalEventCount: 4n,
    transitionStepCount: 4n,
    validationTraceCount: 1n,
  },
};

export const roundTrip = <T>(value: T, schema: unknown): T =>
  Data.from(Data.to(value as any, schema as any), schema as any) as T;

export const expectRoundTrip = <T>(value: T, schema: unknown): void =>
  expect(roundTrip(value, schema)).toEqual(value);

export const encodedFixture = (
  value: unknown,
  schema: unknown,
): Omit<GoldenAbiFixture, "schema"> => {
  const cborHex = Data.to(value as never, schema as never);
  return {
    cborHex,
    byteLength: Buffer.byteLength(cborHex, "hex"),
    sha256: createHash("sha256")
      .update(Buffer.from(cborHex, "hex"))
      .digest("hex"),
  };
};

export const expectGoldenFixture = ({
  name,
  schemaName,
  value,
  schema,
}: {
  readonly name: string;
  readonly schemaName: string;
  readonly value: unknown;
  readonly schema: unknown;
}): void => {
  const expected = transitionTraceAbiGolden.fixtures[name];
  expect(
    expected,
    `missing transition trace ABI fixture ${name}`,
  ).toBeDefined();
  expect(expected).toEqual({
    schema: schemaName,
    ...encodedFixture(value, schema),
  });
  expect(Data.from(expected!.cborHex, schema as never)).toEqual(value);
};

export type AbiFixtureValue = {
  readonly schemaName: string;
  readonly value: unknown;
  readonly schema: unknown;
};

export const rootCountProof = (
  domain: SDK.RootDomain,
  root: string,
  phasRoot: string,
  count: bigint,
): SDK.RootCountProof => ({
  domain,
  root,
  phas_root: phasRoot,
  count,
});
