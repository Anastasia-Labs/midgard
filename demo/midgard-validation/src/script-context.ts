import {
  decodeMidgardAddressBytes,
  hashMidgardVersionedScript,
  type MidgardCredential,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import { Constr, Data } from "@lucid-evolution/lucid";

import {
  cardanoScriptPurposeData,
  DecodedMidgardRedeemer,
  MidgardScriptPurpose,
  midgardScriptPurposeData,
  redeemerDataFromCborHex,
} from "./midgard-redeemers.js";
import { txOutRefData } from "./tx-out-ref.js";

type ResolvedInput = {
  readonly outRefHex: string;
  readonly output: MidgardTxOutput;
};

type AddressEncoding = "cardano" | "midgard";

export type ScriptMintValue = ReadonlyMap<string, ReadonlyMap<string, bigint>>;

export type ScriptContextView = {
  readonly txId: Buffer;
  readonly inputs: readonly ResolvedInput[];
  readonly referenceInputs: readonly ResolvedInput[];
  readonly outputs: readonly MidgardTxOutput[];
  readonly fee: bigint;
  readonly validityIntervalStart?: bigint;
  readonly validityIntervalEnd?: bigint;
  readonly observers: readonly string[];
  readonly signatories: readonly string[];
  readonly mint: ScriptMintValue;
  readonly redeemers: readonly {
    readonly purpose: MidgardScriptPurpose;
    readonly redeemer: DecodedMidgardRedeemer;
  }[];
};

const none = new Constr(1, []);
const some = (value: unknown) => new Constr(0, [value]);
const bool = (value: boolean) => new Constr(value ? 1 : 0, []);

const credentialData = (credential: MidgardCredential): Constr<unknown> =>
  new Constr(credential.kind === "PubKey" ? 0 : 1, [
    credential.hash.toString("hex"),
  ]);

const stakingCredentialData = (
  credential: MidgardCredential | undefined,
): Constr<unknown> =>
  credential === undefined
    ? none
    : some(new Constr(0, [credentialData(credential)]));

const addressData = (
  output: MidgardTxOutput,
  encoding: AddressEncoding,
): Constr<unknown> => {
  const decoded = decodeMidgardAddressBytes(output.address);
  const constructor = encoding === "midgard" && decoded.protected ? 1 : 0;
  return new Constr(constructor, [
    credentialData(decoded.paymentCredential),
    stakingCredentialData(decoded.stakeCredential),
  ]);
};

const valueData = (
  output: MidgardTxOutput,
): Map<string, Map<string, bigint>> => {
  const result = new Map<string, Map<string, bigint>>();
  for (const [policyId, assets] of output.value.assets.entries()) {
    result.set(policyId, new Map(assets));
  }
  const coin = output.value.lovelace;
  if (coin !== 0n) {
    result.set("", new Map([["", coin]]));
  }
  return result;
};

const compareEntryKeys = (
  [left]: readonly [string, unknown],
  [right]: readonly [string, unknown],
): number => (left < right ? -1 : left > right ? 1 : 0);

const mintData = (mint: ScriptMintValue): Map<string, Map<string, bigint>> => {
  const result = new Map<string, Map<string, bigint>>();
  for (const [policyId, assets] of [...mint.entries()].sort(compareEntryKeys)) {
    result.set(policyId, new Map([...assets.entries()].sort(compareEntryKeys)));
  }
  return result;
};

const datumData = (output: MidgardTxOutput): Constr<unknown> => {
  const datum = output.datum;
  if (datum === undefined) {
    return new Constr(0, []);
  }
  return new Constr(2, [Data.from(datum.cbor.toString("hex")) as unknown]);
};

const txOutData = (
  output: MidgardTxOutput,
  addressEncoding: AddressEncoding,
): Constr<unknown> => {
  const scriptRef = output.script_ref;
  return new Constr(0, [
    addressData(output, addressEncoding),
    valueData(output),
    datumData(output),
    scriptRef === undefined
      ? none
      : some(hashMidgardVersionedScript(scriptRef)),
  ]);
};

const txInInfoData = (
  input: ResolvedInput,
  addressEncoding: AddressEncoding,
): Constr<unknown> =>
  new Constr(0, [
    txOutRefData(input.outRefHex),
    txOutData(input.output, addressEncoding),
  ]);

const validRangeData = (
  start: bigint | undefined,
  end: bigint | undefined,
): Constr<unknown> =>
  new Constr(0, [
    new Constr(0, [
      start === undefined ? new Constr(0, []) : new Constr(1, [start]),
      bool(true),
    ]),
    new Constr(0, [
      end === undefined ? new Constr(0, []) : new Constr(1, [end]),
      bool(false),
    ]),
  ]);

const redeemersData = (
  redeemers: ScriptContextView["redeemers"],
  purposeData: (purpose: MidgardScriptPurpose) => Constr<unknown> | undefined,
): Map<Constr<unknown>, unknown> => {
  const result = new Map<Constr<unknown>, unknown>();
  for (const entry of redeemers) {
    const purpose = purposeData(entry.purpose);
    if (purpose === undefined) {
      continue;
    }
    result.set(purpose, redeemerDataFromCborHex(entry.redeemer.dataCborHex));
  }
  return result;
};

const withdrawalsData = (
  observers: ScriptContextView["observers"],
): Map<Constr<unknown>, bigint> =>
  new Map(
    [...observers].sort().map((observer) => [new Constr(1, [observer]), 0n]),
  );

const baseTxInfoData = (
  view: ScriptContextView,
  purposeData: (purpose: MidgardScriptPurpose) => Constr<unknown> | undefined,
): Constr<unknown> =>
  new Constr(0, [
    view.inputs.map((input) => txInInfoData(input, "cardano")),
    view.referenceInputs.map((input) => txInInfoData(input, "cardano")),
    view.outputs.map((output) => txOutData(output, "cardano")),
    view.fee,
    mintData(view.mint),
    [],
    withdrawalsData(view.observers),
    validRangeData(view.validityIntervalStart, view.validityIntervalEnd),
    [...view.signatories].sort(),
    redeemersData(view.redeemers, purposeData),
    new Map(),
    view.txId.toString("hex"),
    new Map(),
    [],
    none,
    none,
  ]);

const spendDatumData = (
  view: ScriptContextView,
  purpose: Extract<MidgardScriptPurpose, { readonly kind: "spend" }>,
): Constr<unknown> => {
  const input = view.inputs.find(
    (candidateInput) => candidateInput.outRefHex === purpose.outRefHex,
  );
  const datum = input?.output.datum;
  if (datum === undefined) {
    return none;
  }

  return some(Data.from(datum.cbor.toString("hex")) as unknown);
};

const cardanoScriptInfoData = (
  view: ScriptContextView,
  purpose: MidgardScriptPurpose,
): Constr<unknown> => {
  switch (purpose.kind) {
    case "mint":
      return new Constr(0, [purpose.policyId]);
    case "spend":
      return new Constr(1, [
        txOutRefData(purpose.outRefHex),
        spendDatumData(view, purpose),
      ]);
    case "observe":
      return new Constr(2, [new Constr(1, [purpose.scriptHash])]);
    case "receive":
      throw new Error("Receiving scripts require MidgardV1 context");
  }
};

const cardanoScriptPurposeDataOrUndefined = (
  purpose: MidgardScriptPurpose,
): Constr<unknown> | undefined =>
  purpose.kind === "receive" ? undefined : cardanoScriptPurposeData(purpose);

export const buildPlutusV3ScriptContext = (
  view: ScriptContextView,
  purpose: MidgardScriptPurpose,
  redeemer: DecodedMidgardRedeemer,
): Constr<unknown> =>
  new Constr(0, [
    baseTxInfoData(view, cardanoScriptPurposeDataOrUndefined),
    redeemerDataFromCborHex(redeemer.dataCborHex),
    cardanoScriptInfoData(view, purpose),
  ]);

export const buildMidgardV1ScriptContext = (
  view: ScriptContextView,
  purpose: MidgardScriptPurpose,
  redeemer: DecodedMidgardRedeemer,
): Constr<unknown> =>
  new Constr(0, [
    new Constr(0, [
      view.inputs.map((input) => txInInfoData(input, "midgard")),
      view.referenceInputs.map((input) => txInInfoData(input, "midgard")),
      view.outputs.map((output) => txOutData(output, "midgard")),
      view.fee,
      validRangeData(view.validityIntervalStart, view.validityIntervalEnd),
      [...view.observers].sort(),
      [...view.signatories].sort(),
      mintData(view.mint),
      redeemersData(view.redeemers, midgardScriptPurposeData),
      view.txId.toString("hex"),
    ]),
    redeemerDataFromCborHex(redeemer.dataCborHex),
    midgardScriptPurposeData(purpose),
  ]);
