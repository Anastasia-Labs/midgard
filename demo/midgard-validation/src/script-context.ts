import { CML, Constr, Data } from "@lucid-evolution/lucid";
import {
  decodeMidgardAddressBytes,
  hashMidgardVersionedScript,
  type MidgardCredential,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  cardanoScriptPurposeData,
  DecodedMidgardRedeemer,
  midgardScriptPurposeData,
  MidgardScriptPurpose,
  outputReferenceData,
  redeemerDataFromCborHex,
} from "./midgard-redeemers.js";

type ResolvedInput = {
  readonly outRefHex: string;
  readonly output: MidgardTxOutput;
};

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

const txOutRefData = (outRefHex: string): Constr<unknown> => {
  const input = CML.TransactionInput.from_cbor_bytes(
    Buffer.from(outRefHex, "hex"),
  );
  return new Constr(0, [
    input.transaction_id().to_hex(),
    BigInt(input.index()),
  ]);
};

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

const addressData = (output: MidgardTxOutput): Constr<unknown> => {
  const decoded = decodeMidgardAddressBytes(output.address);
  return new Constr(0, [
    credentialData(decoded.paymentCredential),
    stakingCredentialData(decoded.stakeCredential),
  ]);
};

const multiassetToData = (
  multiasset: InstanceType<typeof CML.MultiAsset> | undefined,
): Map<string, Map<string, bigint>> => {
  const result = new Map<string, Map<string, bigint>>();
  if (multiasset === undefined) {
    return result;
  }
  const policies = multiasset.keys();
  for (let i = 0; i < policies.len(); i += 1) {
    const policy = policies.get(i);
    const assets = multiasset.get_assets(policy);
    if (assets === undefined) {
      continue;
    }
    const assetMap = new Map<string, bigint>();
    const names = assets.keys();
    for (let j = 0; j < names.len(); j += 1) {
      const name = names.get(j);
      const quantity = assets.get(name);
      if (quantity !== undefined) {
        assetMap.set(
          Buffer.from(name.to_raw_bytes()).toString("hex"),
          quantity,
        );
      }
    }
    result.set(policy.to_hex(), assetMap);
  }
  return result;
};

const valueData = (output: MidgardTxOutput): Map<string, Map<string, bigint>> => {
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

const mintData = (mint: ScriptMintValue): Map<string, Map<string, bigint>> => {
  const result = new Map<string, Map<string, bigint>>();
  for (const [policyId, assets] of [...mint.entries()].sort(([left], [right]) =>
    left.localeCompare(right),
  )) {
    result.set(
      policyId,
      new Map(
        [...assets.entries()].sort(([left], [right]) =>
          left.localeCompare(right),
        ),
      ),
    );
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

const txOutData = (output: MidgardTxOutput): Constr<unknown> => {
  const scriptRef = output.script_ref;
  return new Constr(0, [
    addressData(output),
    valueData(output),
    datumData(output),
    scriptRef === undefined
      ? none
      : some(hashMidgardVersionedScript(scriptRef)),
  ]);
};

const txInInfoData = (input: ResolvedInput): Constr<unknown> =>
  new Constr(0, [txOutRefData(input.outRefHex), txOutData(input.output)]);

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
    [...observers]
      .sort((left, right) => left.localeCompare(right))
      .map((observer) => [new Constr(1, [observer]), 0n]),
  );

const baseTxInfoData = (
  view: ScriptContextView,
  purposeData: (purpose: MidgardScriptPurpose) => Constr<unknown> | undefined,
): Constr<unknown> =>
  new Constr(0, [
    view.inputs.map(txInInfoData),
    view.referenceInputs.map(txInInfoData),
    view.outputs.map(txOutData),
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
        outputReferenceData(purpose.outRefHex),
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
      view.inputs.map(txInInfoData),
      view.referenceInputs.map(txInInfoData),
      view.outputs.map(txOutData),
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
