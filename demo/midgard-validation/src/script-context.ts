import {
  decodeMidgardAddressBytes,
  hashMidgardVersionedScript,
  type MidgardCredential,
  type MidgardTxOutput,
} from "@al-ft/midgard-core/codec";
import {
  type Data,
  DataB,
  DataConstr,
  DataI,
  DataList,
  DataMap,
  type KV,
} from "@harmoniclabs/plutus-data";
import { Constr } from "@lucid-evolution/lucid";

import type { MidgardLedgerRedeemer } from "./ledger-tx/types.js";
import {
  cardanoScriptPurposeData,
  MidgardScriptPurpose,
  midgardScriptPurposeData,
} from "./midgard-redeemers.js";
import { plutusDataFromCborIterative } from "./plutus-data-iterative.decode.js";
import { txOutRefData } from "./tx-out-ref.js";

type ResolvedInput = {
  readonly outRefHex: string;
  readonly output: MidgardTxOutput;
};

export type ScriptContextAddressEncoding = "cardano" | "midgard";

export type ScriptMintValue = ReadonlyMap<string, ReadonlyMap<string, bigint>>;

/**
 * The context's redeemer map is in ledger order, (tag, index) ascending,
 * whatever the order of `redeemers`.
 */
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
    readonly redeemer: MidgardLedgerRedeemer;
  }[];
};

// The context is built as harmonic `Data`, never as Lucid `Data`: Lucid's
// encoder sorts every map and its decoder merges duplicate keys, while the
// fault-proof side commits datum and redeemer maps in their CBOR entry order.
// Maps built here are sorted explicitly where the context orders them.

const constr = (index: number, fields: Data[]): DataConstr =>
  new DataConstr(index, fields);
const bytes = (hex: string): DataB => new DataB(Buffer.from(hex, "hex"));
const none = (): DataConstr => constr(1, []);
const some = (value: Data): DataConstr => constr(0, [value]);
const bool = (value: boolean): DataConstr => constr(value ? 1 : 0, []);

/**
 * Converts the purpose and out-ref Data, which have no maps, from the Lucid
 * form their builders return.
 */
const fixedShapeData = (value: unknown): Data => {
  if (typeof value === "bigint") {
    return new DataI(value);
  }
  if (typeof value === "string") {
    return bytes(value);
  }
  if (Array.isArray(value)) {
    return new DataList(value.map(fixedShapeData));
  }
  if (value instanceof Constr) {
    return constr(value.index, value.fields.map(fixedShapeData));
  }
  throw new Error("script context purpose must be map-free Data");
};

const compareHex = (left: string, right: string): number =>
  left < right ? -1 : left > right ? 1 : 0;

const credentialData = (credential: MidgardCredential): DataConstr =>
  constr(credential.kind === "PubKey" ? 0 : 1, [
    new DataB(Uint8Array.from(credential.hash)),
  ]);

const stakingCredentialData = (
  credential: MidgardCredential | undefined,
): DataConstr =>
  credential === undefined
    ? none()
    : some(constr(0, [credentialData(credential)]));

const addressData = (
  output: MidgardTxOutput,
  encoding: ScriptContextAddressEncoding,
): DataConstr => {
  const decoded = decodeMidgardAddressBytes(output.address);
  const constructor = encoding === "midgard" && decoded.protected ? 1 : 0;
  return constr(constructor, [
    credentialData(decoded.paymentCredential),
    stakingCredentialData(decoded.stakeCredential),
  ]);
};

/** Policies and asset names ordered by their bytes. */
const multiAssetPairs = (assets: ScriptMintValue): KV<Data, Data>[] =>
  [...assets.entries()]
    .sort(([left], [right]) => compareHex(left, right))
    .map(([policyId, names]) => ({
      fst: bytes(policyId),
      snd: new DataMap(
        [...names.entries()]
          .sort(([left], [right]) => compareHex(left, right))
          .map(([name, quantity]) => ({
            fst: bytes(name),
            snd: new DataI(quantity),
          })),
      ),
    }));

const valueData = (output: MidgardTxOutput): DataMap<Data, Data> => {
  const coin = output.value.lovelace;
  return new DataMap([
    ...(coin === 0n
      ? []
      : [
          {
            fst: bytes(""),
            snd: new DataMap([{ fst: bytes(""), snd: new DataI(coin) }]),
          },
        ]),
    ...multiAssetPairs(output.value.assets),
  ]);
};

const mintData = (mint: ScriptMintValue): DataMap<Data, Data> =>
  new DataMap(multiAssetPairs(mint));

const datumData = (output: MidgardTxOutput): DataConstr => {
  const datum = output.datum;
  if (datum === undefined) {
    return constr(0, []);
  }
  return constr(2, [plutusDataFromCborIterative(datum.cbor)]);
};

export const scriptContextTxOutData = (
  output: MidgardTxOutput,
  addressEncoding: ScriptContextAddressEncoding,
): DataConstr => {
  const scriptRef = output.script_ref;
  return constr(0, [
    addressData(output, addressEncoding),
    valueData(output),
    datumData(output),
    scriptRef === undefined
      ? none()
      : some(bytes(hashMidgardVersionedScript(scriptRef))),
  ]);
};

export const scriptContextTxInInfoData = (
  input: ResolvedInput,
  addressEncoding: ScriptContextAddressEncoding,
): DataConstr =>
  constr(0, [
    fixedShapeData(txOutRefData(input.outRefHex)),
    scriptContextTxOutData(input.output, addressEncoding),
  ]);

const validRangeData = (
  start: bigint | undefined,
  end: bigint | undefined,
): DataConstr =>
  constr(0, [
    constr(0, [
      start === undefined ? constr(0, []) : constr(1, [new DataI(start)]),
      bool(true),
    ]),
    constr(0, [
      end === undefined ? constr(0, []) : constr(1, [new DataI(end)]),
      bool(false),
    ]),
  ]);

const redeemerData = (redeemer: MidgardLedgerRedeemer): Data =>
  plutusDataFromCborIterative(redeemer.dataCbor);

// Cardano orders its redeemer map by (tag, index), and Midgard's redeemer
// tags follow the same order: spend, mint, reward, then receiving.
const compareRedeemerPointers = (
  left: ScriptContextView["redeemers"][number],
  right: ScriptContextView["redeemers"][number],
): number =>
  left.redeemer.tag - right.redeemer.tag ||
  (left.redeemer.index < right.redeemer.index
    ? -1
    : left.redeemer.index > right.redeemer.index
      ? 1
      : 0);

const redeemersData = (
  redeemers: ScriptContextView["redeemers"],
  purposeData: (purpose: MidgardScriptPurpose) => Constr<unknown> | undefined,
): DataMap<Data, Data> =>
  new DataMap(
    [...redeemers].sort(compareRedeemerPointers).flatMap((entry) => {
      const purpose = purposeData(entry.purpose);
      return purpose === undefined
        ? []
        : [{ fst: fixedShapeData(purpose), snd: redeemerData(entry.redeemer) }];
    }),
  );

const withdrawalsData = (
  observers: ScriptContextView["observers"],
): DataMap<Data, Data> =>
  new DataMap(
    [...observers].sort().map((observer) => ({
      fst: constr(1, [bytes(observer)]),
      snd: new DataI(0n),
    })),
  );

const bytesList = (values: readonly string[]): DataList =>
  new DataList([...values].sort().map(bytes));

const baseTxInfoData = (
  view: ScriptContextView,
  purposeData: (purpose: MidgardScriptPurpose) => Constr<unknown> | undefined,
): DataConstr =>
  constr(0, [
    new DataList(
      view.inputs.map((input) => scriptContextTxInInfoData(input, "cardano")),
    ),
    new DataList(
      view.referenceInputs.map((input) =>
        scriptContextTxInInfoData(input, "cardano"),
      ),
    ),
    new DataList(
      view.outputs.map((output) => scriptContextTxOutData(output, "cardano")),
    ),
    new DataI(view.fee),
    mintData(view.mint),
    new DataList([]),
    withdrawalsData(view.observers),
    validRangeData(view.validityIntervalStart, view.validityIntervalEnd),
    bytesList(view.signatories),
    redeemersData(view.redeemers, purposeData),
    new DataMap([]),
    new DataB(Uint8Array.from(view.txId)),
    new DataMap([]),
    new DataList([]),
    none(),
    none(),
  ]);

const spendDatumData = (
  view: ScriptContextView,
  purpose: Extract<MidgardScriptPurpose, { readonly kind: "spend" }>,
): DataConstr => {
  const input = view.inputs.find(
    (candidateInput) => candidateInput.outRefHex === purpose.outRefHex,
  );
  const datum = input?.output.datum;
  if (datum === undefined) {
    return none();
  }

  return some(plutusDataFromCborIterative(datum.cbor));
};

const cardanoScriptInfoData = (
  view: ScriptContextView,
  purpose: MidgardScriptPurpose,
): DataConstr => {
  switch (purpose.kind) {
    case "mint":
      return constr(0, [bytes(purpose.policyId)]);
    case "spend":
      return constr(1, [
        fixedShapeData(txOutRefData(purpose.outRefHex)),
        spendDatumData(view, purpose),
      ]);
    case "observe":
      return constr(2, [constr(1, [bytes(purpose.scriptHash)])]);
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
  redeemer: MidgardLedgerRedeemer,
): DataConstr =>
  constr(0, [
    baseTxInfoData(view, cardanoScriptPurposeDataOrUndefined),
    redeemerData(redeemer),
    cardanoScriptInfoData(view, purpose),
  ]);

export const buildMidgardScriptContext = (
  view: ScriptContextView,
  purpose: MidgardScriptPurpose,
  redeemer: MidgardLedgerRedeemer,
): DataConstr =>
  constr(0, [
    constr(0, [
      new DataList(
        view.inputs.map((input) => scriptContextTxInInfoData(input, "midgard")),
      ),
      new DataList(
        view.referenceInputs.map((input) =>
          scriptContextTxInInfoData(input, "midgard"),
        ),
      ),
      new DataList(
        view.outputs.map((output) => scriptContextTxOutData(output, "midgard")),
      ),
      new DataI(view.fee),
      validRangeData(view.validityIntervalStart, view.validityIntervalEnd),
      bytesList(view.observers),
      bytesList(view.signatories),
      mintData(view.mint),
      redeemersData(view.redeemers, midgardScriptPurposeData),
      new DataB(Uint8Array.from(view.txId)),
    ]),
    redeemerData(redeemer),
    fixedShapeData(midgardScriptPurposeData(purpose)),
  ]);
