import {
  CborReadError,
  readArray,
  readBytes,
  readHead,
  readInt,
  readMap,
  readSmallUint,
  readTag,
  readUint,
  skipItem,
  slice,
} from "../cbor/reader.js";
import { blake2b224 } from "../codec.js";
import type {
  Assets,
  Credential,
  OutputSummary,
  ScriptRef,
  ScriptType,
} from "../types.js";

/** Payment and stake credentials of a Shelley address (CIP-19). */
export const addressCredentials = (
  address: Buffer,
): { payment: Credential | null; stake: Buffer | null } => {
  if (address.length === 0) return { payment: null, stake: null };
  const type = (address[0] as number) >> 4;
  if (type <= 7 && address.length >= 29) {
    const payment = {
      hash: Buffer.from(address.subarray(1, 29)),
      isScript: (type & 1) === 1,
    };
    const stake =
      type <= 3 && address.length >= 57
        ? Buffer.from(address.subarray(29, 57))
        : null;
    return { payment, stake };
  }
  return { payment: null, stake: null };
};

/** A multi-asset map; `signed` admits negative quantities (mint). */
export const readMultiAsset = (
  bytes: Uint8Array,
  offset: number,
  signed: boolean,
): Assets => {
  const assets = new Map<string, Map<string, bigint>>();
  for (const policy of readMap(bytes, offset).entries) {
    const policyId = readBytes(bytes, policy.key).toString("hex");
    const names = assets.get(policyId) ?? new Map<string, bigint>();
    for (const asset of readMap(bytes, policy.value).entries) {
      const name = readBytes(bytes, asset.key).toString("hex");
      const quantity = signed
        ? readInt(bytes, asset.value)
        : readUint(bytes, asset.value);
      names.set(name, quantity);
    }
    assets.set(policyId, names);
  }
  return assets;
};

const readValue = (
  bytes: Uint8Array,
  offset: number,
): { lovelace: bigint; assets: Assets } => {
  const head = readHead(bytes, offset);
  if (head.major === 0) return { lovelace: head.value, assets: new Map() };
  const [coin, multiAsset] = readArray(bytes, offset).items;
  if (coin === undefined || multiAsset === undefined)
    throw new CborReadError("value must be [coin, multiasset]", offset);
  return {
    lovelace: readUint(bytes, coin),
    assets: readMultiAsset(bytes, multiAsset, false),
  };
};

const SCRIPT_TYPES: readonly ScriptType[] = [
  "native",
  "plutus_v1",
  "plutus_v2",
  "plutus_v3",
];

/** `script_ref = #6.24(bytes .cbor script)`; hash = blake2b-224(tag || bytes). */
const readScriptRef = (bytes: Uint8Array, offset: number): ScriptRef => {
  const { tag, item } = readTag(bytes, offset);
  if (tag !== 24n) throw new CborReadError("script_ref must be tag 24", offset);
  const script = readBytes(bytes, item);
  const [typeOffset, bodyOffset] = readArray(script, 0).items;
  if (typeOffset === undefined || bodyOffset === undefined)
    throw new CborReadError("script must be [type, body]", offset);
  const typeIndex = readSmallUint(script, typeOffset);
  const type = SCRIPT_TYPES[typeIndex];
  if (type === undefined)
    throw new CborReadError(`unknown script type ${typeIndex}`, offset);
  const body =
    type === "native"
      ? slice(script, bodyOffset, skipItem(script, bodyOffset))
      : readBytes(script, bodyOffset);
  return {
    hash: blake2b224(Buffer.concat([Buffer.of(typeIndex), body])),
    type,
    bytes: body,
  };
};

const readDatumOption = (
  bytes: Uint8Array,
  offset: number,
): { datumHash: Buffer | null; datum: Buffer | null } => {
  const [kind, content] = readArray(bytes, offset).items;
  if (kind === undefined || content === undefined)
    throw new CborReadError("datum option must be [kind, content]", offset);
  if (readSmallUint(bytes, kind) === 0)
    return { datumHash: readBytes(bytes, content), datum: null };
  const { tag, item } = readTag(bytes, content);
  if (tag !== 24n)
    throw new CborReadError("inline datum must be tag 24", offset);
  return { datumHash: null, datum: readBytes(bytes, item) };
};

const buildOutput = (
  address: Buffer,
  value: { lovelace: bigint; assets: Assets },
  datum: { datumHash: Buffer | null; datum: Buffer | null },
  scriptRef: ScriptRef | null,
): OutputSummary => {
  const credentials = addressCredentials(address);
  return {
    address,
    paymentCredential: credentials.payment,
    stakeCredential: credentials.stake,
    lovelace: value.lovelace,
    assets: value.assets,
    datumHash: datum.datumHash,
    datum: datum.datum,
    scriptRef,
  };
};

/** A legacy (array) or post-Alonzo (map) transaction output. */
export const readOutput = (
  bytes: Uint8Array,
  offset: number,
): OutputSummary => {
  const head = readHead(bytes, offset);
  if (head.major === 4) {
    const [address, value, datumHash] = readArray(bytes, offset).items;
    if (address === undefined || value === undefined)
      throw new CborReadError("legacy output needs address and value", offset);
    return buildOutput(
      readBytes(bytes, address),
      readValue(bytes, value),
      {
        datumHash: datumHash === undefined ? null : readBytes(bytes, datumHash),
        datum: null,
      },
      null,
    );
  }
  let address: Buffer | null = null;
  let value: { lovelace: bigint; assets: Assets } | null = null;
  let datum = {
    datumHash: null as Buffer | null,
    datum: null as Buffer | null,
  };
  let scriptRef: ScriptRef | null = null;
  for (const entry of readMap(bytes, offset).entries) {
    switch (readSmallUint(bytes, entry.key)) {
      case 0:
        address = readBytes(bytes, entry.value);
        break;
      case 1:
        value = readValue(bytes, entry.value);
        break;
      case 2:
        datum = readDatumOption(bytes, entry.value);
        break;
      case 3:
        scriptRef = readScriptRef(bytes, entry.value);
        break;
      default:
        break;
    }
  }
  if (address === null || value === null)
    throw new CborReadError("output map needs address and value", offset);
  return buildOutput(address, value, datum, scriptRef);
};
