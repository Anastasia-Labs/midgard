import { MidgardTxCodecError, MidgardTxCodecErrorCodes } from "./errors.js";

// Midgard uses the otherwise-reserved Shelley network-nibble bit 3 for output
// protection while preserving the Cardano Shelley address-family high nibble.
export const MIDGARD_PROTECTED_ADDRESS_HEADER_MASK = 0x08;
export const MIDGARD_ADDRESS_MAINNET_HRP = "addr";
export const MIDGARD_ADDRESS_TESTNET_HRP = "addr_test";
export const MIDGARD_ADDRESS_MAINNET_NETWORK_ID = 1;
export const MIDGARD_ADDRESS_TESTNET_NETWORK_ID = 0;

const BECH32_ALPHABET = "qpzry9x8gf2tvdw0s3jn54khce6mua7l";
const BECH32_ALPHABET_LOOKUP = new Map(
  [...BECH32_ALPHABET].map((char, index) => [char, index] as const),
);

export type MidgardAddress = Buffer;

export type MidgardCredentialKind = "PubKey" | "Script";

export type MidgardCredential = {
  readonly kind: MidgardCredentialKind;
  readonly hash: Buffer;
};

export type DecodedMidgardAddress = {
  readonly protected: boolean;
  readonly networkId: number;
  readonly paymentCredential: MidgardCredential;
  readonly stakeCredential?: MidgardCredential;
};

const fail = (message: string, detail?: string): never => {
  throw new MidgardTxCodecError(
    MidgardTxCodecErrorCodes.InvalidFieldType,
    message,
    detail,
  );
};

const validateNetworkForHrp = (hrp: string, networkId: number): void => {
  if (hrp === MIDGARD_ADDRESS_MAINNET_HRP) {
    if (networkId !== MIDGARD_ADDRESS_MAINNET_NETWORK_ID) {
      fail("Mainnet Midgard address text must use network id 1");
    }
    return;
  }
  if (hrp === MIDGARD_ADDRESS_TESTNET_HRP) {
    if (networkId !== MIDGARD_ADDRESS_TESTNET_NETWORK_ID) {
      fail("Testnet Midgard address text must use network id 0");
    }
    return;
  }
  fail("Unsupported Midgard address HRP", hrp);
};

const validateHrpForNetwork = (networkId: number): string => {
  if (networkId === MIDGARD_ADDRESS_MAINNET_NETWORK_ID) {
    return MIDGARD_ADDRESS_MAINNET_HRP;
  }
  if (networkId === MIDGARD_ADDRESS_TESTNET_NETWORK_ID) {
    return MIDGARD_ADDRESS_TESTNET_HRP;
  }
  return fail("Unsupported Midgard address network id", networkId.toString());
};

const bech32Polymod = (values: readonly number[]): number => {
  const generators = [
    0x3b6a57b2, 0x26508e6d, 0x1ea119fa, 0x3d4233dd, 0x2a1462b3,
  ];
  let chk = 1;
  for (const value of values) {
    const top = chk >> 25;
    chk = ((chk & 0x1ffffff) << 5) ^ value;
    for (let i = 0; i < 5; i += 1) {
      if (((top >> i) & 1) !== 0) {
        chk ^= generators[i];
      }
    }
  }
  return chk;
};

const bech32HrpExpand = (hrp: string): number[] => [
  ...[...hrp].map((char) => char.charCodeAt(0) >> 5),
  0,
  ...[...hrp].map((char) => char.charCodeAt(0) & 31),
];

const bech32CreateChecksum = (
  hrp: string,
  data: readonly number[],
): number[] => {
  const values = [...bech32HrpExpand(hrp), ...data, 0, 0, 0, 0, 0, 0];
  const polymod = bech32Polymod(values) ^ 1;
  const checksum: number[] = [];
  for (let i = 0; i < 6; i += 1) {
    checksum.push((polymod >> (5 * (5 - i))) & 31);
  }
  return checksum;
};

const bech32VerifyChecksum = (hrp: string, data: readonly number[]): boolean =>
  bech32Polymod([...bech32HrpExpand(hrp), ...data]) === 1;

const convertBits = (
  data: readonly number[],
  fromBits: number,
  toBits: number,
  pad: boolean,
): number[] => {
  let acc = 0;
  let bits = 0;
  const maxv = (1 << toBits) - 1;
  const maxAcc = (1 << (fromBits + toBits - 1)) - 1;
  const ret: number[] = [];
  for (const value of data) {
    if (value < 0 || value >> fromBits !== 0) {
      fail("Invalid Bech32 data value");
    }
    acc = ((acc << fromBits) | value) & maxAcc;
    bits += fromBits;
    while (bits >= toBits) {
      bits -= toBits;
      ret.push((acc >> bits) & maxv);
    }
  }
  if (pad) {
    if (bits > 0) {
      ret.push((acc << (toBits - bits)) & maxv);
    }
  } else if (bits >= fromBits || ((acc << (toBits - bits)) & maxv) !== 0) {
    fail("Invalid non-zero padding in Bech32 data");
  }
  return ret;
};

const bech32Encode = (hrp: string, payload: Uint8Array): string => {
  const data = convertBits([...payload], 8, 5, true);
  const combined = [...data, ...bech32CreateChecksum(hrp, data)];
  return `${hrp}1${combined.map((value) => BECH32_ALPHABET[value]).join("")}`;
};

const bech32Decode = (
  text: string,
): { readonly hrp: string; readonly payload: Buffer } => {
  if (text.length > 1000) {
    fail("Midgard address text is too long");
  }
  if (text !== text.toLowerCase()) {
    fail("Midgard address text must be lowercase Bech32");
  }
  const separator = text.lastIndexOf("1");
  if (separator <= 0 || separator + 7 > text.length) {
    fail("Invalid Midgard Bech32 address separator");
  }
  const hrp = text.slice(0, separator);
  const rawData = text.slice(separator + 1);
  const data: number[] = [];
  for (const char of rawData) {
    const value = BECH32_ALPHABET_LOOKUP.get(char);
    if (value === undefined) {
      fail("Invalid Midgard Bech32 address character", char);
    }
    data.push(value as number);
  }
  if (!bech32VerifyChecksum(hrp, data)) {
    fail("Invalid Midgard Bech32 checksum");
  }
  return {
    hrp,
    payload: Buffer.from(convertBits(data.slice(0, -6), 5, 8, false)),
  };
};

export const decodeMidgardAddressBytes = (
  address: Uint8Array,
): DecodedMidgardAddress => {
  const bytes = Buffer.from(address);
  if (bytes.length !== 57 && bytes.length !== 29) {
    fail(
      "Midgard address must be a base or enterprise Shelley payment address",
      `length=${bytes.length}`,
    );
  }
  const header = bytes[0];
  const protectedAddress =
    (header & MIDGARD_PROTECTED_ADDRESS_HEADER_MASK) !== 0;
  const unprotectedHeader = header & ~MIDGARD_PROTECTED_ADDRESS_HEADER_MASK;
  const addressType = unprotectedHeader >> 4;
  const networkId = unprotectedHeader & 0x0f;

  if (networkId !== 0 && networkId !== 1) {
    fail("Unsupported Midgard address network id", networkId.toString());
  }

  const isBase = addressType >= 0 && addressType <= 3;
  const isEnterprise = addressType === 6 || addressType === 7;
  if (!isBase && !isEnterprise) {
    fail("Unsupported Midgard address family", addressType.toString());
  }
  if (isBase && bytes.length !== 57) {
    fail("Base Midgard address must be 57 bytes", `length=${bytes.length}`);
  }
  if (isEnterprise && bytes.length !== 29) {
    fail(
      "Enterprise Midgard address must be 29 bytes",
      `length=${bytes.length}`,
    );
  }

  const paymentCredentialKind: MidgardCredentialKind =
    addressType === 1 || addressType === 3 || addressType === 7
      ? "Script"
      : "PubKey";
  const stakeCredentialKind: MidgardCredentialKind | undefined = !isBase
    ? undefined
    : addressType === 2 || addressType === 3
      ? "Script"
      : "PubKey";

  const paymentCredentialHash = Buffer.from(bytes.subarray(1, 29));
  const stakeCredentialHash = isBase
    ? Buffer.from(bytes.subarray(29, 57))
    : undefined;

  return {
    protected: protectedAddress,
    networkId,
    paymentCredential: {
      kind: paymentCredentialKind,
      hash: paymentCredentialHash,
    },
    ...(stakeCredentialHash === undefined || stakeCredentialKind === undefined
      ? {}
      : {
          stakeCredential: {
            kind: stakeCredentialKind,
            hash: stakeCredentialHash,
          },
        }),
  };
};

export const encodeMidgardAddressBytes = (address: Uint8Array): Buffer => {
  decodeMidgardAddressBytes(address);
  return Buffer.from(address);
};

export const midgardAddressFromText = (text: string): Buffer => {
  const normalized = text.trim();
  const decoded = bech32Decode(normalized);
  const address = decodeMidgardAddressBytes(decoded.payload);
  validateNetworkForHrp(decoded.hrp, address.networkId);
  const canonical = encodeMidgardAddressText(decoded.payload);
  if (canonical !== normalized) {
    fail("Midgard address text is not canonical lowercase Bech32");
  }
  return decoded.payload;
};

export const encodeMidgardAddressText = (address: Uint8Array): string => {
  const decoded = decodeMidgardAddressBytes(address);
  return bech32Encode(validateHrpForNetwork(decoded.networkId), address);
};

export const midgardAddressToText = encodeMidgardAddressText;

export const decodeMidgardAddressText = (text: string): DecodedMidgardAddress =>
  decodeMidgardAddressBytes(midgardAddressFromText(text));

export const isProtectedMidgardAddress = (address: Uint8Array): boolean =>
  decodeMidgardAddressBytes(address).protected;

export const unprotectMidgardAddress = (address: Uint8Array): Buffer => {
  decodeMidgardAddressBytes(address);
  const bytes = Buffer.from(address);
  bytes[0] &= ~MIDGARD_PROTECTED_ADDRESS_HEADER_MASK;
  return bytes;
};

export const protectMidgardAddress = (address: Uint8Array): Buffer => {
  decodeMidgardAddressBytes(address);
  const bytes = Buffer.from(address);
  bytes[0] |= MIDGARD_PROTECTED_ADDRESS_HEADER_MASK;
  return bytes;
};

export const paymentCredentialFromMidgardAddress = (
  address: Uint8Array,
): MidgardCredential => {
  const decoded = decodeMidgardAddressBytes(address);
  return decoded.paymentCredential;
};
