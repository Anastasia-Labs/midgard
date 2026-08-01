/**
 * Deterministic CML exercise used to prove the C26 wasm shadow-stack patch is
 * output-equivalent to the stock binary (owner condition #2).
 *
 * Usage: node --stack-size=<kb> cml-wasm-differential-scenarios.cjs <cmlMainJs>
 *
 * It is run twice with identical Node flags — once against the installed
 * (patched) CML package and once against a temporary package directory holding
 * the byte-exact stock wasm reconstructed by `--revert` — and the two stdout
 * strings are compared byte for byte. Every value printed here is derived from
 * fixed inputs, so any divergence is attributable to the binary.
 *
 * Nothing in here may read the clock, the filesystem, RNG, or the environment.
 */
"use strict";

const cmlMainPath = process.argv[2];
if (!cmlMainPath) {
  process.stderr.write("usage: cml-wasm-differential-scenarios.cjs <cmlMainJs>\n");
  process.exit(2);
}
const CML = require(cmlMainPath);

const hex = (bytes) => Buffer.from(bytes).toString("hex");
const unary = (depth) => "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

/* ---------- raw CBOR helpers (no CML involvement) ---------- */
const head = (major, value) => {
  if (value < 24n) return Buffer.from([(major << 5) | Number(value)]);
  const widths = [
    [0xffn, 24, 1],
    [0xffffn, 25, 2],
    [0xffff_ffffn, 26, 4],
    [0xffff_ffff_ffff_ffffn, 27, 8],
  ];
  const width = widths.find(([limit]) => value <= limit);
  const out = Buffer.alloc(1 + width[2]);
  out[0] = (major << 5) | width[1];
  let rest = value;
  for (let index = width[2]; index > 0; index -= 1) {
    out[index] = Number(rest & 0xffn);
    rest >>= 8n;
  }
  return out;
};
const uint = (value) => head(0, value);
const bytes = (buffer) => Buffer.concat([head(2, BigInt(buffer.length)), Buffer.from(buffer)]);
const array = (items) => Buffer.concat([head(4, BigInt(items.length)), ...items.map(Buffer.from)]);
const map = (entries) =>
  Buffer.concat([head(5, BigInt(entries.length)), ...entries.flatMap(([k, v]) => [Buffer.from(k), Buffer.from(v)])]);
const tag = (value, item) => Buffer.concat([head(6, value), Buffer.from(item)]);

const scenarios = {};

/* ---------- 1. Ed25519 key derivation, signing, verification ---------- */
const seed = Buffer.alloc(32);
for (let index = 0; index < 32; index += 1) seed[index] = (index * 7 + 3) & 0xff;
const privateKey = CML.PrivateKey.from_normal_bytes(seed);
const publicKey = privateKey.to_public();
const message = Buffer.from("c26 cml wasm differential message", "utf8");
const signature = privateKey.sign(message);
scenarios.ed25519 = {
  privateKeyBech32: privateKey.to_bech32(),
  publicKeyHex: hex(publicKey.to_raw_bytes()),
  publicKeyBech32: publicKey.to_bech32(),
  keyHashHex: hex(publicKey.hash().to_raw_bytes()),
  signatureHex: hex(signature.to_raw_bytes()),
  signatureBech32: signature.to_bech32(),
  verifies: publicKey.verify(message, signature),
  rejectsTamperedMessage: publicKey.verify(Buffer.from("tampered", "utf8"), signature),
};

/* ---------- 2. Address construction and bech32 round trip ---------- */
const credential = CML.Credential.new_pub_key(publicKey.hash());
const stakeSeed = Buffer.alloc(32, 0x5a);
const stakeCredential = CML.Credential.new_pub_key(
  CML.PrivateKey.from_normal_bytes(stakeSeed).to_public().hash(),
);
const enterprise = CML.EnterpriseAddress.new(0, credential).to_address();
const base = CML.BaseAddress.new(1, credential, stakeCredential).to_address();
scenarios.addresses = {
  enterpriseBech32: enterprise.to_bech32(),
  enterpriseRawHex: hex(enterprise.to_raw_bytes()),
  enterpriseRoundTripHex: hex(CML.Address.from_bech32(enterprise.to_bech32()).to_raw_bytes()),
  baseBech32: base.to_bech32(),
  baseRawHex: hex(base.to_raw_bytes()),
  baseNetworkId: base.network_id(),
  rewardBech32: CML.RewardAddress.new(1, stakeCredential).to_address().to_bech32(),
};

/* ---------- 3. Plutus Data shapes, including deep nesting ---------- */
const plutusDataCases = [
  ["unit", "d87980"],
  ["intZero", "00"],
  ["intNegative", "3903e7"],
  ["bigPositive", "c249010000000000000000"],
  ["bigNegative", "c349010000000000000000"],
  ["byteString", "581c00112233445566778899aabbccddeeff00112233445566778899aabb"],
  ["chunkedByteString", "5f5840" + "3d".repeat(64) + "43aabbcc" + "ff"],
  ["listMixed", "9f0102581c" + "aa".repeat(28) + "d87980ff"],
  ["mapOrdered", "a3000102030405"],
  ["constrLarge", "d905009f0102ff"],
  ["nestedShallow", unary(8)],
  ["nested300", unary(300)],
  ["nested1400", unary(1_400)],
];
scenarios.plutusData = plutusDataCases.map(([name, cborHex]) => {
  const data = CML.PlutusData.from_cbor_hex(cborHex);
  return {
    name,
    kind: data.kind(),
    roundTripHex: data.to_cbor_hex(),
    canonicalHex: data.to_canonical_cbor_hex(),
    hashHex: hex(CML.hash_plutus_data(data).to_raw_bytes()),
    jsonSchemaDetailed: CML.decode_plutus_datum_to_json_str(data, CML.CardanoNodePlutusDatumSchema.DetailedSchema).length,
  };
});

/* ---------- 4. Multi-asset Value CBOR ---------- */
const multiAssetHex = array([
  uint(4_500_000n),
  map([
    [
      bytes(Buffer.alloc(28, 0x11)),
      map([
        [bytes(Buffer.from("MidgardA", "utf8")), uint(17n)],
        [bytes(Buffer.from("MidgardB", "utf8")), uint(999_999n)],
      ]),
    ],
    [bytes(Buffer.alloc(28, 0x22)), map([[bytes(Buffer.alloc(0)), uint(1n)]])],
  ]),
]).toString("hex");
const value = CML.Value.from_cbor_hex(multiAssetHex);
scenarios.value = {
  roundTripHex: value.to_cbor_hex(),
  coin: value.coin().toString(),
  policyCount: value.multi_asset().policy_count(),
  checkedAddHex: value.checked_add(CML.Value.from_coin(1_000_000n)).to_cbor_hex(),
  minAdaRequired: CML.min_ada_required(
    CML.TransactionOutput.new(enterprise, value, undefined, undefined),
    4_310n,
  ).toString(),
};

/* ---------- 5. Native script construction and hashing ---------- */
const scriptPubkey = CML.NativeScript.new_script_pubkey(publicKey.hash());
const scriptTimelock = CML.NativeScript.new_script_invalid_hereafter(99_999n);
const scriptList = CML.NativeScriptList.new();
scriptList.add(scriptPubkey);
scriptList.add(scriptTimelock);
const scriptAll = CML.NativeScript.new_script_all(scriptList);
scenarios.nativeScript = {
  pubkeyHashHex: hex(scriptPubkey.hash().to_raw_bytes()),
  allHashHex: hex(scriptAll.hash().to_raw_bytes()),
  allCborHex: scriptAll.to_cbor_hex(),
  jsonLength: scriptAll.to_json().length,
};

/* ---------- 6. Whole transaction with an 800-deep inline datum ---------- */
const deepDatum = Buffer.from(unary(800), "hex");
const txInputs = tag(258n, array([array([bytes(Buffer.alloc(32, 0x3c)), uint(0n)])]));
const txOutput = map([
  [uint(0n), bytes(enterprise.to_raw_bytes())],
  [uint(1n), uint(39_998_000_000n)],
  [uint(2n), array([uint(1n), tag(24n, bytes(deepDatum))])],
]);
const txBody = map([
  [uint(0n), txInputs],
  [uint(1n), array([txOutput])],
  [uint(2n), uint(2_000_000n)],
]);
const txWitnessSet = map([
  [
    uint(0n),
    tag(258n, array([array([bytes(publicKey.to_raw_bytes()), bytes(signature.to_raw_bytes())])])),
  ],
]);
const txHex = array([txBody, txWitnessSet, Buffer.from([0xf5]), Buffer.from([0xf6])]).toString("hex");
const transaction = CML.Transaction.from_cbor_hex(txHex);
scenarios.transaction = {
  roundTripHex: transaction.to_cbor_hex(),
  roundTripIsInputIdentical: transaction.to_cbor_hex() === txHex,
  txIdHex: hex(CML.hash_transaction(transaction.body()).to_raw_bytes()),
  bodyCborHex: transaction.body().to_cbor_hex(),
  outputDatumHex: transaction.body().outputs().get(0).datum().as_datum().to_cbor_hex(),
  outputDatumHashHex: hex(
    CML.hash_plutus_data(transaction.body().outputs().get(0).datum().as_datum()).to_raw_bytes(),
  ),
  minNoScriptFee: CML.min_no_script_fee(transaction, CML.LinearFee.new(44n, 155_381n, 15n)).toString(),
  auxiliaryDataPresent: transaction.auxiliary_data() !== undefined,
};

/* ---------- 7. BigInteger arithmetic and encoding ---------- */
const bigIntegerCases = [
  "0",
  "-1",
  "18446744073709551615",
  "-18446744073709551616",
  "115792089237316195423570985008687907853269984665640564039457584007913129639935",
];
scenarios.bigInteger = bigIntegerCases.map((decimal) => {
  const parsed = CML.BigInteger.from_str(decimal);
  return {
    decimal,
    cborHex: parsed.to_cbor_hex(),
    roundTrip: CML.BigInteger.from_cbor_hex(parsed.to_cbor_hex()).to_str(),
    asU64: parsed.as_u64() === undefined ? null : parsed.as_u64().toString(),
  };
});

/* ---------- 8. Allocation churn (4,000 iterations) ---------- */
let churnAccumulator = 0;
let churnLastHex = "";
for (let iteration = 0; iteration < 4_000; iteration += 1) {
  const datum = CML.PlutusData.new_integer(CML.BigInteger.from_str(String(iteration)));
  const list = CML.PlutusDataList.new();
  list.add(datum);
  list.add(CML.PlutusData.new_bytes(Buffer.alloc((iteration % 32) + 1, iteration & 0xff)));
  const constr = CML.PlutusData.new_constr_plutus_data(
    CML.ConstrPlutusData.new(BigInt(iteration % 7), list),
  );
  churnLastHex = constr.to_cbor_hex();
  churnAccumulator = (churnAccumulator + churnLastHex.length) % 1_000_003;
}
scenarios.allocationChurn = { iterations: 4_000, accumulator: churnAccumulator, lastHex: churnLastHex };

process.stdout.write(`${JSON.stringify(scenarios, null, 2)}\n`);
