/**
 * Out-of-process runner for the C26 maximum-depth CML work.
 *
 * Two reasons this is a separate process rather than inline test code:
 *
 *  1. `--stack-size`. Past the 16 MiB CML wasm shadow stack (built into CML
 *     `6.2.0-2` at source; previously the retired install-time patcher raised
 *     it), a second, independent limit applies: V8 executes wasm frames on the
 *     real machine stack, so depth 4,043 also needs `--stack-size >= 1400`
 *     (2,000 is used for headroom). Setting that per-suite in a child process
 *     keeps default vitest execution — every other suite in every package —
 *     completely unchanged.
 *
 *  2. Trap containment. A CML wasm trap permanently poisons the
 *     `WebAssembly.Instance` for the whole process, so any case that is
 *     *expected* to trap (historically the stock-CML controls) must never
 *     share a worker with real assertions.
 *
 * Usage:  node --stack-size=2000 cml-max-depth-runner-v1.mjs <requestJsonPath>
 * Writes a single JSON object to stdout. Never throws out of `main`; failures
 * are reported as `{ ok: false, ... }` so the caller can assert on them.
 *
 * Request shapes (`bigint` values are encoded as `{"__bigint":"<decimal>"}`):
 *
 *   { "operation": "plutusDataParse",  "cmlMainPath": "...", "depth": 4043 }
 *   { "operation": "transactionParse", "cmlMainPath": "...", "signedTxHex": "..." }
 *   { "operation": "emulatorAdmission",
 *     "signedTxHex": "...", "expectedDatumHex": "...",
 *     "account": { "privateKey": "...", "address": "...",
 *                  "assets": { "lovelace": {"__bigint":"..."} } },
 *     "protocolParameters": { ... } }
 */
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { dirname } from "node:path";
import { fileURLToPath } from "node:url";

const require = createRequire(import.meta.url);
const HERE = dirname(fileURLToPath(import.meta.url));

const reviveBigints = (value) => {
  if (Array.isArray(value)) return value.map(reviveBigints);
  if (value && typeof value === "object") {
    if (typeof value.__bigint === "string") return BigInt(value.__bigint);
    return Object.fromEntries(
      Object.entries(value).map(([key, entry]) => [key, reviveBigints(entry)]),
    );
  }
  return value;
};

const unaryConstructorDataCborHex = (depth) =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

const describeFailure = (cause) => ({
  ok: false,
  errorName: cause && cause.constructor ? cause.constructor.name : typeof cause,
  message: String(cause && cause.message ? cause.message : cause).slice(0, 240),
});

const main = async () => {
  const request = reviveBigints(
    JSON.parse(readFileSync(process.argv[2], "utf8")),
  );

  if (request.operation === "plutusDataParse") {
    const CML = require(request.cmlMainPath);
    const started = process.hrtime.bigint();
    const data = CML.PlutusData.from_cbor_hex(
      unaryConstructorDataCborHex(request.depth),
    );
    const roundTripHex = data.to_cbor_hex();
    const canonicalHex = data.to_canonical_cbor_hex();
    const hashHex = Buffer.from(
      CML.hash_plutus_data(data).to_raw_bytes(),
    ).toString("hex");
    return {
      ok: true,
      operation: request.operation,
      depth: request.depth,
      roundTripIsInput:
        roundTripHex === unaryConstructorDataCborHex(request.depth),
      canonicalEqualsRoundTrip: canonicalHex === roundTripHex,
      hashHex,
      elapsedMs: Number(process.hrtime.bigint() - started) / 1e6,
    };
  }

  if (request.operation === "transactionParse") {
    const CML = require(request.cmlMainPath);
    const started = process.hrtime.bigint();
    const transaction = CML.Transaction.from_cbor_hex(request.signedTxHex);
    const datumHex = transaction
      .body()
      .outputs()
      .get(0)
      .datum()
      .as_datum()
      .to_cbor_hex();
    return {
      ok: true,
      operation: request.operation,
      roundTripIsInput: transaction.to_cbor_hex() === request.signedTxHex,
      datumHex,
      txIdHex: Buffer.from(
        CML.hash_transaction(transaction.body()).to_raw_bytes(),
      ).toString("hex"),
      elapsedMs: Number(process.hrtime.bigint() - started) / 1e6,
    };
  }

  if (request.operation === "emulatorAdmission") {
    // Resolved from this file's directory so it picks up exactly the
    // `@lucid-evolution/lucid` (and therefore exactly the CML wasm) that the
    // midgard-validation suites use.
    const { Emulator } = await import("@lucid-evolution/lucid");
    const account = { seedPhrase: "", ...request.account };
    const emulator = new Emulator([account], request.protocolParameters);
    const started = process.hrtime.bigint();
    const txHash = await emulator.submitTx(request.signedTxHex);
    const confirmed = await emulator.awaitTx(txHash);
    const utxos = await emulator.getUtxos(account.address);
    const admitted = utxos.filter((utxo) => utxo.txHash === txHash);
    return {
      ok: true,
      operation: request.operation,
      signedBytes: request.signedTxHex.length / 2,
      maxTxSize: request.protocolParameters.maxTxSize,
      withinMaxTxSize:
        request.signedTxHex.length / 2 <= request.protocolParameters.maxTxSize,
      txHash,
      confirmed,
      admittedOutputCount: admitted.length,
      emulatorReturnedExactDatum:
        admitted.length === 1 && admitted[0].datum === request.expectedDatumHex,
      returnedDatumBytes:
        admitted.length === 1 && typeof admitted[0].datum === "string"
          ? admitted[0].datum.length / 2
          : null,
      elapsedMs: Number(process.hrtime.bigint() - started) / 1e6,
      runnerDirectory: HERE,
    };
  }

  throw new Error(`unknown operation: ${String(request.operation)}`);
};

main()
  .then((result) => {
    process.stdout.write(JSON.stringify(result));
  })
  .catch((cause) => {
    process.stdout.write(JSON.stringify(describeFailure(cause)));
  });
