// A fresh child process exercises the actual local UPLC evaluator in each format.
import assert from "node:assert/strict";
import { createRequire } from "node:module";
import { readFileSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
const root = fileURLToPath(new URL("../../../", import.meta.url));
const require = createRequire(import.meta.url);
const lucidRequirePath = require.resolve("@lucid-evolution/lucid");
const lucidRequire = createRequire(lucidRequirePath);
const uplc = lucidRequire("@lucid-evolution/uplc");
const original = uplc.eval_phase_two_raw;
const requests = [];
let CML;
uplc.eval_phase_two_raw = function (...args) {
  const result = original(...args);
  requests.push({
    tx: Buffer.from(args[0]).toString("hex"),
    redeemers: result.map((x) => Buffer.from(x).toString("hex")),
  });
  return result;
};
const format = process.argv[2] ?? "esm";
const L =
  format === "cjs"
    ? require(lucidRequirePath)
    : await import(
        pathToFileURL(join(dirname(lucidRequirePath), "index.js")).href
      );
CML = L.CML;
const {
  Data,
  Emulator,
  Lucid,
  PROTOCOL_PARAMETERS_DEFAULT,
  credentialToAddress,
  validatorToAddress,
  applyDoubleCborEncoding,
} = L;
const key = CML.PrivateKey.from_normal_bytes(new Uint8Array(32).fill(7));
const address = credentialToAddress("Custom", {
  type: "Key",
  hash: key.to_public().hash().to_hex(),
});
const blueprint = JSON.parse(
  readFileSync(
    root + "/midgard-node/blueprints/always-succeeds/plutus.json",
    "utf8",
  ),
);
const simple = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding(blueprint.validators[0].compiledCode),
};
function body(hex) {
  return JSON.parse(CML.Transaction.from_cbor_hex(hex).body().to_json());
}
function units(r) {
  return r.redeemers.map((hex) =>
    JSON.parse(CML.LegacyRedeemer.from_cbor_hex(hex).to_json()),
  );
}
function normalized(b) {
  b = { ...b };
  delete b.script_data_hash;
  return JSON.stringify(b);
}
async function fixture(script = simple) {
  const emulator = new Emulator(
    [{ address, assets: { lovelace: 1_000_000_000n } }],
    { ...PROTOCOL_PARAMETERS_DEFAULT, maxCollateralInputs: 3 },
  );
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromPrivateKey(key.to_bech32());
  const scriptAddress = validatorToAddress("Custom", script);
  const setup = await lucid
    .newTx()
    .pay.ToContract(
      scriptAddress,
      { kind: "inline", value: Data.void() },
      { lovelace: 20_000_000n },
    )
    .pay.ToAddress(address, { lovelace: 6_000_000n })
    .complete();
  await lucid.awaitTx(
    await (await setup.sign.withWallet().complete()).submit(),
  );
  const [input] = await lucid.utxosAt(scriptAddress);
  assert(input);
  return { emulator, lucid, input, script };
}
async function complete(
  f,
  amount = 10_000_000n,
  options = {},
  builderLucid = f.lucid,
) {
  const start = requests.length;
  const tx = await builderLucid
    .newTx()
    .collectFrom([f.input], Data.void())
    .attach.SpendingValidator(f.script)
    .pay.ToAddress(address, { lovelace: amount })
    .complete({ coinSelection: false, localUPLCEval: true, ...options });
  const cbor = (await tx.sign.withWallet().complete()).toCBOR();
  return { cbor, requests: requests.slice(start) };
}
function verifyFinal(result) {
  const b = body(result.cbor);
  const match = result.requests
    .filter((x) => normalized(body(x.tx)) === normalized(b))
    .at(-1);
  assert(match, "final body must be actually evaluated");
  const redeemers = CML.Transaction.from_cbor_hex(result.cbor)
    .witness_set()
    .redeemers()
    .to_flat_format();
  assert.deepEqual(
    units(match),
    Array.from({ length: redeemers.len() }, (_, i) =>
      JSON.parse(redeemers.get(i).to_json()),
    ),
  );
  assert(BigInt(b.total_collateral) >= (BigInt(b.fee) * 150n + 99n) / 100n);
  return {
    fee: b.fee,
    collateral: b.total_collateral,
    requestCount: result.requests.length,
    fees: result.requests.map((x) => body(x.tx).fee),
    units: result.requests.map(units),
  };
}
const report = { format };
try {
  const f = await fixture();
  const first = await complete(f);
  const second = await complete(f);
  assert.equal(first.cbor, second.cbor);
  assert(second.requests.length > 0);
  report.static = verifyFinal(first);
  assert.equal(first.requests.length, 2);
  const custom = [];
  const configured = [];
  const evaluator = (target) => ({
    name: "real-provider-recording",
    evaluate: async ({ tx, additionalUTxOs }) => {
      target.push(tx);
      return f.emulator.evaluateTx(tx, additionalUTxOs);
    },
  });
  await complete(f, 10_000_000n, { evaluator: evaluator(custom) });
  const c = await Lucid(f.emulator, "Custom", {
    evaluator: evaluator(configured),
  });
  c.selectWallet.fromPrivateKey(key.to_bech32());
  await complete(f, 10_000_000n, {}, c);
  report.customRequests = custom.length;
  report.configuredRequests = configured.length;
  assert.equal(custom.length, 3);
  assert.equal(configured.length, 3);
  const selected = await complete(f, 10_000_000n, { coinSelection: true });
  report.coinSelection = verifyFinal(selected);
  assert.equal(selected.requests.length, 3);
  await assert.rejects(complete(f, 20_000_000n), (error) => {
    report.insufficientFundsError = String(error);
    return /insufficient|enough|balance|negative/i.test(String(error));
  });
  report.insufficientFundsRefused = true;
  await assert.rejects(
    complete(f, 10_000_000n, { setCollateral: 0n }),
    (error) => {
      report.finalFeeCollateralError = String(error);
      return /Final transaction requires .* collateral/.test(String(error));
    },
  );
  report.finalFeeCollateralRefused = true;
  await f.lucid.awaitTx(
    await f.lucid
      .fromTx(first.cbor)
      .complete()
      .then((x) => x.submit()),
  );
  report.staticSubmitted = true;
  // Real UPLC fixture: succeeds in both branches, charging more work above a fee threshold.
  const U = lucidRequire("@harmoniclabs/uplc");
  const { ByteString } = lucidRequire("@harmoniclabs/bytestring");
  const app = (f, ...xs) => xs.reduce((f, x) => new U.Application(f, x), f);
  // This encoder inserts each builtin's required forces.
  const bi = (name) => new U.Builtin(U.UPLCBuiltinTag[name]);
  const fields = (x) => app(bi("sndPair", 2), app(bi("unConstrData"), x));
  const nth = (x, n) => {
    while (n--) x = app(bi("tailList", 1), x);
    return app(bi("headList", 1), x);
  };
  const work = (n) => {
    let x = U.UPLCConst.byteString(new ByteString("ab".repeat(32)));
    while (n--) x = app(bi("sha2_256"), x);
    return app(new U.Lambda(U.UPLCConst.unit), x);
  };
  const feeScript = (threshold) => {
    const fee = app(
      bi("unIData"),
      nth(fields(nth(fields(new U.UPLCVar(0)), 0)), 3),
    );
    const choice = app(
      bi("ifThenElse", 1),
      app(bi("lessThanInteger"), fee, U.UPLCConst.int(threshold)),
      new U.Delay(work(64)),
      new U.Delay(work(256)),
    );
    const program = new U.UPLCProgram(
      [1, 1, 0],
      new U.Lambda(new U.Force(choice)),
    );
    return {
      type: "PlutusV3",
      script: applyDoubleCborEncoding(
        Buffer.from(U.compileUPLC(program).toBuffer().buffer).toString("hex"),
      ),
    };
  };
  const probe = await complete(await fixture(feeScript(100_000_000n)));
  const p = verifyFinal(probe);
  const threshold = BigInt(Math.floor((p.fees[0] + p.fee) / 2));
  const sensitive = await fixture(feeScript(threshold));
  const result = await complete(sensitive);
  report.feeSensitive = verifyFinal(result);
  report.feeSensitive.threshold = String(threshold);
  const allUnits = result.requests.map((r) =>
    units(r).reduce((n, x) => n + BigInt(x.ex_units.steps), 0n),
  );
  assert(allUnits.at(-1) > allUnits[0]);
  assert(
    result.requests.length >= 3,
    "final fee-sensitive cost must cause another full-context evaluation",
  );
  await assert.rejects(
    complete(sensitive, 10_000_000n, { setCollateral: 0n }),
    (error) => {
      report.feeSensitiveCollateralError = String(error);
      return /Final transaction requires .* collateral/.test(String(error));
    },
  );
  const sufficient = await complete(sensitive, 10_000_000n, {
    setCollateral: 1_000_000n,
  });
  report.feeSensitiveCollateralRetry = verifyFinal(sufficient);
  await sensitive.lucid.awaitTx(
    await sensitive.lucid
      .fromTx(sufficient.cbor)
      .complete()
      .then((x) => x.submit()),
  );
  report.feeSensitiveSubmitted = true;
} catch (error) {
  console.error(JSON.stringify(report));
  throw error;
} finally {
  uplc.eval_phase_two_raw = original;
}
console.log(JSON.stringify(report));
