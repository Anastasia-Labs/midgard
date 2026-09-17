import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { createRequire } from "node:module";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";

// A fresh child process installs this hook before either Lucid or its UPLC
// namespace exists. It counts real evaluations without replacing their results.
const require = createRequire(import.meta.url);
const format = process.argv[2];
assert(format === "esm" || format === "cjs");
const lucidRequirePath = require.resolve("@lucid-evolution/lucid");
const lucidPath = join(dirname(lucidRequirePath), "index.js");
const lucidRequire = createRequire(lucidPath);
const uplcPath = lucidRequire.resolve("@lucid-evolution/uplc");
const uplc = lucidRequire("@lucid-evolution/uplc");
const descriptor = Object.getOwnPropertyDescriptor(uplc, "eval_phase_two_raw");
assert(descriptor?.writable && descriptor.configurable);
const original = descriptor.value;
const requests = [];
const digest = (value) =>
  createHash("sha256").update(JSON.stringify(value)).digest("hex");
const encode = (value) =>
  value instanceof Uint8Array
    ? ["bytes", Buffer.from(value).toString("hex")]
    : Array.isArray(value)
      ? value.map(encode)
      : [typeof value, String(value)];
Object.defineProperty(uplc, "eval_phase_two_raw", {
  ...descriptor,
  value(...args) {
    assert.equal(args.length, 9);
    requests.push(digest(encode(args)));
    return Reflect.apply(original, this, args);
  },
});
let report;
try {
  const namespace = await import(pathToFileURL(uplcPath).href);
  assert.equal(namespace.eval_phase_two_raw, uplc.eval_phase_two_raw);
  const {
    CML,
    Data,
    Emulator,
    Lucid,
    PROTOCOL_PARAMETERS_DEFAULT,
    applyDoubleCborEncoding,
    credentialToAddress,
    validatorToAddress,
  } =
    format === "cjs"
      ? require(lucidRequirePath)
      : await import(pathToFileURL(lucidPath).href);
  const privateKey = CML.PrivateKey.from_normal_bytes(
    new Uint8Array(32).fill(7),
  );
  const address = credentialToAddress("Custom", {
    type: "Key",
    hash: privateKey.to_public().hash().to_hex(),
  });
  const account = { address, assets: { lovelace: 1_000_000_000n } };
  const emulator = new Emulator([account], {
    ...PROTOCOL_PARAMETERS_DEFAULT,
    maxCollateralInputs: 3,
  });
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromPrivateKey(privateKey.to_bech32());
  const blueprint = JSON.parse(
    readFileSync(
      new URL(
        "../../../midgard-node/blueprints/always-succeeds/plutus.json",
        import.meta.url,
      ),
      "utf8",
    ),
  );
  const script = {
    type: "PlutusV3",
    script: applyDoubleCborEncoding(blueprint.validators[0].compiledCode),
  };
  const scriptAddress = validatorToAddress("Custom", script);
  const setup = await lucid
    .newTx()
    .pay.ToContract(
      scriptAddress,
      { kind: "inline", value: Data.void() },
      { lovelace: 20_000_000n },
    )
    .pay.ToAddress(address, { lovelace: 6_000_000n })
    .pay.ToAddress(address, { lovelace: 9_000_000n })
    .complete({ localUPLCEval: true });
  const signedSetup = await setup.sign.withWallet().complete();
  await lucid.awaitTx(await signedSetup.submit());
  const [input] = await lucid.utxosAt(scriptAddress);
  assert(input);
  const complete = async (
    amount = 10_000_000n,
    options = {},
    builderLucid = lucid,
  ) => {
    const start = requests.length;
    const callbacks = [];
    const tx = await builderLucid
      .newTx()
      .collectFrom([input], (context) => {
        const index = context.inputIndex(input);
        callbacks.push(String(index));
        return Data.to(index);
      })
      .attach.SpendingValidator(script)
      .pay.ToAddress(address, { lovelace: amount })
      .complete({ ...options, localUPLCEval: true });
    const signed = await tx.sign.withWallet().complete();
    return {
      requests: requests.slice(start),
      callbacks,
      cbor: signed.toCBOR(),
      hash: signed.toHash(),
      collateral: tx.toTransaction().body().total_collateral()?.toString(),
    };
  };
  const first = await complete();
  const second = await complete();
  const changed = await complete(11_000_000n);
  const changedCollateral = await complete(10_000_000n, {
    setCollateral: 8_000_000n,
  });
  const customFailure =
    "custom evaluator rejects the repeated convergence request";
  // This deliberately stateful adapter checks callback semantics only. The
  // emulator provider returns embedded units; actual UPLC is covered above.
  const customEvaluator = (recorded) => ({
    name: "stateful-callback-semantics",
    async evaluate({ tx, additionalUTxOs }) {
      recorded.push(digest(tx));
      if (recorded.length === 4) throw new Error(customFailure);
      return await emulator.evaluateTx(tx, additionalUTxOs);
    },
  });
  const customRequests = [];
  await assert.rejects(
    complete(10_000_000n, {
      evaluator: customEvaluator(customRequests),
    }),
    new RegExp(customFailure),
  );
  const configuredCustomRequests = [];
  const configured = await Lucid(emulator, "Custom", {
    evaluator: customEvaluator(configuredCustomRequests),
  });
  configured.selectWallet.fromPrivateKey(privateKey.to_bech32());
  await assert.rejects(
    complete(10_000_000n, {}, configured),
    new RegExp(customFailure),
  );
  const submittedHash = await lucid
    .fromTx(first.cbor)
    .complete()
    .then((tx) => tx.submit());
  await lucid.awaitTx(submittedHash);
  report = {
    first,
    second,
    changed,
    changedCollateral,
    customRequests,
    configuredCustomRequests,
    submittedHash,
  };
} finally {
  Object.defineProperty(uplc, "eval_phase_two_raw", descriptor);
  assert.equal(uplc.eval_phase_two_raw, original);
}
console.log(JSON.stringify({ ...report, hookRestored: true }));
