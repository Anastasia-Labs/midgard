import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { ensureReferenceScriptTargetsProgram } from "../src/transactions/reference-scripts.js";

it("refuses completion when confirmed reference outputs disappear from the canonical provider", async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const provider = new Emulator([account]);
  const lucid = await Lucid(provider, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = [
    { name: "hub-oracle minting", script: authPolicy.mintingScript },
  ];
  const getUtxos = provider.getUtxos.bind(provider);
  provider.getUtxos = async (address) =>
    (await getUtxos(address)).filter((utxo) => !utxo.scriptRef);
  provider.getUtxosByOutRef = async () => [];
  provider.awaitTx = async () => {
    provider.awaitBlock(1);
    return true;
  };
  await expect(
    Effect.runPromise(
      ensureReferenceScriptTargetsProgram(lucid, "test", targets, authPolicy),
    ),
  ).rejects.toThrow(/reference|publication/i);
});

it("submits children from exact accepted parent change before any reference is indexed", async () => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000n });
  const provider = new Emulator([account]);
  const lucid = await Lucid(provider, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, 28)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  provider.awaitTx = async () => {
    provider.awaitBlock(1);
    sinceBlock = 0;
    return true;
  };
  let submitted = 0;
  let peakBeforeBlock = 0;
  let sinceBlock = 0;
  const submit = provider.submitTx.bind(provider);
  provider.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    submitted += 1;
    peakBeforeBlock = Math.max(peakBeforeBlock, ++sinceBlock);
    return hash;
  };
  const result = await Effect.runPromise(
    ensureReferenceScriptTargetsProgram(
      lucid,
      "test",
      targets,
      authPolicy,
      lucid,
      undefined,
      1,
      new Set(),
      {
        mode: "chained",
        synchronize: async () => provider.slot,
        wait: async () => {
          provider.awaitBlock(1);
          sinceBlock = 0;
        },
      },
    ),
  );
  expect(result.map(({ name }) => name)).toEqual(
    targets.map(({ name }) => name),
  );
  expect(submitted).toBeGreaterThan(6);
  expect(peakBeforeBlock).toBe(6);
});

const publicationFixture = async (count = 28) => {
  const account = generateEmulatorAccount({ lovelace: 10_000_000_000n });
  const provider = new Emulator([account]);
  const lucid = await Lucid(provider, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(lucid);
  const targets = Object.keys(SDK.REFERENCE_SCRIPT_AUTH_TOKEN_NAMES)
    .slice(0, count)
    .map((name) => ({ name, script: authPolicy.mintingScript }));
  const options = {
    mode: "chained" as const,
    synchronize: async () => provider.slot,
    wait: async () => {
      provider.awaitBlock(1);
    },
  };
  provider.awaitTx = async () => {
    await options.wait();
    return true;
  };
  const run = () =>
    Effect.runPromise(
      ensureReferenceScriptTargetsProgram(
        lucid,
        "test",
        targets,
        authPolicy,
        lucid,
        undefined,
        1,
        new Set(),
        options,
      ),
    );
  return { provider, lucid, authPolicy, targets, options, run };
};

it("pauses an ambiguous lane, retries identical bytes, and lets the other lane progress", async () => {
  const f = await publicationFixture();
  const submit = f.provider.submitTx.bind(f.provider);
  let ambiguous: string | undefined;
  const attempts: string[] = [];
  let accepted = 0;
  f.provider.submitTx = async (cbor) => {
    attempts.push(cbor);
    if (cbor === ambiguous) throw new Error("transport unavailable");
    const hash = await submit(cbor);
    accepted += 1;
    if (accepted === 2) {
      ambiguous = cbor;
      throw new Error("lost acknowledgement");
    }
    return hash;
  };
  let otherLaneProgress = false;
  f.options.wait = async () => {
    if (ambiguous !== undefined && accepted >= 5) otherLaneProgress = true;
    if (
      ambiguous !== undefined &&
      attempts.filter((cbor) => cbor === ambiguous).length < 2
    )
      return;
    f.provider.awaitBlock(1);
  };
  const result = await f.run();
  expect(otherLaneProgress).toBe(true);
  expect(attempts.filter((cbor) => cbor === ambiguous).length).toBeGreaterThan(
    1,
  );
  expect(result).toHaveLength(28);
  for (const target of f.targets) {
    expect(
      (await f.lucid.utxosAt(await f.lucid.wallet().address())).filter((utxo) =>
        SDK.hasReferenceScriptAuthRole(utxo, target, f.authPolicy),
      ),
    ).toHaveLength(1);
  }
});

it("rebuilds only a missing suffix after its rejected attempt expires", async () => {
  const f = await publicationFixture();
  const submit = f.provider.submitTx.bind(f.provider);
  let rejected: string | undefined;
  let submissions = 0;
  let expiry = 0;
  let replacedAfterExpiry = false;
  f.provider.submitTx = async (cbor) => {
    submissions += 1;
    if (submissions === 2) {
      rejected = cbor;
      expiry = Number(CML.Transaction.from_cbor_hex(cbor).body().ttl());
    }
    if (cbor === rejected) throw new Error("temporary rejection");
    if (rejected !== undefined && f.provider.slot >= expiry)
      replacedAfterExpiry = true;
    return submit(cbor);
  };
  expect(await f.run()).toHaveLength(28);
  expect(replacedAfterExpiry).toBe(true);
  const outputs = await f.lucid.utxosAt(await f.lucid.wallet().address());
  for (const target of f.targets)
    expect(
      outputs.filter((utxo) =>
        SDK.hasReferenceScriptAuthRole(utxo, target, f.authPolicy),
      ),
    ).toHaveLength(1);
});

it("waits out lost memory before rediscovering accepted publications on restart", async () => {
  const f = await publicationFixture(4);
  const submit = f.provider.submitTx.bind(f.provider);
  let submitted = 0;
  f.provider.submitTx = async (cbor) => {
    submitted += 1;
    return submit(cbor);
  };
  const wait = f.options.wait;
  f.options.wait = async () => {
    if (submitted >= 2) throw new Error("process interrupted");
    await wait();
  };
  await expect(f.run()).rejects.toThrow(/publication/);
  const interruptedSlot = f.provider.slot;
  f.options.wait = wait;
  const result = await f.run();
  expect(result).toHaveLength(4);
  expect(submitted).toBe(2);
  expect(f.provider.slot).toBeGreaterThanOrEqual(interruptedSlot + 300);
});

it("does not reuse stale confirmation evidence after a publication rollback", async () => {
  const f = await publicationFixture(4);
  const getUtxos = f.provider.getUtxos.bind(f.provider);
  let exposed = false;
  let rolledBack = false;
  f.provider.getUtxos = async (address) => {
    const outputs = await getUtxos(address);
    if (outputs.some((utxo) => utxo.scriptRef)) {
      if (!exposed) {
        exposed = true;
        return outputs;
      }
      rolledBack = true;
      return outputs.filter((utxo) => !utxo.scriptRef);
    }
    return outputs;
  };
  f.options.wait = async () => {
    if (rolledBack) throw new Error("canonical rollback remains unresolved");
    f.provider.awaitBlock(1);
  };
  await expect(f.run()).rejects.toThrow(/publication/);
  expect(rolledBack).toBe(true);
});

it("keeps serial execution at one unconfirmed publication", async () => {
  const f = await publicationFixture();
  const submit = f.provider.submitTx.bind(f.provider);
  let outstanding = 0;
  let peak = 0;
  f.provider.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    peak = Math.max(peak, ++outstanding);
    return hash;
  };
  const result = await Effect.runPromise(
    ensureReferenceScriptTargetsProgram(
      f.lucid,
      "test",
      f.targets,
      f.authPolicy,
      f.lucid,
      undefined,
      1,
      new Set(),
      {
        ...f.options,
        mode: "serial",
        wait: async () => {
          f.provider.awaitBlock(1);
          outstanding = 0;
        },
      },
    ),
  );
  expect(result).toHaveLength(28);
  expect(peak).toBe(1);
});

it("keeps rollback resubmission within the outstanding bounds across formerly confirmed batches", async () => {
  const f = await publicationFixture(40);
  const submit = f.provider.submitTx.bind(f.provider);
  let submissions = 0;
  let sinceBlock = 0;
  let peak = 0;
  let splitLedger: typeof f.provider.ledger | undefined;
  let rolledBack = false;
  f.provider.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    submissions += 1;
    peak = Math.max(peak, ++sinceBlock);
    return hash;
  };
  f.options.wait = async () => {
    if (!rolledBack && submissions >= 11 && splitLedger !== undefined) {
      f.provider.ledger = structuredClone(splitLedger);
      f.provider.mempool = {};
      rolledBack = true;
    }
    f.provider.awaitBlock(1);
    if (submissions === 1) splitLedger = structuredClone(f.provider.ledger);
    sinceBlock = 0;
  };
  expect(await f.run()).toHaveLength(40);
  expect(rolledBack).toBe(true);
  expect(peak).toBeLessThanOrEqual(6);
});

it("bounds signed bytes independently of lane depth without changing the packing rule", async () => {
  const f = await publicationFixture(28);
  const scripts = CML.NativeScriptList.new();
  for (let i = 0; i < 410; i++)
    scripts.add(
      CML.NativeScript.new_script_pubkey(
        CML.Ed25519KeyHash.from_hex("ab".repeat(28)),
      ),
    );
  const largeScript = {
    type: "Native" as const,
    script: CML.NativeScript.new_script_all(scripts).to_cbor_hex(),
  };
  const targets = f.targets.map((target) => ({
    ...target,
    script: largeScript,
  }));
  const submit = f.provider.submitTx.bind(f.provider);
  let bytes = 0;
  let count = 0;
  let peakBytes = 0;
  let peakCount = 0;
  f.provider.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    bytes += cbor.length / 2;
    peakBytes = Math.max(peakBytes, bytes);
    peakCount = Math.max(peakCount, ++count);
    return hash;
  };
  f.options.wait = async () => {
    f.provider.awaitBlock(1);
    bytes = 0;
    count = 0;
  };
  const result = await Effect.runPromise(
    ensureReferenceScriptTargetsProgram(
      f.lucid,
      "test",
      targets,
      f.authPolicy,
      f.lucid,
      undefined,
      1,
      new Set(),
      f.options,
    ),
  );
  expect(result).toHaveLength(28);
  expect(peakBytes).toBeLessThanOrEqual(65_536);
  expect(peakCount).toBeLessThan(6);
  expect(peakCount).toBeGreaterThan(1);
});

it("allocates existing disjoint confirmed funding without an unnecessary split", async () => {
  const f = await publicationFixture(8);
  const address = await f.lucid.wallet().address();
  const split = await f.lucid
    .newTx()
    .pay.ToAddress(address, { lovelace: 4_000_000_000n })
    .complete({ localUPLCEval: true });
  await (await split.sign.withWallet().complete()).submit();
  f.provider.awaitBlock(1);
  const submit = f.provider.submitTx.bind(f.provider);
  let nonPublicationSubmissions = 0;
  f.provider.submitTx = async (cbor) => {
    if (CML.Transaction.from_cbor_hex(cbor).body().mint() === undefined)
      nonPublicationSubmissions += 1;
    return submit(cbor);
  };
  expect(await f.run()).toHaveLength(8);
  expect(nonPublicationSubmissions).toBe(0);
});

it("fills both bounded lanes even when Kupo falls behind after parent acceptance", async () => {
  const f = await publicationFixture();
  const submit = f.provider.submitTx.bind(f.provider);
  let submissions = 0;
  f.provider.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    submissions += 1;
    return hash;
  };
  f.options.synchronize = async () => {
    if (submissions >= 2 && submissions < 7)
      throw new Error("Kupo is behind; children must not require this barrier");
    return f.provider.slot;
  };
  expect(await f.run()).toHaveLength(28);
});

it("recovers the original confirmed wallet inputs when a rollback removes the funding split", async () => {
  const f = await publicationFixture();
  const originalLedger = structuredClone(f.provider.ledger);
  const submit = f.provider.submitTx.bind(f.provider);
  let submissions = 0;
  let rolledBack = false;
  f.provider.submitTx = async (cbor) => {
    const hash = await submit(cbor);
    submissions += 1;
    return hash;
  };
  f.options.synchronize = async () => {
    if (!rolledBack && submissions >= 7) {
      rolledBack = true;
      f.provider.ledger = structuredClone(originalLedger);
      f.provider.mempool = {};
      f.provider.transactionHistory = {};
      f.provider.awaitBlock(20);
    }
    return f.provider.slot;
  };
  expect(await f.run()).toHaveLength(28);
  expect(rolledBack).toBe(true);
});

it("rejects a canonical authenticated role with the wrong script before publishing a duplicate", async () => {
  const f = await publicationFixture(4);
  const getUtxos = f.provider.getUtxos.bind(f.provider);
  f.provider.getUtxos = async (address) =>
    (await getUtxos(address)).map((utxo) =>
      utxo.scriptRef
        ? {
            ...utxo,
            scriptRef: {
              type: "Native" as const,
              script: `8200581c${"cd".repeat(28)}`,
            },
          }
        : utxo,
    );
  let waits = 0;
  f.options.wait = async () => {
    if (++waits > 40)
      throw new Error("did not reject the malformed role promptly");
    f.provider.awaitBlock(1);
  };
  await expect(f.run()).rejects.toThrow(/Malformed authenticated reference/);
});
