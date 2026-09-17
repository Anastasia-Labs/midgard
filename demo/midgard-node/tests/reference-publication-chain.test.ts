import { mkdtemp, readFile } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Emulator,
  generateEmulatorAccount,
  Lucid,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { expect, it } from "vitest";

import {
  DEFAULT_PUBLICATION_SCHEDULE,
  publicationAuthorityLifetime,
  type PublicationChainBackend,
  PublicationJournal,
  type PublicationTransaction,
  publicationTransaction,
  publishReferenceChain,
  ReferencePublicationChain,
} from "./helpers/reference-publication-chain.js";

const fixture = async (count = 5, largeReferences = false) => {
  const account = generateEmulatorAccount({ lovelace: 1_000_000_000_000n });
  const emulator = new Emulator([account]);
  const lucid = await Lucid(emulator, "Custom");
  lucid.selectWallet.fromSeed(account.seedPhrase);
  const authPolicy = await SDK.createReferenceScriptAuthPolicy(
    lucid,
    emulator.now(),
    900_137,
  );
  const scripts = CML.NativeScriptList.new();
  for (let index = 0; index < 280; index += 1)
    scripts.add(
      CML.NativeScript.new_script_pubkey(
        CML.Ed25519KeyHash.from_hex("ab".repeat(28)),
      ),
    );
  const referenceScript = largeReferences
    ? {
        type: "Native" as const,
        script: CML.NativeScript.new_script_all(scripts).to_cbor_hex(),
      }
    : authPolicy.mintingScript;
  const targets = Object.keys(
    DEPLOYMENT_MANIFEST_REFERENCE_SCRIPT_CONTRACT_BY_ROLE,
  )
    .slice(0, count)
    .map((name) => ({ name, script: referenceScript }));
  const path = join(
    await mkdtemp(join(tmpdir(), "publication-test-")),
    "journal.ndjson",
  );
  const schedule = {
    ...DEFAULT_PUBLICATION_SCHEDULE,
    maxUnconfirmedTransactions: 2,
  };
  const build = async () => {
    let funding = await lucid.wallet().getUtxos();
    const transactions: PublicationTransaction[] = [];
    for (const target of targets) {
      const { tx, layout } = await Effect.runPromise(
        SDK.completeReferenceScriptPublicationTxProgram({
          lucid,
          selectedFundingInputs: funding,
          walletAddress: account.address,
          referenceScriptsAddress: account.address,
          missingTargets: [target],
          authPolicy,
        }),
      );
      lucid.overrideUTxOs([...funding]);
      const signed = await tx.sign
        .withWallet()
        .complete()
        .finally(() => lucid.clearUTxOOverride());
      const transaction = publicationTransaction(
        signed.toCBOR(),
        [
          {
            role: target.name,
            outputIndex: layout.localReferenceOutputs.get(target.name)!
              .outputIndex,
          },
        ],
        new Set(transactions.map(({ hash }) => hash)),
        Date.now(),
      );
      transactions.push(transaction);
      funding = [transaction.outputs[transaction.fundingOutputIndex]!.utxo];
    }
    return transactions;
  };
  const submitted: string[] = [];
  let waits = 0;
  const backend: PublicationChainBackend = {
    submit: async (tx) => {
      // The entire signed record and unknown outcome must be durable before I/O.
      const contents = await readFile(path, "utf8");
      expect(contents).toContain(tx.signedCbor);
      expect(contents).toContain('"outcome":"unknown"');
      const hash = await emulator.submitTx(tx.signedCbor);
      submitted.push(hash);
      return hash;
    },
    observe: async (tx) => ({
      slot: emulator.slot,
      confirmed:
        (
          await emulator.getUtxosByOutRef(
            tx.roles.map(({ outputIndex }) => ({
              txHash: tx.hash,
              outputIndex,
            })),
          )
        ).length === tx.roles.length,
    }),
    wait: async () => {
      waits += 1;
      emulator.awaitBlock(1);
    },
  };
  return {
    emulator,
    lucid,
    targets,
    authPolicy,
    path,
    schedule,
    build,
    backend,
    submitted,
    waits: () => waits,
  };
};

it("publishes chained parents before children with bounded backpressure and preserves every role output", async () => {
  const f = await fixture();
  const transactions = await f.build();
  const journal = await PublicationJournal.open(f.path);
  const chain = new ReferencePublicationChain(journal, f.backend, f.schedule);
  try {
    await chain.enqueue(transactions[0]!);
    await chain.enqueue(transactions[1]!);
    expect(f.waits()).toBe(0);
    expect(f.submitted).toEqual(
      transactions.slice(0, 2).map(({ hash }) => hash),
    );
    for (const tx of transactions.slice(2)) await chain.enqueue(tx);
    await chain.drain();
    expect(chain.metrics.peakUnconfirmedTransactions).toBe(2);
    expect(chain.metrics.peakUnconfirmedBytes).toBeLessThanOrEqual(
      f.schedule.maxUnconfirmedBytes,
    );
    expect(chain.metrics.backpressureCount).toBeGreaterThan(0);
    for (const tx of transactions) {
      expect(journal.records.get(tx.hash)?.outcome).toBe("confirmed");
      const references = await f.emulator.getUtxosByOutRef(
        tx.roles.map(({ outputIndex }) => ({ txHash: tx.hash, outputIndex })),
      );
      expect(references).toHaveLength(1);
      expect(
        references[0]!.assets[
          SDK.referenceScriptAuthUnit(f.authPolicy.policyId, tx.roles[0]!.role)
        ],
      ).toBe(1n);
    }
    for (const [index, tx] of transactions.entries()) {
      if (index > 0)
        expect(tx.inputs).toContainEqual({
          txHash: transactions[index - 1]!.hash,
          outputIndex: transactions[index - 1]!.fundingOutputIndex,
        });
    }
  } finally {
    await journal.close();
  }
});

it("applies byte backpressure independently of the transaction count using actual signed sizes", async () => {
  const f = await fixture(3, true);
  const transactions = await f.build();
  const journal = await PublicationJournal.open(f.path);
  const chain = new ReferencePublicationChain(journal, f.backend, {
    ...f.schedule,
    maxUnconfirmedTransactions: 100,
    maxUnconfirmedBytes: 16_384,
  });
  try {
    expect(
      transactions[0]!.signedBytes + transactions[1]!.signedBytes,
    ).toBeGreaterThan(16_384);
    for (const tx of transactions) await chain.enqueue(tx);
    await chain.drain();
    expect(chain.metrics.peakUnconfirmedTransactions).toBe(1);
    expect(chain.metrics.peakUnconfirmedBytes).toBeLessThanOrEqual(16_384);
    expect(chain.metrics.backpressureCount).toBeGreaterThan(0);
    expect(f.submitted).toHaveLength(3);
  } finally {
    await journal.close();
  }
});

it("reconciles interruption after node acceptance before acknowledgement and resumes identical bytes", async () => {
  const f = await fixture(3);
  const transactions = await f.build();
  let journal = await PublicationJournal.open(f.path);
  await journal.prepare(transactions[0]!);
  await journal.outcome(
    transactions[0]!.hash,
    "unknown",
    "submission in progress",
  );
  await f.emulator.submitTx(transactions[0]!.signedCbor);
  await journal.prepare(transactions[1]!);
  // Process died here: no submission acknowledgement or confirmed receipt.
  await journal.close();
  journal = await PublicationJournal.open(f.path);
  const chain = new ReferencePublicationChain(journal, f.backend, f.schedule);
  try {
    await chain.resume();
    expect(journal.records.get(transactions[0]!.hash)?.outcome).toBe(
      "confirmed",
    );
    for (const tx of transactions.slice(2)) await chain.enqueue(tx);
    await chain.drain();
    expect(journal.records.size).toBe(3);
    expect(
      new Set(
        [...journal.records.values()].flatMap(({ transaction }) =>
          transaction.roles.map(({ role }) => role),
        ),
      ).size,
    ).toBe(3);
    expect(
      [...journal.records.values()].every(
        ({ outcome }) => outcome === "confirmed",
      ),
    ).toBe(true);
  } finally {
    await journal.close();
  }
});

it("resolves expired parents and all pending descendants before any replacement can be constructed", async () => {
  const f = await fixture(3);
  const transactions = await f.build();
  const journal = await PublicationJournal.open(f.path);
  try {
    for (const tx of transactions) {
      await journal.prepare(tx);
      await journal.outcome(tx.hash, "unknown", "interrupted submit");
    }
    const submissions: string[] = [];
    const chain = new ReferencePublicationChain(
      journal,
      {
        ...f.backend,
        observe: async () => ({
          slot: transactions[0]!.expiresAtSlot,
          confirmed: false,
        }),
        submit: async (tx) => {
          submissions.push(tx.hash);
          return tx.hash;
        },
      },
      f.schedule,
    );
    await chain.resume();
    expect(submissions).toEqual([]);
    expect(chain.pending()).toEqual([]);
    expect([...journal.records.values()].map(({ outcome }) => outcome)).toEqual(
      ["rejected", "rejected", "rejected"],
    );
    expect(journal.records.size).toBe(3);
  } finally {
    await journal.close();
  }
});

it("resolves a rejected parent and descendants without mistaking provider errors for definitive rejection", async () => {
  const f = await fixture(3);
  const transactions = await f.build();
  const journal = await PublicationJournal.open(f.path);
  try {
    for (const tx of transactions) await journal.prepare(tx);
    const chain = new ReferencePublicationChain(
      journal,
      {
        ...f.backend,
        observe: async (tx) => ({
          slot: 0,
          confirmed: false,
          ...(tx.hash === transactions[0]!.hash
            ? { conflictingInputs: tx.inputs }
            : {}),
        }),
      },
      f.schedule,
    );
    await chain.resume();
    expect(chain.pending()).toEqual([]);
    expect(
      [...journal.records.values()].every(
        ({ outcome }) => outcome === "rejected",
      ),
    ).toBe(true);
    expect(f.submitted).toEqual([]);
  } finally {
    await journal.close();
  }
});

it("uses existing builders and exact pending predecessor outputs in the complete publication scheduler", async () => {
  const f = await fixture();
  const result = await publishReferenceChain({
    lucid: f.lucid,
    targets: f.targets,
    authPolicy: f.authPolicy,
    journalPath: f.path,
    maxTargetsPerBatch: 2,
    schedule: f.schedule,
    publicationLimit: () => 15_872,
    synchronize: async () => f.emulator.slot,
    wait: async () => f.emulator.awaitBlock(1),
    now: Date.now,
  });
  expect(result.transactions).toHaveLength(3);
  expect(result.metrics.peakUnconfirmedTransactions).toBe(2);
  expect(result.transactions.flatMap(({ roles }) => roles)).toHaveLength(5);
  const restarted = await publishReferenceChain({
    lucid: f.lucid,
    targets: f.targets,
    authPolicy: f.authPolicy,
    journalPath: f.path,
    maxTargetsPerBatch: 2,
    schedule: f.schedule,
    publicationLimit: () => 15_872,
    synchronize: async () => f.emulator.slot,
    wait: async () => f.emulator.awaitBlock(1),
    now: Date.now,
  });
  expect(restarted.transactions.map(({ hash }) => hash)).toEqual(
    result.transactions.map(({ hash }) => hash),
  );
  expect(restarted.metrics.resubmissions).toBe(0);
});

it("sizes the authority from workload and throughput with bounded confirmation allowance", () => {
  const schedule = DEFAULT_PUBLICATION_SCHEDULE;
  expect(publicationAuthorityLifetime(517, schedule)).toBeGreaterThan(
    15 * 60_000,
  );
  expect(
    publicationAuthorityLifetime(517, {
      ...schedule,
      measuredBytesPerSecond: schedule.measuredBytesPerSecond / 2,
    }),
  ).toBeGreaterThan(publicationAuthorityLifetime(517, schedule));
  expect(() =>
    publicationAuthorityLifetime(517, {
      ...schedule,
      confirmationAllowanceMs: 72 * 60 * 60_000,
    }),
  ).toThrow();
});

it("resolves a canonically invalid chain and refuses replacements without mutually exclusive funding evidence", async () => {
  const f = await fixture(3);
  const obsolete = await f.build();
  const journal = await PublicationJournal.open(f.path);
  for (const tx of obsolete) await journal.prepare(tx);
  await journal.close();
  // Another confirmed wallet transaction consumed the old root. Its returned
  // plain funding can safely anchor a replacement under the same live authority.
  const conflict = await f.lucid
    .newTx()
    .collectFrom(await f.lucid.wallet().getUtxos())
    .pay.ToAddress(await f.lucid.wallet().address(), {
      lovelace: 900_000_000_000n,
    })
    .complete({ localUPLCEval: true });
  await (await conflict.sign.withWallet().complete()).submit();
  f.emulator.awaitBlock(1);
  await expect(
    publishReferenceChain({
      lucid: f.lucid,
      targets: f.targets,
      authPolicy: f.authPolicy,
      journalPath: f.path,
      maxTargetsPerBatch: 1,
      schedule: f.schedule,
      publicationLimit: () => 15_872,
      synchronize: async () => f.emulator.slot,
      wait: async () => f.emulator.awaitBlock(1),
      now: Date.now,
    }),
  ).rejects.toThrow(/mutually exclusive funding/);
  const restored = await PublicationJournal.open(f.path);
  try {
    expect(restored.records.size).toBe(3);
    expect(
      obsolete.every(
        ({ hash }) => restored.records.get(hash)?.outcome === "rejected",
      ),
    ).toBe(true);
    const outputs = await f.lucid.wallet().getUtxos();
    for (const { name } of f.targets) {
      const unit = SDK.referenceScriptAuthUnit(f.authPolicy.policyId, name);
      expect(
        outputs.reduce((sum, output) => sum + (output.assets[unit] ?? 0n), 0n),
      ).toBe(0n);
    }
  } finally {
    await restored.close();
  }
});
