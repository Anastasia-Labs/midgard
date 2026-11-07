/**
 * `scriptIntegrityHashMismatch` at the consensus maximum purpose shape, from a
 * genuine Init: the spend-input, output, required-observer and mint fields
 * each hold as many purposes as their byte cap admits, with the script-witness
 * and redeemer fields within theirs. Every spend is script-locked, every
 * output pays a protected script credential, and every observer and mint
 * policy has its own source, so the walk resolves an execution for every item
 * of every purpose field. A wrong hash convicts; the same shape with the
 * expected hash is refused at the terminal.
 */
import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
  hashMidgardVersionedScript,
  MIDGARD_CONSENSUS_LIMITS,
  type MidgardVersionedScript,
} from "@al-ft/midgard-core";
import {
  computeHash28,
  computeHash32,
  encodeMidgardNativeScript,
} from "@al-ft/midgard-core/codec";
import { describe, expect, it } from "vitest";

import type { LedgerEntry } from "../src/ledger-at.js";
import {
  requireConvictingScriptIntegrityHashMismatchProof,
  ScriptIntegrityHashMismatchHonestLeaf,
} from "../src/script-integrity-hash-mismatch/index.js";
import { keyValuePhasMembershipProofs } from "../src/transition-trace/phas.js";
import { expectOnchainRefusal } from "./support/emulator/expect-onchain-refusal.js";
import { scriptAddress } from "./support/missing-redeemer-emulator.purposes.js";
import {
  driveScriptIntegrityHashMismatchThread,
  makeScriptIntegrityHashMismatchHarness,
  publishScriptIntegrityHashMismatchRoles,
  SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS,
  type StepMeasurement,
  stepTable,
} from "./support/script-integrity-hash-mismatch-emulator.js";
import {
  buildIntegritySubject,
  expectedHash,
  type IntegritySubjectShape,
  nativeScript,
  plutusScript,
  setupIntegrityScenario,
  WRONG_HASH,
} from "./support/script-integrity-hash-mismatch-emulator.subject.js";
import { expectSingleUtxoWithUnit } from "./support/submit-init-emulator-shared.js";

const FIELD_CAP = MIDGARD_CONSENSUS_LIMITS.maxTransactionAggregateFieldBytes;

type Dimensions = {
  readonly spends: number;
  readonly outputs: number;
  readonly observers: number;
  readonly mints: number;
};

/** The one Plutus V3 script every spent output is locked by, and the one
 * every output pays. */
const SPENT = plutusScript(1);
const RECEIVED = plutusScript(2);

const shapeOf = (
  { spends, outputs, observers, mints }: Dimensions,
  integrity: IntegritySubjectShape["integrity"],
): IntegritySubjectShape => ({
  spend: Array.from({ length: spends }, (_, index) => ({
    outRef: encodeMidgardSpendInputItem({
      txId: Buffer.alloc(32, 0x41),
      outputIndex: index,
    }),
    output: encodeMidgardTxOutput({
      address: scriptAddress(SPENT),
      value: { lovelace: 5_000_000n, assets: new Map() },
    }),
  })),
  spendRedeemers: Array.from({ length: spends }, (_, index) => BigInt(index)),
  receives: Array.from({ length: outputs }, () => ({
    script: RECEIVED,
    witness: false,
    redeemer: true,
  })),
  observers: Array.from({ length: observers }, (_, index) => ({
    script: nativeScript(BigInt(1_000_000 + index)),
    redeemer: false,
  })),
  mints: Array.from({ length: mints }, (_, index) => ({
    script: nativeScript(BigInt(index)),
    redeemer: false,
  })),
  witnesses: [SPENT, RECEIVED],
  integrity,
});

const fieldBytes = (dimensions: Dimensions) => {
  const tx = buildIntegritySubject(shapeOf(dimensions, WRONG_HASH));
  return {
    spends: tx.body.spendInputsPreimageCbor.length,
    outputs: tx.body.outputsPreimageCbor.length,
    observers: tx.body.requiredObserversPreimageCbor.length,
    mints: tx.body.mintPreimageCbor.length,
    witnesses: tx.witnessSet.scriptTxWitsPreimageCbor.length,
    redeemers: tx.witnessSet.redeemerTxWitsPreimageCbor.length,
  };
};

/** The largest value in [low, high) for which `fits` holds (`fits(low)`
 * must). */
const largest = (
  low: number,
  high: number,
  fits: (value: number) => boolean,
): number => {
  let [ok, bad] = [low, high];
  while (bad - ok > 1) {
    const mid = Math.floor((ok + bad) / 2);
    if (fits(mid)) ok = mid;
    else bad = mid;
  }
  return ok;
};

const ONE: Dimensions = { spends: 1, outputs: 1, observers: 1, mints: 1 };

/** Each purpose field filled to its own cap. */
const maximumShape = (): Dimensions => {
  const fill = (key: keyof Dimensions) =>
    largest(
      1,
      FIELD_CAP,
      (value) => fieldBytes({ ...ONE, [key]: value })[key] <= FIELD_CAP,
    );
  return {
    spends: fill("spends"),
    outputs: fill("outputs"),
    observers: fill("observers"),
    mints: fill("mints"),
  };
};

/** Whether a trie path (the Blake2b-256 of the key) starts with the hex
 * digits of `prefix`. */
const pathStarts = (key: Uint8Array, prefix: string): boolean => {
  const path = computeHash32(key);
  for (let digit = 0; digit < prefix.length; digit += 1) {
    const nibble = (path[digit >> 1]! >> (digit % 2 === 0 ? 4 : 0)) & 0xf;
    if (nibble !== Number.parseInt(prefix[digit]!, 16)) return false;
  }
  return true;
};

/** A path that follows `prefix` for exactly `depth` digits, then leaves it. */
const leavesAt = (key: Uint8Array, prefix: string, depth: number) =>
  pathStarts(key, prefix.slice(0, depth)) &&
  !pathStarts(key, prefix.slice(0, depth + 1));

/** Digits every clustered key's path shares; one padding key leaves the
 * cluster at each of them, so every clustered key's proof steps through a
 * branch at each shared digit before the cluster's own branches. */
const CLUSTER_DIGITS = 3;
const LEDGER_CLUSTER = "a5c";

const KEY_LOCKED = encodeMidgardTxOutput({
  address: Buffer.from(`60${"44".repeat(28)}`, "hex"),
  value: { lovelace: 5_000_000n, assets: new Map() },
});

/** The first out-ref at `outputIndex`, varying its transaction id, whose path
 * satisfies `accepts`. */
const groundOutRef = (
  outputIndex: number,
  fill: number,
  accepts: (key: Buffer) => boolean,
): Buffer => {
  const outRef = encodeMidgardSpendInputItem({
    txId: Buffer.alloc(32, fill),
    outputIndex,
  });
  for (let nonce = 0; ; nonce += 1) {
    outRef.writeUInt32BE(nonce, 3);
    if (accepts(outRef)) return outRef;
  }
};

/** Native `after` scripts with a four-byte slot, in slot order from 2^24,
 * whose hashes' paths satisfy `accepts`. */
const groundNatives = (
  count: number,
  from: number,
  accepts: (hash: Buffer) => boolean,
): { readonly scripts: MidgardVersionedScript[]; readonly next: number } => {
  const preimage = Buffer.concat([
    Buffer.from([0x00]),
    encodeMidgardNativeScript({ type: "after", slot: 1n << 24n }),
  ]);
  const slotAt = preimage.length - 4;
  const scripts: MidgardVersionedScript[] = [];
  let slot = from;
  for (; scripts.length < count; slot += 1) {
    preimage.writeUInt32BE(slot, slotAt);
    const hash = computeHash28(preimage);
    if (!accepts(hash)) continue;
    const script = nativeScript(BigInt(slot));
    if (hashMidgardVersionedScript(script) !== hash.toString("hex"))
      throw new Error("native script preimage differs");
    scripts.push(script);
  }
  return { scripts, next: slot };
};

/**
 * The maximum shape over a deep predecessor ledger and a deep source map:
 * every spent out-ref and every purpose script hash is ground into one
 * three-digit path cluster per trie, and one padding key leaves each cluster
 * at each of its digits, so every opened key sits at least five branches
 * deep.
 */
const deepShapeOf = (
  dimensions: Dimensions,
  integrity: IntegritySubjectShape["integrity"],
): IntegritySubjectShape => {
  const base = shapeOf(dimensions, integrity);
  const sourceCluster = computeHash32(
    Buffer.from(hashMidgardVersionedScript(SPENT), "hex"),
  )
    .toString("hex")
    .slice(0, CLUSTER_DIGITS);
  const inSourceCluster = (hash: Buffer) => pathStarts(hash, sourceCluster);
  let seed = 2;
  while (
    !inSourceCluster(
      Buffer.from(hashMidgardVersionedScript(plutusScript(seed)), "hex"),
    )
  )
    seed += 1;
  const received = plutusScript(seed);
  const purposes = groundNatives(
    dimensions.observers + dimensions.mints,
    1 << 24,
    inSourceCluster,
  );
  let next = purposes.next;
  const sourcePadding = Array.from({ length: CLUSTER_DIGITS }, (_, depth) => {
    const ground = groundNatives(1, next, (hash) =>
      leavesAt(hash, sourceCluster, depth),
    );
    next = ground.next;
    return ground.scripts[0]!;
  });
  const ledgerPadding: LedgerEntry[] = Array.from(
    { length: CLUSTER_DIGITS },
    (_, depth) => ({
      outRef: groundOutRef(0, 0x50 + depth, (key) =>
        leavesAt(key, LEDGER_CLUSTER, depth),
      ),
      output: KEY_LOCKED,
    }),
  );
  return {
    ...base,
    spend: (base.spend ?? []).map((entry, index) => ({
      ...entry,
      outRef: groundOutRef(index, 0x41, (key) =>
        pathStarts(key, LEDGER_CLUSTER),
      ),
    })),
    padding: ledgerPadding,
    receives: (base.receives ?? []).map((entry) => ({
      ...entry,
      script: received,
    })),
    observers: purposes.scripts
      .slice(0, dimensions.observers)
      .map((script) => ({ script, redeemer: false })),
    mints: purposes.scripts
      .slice(dimensions.observers)
      .map((script) => ({ script, redeemer: false })),
    witnesses: [SPENT, received, ...sourcePadding],
  };
};

/** Each page's item count, by the label prefix of its transactions. */
const pageSizes = (steps: readonly StepMeasurement[], prefix: string) =>
  steps.flatMap(({ label }) => {
    const match = label.endsWith(")") && label.startsWith(`${prefix} `);
    return match ? [Number(label.slice(label.lastIndexOf("(") + 1, -1))] : [];
  });

const expectWithinBounds = (steps: readonly StepMeasurement[]): void => {
  for (const { label, measurement } of steps) {
    expect(measurement.executionMemory, `${label} memory`).toBeLessThanOrEqual(
      SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.memory,
    );
    expect(measurement.executionSteps, `${label} cpu`).toBeLessThanOrEqual(
      SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.steps,
    );
    expect(
      measurement.completeSignedBytes,
      `${label} bytes`,
    ).toBeLessThanOrEqual(SCRIPT_INTEGRITY_HASH_MISMATCH_STEP_BOUNDS.bytes);
  }
};

const summary = (steps: readonly StepMeasurement[]) => {
  const table = stepTable(steps);
  const max = (key: "memory" | "cpu") =>
    table.reduce(
      (best, row) => (BigInt(row[key]) > BigInt(best[key]) ? row : best),
      table[0]!,
    );
  return {
    transactions: table.length,
    maxMemory: max("memory"),
    maxCpu: max("cpu"),
    maxBytes: Math.max(...table.map((row) => row.bytes)),
  };
};

describe("script-integrity-hash-mismatch consensus-maximum purpose shape", () => {
  it("fills every purpose field to its cap, with witnesses and redeemers within theirs", () => {
    const dimensions = maximumShape();
    const at = fieldBytes(dimensions);
    for (const value of Object.values(at))
      expect(value).toBeLessThanOrEqual(FIELD_CAP);
    for (const key of Object.keys(dimensions) as (keyof Dimensions)[])
      expect(
        fieldBytes({ ...dimensions, [key]: dimensions[key] + 1 })[key],
      ).toBeGreaterThan(FIELD_CAP);
  }, 120_000);

  it("convicts an accepted transaction carrying a wrong hash", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    const dimensions = maximumShape();
    const scenario = await setupIntegrityScenario({
      harness,
      shape: shapeOf(dimensions, WRONG_HASH),
      source: { kind: "normal" },
    });
    const run = await driveScriptIntegrityHashMismatchThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    expect(run.prepared.final.facts.language_bitmap).toBe(1n);
    const outcome = await run.terminal();
    console.log(
      "SIHM max-shape conviction",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.scriptIntegrityHashMismatch.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("fits every page to the budget over a deep ledger and source map, and convicts", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    const dimensions = maximumShape();
    const shape = deepShapeOf(dimensions, WRONG_HASH);
    const tx = buildIntegritySubject(shape);
    for (const bytes of [
      tx.body.spendInputsPreimageCbor,
      tx.body.outputsPreimageCbor,
      tx.body.requiredObserversPreimageCbor,
      tx.body.mintPreimageCbor,
      tx.witnessSet.scriptTxWitsPreimageCbor,
      tx.witnessSet.redeemerTxWitsPreimageCbor,
    ])
      expect(bytes.length).toBeLessThanOrEqual(FIELD_CAP);
    const scenario = await setupIntegrityScenario({
      harness,
      shape,
      source: { kind: "normal" },
    });
    const run = await driveScriptIntegrityHashMismatchThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(true);
    const { ledger } = scenario.material;
    const ledgerDepths = (
      await keyValuePhasMembershipProofs(
        ledger.root,
        scenario.material.fields[0]!.map(
          (item) =>
            ledger.root.entries.find((entry) => entry.key.equals(item))!,
        ),
      )
    ).map((proof) => proof.length);
    // Every script an execution opens in the source map; the padding
    // scripts are never opened.
    const opened = new Set(
      [
        SPENT,
        ...[
          ...(shape.receives ?? []),
          ...(shape.observers ?? []),
          ...(shape.mints ?? []),
        ].map((entry) => entry.script),
      ].map(hashMidgardVersionedScript),
    );
    const sourceDepths = await Promise.all(
      [...opened].map(
        async (hash) => (await run.prepared.evidence.lookup(hash)).proof.length,
      ),
    );
    expect(Math.min(...ledgerDepths)).toBeGreaterThanOrEqual(5);
    expect(Math.min(...sourceDepths)).toBeGreaterThanOrEqual(5);
    const outcome = await run.terminal();
    const sizes = {
      spend: pageSizes(run.steps, "spend take"),
      mint: pageSizes(run.steps, "field 5 walk"),
      observe: pageSizes(run.steps, "field 3 walk"),
      receive: pageSizes(run.steps, "field 2 walk"),
    };
    console.log(
      "SIHM deep max-shape conviction",
      JSON.stringify({
        dimensions,
        ledgerDepth: [Math.min(...ledgerDepths), Math.max(...ledgerDepths)],
        sourceDepth: [Math.min(...sourceDepths), Math.max(...sourceDepths)],
        pageSizes: Object.fromEntries(
          Object.entries(sizes).map(([kind, pages]) => [
            kind,
            {
              pages: pages.length,
              min: Math.min(...pages),
              max: Math.max(...pages),
            },
          ]),
        ),
        walk: summary(
          run.steps.filter(({ label }) =>
            ["spend ", "field 5 walk", "field 3 walk", "field 2 walk"].some(
              (prefix) => label.startsWith(prefix),
            ),
          ),
        ),
        ...summary(run.steps),
      }),
    );
    expectWithinBounds(run.steps);
    expect(sizes.spend.reduce((sum, size) => sum + size, 0)).toBe(
      dimensions.spends,
    );
    // Every spend page opens two proofs per item and was taken below the
    // configured eight.
    expect(Math.max(...sizes.spend)).toBeLessThan(8);
    expect(outcome.fraudulentHeaderHash).toBe(scenario.setup.headerHash);
    await expectSingleUtxoWithUnit(
      harness.proverLucid,
      harness.scriptIntegrityHashMismatch.fraudProof.spendingScriptAddress,
      outcome.fraudProofUnit,
    );
  }, 3_600_000);

  it("refuses the same shape carrying the expected hash", async () => {
    const harness = await makeScriptIntegrityHashMismatchHarness();
    const references = await publishScriptIntegrityHashMismatchRoles(harness);
    const dimensions = maximumShape();
    const scenario = await setupIntegrityScenario({
      harness,
      shape: shapeOf(dimensions, expectedHash("PlutusV3")),
      source: { kind: "normal" },
    });
    await expect(
      requireConvictingScriptIntegrityHashMismatchProof({
        material: scenario.material,
        contracts: harness.scriptIntegrityHashMismatch,
        prover: harness.proverSigner.paymentKeyHash,
      }),
    ).rejects.toThrow(ScriptIntegrityHashMismatchHonestLeaf);
    const run = await driveScriptIntegrityHashMismatchThread({
      harness,
      scenario,
      references,
    });
    expect(run.prepared.convicts).toBe(false);
    console.log(
      "SIHM max-shape honest",
      JSON.stringify({ dimensions, ...summary(run.steps) }),
    );
    expectWithinBounds(run.steps);
    await expectOnchainRefusal(() => run.terminal(false), {
      refusedBy: "fraud_proofs/script_integrity_hash_mismatch/terminal",
      check: /^Validator returned false$/u,
    });
  }, 3_600_000);
});
